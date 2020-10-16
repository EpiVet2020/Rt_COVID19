#Libraries

library(testthat)
library(rlang)
library(dplyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(grid)
library(tibble)
library(ggpubr)
library(ggiraph)
library(gganimate)
library(plotly)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(htmltools)
library(devtools)
install_github("holtzy/epuRate")
library(epuRate)
library(EpiEstim)
library(tidyr)
library(readr)
library(forcats)
library(lubridate)
library(googlesheets)
require(RCurl)
library(viridis)
library(flexdashboard)
library(here)
library(rjson)
library(jsonlite)
library(RCurl)
library(highcharter)
library(here)
library(incidence)
library(purrr)
library(magrittr)


# Set working directory
setwd("~/Desktop/Treino Estágio 2020-2021")

#Data
covid19pt <-read.csv("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv", stringsAsFactors = FALSE)

#Transformar para formato de data
covid19pt$data <- as.Date(covid19pt$data,"%d-%m-%Y")


# Criar novas variáveis da variação do nº confirmados (t - (t-1)) e criar uma tabela
covid19pt <- mutate(covid19pt, 
                   confirmados_lag = lag(confirmados),
                   confirmados_var=confirmados-confirmados_lag,
                   
                   confirmados_lag_n = lag(confirmados_arsnorte),
                   confirmados_var_norte=confirmados_arsnorte-confirmados_lag_n,
                   
                   confirmados_lag_centro = lag(x = confirmados_arscentro),
                   confirmados_var_centro=confirmados_arscentro-confirmados_lag_centro,                   
                   
                   confirmados_lag_lvt = lag(confirmados_arslvt),
                   confirmados_var_lvt=confirmados_arslvt-confirmados_lag_lvt, 
                   
                   confirmados_lag_alentejo = lag(confirmados_arsalentejo),
                   confirmados_var_alentejo=confirmados_arsalentejo-confirmados_lag_alentejo, 
                   
                   confirmados_lag_algarve = lag(confirmados_arsalgarve),
                   confirmados_var_algarve=confirmados_arsalgarve-confirmados_lag_algarve, 
                   
                   confirmados_lag_acores = lag(confirmados_acores ),
                   confirmados_var_acores=confirmados_acores-confirmados_lag_acores, 
                   
                   confirmados_lag_madeira = lag(x = confirmados_madeira),
                   confirmados_var_madeira=confirmados_madeira-confirmados_lag_madeira,
)

# Criar tabela
covid19pt_var <- covid19pt %>%
    select(
        data, confirmados_var, confirmados_var_norte, confirmados_var_centro, confirmados_var_lvt, confirmados_var_alentejo, confirmados_var_algarve, confirmados_var_acores, confirmados_var_madeira
    )

# Previsão da evolução

library(purrr)
library(incidence)

covid_pt_var <- covid19pt_var  %>%
    filter(covid19pt_var$data > as.Date("2020-02-28")) %>%       
    dplyr::mutate(t_start = dplyr::row_number())


### Cálculo do Rt - Uncertainty method --> "uncertain_si"
### Serial Interval (By André Peralta)
### data from https://cmmid.github.io/topics/covid19/current-patterns-transmission/global-time-varying-transmission.html
### -- mean 4.7 (95% CrI: 3.7, 6.0), truncated at 3,7 and 6,0
### -- sd 2.9 (95% CrI: 1.9, 4.9), truncated at 1,9 and 4,9


# Rt Portugal (total)
## Definir o Serial Interval e window
sens_configs <- 
    make_config(
        list(
            mean_si = 4.7, std_mean_si = 0.7,
            min_mean_si = 3.7, max_mean_si = 6.0,
            std_si = 2.9, std_std_si = 0.5,
            min_std_si = 1.9, max_std_si = 4.9,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )

## Aplicar a função Estimate_R
Rt_nonparam_si <- estimate_R(covid_pt_var$confirmados_var, 
        method = "uncertain_si",
        config = sens_configs
    )


### Caracterização dos valores em gráfico 
plot(Rt_nonparam_si, legend = FALSE)

## Determinação do Rt (sample from the posterior R distribution)
### Definir a nossa janela com base no t_start
sample_windows <- seq(length(Rt_nonparam_si$R$t_start))

# Criar um data frame com valores de R
posterior_R_t <- 
    map(.x = sample_windows,
        .f = function(x) {
            
            posterior_sample_obj <- 
                sample_posterior_R(
                    R = Rt_nonparam_si,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si$R$t_start[x],
                    window_t_end = Rt_nonparam_si$R$t_end[x],
                    date_point = covid_pt_var[covid_pt_var$t_start == Rt_nonparam_si$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj),
                    R_e_q0025 = quantile(posterior_sample_obj, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj, probs = 0.975))
            
            return(posterior_sample_estim)}
        ) %>% 
   
     reduce(bind_rows)

##GRÁFICO
### Nº Reprodutivo Diário (Rt) para Portugal
posterior_R_e <- posterior_R_t %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))

highchart() %>%
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(text = "Número Reprodutivo Rt - número médio de casos secundários por nova infecção (janela temporal de 7 dias)") %>% 
    hc_subtitle(text = "Fonte: Autores baseados nos dados da DGS") %>% 
    hc_xAxis(categories = format(posterior_R_e$date_point, "%b %d"),
             tickmarkPlacement = "on",
             title = list(enabled = FALSE)) %>% 
    hc_yAxis(title = list(text = "Rt"),min = 0, 
             plotLines = list(
                 list(label = list(text = "Rt = 1"),
                      color = "#525252",
                      width = 2,
                      value = 1,
                      dashStyle = "shortdash"))) %>% 
    
    hc_add_series(posterior_R_e,
                  hcaes( low = lwr, high = upr),     
                  # id = "ForecastRange-FL",
                  type = "arearange", 
                  name = "Incerteza", 
                  color = "#d9d9d9") %>% 
    hc_add_series(data = posterior_R_e$fit,
                  name = "Rt", 
                  color = "#e6550d")


# Rt Diário ARS Norte

## Substituir valores negativos por 0
covid_pt_var[-c(2)] <- replace(covid_pt_var[-c(2)], covid_pt_var[-c(2)] < 0, 0)

## Função
Rt_nonparam_si1 <- 
    estimate_R(
        covid_pt_var$confirmados_var_norte, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
plot(Rt_nonparam_si1, legend = FALSE)

## Posterior sample Rt estimate
sample_windows <- seq(length(Rt_nonparam_si1$R$t_start))

posterior_R_t1 <- 
    map(
        .x = sample_windows,
        .f = function(x) {
            
            posterior_sample_obj1 <- 
                sample_posterior_R(
                    R = Rt_nonparam_si1,
                    n = 1000, 
                    window = x
                )
            
            posterior_sample_estim1 <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si1$R$t_start[x],
                    window_t_end = Rt_nonparam_si1$R$t_end[x],
                    date_point = covid_pt_var[covid_pt_var$t_start == Rt_nonparam_si1$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj1),
                    R_e_q0025 = quantile(posterior_sample_obj1, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj1, probs = 0.975)
                )
            
            return(posterior_sample_estim1)
            
        }
    ) %>% 
    reduce(bind_rows)


posterior_R_e1 <- posterior_R_t1 %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))


## Gráfico Rt diário
highchart() %>%
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(text = "Número Reprodutivo Rt ARS Norte - número médio de casos secundários por nova infecção (janela temporal de 7 dias)") %>% 
    hc_subtitle(text = "Fonte: Autores baseados nos dados da DGS") %>% 
    hc_xAxis(categories = format(posterior_R_e1$date_point, "%b %d"),
             tickmarkPlacement = "on",
             title = list(enabled = FALSE)) %>% 
    hc_yAxis(title = list(text = "Rt"),min = 0, 
             plotLines = list(
                 list(label = list(text = "Rt = 1"),
                      color = "#525252",
                      width = 2,
                      value = 1,
                      dashStyle = "shortdash"))) %>% 
    
    hc_add_series(posterior_R_e1, 
                  hcaes( low = lwr, high = upr),     
                  #                id = "ForecastRange-FL", 
                  type = "arearange", 
                  name = "Incerteza", 
                  color = "#d9d9d9") %>% 
    hc_add_series(data = posterior_R_e1$fit,
                  name = "Rt", 
                  color = "#e6550d")


# Rt Diário ARS Centro 

Rt_nonparam_si2 <- 
    estimate_R(
        covid_pt_var$confirmados_var_centro, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
plot(Rt_nonparam_si2, legend = FALSE)

## Posterior sample Rt estimate
sample_windows <- seq(length(Rt_nonparam_si2$R$t_start))

posterior_R_t2 <- 
    map(
        .x = sample_windows,
        .f = function(x) {
            
            posterior_sample_obj2 <- 
                sample_posterior_R(
                    R = Rt_nonparam_si2,
                    n = 1000, 
                    window = x
                )
            
            posterior_sample_estim2 <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si2$R$t_start[x],
                    window_t_end = Rt_nonparam_si2$R$t_end[x],
                    date_point = covid_pt_var[covid_pt_var$t_start == Rt_nonparam_si2$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj2),
                    R_e_q0025 = quantile(posterior_sample_obj2, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj2, probs = 0.975)
                )
            
            return(posterior_sample_estim2)
        }
    ) %>% 
    reduce(bind_rows)

posterior_R_e2 <- posterior_R_t2 %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))


## Gráfico Rt diário
highchart() %>%
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(text = "Número Reprodutivo Rt ARS Centro - número médio de casos secundários por nova infecção (janela temporal de 7 dias)") %>% 
    hc_subtitle(text = "Fonte: Autores baseados nos dados da DGS") %>% 
    hc_xAxis(categories = format(posterior_R_e2$date_point, "%b %d"),
             tickmarkPlacement = "on",
             title = list(enabled = FALSE)) %>% 
    hc_yAxis(title = list(text = "Rt"),min = 0, 
             plotLines = list(
                 list(label = list(text = "Rt = 1"),
                      color = "#525252",
                      width = 2,
                      value = 1,
                      dashStyle = "shortdash"))) %>% 
    
    hc_add_series(posterior_R_e2, 
                  hcaes( low = lwr, high = upr),     
                  #                id = "ForecastRange-FL", 
                  type = "arearange", 
                  name = "Incerteza", 
                  color = "#d9d9d9") %>% 
    hc_add_series(data = posterior_R_e2$fit,
                  name = "Rt", 
                  color = "#e6550d")



# Rt ARS Lisboa e Vale do Tejo

Rt_nonparam_si3 <- 
    estimate_R(
        covid_pt_var$confirmados_var_lvt, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
plot(Rt_nonparam_si3, legend = FALSE)

## Posterior sample Rt estimate
sample_windows <- seq(length(Rt_nonparam_si3$R$t_start))

posterior_R_t3 <- 
    map(
        .x = sample_windows,
        .f = function(x) {
            
            posterior_sample_obj3 <- 
                sample_posterior_R(
                    R = Rt_nonparam_si3,
                    n = 1000, 
                    window = x
                )
            
            posterior_sample_estim3 <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si3$R$t_start[x],
                    window_t_end = Rt_nonparam_si3$R$t_end[x],
                    date_point = covid_pt_var[covid_pt_var$t_start == Rt_nonparam_si3$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj3),
                    R_e_q0025 = quantile(posterior_sample_obj3, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj3, probs = 0.975)
                )
            
            return(posterior_sample_estim3)
            
        }
    ) %>% 
    reduce(bind_rows)

posterior_R_e3 <- posterior_R_t3 %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))


## Gráfico Rt Diário
highchart() %>%
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(text = "Número Reprodutivo Rt ARS LVT - número médio de casos secundários por nova infecção (janela temporal de 7 dias)") %>% 
    hc_subtitle(text = "Fonte: Autores baseados nos dados da DGS") %>% 
    hc_xAxis(categories = format(posterior_R_e3$date_point, "%b %d"),
             tickmarkPlacement = "on",
             title = list(enabled = FALSE)) %>% 
    hc_yAxis(title = list(text = "Rt"),min = 0, 
             plotLines = list(
                 list(label = list(text = "Rt = 1"),
                      color = "#525252",
                      width = 2,
                      value = 1,
                      dashStyle = "shortdash"))) %>% 
    
    hc_add_series(posterior_R_e3, 
                  hcaes( low = lwr, high = upr),     
                  #                id = "ForecastRange-FL", 
                  type = "arearange", 
                  name = "Incerteza", 
                  color = "#d9d9d9") %>% 
    hc_add_series(data = posterior_R_e3$fit,
                  name = "Rt", 
                  color = "#e6550d")


# Rt ARS Alentejo

Rt_nonparam_si4 <- 
    estimate_R(
        covid_pt_var$confirmados_var_alentejo, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
plot(Rt_nonparam_si4, legend = FALSE)

## Posterior sample Rt estimate
sample_windows <- seq(length(Rt_nonparam_si4$R$t_start))

posterior_R_t4 <- 
    map(
        .x = sample_windows,
        .f = function(x) {
            
            posterior_sample_obj4 <- 
                sample_posterior_R(
                    R = Rt_nonparam_si4,
                    n = 1000, 
                    window = x
                )
            
            posterior_sample_estim4 <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si4$R$t_start[x],
                    window_t_end = Rt_nonparam_si4$R$t_end[x],
                    date_point = covid19pt_var[covid_pt_var$t_start == Rt_nonparam_si4$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj4),
                    R_e_q0025 = quantile(posterior_sample_obj4, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj4, probs = 0.975)
                )
            
            return(posterior_sample_estim4)
            
        }
    ) %>% 
    reduce(bind_rows)

posterior_R_e4 <- posterior_R_t4 %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))


## Gráfico Rt Diário
highchart() %>%
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(text = "Número Reprodutivo Rt ARS Alentejo - número médio de casos secundários por nova infecção (janela temporal de 7 dias)") %>% 
    hc_subtitle(text = "Fonte: Autores baseados nos dados da DGS") %>% 
    hc_xAxis(categories = format(posterior_R_e4$date_point, "%b %d"),
             tickmarkPlacement = "on",
             title = list(enabled = FALSE)) %>% 
    hc_yAxis(title = list(text = "Rt"),min = 0, 
             plotLines = list(
                 list(label = list(text = "Rt = 1"),
                      color = "#525252",
                      width = 2,
                      value = 1,
                      dashStyle = "shortdash"))) %>% 
    
    hc_add_series(posterior_R_e4, 
                  hcaes( low = lwr, high = upr),     
                  #                id = "ForecastRange-FL", 
                  type = "arearange", 
                  name = "Incerteza", 
                  color = "#d9d9d9") %>% 
    hc_add_series(data = posterior_R_e4$fit,
                  name = "Rt", 
                  color = "#e6550d")

 
 # Rt ARS Algarve

Rt_nonparam_si5 <- 
    estimate_R(
        covid_pt_var$confirmados_var_algarve, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
plot(Rt_nonparam_si5, legend = FALSE)

## Posterior sample Rt estimate
sample_windows <- seq(length(Rt_nonparam_si5$R$t_start))

posterior_R_t5 <- 
    map(
        .x = sample_windows,
        .f = function(x) {
            
            posterior_sample_obj5 <- 
                sample_posterior_R(
                    R = Rt_nonparam_si5,
                    n = 1000, 
                    window = x
                )
            
            posterior_sample_estim5 <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si5$R$t_start[x],
                    window_t_end = Rt_nonparam_si5$R$t_end[x],
                    date_point = covid_pt_var[covid_pt_var$t_start == Rt_nonparam_si5$R$t_end[x], "Data"],
                    R_e_median = median(posterior_sample_obj5),
                    R_e_q0025 = quantile(posterior_sample_obj5, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj5, probs = 0.975)
                )
            
            return(posterior_sample_estim5)
            
        }
    ) %>% 
    reduce(bind_rows)


posterior_R_e5 <- posterior_R_t5 %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))

## Gráfico Rt Diário
highchart() %>%
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(text = "Número Reprodutivo Rt ARS Algarve - número médio de casos secundários por nova infecção (janela temporal de 7 dias)") %>% 
    hc_subtitle(text = "Fonte: Autores baseados nos dados da DGS") %>% 
    hc_xAxis(categories = format(posterior_R_e5$date_point, "%b %d"),
             tickmarkPlacement = "on",
             title = list(enabled = FALSE)) %>% 
    hc_yAxis(title = list(text = "Rt"),min = 0, 
             plotLines = list(
                 list(label = list(text = "Rt = 1"),
                      color = "#525252",
                      width = 2,
                      value = 1,
                      dashStyle = "shortdash"))) %>% 
    
    hc_add_series(posterior_R_e5, 
                  hcaes( low = lwr, high = upr),     
                  #                id = "ForecastRange-FL", 
                  type = "arearange", 
                  name = "Incerteza", 
                  color = "#d9d9d9") %>% 
    hc_add_series(data = posterior_R_e5$fit,
                  name = "Rt", 
                  color = "#e6550d")


# Rt ARS Açores

Rt_nonparam_si6 <- 
    estimate_R(
        covid_pt_var$confirmados_var_acores, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
plot(Rt_nonparam_si6, legend = FALSE)

## Posterior sample Rt estimate
sample_windows <- seq(length(Rt_nonparam_si6$R$t_start))

posterior_R_t <- 
    map(
        .x = sample_windows,
        .f = function(x) {
            
            posterior_sample_obj6 <- 
                sample_posterior_R(
                    R = Rt_nonparam_si6,
                    n = 1000, 
                    window = x
                )
            
            posterior_sample_estim6 <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si6$R$t_start[x],
                    window_t_end = Rt_nonparam_si6$R$t_end[x],
                    date_point = covid_pt_var[covid_pt_var$t_start == Rt_nonparam_si6$R$t_end[x], "Data"],
                    R_e_median = median(posterior_sample_obj6),
                    R_e_q0025 = quantile(posterior_sample_obj6, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj6, probs = 0.975)
                )
            
            return(posterior_sample_estim6)
            
        }
    ) %>% 
    reduce(bind_rows)


posterior_R_e6 <- posterior_R_t6 %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))

## Gráfico Rt Diário
highchart() %>%
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(text = "Número Reprodutivo Rt  Acores - número médio de casos secundários por nova infecção (janela temporal de 7 dias)") %>% 
    hc_subtitle(text = "Fonte: Autores baseados nos dados da DGS") %>% 
    hc_xAxis(categories = format(posterior_R_e6$date_point, "%b %d"),
             tickmarkPlacement = "on",
             title = list(enabled = FALSE)) %>% 
    hc_yAxis(title = list(text = "Rt"),min = 0, 
             plotLines = list(
                 list(label = list(text = "Rt = 1"),
                      color = "#525252",
                      width = 2,
                      value = 1,
                      dashStyle = "shortdash"))) %>% 
    
    hc_add_series(posterior_R_e6, 
                  hcaes( low = lwr, high = upr),     
                  #                id = "ForecastRange-FL", 
                  type = "arearange", 
                  name = "Incerteza", 
                  color = "#d9d9d9") %>% 
    hc_add_series(data = posterior_R_e6$fit,
                  name = "Rt", 
                  color = "#e6550d")


# Rt ARS Madeira

Rt_nonparam_si7 <- 
    estimate_R(
        covid_pt_var$confirmados_var_madeira, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
plot(Rt_nonparam_si7, legend = FALSE)

## Posterior sample Rt estimate
sample_windows <- seq(length(Rt_nonparam_si7$R$t_start))

posterior_R_t7 <- 
    map(
        .x = sample_windows,
        .f = function(x) {
            
            posterior_sample_obj7 <- 
                sample_posterior_R(
                    R = Rt_nonparam_si7,
                    n = 1000, 
                    window = x
                )
            
            posterior_sample_estim7 <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si7$R$t_start[x],
                    window_t_end = Rt_nonparam_si7$R$t_end[x],
                    date_point = covid_pt_var[covid_pt_var$t_start == Rt_nonparam_si7$R$t_end[x], "Data"],
                    R_e_median = median(posterior_sample_obj7),
                    R_e_q0025 = quantile(posterior_sample_obj7, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj7, probs = 0.975)
                )
            
            return(posterior_sample_estim7)
            
        }
    ) %>% 
    reduce(bind_rows)


posterior_R_e7 <- posterior_R_t7 %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))

highchart() %>%
    hc_add_theme(hc_theme_smpl()) %>% 
    hc_title(text = "Número Reprodutivo Rt Madeira - número médio de casos secundários por nova infecção (janela temporal de 7 dias)") %>% 
    hc_subtitle(text = "Fonte: Autores baseados nos dados da DGS") %>% 
    hc_xAxis(categories = format(posterior_R_e7$date_point, "%b %d"),
             tickmarkPlacement = "on",
             title = list(enabled = FALSE)) %>% 
    hc_yAxis(title = list(text = "Rt"),min = 0, 
             plotLines = list(
                 list(label = list(text = "Rt = 1"),
                      color = "#525252",
                      width = 2,
                      value = 1,
                      dashStyle = "shortdash"))) %>% 
    
    hc_add_series(posterior_R_e7, 
                  hcaes( low = lwr, high = upr),     
                  #                id = "ForecastRange-FL", 
                  type = "arearange", 
                  name = "Incerteza", 
                  color = "#d9d9d9") %>% 
    hc_add_series(data = posterior_R_e7$fit,
                  name = "Rt", 
                  color = "#e6550d")



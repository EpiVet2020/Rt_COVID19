#Libraries

library(testthat)
library(rlang)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(data.table)
library(ggpubr)
library(devtools)
install_github("holtzy/epuRate")
library(epuRate)
library(EpiEstim)
library(tidyr)
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
library(RColorBrewer)
library()


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
covid_pt_var <- covid19pt_var  %>%
    filter(covid19pt_var$data > as.Date("2020-02-28")) %>%       
    dplyr::mutate(t_start = dplyr::row_number())


### Cálculo do Rt - Uncertainty method --> "uncertain_si"
### Serial Interval (By André Peralta)
### data from https://cmmid.github.io/topics/covid19/current-patterns-transmission/global-time-varying-transmission.html

# A estimativa do nº reprodutivo efetivo diário foi realizada segundo uma janela de 7 dias
# Recorremos ao EpiEstim [4,5], ajustado aos casos importados e assumindo o seguinte serial interval (método "uncertain"):
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

# Map function - applies a function to each element of a list and returns an object of the same type as the input
posterior_R_t <- 
    map(.x = sample_windows,
        .f = function(x) {
           
             ## Sample from the posterior R distribution
            posterior_sample_obj <- 
                sample_posterior_R(
                    R = Rt_nonparam_si,
                    n = 1000, 
                    window = x )
            
            ## Returns a data frame
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
    ## Combines the elements into a single value 
    reduce(bind_rows)



##GRÁFICO highchart
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

## GRÁFICO ggplot
graph_PT<- ggplot(posterior_R_t, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "palegreen4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
    
    labs(title = " Evolução do Número Efetivo Reprodutivo ao longo do tempo",
          subtitle = "Fonte de dados: DGS ",
          x = "Tempo",
          y = "Nº de reprodução efetivo (Rt)"
    ) +
    
    theme_minimal() +
   
     theme(axis.title = element_text(size = 10, hjust =0.5),
          plot.subtitle = element_text(size= 8),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          ) +
    
    scale_x_date(
        date_breaks = "1 month",
        limits = c(min(covid_pt_var$data), max(posterior_R_t$date_point))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_R_t$R_e_q0975)),
        limits = c(0, NA)
    ) +

    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4) 
 

### Tornar gráfico interativo
ggplotly(graph_PT) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))


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
sample_windows1 <- seq(length(Rt_nonparam_si1$R$t_start))

posterior_R_t1 <- 
    map(
        .x = sample_windows1,
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

## GRÁFICO GGPLOT

graph_PT1<- ggplot(posterior_R_t1, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "palegreen4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
    
    labs( title = " ARS Norte - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados: DGS ",
          x = "Tempo",
          y = "Nº de reprodução efetivo (Rt)"
    ) +
    
    theme_minimal() +
    
    theme(axis.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size= 8),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
    ) +
    
    scale_x_date(
        date_breaks = "1 month",
        limits = c(min(covid_pt_var$data), max(posterior_R_t1$date_point))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_R_t1$R_e_q0975)),
        limits = c(0, NA)
    ) +
    
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4) 


### Tornar gráfico interativo
ggplotly(graph_PT1) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))


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
sample_windows2 <- seq(length(Rt_nonparam_si2$R$t_start))

posterior_R_t2 <- 
    map(
        .x = sample_windows2,
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

## GGPLOT
## GRÁFICO GGPLOT

graph_PT2<- ggplot(posterior_R_t2, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "palegreen4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
    
    labs( title = " ARS Centro - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados: DGS ",
          x = "Tempo",
          y = "Nº de reprodução efetivo (Rt)"
    ) +
    
    theme_minimal() +
    
    theme(axis.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size= 8),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
    ) +
    
    scale_x_date(
        date_breaks = "1 month",
        limits = c(min(covid_pt_var$data), max(posterior_R_t2$date_point))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_R_t2$R_e_q0975)),
        limits = c(0, NA)
    ) +
    
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4) 


### Tornar gráfico interativo
ggplotly(graph_PT2) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

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
sample_windows3 <- seq(length(Rt_nonparam_si3$R$t_start))

posterior_R_t3 <- 
    map(
        .x = sample_windows3,
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
                  #id = "ForecastRange-FL", 
                  type = "arearange", 
                  name = "Incerteza", 
                  color = "#d9d9d9") %>% 
    hc_add_series(data = posterior_R_e3$fit,
                  name = "Rt", 
                  color = "#e6550d")
## GGPLOT

## GRÁFICO GGPLOT

graph_PT3<- ggplot(posterior_R_t3, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "palegreen4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
    
    labs( title = " ARS LVT - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados: DGS ",
          x = "Tempo",
          y = "Nº de reprodução efetivo (Rt)"
    ) +
    
    theme_minimal() +
    
    theme(axis.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size= 8),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
    ) +
    
    scale_x_date(
        date_breaks = "1 month",
        limits = c(min(covid_pt_var$data), max(posterior_R_t3$date_point))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_R_t3$R_e_q0975)),
        limits = c(0, NA)
    ) +
    
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4) 


### Tornar gráfico interativo
ggplotly(graph_PT3) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

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
sample_windows4 <- seq(length(Rt_nonparam_si4$R$t_start))

posterior_R_t4 <- 
    map(
        .x = sample_windows4,
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

 ## GGPLOT

## GRÁFICO GGPLOT

graph_PT4<- ggplot(posterior_R_t4, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "palegreen4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
    
        labs( title = " ARS Alentejo - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados: DGS ",
          x = "Tempo",
          y = "Nº de reprodução efetivo (Rt)"
    ) +
    
    theme_minimal() +
    
    theme(axis.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size= 8),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
    ) +
    
    scale_x_date(
        date_breaks = "1 month",
        limits = c(min(covid_pt_var$data), max(posterior_R_t4$date_point))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_R_t4$R_e_q0975)),
        limits = c(0, NA)
    ) +
    
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4) 


### Tornar gráfico interativo
ggplotly(graph_PT4) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))


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
sample_windows5 <- seq(length(Rt_nonparam_si5$R$t_start))

posterior_R_t5 <- 
    map(
        .x = sample_windows5,
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
                    date_point = covid19pt_var[covid_pt_var$t_start == Rt_nonparam_si5$R$t_end[x], "data"],
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
                  # id = "ForecastRange-FL", 
                  type = "arearange", 
                  name = "Incerteza", 
                  color = "#d9d9d9") %>% 
    hc_add_series(data = posterior_R_e5$fit,
                  name = "Rt", 
                  color = "#e6550d")

## GGPLOT
## GRÁFICO GGPLOT

graph_PT5<- ggplot(posterior_R_t5, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "palegreen4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
    
    labs( title = " ARS Algarve - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados: DGS ",
          x = "Tempo",
          y = "Nº de reprodução efetivo (Rt)"
    ) +
    
    theme_minimal() +
    
    theme(axis.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size= 8),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
    ) +
    
    scale_x_date(
        date_breaks = "1 month",
        limits = c(min(covid_pt_var$data), max(posterior_R_t5$date_point))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_R_t5$R_e_q0975)),
        limits = c(0, NA)
    ) +
    
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4) 


### Tornar gráfico interativo
ggplotly(graph_PT5) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))


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
sample_windows6 <- seq(length(Rt_nonparam_si6$R$t_start))

posterior_R_t6 <- 
    map(
        .x = sample_windows6,
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
                    date_point = covid_pt_var[covid_pt_var$t_start == Rt_nonparam_si6$R$t_end[x], "data"],
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

## GGPLOT
## GRÁFICO GGPLOT

graph_PT6<- ggplot(posterior_R_t6, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "palegreen4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
    
    labs( title = " ARS Norte - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados: DGS ",
          x = "Tempo",
          y = "Nº de reprodução efetivo (Rt)"
    ) +
    
    theme_minimal() +
    
    theme(axis.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size= 8),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
    ) +
    
    scale_x_date(
        date_breaks = "1 month",
        limits = c(min(covid_pt_var$data), max(posterior_R_t6$date_point))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_R_t6$R_e_q0975)),
        limits = c(0, NA)
    ) +
    
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4) 


### Tornar gráfico interativo
ggplotly(graph_PT6) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))



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
sample_windows7 <- seq(length(Rt_nonparam_si7$R$t_start))

posterior_R_t7 <- 
    map(
        .x = sample_windows7,
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
                    date_point = covid_pt_var[covid_pt_var$t_start == Rt_nonparam_si7$R$t_end[x], "data"],
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
                  # id = "ForecastRange-FL", 
                  type = "arearange", 
                  name = "Incerteza", 
                  color = "#d9d9d9") %>% 
    hc_add_series(data = posterior_R_e7$fit,
                  name = "Rt", 
                  color = "#e6550d")

## GGPLOT
## GRÁFICO GGPLOT

graph_PT7<- ggplot(posterior_R_t7, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "palegreen4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
    
    labs( title = " ARS Norte - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados: DGS ",
          x = "Tempo",
          y = "Nº de reprodução efetivo (Rt)"
    ) +
    
    theme_minimal() +
    
    theme(axis.title = element_text(size = 10, hjust = 0.5),
          plot.subtitle = element_text(size= 8),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
    ) +
    
    scale_x_date(
        date_breaks = "1 month",
        limits = c(min(covid_pt_var$data), max(posterior_R_t7$date_point))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_R_t7$R_e_q0975)),
        limits = c(0, NA)
    ) +
    
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4) 


### Tornar gráfico interativo
ggplotly(graph_PT7) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))


# Método Paramétrico
## Cálculo do Rt ou Re

confirmados_var <- as.data.frame(covid_pt_var$confirmados_var)
confirmados_var <-cbind(covid_pt_var$data, confirmados_var)
names(confirmados_var)[1]<-"Data"
names(confirmados_var)[2]<-"I"

res_parametric_si <- 
    estimate_R(
        confirmados_var, 
        method ="parametric_si",
        config = make_config(
            list(
                mean_si = 4.7, 
                std_si = 2.9
            )
        )
    )

plot(res_parametric_si, legend = FALSE)

r_prt <- as.data.frame(res_parametric_si$R)

r_prt <- left_join(covid_pt_var, r_prt, by="t_start")

### join by t-end
left_join(
    x = covid_pt_var, 
    y = dplyr::select(
        r_prt,
        c("t_end", "Mean(R)", "Quantile.0.025(R)", "Quantile.0.975(R)")
    ), 
    by = c("t_start" = "t_end")
)

r_prt %>% 
    rename(
        r_efect = "Mean(R)",
        r_low = "Quantile.0.025(R)",
        r_high = "Quantile.0.975(R)"
    )


r_efect <- r_prt$`Mean(R)`
r_low <- r_prt$`Quantile.0.025(R)`
r_high <- r_prt$`Quantile.0.975(R)`

ggplot() +
    
    geom_line(
        data = r_prt,                 
        aes(
            x = data,               
            y = r_efect
        ),
        alpha = 0.7,
        size = 1
    ) +
    
    geom_hline(
        yintercept=1, 
        linetype="dashed", 
        color = "black"
    ) +
    
    geom_hline(
        yintercept=0, 
        color = "black"
    ) +
    
    geom_ribbon(
        data = r_prt, 
        aes(
            ymin = r_low, 
            ymax = r_high,
            x = data
        ), 
        alpha=0.5,
        fill = "grey70"
    ) +
    
    scale_x_date(
        breaks = "2 day", 
        date_labels = "%b %d"
    ) +
    
    labs(
        title = "COVID-19 Effective reproduction",
        subtitle = "Portugal",
        y = "Effective reproduction n",
        x = "", 
        caption = "Fonte: Dados da DGS |Modelo dos autores"
    ) +
    
    theme_minimal() +
    
    theme(
        panel.grid.minor = element_blank(),
        # panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(face = "bold", size = 8, color = "black"),
        axis.title = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
    )




r estim-Re-uncertain-si-imported}
### the model can account for imported cases at the beginning of the outbreak
### perhaps consider the first 2 ??? or 4 on the first two days
### if so, it will most likely decrease the initial R_e but increase it afterwards due to undiagnosed community transmission




{r log-linear-growth}
covid_r_inc <- 
    rep(
        x = unlist(covid_pt_var$data), 
        times = unlist(covid_pt_var$confirmados_var)
    ) %>% 
    incidence(
        dates = .,
        interval = "1 day",
        standard = TRUE,
        first_date = min(covid_pt_var$data),
        last_date = max(covid_pt_var$data)
    )

### find peak for adjustment of trend on model by the split argument    
covid_r_inc_peak <- find_peak(covid_r_inc)

### fit log-linear model
### fits two exponential models to incidence data, 
### of the form: log(y) = r * t + b , where
### 'y' is the incidence,
### 't' is time (in days)
### 'r' is the growth rate
### 'b' is the origin
### function fit will fit one model by default, 
### but will fit two models on either side of a splitting date 
### (typically the peak of the epidemic) if the argument split is provided
covid_r_inc_model <- 
    fit(
        x = covid_r_inc,
        # split = covid_r_inc_peak,
        NULL
    )

# check object entirely
covid_r_inc_model

# (daily growth rate)
covid_r_inc_model$info$r
covid_r_inc_model$info$r.conf

# (doubling time in days)
covid_r_inc_model$info$doubling
covid_r_inc_model$info$doubling.conf

# incidence predictions (fitted vs observed data)
plot(covid_r_inc, fit = covid_r_inc_model)
```


```{r 3-day-prediction-log-linear-growth}
### predict number cases next 3 days maintaing current exponential growth
### model elements for forecast are in covid_r_inc_model$model
### structure of dataset for prediction can be checked with
# head(covid_r_inc_model$info$pred)
### must provide x-axis data as a mid-point from t_0
### create x vector for forecasting on the next 3 days (reasonable amount time)
case_pred_3_day <- 
    data.frame(
        dates = covid_r_inc_model$info$pred$dates[nrow(covid_r_inc_model$info$pred)] + 1:3,
        dates.x = covid_r_inc_model$info$pred$dates.x[nrow(covid_r_inc_model$info$pred)] + 1:3
    )

n_case_pred_3_day <- 
    predict(
        object = covid_r_inc_model$model, 
        newdata = case_pred_3_day, 
        se.fit = TRUE, 
        # type = "response",
        interval = "prediction"
    )

### log-linear model
### predictions are in log scale
### anti-log to get final count predictions
n_case_pred_3_day <- 
    exp(x = n_case_pred_3_day[["fit"]])

case_pred_3_day <- 
    dplyr::bind_cols(
        case_pred_3_day,
        as.data.frame(n_case_pred_3_day)
    ) %>% 
    mutate(
        type = "predict"
    )

case_obs_fit <- covid_r_inc_model$info$pred %>% 
    mutate(type = "fit")

### final prediction
covid_pred_3_day <- 
    bind_rows(
        case_obs_fit,
        case_pred_3_day
    )

### plot not perfect
### points and lines not coinciding on the x axis with the geom_col
plot_pred_3_day <- 
    ggplot() +
    # geom_col(
    #     data = covid_r, 
    #     mapping = aes(x = Data, y = Confirmados_var), 
    #     fill = "grey90"
    #     ) + 
    geom_point(
        data = covid_pred_3_day, 
        mapping = aes(x = dates, y = fit, colour = type), 
        alpha = 0.7, 
        size = 1.5
    ) +
    geom_line(
        data = covid_pred_3_day, 
        mapping = aes(x = dates, y = fit, colour = type), 
        alpha = 0.7, 
        size = 1.5
    ) + 
    geom_ribbon(
        data = covid_pred_3_day, 
        mapping = aes(x = dates, ymin = lwr, ymax = upr, fill = type), 
        alpha = 0.25
    ) +
    scale_x_date(breaks = "1 day") + 
    scale_y_continuous(
        limits = c(0, max(covid_pred_3_day$upr)), 
        breaks = pretty(covid_pred_3_day$upr),
        labels = pretty(covid_pred_3_day$upr), 
        name = "Medida"
    ) +
    theme_classic() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(
        y = "Casos (n)", 
        x = "Data (dias)", 
        fill = "Medida"
    )

### display plot
plot_pred_3_day

```







```{r lambda-overall-infectivity}
### computes the overall infectivity (lambda) due to previously infected individuals
### λ_t = ∑_{k=1}^{t-1}I_{t-k}w_k 
lambda_covid_pt <- 
    overall_infectivity(
        incid = data.frame(I = covid_r_inc$counts), 
        si_distr = discr_si(k = c(100, 1:100), mu = 4.7, sigma = 2.9)
    )

plot(lambda_covid_pt)

```

```{r}
covid_pt %>% 
    mutate(week = (year(Order_Date) - year(min(Order_Date)))*52 + 
               week(Order_Date) - week(min(Order_Date)),
           week2 = (as.numeric(Order_Date) %/% 7) - (as.numeric(min(Order_Date))
                                                     %/% 7)) %>%
    arrange(Order_Date)





covid_r<-covid_pt  %>%
    group_by(epiweek) %>%
    summarise(
        incidence=sum(Confirmados_var)
    ) %>%
    filter(
        epiweek>6
    )

covid_r<-covid_pt  %>%
    select(
        Data,Confirmados_var
    )  %>%
    filter(
        Data>as.Date("2020-02-28")
    ) %>%
    dplyr::mutate(
        t_start = dplyr::row_number() %>% as.numeric(),
        t_end = t_start + 6
    )

```



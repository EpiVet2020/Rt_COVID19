#Libraries
library(testthat)
library(rlang)
library(dplyr)
library(ggplot2)
library(ggpubr)
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
library(purrr)
library(magrittr)
library(RColorBrewer)
library(rjson)
library(readr)
library(readxl)

setwd("~/Desktop/Treino Estágio 2020-2021/Rt Project")

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
### A estimativa do nº reprodutivo efetivo diário foi realizada segundo uma janela de 7dias;
### Recorremos ao EpiEstim, ajustado aos casos importados e assumindo o seguinte serial interval (método uncertain):
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

# Map function: applies a function to each element of a list and returns an object of the same type as the input 
# Criar um data frame com valores de R
posterior_R_t <- 
    map(.x = sample_windows,
        .f = function(x) {
            ## Sample from  the posterior R distribution
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
    ##Combines elements into a single value
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
PT <- ggplotly(graph_PT) %>%
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

graph_Norte<- ggplot(posterior_R_t1, aes(x = date_point, y = R_e_median)) +
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
Norte <- ggplotly(graph_Norte) %>%
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


## Gráfico Rt diário
posterior_R_e2 <- posterior_R_t2 %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))


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

## GRÁFICO GGPLOT
graph_Centro <- ggplot(posterior_R_t2, aes(x = date_point, y = R_e_median)) +
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
Centro <- ggplotly(graph_Centro) %>%
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


## Gráfico Rt Diário
posterior_R_e3 <- posterior_R_t3 %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))

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

## GRÁFICO GGPLOT

graph_LVT<- ggplot(posterior_R_t3, aes(x = date_point, y = R_e_median)) +
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
LVT <- ggplotly(graph_LVT) %>%
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


## Gráfico Rt Diário
posterior_R_e4 <- posterior_R_t4 %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))

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

## GRÁFICO GGPLOT

graph_Alentejo <- ggplot(posterior_R_t4, aes(x = date_point, y = R_e_median)) +
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
Alentejo <- ggplotly(graph_Alentejo) %>%
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


## Gráfico Rt Diário
posterior_R_e5 <- posterior_R_t5 %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))

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


## GRÁFICO GGPLOT

graph_Algarve<- ggplot(posterior_R_t5, aes(x = date_point, y = R_e_median)) +
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
Algarve <- ggplotly(graph_Algarve) %>%
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


## Gráfico Rt Diário
posterior_R_e6 <- posterior_R_t6 %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))

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

## GRÁFICO GGPLOT

graph_Açores <- ggplot(posterior_R_t6, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "palegreen4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
    
    labs( title = " ARS Açores - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
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
Açores <- ggplotly(graph_Açores) %>%
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


## Gráfico Rt diário
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


## GRÁFICO GGPLOT

graph_Madeira <- ggplot(posterior_R_t7, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "palegreen4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
    
    labs( title = " ARS Madeira - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
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
Madeira <- ggplotly(graph_Madeira) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))


ggarrange(PT, Norte, Centro, LVT, Alentejo, Algarve, Açores, Madeira,
          labels = "Portugal", "Norte", "Lisboa e Vale do Tejo", "Alentejo", "Algarve", "Açores", "Madeira",
          ncol = 4, nrow = 2)








#Data OUTROS PAÍSES e transformação para formato de data

# ITÁLIA (https://github.com/pcm-dpc/COVID-19/blob/master/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv)
italy <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/legacy/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv", stringsAsFactors = FALSE)

## Remover horas e minutos
italy$data <- strftime(italy$data, format = "%Y-%m-%d")

## Alterar para formato de data
italy$data <- as.Date(italy$data,  "%Y-%m-%d")

## Tabela: Data e confirmados novos
it_var <- italy %>%
    select(data, nuovi_positivi)
names(it_var) <- c("data", "confirmados_novos")


## Previsão da evolução
covid_it_var <- it_var  %>%
    filter(it_var$data > as.Date("2020-02-28")) %>%  
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Itália- Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_it <- estimate_R(as.numeric(covid_it_var$confirmados_novos), 
                                method = "uncertain_si",
                                config = sens_configs
)

sample_windows_it <- seq(length(Rt_nonparam_si_it$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_it <- 
    map(.x = sample_windows_it,
        .f = function(x) {
            
            posterior_sample_obj_it <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_it,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_it <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_it$R$t_start[x],
                    window_t_end = Rt_nonparam_si_it$R$t_end[x],
                    date_point = covid_it_var[covid_it_var$t_start == Rt_nonparam_si_it$R$t_end[x], "Date"],
                    R_e_median = median(posterior_sample_obj_it),
                    R_e_q0025 = quantile(posterior_sample_obj_it, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_it, probs = 0.975))
            
            return(posterior_sample_estim_it)}
    ) %>% 
    
    reduce(bind_rows)


## Gráfico Itália ggplot

graph_it<- ggplot(posterior_Rt_it, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "indianred",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "indianred3") +
    
    labs( title = "Itália - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_it_var$Date), max((posterior_Rt_it$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_it$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)


### Tornar gráfico interativo
ggplotly(graph_it) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))



# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = 6.75, std_mean_si = 3.76,
            min_mean_si = 5.98, max_mean_si = 13.17,
            std_si = , std_std_si = ,
            min_std_si = , max_std_si = ,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )







# ALEMANHA (https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/data)
germany <- fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
germany <- germany$features

## Remover horas e minutos
germany$properties$Meldedatum <- strftime(germany$properties$Meldedatum, format = "%Y-%m-%d")
## Alterar para formato de data
germany$properties$Meldedatum <- as.Date(germany$properties$Meldedatum, format = "%Y-%m-%d")

### Ordenar por data
germany <- as.data.frame(germany[order(germany$properties$Meldedatum), ])

### Nº de registos por dia (agregar os confirmados )
ger_var <- as.data.frame(aggregate(x = germany, list(Data = germany$properties$Meldedatum), FUN = length))
ger_var <- ger_var[,1:2]
names(ger_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_ger_var <- ger_var  %>%
    filter(ger_var$Data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Alemanha - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_ger <- estimate_R(as.numeric(covid_ger_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_ger <- seq(length(Rt_nonparam_si_ger$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_ger <- 
    map(.x = sample_windows_ger,
        .f = function(x) {
            
            posterior_sample_obj_ger <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_ger,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_ger <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_ger$R$t_start[x],
                    window_t_end = Rt_nonparam_si_ger$R$t_end[x],
                    date_point = covid_ger_var[covid_ger_var$t_start == Rt_nonparam_si_ger$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_ger),
                    R_e_q0025 = quantile(posterior_sample_obj_ger, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_ger, probs = 0.975))
            
            return(posterior_sample_estim_ger)}
    ) %>% 
    
    reduce(bind_rows)


## Gráfico Alemanha ggplot

graph_ger<- ggplot(posterior_Rt_ger, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "goldenrod",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "goldenrod1") +
    
    labs( title = "Alemanha - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_ger_var$data), max((posterior_Rt_ger$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_ger$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)


### Tornar gráfico interativo
ggplotly(graph_ger) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = 6.75, std_mean_si = 3.76,
            min_mean_si = 5.98, max_mean_si = 13.17,
            std_si = 2.9, std_std_si = 0.5,
            min_std_si = 1.9, max_std_si = 4.9,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )







# ESPANHA (https://cnecovid.isciii.es/covid19/#documentaci%C3%B3n-y-datos)
spain <- read.csv("https://cnecovid.isciii.es/covid19/resources/datos_ccaas.csv")

## Alterar para formato Data
spain$fecha <- as.Date(spain$fecha, "%Y-%m-%d")

## Tabela confirmados novos (somar registos por dia (juntar regiões))
spa_var <- as.data.frame(aggregate(spain$num_casos, by = list(spain$fecha), FUN = sum))
names(spa_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_spa_var <- spa_var  %>%
    filter(spa_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Espanha - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_spa <- estimate_R(as.numeric(covid_spa_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_spa <- seq(length(Rt_nonparam_si_spa$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_spa <- 
    map(.x = sample_windows_spa,
        .f = function(x) {
            
            posterior_sample_obj_spa <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_spa,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_spa <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_spa$R$t_start[x],
                    window_t_end = Rt_nonparam_si_spa$R$t_end[x],
                    date_point = covid_spa_var[covid_spa_var$t_start == Rt_nonparam_si_spa$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_spa),
                    R_e_q0025 = quantile(posterior_sample_obj_spa, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_spa, probs = 0.975))
            
            return(posterior_sample_estim_spa)}
    ) %>% 
    
    reduce(bind_rows)


## Gráfico Espanha ggplot

graph_spa<- ggplot(posterior_Rt_spa, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "thistle3",  alpha = 0.8, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.3, fill = "thistle2") +
    
    labs( title = "Espanha - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_spa_var$data), max((posterior_Rt_spa$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_spa$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)


### Tornar gráfico interativo
ggplotly(graph_spa) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = 6.75, std_mean_si = 3.76,
            min_mean_si = 5.98, max_mean_si = 13.17,
            std_si = 2.9, std_std_si = 0.5,
            min_std_si = 1.9, max_std_si = 4.9,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )









# BÉLGICA (https://epistat.wiv-isp.be/covid/)
belgium <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")

## Alterar para formato de data
belgium$DATE <- as.Date(belgium$DATE, "%Y-%m-%d")

## Tabela de confirmados novos - Soma dos registos diários (juntar regiões)
bel_var <- as.data.frame(aggregate(belgium$CASES, by = list(Data = belgium$DATE), FUN = sum))
names(bel_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_bel_var <- bel_var  %>%
    # Neste caso não se filtrou > 28-02-2020 , uma vez que só reportaram a partir de Março.
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Bélgica - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_bel <- estimate_R(as.numeric(covid_bel_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_bel <- seq(length(Rt_nonparam_si_bel$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_bel <- 
    map(.x = sample_windows_bel,
        .f = function(x) {
            
            posterior_sample_obj_bel <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_bel,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_bel <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_bel$R$t_start[x],
                    window_t_end = Rt_nonparam_si_bel$R$t_end[x],
                    date_point = covid_bel_var[covid_bel_var$t_start == Rt_nonparam_si_bel$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_bel),
                    R_e_q0025 = quantile(posterior_sample_obj_bel, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_bel, probs = 0.975))
            
            return(posterior_sample_estim_bel)}
    ) %>% 
    
    reduce(bind_rows)


## Gráfico Bélgica ggplot

graph_bel<- ggplot(posterior_Rt_bel, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "royalblue2",  alpha = 0.65, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "royalblue1") +
    
    labs( title = "Bélgica - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_bel_var$data), max((posterior_Rt_bel$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_bel$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)


### Tornar gráfico interativo
ggplotly(graph_bel) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = 6.75, std_mean_si = 3.76,
            min_mean_si = 5.98, max_mean_si = 13.17,
            std_si = 2.9, std_std_si = 0.5,
            min_std_si = 1.9, max_std_si = 4.9,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )









# REPÚBLICA CHECA (https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19)
czechr <- read.csv("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.csv")

## Alterar formato para data
czechr$datum <- as.Date(czechr$datum, "%Y-%m-%d")

## Tabela confirmados novos
cz_var <- czechr %>%
    select(datum, prirustkovy_pocet_nakazenych)
names(cz_var) <- c("data", "confirmados_novos")


## Previsão da evolução
covid_cz_var <- cz_var  %>%
    filter(cz_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Rép. Checa - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_cz <- estimate_R(as.numeric(covid_cz_var$confirmados_novos), 
                                method = "uncertain_si",
                                config = sens_configs
)

sample_windows_cz <- seq(length(Rt_nonparam_si_cz$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_cz <- 
    map(.x = sample_windows_cz,
        .f = function(x) {
            
            posterior_sample_obj_cz <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_cz,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_cz <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_cz$R$t_start[x],
                    window_t_end = Rt_nonparam_si_cz$R$t_end[x],
                    date_point = covid_cz_var[covid_cz_var$t_start == Rt_nonparam_si_cz$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_cz),
                    R_e_q0025 = quantile(posterior_sample_obj_cz, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_cz, probs = 0.975))
            
            return(posterior_sample_estim_cz)}
    ) %>% 
    
    reduce(bind_rows)


## Gráfico Républica Checa ggplot

graph_cz <- ggplot(posterior_Rt_cz, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "steelblue3",  alpha = 0.65, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "steelblue1") +
    
    labs( title = "Républica Checa - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_cz_var$data), max((posterior_Rt_cz$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_cz$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)


### Tornar gráfico interativo
ggplotly(graph_cz) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = , std_mean_si = ,
            min_mean_si = , max_mean_si = ,
            std_si = , std_std_si = ,
            min_std_si = , max_std_si = ,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )








# SUIÇA (https://www.bag.admin.ch/bag/en/home/krankheiten/ausbrueche-epidemien-pandemien/aktuelle-ausbrueche-epidemien/novel-cov/situation-schweiz-und-international.html#-640157857)
switzerland <- rio::import(file ="https://www.bag.admin.ch/dam/bag/en/dokumente/mt/k-und-i/aktuelle-ausbrueche-pandemien/2019-nCoV/covid-19-basisdaten-labortests.xlsx.download.xlsx/Dashboard_3_COVID19_labtests_positivity.xlsx")

## Alterar formato para Data
switzerland$Datum <- as.Date(switzerland$Datum, "%Y-%m-%d")

# Selecionar casos positivos diários e criar tabela com a data
swi_var <- switzerland %>%
    filter(Outcome_tests == "Positive") %>%
    select(Datum, Number_of_tests)
names(swi_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_swi_var <- swi_var  %>%
    filter(swi_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Rép. Checa - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_swi <- estimate_R(as.numeric(covid_swi_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_swi <- seq(length(Rt_nonparam_si_swi$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_swi <- 
    map(.x = sample_windows_swi,
        .f = function(x) {
            
            posterior_sample_obj_swi <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_swi,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_swi <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_swi$R$t_start[x],
                    window_t_end = Rt_nonparam_si_swi$R$t_end[x],
                    date_point = covid_swi_var[covid_swi_var$t_start == Rt_nonparam_si_swi$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_swi),
                    R_e_q0025 = quantile(posterior_sample_obj_swi, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_swi, probs = 0.975))
            
            return(posterior_sample_estim_swi)}
    ) %>% 
    
    reduce(bind_rows)


## Gráfico Suíça ggplot

graph_swi<- ggplot(posterior_Rt_swi, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "antiquewhite4",  alpha = 0.65, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.3, fill = "antiquewhite3") +
    
    labs( title = "Suiça - Evolução do Número Efetivo Reprodutivo longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_swi_var$data), max((posterior_Rt_swi$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_swi$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)


### Tornar gráfico interativo
ggplotly(graph_swi) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = 6.75, std_mean_si = 3.76,
            min_mean_si = 5.98, max_mean_si = 13.17,
            std_si = 2.9, std_std_si = 0.5,
            min_std_si = 1.9, max_std_si = 4.9,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )








# REINO UNIDO (https://coronavirus.data.gov.uk/cases)
uk <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesBySpecimenDate%22:%22newCasesBySpecimenDate%22,%22cumCasesBySpecimenDate%22:%22cumCasesBySpecimenDate%22%7D&format=json")
uk <- uk$data

## Alterar para formato Data
uk$date <- as.Date(uk$date, "%Y-%m-%d")
### Ordenar por data
uk <- as.data.frame(uk[order(uk$date), ])

## Tabela confirmados novos
uk_var <- uk %>%
    select(date, newCasesBySpecimenDate)
names(uk_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_uk_var <- uk_var  %>%
    filter(uk_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Reino Unido - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_uk <- estimate_R(as.numeric(covid_uk_var$confirmados_novos), 
                                method = "uncertain_si",
                                config = sens_configs
)

sample_windows_uk <- seq(length(Rt_nonparam_si_uk$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_uk <- 
    map(.x = sample_windows_uk,
        .f = function(x) {
            
            posterior_sample_obj_uk <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_uk,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_uk <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_uk$R$t_start[x],
                    window_t_end = Rt_nonparam_si_uk$R$t_end[x],
                    date_point = covid_uk_var[covid_uk_var$t_start == Rt_nonparam_si_uk$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_uk),
                    R_e_q0025 = quantile(posterior_sample_obj_uk, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_uk, probs = 0.975))
            
            return(posterior_sample_estim_uk)}
    ) %>% 
    
    reduce(bind_rows)


## Gráfico Reino Unido ggplot

graph_uk <- ggplot(posterior_Rt_uk, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "rosybrown3",  alpha = 0.65, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.2, fill = "rosybrown2") +
    
    labs( title = "Reino Unido - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_uk_var$data), max((posterior_Rt_uk$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_uk$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)


### Tornar gráfico interativo
ggplotly(graph_uk) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = 6.75, std_mean_si = 3.76,
            min_mean_si = 5.98, max_mean_si = 13.17,
            std_si = 2.9, std_std_si = 0.5,
            min_std_si = 1.9, max_std_si = 4.9,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )








#SUÉCIA (https://experience.arcgis.com/experience/09f821667ce64bf7be6f9f87457ed9aa/page/page_0/)
sweden <- "https://fohm.maps.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data"
sweden <- rio::import(file = sweden)

## Alterar formato de Data
sweden$Statistikdatum <- as.Date(sweden$Statistikdatum, "%Y-%m-%d")

## Tabela de confirmados novos
swe_var <- sweden %>%
    select(Statistikdatum, Totalt_antal_fall)
names(swe_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_swe_var <- swe_var  %>%
    filter(swe_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Suécia- Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)
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
Rt_nonparam_si_sc <- estimate_R(as.numeric(covid_swe_var$confirmados_novos),
                                method = "uncertain_si",
                                config = sens_configs)

sample_windows_sc <- seq(length(Rt_nonparam_si_swe$R$t_start))

# Criar um data frame com valores de R
posterior_Rt_swe <- 
    map(
        .x = sample_windows_swe,
        .f = function(x) {
            
            posterior_sample_obj_swe <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_swe,
                    n = 1000, 
                    window = x
                )
            
            posterior_sample_estim_swe <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_swe$R$t_start[x],
                    window_t_end = Rt_nonparam_si_swe$R$t_end[x],
                    date_point = covid_swe_var[covid_swe_var$t_start == Rt_nonparam_si_swe$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_swe),
                    R_e_q0025 = quantile(posterior_sample_obj_swe, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_swe, probs = 0.975)
                )
            
            return(posterior_sample_estim_swe)
            
        }
    ) %>% 
    reduce(bind_rows)

posterior_Re_swe <- posterior_Rt_swe %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))

#Grafico Suécia 
graph_swe <- ggplot(posterior_Rt_swe, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "yellow4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "yellow3") +
    
    labs( title = "Suécia - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:FOHM ",
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
        limits = c(min(covid_swe_var$data), max(posterior_Rt_swe$date_point))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_swe$R_e_q0975)),
        limits = c(0, NA)
    ) +
    
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)

#Tornar o grafico interativo
ggplotly(graph_swe) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = 6.75, std_mean_si = 3.76,
            min_mean_si = 5.98, max_mean_si = 13.17,
            std_si = 2.9, std_std_si = 0.5,
            min_std_si = 1.9, max_std_si = 4.9,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )










#AUSTRÁLIA (https://github.com/M3IT/COVID-19_Data/blob/master/Data/COVID_AU_national_daily_change.csv)
australia <- read.csv("https://raw.githubusercontent.com/M3IT/COVID-19_Data/master/Data/COVID_AU_national_daily_change.csv")

## Alterar para formato Data
australia$date <- as.Date(australia$date, "%Y-%m-%d")

## Tabela confirmados novos
aus_var <- australia %>%
    select(date, confirmed)
names(aus_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_aus_var <- aus_var  %>%
    filter(aus_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Australia- Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_aus <- estimate_R(as.numeric(covid_aus_var$confirmados_novos),
                                 method = "uncertain_si",
                                 config = sens_configs)

sample_windows_aus <- seq(length(Rt_nonparam_si_aus$R$t_start))

# Criar um data frame com valores de R
posterior_Rt_aus <- 
    map(
        .x = sample_windows_aus,
        .f = function(x) {
            
            posterior_sample_obj_aus <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_aus,
                    n = 1000, 
                    window = x
                )
            
            posterior_sample_estim_aus <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_aus$R$t_start[x],
                    window_t_end = Rt_nonparam_si_aus$R$t_end[x],
                    date_point = covid_aus_var[covid_aus_var$t_start == Rt_nonparam_si_aus$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_aus),
                    R_e_q0025 = quantile(posterior_sample_obj_aus, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_aus, probs = 0.975)
                )
            
            return(posterior_sample_estim_aus)
            
        }
    ) %>% 
    reduce(bind_rows)

posterior_Re_aus <- posterior_Rt_aus %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))

#Grafico Australia
graph_aus<- ggplot(posterior_Rt_aus, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "cyan4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "cyan3") +
    
    labs( title = " Austrália - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados: ",
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
        limits = c(min(covid_aus_var$data), max(posterior_Rt_aus$date_point))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_aus$R_e_q0975)),
        limits = c(0, NA)
    ) +
    
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)

#Tornar o grafico interativo
ggplotly(graph_aus) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = 6.75, std_mean_si = 3.76,
            min_mean_si = 5.98, max_mean_si = 13.17,
            std_si = 2.9, std_std_si = 0.5,
            min_std_si = 1.9, max_std_si = 4.9,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )










#ÍNDIA (https://api.covid19india.org/documentation/csv/)
india <- read.csv("https://api.covid19india.org/csv/latest/case_time_series.csv")

india$Date_YMD <- as.Date(india$Date_YMD, "%Y-%m-%d")

# Tabela
india_var <- india %>%
    select(Date_YMD, Daily.Confirmed)
names(india_var) <- c("data", "confirmados_novos")

# Previsão da evolução - acrescentar coluna numerada 
covid_india_var <- india_var  %>%
    filter(india_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

### Cálculo do Rt India- Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_india <- estimate_R(as.numeric(covid_india_var$confirmados_novos),
                                   method = "uncertain_si",
                                   config = sens_configs)

sample_windows_india <- seq(length(Rt_nonparam_si_india$R$t_start))

# Criar um data frame com valores de R
posterior_Rt_india <- 
    map(
        .x = sample_windows_india,
        .f = function(x) {
            
            posterior_sample_obj_india <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_india,
                    n = 1000, 
                    window = x
                )
            
            posterior_sample_estim_india <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_india$R$t_start[x],
                    window_t_end = Rt_nonparam_si_india$R$t_end[x],
                    date_point = covid_india_var[covid_india_var$t_start == Rt_nonparam_si_india$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_india),
                    R_e_q0025 = quantile(posterior_sample_obj_india, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_india, probs = 0.975)
                )
            
            return(posterior_sample_estim_india)
            
        }
    ) %>% 
    reduce(bind_rows)

posterior_Re_india <- posterior_Rt_india %>%
    mutate(fit = round(R_e_median, 2),
           lwr=round(R_e_q0025, 2),
           upr=round(R_e_q0975, 2))

#Grafico Índia
graph_india<- ggplot(posterior_Rt_india, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "coral3",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "coral") +
    
    labs( title = "Índia - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados: ",
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
        limits = c(min(covid_india_var$data), max(posterior_Rt_india$date_point))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_india$R_e_q0975)),
        limits = c(0, NA)
    ) +
    
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)

#Tornar o grafico interativo
ggplotly(graph_india) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = 6.75, std_mean_si = 3.76,
            min_mean_si = 5.98, max_mean_si = 13.17,
            std_si = 2.9, std_std_si = 0.5,
            min_std_si = 1.9, max_std_si = 4.9,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )








# HONG KONG (https://data.gov.hk/en-data/dataset/hk-dh-chpsebcddr-novel-infectious-agent)
hk <- read.csv("http://www.chp.gov.hk/files/misc/enhanced_sur_covid_19_eng.csv")

## Alterar formato de Data
hk$Report.date <- as.Date(hk$Report.date, format = "%d/%m/%Y")

## Selecionar apenas os casos confirmados
hk_var <- hk %>%
    filter(Confirmed.probable == "Confirmed")

## Agrupar o nº de registos por dia
hk_var <- as.data.frame(aggregate(x = hk_var, list(hk_var$Report.date), FUN = length))

## Criar tabela de confirmados novos
hk_var <- hk_var %>%
    select(Group.1, Report.date)
names(hk_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_hk_var <- hk_var  %>%
    filter(hk_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt HK - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_hk <- estimate_R(as.numeric(covid_hk_var$confirmados_novos), 
                                method = "uncertain_si",
                                config = sens_configs
)

sample_windows_hk <- seq(length(Rt_nonparam_si_hk$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_hk <- 
    map(.x = sample_windows_hk,
        .f = function(x) {
            
            posterior_sample_obj_hk <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_hk,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_hk <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_hk$R$t_start[x],
                    window_t_end = Rt_nonparam_si_hk$R$t_end[x],
                    date_point = covid_hk_var[covid_hk_var$t_start == Rt_nonparam_si_hk$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_hk),
                    R_e_q0025 = quantile(posterior_sample_obj_hk, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_hk, probs = 0.975))
            
            return(posterior_sample_estim_hk)}
    ) %>% 
    
    reduce(bind_rows)


## Gráfico Hong Kong ggplot

graph_hk <- ggplot(posterior_Rt_hk, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "palevioletred3",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palevioletred1") +
    
    labs( title = "Hong Kong - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_hk_var$data), max((posterior_Rt_hk$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_hk$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)


### Tornar gráfico interativo
ggplotly(graph_hk) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))


# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = 6.75, std_mean_si = 3.76,
            min_mean_si = 5.98, max_mean_si = 13.17,
            std_si = 2.9, std_std_si = 0.5,
            min_std_si = 1.9, max_std_si = 4.9,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )









# EUA (https://covid.cdc.gov/covid-data-tracker/#cases_casesinlast7days ou https://data.cdc.gov/Case-Surveillance/COVID-19-Case-Surveillance-Public-Use-Data/vbim-akqf)
usa <- "https://data.cdc.gov/api/views/vbim-akqf/rows.csv?accessType=DOWNLOAD&bom=true&format=true"
usa <- as.data.frame(rio::import(file = usa))

## Alterar para formato Data
usa$cdc_report_dt <- as.Date(usa$cdc_report_dt, "%Y/%m/%d")

## Ordenar por data
usa <- as.data.frame(usa[order(usa$cdc_report_dt), ])

## Agrupar confirmados diários
usa_var <- usa %>%
    filter(current_status == "Laboratory-confirmed case") #selecionar apenas casos confirmados
usa_var <- as.data.frame(aggregate(x = usa_var, list(usa_var$cdc_report_dt), FUN = length)) #nº registos por dia

## Criar tabela confirmados novos
usa_var <- usa_var[, 1:2]
names(usa_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_usa_var <- usa_var  %>%
    filter(usa_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt USA - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_usa <- estimate_R(as.numeric(covid_usa_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_usa <- seq(length(Rt_nonparam_si_usa$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_usa <- 
    map(.x = sample_windows_usa,
        .f = function(x) {
            
            posterior_sample_obj_usa <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_usa,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_usa <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_usa$R$t_start[x],
                    window_t_end = Rt_nonparam_si_usa$R$t_end[x],
                    date_point = covid_usa_var[covid_usa_var$t_start == Rt_nonparam_si_usa$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_usa),
                    R_e_q0025 = quantile(posterior_sample_obj_usa, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_usa, probs = 0.975))
            
            return(posterior_sample_estim_usa)}
    ) %>% 
    
    reduce(bind_rows)


## Gráfico USA ggplot

graph_usa <- ggplot(posterior_Rt_usa, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "royalblue4",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "royalblue2") +
    
    labs( title = " Estados Unidos da América - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_usa_var$data), max((posterior_Rt_usa$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_usa$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)


### Tornar gráfico interativo
ggplotly(graph_usa) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = 6.75, std_mean_si = 3.76,
            min_mean_si = 5.98, max_mean_si = 13.17,
            std_si = 2.9, std_std_si = 0.5,
            min_std_si = 1.9, max_std_si = 4.9,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )








#JAPÃO - alterar data do j.son todos os dias (https://github.com/reustle/covid19japan-data/tree/master/docs/summary)
japan <- fromJSON("https://raw.githubusercontent.com/reustle/covid19japan-data/master/docs/summary/2020-10-24.json")
japan <- japan$daily

## Alterar formato para data
japan$date <- as.Date(japan$date, "%Y-%m-%d")

## Criar tabela confirmados novos
jap_var <- japan %>%
    select(confirmed, date)
jap_var <- jap_var[, c(2,1)] #trocar posição das colunas
names(jap_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_jap_var <- jap_var  %>%
    filter(jap_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Japão - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_jap <- estimate_R(as.numeric(covid_jap_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_jap <- seq(length(Rt_nonparam_si_jap$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_jap <- 
    map(.x = sample_windows_jap,
        .f = function(x) {
            
            posterior_sample_obj_jap <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_jap,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_jap <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_jap$R$t_start[x],
                    window_t_end = Rt_nonparam_si_jap$R$t_end[x],
                    date_point = covid_jap_var[covid_jap_var$t_start == Rt_nonparam_si_jap$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_jap),
                    R_e_q0025 = quantile(posterior_sample_obj_jap, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_jap, probs = 0.975))
            
            return(posterior_sample_estim_jap)}
    ) %>% 
    
    reduce(bind_rows)


## Gráfico Japão ggplot

graph_jap <- ggplot(posterior_Rt_jap, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "chocolate3",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "chocolate1") +
    
    labs( title = " Japão - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_jap_var$data), max((posterior_Rt_jap$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_jap$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)


### Tornar gráfico interativo
ggplotly(graph_jap) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))


# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = 6.75, std_mean_si = 3.76,
            min_mean_si = 5.98, max_mean_si = 13.17,
            std_si = 2.9, std_std_si = 0.5,
            min_std_si = 1.9, max_std_si = 4.9,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )









# NOVA ZELÂNDIA (alterar diariamente em https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-current-cases-details#download)
nzealand <- "https://www.health.govt.nz/system/files/documents/pages/covid-cases-24oct20_0.xlsx"
nzealand <- rio::import(file = nzealand)
nzealand <- nzealand[-c(1,2), ]

## Alterar formato para data
nzealand$`Confirmed Covid-19 cases` <- openxlsx::convertToDate(nzealand$`Confirmed Covid-19 cases`) #alterar formato de data excel para Date no R

## Criar tabela confirmados novos
nze_var <- as.data.frame(aggregate(x = nzealand, list(nzealand$`Confirmed Covid-19 cases`), FUN = length)) #nº registos por dia
nze_var <- nze_var[, 1:2]
names(nze_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_nze_var <- nze_var  %>%
    filter(nze_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Nova Zelândia - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_nze <- estimate_R(as.numeric(covid_nze_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_nze <- seq(length(Rt_nonparam_si_nze$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_nze <- 
    map(.x = sample_windows_nze,
        .f = function(x) {
            
            posterior_sample_obj_nze <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_nze,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_nze <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_nze$R$t_start[x],
                    window_t_end = Rt_nonparam_si_nze$R$t_end[x],
                    date_point = covid_nze_var[covid_nze_var$t_start == Rt_nonparam_si_nze$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_nze),
                    R_e_q0025 = quantile(posterior_sample_obj_nze, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_nze, probs = 0.975))
            
            return(posterior_sample_estim_nze)}
    ) %>% 
    
    reduce(bind_rows)

## Gráfico Nova Zelândia ggplot

graph_nze <- ggplot(posterior_Rt_nze, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "chocolate3",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "chocolate1") +
    
    labs( title = " Nova Zelândia - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_nze_var$data), max((posterior_Rt_nze$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_nze$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)

### Tornar gráfico interativo
ggplotly(graph_nze) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = , std_mean_si = ,
            min_mean_si = , max_mean_si = ,
            std_si = , std_std_si = ,
            min_std_si = , max_std_si = ,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )







# MÉXICO (https://www.gob.mx/salud/documentos/datos-abiertos-152127)
## Criar pasta temporária para guardar zip do mexico
MEXDATA <- tempfile() 

## Download do zip para a pasta temporária
download.file("http://datosabiertos.salud.gob.mx/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip", MEXDATA)

## Fazer unzip do cvs 
mexico <- read.csv(unz(MEXDATA, "201024COVID19MEXICO.csv")) 

## Eliminar pasta temporária
unlink(MEXDATA) 

## Alterar formato para data
mexico$FECHA_INGRESO <- as.Date(mexico$FECHA_INGRESO, "%Y-%m-%d")

## Criar tabela confirmados novos
### Ordenar por data
mexico <- as.data.frame(mexico[order(mexico$FECHA_INGRESO),])
### Selecionar apenas casos confirmados, que correspondem aos nº 1 na coluna do resultado lab
mex_var <- mexico %>%
    filter(RESULTADO_LAB == "1") 

mex_var <- as.data.frame(aggregate(x = mex_var , list(mex_var$FECHA_INGRESO), FUN = length)) #nº registos por dia
mex_var <- mex_var %>%
    select(Group.1, FECHA_INGRESO)
names(mex_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_mex_var <- mex_var  %>%
    filter(mex_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt México - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_mex <- estimate_R(as.numeric(covid_mex_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_mex <- seq(length(Rt_nonparam_si_mex$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_mex <- 
    map(.x = sample_windows_mex,
        .f = function(x) {
            
            posterior_sample_obj_mex <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_mex,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_mex <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_mex$R$t_start[x],
                    window_t_end = Rt_nonparam_si_mex$R$t_end[x],
                    date_point = covid_mex_var[covid_mex_var$t_start == Rt_nonparam_si_mex$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_mex),
                    R_e_q0025 = quantile(posterior_sample_obj_mex, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_mex, probs = 0.975))
            
            return(posterior_sample_estim_mex)}
    ) %>% 
    
    reduce(bind_rows)

## Gráfico México ggplot

graph_mex <- ggplot(posterior_Rt_mex, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "chocolate3",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "chocolate1") +
    
    labs( title = " México - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_mex_var$data), max((posterior_Rt_mex$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_mex$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)

### Tornar gráfico interativo
ggplotly(graph_mex) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))


# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = , std_mean_si = ,
            min_mean_si = , max_mean_si = ,
            std_si = , std_std_si = ,
            min_std_si = , max_std_si = ,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )






# COREIA DO SUL (https://github.com/owid/covid-19-data/blob/master/public/data/ecdc/new_cases.csv)
korea <- read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/ecdc/new_cases.csv")

## Alterar formato para data
korea$date <- as.Date(korea$date, "%Y-%m-%d")

## Criar tabela confirmados novos
kor_var <- korea %>% 
    select(date, South.Korea)
names(kor_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_kor_var <- kor_var  %>%
    filter(kor_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Coreia do Sul - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_kor <- estimate_R(as.numeric(covid_kor_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_kor <- seq(length(Rt_nonparam_si_kor$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_kor <- 
    map(.x = sample_windows_kor,
        .f = function(x) {
            
            posterior_sample_obj_kor <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_kor,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_kor <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_kor$R$t_start[x],
                    window_t_end = Rt_nonparam_si_kor$R$t_end[x],
                    date_point = covid_kor_var[covid_kor_var$t_start == Rt_nonparam_si_kor$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_kor),
                    R_e_q0025 = quantile(posterior_sample_obj_kor, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_kor, probs = 0.975))
            
            return(posterior_sample_estim_kor)}
    ) %>% 
    
    reduce(bind_rows)

## Gráfico Coreia do Sul ggplot

graph_kor <- ggplot(posterior_Rt_kor, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "chocolate3",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "chocolate1") +
    
    labs( title = " Coreia do Sul - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_kor_var$data), max((posterior_Rt_kor$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_kor$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)

### Tornar gráfico interativo
ggplotly(graph_kor) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = , std_mean_si = ,
            min_mean_si = , max_mean_si = ,
            std_si = , std_std_si = ,
            min_std_si = , max_std_si = ,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )








#BRASIL (https://covid19.who.int/table)
brasil <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")

## Alterar formato para data
brasil$ï..Date_reported <- as.Date(brasil$ï..Date_reported, "%Y-%m-%d")

## Criar tabela confirmados novos
bra_var <- brasil %>%
    filter(Country == "Brazil") %>%
    select(ï..Date_reported, New_cases)
names(bra_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_bra_var <- bra_var  %>%
    filter(bra_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt Brasil - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_bra <- estimate_R(as.numeric(covid_bra_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_bra <- seq(length(Rt_nonparam_si_bra$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_bra <- 
    map(.x = sample_windows_bra,
        .f = function(x) {
            
            posterior_sample_obj_bra <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_bra,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_bra <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_bra$R$t_start[x],
                    window_t_end = Rt_nonparam_si_bra$R$t_end[x],
                    date_point = covid_bra_var[covid_bra_var$t_start == Rt_nonparam_si_bra$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_bra),
                    R_e_q0025 = quantile(posterior_sample_obj_bra, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_bra, probs = 0.975))
            
            return(posterior_sample_estim_bra)}
    ) %>% 
    
    reduce(bind_rows)

## Gráfico Brasil ggplot

graph_bra <- ggplot(posterior_Rt_bra, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "chocolate3",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "chocolate1") +
    
    labs( title = " Brasil - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_bra_var$data), max((posterior_Rt_bra$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_bra$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)

### Tornar gráfico interativo
ggplotly(graph_bra) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = , std_mean_si = ,
            min_mean_si = , max_mean_si = ,
            std_si = , std_std_si = ,
            min_std_si = , max_std_si = ,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )










#CHINA (https://covid19.who.int/table)
china <- read.csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")

## Alterar formato para data
china$ï..Date_reported <- as.Date(china$ï..Date_reported, "%Y-%m-%d")

## Criar tabela confirmados novos
chi_var <- china %>%
    filter(Country == "China") %>%
    select(ï..Date_reported, New_cases)
names(chi_var) <- c("data", "confirmados_novos")

## Previsão da evolução
covid_chi_var <- chi_var  %>%
    filter(chi_var$data > as.Date("2020-02-28")) %>% 
    dplyr::mutate(t_start = dplyr::row_number())

## Cálculo do Rt China - Uncertainty method --> "uncertain_si"
### Serial Interval (c/ base nos valores anteriores)

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
Rt_nonparam_si_chi <- estimate_R(as.numeric(covid_chi_var$confirmados_novos), 
                                 method = "uncertain_si",
                                 config = sens_configs
)

sample_windows_chi <- seq(length(Rt_nonparam_si_chi$R$t_start))

## Criar um data frame com valores de R
posterior_Rt_chi <- 
    map(.x = sample_windows_chi,
        .f = function(x) {
            
            posterior_sample_obj_chi <- 
                sample_posterior_R(
                    R = Rt_nonparam_si_chi,
                    n = 1000, 
                    window = x )
            
            posterior_sample_estim_chi <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si_chi$R$t_start[x],
                    window_t_end = Rt_nonparam_si_chi$R$t_end[x],
                    date_point = covid_chi_var[covid_chi_var$t_start == Rt_nonparam_si_chi$R$t_end[x], "data"],
                    R_e_median = median(posterior_sample_obj_chi),
                    R_e_q0025 = quantile(posterior_sample_obj_chi, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj_chi, probs = 0.975))
            
            return(posterior_sample_estim_chi)}
    ) %>% 
    
    reduce(bind_rows)

## Gráfico China ggplot

graph_chi <- ggplot(posterior_Rt_chi, aes(x = date_point, y = R_e_median)) +
    geom_line(colour = "chocolate3",  alpha = 0.5, size = 1.5) +
    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "chocolate1") +
    
    labs( title = " China - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
          subtitle = "Fonte de dados:  ",
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
        limits = c(min(covid_chi_var$data), max((posterior_Rt_chi$date_point)))
    ) +
    
    scale_y_continuous(
        breaks = 0:ceiling(max(posterior_Rt_chi$R_e_q0975)),
        limits = c(0, NA)
    ) +
    geom_hline(yintercept = 1, colour= "grey1", alpha= 0.4)

### Tornar gráfico interativo
ggplotly(graph_chi) %>%
    layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                         "Nº de reprodução efetivo (Rt)",
                                         rep("&nbsp;", 20),
                                         rep("\n&nbsp;", 2)),
                                       collapse = "")))

# Serial Interval específico
sens_configs <- 
    make_config(
        list(
            mean_si = , std_mean_si = ,
            min_mean_si = , max_mean_si = ,
            std_si = , std_std_si = ,
            min_std_si = , max_std_si = ,
            n1 = 1000,
            n2 = 100,
            seed = 123456789
        )
    )











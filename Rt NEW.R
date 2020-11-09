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
library(scales)


setwd("C:/Users/teres/Desktop/EPIVET/COVID19/Rt_COVID19")
setwd("~/Desktop/Treino Estágio 2020-2021/Rt_COVID19")


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
#plot(Rt_nonparam_si, legend = FALSE)

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


## GRÁFICO ggplot

## Linhas a adicionar no gráfico
d=data.frame(date=as.Date(c("2020-03-16", "2020-03-18", "2020-10-15", "2020-11-09")), Evento=c("Encerramento das Escolas", "Estado de Emergência", "Estado de Calamidade", "Estado de Emergência"))


graph_PT<- ggplot(posterior_R_t, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "palegreen4",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                       '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.3, fill = "palegreen3") +
  
  labs(title = " Evolução do Número Efetivo Reprodutivo ao longo do tempo",
       subtitle = "Fonte de dados: DGS ",
       x = "Data",
       y = "Nº de reprodução efetivo (Rt)"
  ) +
  
  theme_minimal() +
  
  theme(title = element_text(size=15),
        axis.title = element_text(size = 12, hjust =0.5),
        plot.subtitle = element_text(size= 8),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_pt_var$data), max(posterior_R_t$date_point))
  ) +
  
  scale_y_continuous(
    breaks = c(0:15),
    limits = c(0, 15)
  ) +
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-16", "2020-03-18", "2020-10-15", "2020-11-09"))), linetype= c("solid", "dotted", "twodash", "dotted"), colour = "indianred4", alpha = 0.5) +
  geom_vline(data=d, mapping =  aes(xintercept = date, linetype = Evento), size = 1, colour = 'indianred4', alpha = 0.5, show.legend = TRUE)


### Tornar gráfico interativo
ggplotly(graph_PT, tooltip = "text")


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
#plot(Rt_nonparam_si1, legend = FALSE)

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


## GRÁFICO GGPLOT
## Linhas a adicionar no gráfico
d=data.frame(date=as.Date(c("2020-03-16", "2020-03-18", "2020-10-15")), Evento=c("Encerramento das Escolas", "Estado de Emergência", "Estado de Calamidade"))

graph_Norte <- ggplot(posterior_R_t1, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "palegreen4",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                       '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
  
  labs( title = " ARS Norte - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
        subtitle = "Fonte de dados: DGS ",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_pt_var$data), max(posterior_R_t1$date_point))
  ) +
  
  scale_y_continuous(
    breaks = c(0:15),
    limits = c(0, 15)
  ) +
  
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-16", "2020-03-18", "2020-10-15" ))), linetype= c("solid", "dotted", "twodash"), colour = "indianred4", alpha = 0.5) +
  geom_vline(data=d, mapping =  aes(xintercept = date, linetype = Evento), size = 1, colour = 'indianred4', alpha = 0.5, show.legend = TRUE)


### Tornar gráfico interativo
ggplotly(graph_Norte, tooltip = "text")




# Rt Diário ARS Centro 
Rt_nonparam_si2 <- 
    estimate_R(
        covid_pt_var$confirmados_var_centro, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
#plot(Rt_nonparam_si2, legend = FALSE)

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


## GRÁFICO GGPLOT
## Linhas a adicionar no gráfico
d=data.frame(date=as.Date(c("2020-03-16", "2020-03-18", "2020-10-15")), Evento=c("Encerramento das Escolas", "Estado de Emergência", "Estado de Calamidade"))

graph_Centro <- ggplot(posterior_R_t2, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "palegreen4",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                       '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
  
  labs( title = " ARS Centro - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
        subtitle = "Fonte de dados: DGS ",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_pt_var$data), max(posterior_R_t2$date_point))
  ) +
  
  scale_y_continuous(
    breaks = c(0:15),
    limits = c(0, 15)
  ) +
  
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-16", "2020-03-18", "2020-10-15" ))), linetype= c("solid", "twodash", "dotted"), colour = "indianred4", alpha = 0.5) +
  geom_vline(data=d, mapping =  aes(xintercept = date, linetype = Evento), size = 1, colour = 'indianred4', alpha = 0.5, show.legend = TRUE)


### Tornar gráfico interativo
ggplotly(graph_Centro, tooltip = "text")




# Rt ARS Lisboa e Vale do Tejo
Rt_nonparam_si3 <- 
    estimate_R(
        covid_pt_var$confirmados_var_lvt, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
#plot(Rt_nonparam_si3, legend = FALSE)

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


## GRÁFICO GGPLOT
## Linhas a adicionar no gráfico
d=data.frame(date=as.Date(c("2020-03-16", "2020-03-18", "2020-10-15")), Evento = c("Encerramento das Escolas", "Estado de Emergência", "Estado de Calamidade"))

graph_LVT<- ggplot(posterior_R_t3, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "palegreen4",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                       '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
  
  labs( title = " ARS LVT - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
        subtitle = "Fonte de dados: DGS ",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_pt_var$data), max(posterior_R_t3$date_point))
  ) +
  
  scale_y_continuous(
    breaks = c(0:15),
    limits = c(0, 15)
  ) +
  
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-16", "2020-03-18", "2020-10-15" ))), linetype= c("solid", "twodash", "dotted"), colour = "indianred4", alpha = 0.5) +
  geom_vline(data=d, mapping =  aes(xintercept = date, linetype = Evento), size = 1, colour = 'indianred4', alpha = 0.5, show.legend = TRUE)


### Tornar gráfico interativo
ggplotly(graph_LVT, tooltip = "text")




# Rt ARS Alentejo
Rt_nonparam_si4 <- 
    estimate_R(
        covid_pt_var$confirmados_var_alentejo, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
#plot(Rt_nonparam_si4, legend = FALSE)

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



## GRÁFICO GGPLOT
## Linhas a adicionar no gráfico
d=data.frame(date=as.Date(c("2020-03-16", "2020-03-18", "2020-10-15")), Evento = c("Encerramento das Escolas", "Estado de Emergência", "Estado de Calamidade"))

graph_Alentejo <- ggplot(posterior_R_t4, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "palegreen4",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                       '<br>Rt médio: ', R_e_median))) +    geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
  
  labs( title = " ARS Alentejo - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
        subtitle = "Fonte de dados: DGS ",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_pt_var$data), max(posterior_R_t4$date_point))
  ) +
  
  scale_y_continuous(
    limits = c(0, 15),
    breaks = c(0:15),
  ) +
  
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-16", "2020-03-18", "2020-10-15" ))), linetype= c("solid", "dotted", "twodash"), colour = "indianred4", alpha = 0.5) +
  geom_vline(data=d, mapping =  aes(xintercept = date, linetype = Evento), size = 1, colour = 'indianred4', alpha = 0.5, show.legend = TRUE)

### Tornar gráfico interativo
ggplotly(graph_Alentejo, tooltip = "text")





# Rt ARS Algarve
Rt_nonparam_si5 <- 
    estimate_R(
        covid_pt_var$confirmados_var_algarve, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
#plot(Rt_nonparam_si5, legend = FALSE)

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


## GRÁFICO GGPLOT
## Linhas a adicionar no gráfico
d=data.frame(date=as.Date(c("2020-03-16", "2020-03-18", "2020-10-15")), Evento = c("Encerramento das Escolas", "Estado de Emergência", "Estado de Calamidade"))

graph_Algarve<- ggplot(posterior_R_t5, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "palegreen4",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                       '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
  
  labs( title = " ARS Algarve - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
        subtitle = "Fonte de dados: DGS ",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_pt_var$data), max(posterior_R_t5$date_point))
  ) +
  
  scale_y_continuous(
    breaks = 0:ceiling(max(posterior_R_t5$R_e_q0975)),
    limits = c(0, 20)
  ) +
  
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-16", "2020-03-18", "2020-10-15" ))), linetype= c("solid", "dotted", "twodash"), colour = "indianred4", alpha = 0.5) +
  geom_vline(data=d, mapping =  aes(xintercept = date, linetype = Evento), size = 1, colour = 'indianred4', alpha = 0.5, show.legend = TRUE)

### Tornar gráfico interativo
ggplotly(graph_Algarve, tooltip = "text")





# Rt ARS Açores
Rt_nonparam_si6 <- 
    estimate_R(
        covid_pt_var$confirmados_var_acores, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
#plot(Rt_nonparam_si6, legend = FALSE)

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


## GRÁFICO GGPLOT
d_azo=data.frame(date=as.Date(c("2020-03-16", "2020-03-18","2020-03-26", "2020-05-17")), Evento = c("Encerramento das Escolas", "Estado de Emergência Nacional", "Confinamento Obrigatório de Passageiros", "Testagem e/ou Quarentena Obrigatória de Passageiros"))

graph_Açores <- ggplot(posterior_R_t6, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "palegreen4",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                       '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
  
  labs( title = " ARS Açores - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
        subtitle = "Fonte de dados: DGS ",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_pt_var$data), max(posterior_R_t6$date_point))
  ) +
  
  scale_y_continuous(
    breaks = 0:ceiling(max(posterior_R_t6$R_e_q0975)),
    limits = c(0, 20)
  ) +
  
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-16", "2020-03-18","2020-03-26", "2020-05-17"))), linetype= c("twodash", "dotted", "solid", "dotdash"), colour = "indianred4", alpha = 0.5) +
  geom_vline(data=d_azo, mapping =  aes(xintercept = date, linetype = Evento), size = 1, colour = 'indianred4', alpha = 0.5, show.legend = TRUE)

### Tornar gráfico interativo
ggplotly(graph_Açores, tooltip = "text")






# Rt ARS Madeira
Rt_nonparam_si7 <- 
    estimate_R(
        covid_pt_var$confirmados_var_madeira, 
        method = "uncertain_si",
        config = sens_configs
    )

## Gráfico
#plot(Rt_nonparam_si7, legend = FALSE)

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

## GRÁFICO GGPLOT
d_mad=data.frame(date=as.Date(c("2020-03-15", "2020-03-18", "2020-05-11", "2020-05-28")), Evento = c("Quarentena obrigatória para todos os passageiros", "Estado de Emergência Nacional", "Início do desconfinamento", "Obrigatoriedade de teste negativo e/ou quarentena para todos os passageiros"))

graph_Madeira <- ggplot(posterior_R_t7, aes(x = date_point, y = R_e_median)) +
  geom_line(colour = "palegreen4",  alpha = 0.5, size = 1, aes(group = 1, text = paste('Data: ', date_point,
                                                                                       '<br>Rt médio: ', R_e_median))) +
  geom_ribbon(aes(ymin = R_e_q0025, ymax = R_e_q0975), alpha = 0.15, fill = "palegreen3") +
  
  labs( title = " ARS Madeira - Evolução do Número Efetivo Reprodutivo ao longo do tempo", size= 10,
        subtitle = "Fonte de dados: DGS ",
        x = "Data",
        y = "Nº de reprodução efetivo (Rt)"
  ) +
  
  theme_minimal() +
  
  theme(axis.title = element_text(size = 10, hjust = 0.5),
        plot.subtitle = element_text(size= 8),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        axis.text.x = element_text(angle = 60, hjust = 1)
  ) +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_pt_var$data), max(posterior_R_t7$date_point))
  ) +
  
  scale_y_continuous(
    breaks = 0:ceiling(max(posterior_R_t7$R_e_q0975)),
    limits = c(0, NA)
  ) +
  
  geom_hline(yintercept = 1, colour= "grey65", alpha= 0.4) +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-03-15", "2020-03-18", "2020-05-11", "2020-05-28"))), linetype= c("dotdash", "solid", "twodash", "dotted"), colour = "indianred4", alpha = 0.5) +
  geom_vline(data=d_mad, mapping =  aes(xintercept = date, linetype = Evento), size = 1, colour = 'indianred4', alpha = 0.5, show.legend = TRUE)

### Tornar gráfico interativo
ggplotly(graph_Madeira, tooltip = "text")







# Análise comparativa entre regiões

## Data frame com as regiões que começam no dia 7 de Março
regioes_Rt7 <- as.data.frame(cbind(posterior_R_t1$R_e_median, posterior_R_t2$R_e_median, posterior_R_t3$R_e_median, posterior_R_t6$R_e_median, posterior_R_t7$R_e_median))
names(regioes_Rt7) <- c("Norte", "Centro", "LVT", "Açores", "Madeira")
regioes_Rt7_tempo <- as.data.frame(cbind(posterior_R_t1$date_point, regioes_Rt7))
names(regioes_Rt7_tempo) <- c("Data","Norte", "Centro", "LVT", "Açores", "Madeira")

regioes_Rt7_tempo_melt <- reshape2::melt(regioes_Rt7_tempo, id.vars="Data")
names(regioes_Rt7_tempo_melt)[2:3] <- c("Regiões", "Rt_Médio")


## Data frame com as regiões que começam no dia 8 de Março
regioes_Rt4 <- as.data.frame(cbind(posterior_R_t4$R_e_median, posterior_R_t5$R_e_median))
names(regioes_Rt4) <- c("Alentejo", "Algarve")
regioes_Rt4_tempo <- as.data.frame(cbind(posterior_R_t4$date_point, regioes_Rt4))
names(regioes_Rt4_tempo) <- c("Data","Alentejo", "Algarve")

regioes_Rt4_tempo_melt <- reshape2::melt(regioes_Rt4_tempo, id.vars="Data")
names(regioes_Rt4_tempo_melt)[2:3] <- c("Regiões", "Rt_Médio")


regioes_Rt_tempo <- rbind(regioes_Rt7_tempo_melt, regioes_Rt4_tempo_melt)


## Fazer o gráfico de linhas
Rt_regioes_tempo_graph <- ggplot(regioes_Rt_tempo, aes(x = Data, y = Rt_Médio, color = Regiões, group=1)) +
  geom_point(aes(text = paste('Data: ', Data,
                              '<br>ARS: ', Regiões,
                              '<br>Rt Médio: ', Rt_Médio)), size = 0.1 ) +
  geom_line(size=0.5) +
  scale_color_discrete(labels = c("Norte", "Centro", "LVT", "Açores", "Madeira", "Alentejo", "Algarve")) +
  xlab("Regiões") +
  ylab("Rt Médio") +
  labs(title="Evolução do Rt por regiões ao longo do tempo") +

  theme(legend.title = element_blank(),
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7)) +
  
  labs(title = "Evolução do Número de Reprodução Efetivo por região",
       x = "",
       y = "Incidência") +
  
  scale_x_date(
    date_breaks = "2 weeks", labels = date_format("%b %d"),
    limits = c(min(covid_pt_var$data), max(posterior_R_t7$date_point))) +
 
   scale_y_continuous(
    breaks = 0:ceiling(max(posterior_R_t7$R_e_q0975)),
    limits = c(0, NA)
  )
  
  
### Fazer com que gráfico seja interativo
ggplotly(Rt_regioes_tempo_graph, tooltip = "text") %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Rt Médio",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))




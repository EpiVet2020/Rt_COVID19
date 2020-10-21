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
library(rjson)
library(readr)
library(readxl)


# Set working directory
setwd("C:/Users/ines/Documents/Estágio Epidemiologia/COVID19/R0")

#Data
covid19pt <-read.csv("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv", stringsAsFactors = FALSE)

##Data outros países
italy <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/legacy/dati-andamento-nazionale/dpc-covid19-ita-andamento-nazionale.csv", stringsAsFactors = FALSE)

germany <- fromJSON("https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.geojson")
germany <- germany$features

spain <- read.csv("https://cnecovid.isciii.es/covid19/resources/datos_ccaas.csv")

belgium <- read.csv("https://epistat.sciensano.be/Data/COVID19BE_CASES_AGESEX.csv")

nzealand <- "https://www.health.govt.nz/system/files/documents/pages/covid-cases-21oct20_0.xlsx"
nzealand <- rio::import(file = nzealand)
nzealand <- nzealand[-c(1,2), ]
nzealand <- cbind ((openxlsx::convertToDate(nzealand$Date)), nzealand)
names(nzealand) <- c("Date", "Datecode", "Gender", "Age group", "DHB", "Overseas travel")

czechr <- read.csv("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.csv")

switzerland <- "https://www.bag.admin.ch/dam/bag/en/dokumente/mt/k-und-i/aktuelle-ausbrueche-pandemien/2019-nCoV/covid-19-basisdaten-labortests.xlsx.download.xlsx/Dashboard_3_COVID19_labtests_positivity.xlsx"
switzerland <- rio::import(file = switzerland)

uk <- fromJSON("https://api.coronavirus.data.gov.uk/v1/data?filters=areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesBySpecimenDate%22:%22newCasesBySpecimenDate%22,%22cumCasesBySpecimenDate%22:%22cumCasesBySpecimenDate%22%7D&format=json")
uk <- uk$data

usa <- "https://data.cdc.gov/api/views/vbim-akqf/rows.csv?accessType=DOWNLOAD&bom=true&format=true"
usa <- rio::import(file = usa)

brasil <- read.csv("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/SRAG/2020/INFLUD-12-10-2020.csv")


#Transformar para formato de data
covid19pt$data <- as.Date(covid19pt$data,"%d-%m-%Y")

italy <- cbind(as.data.frame(strftime(italy$data, format = "%Y-%m-%d")), italy)
names(italy)[1] <- "Date"

germany <- cbind(as.data.frame(strftime(germany$properties$Meldedatum, format = "%Y-%m-%d")), germany)
names(germany)[1] <- "Date"

czechr$ï..datum <- as.Date(czechr$ï..datum, "%Y-%m-%d")
names(czechr)[1] <- "Date"

uk$date <- as.Date(uk$date, "%Y-%m-%d")

usa <- cbind(as.data.frame(strftime(usa$cdc_report_dt, format = "%Y-%m-%d")), usa)
names(usa)[1] <- "Date"

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
                  #                id = "ForecastRange-FL", 
                  type = "arearange", 
                  name = "Incerteza", 
                  color = "#d9d9d9") %>% 
    hc_add_series(data = posterior_R_e7$fit,
                  name = "Rt", 
                  color = "#e6550d")

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



#TABELA E GRÁFICO (NOVOS CASOS)
##Italy
it_var <- as.data.frame(cbind(italy$Date, italy$nuovi_positivi))
names(it_var) <- c("Data", "Novos")

# Previsão da evolução
covid_it_var <- it_var  %>%
    filter(it_var$Data > as.Date("2020-02-25")) %>%       
    dplyr::mutate(t_start = dplyr::row_number())


### Cálculo do Rt Itália- Uncertainty method --> "uncertain_si"
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
Rt_nonparam_si_it <- estimate_R(covid_it_var, 
                             method = "uncertain_si",
                             config = sens_configs
)

sample_windows <- seq(length(Rt_nonparam_si_it$R$t_start))

# Criar um data frame com valores de R
posterior_Rt_it <- 
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

##Germany
germany <- as.data.frame(germany[order(germany$Date), ])
nrow(germany$Date)
ger_var <- as.data.frame(aggregate(x = germany, list(germany$Date), FUN = length))
ger_var <- ger_var[,1:2]
names(ger_var) <- c("Date", "Casos")

##Spain
spa_var <- as.data.frame(aggregate(spain$num_casos, by = list(Data = spain$fecha), FUN = sum))
names(spa_var)[2] <- "Casos"

##Belgium
bel_var <- as.data.frame(aggregate(belgium$CASES, by = list(Data = belgium$DATE), FUN = sum))
names(bel_var)[2] <- "Casos"

##New Zealand
nze_var <- as.data.frame(aggregate(x = nzealand, list(nzealand$Date), FUN = length))
nze_var <- nze_var[, 1:2]
names(nze_var) <- c("Date", "Cases")

##Czech Republic 
czech_var <- czechr[,1:2]
names(czech_var) <- c("Date", "Cases")

##United Kingdom 
uk_var <- uk[, 4:5]
names(uk_var)[2]<- "Cases"

##Switzerland 
swi_var <- switzerland %>%
    filter(Outcome_tests == "Positive") %>%
    select(Datum, Number_of_tests)

##USA
usa <- as.data.frame(usa[order(usa$Date), ])
usa_var <- usa %>%
    filter(current_status == "Laboratory-confirmed case")
usa_var <- as.data.frame(aggregate(x = usa_var, list(usa_var$Date), FUN = length))
usa_var <- usa_var[, 1:2]
names(usa_var) <- c("Date", "Cases")

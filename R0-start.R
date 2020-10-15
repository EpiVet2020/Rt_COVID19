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


### Cálculo do Re - Uncertainty method --> "uncertain_si"
### Serial Interval
### -- mean 4.7 (95% CrI: 3.7, 6.0)
### -- sd 2.9 (95% CrI: 1.9, 4.9)
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

Rt_nonparam_si <- 
    estimate_R(
        covid_pt_var$confirmados_var, 
        method = "uncertain_si",
        config = sens_configs
    )


### inspect R_e estimate
#plot(Re_nonparam_si, legend = FALSE)
## Posterio sample R_e estimate
## Posterio sample R_e estimate
sample_windows <- seq(length(Rt_nonparam_si$R$t_start))

posterior_R_t <- 
    map(
        .x = sample_windows,
        .f = function(x) {
            
            posterior_sample_obj <- 
                sample_posterior_R(
                    R = Rt_nonparam_si,
                    n = 1000, 
                    window = x
                )
            
            posterior_sample_estim <- 
                data.frame(
                    window_index = x,
                    window_t_start = Rt_nonparam_si$R$t_start[x],
                    window_t_end = Rt_nonparam_si$R$t_end[x],
                    date_point = covid_r[covid_r$t_start == Rt_nonparam_si$R$t_end[x], "Data"],
                    R_e_median = median(posterior_sample_obj),
                    R_e_q0025 = quantile(posterior_sample_obj, probs = 0.025),
                    R_e_q0975 = quantile(posterior_sample_obj, probs = 0.975)
                )
            
            return(posterior_sample_estim)
            
        }
    ) %>% 
    reduce(bind_rows)


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
library(zoo)


#package para o R0 (Estimation of reproduction numbers for disease outbreak, based on incidence data)
library(R0)

# Set working directory
setwd("C:/Users/teres/Desktop/EPIVET/R0")
#Data
covid19pt <-read.csv("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv", stringsAsFactors = FALSE)
#Transformar para formato de data
covid19pt$data <- as.Date(covid19pt$data,"%d-%m-%Y")




# Como tem evoluído a transmissão em cada ARS ( R0)

## R0? Nº Básico de Reprodução que indica, em média, quantas pessoas serão infetadas por um único doente.
## Nº médio de casos secundários originados a partir de um caso primário, quando o vírus é introduzido numa população que consiste somente em indivíduos suscetíveis.
## Quanto + alto, + rápida é a propagação da doença

## Re? Nº Efetivo de Reprodução que indica, em média, o nº de infeções que cada doente provoca, mas tem como base de partida uma população que já esteve exposta ao vírus.
## Ou seja, leva em conta as medidas postas em prática para travar a disseminação.

## Qd R0 = 1, ocorre o limiar da transmissão (abaixo deste valor a infeção é incapaz de se manter numa população; acima há possibilidade de disseminação)

##“Se o R0 era pouco acima de 2 e [o R efectivo] continua acima de 1, isso quer obrigatoriamente dizer que as medidas impostas tiveram um efeito reduzido. 
##Admitindo que o R0 seja 2.5, com uma redução de 60% tornar-se-ia 1. Logo, se continua acima de 1, a transmissão foi reduzida em menos de 60%.”

### OBJETIVO: Manter o R0 > ou = a 1 ate existir vacina!

## Fatores de risco : período infeccioso, taxa de contacto, modo de transmissão




# REGIÕES compararação de regiões relativamente a:
## Nº de casos 

casos_região <- as.data.frame(t(as.data.frame(lapply(covid19pt[,4:10], last))))

casos_região <- casos_região %>% 
  rownames_to_column(var="Regiões")
names(casos_região)[2] <- "N_casos"

casos_regiao_total <- as.data.frame(cbind(covid19pt$data, covid19pt[,4:10]))
names(casos_regiao_total) <- c("Data", "Norte", "Centro", "Lisboa", "Alentejo", "Algarve", "Açores", "Madeira")

casos_regiao_total_melt <- melt(casos_regiao_total, id.vars = "Data")
names(casos_regiao_total_melt) <- c("Data", "Regiões", "Casos")
                                
                                    
##Gráfico evolução
casos_grafico <- ggplot(casos_regiao_total_melt, aes(x = Data, y = Casos, color = Regiões)) +
  geom_line() +
  theme(legend.title = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 7)) +
  labs(title = "Evolução do nº casos total",
       x = "Tempo",
       y = "Casos") +
  scale_x_date(breaks = "months", date_labels = "%B")

ggplotly(casos_grafico) %>% 
  layout(yaxis = list(title = paste0(c(rep("&nbsp;", 20),
                                       "Casos",
                                       rep("&nbsp;", 20),
                                       rep("\n&nbsp;", 2)),
                                     collapse = "")),
         legend = list(x = 1, y = 0))


## Taxa de incidência cumulativa ou Risco
### Nº pessoas que desenvolvem a doença / Nº pessoas sem a doença inicialmente (total - mortes - recuperados (por região ñ há))
### Valores da população de cada Região com base nas CCDRs
acores = 242796
alentejo = 503507
algarve = 450484
centro = 2217285
lisboa = 3631738
madeira = 253945
norte = 3575338

### Criar uma tabela com uma coluna para as Regiões e outra para o número de pessoas nessa Região
populacao_regioes <- as.data.frame(c(norte, centro, lisboa, alentejo, algarve, acores, madeira), 
                                   c("norte", "centro", "lisboa", "alentejo", "algarve", "açores", "madeira"))
colnames(populacao_regioes) <- "População"

## Subtrair mortalidade por região à população para obter população suscetível sem doença
mortes_região <- as.data.frame(t(as.data.frame(lapply(covid19pt[, 49:55], last))))

mortes_região <- mortes_região %>% 
  rownames_to_column(var="Regiões")
names(mortes_região)[2] <- "N_mortes"


## Incidência cumulativa ou Attack Rate
pop_suscetível <- as.data.frame(populacao_regioes[,1] - mortes_região[,2])

ic_regiao <- as.data.frame(t(casos_região[,2]) / pop_suscetível) %>% 
  rownames_to_column(var="Regiao")
colnames(ic_regiao)[2] <- "IC"
ic_regiao[,1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

### Incidência cumulativa ou Attack rate até ao dia 1 Maio
pop_suscetível_1maio <- as.data.frame(t(populacao_regioes[,1] - covid19pt[66, 49:55]))

ic_regiao_1maio <- as.data.frame(t(covid19pt[66, 4:10]) / pop_suscetível_1maio) %>%
  rownames_to_column(var= "Região")
colnames(ic_regiao_1maio)[2] <- "IC"
ic_regiao_1maio[,1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

# R PACKAGE
# Estimativa R0 segundo attack rate (considerando Pico/ até Maio)
R0_maio <- est.R0.AR(AR = ic_regiao_1maio[,2], pop.size = pop_suscetível_1maio, S0 = 1)

# Estimativa R0 segundo attack rate (atual)
R0 <- est.R0.AR(AR = ic_regiao[,2], pop.size = pop_suscetível[,1], S0 = 1)

# Estimate R (até Maio)
estimate.R(epid = ic_regiao_1maio, GT = 1)





# ARTIGO
# Rate of exponential growth (r) 
## nº novos casos per capita / unidade de tempo (dia)
r <- (covid19pt[1:66,4:10] - lag(covid19pt[1:66, 4:10])) / 66

## Fazer a média, removendo primeira linha de NA's
r_média <- as.data.frame(lapply(r[-1,1:7], mean))

# Mean generation interval (Tc)














# MÉTODO SIR - Portugal
## Variação do nº suscetíveis

suscetiveis_norte <- as.data.frame(norte - covid19pt$obitos_arsnorte)
suscetiveis_centro <- as.data.frame(centro - covid19pt$obitos_arscentro)
suscetiveis_lisboa <- as.data.frame(lisboa - covid19pt$obitos_arslvt)
suscetiveis_alentejo <- as.data.frame(alentejo - covid19pt$obitos_arsalentejo)
suscetiveis_algarve <- as.data.frame(algarve - covid19pt$obitos_arsalgarve)
suscetiveis_acores <- as.data.frame(acores - covid19pt$obitos_acores)
suscetiveis_madeira <- as.data.frame(madeira - covid19pt$obitos_madeira)

suscetiveis <- cbind(covid19pt$data, suscetiveis_norte, suscetiveis_centro, suscetiveis_lisboa, suscetiveis_alentejo, suscetiveis_algarve, suscetiveis_acores, suscetiveis_madeira)
names(suscetiveis) <- c("Data", "Norte", "Centro", "Lisboa", "Alentejo", "Algarve", "Açores", "Madeira")

suscetiveis_dia <- cbind(suscetiveis$Data, (suscetiveis[, 2:8] - lag(suscetiveis[, 2:8])))
names(suscetiveis_dia)[1] <- "Data"

## Variação do nº infetados

infetados <- cbind(covid19pt$data, covid19pt[, 4:10])
names(infetados) <- c("Data", "Norte", "Centro", "Lisboa", "Alentejo", "Algarve", "Açores", "Madeira")

infetados_dia <- cbind(covid19pt$data, (covid19pt[, 4:10]- lag(covid19pt[, 4:10])))
names(infetados_dia) <- c("Data", "Norte", "Centro", "Lisboa", "Alentejo", "Algarve", "Açores", "Madeira")


##Taxa de recuperação

##Nº de infetados recuperados

##Taxa de contacto

##Nº de suscetíveis infetados

##Coeficiente de transmissão


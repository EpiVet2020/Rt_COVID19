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



# REGIÕES comparar regiões relativamente a:
## Nº de casos 

casos_região <- as.data.frame(t(as.data.frame(lapply(covid19pt[,4:10], last))))

casos_região <- casos_região %>% 
  rownames_to_column(var="Regiões")
names(casos_região)[2] <- "N_casos"


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
portugal = 10295909

### Criar uma tabela com uma coluna para as Regiãoes e outra para o número de pessoas nessa Região
populacao_regioes <- as.data.frame(c(norte, centro, lisboa, alentejo, algarve, acores, madeira), 
                                   c("norte", "centro", "lisboa", "alentejo", "algarve", "açores", "madeira"))
colnames(populacao_regioes) <- "População"
populacao_regioes_invertido <- t(populacao_regioes)

## Subtrair mortalidade por região à população para obter população suscetível sem doença
mortes_região <- as.data.frame(t(as.data.frame(lapply(covid19pt[, 49:55], last))))

mortes_região <- mortes_região %>% 
  rownames_to_column(var="Regiões")
names(mortes_região)[2] <- "N_mortes"

pop_suscetível <- as.data.frame(populacao_regioes[,1] - mortes_região[,2])

## Attack rate ou Incidência cumulativa
ic_regiao <- as.data.frame(t(casos_região[,2])*100 / pop_suscetível) %>% 
  rownames_to_column(var="Regiao")
colnames(ic_regiao)[2] <- "IC"
ic_regiao[,1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

# Estimativa R0 segundo attack rate
R0 <- est.R0.AR(AR = ic_regiao[,2], pop.size =  populacao_regioes[,1], S0 = 1)


#Taxa de incidência 
pop_rep <- as.data.frame(t(populacao_regioes[rep(seq_len(ncol(populacao_regioes)), each=nrow(covid19pt))]))

incidencia_região <- as.data.frame(covid19pt[, 4:10] - lag(covid19pt[, 4:10]))*100 / (pop_rep - covid19pt[, 4:10] - covid19pt[, 49:55])





# MÉTODO SIR
## Variação do nº suscetíveis

suscetiveis_norte <- as.data.frame(norte - covid19pt$obitos_arsnorte - (covid19pt$confirmados_arsnorte - lag(covid19pt$confirmados_arsnorte)))
suscetiveis_centro <- as.data.frame(centro - covid19pt$obitos_arscentro - (covid19pt$confirmados_arscentro - lag(covid19pt$confirmados_arscentro)))
suscetiveis_lisboa <- as.data.frame(lisboa - covid19pt$obitos_arslvt- (covid19pt$confirmados_arslvt - lag(covid19pt$confirmados_arslvt)))
suscetiveis_alentejo <- as.data.frame(alentejo - covid19pt$obitos_arsalentejo - (covid19pt$confirmados_arsalentejo - lag(covid19pt$confirmados_arsalentejo)))
suscetiveis_algarve <- as.data.frame(algarve - covid19pt$obitos_arsalgarve - (covid19pt$confirmados_arsalgarve - lag(covid19pt$confirmados_arsalgarve)))
suscetiveis_acores <- as.data.frame(acores - covid19pt$obitos_acores - (covid19pt$confirmados_acores - lag(covid19pt$confirmados_acores)))
suscetiveis_madeira <- as.data.frame(madeira - covid19pt$obitos_madeira - (covid19pt$confirmados_madeira - lag(covid19pt$confirmados_madeira)))

suscetiveis <- cbind(covid19pt$data, suscetiveis_norte, suscetiveis_centro, suscetiveis_lisboa, suscetiveis_alentejo, suscetiveis_algarve, suscetiveis_acores, suscetiveis_madeira)
names(suscetiveis) <- c("Data", "Norte", "Centro", "Lisboa", "Alentejo", "Algarve", "Açores", "Madeira")

suscetiveis_dia <- cbind(suscetiveis$Data, (suscetiveis[, 2:8] - lag(suscetiveis[, 2:8])))
names(suscetiveis_dia)[1] <- "Data"

## Variação do nº infetados

infetados <- cbind(covid19pt$data, covid19pt[, 4:10])
names(infetados) <- c("Data", "Norte", "Centro", "Lisboa", "Alentejo", "Algarve", "Açores", "Madeira")
infetados_melt <- melt(infetados, id.vars = "Data")

ggplot(infetados_melt, aes(x = Data, y = value, col = variable)) + 
  geom_line() + 
  xlab("Data") + 
  ylab("Infetados") + 
  scale_x_date(date_breaks = "months", date_labels = "%b")


infetados_dia <- cbind(covid19pt$data, (covid19pt[, 4:10]- lag(covid19pt[, 4:10])))
names(infetados_dia) <- c("Data", "Norte", "Centro", "Lisboa", "Alentejo", "Algarve", "Açores", "Madeira")
infetados_dia_melt <- melt(infetados_dia, id.vars = "Data")

ggplot(infetados_dia_melt, aes(x = Data, y = value, col = variable)) + 
  geom_line() + 
  xlab("Data") + 
  ylab("Infetados por dia") + 
  scale_x_date(date_breaks = "months", date_labels = "%b")

##R0 até 1 de Maio
casos_maio <- as.data.frame(t(as.data.frame(lapply(covid19pt[1:65, 4:10], last))))
obitos_maio <- as.data.frame(t(as.data.frame(lapply(covid19pt[1:65, 49:55], last))))
pop_susc_maio <- as.data.frame(populacao_regioes_invertido[,1] - obitos_maio[,1])
ic_maio <- as.data.frame(casos_maio[, 1] / pop_susc_maio[,1] *100)

#R0 Portugal - SIR

ic_pt <- as.data.frame((covid19pt$confirmados - lag(covid19pt$confirmados)) /  (portugal - covid19pt$confirmados)*100)
ic_pt <- cbind(covid19pt$data, ic_pt)
ic_pt$observation <- 1:nrow(ic_pt)                       
ic_pt <- ic_pt[, c(3,1,2)]                       
names(ic_pt) <- c("Dias", "Data", "Incidencia")
                       
  
ggplot(ic_pt, aes(x=Dias , y=Incidencia)) + 
  geom_line() + 
  xlab("Dias desde o início") + 
  ylab("Taxa de incidencia (por 100 habitantes)") +
  scale_x_continuous(breaks = c(20, 40,60,80,100,120,140,160,180,200,220,240))

lm(formula = log(Incidencia) ~ Dias, data = ic_pt)


##Suscetiveis = -r.I.S
S <- 






       

################################################################################
################################# DADOS DE 2016 ################################
################################################################################

# importando as bibliotecas
library(data.table)
library(dplyr)
library(ggplot2)

#####################
# leitura dos dados #
#####################

# colunas relacionadas a veiculos
nquero = c('PLACA_VEICULO', 'UF_VEICULO', 'CIDADE_VEICULO', 
           'DESCR_COR_VEICULO', 'DESCR_MARCA_VEICULO', 'ANO_FABRICACAO',
           'DESCR_TIPO_VEICULO', 'LOGRADOURO', 'LATITUDE',
           'LONGITUDE')

# --------------- #
# janeiro de 2016 #
# --------------- #
dados1601 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2016_1.txt', 
                  drop = nquero)

# ----------------- #
# fevereiro de 2016 #
# ----------------- #
dados1602 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2016_2.txt', 
                  drop = nquero)

# ------------- #
# marco de 2016 #
# ------------- #
dados1603 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2016_3.txt', 
                  drop = nquero)

# ------------- #
# abril de 2016 #
# ------------- #
dados1604 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2016_4.txt', 
                  drop = nquero)

# ------------ #
# maio de 2016 #
# ------------ #
dados1605 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2016_5.txt', 
                  drop = nquero)

# ------------- #
# junho de 2016 #
# ------------- #
dados1606 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2016_6.txt', 
                  drop = nquero)

# ------------- #
# julho de 2016 #
# ------------- #
dados1607 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2016_7.txt', 
                  drop = nquero)

# -------------- #
# agosto de 2016 #
# -------------- #
dados1608 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2016_8.txt', 
                  drop = nquero)

# ---------------- #
# setembro de 2016 #
# ---------------- #
dados1609 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2016_9.txt', 
                  drop = nquero)

# --------------- #
# outubro de 2016 #
# --------------- #
dados1610 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2016_10.txt', 
                  drop = nquero)

# ---------------- #
# novembro de 2016 #
# ---------------- #
dados1611 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2016_11.txt', 
                  drop = nquero)

# ---------------- #
# dezembro de 2016 #
# ---------------- #
dados1612 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2016_12.txt', 
                  drop = nquero)

#########################################################
# grafico de linhas com o numero de BO distintos no mes #
#########################################################

meses <- c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho',
           'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')

vol_2016 <- c(nrow(dados1601 %>% distinct(NUM_BO)),
              nrow(dados1602 %>% distinct(NUM_BO)),
              nrow(dados1603 %>% distinct(NUM_BO)),
              nrow(dados1604 %>% distinct(NUM_BO)),
              nrow(dados1605 %>% distinct(NUM_BO)),
              nrow(dados1606 %>% distinct(NUM_BO)),
              nrow(dados1607 %>% distinct(NUM_BO)),
              nrow(dados1608 %>% distinct(NUM_BO)),
              nrow(dados1609 %>% distinct(NUM_BO)),
              nrow(dados1610 %>% distinct(NUM_BO)),
              nrow(dados1611 %>% distinct(NUM_BO)),
              nrow(dados1612 %>% distinct(NUM_BO))
)

numeros <- 1:12

volumetria2016 <- data.frame(meses, vol_2016, numeros)
volumetria2016$meses <- factor(volumetria2016$meses, level = meses)


ggplot(volumetria2016, aes(x = meses, y = vol_2016)) +
  geom_point(mapping = aes(x = meses, y = vol_2016)) +
  geom_line(mapping = aes(x = numeros, y = vol_2016)) +
  labs(x = 'Mês', y = 'Número total de feminicídio', title = '2016')

###########################################
# concatenando todos os dados em um unico #
###########################################

dados2016 = rbind(dados1601, dados1602, dados1603, dados1604,
                  dados1605, dados1606, dados1607, dados1608,
                  dados1609, dados1610, dados1611, dados1612)

###################################
# numeros distintos de BO em 2016 #
###################################
dados2016 %>% distinct(NUM_BO) %>% count() # sao 59 feminicidios em 2016

#########################################
# numero de linhas para cada ocorrencia #
#########################################
linhas_ocorrencias <- dados2016 %>% 
  group_by(NUM_BO) %>% count()

###################################
# quantidade de BO's com n linhas #
###################################
linhas_ocorrencias %>% group_by(n) %>% count()
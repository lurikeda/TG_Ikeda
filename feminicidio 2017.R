################################################################################
################################# DADOS DE 2017 ################################
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
# janeiro de 2017 #
# --------------- #
dados1701 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2017_1.txt', 
                  drop = nquero)

# ----------------- #
# fevereiro de 2017 #
# ----------------- #
dados1702 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2017_2.txt', 
                  drop = nquero)

# ------------- #
# marco de 2017 #
# ------------- #
dados1703 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2017_3.txt', 
                  drop = nquero)

# ------------- #
# abril de 2017 #
# ------------- #
dados1704 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2017_4.txt', 
                  drop = nquero)

# ------------ #
# maio de 2017 #
# ------------ #
dados1705 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2017_5.txt', 
                  drop = nquero)

# ------------- #
# junho de 2017 #
# ------------- #
dados1706 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2017_6.txt', 
                  drop = nquero)

# ------------- #
# julho de 2017 #
# ------------- #
dados1707 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2017_7.txt', 
                  drop = nquero)

# -------------- #
# agosto de 2017 #
# -------------- #
dados1708 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2017_8.txt', 
                  drop = nquero)

# ---------------- #
# setembro de 2017 #
# ---------------- #
dados1709 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2017_9.txt', 
                  drop = nquero)

# --------------- #
# outubro de 2017 #
# --------------- #
dados1710 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2017_10.txt', 
                  drop = nquero)

# ---------------- #
# novembro de 2017 #
# ---------------- #
dados1711 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2017_11.txt', 
                  drop = nquero)

# ---------------- #
# dezembro de 2017 #
# ---------------- #
dados1712 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2017_12.txt', 
                  drop = nquero)

#########################################################
# grafico de linhas com o numero de BO distintos no mes #
#########################################################

meses <- c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho',
           'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')

vol_2017 <- c(nrow(dados1701 %>% distinct(NUM_BO)),
              nrow(dados1702 %>% distinct(NUM_BO)),
              nrow(dados1703 %>% distinct(NUM_BO)),
              nrow(dados1704 %>% distinct(NUM_BO)),
              nrow(dados1705 %>% distinct(NUM_BO)),
              nrow(dados1706 %>% distinct(NUM_BO)),
              nrow(dados1707 %>% distinct(NUM_BO)),
              nrow(dados1708 %>% distinct(NUM_BO)),
              nrow(dados1709 %>% distinct(NUM_BO)),
              nrow(dados1710 %>% distinct(NUM_BO)),
              nrow(dados1711 %>% distinct(NUM_BO)),
              nrow(dados1712 %>% distinct(NUM_BO))
)

numeros <- 1:12

volumetria2017 <- data.frame(meses, vol_2017, numeros)
volumetria2017$meses <- factor(volumetria2017$meses, level = meses)


ggplot(volumetria2017, aes(x = meses, y = vol_2017)) +
  geom_point(mapping = aes(x = meses, y = vol_2017)) +
  geom_line(mapping = aes(x = numeros, y = vol_2017)) +
  labs(x = 'Mês', y = 'Número total de feminicídio', title = '2017')

###########################################
# concatenando todos os dados em um unico #
###########################################

dados2017 = rbind(dados1701, dados1702, dados1703, dados1704,
                  dados1705, dados1706, dados1707, dados1708,
                  dados1709, dados1710, dados1711, dados1712)

###################################
# numeros distintos de BO em 2017 #
###################################
dados2017 %>% distinct(NUM_BO) %>% count() # sao 109 feminicidios em 2017

#########################################
# numero de linhas para cada ocorrencia #
#########################################
linhas_ocorrencias <- dados2017 %>% 
  group_by(NUM_BO) %>% count()

###################################
# quantidade de BO's com n linhas #
###################################
linhas_ocorrencias %>% group_by(n) %>% count()
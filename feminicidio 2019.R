################################################################################
################################# DADOS DE 2019 ################################
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
# janeiro de 2019 #
# --------------- #
dados1901 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2019_1.txt', 
                  drop = nquero)

# ----------------- #
# fevereiro de 2019 #
# ----------------- #
dados1902 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2019_2.txt', 
                  drop = nquero)

# ------------- #
# marco de 2019 #
# ------------- #
dados1903 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2019_3.txt', 
                  drop = nquero)

# ------------- #
# abril de 2019 #
# ------------- #
dados1904 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2019_4.txt', 
                  drop = nquero)

# ------------ #
# maio de 2019 #
# ------------ #
dados1905 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2019_5.txt', 
                  drop = nquero)

# ------------- #
# junho de 2019 #
# ------------- #
dados1906 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2019_6.txt', 
                  drop = nquero)

# ------------- #
# julho de 2019 #
# ------------- #
dados1907 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2019_7.txt', 
                  drop = nquero)

# -------------- #
# agosto de 2019 #
# -------------- #
dados1908 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2019_8.txt', 
                  drop = nquero)

# ---------------- #
# setembro de 2019 #
# ---------------- #
dados1909 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2019_9.txt', 
                  drop = nquero)

# --------------- #
# outubro de 2019 #
# --------------- #
dados1910 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2019_10.txt', 
                  drop = nquero)

# ---------------- #
# novembro de 2019 #
# ---------------- #
dados1911 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2019_11.txt', 
                  drop = nquero)

# ---------------- #
# dezembro de 2019 #
# ---------------- #
dados1912 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2019_12.txt', 
                  drop = nquero)

#########################################################
# grafico de linhas com o numero de BO distintos no mes #
#########################################################

meses <- c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho',
           'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')

vol_2019 <- c(nrow(dados1901 %>% distinct(NUM_BO)),
              nrow(dados1902 %>% distinct(NUM_BO)),
              nrow(dados1903 %>% distinct(NUM_BO)),
              nrow(dados1904 %>% distinct(NUM_BO)),
              nrow(dados1905 %>% distinct(NUM_BO)),
              nrow(dados1906 %>% distinct(NUM_BO)),
              nrow(dados1907 %>% distinct(NUM_BO)),
              nrow(dados1908 %>% distinct(NUM_BO)),
              nrow(dados1909 %>% distinct(NUM_BO)),
              nrow(dados1910 %>% distinct(NUM_BO)),
              nrow(dados1911 %>% distinct(NUM_BO)),
              nrow(dados1912 %>% distinct(NUM_BO))
)

numeros <- 1:12

volumetria2019 <- data.frame(meses, vol_2019, numeros)
volumetria2019$meses <- factor(volumetria2019$meses, level = meses)


ggplot(volumetria2019, aes(x = meses, y = vol_2019)) +
  geom_point(mapping = aes(x = meses, y = vol_2019)) +
  geom_line(mapping = aes(x = numeros, y = vol_2019)) +
  labs(x = 'Mês', y = 'Número total de feminicídio', title = '2019')


###########################################
# concatenando todos os dados em um unico #
###########################################

dados2019 = rbind(dados1901, dados1902, dados1903, dados1904,
                  dados1905, dados1906, dados1907, dados1908,
                  dados1909, dados1910, dados1911, dados1912)

###################################
# numeros distintos de BO em 2019 #
###################################
dados2019 %>% distinct(NUM_BO) %>% count() # sao 174 feminicidios em 2019

#########################################
# numero de linhas para cada ocorrencia #
#########################################
linhas_ocorrencias <- dados2019 %>% 
  group_by(NUM_BO) %>% count()

###################################
# quantidade de BO's com n linhas #
###################################
linhas_ocorrencias %>% group_by(n) %>% count()

############
# RUBRICAS #
############
rubrica <- dados2019 %>%
  distinct(RUBRICA)

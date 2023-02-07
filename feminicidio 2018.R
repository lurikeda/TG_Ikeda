################################################################################
################################# DADOS DE 2018 ################################
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
# janeiro de 2018 #
# --------------- #
dados1801 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2018_1.txt', 
                  drop = nquero)

# ----------------- #
# fevereiro de 2018 #
# ----------------- #
dados1802 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2018_2.txt', 
                  drop = nquero)

# ------------- #
# marco de 2018 #
# ------------- #
dados1803 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2018_3.txt', 
                  drop = nquero)

# ------------- #
# abril de 2018 #
# ------------- #
dados1804 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2018_4.txt', 
                  drop = nquero)

# ------------ #
# maio de 2018 #
# ------------ #
dados1805 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2018_5.txt', 
                  drop = nquero)

# ------------- #
# junho de 2018 #
# ------------- #
dados1806 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2018_6.txt', 
                  drop = nquero)

# ------------- #
# julho de 2018 #
# ------------- #
dados1807 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2018_7.txt', 
                  drop = nquero)

# -------------- #
# agosto de 2018 #
# -------------- #
dados1808 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2018_8.txt', 
                  drop = nquero)

# ---------------- #
# setembro de 2018 #
# ---------------- #
dados1809 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2018_9.txt', 
                  drop = nquero)

# --------------- #
# outubro de 2018 #
# --------------- #
dados1810 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2018_10.txt', 
                  drop = nquero)

# ---------------- #
# novembro de 2018 #
# ---------------- #
dados1811 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2018_11.txt', 
                  drop = nquero)

# ---------------- #
# dezembro de 2018 #
# ---------------- #
dados1812 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2018_12.txt', 
                  drop = nquero)

#########################################################
# grafico de linhas com o numero de BO distintos no mes #
#########################################################

meses <- c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho',
             'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')

vol_2018 <- c(nrow(dados1801 %>% distinct(NUM_BO)),
              nrow(dados1802 %>% distinct(NUM_BO)),
              nrow(dados1803 %>% distinct(NUM_BO)),
              nrow(dados1804 %>% distinct(NUM_BO)),
              nrow(dados1805 %>% distinct(NUM_BO)),
              nrow(dados1806 %>% distinct(NUM_BO)),
              nrow(dados1807 %>% distinct(NUM_BO)),
              nrow(dados1808 %>% distinct(NUM_BO)),
              nrow(dados1809 %>% distinct(NUM_BO)),
              nrow(dados1810 %>% distinct(NUM_BO)),
              nrow(dados1811 %>% distinct(NUM_BO)),
              nrow(dados1812 %>% distinct(NUM_BO))
              )

numeros <- 1:12

volumetria2018 <- data.frame(meses, vol_2018, numeros)
volumetria2018$meses <- factor(volumetria2018$meses, level = meses)


ggplot(volumetria2018, aes(x = meses, y = vol_2018)) +
  geom_point(mapping = aes(x = meses, y = vol_2018)) +
  geom_line(mapping = aes(x = numeros, y = vol_2018)) +
  labs(x = 'Mês', y = 'Número total de feminicídio', title = '2018')

###########################################
# concatenando todos os dados em um unico #
###########################################

dados2018 = rbind(dados1801, dados1802, dados1803, dados1804,
                  dados1805, dados1806, dados1807, dados1808,
                  dados1809, dados1810, dados1811, dados1812)

###################################
# numeros distintos de BO em 2018 #
###################################
dados2018 %>% distinct(NUM_BO) %>% count() # sao 132 feminicidios em 2018

#########################################
# numero de linhas para cada ocorrencia #
#########################################
linhas_ocorrencias <- dados2018 %>% 
  group_by(NUM_BO) %>% count()

###################################
# quantidade de BO's com n linhas #
###################################
linhas_ocorrencias %>% group_by(n) %>% count()

############
# RUBRICAS #
############
rubrica <- dados2018 %>%
  distinct(RUBRICA)

rubrica$proporcao <- dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(sum(RUBRICA == rubrica)) %>%
  colMeans()


for (i in length(rubrica)) {
  proporcao_rubrica[i] <- dados2018 %>% 
    group_by(NUM_BO) %>% 
    summarise(prop := sum(RUBRICA == rubrica[i])) %>% 
    colMeans()
}

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(suicidio_consumado = sum(RUBRICA == rubrica[1])) %>%
  colMeans()

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(homicidio_qualificado = sum(RUBRICA == rubrica[2])) %>% 
  colMeans()

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(violencia_domestica = sum(RUBRICA == rubrica[3])) %>% 
  colMeans()

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(suicidio_tentado = sum(RUBRICA == rubrica[4])) %>% 
  colMeans()

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(ameaca = sum(RUBRICA == rubrica[6])) %>% 
  colMeans()

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(lesao_corporal1 = sum(RUBRICA == rubrica[9])) %>% 
  colMeans()

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(incendio = sum(RUBRICA == rubrica[10])) %>% 
  colMeans()

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(homicidio_simples = sum(RUBRICA == rubrica[11])) %>% 
  colMeans()

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(lesao_corporal2 = sum(RUBRICA == rubrica[13])) %>% 
  colMeans()

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(aborto = sum(RUBRICA == rubrica[15])) %>% 
  colMeans()

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(cadaver = sum(RUBRICA == rubrica[20])) %>% 
  colMeans()

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(ameaca = sum(RUBRICA == rubrica[6])) %>% 
  colMeans()

dados2018 %>%
  group_by(NUM_BO) %>% 
  summarise(medida_protetiva = sum(RUBRICA == rubrica[21])) %>% 
  colMeans()

dt <- dados2018 %>% group_by(NUM_BO) %>% 
  mutate(RUBRICA = paste(RUBRICA)) %>% ungroup() %>%
  select(RUBRICA) %>% count()

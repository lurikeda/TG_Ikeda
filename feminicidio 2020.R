################################################################################
################################# DADOS DE 2020 ################################
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
# janeiro de 2020 #
# --------------- #
dados2001 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2020_1.txt', 
                  drop = nquero)

# ----------------- #
# fevereiro de 2020 #
# ----------------- #
dados2002 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2020_2.txt', 
                  drop = nquero)

# ------------- #
# marco de 2020 #
# ------------- #
dados2003 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2020_3.txt', 
                  drop = nquero)

# ------------- #
# abril de 2020 #
# ------------- #
dados2004 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2020_4.txt', 
                  drop = nquero)

# ------------ #
# maio de 2020 #
# ------------ #
dados2005 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2020_5.txt', 
                  drop = nquero)

# ------------- #
# junho de 2020 #
# ------------- #
dados2006 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2020_6.txt', 
                  drop = nquero)

# ------------- #
# julho de 2020 #
# ------------- #
dados2007 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2020_7.txt', 
                  drop = nquero)

# -------------- #
# agosto de 2020 #
# -------------- #
dados2008 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2020_8.txt', 
                  drop = nquero)

# ---------------- #
# setembro de 2020 #
# ---------------- #
dados2009 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2020_9.txt', 
                  drop = nquero)

# --------------- #
# outubro de 2020 #
# --------------- #
dados2010 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2020_10.txt', 
                  drop = nquero)

# ---------------- #
# novembro de 2020 #
# ---------------- #
dados2011 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2020_11.txt', 
                  drop = nquero)

# ---------------- #
# dezembro de 2020 #
# ---------------- #
dados2012 = fread(file = 'D:/Luri/4 - UFSCAR/dados/DadosBO_2020_12.txt', 
                  drop = nquero)

#########################################################
# grafico de linhas com o numero de BO distintos no mes #
#########################################################

meses <- c('Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho',
           'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro')

vol_2020 <- c(nrow(dados2001 %>% distinct(NUM_BO)),
              nrow(dados2002 %>% distinct(NUM_BO)),
              nrow(dados2003 %>% distinct(NUM_BO)),
              nrow(dados2004 %>% distinct(NUM_BO)),
              nrow(dados2005 %>% distinct(NUM_BO)),
              nrow(dados2006 %>% distinct(NUM_BO)),
              nrow(dados2007 %>% distinct(NUM_BO)),
              nrow(dados2008 %>% distinct(NUM_BO)),
              nrow(dados2009 %>% distinct(NUM_BO)),
              nrow(dados2010 %>% distinct(NUM_BO)),
              nrow(dados2011 %>% distinct(NUM_BO)),
              nrow(dados2012 %>% distinct(NUM_BO))
)

numeros <- 1:12

volumetria2020 <- data.frame(meses, vol_2020, numeros)
volumetria2020$meses <- factor(volumetria2020$meses, level = meses)


ggplot(volumetria2020, aes(x = meses, y = vol_2020)) +
  geom_point(mapping = aes(x = meses, y = vol_2020)) +
  geom_line(mapping = aes(x = numeros, y = vol_2020)) +
  labs(x = 'Mês', y = 'Número total de feminicídio', title = '2020')

###########################################
# concatenando todos os dados em um unico #
###########################################

dados2020 = rbind(dados2001, dados2002, dados2003, dados2004,
                  dados2005, dados2006, dados2007, dados2008,
                  dados2009, dados2010, dados2011, dados2012)

###################################
# numeros distintos de BO em 2020 #
###################################
dados2020 %>% distinct(NUM_BO) %>% count() # sao 165 feminicidios em 2020

#########################################
# numero de linhas para cada ocorrencia #
#########################################
linhas_ocorrencias <- dados2020 %>% 
  group_by(NUM_BO) %>% count()

###################################
# quantidade de BO's com n linhas #
###################################
linhas_ocorrencias %>% group_by(n) %>% count()

############
# RUBRICAS #
############
rubrica <- dados2020 %>%
  distinct(RUBRICA)

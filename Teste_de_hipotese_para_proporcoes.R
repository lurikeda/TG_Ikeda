library(readr)
dados <- read_csv("~/2022 01 - UFSCAR/TG/dados/TG1_dados_trabalhados - dados.csv")


################################################################################
#               TESTE DE HIPOSETE DE PROPORCAO DE FAIXAS ESTARIAS              #
################################################################################

#-------------------------------------------------------------------------------
# INFORMACOES PARA O TESTE DE HIPOTESE

# criando novas colunas dummy com a faixa etaria da vitima
dados$prop_faixa_etaria1 = ifelse(dados$faixa_etaria == "0 a 4 anos", 1, 0)
dados$prop_faixa_etaria2 = ifelse(dados$faixa_etaria == "5 a 9 anos", 1, 0)
dados$prop_faixa_etaria3 = ifelse(dados$faixa_etaria == "10 a 14 anos", 1, 0)
dados$prop_faixa_etaria4 = ifelse(dados$faixa_etaria == "15 a 19 anos", 1, 0)
dados$prop_faixa_etaria5 = ifelse(dados$faixa_etaria == "20 a 24 anos", 1, 0)
dados$prop_faixa_etaria6 = ifelse(dados$faixa_etaria == "25 a 29 anos", 1, 0)
dados$prop_faixa_etaria7 = ifelse(dados$faixa_etaria == "30 a 34 anos", 1, 0)
dados$prop_faixa_etaria8 = ifelse(dados$faixa_etaria == "35 a 39 anos", 1, 0)
dados$prop_faixa_etaria9 = ifelse(dados$faixa_etaria == "40 a 44 anos", 1, 0)
dados$prop_faixa_etaria10 = ifelse(dados$faixa_etaria == "45 a 49 anos", 1, 0)
dados$prop_faixa_etaria11 = ifelse(dados$faixa_etaria == "50 a 54 anos", 1, 0)
dados$prop_faixa_etaria12 = ifelse(dados$faixa_etaria == "55 a 59 anos", 1, 0)
dados$prop_faixa_etaria13 = ifelse(dados$faixa_etaria == "60 a 64 anos", 1, 0)
dados$prop_faixa_etaria14 = ifelse(dados$faixa_etaria == "65 a 69 anos", 1, 0)
dados$prop_faixa_etaria15 = ifelse(dados$faixa_etaria == "70 anos ou mais", 1, 0)

# propocao de pessoas da faixa etaria verdadeira (dados do IBGE)
prop_idade = c(0.05400284225, 0.06050557685, 0.06545799061, 0.07587959175,
               0.06976443736, 0.07161620947, 0.08096119891, 0.07798975066,
               0.07557814048, 0.06967830843, 0.06924766375, 0.06063477025,
               0.05197881228, 0.04134188881, 0.07536281814)

#-------------------------------------------------------------------------------
# TESTE DE IPOTESES DE PROPORCOES

teste_prop_idade1 = prop.test(x = sum(dados$prop_faixa_etaria1),
                              n = nrow(dados),
                              p = prop_idade[1], #p0
                              alternative = "greater")
# valor-p = 1 --> aceita H0: a prop_vitima eh <= prop_populacional 

teste_prop_idade2 = prop.test(sum(dados$prop_faixa_etaria2),
                              nrow(dados),
                              prop_idade[2], 
                              alternative = "greater")
# valor-p = 1 --> aceita H0: a prop_vitima eh <= prop_populacional

teste_prop_idade3 = prop.test(sum(dados$prop_faixa_etaria3),
                              nrow(dados),
                              prop_idade[3], 
                              alternative = "greater")
# valor-p = 1 --> aceita H0: a prop_vitima eh <= prop_populacional

teste_prop_idade4 = prop.test(sum(dados$prop_faixa_etaria4),
                              nrow(dados),
                              prop_idade[4], 
                              alternative = "greater")
# valor-p = 0.9733 --> aceita H0: a prop_vitima eh <= prop_populacional

teste_prop_idade5 = prop.test(sum(dados$prop_faixa_etaria5),
                              nrow(dados),
                              prop_idade[5], 
                              alternative = "greater")
# valor-p = 4.245e-07 --> rejeita H0: a prop_vitima eh > prop_populacional

teste_prop_idade6 = prop.test(sum(dados$prop_faixa_etaria6),
                              nrow(dados),
                              prop_idade[6], 
                              alternative = "greater")
# valor-p = 2.2e-16 --> rejeita H0: a prop_vitima eh > prop_populacional

teste_prop_idade7 = prop.test(sum(dados$prop_faixa_etaria7),
                              nrow(dados),
                              prop_idade[7], 
                              alternative = "greater")
# valor-p = 2.2e-16 --> rejeita H0: a prop_vitima eh > prop_populacional

teste_prop_idade8 = prop.test(sum(dados$prop_faixa_etaria8),
                              nrow(dados),
                              prop_idade[8], 
                              alternative = "greater")
# valor-p = 2.535e-11 --> rejeita H0: a prop_vitima eh > prop_populacional

teste_prop_idade9 = prop.test(sum(dados$prop_faixa_etaria9),
                              nrow(dados),
                              prop_idade[9], 
                              alternative = "greater")
# valor-p = 3.864e-05 --> rejeita H0: a prop_vitima eh > prop_populacional

teste_prop_idade10 = prop.test(sum(dados$prop_faixa_etaria10),
                              nrow(dados),
                              prop_idade[10], 
                              alternative = "greater")
# valor-p = 0.9189 --> aceita H0: a prop_vitima eh <= prop_populacional

teste_prop_idade11 = prop.test(sum(dados$prop_faixa_etaria11),
                              nrow(dados),
                              prop_idade[11], 
                              alternative = "greater")
# valor-p = 0.9463 --> aceita H0: a prop_vitima eh <= prop_populacional

teste_prop_idade12 = prop.test(sum(dados$prop_faixa_etaria12),
                              nrow(dados),
                              prop_idade[12], 
                              alternative = "greater")
# valor-p = 0.9999 --> aceita H0: a prop_vitima eh <= prop_populacional

teste_prop_idade13 = prop.test(sum(dados$prop_faixa_etaria13),
                              nrow(dados),
                              prop_idade[13], 
                              alternative = "greater")
# valor-p = 1 --> aceita H0: a prop_vitima eh <= prop_populacional

teste_prop_idade14 = prop.test(sum(dados$prop_faixa_etaria14),
                              nrow(dados),
                              prop_idade[14], 
                              alternative = "greater")
# valor-p = 1 --> aceita H0: a prop_vitima eh <= prop_populacional

teste_prop_idade15 = prop.test(sum(dados$prop_faixa_etaria15),
                              nrow(dados),
                              prop_idade[15], 
                              alternative = "greater")
# valor-p = 1 --> aceita H0: a prop_vitima eh <= prop_populacional

# DE 20 A 44 ANOS EH QUANDO A PROPORCAO DE VITIMAS EH MAIOR QUE A PROPORCAO DE MULHERES #

################################################################################
#                         TESTE DE HIPOSETE DE COR PELE                        #
################################################################################

#-------------------------------------------------------------------------------
# INFORMACOES PARA O TESTE DE HIPOTESE

# criando novas colunas dummy com a cor de pele da vitima
dados$prop_cor_branca = ifelse(dados$COR_PELE == "Branca", 1, 0)
dados$prop_cor_amarela = ifelse(dados$COR_PELE == "Amarela", 1, 0)
dados$prop_cor_parda = ifelse(dados$COR_PELE == "Parda", 1, 0)
dados$prop_cor_preta = ifelse(dados$COR_PELE == "Preta", 1, 0)

# propocao de pessoas da cor (dados do IBGE)
prop_cor_branca = 0.6287363253
prop_cor_amarela = 0.0118873288
prop_cor_parda = 0.2903781549
prop_cor_preta = 0.06658626927

#-------------------------------------------------------------------------------
# TESTE DE IPOTESES DE PROPORCOES

teste_prop_cor_branca = prop.test(sum(dados$prop_cor_branca),
                                  nrow(dados),
                                  prop_cor_branca,
                                  alternative = "greater")
# valor-p = 1 --> aceita H0: a prop_vitima branca eh <= prop_populacional branca

teste_prop_cor_amarela = prop.test(sum(dados$prop_cor_amarela),
                                  nrow(dados),
                                  prop_cor_amarela,
                                  alternative = "greater")
# valor-p = 0.9753 --> aceita H0: a prop_vitima amarela eh <= prop_populacional amarela

teste_prop_cor_parda = prop.test(sum(dados$prop_cor_parda),
                                  nrow(dados),
                                  prop_cor_parda,
                                  alternative = "greater")
# valor-p = 0.01122 --> rejeita H0: a prop_vitima parda eh > prop_populacional parda

teste_prop_cor_preta = prop.test(sum(dados$prop_cor_preta),
                                  nrow(dados),
                                  prop_cor_preta,
                                  alternative = "greater")
# valor-p = 0.01122 --> rejeita H0: a prop_vitima preta eh > prop_populacional preta

# MULHERES PRETAS E PARDAS SAO EM PROPORCAO MAIORES VITIMAS #

################################################################################
#                    TESTE DE HIPOSETE DE BRANCAS/NAO-BRANCAS                  #
################################################################################

# INFORMACOES PARA O TESTE DE HIPOTESE

# criando novas colunas dummy se for branca ou nao
dados$prop_nao_eh_branca = ifelse(dados$COR_PELE == "Branca", 0, 1)

# propocao de pessoas nao brancas (dados do IBGE)
prop_nao_eh_branca = 0.3712636747

#-------------------------------------------------------------------------------
# TESTE DE IPOTESES DE PROPORCOES

teste_prop_nao_branca = prop.test(sum(dados$prop_nao_eh_branca),
                                 nrow(dados),
                                 prop_nao_eh_branca,
                                 alternative = "greater")
# valor-p = 3.501*10^-7 --> rejeita H0: a prop_vitima nao brancas eh > prop_populacional nao branca

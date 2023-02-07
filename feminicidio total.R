################################################################################
############################### FEMINICIDIO TOTAL ##############################
################################################################################

anos <- c('2016','2017', '2018', '2019', '2020')
violencia <- c(59, 109, 132, 174, 165)
numeros <- 1:5

total <- data.frame(anos, violencia, numeros)

ggplot(total, aes(x = anos, y = violencia)) +
  geom_point(mapping = aes(x = anos, y = violencia)) +
  labs(x = 'Ano', y = 'Número total de feminicídio', 
       title = 'Volumetria total') + 
  geom_line(mapping = aes(x = numeros, y = violencia))

safra <- c('16-01', '16-02', '16-03', '16-04', '16-05', '16-06',
           '16-07', '16-08', '16-09', '16-10', '16-11', '16-12',
           
           '17-01', '17-02', '17-03', '17-04', '17-05', '17-06',
           '17-07', '17-08', '17-09', '17-10', '17-11', '17-12',
           
           '18-01', '18-02', '18-03', '18-04', '18-05', '18-06',
           '18-07', '18-08', '18-09', '18-10', '18-11', '18-12',
           
           '19-01', '19-02', '19-03', '19-04', '19-05', '19-06',
           '19-07', '19-08', '19-09', '19-10', '19-11', '19-12',
           
           '20-01', '20-02', '20-03', '20-04', '20-05', '20-06',
           '20-07', '20-08', '20-09', '20-10', '20-11', '20-12')

meses <- c(nrow(dados1601 %>% distinct(NUM_BO)),
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
           nrow(dados1612 %>% distinct(NUM_BO)),
           
           nrow(dados1701 %>% distinct(NUM_BO)),
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
           nrow(dados1712 %>% distinct(NUM_BO)),
           
           nrow(dados1801 %>% distinct(NUM_BO)),
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
           nrow(dados1812 %>% distinct(NUM_BO)),
           
           nrow(dados1901 %>% distinct(NUM_BO)),
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
           nrow(dados1912 %>% distinct(NUM_BO)),
           
           nrow(dados2001 %>% distinct(NUM_BO)),
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
           nrow(dados2012 %>% distinct(NUM_BO)))

numeros <- 1:60
total_safra <- as.data.frame(safra, meses, numeros)

ggplot(total_safra, aes(x = safra, y = meses)) +
  geom_point(aes(x = safra, y = meses)) +
  geom_line(mapping = aes(x = numeros, y = meses)) +
  labs(x = 'Ano-Mês', y = 'Número total de feminicídio', title = '2016-2020')+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))


# parece ter uma sazonalidade: baixa nos meses de janeiro, julho e alta no mês 
# de dezembro.

#dados completos
ano2016 <- rbind(dados1601, dados1602, dados1603, dados1604,
                 dados1605, dados1606, dados1607, dados1608,
                 dados1609, dados1610, dados1611, dados1612)

ano2017 <- rbind(dados1701, dados1702, dados1703, dados1704,
                 dados1705, dados1706, dados1707, dados1708,
                 dados1709, dados1710, dados1711, dados1712)

ano2018 <- rbind(dados1801, dados1802, dados1803, dados1804,
                 dados1805, dados1806, dados1807, dados1808,
                 dados1809, dados1810, dados1811, dados1812)

ano2019 <- rbind(dados1901, dados1902, dados1903, dados1904,
                 dados1905, dados1906, dados1907, dados1908,
                 dados1909, dados1910, dados1911, dados1912)

ano2020 <- rbind(dados2001, dados2002, dados2003, dados2004,
                 dados2005, dados2006, dados2007, dados2008,
                 dados2009, dados2010, dados2011, dados2012)

anos_completos <- rbind(ano2016, ano2017, ano2018, ano2019, ano2020)
write.csv(anos_completos, 
          'D:/Luri/4 - UFSCAR/dados/dados completos.csv',
          row.names = FALSE,
          na = '')

rm(list = ls())
graphics.off()
cat("\014")
#Pacotes
library(tidyverse)
library(data.table)
library(stringr)
library(lubridate)
#Leitura do banco de dados inicial
url1 <- "https://raw.githack.com/fsbmat-ufv/brasileiraoTCC/main/dados/campeonato_brasileiro_2020.csv"
dt20 <- data.table::fread(url1, encoding = "UTF-8")
#Mudanca do nome das variaveis
names(dt20)<- c( "ID"             ,
                 "Rodada"         ,
                 "Data"           ,
                 "Horario"        ,
                 "Dia"            ,
                 "Mandante"       ,
                 "Visitante"      ,
                 "Vencedor"       ,
                 "Arena"          ,
                 "GolsMan"        ,
                 "GolsVisit"      ,
                 "EstadoMan"      ,
                 "EstadoVisit"    ,
                 "EstadoVenc")

#Limpeza e organizacao do banco de dados
#Retirada de acentos das palavras
dt20$Mandante <- abjutils::rm_accent(dt20$Mandante)
dt20$Visitante <- abjutils::rm_accent(dt20$Visitante)
dt20$Dia <- abjutils::rm_accent(dt20$Dia)
dt20$Arena <- abjutils::rm_accent(dt20$Arena)
dt20$Vencedor <- abjutils::rm_accent(dt20$Vencedor)
dt20$Rodada <- abjutils::rm_accent(dt20$Rodada)

#Remover espacos entre as strings
dt20$Mandante <- str_squish(dt20$Mandante)
dt20$Mandante <- str_trim(dt20$Mandante, side = c("both", "left", "right"))
dt20$Visitante <- str_squish(dt20$Visitante)
dt20$Visitante <- str_trim(dt20$Visitante, side = c("both", "left", "right"))
dt20$Dia <- str_squish(dt20$Dia)
dt20$Dia <- str_trim(dt20$Dia, side = c("both", "left", "right"))
dt20$Arena <- str_squish(dt20$Arena)
dt20$Arena <- str_trim(dt20$Arena, side = c("both", "left", "right"))
dt20$Vencedor <- str_squish(dt20$Vencedor)
dt20$Vencedor <- str_trim(dt20$Vencedor, side = c("both", "left", "right"))
dt20$Rodada <- str_squish(dt20$Rodada)
dt20$Rodada <- str_trim(dt20$Rodada, side = c("both", "left", "right"))

#Colocar em caixa alta
dt20$Mandante <- str_to_upper(dt20$Mandante)
dt20$Visitante <- str_to_upper(dt20$Visitante)
dt20$Dia <- str_to_upper(dt20$Dia)
dt20$Arena <- str_to_upper(dt20$Arena)
dt20$EstadoVenc <- str_to_upper(dt20$EstadoVenc)
dt20$Vencedor <- str_to_upper(dt20$Vencedor)
dt20$Rodada <- str_to_upper(dt20$Rodada)
dt20$Rodada <- str_replace_all(dt20$Rodada, "ª", "")

#Criar a coluna de data no formato padrao brasileiro
dt20$Data <- as.Date((dt20$Data), format = "%d-%m-%Y")
df <- dt20 %>% filter(Data>=as.Date("2003-01-01"))
remove(dt20, url1)

#Correcao de erros encontrados no data.frame
df$Data[df$Data=="2007-05-17"] <- "2008-05-17"
df$Mandante[df$Data=="2009-07-19"&df$Mandante=="BOTAFOGO-RJ"] <- "FLAMENGO"
df$Visitante[df$Data=="2009-07-19"&df$Visitante=="FLAMENGO"] <- "BOTAFOGO-RJ"

#Ajustes no data.frame
df$Temporada <- year(df$Data)
df$Temporada[df$Temporada=="2021"] <- "2020"
df$Vencedor[df$Vencedor=="-"] <- "EMPATE"
df$EstadoVenc[df$EstadoVenc=="-"] <- "EMPATE"
df$Derrotado <- ifelse(df$Vencedor=="EMPATE", "EMPATE",ifelse(df$Visitante==df$Vencedor,df$Mandante, df$Visitante))
df$PontMandante <- ifelse(df$Vencedor=="EMPATE", 1,ifelse(df$Mandante==df$Vencedor&df$Vencedor!="EMPATE",3, 0))
df$PontVisitante <- ifelse(df$Vencedor=="EMPATE", 1,ifelse(df$Visitante==df$Vencedor&df$Vencedor!="EMPATE",3, 0))
df$ID <- NULL

#Selecao das variaveis de interesse
df <- df %>%
  select(Temporada,Data,Dia,Horario,Rodada,Arena,Mandante,Visitante,Vencedor,Derrotado,
         GolsMan,GolsVisit,PontMandante,PontVisitante,EstadoMan,EstadoVisit,EstadoVenc) 
#Salvamos o banco de dados em formato rds
saveRDS(df,"dados/d2021.rds") 

  



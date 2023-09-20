#In this version I added some columns I use to decide for investments. For the #last two years, I analyze the total payout (including both JCP and dividends) 
#Please note that this code is not a recommendation for investments. 
#install packages before using libraries
library(tidyverse)
library(ralger)

#function to populate our dataframes with financial indicators x stock
stockinfo <- function(stockname){
  
  # extracting tables from fundamentus website
  dataframe1 <- table_scrap(paste0("https://www.fundamentus.com.br/detalhes.php?papel=",stockname),choose = 3)
  dataframe2 <- table_scrap(paste0("https://www.fundamentus.com.br/detalhes.php?papel=",stockname),choose = 1)
  dataframe3 <- table_scrap(paste0("https://www.fundamentus.com.br/proventos.php?papel=",stockname),choose = 1)
  # extracting stock price from table 1
  preco_acao <- colnames(dataframe2[(4)])

  # cleaning table 3
  dataframe1 <- dataframe1[,-c(1,2)]
  dataframe11 <- dataframe1[,1:2]
  dataframe12 <- dataframe1[,3:4]
  dataframe1_all <- rbind(dataframe11, dataframe12)
  dataframe1_all <- dataframe1_all %>% `colnames<-`(c("indicador", "valor"))
 
  # cleaning rows from '?'
  dataframe1_all$indicador <- sub("^.", "", dataframe1_all$indicador)
  
  # merging tables with price and renaming
  dataframe1_all <- dataframe1_all %>%
  mutate(Acao = stockname) %>%
  spread(key = indicador, value = valor)
  preco_hoje = c(preco_acao)
  dataframe1_all <- cbind(dataframe1_all,preco_hoje)
  dataframe1_all$preco_hoje <- as.numeric(gsub(",", ".",dataframe1_all$preco_hoje))
  dataframe3$Data <- as.Date(dataframe3$Data, format = "%d/%m/%Y")
  dataframe3$Valor <- as.numeric(gsub(",", ".",dataframe3$Valor))
  dataframe3[order(dataframe3$Data),]
  y <- ifelse(dataframe3$Data>="2023-1-1" & dataframe3$Tipo == "JRS CAP PROPRIO",dataframe3$Valor,0)
  z <- ifelse(dataframe3$Data>="2023-1-1" & dataframe3$Tipo == "DIVIDENDO",dataframe3$Valor,0)
  Div_23 <- sum(z)*100
  JurosCP_23<- sum(y)*100
  Totalpayout_23 <- sum(Div_23,JurosCP_23,na.rm=TRUE)
  dataframe1_all <- cbind(dataframe1_all,JurosCP_23)
  dataframe1_all <- cbind(dataframe1_all,Div_23)
  dataframe1_all <- cbind(dataframe1_all,Totalpayout_23)
  P_payout_23 <- dataframe1_all$preco_hoje/dataframe1_all$Totalpayout_23
  dataframe1_all <- cbind(dataframe1_all,P_payout_23)
  y_21 <- ifelse(dataframe3$Data>="2022-1-1" & dataframe3$Data<"2023-1-1" & dataframe3$Tipo == "JRS CAP PROPRIO",dataframe3$Valor,0)
  z_21 <- ifelse(dataframe3$Data>="2022-1-1" & dataframe3$Data<"2023-1-1" & dataframe3$Tipo == "DIVIDENDO",dataframe3$Valor,0)
  Div_22 <- sum(z_21)*100
  JurosCP_22<- sum(y_21)*100
  Totalpayout_2yago <- sum(Div_22,JurosCP_22,na.rm=TRUE)
  dataframe1_all <- cbind(dataframe1_all,JurosCP_22)
  dataframe1_all <- cbind(dataframe1_all,Div_22)
  dataframe1_all <- cbind(dataframe1_all,Totalpayout_2yago)
  P_payout_2yago <- dataframe1_all$preco_hoje/dataframe1_all$Totalpayout_2yago
  dataframe1_all <- cbind(dataframe1_all,P_payout_2yago)
  }

# Build here your portfolio and shortlist, for example "BBAS3" 

portfolio <- c("TAEE3","cmig4"
               )
bovespa <- c("ASAI3",             "FLRY3",            "RENT3",             "EGIE3",
             "ENBR3",             "ELET6",            "HYPE3",             "GOAU4",
             "TIMS3",             "WEGE3",            "ABEV3",            "GGBR4",
             "UGPA3",             "TAEE11",           "CPFE3",             "BPAC11",
             "ITUB4",             "RAIL3",            "BBSE3",             "EQTL3",
             "RADL3",             "BEEF3",            "ITSA4",             "PETR3",
             "CIEL3",             "BRAP4",            "VALE3",             "SUZB3",
             "KLBN11",            "MGLU3",             "CYRE3",            "ALSO3",
             "PETZ3",             "AMER3",            "BRML3",             "IGTI11",
             "IRBR3",             "EZTC3",            "LWSA3",             "GOLL4",
             "VIIA3",             "JBSS3",            "MRVE3",             "CRFB3",
             "BRFS3",             "DXCO3",            "SOMA3",             "BPAN4",  
             "CASH3",             "ALPA4",            "NTCO3",             "LREN3",    
             "BBAS3",             "EMBR3",            "ECOR3",             "QUAL3",   
             "TOTS3",             "RDOR3",            "B3SA3",             "ARZZ3",
             "SLCE3",             "SULA11",           "VBBR3",             "CCRO3", 
             "PETR4",             "SMTO3",            "PCAR3",             "YDUQ3",  
             "CVCB3",             "ENEV3",            "VIVT3",             "SANB11",
             "BBDC3",             "MRFG3",            "COGN3",             "BBDC4", 
             "BRKM5",             "CMIN3",            "POSI3",             "CSAN3",   
             "HAPV3",             "RAIZ4",            "CSNA3",             "MULT3",
             "USIM5",             "UGPA3",            "ENAT3",             "SBSP3",
             "CPLE3",             "unip6",            "clsc3",             "fesa4",
             "romi3",             "agro3",            "bees3",             "brsr3",
             "soja3",             "baza3",            "ceeb3",             "sapr4"
              )

# building tables with indicators
portfolioinfo <- map_df(portfolio, stockinfo)
portfolioinfo <- portfolioinfo[,c(-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-16,-17,-20)]
portfolioinfo <- as.data.frame(sapply(portfolioinfo,function(x) gsub("%","", as.character(x))))
portfolioinfo[,c(2,3,4,5,6,7,8,9,10)] <- as.data.frame(sapply(portfolioinfo[,c(2,3,4,5,6,7,8,9,10)], 
                                                              function(x) as.numeric(as.numeric(gsub(",", ".", gsub("\\.", "",
                                                                                                                    as.character(x)))))))
bovespainfo <- map_df(bovespa, stockinfo)

# cleaning columns from some indicators, delete paragraph if you want to see all                                                   

bovespainfo <- bovespainfo[,c(-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-16,-17,-20)]
# converting data to numeric
bovespainfo <- as.data.frame(sapply(bovespainfo,function(x) gsub("%","", as.character(x))))
bovespainfo[,c(2,3,4,5,6,7,8,9,10)] <- as.data.frame(sapply(bovespainfo[,c(2,3,4,5,6,7,8,9,10)], 
                                                          function(x) as.numeric(as.numeric(gsub(",", ".", gsub("\\.", "",
                                                                                                                as.character(x)))))))

# filtering discounted stocks based on parameters
cheap_stocks <- bovespainfo %>% filter(ROE > 10,`Cres. Rec (5a)` > 0,`Div Br/ Patrim` <= 1.5,`Div Br/ Patrim` > 0,`P/L` < 5,`P/VP`>0, `P/L`>0
                                                          )

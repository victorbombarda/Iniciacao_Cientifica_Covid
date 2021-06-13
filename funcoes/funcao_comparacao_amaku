#Bibliotecas necessárias
library(dplyr)
library(RColorBrewer)
library(ggplot2)

#Carregando dados
planilha_amaku <- read.csv("C:/path/planilha_amaku.csv")
#planilha_amaku <- read.csv("C:/Users/solbo/Downloads/planilha_amaku.csv")

#Limpando dados
planilha_amaku$Data <- as.Date(planilha_amaku$Data, format =  "%d/%m/%Y")
planilha_amaku$Training1 <- as.Date(planilha_amaku$Training, format =  "%d/%m/%Y") #Para ser lido como contínuo

primeira_data <- as.Date("2020-11-22") #Selecionando a data pois começa neste determinada projeção
ultima_data <- as.Date(planilha_amaku$Data[length(planilha_amaku$Data)])
planilha_amaku <- filter(planilha_amaku, planilha_amaku$Data >= primeira_data)

#Carregando e limpando dados reais
dados_sp_observados <- read.csv("C:/path/Dados-covid-19-estado.csv", sep = ";")
datas_dados <- c(as.Date("2020-02-26"))
for(i in 1:(length(dados_sp_observados$Data)-1)){datas_dados = append(datas_dados, datas_dados[i] + 1)}
dados_sp_observados$Data <- datas_dados
dados_sp_observados <- filter(dados_sp_observados, dados_sp_observados$Data >= primeira_data & dados_sp_observados$Data <= ultima_data)

#Cores 
cores <- colorRampPalette( brewer.pal( 9 , "Blues" ) )

ggplot() +

  #Não consegui gerar legenda para os dados (ficaram em preto na imagem)
   geom_point(data = dados_sp_observados, aes(x =Data, y = Total.de.casos),  size = 1.25) + 
   geom_line(data = dados_sp_observados, aes(x =Data, y = Total.de.casos),  size = 1.25)   +
  
  geom_point(data=planilha_amaku, aes(x = Data, y = Point, color = Training1)) + 
  geom_line(data=planilha_amaku, aes(x = Data, y = Point, color = Training1)) +
  
  #Não consegui usar o Fill e associar à coluna de datas contínuas, se conseguisse acho que resolvia o 
  #problema de ter de selecionar as cores manualmente
  geom_ribbon(data = planilha_amaku, aes(x=Data ,ymin = Lwr, ymax = Upr, fill  = Training), alpha=0.2)+  guides(fill=FALSE)+
  #scale_color_continuous(high = "#132B43", low = "#56B1F7")+
  #scale_color_gradient(low = "black", high = "blue")+ scale_fill_gradient(low = "black", high = "blue")
  
  
  #Por algum motivo não consegui trabalhar com as datas contínuas então tive de associar manualmente cada cor
  #à uma projeção diferente
  scale_fill_manual(values=c("20/11/2020"= "#F7FBFF","27/11/2020"= "#E3EEF8",
                             "04/12/2020" =  "#CFE1F2","11/12/2020"= "#B5D4E9",
                             "18/12/2020" = "#93C4DE","15/01/2021"= "#6BAED6",
                             "28/01/2021"= "#4A97C9","04/02/2021"= "#2E7EBB",
                             "26/02/2021"= "#1664AB", "03/03/2021"= "#084A92"))+
  
  labs(x="Data",y="Casos Acumulados") +
  theme_bw(base_size = 16)+
  scale_x_date(breaks = seq(primeira_data, ultima_data, by = 15)) +
  ggtitle("Comparação de previsões")  +
  scale_y_continuous(breaks = seq(120000, 2500000, by = 175000)) +
  guides(colour=guide_legend(title="Legenda"))

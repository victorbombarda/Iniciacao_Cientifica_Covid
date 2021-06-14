##Código para plotagem dos gráficos comparativos

#pacotes usados neste código
library(PandemicLP)
library(ggplot2)

#Usando uma função do PandemicLP carregamos os dados sobre a COVID
#E criamos uma coluna provisória para ajudar na filtragem dos dados
df_BR <- load_covid("Brazil")
df_BR$data$y <- c(1:length(df_BR$data$date))

#A partir da coluna criada, filtramos os dados manualmente (Espero solucionar em breve)
#E redefino algumas propriedades da lista para que possamos criar o modelo a partir dela
df <- lapply(df_BR, subset, df_BR$data$y < X) #Sendo X o valor usado para filtrar
df$name <- "Brazil"
df$population <- 210147125

#Aplicamos a lista à função do modelo
modelo <- pandemic_model(df)
posterior_predict(modelo)
posterior_predict_modelo <- posterior_predict(modelo) 

#Agregamos as datas para após o dado coletado para o modelo
df_post <- lapply(df_BR, subset, df_BR$data$y > (X-1) )#Datas subsequentes
datas_post <- df_post$data$date

#Extraimos os dados gerados pela previsão
previsoes_post <- posterior_predict_modelo$predictive_Long[2,]
df_previsoes_post <- data.frame(datas = datas_post, previsoes = previsoes_post[1:length(datas_post)])

#Grafico
ggplot() + 
  
  #Plotados os dados reais observados
  geom_point(df_BR$data, mapping= aes(x = df_BR$data$date,
                                      y =df_BR$data$new_cases ,
                                      group = 1, color = "Dados reais observados")) +
  
  geom_line(df_BR$data, mapping= aes(x = df_BR$data$date,
                                     y =df_BR$data$new_cases ,
                                     group = 1, color = "Dados reais observados")) +
  
  #Plotados a informação dado ao modelo
  geom_point(posterior_predict_modelo$data, mapping = aes(x = date,
                                                          y = new_cases,
                                                          group = 2, color = "Dados fornecidos ao modelo")) +
  
  geom_line(posterior_predict_modelo$data, mapping = aes(x = date,
                                                         y = new_cases,
                                                         group = 2, color = "Dados fornecidos ao modelo")) +
  
  #Plotadas as previsões
  geom_point(df_previsoes_post, mapping =  aes(x = df_previsoes_post$datas,
                                               y = df_previsoes_post$previsoes,
                                               group = 3, color = "Previsões")) +
  
  geom_line(df_previsoes_post, mapping = aes(x = df_previsoes_post$datas,
                                             y = df_previsoes_post$previsoes
                                             ,group = 3, color = "Previsões"))+
  
  #Código que mexe com a estética do gráfico  
  labs(x="Data",y="Incidência de novos casos") +
  theme_bw(base_size = 16)+
  scale_x_date() + scale_y_continuous(expand = c(0,0), limits = c(0,73000)) +
  ggtitle("Comparação com modelo - Até Março") + 
  guides(colour=guide_legend(title="Legenda"))

###Plots comparativos

library(PandemicLP)
library(ggplot2)
df_BR <- load_covid("Brazil")

#Criei uma coluna y no dataframe para conseguir filtrá-lo
#Ainda não pensei em uma forma de filtrar sem que seja manualmente
df_BR$data$y <- c(1:length(df_BR$data$date))

##Até março
#Dado sendo filtrado
df_marco <- lapply(df_BR, subset, df_BR$data$y < 37)

#Após filtrar dessa maneira, precisei redefinir algumas variáveis
#na lista de informação pois para cada item que não é do dataframe 
#dos dados, aparecem n NAs (que não podem aparecer na função pandemic_model() ).
df_marco$name <- "Brazil"
df_marco$population <- 210147125
model_marco <- pandemic_model(df_marco)
predict_marco <- posterior_predict(model_marco) #Guardo em uma variável para não 
#ter que calcular sempre que formos usar.

#Reuni as datas para além dos dados fornecidos para poder associar às previsões.
df_post_marco <- lapply(df_BR, subset, df_BR$data$y > 36)
datas_post_marco <- df_post_marco$data$date

#Peguei as previsões do modelo fornecido
#VER DADOS DA MATRIZ E ENTENDER
previsoes_post_marco <- predict_marco$predictive_Long[2,]

#Criado dataframe com previsões
df_previsoes_post_marco <- data.frame(datas = datas_post_marco, previsoes = previsoes_post_marco[1:length(datas_post_marco)])

#Grafico
ggplot() +
  
  geom_point(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
  geom_line(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
  
  geom_point(predict_marco$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
  geom_line(predict_marco$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
  
  geom_point(df_previsoes_post_marco, mapping =  aes(x = df_previsoes_post_marco$datas, y = df_previsoes_post_marco$previsoes,group = 3, color = "Previsões do Modelo")) +
  geom_line(df_previsoes_post_marco, mapping = aes(x = df_previsoes_post_marco$datas, y = df_previsoes_post_marco$previsoes,group = 3, color = "Previsões do Modelo"))


###Até maio
df_maio <- lapply(df_BR, subset, df_BR$data$y < 98)
df_maio$name <- "Brazil"
df_maio$population <- 210147125
model_maio <- pandemic_model(df_maio)
a1 <- posterior_predict(model_maio)

df_post_maio <- lapply(df_BR, subset, df_BR$data$y > 97)
datas_post_maio <- df_post_maio$data$date

previsoes_post_maio <- a1$predictive_Long[2,]
df_previsoes_post_maio <- data.frame(datas = datas_post_maio, previsoes = previsoes_post_maio[1:length(datas_post_maio)])

ggplot() +
  
  geom_point(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
  geom_line(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
  geom_point(a1$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
  geom_line(a1$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
  geom_point(df_previsoes_post_maio, mapping =  aes(x = df_previsoes_post_maio$datas, y = df_previsoes_post_maio$previsoes,group = 3, color = "Previsões do Modelo")) +
  geom_line(df_previsoes_post_maio, mapping = aes(x = df_previsoes_post_maio$datas, y = df_previsoes_post_maio$previsoes,group = 3, color = "Previsões do Modelo"))




###Até agosto
df_agosto <- lapply(df_BR, subset, df_BR$data$y < 159)
df_agosto$name <- "Brazil"
df_agosto$population <- 210147125
model_agosto <- pandemic_model(df_agosto)
a2 <- posterior_predict(model_agosto)

df_post_agosto <- lapply(df_BR, subset, df_BR$data$y > 158)
datas_post_agosto <- df_post_agosto$data$date

previsoes_post_agosto <- a2$predictive_Long[2,]
df_previsoes_post_agosto <- data.frame(datas = datas_post_agosto, previsoes = previsoes_post_agosto[1:length(datas_post_agosto)])

ggplot() +
  
  geom_point(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
  geom_line(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
  geom_point(a2$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
  geom_line(a2$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
  geom_point(df_previsoes_post_agosto, mapping =  aes(x = df_previsoes_post_agosto$datas, y = df_previsoes_post_agosto$previsoes,group = 3, color = "Previsões do Modelo")) +
  geom_line(df_previsoes_post_agosto, mapping = aes(x = df_previsoes_post_agosto$datas, y = df_previsoes_post_agosto$previsoes,group = 3, color = "Previsões do Modelo"))


###Até outubro
df_outubro <- lapply(df_BR, subset, df_BR$data$y < 220)
df_outubro$name <- "Brazil"
df_outubro$population <- 210147125
model_outubro <- pandemic_model(df_outubro)
a3 <- posterior_predict(model_outubro)

df_post_outubro <- lapply(df_BR, subset, df_BR$data$y > 219)
datas_post_outubro <- df_post_outubro$data$date

previsoes_post_outubro <- a3$predictive_Long[2,]
df_previsoes_post_outubro <- data.frame(datas = datas_post_outubro, previsoes = previsoes_post_outubro[1:length(datas_post_outubro)])

ggplot() +
  
  geom_point(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
  geom_line(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
  geom_point(a3$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
  geom_line(a3$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
  geom_point(df_previsoes_post_outubro, mapping =  aes(x = df_previsoes_post_outubro$datas, y = df_previsoes_post_outubro$previsoes,group = 3, color = "Previsões do Modelo")) +
  geom_line(df_previsoes_post_outubro, mapping = aes(x = df_previsoes_post_outubro$datas, y = df_previsoes_post_outubro$previsoes,group = 3, color = "Previsões do Modelo"))


library(gridExtra)

func <- function(pais, estado = "NA"){
  require(gridExtra)
  require("PandemicLP")
  require("ggplot2")
  df <- load_covid(pais, estado)
  df$data$y <- c(1:length(df_BR$data$date))
  
  #Grafico1
  df_marco <- lapply(df_BR, subset, df_BR$data$y < 37)
  df_marco$name <- df_marco$name[1]
  df_marco$population <- df_marco$population[1]
  model_marco <- pandemic_model(df_marco)
  posterior_predict(model_marco)
  a <- posterior_predict(model_marco) 
  
  df_post_marco <- lapply(df_BR, subset, df_BR$data$y > 36)
  datas_post_marco <- df_post_marco$data$date
  
  previsoes_post_marco <- a$predictive_Long[2,]
  df_previsoes_post_marco <- data.frame(datas = datas_post_marco, previsoes = previsoes_post_marco[1:length(datas_post_marco)])
  
  graph1 <- ggplot() + 
    
    geom_point(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
    geom_line(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
    geom_point(a$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
    geom_line(a$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
    geom_point(df_previsoes_post_marco, mapping =  aes(x = df_previsoes_post_marco$datas, y = df_previsoes_post_marco$previsoes,group = 3, color = "Previsões")) +
    geom_line(df_previsoes_post_marco, mapping = aes(x = df_previsoes_post_marco$datas, y = df_previsoes_post_marco$previsoes,group = 3, color = "Previsões"))+
    labs(x="Data",y=" ") +
    theme_bw(base_size = 16)+
    scale_x_date() + scale_y_continuous(expand = c(0,0), limits = c(0,73000)) +
    ggtitle("Comparação com modelo") + 
    guides(colour=guide_legend(title="Legenda"))
  
  #Grafico2
  df_maio <- lapply(df_BR, subset, df_BR$data$y < 98)
  df_maio$name <- df_maio$name[1]
  df_maio$population <- df_maio$population[1]
  model_maio <- pandemic_model(df_maio)
  a1 <- posterior_predict(model_maio)
  
  df_post_maio <- lapply(df_BR, subset, df_BR$data$y > 97)
  datas_post_maio <- df_post_maio$data$date
  
  previsoes_post_maio <- a1$predictive_Long[2,]
  df_previsoes_post_maio <- data.frame(datas = datas_post_maio, previsoes = previsoes_post_maio[1:length(datas_post_maio)])
  
  graph2 <- ggplot() +
    
    geom_point(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
    geom_line(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
    geom_point(a1$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
    geom_line(a1$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
    geom_point(df_previsoes_post_maio, mapping =  aes(x = df_previsoes_post_maio$datas, y = df_previsoes_post_maio$previsoes,group = 3, color = "Previsões")) +
    geom_line(df_previsoes_post_maio, mapping = aes(x = df_previsoes_post_maio$datas, y = df_previsoes_post_maio$previsoes,group = 3, color = "Previsões"))+ # PREVIsôes do modelo
    labs(x="Data",y=" ") +
    theme_bw(base_size = 16)+
    scale_x_date() + scale_y_continuous(expand = c(0,0), limits = c(0,73000))+
    #ggtitle("Comparação com modelo - Até Maio") + 
    guides(colour=guide_legend(title="Legenda"))
  
  #grafico3
  df_agosto <- lapply(df_BR, subset, df_BR$data$y < 159)
  df_agosto$name <- df_agosto$name[1]
  df_agosto$population <- df_agosto$population[1]
  model_agosto <- pandemic_model(df_agosto)
  a2 <- posterior_predict(model_agosto)
  
  df_post_agosto <- lapply(df_BR, subset, df_BR$data$y > 158)
  datas_post_agosto <- df_post_agosto$data$date
  
  previsoes_post_agosto <- a2$predictive_Long[2,]
  df_previsoes_post_agosto <- data.frame(datas = datas_post_agosto, previsoes = previsoes_post_agosto[1:length(datas_post_agosto)])
  
  graph3 <- ggplot() +
    
    geom_point(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
    geom_line(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
    geom_point(a2$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
    geom_line(a2$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
    geom_point(df_previsoes_post_agosto, mapping =  aes(x = df_previsoes_post_agosto$datas, y = df_previsoes_post_agosto$previsoes,group = 3, color = "Previsões")) +
    geom_line(df_previsoes_post_agosto, mapping = aes(x = df_previsoes_post_agosto$datas, y = df_previsoes_post_agosto$previsoes,group = 3, color = "Previsões"))+
    labs(x="Data",y="Incidência de novos casos") +
    theme_bw(base_size = 16)+
    scale_x_date() + scale_y_continuous(expand = c(0,0), limits = c(0,73000))+
    #ggtitle("Comparação com modelo - Até Agosto") + 
    guides(colour=guide_legend(title="Legenda"))
  
  #grafico4
  df_outubro <- lapply(df_BR, subset, df_BR$data$y < 220)
  df_outubro$name <- df_outubro$name[1]
  df_outubro$population <- df_outubro$population[1]
  model_outubro <- pandemic_model(df_outubro)
  a3 <- posterior_predict(model_outubro)
  
  df_post_outubro <- lapply(df_BR, subset, df_BR$data$y > 219)
  datas_post_outubro <- df_post_outubro$data$date
  
  previsoes_post_outubro <- a3$predictive_Long[2,]
  df_previsoes_post_outubro <- data.frame(datas = datas_post_outubro, previsoes = previsoes_post_outubro[1:length(datas_post_outubro)])
  
  graph4 <- ggplot() +
    
    geom_point(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
    geom_line(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
    geom_point(a3$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
    geom_line(a3$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
    geom_point(df_previsoes_post_outubro, mapping =  aes(x = df_previsoes_post_outubro$datas, y = df_previsoes_post_outubro$previsoes,group = 3, color = "Previsões")) +
    geom_line(df_previsoes_post_outubro, mapping = aes(x = df_previsoes_post_outubro$datas, y = df_previsoes_post_outubro$previsoes,group = 3, color = "Previsões"))+
    labs(x="Data",y=" ") +
    theme_bw(base_size = 16)+
    scale_x_date() + scale_y_continuous(expand = c(0,0), limits = c(0,73000))+
    #ggtitle("Comparação com modelo - Até Outubro") + 
    guides(colour=guide_legend(title="Legenda"))
  
  
  #grafico5
  df_dezembro <- lapply(df_BR, subset, df_BR$data$y < 250)
  df_dezembro$name <- df_dezembro$name[1]
  df_dezembro$population <- df_dezembro$population[1]
  model_dezembro <- pandemic_model(df_dezembro)
  a4 <- posterior_predict(model_dezembro)
  
  df_post_dezembro <- lapply(df_BR, subset, df_BR$data$y > 249)
  datas_post_dezembro <- df_post_dezembro$data$date
  
  previsoes_post_dezembro <- a3$predictive_Long[2,]
  df_previsoes_post_dezembro <- data.frame(datas = datas_post_dezembro, previsoes = previsoes_post_dezembro[1:length(datas_post_dezembro)])
  
  graph5 <- ggplot() +
    
    geom_point(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
    geom_line(df_BR$data, mapping= aes(x = df_BR$data$date, y =df_BR$data$new_cases , group = 1, color = "Dados reais observados")) +
    geom_point(a3$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
    geom_line(a3$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
    geom_point(df_previsoes_post_dezembro, mapping =  aes(x = df_previsoes_post_dezembro$datas, y = df_previsoes_post_dezembro$previsoes,group = 3, color = "Previsões")) +
    geom_line(df_previsoes_post_dezembro, mapping = aes(x = df_previsoes_post_dezembro$datas, y = df_previsoes_post_dezembro$previsoes,group = 3, color = "Previsões"))+
    labs(x="Data",y=" ") +
    theme_bw(base_size = 16)+
    scale_x_date() + scale_y_continuous(expand = c(0,0), limits = c(0,73000))+
   # ggtitle("Comparação com modelo - Até Dezembro") + 
    guides(colour=guide_legend(title="Legenda"))
  
  return(grid.arrange(graph1,graph2,graph3,graph4,graph5))
}

func("Brazil", "RJ")

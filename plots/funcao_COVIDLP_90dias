func_grafico_CovidLP_90 <- function(pais, estado = NULL, data_base){
  #argumentos devem ser todos fornecidos do tipo string - tanto a data piso quando a data teto devem ter o modelo de data %YYYY-%MM-%DD
  require("PandemicLP")
  require("ggplot2")
  require("dplyr")
  
  #"2020-12-22" data máxima da função load_covid()
  iteracoes <- floor(as.numeric(as.Date("2020-12-22") - as.Date(data_base)) /90 )
  plot_list = list()
  previsoes_list = list()
  data_limite <- as.Date("2021-03-20") #Para que o gráfico não penda muito para a direita
  #E permita análises melhores no intervalo no qual temos os dados
  
  for(i in 1:iteracoes){
    ##DADOS PARA GERAR MODELO
    df <- load_covid(pais, estado)
    df$data <-filter(df$data, df$data$date < as.Date(data_base) + i*90) 
    
    novos_casos_1 <- df$data$new_cases
    datas_1 <- df$data$date
    
    model <- pandemic_model(df)
    posterior <- posterior_predict(model)
    
    ##DADOS PARA COMPARAÇÃO
    df2 <- load_covid(pais, estado)
    df2 <- filter(df2$data, df2$data$date >= as.Date(data_base) + i*90)
    novos_casos <- df2$new_cases
    datas <- df2$date
    
    
    ##ADQUIRINDO COMPARAÇÕES E GERANDO DATA FRAMES
    previsoes <- c()
    Mu <- c()
    for(j in 1:(ncol(posterior$predictive_Long))){previsoes <- append(previsoes, median(posterior$predictive_Long[,j]         )) }
    for(k in 1:(ncol(posterior$predictive_Long) - length(datas))){datas <- append(datas, datas[length(datas)] + 1)  }
    for(v in 1:(ncol(posterior$pastMu))){Mu <- append(Mu, median(posterior$pastMu[,v]))}
    
    print("ITERACAO - ")
    print(i)
    
    df_dados_1 <- data.frame(datas = datas_1 , "novos casos" = novos_casos_1, pred_in_sample = round(Mu,2), pred_out_sample = NA, "type" = "Training")
    for(j in 1:(length(datas) - length(novos_casos))){novos_casos <- append(novos_casos, NaN)}
    df_previsoes <- data.frame(datas = datas, "novos casos" = novos_casos , pred_in_sample = NA, pred_out_sample = previsoes, "type" = "Test")
    
    df_previsoes <- rbind(df_dados_1, df_previsoes)
    
    df_previsoes <- filter(df_previsoes, df_previsoes$datas < data_limite)
    df_pastMu <- data.frame("datas" = datas_1, "previsoes" = Mu)
    
    previsoes_list[[i]] = df_previsoes
    
    ##GERANDO GRAFICOS
    p = ggplot() + 
      geom_point(df2, mapping= aes(x = date, y =new_cases , group = 1, color = "Dados reais observados"), shape = 23) +
      geom_line(df2, mapping= aes(x = date, y =new_cases , group = 1, color = "Dados reais observados")) +
      
      geom_point(df_pastMu, mapping= aes(x = datas, y =previsoes , group = 4, color = "Modelo In-sample"), shape = 20) +
      geom_line(df_pastMu, mapping= aes(x = datas, y =previsoes , group = 4, color = "Modelo In-sample")) +
      
      geom_point(df$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo"), shape = 21) +
      geom_line(df$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
      
      geom_point(df_previsoes, mapping =  aes(x = datas, y = pred_out_sample,group = 3, color = "Previsões")) +
      geom_line(df_previsoes, mapping = aes(x = datas, y = pred_out_sample,group = 3, color = "Previsões"))+
      
      labs(x="Data",y="Novos casos") +
      theme_bw(base_size = 16)+
      scale_x_date() + scale_y_continuous(expand = c(0,0))+ #, limits = c(0,73000)) +
      ggtitle("Comparação com modelo") + 
      guides(colour=guide_legend(title="Legenda")) 
    
    plot_list[[i]] = p
    print("TERIMNOU O GRÁFICO NÚMERO -")
    print(i)
    
  }
  print("SALVANDO GRÁFICOS") #GRÁFICOS SÃO SALVOS NO DEVIDO WORKING DIRECTORY 
  for (i in 1:iteracoes) {
    file_name = paste("COVIDLP_90dias_",pais, "_", estado,"_", i, ".png", sep="")
    file_name_csv =paste("COVIDLP_90dias_",pais, "_", estado,"_", i, ".csv", sep="") 
    png(file_name,width=1000,height=900)
    print(plot_list[[i]])
    dev.off()
    write.csv(previsoes_list[[i]], file_name_csv)
  }
}  

func_grafico_CovidLP <- function(pais, estado = NULL, data_teto, data_limite = Sys.Date()){
  #argumentos devem ser todos fornecidos do tipo string
  #tanto a data piso quando a data teto devem ter o
  #modelo de data %YYYY-%MM-%DD
  require("PandemicLP")
  require("ggplot2")
  require("dplyr")
  df <- load_covid(pais, estado)
  df$data <-filter(df$data, df$data$date < as.Date(data_teto))  
  model <- pandemic_model(df)
  posterior <- posterior_predict(model)

  
  df2 <- load_covid(pais, estado)
  df2 <- filter(df2$data, df2$data$date >= as.Date(data_teto) & df2$data$date < as.Date(data_limite))
  datas <- df2$date

  previsoes <- c()
  for(i in 1:(ncol(posterior$predictive_Long))){previsoes <- append(previsoes, median(posterior$predictive_Long[,i]         )) }
  for(i in 1:(ncol(posterior$predictive_Long) - length(datas))){datas <- append(datas, datas[length(datas)] + 1)  }
  
  df_previsoes <- data.frame(datas = datas, previsoes = previsoes)
  return(ggplot() + 
           
           geom_point(df2, mapping= aes(x = df2$date, y =df2$new_cases , group = 1, color = "Dados reais observados"), shape = 23) +
           geom_line(df2, mapping= aes(x = df2$date, y =df2$new_cases , group = 1, color = "Dados reais observados")) +
           geom_point(df$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo"), shape = 21) +
           geom_line(df$data, mapping = aes(x = date, y = new_cases,group = 2, color = "Dados fornecidos ao modelo")) +
           geom_point(df_previsoes, mapping =  aes(x = df_previsoes$datas, y = df_previsoes$previsoes,group = 3, color = "Previsões")) +
           geom_line(df_previsoes, mapping = aes(x = df_previsoes$datas, y = df_previsoes$previsoes,group = 3, color = "Previsões"))+
           labs(x="Data",y=" ") +
           theme_bw(base_size = 16)+
           scale_x_date() + scale_y_continuous(expand = c(0,0))+ #, limits = c(0,73000)) +
           ggtitle("Comparação com modelo") + 
           guides(colour=guide_legend(title="Legenda"))) +  scale_colour_manual(values=cbPalette)
}

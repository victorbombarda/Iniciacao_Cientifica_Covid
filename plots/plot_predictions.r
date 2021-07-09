###$$$$$ Pipeline
## 0. Pull data up to 'base_date'
## 1. Fit the model to data from begin_date to 'base_date'
## 2. Produce predictions for the next 'window_size' units of time
## 3. Plot all of that
pull_data <- function(
  country,
  state,
  base_date,
  max_date = "2020-12-22"
){
  raw <- PandemicLP::load_covid(country_name = country,
                                state_name = state,
                                last_date = base_date)
  raw$data$case_incidence <- raw$data$new_cases/raw$population* 1E5
  raw$data$death_incidence <- raw$data$new_deaths/raw$population* 1E5
  return(raw)
}
#
summy_fun <- function(x, alpha = 0.95){
  return(
    data.frame(
      pred_mean = mean(x, na.rm = TRUE),
      pred_median = median(x, na.rm = TRUE),
      pred_lwr = quantile(x, prob = (1-alpha)/2, na.rm = TRUE),
      pred_upr = quantile(x, prob = (1 + alpha)/2, na.rm = TRUE)
    )
  )
}
#
get_pp <- function(fit, window_size, pop){
  
  past_raw <- apply(fit$pastMu, 2,
                    function(l) rpois(n = length(l), lambda = l) )
  pred_past <- do.call(rbind, apply(past_raw, 2, summy_fun))
  pred_fut <- do.call(rbind, apply(fit$predictive_Long, 2, summy_fun))
  
  last.date <- max(fit$data$date)
  K <- nrow(fit$data)
  
  predictions <- data.frame(
    date = c(fit$data$date, last.date + 1:window_size),
    rbind(
      pred_past,
      pred_fut[1:window_size, ]
    ),
    type = c(rep("training", K),
             rep("testing", window_size))
  )
  rownames(predictions) <- NULL
  
  return(predictions)  
}
##
make_predictions <- function(training_data,
                             window_size,
                             max_date = "2020-12-22"){
  
  ss <- strsplit(training_data$name, "_")
  country <- ss[[1]][1]
  state <- ss[[1]][2]
  last.date <- max(training_data$data$date)
  training_data$data$type <- "training"
  ############
  final.date <- min(
    last.date + window_size,
    max_date
  )
  
  pred.data <- pull_data(country = country, state = state,
                         base_date = final.date, max_date = final.date)
  pred.data$data$type <- "testing"
  pred.data$data <- subset(pred.data$data, date > last.date)
  
  all.data <- rbind(training_data$data, pred.data$data)
  
  ### Fit the model
  options(mc.cores = 4)

  # TODO: check mcmc does not show convergence problems
  fail <- 1
  iter <- 0
  while(fail == 1 && iter <= Niter){
    model <- PandemicLP::pandemic_model(training_data,
                                        chains = 4)
    fail <- check_fail(model)
    iter <- iter + 1
  }
  
  posterior <- rstantools::posterior_predict(model)
  model.preds <- get_pp(fit = posterior, window_size = window_size)
  ## Consolidate output
  out <- merge(all.data, model.preds,
               by = c("date", "type"))
  
  return(out)
}
##
get_all_predictions <- function(country,
                             state,
                             window_size = 15,
                             base_date,
                             max_date = "2020-12-22"){
  
  require("PandemicLP")
  require("ggplot2")
  require("dplyr")
  #####
  dbt <- as.Date(base_date)
  dtl <- as.Date(max_date)
  n.windows <- floor(as.numeric(dtl - dbt) / window_size )
  
  all.preds <- vector(n.windows, mode = "list")
  
  for(w in 1:n.windows){
    cat(paste(as.Date(dbt)), "\n")
    
    training <- pull_data(country = country,
                          state = state,
                          base_date = dbt)
    preds <- make_predictions(training_data = training,
                              window_size = window_size)
    preds$final_date <- as.factor(dbt)
    preds$window <- paste0("window_", w)
    
    all.preds[[w]] <- preds
    
    dbt <- dbt + window_size
  }
  
  return(
    do.call(rbind, all.preds)
  )
}

comp.time <- system.time(
  one.wave.preds <- get_all_predictions(country = "Brazil",
                                        state = "SP",
                                        window_size = 15,
                                        base_date = "2020-06-15",
                                        max_date = "2020-12-22")
)

comp.time

head(one.wave.preds)
tail(one.wave.preds)

one.wave.preds$type <- factor(one.wave.preds$type,
                                 levels = c("training", "testing"))

library(ggplot2)


ma <- function(x, n = 7){
  # https://stackoverflow.com/questions/743812/calculating-moving-average
  stats::filter(x, rep(1 / n, n), sides = 2)
}

one.wave.preds$smoothed_new_cases <- ma(x = one.wave.preds$new_cases, n = 14)

p <- ggplot(data = one.wave.preds,
            aes(x = date, y = pred_mean,
                colour = window, fill = window)) +
  geom_line(aes(linetype = type)) +
  geom_ribbon(aes(ymin = pred_lwr, ymax = pred_upr, linetype = type),
              alpha = 0.4) +
  geom_point(data = one.wave.preds,
              aes(x = date, y = new_cases),
              colour = "black",
              inherit.aes = FALSE) +
  geom_vline(xintercept = as.Date(unique(one.wave.preds$final_date)),
             linetype = "solid") + 
  scale_x_date("Time") +
  scale_y_continuous("New cases") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")
p
#####
## cumulative version

one.wave.preds$cum_pred_mean <- cumsum(one.wave.preds$pred_mean)
one.wave.preds$cum_pred_lwr <- cumsum(one.wave.preds$pred_lwr)
one.wave.preds$cum_pred_upr <- cumsum(one.wave.preds$pred_upr)

p.cumulative <- ggplot(data = one.wave.preds,
            aes(x = date, y = cum_pred_mean,
                colour = window, fill = window)) +
  geom_line(aes(linetype = type)) +
  geom_ribbon(aes(ymin = cum_pred_lwr,
                  ymax = cum_pred_upr,
                  linetype = type),
              alpha = 0.4) +
  geom_point(data = one.wave.preds,
             aes(x = date, y = cases),
             colour = "black",
             inherit.aes = FALSE) +
  # geom_smooth(data = one.wave.preds,
  #            aes(x = date, y = new_cases),
  #            colour = "black",
  #            inherit.aes = FALSE) +
  geom_vline(xintercept = as.Date(unique(one.wave.preds$final_date)),
             linetype = "solid") +
  scale_x_date("Time") +
  scale_y_continuous("New cases") +
  theme_bw(base_size = 16) +
  theme(legend.position = "none")
p.cumulative
  
# max_date <- "2020-12-22"
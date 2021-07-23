source("covid_aux.r")

comp.time <- system.time(
  one.wave.preds <- get_all_predictions(country = "Brazil",
                                        state = "SP",
                                        window_size = 15,
                                        base_date = "2020-06-15",
                                        max_date = "2020-06-30")
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

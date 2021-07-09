Y0=load_covid(country_name="Brazil",state_name="SP",last_date='2020-04-25')
plot(Y0,cases="new")
output0=pandemic_model(Y0, chains = 4)
output0
output0$fit
rstan::check_hmc_diagnostics(output0$fit)
mm <- rstan::monitor(output0$fit)
fail.neff <- ifelse(mm$n_eff < 100, 1, 0)
fail.rhat <- ifelse(mm$Rhat > 1.05, 1, 0)

sum(fail.neff) >= 1
sum(fail.rhat) >= 1

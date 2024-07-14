

distr <- rgamma(n = 10000, shape = 1.183519, rate = 7.13711)

# let's pretend that there are no observations below 150m

distr <- distr[which(distr > (150/2183.475))]

t <- MASS::fitdistr(distr, "gamma", start=list(shape=1, rate=1))
t$estimate

distr_new <- rgamma(n = 10000, shape = 2.892393, rate = 12.979560)

distr_new <- distr_new[which(distr_new > (150/2183.475))]

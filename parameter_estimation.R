#Reading in the data
Shrike = read.csv('Shrike.csv', header = TRUE)
plot(Shrike$Precip.Index, Shrike$Fledge.Counts)

#Storage location for parameter values that can be updated later
LL.min = 10e6
ap.min = 10e6
bp.min = 10e6

#Estimating parameter values by finding the lowest log-likelihood score
#Poisson distributed data
for (a in seq(0, 1.5, by = .01)){
  for (b in seq(.5, 1.5, by = .01)){
    LL.temp = 0
    lam = array(data = NA, dim = length(Shrike$Precip.Index))
    for (i in 1:length(Shrike$Precip.Index)){
      lam[i] = exp(a + b*Shrike$Precip.Index[i])
    }# End of i loop
    LL.temp = -sum(dpois(Shrike$Fledge.Counts, lam, log = TRUE))
    if (LL.temp < LL.min){
      LL.min = LL.temp
      ap.min = a
      bp.min = b
    }
  }# End of b loop
}# End of a loop
abline(a = ap.min, b = bp.min)

boot.n = 10000
ap.boot = array(data = NA, dim = length(boot.n))
bp.boot = array(data = NA, dim = length(boot.n))

#Repeating the same parameter estimation, this time using bootstrapping
for (i in 1:boot.n){
  y = array(data = NA, dim = length(Shrike$Precip.Index))
  PI.boot = array(data = NA, dim = length(Shrike$Precip.Index))
  picks = sample.int(40, size = 40, replace = TRUE)
  for (j in 1:length(y)){
    y[j] = Shrike$Fledge.Counts[picks[j]]
    PI.boot[j] = Shrike$Precip.Index[picks[j]]
  }
  lm.temp = glm(y ~ PI.boot, family = poisson(link = 'log'))
  ap.boot[i] = lm.temp$coefficients[1]
  bp.boot[i] = lm.temp$coefficients[2]
}

# Plotting the scatterplot
plot(ap.boot, bp.boot)

# Calculating confidence intervals
ap.conf = quantile(ap.boot, probs = c(.025, .5, .975))
bp.conf = quantile(bp.boot, probs = c(.025, .5, .975))

# Plotting the histograms and confidence intervals
par(mfrow = c(1, 2))
hist(ap.boot, main = 'Bootstrap estimate - ap (10,000 iterations)')
abline(v = ap.conf, lty = 2, col = c('darkblue', 'darkred', 'darkblue'))
hist(bp.boot, main = 'Bootstrap estimate - bp (10,000 iterations)')
abline(v = bp.conf, lty = 2, col = c('darkblue', 'darkred', 'darkblue'))
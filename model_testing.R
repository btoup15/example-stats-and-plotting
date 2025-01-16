setwd('C:/Users/ben/Desktop/General/Documents/Analytical/')
shrike = read.csv('ShrikeLocation.csv', header = TRUE)

# setting up our model
## Model one, models only the intercept
g1 = glm(Fledge.Counts ~ 1, family = poisson(link = 'log'), data = shrike)
## Model two, models the intercept and precipitation index
g2 = glm(Fledge.Counts ~ Precip.Index, family = poisson(link = 'log'), data = shrike)
## Model three, models intercept and location
g3 = glm(Fledge.Counts ~ Location, family = poisson(link = 'log'), data = shrike)
## Model four, models intercept, precip. index, and location
g4 = glm(Fledge.Counts ~ Precip.Index + Location, family = poisson(link = 'log'), data = shrike)

# Log Likelihood of each model
LL = round(c(logLik(g1), logLik(g2), logLik(g3), logLik(g4)), 3)

# Number of parameters for each
K = c(length(coef(g1)), length(coef(g2)), length(coef(g3)), length(coef(g4)))

# Calculating AIC (corrected for small sample size)
AIC.c = round(-2*LL + 2*K + (2*K*(K+1)) / (nrow(shrike) - K - 1), 3)

# Calculating deltaAIC
delta = round(AIC.c - min(AIC.c), 3)
weights = round(exp(-0.5 * delta) / sum(exp(-0.5 * delta)), 4)

# Model Ranks
model.rank = rank(AIC.c)

# Model Table
shrike.results = data.frame(Model = 1:4, K = K, LL, AIC.c, delta, weights, model.rank)
print(shrike.results)
print(paste('Model Selected: ', shrike.results$Model[shrike.results$model.rank == min(shrike.results$model.rank)], ' (AIC = ', shrike.results$AIC.c[shrike.results$model.rank == min(shrike.results$model.rank)], ')'))

plot(shrike$Precip.Index[shrike$Location == 'LocationTwo'], 
     shrike$Fledge.Counts[shrike$Location == 'LocationTwo'], pch = 16, xlab = 'Precip. Index',
     ylab = 'Fledge Count', col = 'red')
points(shrike$Precip.Index[shrike$Location == 'LocationOne'], 
       shrike$Fledge.Counts[shrike$Location == 'LocationOne'], pch = 16, col = 'blue')

# Creating a sequence of x values with which to calculate exponentiated y's
x = seq(0, 1, length.out = 100)

# Calculating y's using the intercepts and slopes from model 4
y1 = exp(coef(g4)[1] + coef(g4)[2]*x)
y2 = exp(coef(g4)[1] + (coef(g4)[2] + coef(g4)[3])*x)

# Adding exponentiated best fit lines
lines(x, y1, lwd = 2, col = 'blue')
lines(x, y2, lwd = 2, col = 'red')
# Adding legend
legend('topleft', c('Location 1', 'Location 2'), col = c('blue', 'red'), pch = c(16, 16),
       bty = 'n')
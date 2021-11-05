# First let's make a Bayesian Linear Regression example


# Load data and inspect
h <- read.csv('datosClase.csv', sep = ',')
head(h)

# Plot
plot(h$x, h$y1)
points(h$x, h$y2, col = 4)

# We can see that the values are not the same. y1 is spreads as x increases.

# Calculate regression lines and inspect coefficients
l1 <- lm(formula = y1 ~ x, data = h)
l2 <- lm(formula = y2 ~ x, data = h)
l1$coefficients
l2$coefficients
summary(l1)$r.squared
summary(l2)$r.squared

# Pretty much the same values, although graphically we saw that they were not
# the same

# Now let's make a Bayesian linear regression to explore the distribution of 
# results obtained.


# First load the required library
library('MCMCpack')

# Using the MCMCregress method we perform a Bayesian Linear Regression over 5000
# different samples for both y1 and y2,
theta1 <- MCMCregress(y1 ~ x,
                      data = h,
                      mcmc = 5000,
                      seed = 42)
theta2 <- MCMCregress(y2 ~ x,
                      data = h,
                      mcmc = 5000,
                      seed = 42)

# Checking density and residuals of the intercept, slope and sigma
plot(theta1)
plot(theta2)
#Comparing the previous plots we can notice that there's a shif in the means
# of the density plots

# Inspecting the values
summary(theta1)
summary(theta2)

# Now we are gonna test which is the impact of these small differences in the
# values by performing an extrapolation

# For this we need to transform the results of the regression to data frame format
theta1 <- as.data.frame(theta1)
theta2 <- as.data.frame(theta2)

# Build a set of point where we want to extrapolate
X <- rbind(c(1, 15), c(1, 20), c(1, 30))

# Build the matrix and vector to save the values of each intercept, slope and sigma.
beta <- matrix(nrow = 5000, ncol = 2)
sigma <- numeric(5000)
colnames(beta) <- c('x1', 'x2')
theta11 <- list(beta = beta, sigma = sigma)
theta22 <- list(beta = beta, sigma = sigma)


theta11$beta[, 1] <- theta1$`(Intercept)`
theta11$beta[, 2] <- theta1$x
theta11$sigma <- theta1$sigma2

theta22$beta[, 1] <- theta2$`(Intercept)`
theta22$beta[, 2] <- theta2$x
theta22$sigma <- theta2$sigma2

# Extrapolate
media.y1 <- blinregexpected(X, theta11)
media.y2 <- blinregexpected(X, theta22)

# Plot for x=15
p1 <- hist(media.y1[, 1], main = 'X = 15', xlab = 'x')
p2 <- hist(media.y2[, 1], main = 'X = 15', xlab = 'x')
plot(
  p1,
  col = rgb(0, 0, 1, 1 / 4),
  main = 'X = 15',
  xlab = 'x',
  xlim = c(46.85, 47.25)
)
plot(p2,
     col = rgb(1, 0, 0, 1 / 4), add = T)

# As we can notice, the histograms differ quite a lot. If we take, for example,
# the value x = 47.1 for the histogram on the left is an outlier whereas for the
# other, it lays on the center of the histogram

#-----------------------------------------------------------------------
# Now let's build a simple Bayesian Network. A Bayesian network is an oriented 
# graph where each node has probability information. 

# The basic idea of this example is the following:
  
  # The upper nodes (visit to Asia, smoker) are observable variables on the 
  # behavior of some patients.

  # The lower nodes (X-rays, dyspnea) are also observable variables, symptoms 
  # of these patients.

  # The central nodes, the most important, are not observable: they are various 
  # diseases that the individuals in question may be suffering from. 
  # A scheme of the network is available at: 
  # https://www.datanalytics.com/wp-uploads/2013/11/red_asia.jpg


# Load the required library
library(bnlearn)

# Build the model and plot
bn.asia <-model2network('[Asia][Smoker][Tuberculosis|Asia][Cancer|Smoker][Tub_Cancer|Tuberculosis:Cancer][Radiography|Tub_Cancer][Bronchitis|Smoker][Dyspnoea|Bronchitis:Tub_Cancer]' )
plot(bn.asia)

yn <- c("yes", "no") # Possible outcomes
a <- array(dimnames = list(Asia = yn), dim = 2, c(0.01, 0.99))
b <-
  array(
    dimnames = list(Bronchitis = yn, Smoker = yn),
    dim = c(2, 2),
    c(0.60, 0.40, 0.30, 0.70)
  )
t.a <-
  array(
    dimnames = list(Tuberculosis = yn, Asia = yn),
    dim = c(2, 2),
    c(0.05, 0.95, 0.01, 0.99)
  )
s <- array(dimnames = list(Smoker = yn), dim = 2, c(0.5, 0.5))
c.s <-
  array(
    dimnames = list(Cancer = yn, Smoker = yn),
    dim = c(2, 2),
    c(0.10, 0.90, 0.01, 0.99)
  )
x.e <-
  array(
    dimnames = list(Radiography = yn, Tub_Cancer = yn),
    dim = c(2, 2),
    c(0.98, 0.02, 0.05, 0.95)
  )
d.be <-
  array(
    dimnames = list(
      Dyspnoea = yn,
      Tub_Cancer = yn,
      Bronchitis = yn
    ),
    dim = c(2, 2, 2),
    c(0.90, 0.10, 0.70, 0.30, 0.80, 0.20, 0.10, 0.90)
  )
e.lt <-
  array(
    dimnames = list(
      Tub_Cancer = yn,
      Cancer = yn,
      Tuberculosis = yn
    ),
    dim = c(2, 2, 2),
    c(1, 0, 1, 0, 1, 0, 0, 1)
    
  )
cpts <-
  list(
    Bronchitis = b,
    Asia = a,
    Tuberculosis = t.a,
    Smoker = s,
    Cancer = c.s,
    Radiography = x.e,
    Dyspnoea = d.be,
    Tub_Cancer = e.lt
  )
bn.asia.fit = custom.fit(bn.asia, cpts)

# For example we can check the probability of having a positive Radiography after
# visiting Asia
bn.fit.barchart(bn.asia.fit$Radiography) 

# For exact inference we will use the gRain library
# We can convert the bnlearn objects to gRain ones
library('gRain')

gr.bn.asia <- as.grain(bn.asia.fit)

# Now for example we can check which is the probability of someone that has
# visited Asia has difficulties to breath and is a non smoker
tmp <- setFinding(gr.bn.asia, nodes = c("Dyspnoea", "Smoker"), states = c("no", "yes"))
querygrain( tmp, nodes = "Asia")

# Or checking which is the probability of having Dyspnoea being a smoker
tmp2 <- setFinding(gr.bn.asia, nodes = "Smoker", states = "yes")
querygrain(tmp2, node= 'Dyspnoea')

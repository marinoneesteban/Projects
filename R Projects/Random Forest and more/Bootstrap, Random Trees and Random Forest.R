# Import needed libraries
library(rpart)
library(ipred)
library(randomForest)
library(caret)

# Set seed for reproducibility

set.seed(42)

# Import data and inspect

h <- read.csv("NoCategoricos5.csv", sep = ";")


# It is observed that there 1000 values with 5 non-categorical variables and a 
# target of 5 possible outcomes (0,1,2,3,4). 
# Delete index column and convert target column to factor to perform classifica_
# tion algorithms.

h <- subset(h, select = -X)
h$R <- as.factor(h$R)


# Create a vector with random numbers of the same length of the dataset to
# split it afterwards.
x <- runif(nrow(h))

# Split data to create 3 random trees using the bootstrap technique manually.
h1 <- h[which(x < 1 / 3.),]

h2 <- h[which(x >= 1 / 3. & x < 2 / 3.),]

h3 <- h[which(x >= 2 / 3.) ,]

# First tree
hTrainAB <- rbind(h1, h2)
hTestAB <- h3

# Second tree
hTrainAC <- rbind(h1, h3)
hTestAC <- h2

# Third tree
hTrainBC <- rbind(h2, h3)
hTestBC <- h1

# Create trees and  predict
tAB <- rpart(data = hTrainAB,
             formula = R ~ .,
             method = 'class')
tBC <- rpart(data = hTrainBC,
             formula = R ~ .,
             method = 'class')
tAC <- rpart(data = hTrainAC,
             formula = R ~ .,
             method = 'class')
pAB <- predict(tAB, newdata = hTestAB, type = 'class')
pBC <- predict(tBC, newdata = hTestBC, type = 'class')
pAC <- predict(tAC, newdata = hTestAC, type = 'class')

# Calculate each error with its respective dataset

errAB <- length(which(pAB == hTestAB$R))/nrow(hTestAB)
errBC <- length(which(pBC == hTestBC$R))/nrow(hTestBC)
errAC <- length(which(pAC == hTestAC$R))/nrow(hTestAC)

# From 63.58% to 73.23%

# Calculate the overall error using the whole dataset (h)

pAB2 <- predict(tAB, newdata = h, type = 'class')
pBC2 <- predict(tBC, newdata = h, type = 'class')
pAC2 <- predict(tAC, newdata = h, type = 'class')

errAB2 <- length(which(pAB2 == h$R))/nrow(h)
errBC2 <- length(which(pBC2 == h$R))/nrow(h)
errAC2 <- length(which(pAC2 == h$R))/nrow(h)


err <- (errAB2+errAC2+errBC2)/3
  
# 74.53%. Better performance that each tree by itself.


# Using bagging method included in the ipred library
# Splitting in train and test
x <- runif(nrow(h))
train <- h[which(x < .7),]
test <- h[which(x >= .7),]

mb <- bagging(R ~ ., data = train, coob = TRUE)
pb <- predict(mb, newdata = test)
length(which(pb == test$R)) / nrow(test)

# 70.12%

# Trying with Random Forest
# Train the model with training data
rf <- randomForest(formula = R ~ ., data = train)

# Make predictions using test data
prf <- predict(rf, newdata = test)

# Calculating and inspecting the confusion matrix
m <- confusionMatrix(prf, test$R)
m

m$overall['Accuracy']

# 71.75%, of accuracy
# Create a valorization ($) matrix. In this particular case and as an examp_
# le the bigger the categorical value the bigger the valorization.

v <- matrix(nrow = 5, ncol = 5, 0)

for (i in 1:5) {
  v[i, i] <- 100 * i
  
}

v

# Now we fill the rest of the matrix with the penalization. In this case, the 
# farthest the error the bigger.

# The values of the penalizations depends on the business that it is being 
# analyzed.

for (i in 1:5) {
  for (j in 1:5) {
    if (i != j) {
      v[i, j] <- -200 * abs(i - j)
      
    }
    
  }
  
}
v

# Multiplying the matrix m and v and then calculating the sum, gives a value of 
# this solution.

# In this case an economical reference for the Random Forest model is obtained.

m <- as.table(m)
value_RF <- sum(m * v)

value_RF


# Using this model a value of $30,600 is achieved.

# As we want to improve this model we must know which is the ideal or maximum 
# value that can be achieved.

w <- as.vector(table(test$R))


# Now we obtain the optimum value

opt_val <- sum(w * c(100, 200, 300, 400, 500))

opt_val

# This is maximum value that can be obtained with a perfect model.

# The first model gave a value of $30,600, so there is still room for optimiza_
# tion of the algorithm.

# Random Forest has some parameters that can be tuned in order to obtain better
# predictions such as ntree, mtry, replace, sampsize and nodesize. We will work 
# on  some of these to improve our model.

# Firstly we try different values of ntree. Default is ntree = 500

rf_2 <-
  randomForest(formula = R ~ .,
               data = train,
               ntree = 1000)


prf_2 <- predict(rf_2, newdata = test)


m2 <- confusionMatrix(prf_2, test$R)

m2$overall['Accuracy']

# 72.40%., slightly better. This value does not say much about the business, 
# we create the valorization matrix.

m2 <- as.table(m2)

value_RF2 <- sum(m2 * v)

value_RF2

# With ntree = 1000 we obtain a value of $31,800. A bit better.
# We try with ntree = 2000

rf_3 <-
  randomForest(formula = R ~ .,
               data = train,
               ntree = 2000)


prf_3 <- predict(rf_3, newdata = test)


m3 <- confusionMatrix(prf_3, test$R)

m3$overall['Accuracy']

# 72.07%., better than the previous model.

m3 <- as.table(m3)

value_RF3 <- sum(m3 * v)

value_RF3


# With ntree = 2000, the value has decreased to $31,200.
# We last try at ntree = 1500

rf_4 <-
  randomForest(formula = R ~ .,
               data = train,
               ntree = 1500)


prf_4 <- predict(rf_4, newdata = test)


m4 <- confusionMatrix(prf_4, test$R)

m4$overall['Accuracy']

# 73.37%., best so far.

m4 <- as.table(m4)

value_RF4 <- sum(m4 * v)

value_RF4
# $33,100, it has increased from the previous value.

# Now we will modify the mtry parameter. This parameter denotes the number of 
# variables randomly sampled as candidates at each split.
# Previously we were ysing mtry = 5 as we have 5 prediction variables. We will 
# now modify this value.
# In this case, instead of testing every value each time we will use a grid 
# search.
# Each axis of the grid is an algorithm parameter, and points in the grid are
# specific combinations of parameters. Because we are only tuning one 
# parameter,
# the grid search is a linear search through a vector of candidate values

control <-
  trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3,
    search = "grid"
  )

tunegrid <- expand.grid(.mtry = c(1:5))

rf_gridsearch <-
  train(
    R ~ .,
    data = h,
    method = "rf",
    metric = 'Accuracy',
    tuneGrid = tunegrid,
    trControl = control
  )
print(rf_gridsearch)
plot(rf_gridsearch)


# As seen in the last plot, the best value for mtry is 3.

rf_5 <-
  randomForest(
    formula = R ~ .,
    data = train,
    ntree = 1500,
    mtry = 3
  )


prf_5 <- predict(rf_5, newdata = test)


m5 <- confusionMatrix(prf_5, test$R)

m5$overall['Accuracy']

# 72.07%. 

m5 <- as.table(m5)

value_RF5 <- sum(m5 * v)

value_RF5

# $31,700. One may think that obtaining a lesser accuracy and value using the
# best values of mtry and ntree it is odd, but it has to be considered that,
# as its name says, this is a random process. Maybe in the next run it performs
# better. For this we are gonna perform a 100 iterations and check the results.


rf_5_acc <- c()
rf_5_val <- c()
acc <- 0
values_RF5 <- 0

for (i in 1:100) {
  rf_5 <-
    randomForest(
      formula = R ~ .,
      data = train,
      ntree = 1500,
      mtry = 3
    )
  
  
  prf_5 <- predict(rf_5, newdata = test)
  
  
  m5 <- confusionMatrix(prf_5, test$R)
  
  acc[i] <- m5$overall['Accuracy']
  
  m5 <- as.table(m5)
  
  values_RF5[i] <- sum(m5 * v)
  rf_5_acc[i] <- acc[i]
  rf_5_val[i] <- values_RF5[i]
}


hist(values_RF5)
hist(acc)

mean(values_RF5)
sd(values_RF5)
mean(acc)
sd(acc)

# As we can see over 100 iterations this model gives a value of $31,904 ± 537.09
# and an accuracy of 72.10%  ± 0.3 %.

# There are plenty of parameters to keep tuning and obtain better results, this was
# a brief demonstration of the most important ones.

# With the VarImplot method how important is each variable for the model. 
varImpPlot(rf_5,
           sort = T,
           main = "Variable Importance")
importance(rf)


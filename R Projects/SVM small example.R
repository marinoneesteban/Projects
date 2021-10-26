# Set seed for reproducibility
set.seed(42)

# Load needed libraries
library(e1071)
library(rpart)
library(caret)

# Load glass data and inspect
data(Glass, package = "mlbench")
head(Glass)

# Checking how many types of glass there are
str(Glass$Type)

# This is the variable we want to predict. Now we separate the data in train 
# and test

x <- runif(nrow(Glass))
train <- Glass[which(x < 0.7), ]
test <- Glass[which(x >= 0.7), ]

# We create the model and predict
svm.model <- svm(Type ~ .,
                 data = train,
                 cost = 100,
                 gamma = 1)

svm.pred <- predict(svm.model, newdata = test)

# We also create a random tree model to compare
tree.model <- rpart(Type ~ ., data = train, method = 'class')
tree.pred <- predict(tree.model, newdata = test, type = 'class')

# Creating both confusion matrix
m.svm <- confusionMatrix(svm.pred,test$Type)
m.tree <- confusionMatrix(tree.pred,test$Type)


m.svm
m.tree

# In both confusion matrix it is easy to see that we have more successful 
# predictions where we have more values.

# This is inherent to any model, as it is best trained with more data.

# Counting the number of successful predictions of each model

svm.acc <- m.svm$overall['Accuracy'] #67.56%
svm.tree <- m.tree$overall['Accuracy'] #63.51%



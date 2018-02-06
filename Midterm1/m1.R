
#Read in the datasets
setwd("/Users/Bear/Studies/Harvard/ESLR/Midterm1")
wine.white <- read.csv("winequality-white.csv", header = TRUE, sep=";")
wine.red <- read.csv("winequality-red.csv", header = TRUE, sep=";")


#Some exploratory analysis to get a feel for the data
# White wine, we can see that we have outliers on fixed.acidity that impacts the max value
# We can examine to see if this affects many or few points with a boxplot

#Volatile acidity also has a rather large max value, however, we can see that its not due to outliers but rather
#a skew in the data distribution. Log transform

#

#Also to confirm that numerics have been read in properly and not as factors or strings
summary(wine.white)

attach(wine.white)

#We can see one/two major outliers that we can remove
par(mfrow=c(1,1))
plot(fixed.acidity)
boxplot(fixed.acidity)


#Not outliers
plot(volatile.acidity)

#We can see right skewing of the data
hist(volatile.acidity)

#Appears to be log normally distributed.Transform to Normal
hist(log(volatile.acidity))


#One clear outlier, however some strange occurrences between index 1400 -1700
plot(citric.acid)
plot(citric.acid[1400:1700])


#One clear outlier, prob 2-3 outliers
plot(residual.sugar)

hist(residual.sugar)
#Attempt at log transformation shows bimodal
hist(log(residual.sugar))



plotType <- function(domain=wine.white,FUN=hist){
  FUN = match.fun(FUN)
  par(mfrow=c(3,4))
  for (i in 1:dim(domain)[2]){
    FUN(domain[,i],xlab = names(domain)[i],main = "")
  }
}
plotType(domain = wine.white, FUN=plot)
plotType(domain = wine.white, FUN=hist)
plotType(domain = wine.white, FUN=boxplot)

#quality.num <- wine.white$quality
#quality.factor <- as.factor(quality.num)

#cv.lm(quality ~., data=)


train=sample(c(TRUE,FALSE), nrow(wine.white),rep=TRUE)
test =(! train )







library(caret)
set.seed(1234)
flds <- createFolds(wine.white, k = 10, list = TRUE, returnTrain = FALSE)
names(flds)[1] <- "train"

trainIndex <- createDataPartition(data.frame(wine.white), p=.9, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]


library(xgboost)
data(agaricus.train, package='xgboost') 

data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

dim(train$data)
dim(test$data)


model <- xgboost(data = train$data, label = train$label,
                 nrounds = 2, objective = "binary:logistic")


#Drop the quality variable
s <- wine.white[,-12]

bst <- xgb.cv(data = s, label = wine.white$quality, nfold = 5,
                 nrounds = 2, objective = "binary:logistic")

pred <- predict(model, t$data)


err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))


#XGBoost 
set.seed(1)
n = nrow(wine.white)
trainIndex = sample(1:n, size = round(0.7*n), replace=FALSE)
white.train = wine.white[trainIndex ,]
white.test = wine.white[-trainIndex ,]


#Remove label
labels = white.train['quality']
#white.train = white.train[-grep('quality', colnames(white.train))]
names(white.train)


dtrain <- xgb.DMatrix(data.matrix(white.train[,-1]), label=t(labels))
dtest <- xgb.DMatrix(data.matrix(white.test))

param <- list("objective" = "multi:softmax",    # binary classification 
              "eval_metric" = "merror",    # evaluation metric 
              "nthread" = 6,   # number of threads to be used 
              "max_depth" = 15,    # maximum depth of tree 
            #  "eta" = 0.08,    # step size shrinkage 
            #  "subsample" = 0.9,    # part of data instances to grow tree 
            #  "colsample_bytree" = 0.9)  # subsample ratio of columns when constructing each tree 
               "num_class" = 10)

xgb <- xgboost(data =dtrain, 
               max_depth = 15, 
               nround=25, 
               objective = "multi:softmax",
               num_class = 10,
               nthread = 3
)

pred <- predict(xgb, dtest)





print(length(pred))


ww <- wine.white %>% mutate(quality = quality -1)

# Make split index
train_index <- sample(1:nrow(ww), nrow(ww)*0.75)
# Full data set
data_variables <- as.matrix(ww[,-12])
data_label <- ww[,"quality"]
data_matrix <- xgb.DMatrix(data = as.matrix(ww), label = data_label)
# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)



numberOfClasses <- 9
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 50 # number of XGBoost rounds
cv.nfold  <- 5
# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)


OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train_label+1 )
head(OOF_prediction)

# confusion matrix

confusionMatrix(factor(OOF_prediction$label), 
                factor(OOF_prediction$max_prob),
                mode = "everything")

bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)
# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
confusionMatrix(factor(test_prediction$label),
                factor(test_prediction$max_prob),
                mode = "everything")


err <- mean(test_prediction$label != test_prediction$max_prob)
print(paste("test-error=", err))

# get the feature real names
names <-  colnames(ww)
# compute feature importance matrix
importance_matrix = xgb.importance(feature_names = names, model = bst_model)
head(importance_matrix)








library(randomForest)
train_data   <- ww[train_index,]
test_data <- ww[-train_index,]
#train_data$quality <- as.factor(train_data$quality)
model <- randomForest(quality ~ . , data = train_data)
#test_data$quality <- as.factor(test_data$quality)
pred <- predict(model, newdata = test_data)
err <- mean(round(pred,0) != test_data$quality)

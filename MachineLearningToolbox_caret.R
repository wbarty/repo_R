library(caret)

# Classification Model -> qualitative
# Regression Model -> quantitative
mydata = data()
x =
regressors = c()
# use metrics to evaluate the model (e.g. RMSE)

#############################################################################
# Regression Models - linear regression
#############################################################################

# training and test set 80/20 split
split <- round(nrow(mydata) * .80)
training = mydata[1:split, ]
test = mydata[(split + 1):nrow(mydata), ]
# select variables to regress upon and predictor variables (~. to select all)
model = lm(x ~. , training)
# create prediction using out-of-sample data
prediction = predict(model, test, type = 'response')
# calculate RMSE
actual = dataset[out-of-sample data, x]
sqrt(mean((prediction-actual)^2))

#############################################################################
# Cross-validation
#############################################################################

set.seed(42) # reproducability
# fit a linear regression model, trControl defines how carat cross-validates
model = train(x ~., mydata,
    method = 'lm',
    trControl = trainControl(
     method = 'repeatedcv',
     number = 10,
     repeats = 5 ,
     verboseIter = T
     )
)

#############################################################################
# Classification Models - Logistic regression
#############################################################################
# training and test set 60/40 split
split <- round(nrow(mydata) * .60)
training = mydata[1:split, ]
test = mydata[(split + 1):nrow(mydata), ]
# fit glm model
model = glm(x ~., family = 'binomial', mydata)
# predict model
prediction = predict(model, test, type = 'response')

####################
# Confusion Matrices
####################
p_n = ifelse(prediction > 0.5, 1, 0) # 50% cutoff threshold
p_class = factor(p_n, levels = levels(test))
confusionMatrix(p_class, test)

####################
# ROC curve, plots true/false positive rates at all thresholds
####################
library(caTools)
colAUC(prediction, test, plotROC = T)
#AUC, perfect model has AUC = 1, 0.5 is a random model
# custom model control
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = T,
  verboseIter = TRUE
)
model <- train(
  x ~ .,
  mydata,
  method = "glm",
  trControl = myControl
)

#############################################################################
# Random Forest machine learning model - Decision Trees
#############################################################################
# use hyperparameters which may need to be specified manually
# mtry is the number of hyperparameters (variables) used at each split, lower value = more random, higher value = less random
set.seed(42)
model = train(x ~.,
 tuneLength = 1
 data = mydata,
 method = 'ranger'
 trControl = trainControl(
    method = 'cv',
    number = 5,
    verboseIter = T
    )
 )
plot(model) #higher/lower predictors will have higher accuracy
# custom tuning grid using ones own estimates for hyperparameters
mygrid = data.frame(.mtry = c(2,3,4,5,10),
 .splitrule = 'variance',
 .min.node.size = 5
 )

set.seed(42)
model = train(x ~.,
 tuneGrid = mygrid
 data = mydata,
 method = 'ranger'
 )
plot(model)

#############################################################################
# glmnet - an extension of the glm model, deals with collinearity/overfitting
#############################################################################
# Lasso regression: penalises number of non-zero coefficients
# Ridge regression: penalises absolute magnitude of coefficients
# glmnet will find a parsimonious model of these two regressions (its a combination of the two)

# alpha[0 to 1] is pure ridge to pure lasso, values in between are a mix
# lambda[0 to infinity] is the size of the penalty
mygrid = expand.grid(alpha = 0:1, lambda = seq(0.0001, 0.1, length = 10))
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = T,
  verboseIter = TRUE
)
model <- train(
  x ~ .,
  mydata,
  method = "glmnet",
  tuneGrid = mygrid
  trControl = myControl
)
max(model[["results"]][["ROC"]])
plot(model) # check to see what values of alpha and lambda are most effective
plot(model$finalModel) # shows full regularisation path

####################
# preProccess - ?preProcess
####################

model <- train(
  x = x,
  y = y,
  method = "glm",
  trControl = myControl,
  preProcess = c("---Impute", "centre", "scale", "pca")
)
# start with imputation, for linear models centre and scale, try PCA and spatial sign, tree-based models dont need preProcessing just imputation

####################
# Median Imputation - dealing with missing values (random NA's)
####################

model = train(X, Y, preProcess = 'medianImpute')
model <- train(
  x = x,
  y = y,
  method = "glm",
  trControl = myControl,
  preProcess = "medianImpute"
)
print(min(model$results$RMSE))

####################
# KNN Imputation - dealing with missing values (patterned NA's)
####################

model = train(X, Y, preProcess = 'knnImpute')
model <- train(
  x = x,
  y = y,
  method = "glm",
  trControl = myControl,
  preProcess = "knnImpute"
)
print(min(model$results$RMSE))



####################
# Handling low-information predictors, low-variance/nearly constant variables
####################

# to preProcess: (before imputation)
# add "zv" to remove constant columns
# add "nzv" to remove nearly constant columns

####################
# PCA analysis
####################
# An alternative to removing low-variance predictors is to run PCA on your dataset. This is sometimes preferable because it does not throw out all of your data: many different low variance predictors may end up combined into one high variance PCA variable, which might have a positive impact on your model's accuracy.


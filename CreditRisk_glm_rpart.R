library(glm)
library(rpart)
library(rpart.plot)
library(pROC)

mydata = data(...)

####################
# Interpolation
####################

na.spline(mydata) # interpolate x values (graphing)
na.approx(mydata) # interpolate data
na.locf(mydata) # interpolate and connect points

####################
# Training and Test sets
####################

set.seed(567)
index_training_data = sample(1:nrow(mydata), 2/3*nrow(mydata))
training = mydata[index_training_data]
test = mydata[-index_training_data]

####################
# Confusion Matrices
####################
_________
|TN|_|FP|
|FN|_|TP|

# Classification accuracy = (TP+TN)/ALL
# Sensitivity = TP/(TP+FN)
# Specificity = TN/(TN+FP)

#############################################################################
# Logistic Regression
#############################################################################

reg1 = glm(var_dep ~ var_indep + var_indep + var_indep, family = 'binomial', data = training)
# for regressing var_dep by all variables
reg1 = glm(var_dep ~. , family = 'binomial', data = training)

#############################################################################
# PD predictor
#############################################################################

as.data.frame(test[1, ])
predict(log_model, newdata = test) # gives linear predictor
pred = predict(log_model, newdata = test, type = 'response') # gives Probability of Default
range(pred) # low range means the model may not be a good fit and not discriminatory enough for predictions

# evaluate confusion matrix
cutoff = 0.51 # x > 0.51 = default
conf = ifelse(pred > cutoff, true_value, false_value)
table(mydata$test, conf)

#############################################################################
# Decision Trees
#############################################################################

weight_def = 3
weight_non_def = 1

tree = rpart(var1 ~.,
    method = 'class',
    data = training,
    control = rpart.control(minsplit = 5, minbucket = 2, cp = 0.001), # cp - complexity parameter, if cp isnt met a split will occur, minsplit - minimmum number of splits per node, minbucket - minimum number of observations in each leaf node
    weights = c(weight_def, weight_non_def),
    parms = list(prior = c(non_default_proportion, default_proportion), # includes prior probability
        loss = matrix(c(0, cost_default_as_nondefault, cost_default_as_default, 0) ncol=2)), # penalises for misclassifying a default i.e. c(0, 10, 1, 0)
)
plot(tree, uniform = T)
text(tree)

####################
# Pruning - minimising cross-validation error
####################

plotcp(tree) # visualise cross-validation error
printcp(tree) # prints table of splits and errors

idx = which.min(table$var[, 'xerror'])
min = table$var[idx, 'CP']
pruned_tree = prune(table_tree, cp = min)
prp(pruned_tree, extra = 1)

#############################################################################
# Bad Rates
#############################################################################

prob_default = predict(training, newdata = test)
cutoff_prior = quantile(prob_default, 0.8) # 0.8 - acceptance rate
bin_pred_prior_80 = ifelse(prob_default > cutoff_prior, 1, 0)
accepted_status_prior_80 = test$table[prob_default == 0]
bad_rate = sum(accepted_status_prior_80) / test$table

#############################################################################
# ROC curve
#############################################################################

# Sensitivity/(1-specificity)
# |          / <-- cutoff = 0
# |        /
# |      /
# |    /
# |  /
# |/ <-- cutoff = 1
# |______________
# a higher AUC is best

pred_roc = predict(log_model, newdata = test)
roc(test$table, pred_roc)
auc(pred_roc)

# use AUC pruning to find higherst AUC model
reg2 = glm(var_dep ~ var_indep + var_indep + var_indep, family = 'binomial', data = training)
pred_reg2 = predict(reg2, newdata = test, type = 'response')
AUC_reg2 = auc(test$table, pred_reg2)
# repeat for all (independent) variable combinations and choose highest AUC

#we've got a multiclass classification problem

library(glmnet) #for regularized logistic regression
library(ranger) #for random forests
library(xgboost) #for boosted trees
library(dplyr) #data manipulation
library(LiblineaR) #logistic regression
library(ggplot2) #plotting

load("data.Rdata")

#~~~~~~~~~~~~~~~~~~~~~~~~
#L_1/L_2 Logistic Regression:
#~~~~~~~~~~~~~~~~~~~~~~~~

model_lasso <- cv.glmnet(x = design_matrix_train[1:1000,], y[1:1000],
                      family = "multinomial")
#glmnet takes a while

X <- as.matrix(design_matrix_train)
X[,1] <- scale(X[,1])
X[,"AgeuponOutcome"] <- scale(X[,"AgeuponOutcome"])

model_lin <- LiblineaR(data = X, target = y)
#34% CV Error

X <- as.matrix(design_matrix_test)

X[,1] <- scale(X[,1])
X[,"AgeuponOutcome"] <- scale(X[,"AgeuponOutcome"])

predict(model_lin, X, proba = TRUE) -> temp

temp <- temp$probabilities
temp <- temp[, c(3, 5, 2, 1, 4)]
colnames(temp) <- levels(y)

solution_lin <- data.frame(ID = test$ID, temp)


#~~~~~~~~~~~~~~~~
#Random Forests:
#~~~~~~~~~~~~~~~~
model_rf <- ranger(OutcomeType ~DateTime +
                       AnimalType +
                       SexuponOutcome +
                       age +
                       AgeuponOutcome +
                       weekend +
                       hour +
                       breed1 +
                       minutes +
                       namelength +
                       named +
                       wday +
                       mix,
                   data = train, mtry = 4,
                   num.trees = 800, probability = TRUE,
                   importance = "impurity", write.forest = TRUE,
                   seed = 3231L)
#performs much better if we use probability trees. + we probabilities are better
#for kaggle since the metric is multi logloss.

model_rf$prediction.error #27.6% OOB error for the probability tree - seems weird.

#variable importance:
model_rf$variable.importance %>% sort(decreasing = TRUE) %>%
    barplot(las = 1, main = "Variable Importance for Random Forest")

predict(model_rf, test) ->rf_pred

rf_pred <- rf_pred$predictions


#~~~~~~~~~
#xgboost:
#~~~~~~~~~

#something is broken with this design_matrix,
#probably the new features.
design_matrix <- sparse.model.matrix( ~   DateTime +
                                          AnimalType +
                                          SexuponOutcome +
                                          age +
                                          less_month +
                                          AgeuponOutcome +
                                          weekend +
                                          hourz +
                                          minute0 + #new
                                          minute +
                                          n + #LEAK
                                          time + #new
                                          miniature + #new
                                          agressive + #new
                                          breed1 +
                                          namelength +
                                          named +
                                          wday +
                                          mix, 
                                      data = data)[,-1]

design_matrix_train <- design_matrix[1:dim(train)[1],]
design_matrix_test <- design_matrix[-(1:dim(train)[1]),]


nround = 400
cv_xgb <- xgb.cv(data = design_matrix_train,label = new_y, 
                 nround = nround,
                 eta = 0.05,
                 objective = "multi:softprob",
                 eval_metric = "mlogloss",
                 #eval_metric = "merror",
                 num_class = 5,
                 max.depth = 8,
                 nfold = 5,
                 min_child_weight = 1,
                 subsample=0.75, 
                 colsample_bytree=0.85,
                 gamma = 0.1)

#0.724 with eta = 0.05, 250 trees and max_depth = 8.
#0.730360 with numeric hourz

#to beat with new numeric date and age and breed:
#0.740 with eta = 0.1, 300 trees 

#but got around 0.72 on the leaderboard!

ggplot(cv_xgb, aes(x = 1:nround)) +
    geom_line(aes(y = train.mlogloss.mean)) +
    geom_line(aes( y = test.mlogloss.mean), color = "red")

which.min(cv_xgb$test.mlogloss.mean) #num of trees 
min(cv_xgb$test.mlogloss.mean)

#replicate(15, {
model_xgb <- xgboost(data = design_matrix_train,
                     label = new_y, 
                     nround = 350,
                     eta = 0.05,
                     objective = "multi:softprob",
                     eval_metric = "mlogloss",
                     num_class = 5,
                     max.depth = 8,
                     min_child_weight = 1,
                     subsample=0.75, 
                     colsample_bytree=0.85,
                     gamma = 0.1)#;
#predict(model_xgb, design_matrix_test)}) -> predz


#let's look at importance
xgb.importance(feature_names = colnames(design_matrix_train), model = model_xgb) -> importance

ggplot(importance, aes( x = reorder(Feature, Gain), y = Gain)) +
    geom_bar(stat = "identity") +
    coord_flip()

predict(model_xgb, design_matrix_test) -> temp
solution <- t(matrix(temp, nrow = 5))
colnames(solution) <- levels(y)


solution <- data.frame(ID = test$ID, solution)

write.csv(solution, "xgboosttry_withnumericft+minutes+hourz2+leak.csv", row.names = FALSE)
#keep as template for predicting probabilities with xgboost.

#trying out some ensembles:
solution_stack <- data.frame((rf_pred + solution)/2)
solution_stack$ID <- test$ID

solution_rf <- data.frame(rf_pred)
solution_rf$ID <- test$ID

best <- read.csv("xgboosttry_withnumericfeats.csv")
best_rf <- (best[,-1] + rf_pred)/2
best_rf$ID <- test$ID

write.csv(best_rf, "rf800trees+bestxgb.csv", row.names = FALSE)


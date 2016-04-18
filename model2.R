library(ranger)

#RF
model_ranger <- ranger(OutcomeType ~ 
                           AnimalType +
                           SexuponOutcome +
                           age +
                           weekend +
                           hour +
                           named +
                           mix,
                          # breed1,
                       data = train[1:10000,],
                       num.trees = 200,
                       importance = "impurity",
                       #probability = TRUE, #this is KEY
                       mtry = 3,
                       write.forest = TRUE)

#tune mtry:
tune.mtry(1:5, n = 3, OutcomeType ~ 
              AnimalType +
              SexuponOutcome +
              age +
              weekend +
              hour +
              named +
              mix,
          # breed1,
          data = train,
          num.trees = 200,
          importance = "impurity",
          probability = TRUE, #this is KEY
          write.forest = TRUE)

#variable importance.
model_ranger$variable.importance %>% sort(decreasing = TRUE) 

#predictions:
predict(model_ranger, test) -> pred
prediction <- pred$predictions
solution1 <- data.frame('ID' = test$ID, prediction)
write.csv(x = solution, file = "rf11_ranger_prediction.csv", row.names = FALSE)


#caret gbm tuning:
library(caret)
library(gbm)

control <- trainControl(method="repeatedcv", number=5, repeats=1)


gbmGrid <-  expand.grid(interaction.depth = c(1:8),
                        n.trees = 200,
                        shrinkage = c(0.1, 0.05),
                        n.minobsinnode = 20)

modelgbm <- train(OutcomeType ~ 
                      AnimalType +
                      SexuponOutcome +
                      #age +
                      weekend +
                      #hour +
                      named,
                      #mix,
                  # breed1,
                  data = train[1:5000,], 
                  method = "gbm",
                  trControl=control,
                  tuneGrid = gbmGrid)

modelgbm$bestTune
modelgbm$results
plot(modelgbm)


#XGBOOST - from https://github.com/fhlgood/K_sa/blob/master/Modeltrain.R

#tree depth: 4, 6, 8 , 10 - grid search

params <- list("objective" = "multi:softprob",
               "eta" = 0.1, 
               "eval_metric" = "mlogloss",
               "num_class" = 5,
               "maxdepth" = 8)

new_y <- as.numeric(y) - 1

model_xgb <- xgboost(data = design_matrix_train, label = new_y,
                     nround = 100,
                     verbose = 1,
                     params = params)


# cross-validation
nround =50
set.seed(123)

temp <- numeric(0)
for (i in c(1,6)) {
params <- list("objective" = "multi:softprob",
               "eta" = 0.1, 
               "eval_metric" = "mlogloss",
               "maxdepth" = i,
               "num_class" = 5);

bst.cv <-  xgb.cv(params = params,
                  data = design_matrix_train,
                  label = new_y, 
                  nfold = 5, 
                  nround = nround, 
                  verbose = T,
                  early.stop.round = 5);

bst.cv$test.mlogloss.mean %>% min %>% append(temp, .) -> temp
}

# cv error plot
cv_error <- bst.cv$test.mlogloss.mean
tr_error <- bst.cv$train.mlogloss.mean
min <- which.min(cv_error)
print(paste(min, cv_error[min]))

# plot
ggplot(bst.cv, aes(x = c(1: dim(bst.cv)[1])))+
    geom_line(aes(y = train.mlogloss.mean), color = "green")+
    geom_line(aes(y = test.mlogloss.mean), color = "blue")+
    geom_vline(aes(xintercept = min), color = "red")+
    xlab("number of iterations")+
    ylab("mlogloss")


#predict using xgboost:
predict(model_xgb, design_matrix_test) -> sol.xgb

#you have to do it by row here!!
sol.xgb <- matrix(sol.xgb, ncol = 5, byrow = TRUE)

colnames(sol.xgb) <- colnames(solution[-1])

solution <- data.frame('ID' = test$ID, sol.xgb)
write.csv(x = solution, file = "rxgb1.csv", row.names = FALSE)


#use rf plus xgboost:

predict(model_ranger, test) -> pred
prediction <- pred$predictions
solution1 <- data.frame('ID' = as.numeric(test$ID), prediction)


colnames(sol.xgb) <- colnames(solution[-1])
solution2 <- data.frame('ID' = as.numeric(test$ID), sol.xgb)

solution <- (solution1 + solution2)/2 

write.csv(x = solution, file = "rfplusxgb1.csv", row.names = FALSE)

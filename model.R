library(LiblineaR)
library(ranger)
library(randomForest)

# run clean.R first
#REG LOGISTIC REGRESSSION:
design_matrix <- as.matrix(design_matrix_train)

model_gen <- LiblineaR(design_matrix, y, type = 0)

predict(model_gen, design_matrix, proba = TRUE)$probabilities -> temp

table(unlist(predict(model_gen, design_matrix)), y) ->results
sum(diag(results))/sum(results)
#65.5 % accuracy on gen



#RANDOM FORESTS:

model_rf <- randomForest(design_matrix, y, ntree = 50)

table(predict(model_rf, design_matrix), y) -> results
sum(diag(results))/sum(results)

plot(model_rf, ylim = c(0, 1))

varImpPlot(model_rf)

#RANGER roughly 5 times faster:
model_ranger<- ranger(y~.,  data = df, num.trees = 500,
                      write.forest = TRUE, probability = TRUE)



#MULTILOGLOSS:
MultiLogLoss <- function(act, pred)
{
    eps = 1e-15;
    nr <- nrow(pred)
    pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
    pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
    ll = sum(act*log(pred) + (1-act)*log(1-pred))
    ll = ll * -1/(nrow(act))      
    return(ll);
}

model.matrix(~ 0 + y) -> realpred

predict(model_ranger, df) -> predicts

MultiLogLoss(predicts$predictions, realpred)



#GBM:
model_gbm <- gbm.fit(design_matrix_gen, y,
                     distribution="multinomial", shrinkage=0.05,
                     n.trees=500,
                     interaction.depth=6L,
                     train.fraction=0.8,
                     keep.data=FALSE,
                     verbose=TRUE)



predict(model_gbm, design_matrix_gen, type = "response")[1:5]
      
  
#xgboost:
library(xgboost)


params <- list("objective" = "multi:softprob",
               "eta" = .1,
               "max_depth" = 8,
               "eval_metric" = "mlogloss",
               "num_class" = 5,
               "subsample" = .8)


model_xgb <- xgboost(data = design_matrix, label = y,
                     nround = 200,
                     verbose = 1,
                     params = params)

# cross-validation
nround =50
set.seed(123)
bst.cv <-  xgb.cv(params = params, data = design_matrix, label = y, nfold = 10, 
                  nround = nround, verbose = T)

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

   
# predicting:
predict(model_xgb, design_matrix_test) -> temp 


temp <- matrix(temp, nrow = dim(test)[1])
                    

predict(model_ranger, df_test) -> temp

prediction <- temp$predictions

solution <- data.frame('ID' = test$ID, prediction)

write.csv(x = solution, file = "rf5_ranger_prediction.csv", row.names = FALSE)



                     
library(xgboost)
library(ggplot2)
library(caret)
# this is on the animal outcomes data:

load("data.Rdata")

sample<- design_matrix_train[1:5000,]
sample_y <- new_y[1:5000]

model <- xgboost(data = sample, label = sample_y,
                 nround = 100,
                 objective = "multi:softprob",
                 num_class = 5)


nround = 100
xgb.cv(data = sample, label = sample_y,
         nround = nround,
         objective = "multi:softprob",
         metrics = "mlogloss",
         num_class = 5,
         eta = 0.1,
         max.depth = 3,
         min_child_weight = 1,
         colsample_bytree = 1,
         gamma = 1,
         nfold = 5, verbose = 1) -> cv

which.min(cv$test.merror.mean)

min(cv$test.merror.mean)
    
ggplot(cv, aes(x = 1:nround)) +
    geom_line(aes(y = train.merror.mean), alpha = 0.5) +
    geom_line(aes(y = test.merror.mean), color = "red") +
    geom_point(aes(x = which.min(cv$test.merror.mean),
                   y = min(cv$test.merror.mean)), size = 5)+
    ylim(c(0.2, 0.4)) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#trying xgboost hyperparameter tuning with caret:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

getModelInfo("xgbTree")$xgbTree$parameters #get the parameters.

xgbGrid <- expand.grid(max_depth = 5:7,
                        nrounds = 200,
                        eta = 0.1,
                        min_child_weight = c(1, 0.8),
                        colsample_bytree = 1,
                        gamma = 1)

trControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1,
                          classProbs = TRUE)


modelxgb <- train(OutcomeType ~ AnimalType + SexuponOutcome + age +
                      hour + named + mix,
                  data = train[1:5000,], 
                  method = "xgbTree",
                  trControl= trControl,
                  tuneGrid = xgbGrid)


plot(modelxgb)




library(xgboost)
# this is on the animal outcomes data:

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
         num_class = 5,
         eta = 0.1,
         max.depth = 3,
         min_child_weight = 1,
         colsample_bytree = 1,
         gamma = 1,
         nfold = 5, verbose = 0) -> cv

which.min(cv$test.merror.mean)

min(cv$test.merror.mean)
    
ggplot(cv, aes(x = 1:nround)) +
    geom_line(aes(y = train.merror.mean), alpha = 0.5) +
    geom_line(aes(y = test.merror.mean), color = "red") +
    geom_point(aes(x = which.min(cv$test.merror.mean),
                   y = min(cv$test.merror.mean)), size = 5)+
    ylim(c(0.2, 0.4)) 


#trying xgboost with caret:
#this works very nicely:

xgbGrid <- expand.grid(max_depth = 1:5,
                        nrounds = 200,
                        eta = 0.1,
                        min_child_weight = c(1, 0.8),
                        colsample_bytree = 1,
                        gamma = 1)

modelxgb <- train(OutcomeType ~ 
                      AnimalType + SexuponOutcome + age + weekend + hour + named + mix,
                  data = train[1:5000,], 
                  method = "xgbTree",
                  trControl=control,
                  tuneGrid = xgbGrid)

train(OutcomeType ~  AnimalType + SexuponOutcome +
          #age +
          weekend +
          #hour +
          named,
      #mix,
      # breed1,
      data = train[1:5000,], 
      method = "xgbTree",
      trControl=control)

plot(modelxgb)


#comparison with the randomforest algo.
library(randomForest)
ntree = 400
model_rf <- randomForest(x = as.matrix(sample), y = as.factor(sample_y), ntree = ntree)
plot(model_rf)

err <- data.frame(model_rf$err.rate)


ggplot(cv, aes(x = 1:nround)) +
   # geom_line(aes(y = train.merror.mean), alpha = 0.5) +
    geom_line(aes(y = test.merror.mean), color = "red") +
    geom_point(aes(x = which.min(cv$test.merror.mean),
                   y = min(cv$test.merror.mean)), size = 5)+
    geom_line(data = err, aes(x = 1:ntree, y = OOB)) +
    ylim(c(0.2, 0.4)) +
    scale_color_brewer(palette = "Set1")


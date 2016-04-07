library(LiblineaR)
library(randomForest)
library(ranger)


y = as.factor(train$OutcomeType)
design_matrix <- cbind(design_matrix_in)
df<- data.frame(design_matrix, y)

#REG LOGISTIC REGRESSSION:

model_gen <- LiblineaR(design_matrix, y, type = 0)

predict(model_gen, design_matrix, proba = TRUE)$probabilities -> temp

table(unlist(predict(model_gen, design_matrix)), y) ->results
sum(diag(results))/sum(results)
#64 % accuracy on gen

#58% for dogs.
#78% for cats.

#breed 6% bad accuracy, color 30%
model_gen <- LiblineaR(design_matrix_color, y, type = 0)

table(unlist(predict(model_gen, design_matrix_gen)), y) ->results
sum(diag(results))/sum(results)

#let's put them all together - breed and color barely help.
design_matrix <- cbind(design_matrix_gen, design_matrix_breed)

model <- LiblineaR(design_matrix_gen, y, type = 1)

table(unlist(predict(model, design_matrix)), y) ->results
sum(diag(results))/sum(results)

#RANDOM FORESTS:

model_rf <- randomForest(design_matrix, y, ntree = 50)

table(predict(model_rf, design_matrix), y) -> results
sum(diag(results))/sum(results)

plot(model_rf, ylim = c(0, 1))
model_rf$err.rate

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
        
#xgboost - what does this even mean?

model_xgb <- xgboost(design_matrix_gen, y, )
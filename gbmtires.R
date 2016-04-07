library(gbm)
library(reshape2)

gbm1 <- gbm.fit(x = design_matrix, y, 
    distribution="multinomial",
    shrinkage=0.05,
    n.trees=400,
    interaction.depth=6,
    train.fraction=0.8,
    keep.data=FALSE,
    verbose=TRUE
)

#plots the best number of trees
temp <- gbm.perf(gbm1, oobag.curve = TRUE, overlay = TRUE, method ="OOB")
temp <- gbm.perf(gbm1)


trainPreds2 <- predict(gbm1,design_matrix, type = "response")
dim(trainPreds2) = c(26729, 5)

round(trainPreds2) -> temp

gbm.perf(gbm1)


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


model.matrix(~ 0 + y) -> temp

predict(model_ranger, df) -> predicts

MultiLogLoss(predicts$predictions, temp)

pred1 = c(0.8,0.2)
pred2 = c(0.6,0.4)
pred <- rbind(pred1,pred2)
pred
act1 <- c(1,0)
act2 <- c(1,0)
act <- rbind(act1,act2)
MultiLogLoss(act,pred)

MultiLogLoss(predict(model_rf, design_matrix, type = "vote"), temp)

temp <- (predict(model_rf, design_matrix, type = "vote") + trainPreds2)/2


MultiLogLoss(temp, model.matrix(~ 0 + y))

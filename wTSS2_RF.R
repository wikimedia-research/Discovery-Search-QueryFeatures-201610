library(randomForest)
######
# Zero Result Rate
######
standardize <- function(x) { return((x - mean(x))/sd(x)) }

searches_subset <- apply(as.matrix(searches[, .(n_terms, n_chars, n_feats)]),2,as.numeric)
searches_subset[, "n_terms"] <- standardize(log10(searches_subset[, "n_terms"] + 1))
searches_subset[, "n_chars"] <- standardize(log10(searches_subset[, "n_chars"] + 1))
searches_subset[, "n_feats"] <- standardize(sqrt(searches_subset[, "n_feats"]))
temp <- cbind(as.matrix(searches[,.(zero_result)]), searches_subset, as.matrix(features_matrix))
rm(searches_subset)

training_idx <- sample.int(nrow(temp), floor(0.8 * nrow(temp)), replace = FALSE)
test_idx <- setdiff(1:nrow(temp), training_idx)
x_train <- temp[training_idx, -1]; x_test <- temp[test_idx, -1]
y_train <- factor(temp[training_idx, 1], 0:1, c("some results", "zero results"))
y_test <- factor(temp[test_idx, 1], 0:1, c("some results", "zero results"))
rm(temp)

# Without Language:
library(foreach)
library(doMC)
registerDoMC(cores = 4)

rf <- foreach(ntree = rep(125, 4), .combine = combine, .multicombine = TRUE, .maxcombine = 4) %dopar%
  randomForest(x = x_train, xtest = x_test, y = y_train, ytest = y_test,
               mtry = 17, ntree = ntree,
               importance = TRUE, keep.forest = TRUE, proximity = FALSE)
rf2 <- randomForest(x = x_train, xtest = x_test, y = y_train, ytest = y_test,
               mtry = 17, ntree = 500,
               importance = TRUE, keep.forest = TRUE, proximity = FALSE)

# plot
# Accuracy:
sum(rf$test$predicted==y_test)/length(y_test) #0.866661


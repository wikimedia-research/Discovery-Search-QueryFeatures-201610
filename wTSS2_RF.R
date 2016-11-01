library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
library(randomForest)
library(caret)
library(parallel)
library(pROC)

dataset_to_use <- "web_excl-known-automata_with_logs.rds"
source("config.R")
load("data/searches.RData")
features_matrix <- readr::read_rds(file.path("data", paste0("features-matrix_", dataset_to_use)))

######
# Zero Result Rate
######
set.seed(777)
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

### Tuning RF
mtry_max <- dim(x_train)[2]
n_folds <- 10
fold_ind <- sample.int(n_folds, length(training_idx), replace = T)

cl <- makeCluster(mc <- getOption("cl.cores", 10))
clusterExport(cl, list("fold_ind","n_folds","x_train","y_train", "mtry_max"))
tuning_set <- parSapply(cl, 1:n_folds, function(this_fold){
  library(randomForest)
  # Tuning mtry
  auc_rf <- sapply(1:mtry_max, function(m){
    rf <- randomForest(x = x_train[fold_ind!=this_fold, ], 
                       y = y_train[fold_ind!=this_fold],
                       nodesize = 10, mtry = m, 
                       sampsize=rep(min(table(y_train[fold_ind!=this_fold])),2), 
                       strata=y_train[fold_ind!=this_fold], 
                       importance = FALSE, keep.forest = T, proximity = FALSE)
    pred_prob <- predict(rf, x_train[fold_ind==this_fold, ], type = "prob")[,2]
    library(pROC)
    return(as.numeric(auc(y_train[fold_ind==this_fold], pred_prob)))
  })
  return(auc_rf)
})
stopCluster(cl)

tuning_result <- data.frame(mtry=1:mtry_max, 
                            point.est = apply(tuning_set, 1, mean),
                            std.dev = apply(tuning_set, 1, sd),
                            lower = apply(tuning_set, 1, function(x) unname(quantile(x, 0.025))),
                            upper = apply(tuning_set, 1, function(x) unname(quantile(x, 0.975))))

print(tuning_result$mtry[which.max(tuning_result$point.est)])
print(max(tuning_result$point.est))
#save(tuning_result, file = "rf_tuning_result_zrr.RData")
# auc=0.7162208 at mtry = 16

# Train RF with tuned mtry:
set.seed(777)
rf <- randomForest(x = x_train, xtest = x_test, y = y_train, ytest = y_test,
               mtry = 16, ntree = 500, nodesize = 10, 
               sampsize=rep(min(table(y_train)),2), strata=y_train, 
               importance = TRUE, keep.forest = TRUE, proximity = FALSE)
#save(rf, file = "random_forest_zrr.RData")
# plot
# Accuracy, confusion matrix, AUC
sum(rf$test$predicted==y_test)/length(y_test) #0.7056147
rf$test$confusion
pred_prob <- predict(rf, x_test, type = "prob")[,2]; auc(y_test, pred_prob) #0.7172

######
# Clickthrough
######
set.seed(777)
standardize <- function(x) { return((x - mean(x))/sd(x)) }

zero_mask <- searches$zero_result
searches_subset <- apply(as.matrix(searches[!zero_mask, .(n_terms, n_chars, n_feats)]),2,as.numeric)
searches_subset[, "n_terms"] <- standardize(log10(searches_subset[, "n_terms"] + 1))
searches_subset[, "n_chars"] <- standardize(log10(searches_subset[, "n_chars"] + 1))
searches_subset[, "n_feats"] <- standardize(sqrt(searches_subset[, "n_feats"]))
temp <- cbind(as.matrix(searches[!zero_mask,.(clickthrough)]), searches_subset, as.matrix(features_matrix[!zero_mask,]))
rm(searches_subset)

training_idx <- sample.int(nrow(temp), floor(0.8 * nrow(temp)), replace = FALSE)
test_idx <- setdiff(1:nrow(temp), training_idx)
x_train <- temp[training_idx, -1]; x_test <- temp[test_idx, -1]
y_train <- factor(temp[training_idx, 1], 0:1, c("No Click", "Clickthrough"))
y_test <- factor(temp[test_idx, 1], 0:1, c("No Click", "Clickthrough"))
rm(temp)

### Tuning RF
mtry_max <- dim(x_train)[2]
n_folds <- 10
fold_ind <- sample.int(n_folds, length(training_idx), replace = T)

cl <- makeCluster(mc <- getOption("cl.cores", 10))
clusterExport(cl, list("fold_ind","n_folds","x_train","y_train", "mtry_max"))
tuning_set <- parSapply(cl, 1:n_folds, function(this_fold){
  library(randomForest)
  # Tuning mtry
  auc_rf <- sapply(1:mtry_max, function(m){
    rf <- randomForest(x = x_train[fold_ind!=this_fold, ], 
                       y = y_train[fold_ind!=this_fold],
                       nodesize = 10, mtry = m, 
                       sampsize=rep(min(table(y_train[fold_ind!=this_fold])),2), 
                       strata=y_train[fold_ind!=this_fold], 
                       importance = FALSE, keep.forest = T, proximity = FALSE)
    pred_prob <- predict(rf, x_train[fold_ind==this_fold, ], type = "prob")[,2]
    library(pROC)
    return(as.numeric(auc(y_train[fold_ind==this_fold], pred_prob)))
  })
  return(auc_rf)
})
stopCluster(cl)

tuning_result <- data.frame(mtry=1:mtry_max, 
                            point.est = apply(tuning_set, 1, mean),
                            std.dev = apply(tuning_set, 1, sd),
                            lower = apply(tuning_set, 1, function(x) unname(quantile(x, 0.025))),
                            upper = apply(tuning_set, 1, function(x) unname(quantile(x, 0.975))))

print(tuning_result$mtry[which.max(tuning_result$point.est)])
print(max(tuning_result$point.est))
#save(tuning_result, file = "tuning_result.RData")
# auc=0.5288875 at mtry = 5(0.01 of all data)
# auc=0.5371213 at mtry = 9(0.1 of all data)

# Train RF with tuned mtry:
set.seed(777)
rf <- randomForest(x = x_train, xtest = x_test, y = y_train, ytest = y_test,
                   mtry = 9, ntree = 500, nodesize = 10, 
                   sampsize=rep(min(table(y_train)),2), strata=y_train, 
                   importance = TRUE, keep.forest = TRUE, proximity = FALSE)
# save(rf, file = "random_forest_ctr.RData")
# plot
# Accuracy, confusion matrix, AUC
sum(rf$test$predicted==y_test)/length(y_test) #0.4399294(strata) #0.6742779(no strata)
rf$test$confusion
pred_prob <- predict(rf, x_test, type = "prob")[,2]; auc(y_test, pred_prob)#0.5414(strata) #0.5004(no strata)


######
# Paul Score
######
set.seed(777)
standardize <- function(x) { return((x - mean(x))/sd(x)) }

clk_mask <- searches$clickthrough
searches_subset <- apply(as.matrix(searches[clk_mask, .(`Query score (F=0.1)`, `Query score (F=0.5)`, `Query score (F=0.9)`, n_terms, n_chars, n_feats)]),2,as.numeric)
searches_subset[, "n_terms"] <- standardize(log10(searches_subset[, "n_terms"] + 1))
searches_subset[, "n_chars"] <- standardize(log10(searches_subset[, "n_chars"] + 1))
searches_subset[, "n_feats"] <- standardize(sqrt(searches_subset[, "n_feats"]))
temp <- cbind(searches_subset, as.matrix(features_matrix[clk_mask,]))
rm(searches_subset)

training_idx <- sample.int(nrow(temp), floor(0.1 * nrow(temp)), replace = FALSE)
test_idx <- setdiff(1:nrow(temp), training_idx)
x_train <- temp[training_idx, -c(1:3)]; x_test <- temp[test_idx, -c(1:3)]
y1_train <- standardize(temp[training_idx, 1]); y5_train <- standardize(temp[training_idx, 2]); y9_train <- standardize(temp[training_idx, 3]); 
y1_test <- standardize(temp[test_idx, 1]); y5_test <- standardize(temp[test_idx, 2]); y9_test <- standardize(temp[test_idx, 3]); 
rm(temp)

### Tuning RF
mtry_max <- dim(x_train)[2]
n_folds <- 10
fold_ind <- sample.int(n_folds, length(training_idx), replace = T)

cl <- makeCluster(mc <- getOption("cl.cores", 4))
clusterExport(cl, list("fold_ind","n_folds","x_train","y9_train", "mtry_max"))
tuning_set <- parSapply(cl, 1:n_folds, function(this_fold){
  library(randomForest)
  # Tuning mtry
  mse_rf <- sapply(1:mtry_max, function(m){
    rf <- randomForest(x = x_train[fold_ind!=this_fold, ], 
                       y = y9_train[fold_ind!=this_fold],
                       xtest = x_train[fold_ind==this_fold, ], 
                       ytest = y9_train[fold_ind==this_fold],
                       nodesize = 10, mtry = m, 
                       importance = FALSE, keep.forest = T, proximity = FALSE)
    return(mean(rf$test$mse))
  })
  return(mse_rf)
})
stopCluster(cl)

tuning_result <- data.frame(mtry=1:mtry_max, 
                            point.est = apply(tuning_set, 1, mean),
                            std.dev = apply(tuning_set, 1, sd),
                            lower = apply(tuning_set, 1, function(x) unname(quantile(x, 0.025))),
                            upper = apply(tuning_set, 1, function(x) unname(quantile(x, 0.975))))

print(tuning_result$mtry[which.min(tuning_result$point.est)])
print(min(tuning_result$point.est))
# F=0.1: mtry= ; mse = 
# F=0.5: mtry= ; mse = 
# F=0.9: mtry= ; mse = 

# Train RF with tuned mtry:
set.seed(777)
rf <- randomForest(x = x_train, xtest = x_test, y = y1_train, ytest = y1_test,
                   mtry = 9, ntree = 500, nodesize = 10, 
                   importance = TRUE, keep.forest = TRUE, proximity = FALSE)
# save(rf, file = "random_forest_ps.RData")
# plot
# MSE, R squared
rf$test$mse
rf$test$rsq

if(FALSE){
  # Tuning RF
  metric <- "ROC"
  control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs = TRUE, summaryFunction = twoClassSummary, search="grid")
  tunegrid <- expand.grid(.mtry=c(1:24))
  library(doMC)
  registerDoMC(cores = 16)
  rf_gridsearch <- train(factor(zero_result, 0:1, c("some_results", "zero_results"))~., data=as.data.frame(train_set), 
                         method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, verbose = FALSE,
                         prox=FALSE, allowParallel=TRUE)
  print(rf_gridsearch)
  plot(rf_gridsearch)
}
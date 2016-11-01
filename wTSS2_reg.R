library(glmnet)
library(doParallel)
library(Matrix)
library(data.table)

standardize <- function(x) { return((x - mean(x))/sd(x)) }
dataset_to_use <- "web_excl-known-automata_with_logs.rds"
source("config.R")
load("data/searches.RData")
features_matrix <- readr::read_rds(file.path("data", paste0("features-matrix_", dataset_to_use)))

######
# Zero Result Rate
######
set.seed(777)
searches_subset <- apply(as.matrix(searches[, .(n_terms, n_chars, n_feats)]),2,as.numeric)
searches_subset[, "n_terms"] <- standardize(log10(searches_subset[, "n_terms"] + 1))
searches_subset[, "n_chars"] <- standardize(log10(searches_subset[, "n_chars"] + 1))
searches_subset[, "n_feats"] <- standardize(sqrt(searches_subset[, "n_feats"]))
temp <- cbind(as.matrix(searches[,.(zero_result)]), searches_subset, as.matrix(features_matrix))
rm(searches_subset)

training_idx <- sample.int(nrow(temp), floor(0.8 * nrow(temp)), replace = FALSE)
test_idx <- setdiff(1:nrow(temp), training_idx)
x_test <- temp[test_idx, -1]
y_test <- factor(temp[test_idx, 1], 0:1, c("some results", "zero results"))
# downsampling
x_train <- temp[training_idx, -1]
y_train <- temp[training_idx, 1]
n_train <- sum(y_train)
x_train <- rbind(x_train[y_train==1,], 
                 x_train[y_train==0,][sample.int(nrow(x_train)-n_train, n_train, replace=F), ])
y_train <- factor(rep(c(1,0), each=n_train), 0:1, c("some results", "zero results"))
rm(temp)

# Tuning glmnet
cl <- makeCluster(10)
registerDoParallel(cl)
out_compare <- NULL
foldid=sample(1:10,size=length(y_train),replace=TRUE)
for (alpha in seq(0, 1, by = 0.1)){
  cvfit <- cv.glmnet(x_train, y_train, family = "binomial", type.measure = "auc", foldid=foldid, alpha = alpha, intercept=F, parallel = TRUE)
  out_compare <- rbind(out_compare, c(alpha, cvfit$lambda.min, max(cvfit$cvm)))
}
colnames(out_compare) <- c("alpha","lambda.min","cvm")
stopCluster(cl)
alpha_fin <- out_compare[which.max(out_compare[,3]),1] #0  
lambda_fin <- out_compare[which.max(out_compare[,3]),2] #0.02368375

# Fit glmnet with tuned alpha and lambda
fit <- glmnet(x_train, y_train, family = "binomial", alpha = alpha_fin, intercept=F)
test_class <- predict(fit, newx = x_test, s = lambda_fin ,type = "class")
# Accuracy, confusion matrix, AUC
sum(test_class==y_test)/length(test_class) #0.6946972
caret::confusionMatrix(test_class, y_test)
test_prob <- predict(fit, newx = x_test, s = lambda_fin ,type = "response")[,1]; auc(y_test, test_prob) #0.6726

# Save result
logit_coefs <- as.matrix(coef(fit, s = lambda_fin))
logit_coefs <- data.frame(Variable=rownames(logit_coefs), Coefficient=logit_coefs[,1], row.names=NULL)
save(logit_coefs, file="logit_coefs_zrr.RData")
#system("scp chelsyx@stat3:~/logit_coefs_zrr.RData results/")


######
# Paul Score
######
set.seed(777)

clk_mask <- searches$clickthrough
searches_subset <- apply(as.matrix(searches[clk_mask, .(`Query score (F=0.1)`, `Query score (F=0.5)`, `Query score (F=0.9)`, n_terms, n_chars, n_feats)]),2,as.numeric)
searches_subset[, "n_terms"] <- standardize(log10(searches_subset[, "n_terms"] + 1))
searches_subset[, "n_chars"] <- standardize(log10(searches_subset[, "n_chars"] + 1))
searches_subset[, "n_feats"] <- standardize(sqrt(searches_subset[, "n_feats"]))
temp <- cbind(searches_subset, as.matrix(features_matrix[clk_mask,]))
rm(searches_subset)

training_idx <- sample.int(nrow(temp), floor(0.8 * nrow(temp)), replace = FALSE)
test_idx <- setdiff(1:nrow(temp), training_idx)
x_train <- temp[training_idx, -c(1:3)]; x_test <- temp[test_idx, -c(1:3)]
y1_train <- standardize(temp[training_idx, 1]); y5_train <- standardize(temp[training_idx, 2]); y9_train <- standardize(temp[training_idx, 3]); 
y1_test <- standardize(temp[test_idx, 1]); y5_test <- standardize(temp[test_idx, 2]); y9_test <- standardize(temp[test_idx, 3]); 
rm(temp)

# Choose alpha,lambda by cv
cl <- makeCluster(4)
registerDoParallel(cl)
out_compare <- NULL
foldid=sample(1:10,size=length(y9_train),replace=TRUE)
for (alpha in seq(0, 1, by = 0.1)){
	cvfit <- cv.glmnet(x_train, y9_train, type.measure = "mse", foldid=foldid, alpha = alpha, intercept=T, parallel = TRUE)
	out_compare <- rbind(out_compare, c(alpha, cvfit$lambda.min, min(cvfit$cvm)))
}
colnames(out_compare) <- c("alpha","lambda.min","cvm")
stopCluster(cl)
alpha_fin <- out_compare[which.min(out_compare[,3]),1] 
lambda_fin <- out_compare[which.min(out_compare[,3]),2]
#0.1 alpha=1 lambda=0.001733801
#0.5 alpha=1 lambda=0.001028922
#0.9 alpha=0 lambda=0.07934394

fit1<- glmnet(x_train, y1_train, alpha = 1, intercept=T); coef.exact = coef(fit1, s = 0.001733801, exact = TRUE)
fit5<- glmnet(x_train, y5_train, alpha = 1, intercept=T); coef.exact = coef(fit5, s = 0.001028922, exact = TRUE)
fit9<- glmnet(x_train, y9_train, alpha = 0, intercept=T); coef.exact = coef(fit9, s = 0.07934394, exact = TRUE)
pred1 <- predict(fit1,newx=x_test,s=0.001733801, exact=T); sqrt(mean((pred1-y1_test)^2)) #0.9948913
pred5 <- predict(fit5,newx=x_test,s=0.001028922, exact=T); sqrt(mean((pred5-y5_test)^2)) #0.9960856
pred9 <- predict(fit9,newx=x_test,s=0.07934394, exact=T); sqrt(mean((pred9-y9_test)^2)) #0.9997686

# lambda is very small, so we can use lm to see
lmfit1 <- lm(y1_train~x_train); summary(lmfit1)

######
# Clickthrough
######
set.seed(777)

zero_mask <- searches$zero_result
searches_subset <- apply(as.matrix(searches[!zero_mask, .(n_terms, n_chars, n_feats)]),2,as.numeric)
searches_subset[, "n_terms"] <- standardize(log10(searches_subset[, "n_terms"] + 1))
searches_subset[, "n_chars"] <- standardize(log10(searches_subset[, "n_chars"] + 1))
searches_subset[, "n_feats"] <- standardize(sqrt(searches_subset[, "n_feats"]))
temp <- cbind(as.matrix(searches[!zero_mask,.(clickthrough)]), searches_subset, as.matrix(features_matrix[!zero_mask,]))
rm(searches_subset)

training_idx <- sample.int(nrow(temp), floor(0.8 * nrow(temp)), replace = FALSE)
test_idx <- setdiff(1:nrow(temp), training_idx)
x_test <- temp[test_idx, -1]
y_test <- factor(temp[test_idx, 1], 0:1, c("no click", "clickthrough"))
# downsampling
x_train <- temp[training_idx, -1]
y_train <- temp[training_idx, 1]
n_train <- sum(y_train)
x_train <- rbind(x_train[y_train==1,], 
                 x_train[y_train==0,][sample.int(nrow(x_train)-n_train, n_train, replace=F), ])
y_train <- factor(rep(c(1,0), each=n_train), 0:1, c("no click", "clickthrough"))
rm(temp)

# Choose alpha,lambda by cv
cl <- makeCluster(10)
registerDoParallel(cl)
out_compare <- NULL
foldid=sample(1:10,size=length(y_train),replace=TRUE)
for (alpha in seq(0, 1, by = 0.1)){
  cvfit <- cv.glmnet(x_train, y_train, family = "binomial", type.measure = "auc", foldid=foldid, alpha = alpha, intercept=F, parallel = TRUE)
  out_compare <- rbind(out_compare, c(alpha, cvfit$lambda.min, max(cvfit$cvm)))
}
colnames(out_compare) <- c("alpha","lambda.min","cvm")
stopCluster(cl)
alpha_fin <- out_compare[which.max(out_compare[,3]),1] #0.3(strata) 0.8(no strata)
lambda_fin <- out_compare[which.max(out_compare[,3]),2] #0.002523441(strata) 0.000780903(no strata)

# Fit glmnet with tuned alpha and lambda
fit <- glmnet(x_train, y_train, family = "binomial", alpha = alpha_fin, intercept=F)
# Accuracy, confusion matrix, AUC
test_class <- predict(fit, newx = x_test, s = lambda_fin ,type = "class")
sum(test_class[,1]==y_test)/length(test_class[,1]) # 0.4801099(strata) 0.6742465(no strata)
caret::confusionMatrix(test_class, y_test)
test_prob <- predict(fit, newx = x_test, s = lambda_fin ,type = "response")[,1]; auc(y_test, test_prob) #0.5314(strata) 0.5312(no strata)

# Save result
logit_coefs <- as.matrix(coef(fit, s = lambda_fin))
logit_coefs <- data.frame(Variable=rownames(logit_coefs), Coefficient=logit_coefs[,1], row.names=NULL)
save(logit_coefs, file="logit_coefs_clt.RData")
#system("scp chelsyx@boo.eqiad.wmflabs:~/logit_coefs_clt.RData results/")

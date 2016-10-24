library(glmnet)
library(doParallel)
library(Matrix)
set.seed(777)

queries <- readr::read_rds("web_excl-known-automata_refined.rds")
features_matrix <- readr::read_rds("features-matrix_web_excl-known-automata.rds")
standardize <- function(x) { return((x - mean(x))/sd(x)) }
use_language <- TRUE

# Preprocess
queries_subset <- as.matrix(as.data.frame(queries)[, c("zero_result", "n_terms", "n_chars", "n_feats")])
queries_subset[, "n_terms"] <- standardize(log10(queries_subset[, "n_terms"] + 1))
queries_subset[, "n_chars"] <- standardize(log10(queries_subset[, "n_chars"] + 1))
queries_subset[, "n_feats"] <- standardize(sqrt(queries_subset[, "n_feats"]))
temp <- as(cbind(queries_subset, as.matrix(features_matrix)), "sparseMatrix")
if(use_language == T){
  language <- sparse.model.matrix(~language-1, data = queries)
  temp <- cBind(temp, language)
  rm(language)
}
rm(queries_subset, queries, features_matrix)

test_idx <- sample.int(nrow(temp), floor(0.2 * nrow(temp)), replace = FALSE)
remainder_idx <- setdiff(1:nrow(temp), test_idx)
validate_idx <- sample(remainder_idx, floor(0.5 * length(remainder_idx)))
training_idx <- setdiff(remainder_idx, validate_idx)

x_validate <- temp[validate_idx, -1]
x_test <- temp[test_idx, -1]
x_train <- temp[training_idx, -1]
y_validate <- factor(temp[validate_idx, 1], 0:1, c("some results", "zero results"))
y_test <- factor(temp[test_idx, 1], 0:1, c("some results", "zero results"))
y_train <- factor(temp[training_idx, 1], 0:1, c("some results", "zero results"))
rm(temp)


# Parallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Choose alpha by cv

out_compare <- NULL

foldid=sample(1:5,size=length(y_validate),replace=TRUE)

for (alpha in seq(0, 1, by = 0.1)){
	cvfit <- cv.glmnet(x_validate, y_validate, family = "binomial", type.measure = "auc", foldid=foldid, alpha = alpha, intercept=F, parallel = TRUE)
	out_compare <- rbind(out_compare, c(alpha, cvfit$lambda.min, max(cvfit$cvm)))
}
colnames(out_compare) <- c("alpha","lambda.min","cvm")

stopCluster(cl)


# Fit model with chosen alpha and lambda, generate probabilities

alpha_fin <- out_compare[which.max(out_compare[,3]),1] #0.4  #with language: 0.2
lambda_fin <- out_compare[which.max(out_compare[,3]),2] #0.01005795 #with language: 0.0005342958
fit <- glmnet(x_train, y_train, family = "binomial", alpha = alpha_fin, intercept=F)
# https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
# coef(fit, s = lambda_fin) 
# no coef confidence interval http://stackoverflow.com/questions/12937331/how-to-get-statistical-summary-information-from-glmnet-model
test_prob <- predict(fit, newx = x_test, s = lambda_fin ,type = "response")
test_class <- predict(fit, newx = x_test, s = lambda_fin ,type = "class")
sum(test_class==y_test)/length(test_class) # 0.8293367 #with language: 0.8364605

# Save result
logit_coefs <- as.matrix(coef(fit, s = lambda_fin))
logit_coefs <- data.frame(Variable=rownames(logit_coefs), Coefficient=logit_coefs[,1], row.names=NULL)
save(logit_coefs, file="logit_coefs.RData")
#system("scp chelsyx@stat3:~/logit_coefs.RData results/")

# logistic regression without penalty

logistic_regression <- glmnet(x_train, y_train, family = "binomial", alpha = 0, lambda=0, intercept=F)
prediction_logit <- predict(logistic_regression, newx = x_test, s=0, type = "class")
sum(prediction_logit==y_test)/length(prediction_logit) # 0.8311267 #with language: 0.8365153

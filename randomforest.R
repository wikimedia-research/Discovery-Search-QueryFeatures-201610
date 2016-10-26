library(data.table)
library(magrittr)
library(randomForest)
if (!"subtitle" %in% names(formals(ggplot2::ggtitle))) {
  devtools::install_github("hadley/ggplot2")
}
library(ggplot2)
library(cowplot)

dataset_to_use <- "web_excl-known-automata.rds"
source("config.R")
queries <- readr::read_rds(file.path(data_root, dataset_to_use))
# queries <- readr::read_rds("web_excl-known-automata_refined.rds")
source("refine.R") # yields: features_matrix
# features_matrix <- readr::read_rds("features-matrix_web_excl-known-automata.rds")

standardize <- function(x) { return((x - mean(x))/sd(x)) }


queries_subset <- as.matrix(as.data.frame(queries)[, c("zero_result", "n_terms", "n_chars", "n_feats")])
queries_subset[, "n_terms"] <- standardize(log10(queries_subset[, "n_terms"] + 1))
queries_subset[, "n_chars"] <- standardize(log10(queries_subset[, "n_chars"] + 1))
queries_subset[, "n_feats"] <- standardize(sqrt(queries_subset[, "n_feats"]))
temp <- cbind(queries_subset, as.matrix(features_matrix))
# library(Matrix)
# temp <- as(temp, "sparseMatrix") #print(temp[1:100,], col.names=T)
# language <- sparse.model.matrix(~language-1,data=queries)
# temp <- cBind(temp, language)
rm(queries_subset, queries, features_matrix, language)
# temp <- as.matrix(temp)

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
               samplesize = 2800000, nodesize = 99, mtry = 33, ntree = ntree,
               importance = TRUE, keep.forest = TRUE, proximity = FALSE)
# save(rf, file = "random_forest.RData")

#system("scp chelsyx@stat3:~/random_forest.RData .")
load("random_forest.RData") 
import::from(dplyr, keep_where = filter)
variable_importance <- data.frame(Variable = rownames(importance(rf)), importance(rf)) %>%
  set_rownames(NULL)
p1 <- ggplot(data = keep_where(variable_importance, abs(zero.results) > 1),
             aes(x = reorder(Variable, -zero.results),
                 y = zero.results)) +
  geom_bar(stat = "identity", aes(fill = ifelse(zero.results >= 0, "Higher", "Lower"))) +
  scale_fill_manual(values = RColorBrewer::brewer.pal(3, "Set1")[2:1],
                    guide = guide_legend(title = "Chances of zero results")) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 16) +
  labs(x = "Variable", y = "Mean Importance", subtitle = fig_subtitle,
       title = "(a) Importance of features in predicting zero results.") +
  theme(legend.position = "bottom")
p2 <- ggplot(data = keep_where(variable_importance, abs(MeanDecreaseGini) > 1),
             aes(x = reorder(Variable, -MeanDecreaseGini),
                 y = MeanDecreaseGini/max(MeanDecreaseGini))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 16) +
  labs(x = "Variable", y = "Mean Decrease Gini (Relative to Max)",
       subtitle = fig_subtitle,
       title = "(b) Importance of features in partitioning.")
p3 <- ggplot(data = keep_where(variable_importance, abs(MeanDecreaseAccuracy) > 1),
             aes(x = reorder(Variable, -MeanDecreaseAccuracy),
                 y = MeanDecreaseAccuracy/max(MeanDecreaseAccuracy))) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 16) +
  labs(x = "Variable", y = "Mean Decrease in Accuracy (Relative to Max)",
       subtitle = fig_subtitle,
       title = "(c) Importance of features in accuracy.") +
  theme(legend.position = "bottom")
p <- plot_grid(p1, p2, p3, nrow = 1)
print(p)
ggsave("var_imp.png", p, path = fig_path, units = "in", dpi = 300, height = 8, width = 24)

################################################

# Caret
train_idx <- sample.int(nrow(temp), size=1000)
rf_model<-train(factor(zero_result, 0:1, c("some results", "zero results"))~.,data=temp[train_idx,],method="rf",
                trControl=trainControl(method="cv",number=2),
                prox=FALSE,allowParallel=TRUE)

library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl)

metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=2, repeats=1, search="grid")
tunegrid <- expand.grid(.mtry=c(1:25))
start_time <- Sys.time()
rf_gridsearch <- train(factor(zero_result, 0:1, c("some results", "zero results"))~., data=temp, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, allowParallel=TRUE)
cat("It takes ", Sys.time()-start_time)
print(rf_gridsearch)
plot(rf_gridsearch)

stopCluster(cl)

################################################

# misclassification_errors$m[which.min(misclassification_errors$point.est)] # 16?
rf <- randomForest(x = temp[training_idx, -1], xtest = temp[test_idx, -1],
                   y = factor(temp[training_idx, 1], 0:1, c("some results", "zero results")),
                   ytest = factor(temp[test_idx, 1], 0:1, c("some results", "zero results")),
                   ntree = 200, samplesize = ceiling(0.6 * nrow(temp)),
                   nodesize = 5, mtry = 12, # mtry = floor(sqrt(ncol(temp)-1)), # tune with CV
                   importance = TRUE, keep.forest = TRUE, proximity = TRUE, do.trace = TRUE)

# plot(rf); varImpPlot(rf); par(mfrow = c(1, 1))
# MDSplot(rf, fac = factor(temp[test_idx, 1], 0:1, c("some results", "zero results")))

library(data.table)
library(magrittr)
#library(caret)
library(randomForest)
library(parallel)
if (!"subtitle" %in% names(formals(ggplot2::ggtitle))) {
  devtools::install_github("hadley/ggplot2")
}
library(ggplot2)
library(cowplot)

dataset_to_use <- "web_excl-known-automata.rds"
source("config.R")
# queries <- readr::read_rds("web_excl-known-automata_refined.rds")
# features_matrix <- readr::read_rds("features-matrix_web_excl-known-automata.rds")

standardize <- function(x) { return((x - mean(x))/sd(x)) }

set.seed(777)

mtry_max <- 28
n_train <- 20000
n_repeat <- 100
train_idx <- matrix(sample.int(nrow(queries), size = n_train*n_repeat, replace = FALSE), ncol=n_repeat)
queries <- queries[,.(zero_result, n_terms, n_chars, n_feats, language)]

cl <- makeCluster(mc <- getOption("cl.cores", 5))
clusterExport(cl, list("queries","features_matrix","standardize","mtry_max"))
tuning_set <- parApply(cl, train_idx, 2, function(training_set){
  
  library(randomForest)
  # Preprocess
  queries_train <- as.matrix(as.data.frame(queries)[training_set, c("zero_result", "n_terms", "n_chars", "n_feats")])
  queries_train[, "n_terms"] <- standardize(log10(queries_train[, "n_terms"] + 1))
  queries_train[, "n_chars"] <- standardize(log10(queries_train[, "n_chars"] + 1))
  queries_train[, "n_feats"] <- standardize(sqrt(queries_train[, "n_feats"]))
  #countries_train <- dummy::dummy(as.data.frame(queries)[training_set, "country", drop = FALSE])
  language_train <- dummy::dummy(as.data.frame(queries)[training_set, "language", drop = FALSE])
  features_matrix_train <- as.matrix(features_matrix[training_set, ])
  temp <- cbind(queries_train, features_matrix_train)
  #temp <- cbind(queries_train, features_matrix_train, countries_train)
  temp <- cbind(queries_train, features_matrix_train, language_train)
  rm(queries_train, features_matrix_train)
  
  # Assign
  this_train <- sample(nrow(temp), size=0.5*nrow(temp), replace = FALSE)
  this_test <- setdiff(1:nrow(temp), this_train)

  # Tuning mtry
  accuracy_rf <- sapply(1:mtry_max, function(m){
    rf <- randomForest(x = temp[this_train, -1], 
                       y = factor(temp[this_train, 1], 0:1, c("some results", "zero results")),
                       nodesize = 3, mtry = m,
                       importance = FALSE, keep.forest = T, proximity = FALSE)
    predictions <- predict(rf, temp[this_test, -1], type = "response")
    levels(predictions) <- union(levels(predictions), c("some results", "zero results"))
    true_labels <- factor(temp[this_test, 1], 0:1, c("some results", "zero results"))
    return(sum(predictions==true_labels)/length(this_test))
  })
  
  return(accuracy_rf)
})
stopCluster(cl)

tuning_result <- data.frame(mtry=1:mtry_max, 
                            point.est = apply(tuning_set, 1, mean),
                            std.dev = apply(tuning_set, 1, sd),
                            lower = apply(tuning_set, 1, function(x) unname(quantile(x, 0.025))),
                            upper = apply(tuning_set, 1, function(x) unname(quantile(x, 0.975))))

print(tuning_result$mtry[which.max(tuning_result$point.est)])
print(max(tuning_result$point.est))
# 0.83187 at mtry = 17 (without language data)
# 0.836859 at mtry = 33 (with language data)

save(tuning_result, file = "tuning_result.RData")

load("tuning_result.RData")
p <- ggplot(data = tuning_result,
            aes(x = mtry, y = point.est)) +
  geom_ribbon(aes(ymax = upper, ymin = lower), alpha = 0.1) +
  geom_line() + geom_point() +
  geom_errorbar(aes(ymax = point.est + std.dev, ymin = point.est - std.dev)) +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  scale_y_continuous("Accuracy", labels = scales::percent_format()) +
  scale_x_continuous(breaks = tuning_result$mtry) +
  labs(x = "Number of Variables Considered At Each Split",
       subtitle = paste("Validating a random forest with", n_repeat, "sets of", n_train, "training and validating units each."),
       title = "Tuning Random Forest Parameter") +
  geom_vline(xintercept = tuning_result$mtry[which.max(tuning_result$point.est)],
             linetype = "dashed")
print(p)
ggsave("accuracy_with_language.png", p, path = fig_path, units = "in", dpi = 150, height = 8, width = 12)


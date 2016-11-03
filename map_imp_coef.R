# devtools::install_github("slowkow/ggrepel")
library(ggrepel)
library(magrittr)
library(randomForest)
library(ggplot2)
library(cowplot)

dataset_to_use <- "web_excl-known-automata_with_logs.rds"
source("config.R")

# ZRR
load("results/random_forest_zrr.RData") 
load("results/logit_coefs_zrr.RData") 
variable_importance <- data.frame(Variable = rownames(importance(rf)), importance(rf)) %>%
  set_rownames(NULL)
temp <- dplyr::left_join(variable_importance, logit_coefs, by = "Variable")

annotations <- data.frame(
  xpos = c(-0.8,-0.8,1.1,1.1),
  ypos =  c(-2.5, 3,-2.5, 3),
  hjustvar = c(0,0,1,1),
  vjustvar = c(0,1,0,1),
  feature = c("less important\nzero results less likely",
              "less important\nzero results more likely",
              "more important\nzero results less likely",
              "more important\nzero results more likely"))
annotations_top <- dplyr::filter(annotations, ypos > 0)
annotations_bottom <- dplyr::filter(annotations, ypos < 0)

p <- temp %>%
  set_colnames(c("Variable",
                 "MDA specific to queries with some results",
                 "MDA specific to queries with zero results",
                 "MDA (across all queries)",
                 "MDI Gini", "Coefficient")) %>%
  tidyr::gather(metric, var.imp, 2:5) %>%
  dplyr::group_by(metric) %>%
  dplyr::mutate(var.imp = var.imp/max(var.imp)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Coefficient)) %>%
  dplyr::mutate(lbl.a = ifelse(var.imp > 0, "more important", "less important"),
                lbl.b = ifelse(Coefficient > 0, "zero results more likely", "zero results less likely"),
                feature = paste(lbl.a, lbl.b, sep = "\n")) %>%
  ggplot(aes(x = var.imp, y = Coefficient)) +
  geom_point(aes(color = feature)) +
  geom_text_repel(aes(label = Variable), force=10) +
  scale_color_brewer(type = "qual", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_y_continuous(limits = c(-2.5, 3)) + scale_x_continuous(limits = c(-0.8, 1.1)) +
  facet_wrap(~metric, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(data = annotations_top,
            aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar,
                label = feature, color = feature)) +
  geom_text(data = annotations_bottom,
            aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar,
                label = feature, color = feature)) +
  labs(title = "Variable Importance vs Logistic Regression Estimates", subtitle = fig_subtitle,
       x = "(Relative) Variable Importance via Mean Decrease Accuracy",
       y = "Logistic Regression Coefficient") +
  ggthemes::theme_few(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom")
print(p)
ggsave("mda_logitcoef_zrr.png", p, path = fig_path, units = "in", dpi = 300, height = 12, width = 16)
rm(temp, p)

# Clickthrough
load("results/random_forest_ctr.RData") 
load("results/logit_coefs_clt.RData") 
variable_importance <- data.frame(Variable = rownames(importance(rf)), importance(rf)) %>%
  set_rownames(NULL)
temp <- dplyr::left_join(variable_importance, logit_coefs, by = "Variable")

annotations <- data.frame(
  xpos = c(-1.8,-1.8,1.1,1.1),
  ypos =  c(-1.3, 0.4,-1.3, 0.4),
  hjustvar = c(0,0,1,1),
  vjustvar = c(0,1,0,1),
  feature = c("less important\nclickthrough less likely",
              "less important\nclickthrough more likely",
              "more important\nclickthrough less likely",
              "more important\nclickthrough more likely"))
annotations_top <- dplyr::filter(annotations, ypos > 0)
annotations_bottom <- dplyr::filter(annotations, ypos < 0)

p <- temp %>%
  set_colnames(c("Variable",
                 "MDA specific to queries with zero click",
                 "MDA specific to queries with clickthrough",
                 "MDA (across all queries)",
                 "MDI Gini", "Coefficient")) %>%
  tidyr::gather(metric, var.imp, 2:5) %>%
  dplyr::group_by(metric) %>%
  dplyr::mutate(var.imp = var.imp/max(var.imp)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Coefficient)) %>%
  dplyr::mutate(lbl.a = ifelse(var.imp > 0, "more important", "less important"),
                lbl.b = ifelse(Coefficient > 0, "clickthrough more likely", "clickthrough less likely"),
                feature = paste(lbl.a, lbl.b, sep = "\n")) %>%
  ggplot(aes(x = var.imp, y = Coefficient)) +
  geom_point(aes(color = feature)) +
  geom_text_repel(aes(label = Variable), force =10) +
  scale_color_brewer(type = "qual", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_y_continuous(limits = c(-1.3, 0.4)) + scale_x_continuous(limits = c(-1.8, 1.1)) +
  facet_wrap(~metric, nrow = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(data = annotations_top,
            aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar,
                label = feature, color = feature)) +
  geom_text(data = annotations_bottom,
            aes(x = xpos, y = ypos, hjust = hjustvar, vjust = vjustvar,
                label = feature, color = feature)) +
  labs(title = "Variable Importance vs Logistic Regression Estimates", subtitle = fig_subtitle,
       x = "(Relative) Variable Importance via Mean Decrease Accuracy",
       y = "Logistic Regression Coefficient") +
  ggthemes::theme_few(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom")
print(p)
ggsave("mda_logitcoef_ctr.png", p, path = fig_path, units = "in", dpi = 300, height = 12, width = 16)
rm(temp, p)

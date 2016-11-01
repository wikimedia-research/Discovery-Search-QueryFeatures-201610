library(data.table)
library(magrittr)
library(tidyr)
library(dplyr)
import::from(dplyr, group_by, summarize, ungroup, mutate, rename, keep_where = filter, tbl_df, arrange, select)
# Make sure we're only using the version of ggplot2 that supports subtitles:
if (!"subtitle" %in% names(formals(ggplot2::ggtitle))) {
  devtools::install_github("hadley/ggplot2")
}
library(ggplot2)
library(cowplot)

dataset_to_use <- "web_excl-known-automata_with_logs.rds"
source("config.R")
if (!dir.exists(fig_path)) dir.create(fig_path, recursive = TRUE)
# queries <- readr::read_rds(file.path(data_root, dataset_to_use))
# queries_sep <- readr::read_rds("data/web_excl-known-automata_with_logs_sep.rds")
# queries <- rbind(queries_sep, queries)
# load("data/tss2_oct.RData") #events
# events_oct <- events
# load("data/tss2_sep.RData")
# events <- rbind(events, events_oct)
# rm(events_oct, queries_sep)
# source("refine_wTSS2.R")

load("data/searches.RData")
features_matrix <- readr::read_rds(file.path("data", paste0("features-matrix_", dataset_to_use)))

# Histogram of searches per session
temp <- searches[, j = list(requests = .N), by = "identity"][, j = list(freq = .N), by = "requests"]
temp$requests[temp$requests >= 10] <- "10+"
p <- temp[, j = list(freq = sum(freq)), by = "requests"] %>%
  mutate(prop = freq/sum(freq)) %>%
  ggplot(aes(x = requests, y = prop)) +
  geom_bar(stat = "identity") +
  scale_y_continuous("Proportion of users", labels = scales::percent_format()) +
  scale_x_discrete("Searches", limits = c(1:9, "10+")) +
  geom_text(aes(label = sprintf("%.1f%%", 100*prop)), nudge_y = 0.025) +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  ggtitle("Proportion of users (identified via 'identity' field) making N searches", subtitle = fig_subtitle)
ggsave("searches_proportions.png", p, path = fig_path, units = "in", dpi = 150, height = 5, width = 8)
rm(temp, p)

# Zero results rate by number of features
searches[, j = list(searches = .N), by = c("query_type", "zero_result", "n_feats")] %>%
  rename(`Number of Features per Query` = n_feats, `Query Type` = query_type) %>%
  mutate(zero_result = ifelse(zero_result, "Queries with zero results", "Queries with some results")) %>%
  spread(zero_result, searches, fill=0) %>%
  mutate(Queries = `Queries with zero results` + `Queries with some results`,
         `Queries with zero results` = sprintf("%.1f%%", 100*`Queries with zero results`/Queries),
         `Queries with some results` = sprintf("%.1f%%", 100*`Queries with some results`/Queries),
         Queries = polloi::compress(Queries)) %>%
  knitr::kable(align = c("l", "r", "r", "r", "r", "r", "r"), format = "markdown")

# Zero results rate by feature
temp <-  lapply(features_matrix, function(feature) {
  return(searches[i = feature, j = list(searches = .N), by = "zero_result"])
})
p <- temp %>%
  dplyr::bind_rows(.id = "feature") %>%
  mutate(zero_result = ifelse(zero_result, "Zero", "Some")) %>%
  tidyr::spread(zero_result, searches, fill = 0) %>%
  group_by(feature) %>%
  mutate(Total = Zero + Some, ZRR = Zero/Total) %>%
  ggplot(aes(x = reorder(feature, -ZRR), y = ZRR, fill = log10(Total))) +
  geom_bar(stat = "identity") +
  scale_x_discrete("Feature") +
  scale_y_continuous("Zero Results Rate",
                     labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.1),
                     limits = c(0, 1.2)) +
  scale_fill_gradient("Number of Queries", low = "#ffeda0", high = "#f03b20",
                      breaks = 1:7, labels = polloi::compress(10^(1:7)),
                      guide = guide_legend(keyheight = 1, heywidth = 3, nrow = 1)) +
  geom_text(aes(label = sprintf("%.1f%% of %s", 100*ZRR, polloi::compress(Total, 1))), nudge_y = 0.11) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  ggtitle("Proportion of searches with zero results by query feature", subtitle = fig_subtitle) +
  theme(legend.position = "bottom")
print(p)
ggsave("zrr_by_feature.png", p, path = fig_path, units = "in", dpi = 150, height = 10, width = 10)
rm(temp, p)

# Clickthrough rate by feature
temp <-  lapply(features_matrix[!searches$zero_result,], function(feature) {
  return(searches[i = feature, j = list(searches = .N), by = "clickthrough"])
})
p <- temp %>%
  dplyr::bind_rows(.id = "feature") %>%
  mutate(clickthrough = ifelse(clickthrough, "Click", "No Click")) %>%
  tidyr::spread(clickthrough, searches, fill = 0) %>%
  group_by(feature) %>%
  mutate(Total = Click + `No Click`, CTR = Click/Total) %>%
  ggplot(aes(x = reorder(feature, -CTR), y = CTR, fill = log10(Total))) +
  geom_bar(stat = "identity") +
  scale_x_discrete("Feature") +
  scale_y_continuous("Clickthrough Rate",
                     labels = scales::percent_format(),
                     breaks = seq(0, 0.6, 0.1),
                     limits = c(0, 0.8)) +
  scale_fill_gradient("Number of Queries", low = "#ffeda0", high = "#f03b20",
                      breaks = 1:7, labels = polloi::compress(10^(1:7)),
                      guide = guide_legend(keyheight = 1, heywidth = 3, nrow = 1)) +
  geom_text(aes(label = sprintf("%.1f%% of %s", 100*CTR, polloi::compress(Total, 1))), nudge_y = 0.11) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  ggtitle("Proportion of searches with clickthrough by query feature", subtitle = fig_subtitle) +
  theme(legend.position = "bottom")
print(p)
ggsave("ctr_by_feature.png", p, path = fig_path, units = "in", dpi = 150, height = 10, width = 10)
rm(temp, p)

# PaulScore by feature
temp <-  lapply(features_matrix[searches$clickthrough,], function(feature) {
  return(searches[i = feature, j = list(`F=0.1` = mean(`Query score (F=0.1)`), 
                                        `F=0.5` = mean(`Query score (F=0.5)`), 
                                        `F=0.9` = mean(`Query score (F=0.9)`))])
})
Total <- data.frame(feature = names(features_matrix), Total = colSums(features_matrix[searches$clickthrough,]))
p <- temp %>%
  dplyr::bind_rows(.id = "feature") %>%
  left_join(Total, by="feature") %>%
  gather(key = "Scoring Factor", value="PaulScore", `F=0.1`, `F=0.5`, `F=0.9`) %>%
  ggplot(aes(x = reorder(feature, -PaulScore), y = PaulScore, fill = log10(Total))) +
  geom_bar(stat = "identity") +
  scale_x_discrete("Feature") +
  scale_y_continuous("Mean PaulScore",
                     #labels = scales::percent_format(),
                     breaks = seq(0, 0.5, 0.1),
                     limits = c(0, 0.6)) +
  scale_fill_gradient("Number of Queries", low = "#ffeda0", high = "#f03b20",
                      breaks = 1:7, labels = polloi::compress(10^(1:7)),
                      guide = guide_legend(keyheight = 1, heywidth = 3, nrow = 1)) +
  geom_text(aes(label = sprintf("%.3f, %s searches", PaulScore, polloi::compress(Total, 1))), nudge_y = 0.07) +
  coord_flip() +
  facet_wrap(~`Scoring Factor`, nrow = 1) +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  ggtitle("Mean PaulScore by query feature", subtitle = fig_subtitle) +
  theme(legend.position = "bottom")
print(p)
ggsave("paulscore_by_feature.png", p, path = fig_path, units = "in", dpi = 150, height = 10, width = 20)
rm(temp, Total, p)

# Most common feature combinations and ZR%
temp <- searches[, j = list(searches = .N), by = c("zero_result", "features")] %>%
  mutate(zero_result = ifelse(zero_result, "Zero Results", "Some Results")) %>%
  tidyr::spread(zero_result, searches, fill = 0) %>%
  mutate(`Total Queries` = `Zero Results` + `Some Results`, `Zero Results Rate` = `Zero Results`/`Total Queries`)
p <- temp %>%
  keep_where(`Total Queries` > 50) %>%
  # ggplot(aes(x = reorder(features, -`Total Queries`), y = `Zero Results Rate`)) +
  ggplot(aes(x = reorder(features, -`Zero Results Rate`), y = `Zero Results Rate`)) +
  geom_bar(stat = "identity", aes(fill = log10(`Total Queries`))) +
  scale_fill_gradient("Number of Queries", low = "#ffeda0", high = "#f03b20",
                      breaks = 1:7, labels = polloi::compress(10^(1:7)),
                      guide = guide_legend(keyheight = 1, heywidth = 3)) +
  scale_x_discrete("Features") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, 0.1), limits = c(0, 1.2)) +
  geom_text(aes(label = sprintf("%.1f%% of %s", 100*`Zero Results Rate`, polloi::compress(`Total Queries`, 1))),
            nudge_y = 0.1) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom") +
  # ggtitle("Proportion of searches with zero results by query features (sorted by # of queries)", subtitle = fig_subtitle)
  ggtitle("Proportion of searches with zero results by query features (sorted by ZRR)", subtitle = fig_subtitle)
# ggsave("zrr_by_feature_combo_sort-count.png", p, path = fig_path, units = "in", dpi = 150, height = 17, width = 13)
print(p)
ggsave("zrr_by_feature_combo_sort-zrr.png", p, path = fig_path, units = "in", dpi = 150, height = 17, width = 13)
rm(temp, p)

# Zero results rate by country
p <- searches[, j = list(searches = .N), by = c("country", "zero_result")] %>%
  mutate(zero_result = ifelse(zero_result, "Zero Results", "Some Results")) %>%
  tidyr::spread(zero_result, searches, fill = 0) %>%
  mutate(`Total Queries` = `Zero Results` + `Some Results`,
         `Zero Results Rate` = `Zero Results`/`Total Queries`,
         `Proportion of Searches` = `Total Queries`/sum(`Total Queries`)) %>%
  dplyr::top_n(20, `Total Queries`) %>%
  ggplot(aes(x = reorder(country, -`Zero Results Rate`), y = `Zero Results Rate`)) +
  geom_bar(stat = "identity", aes(fill = `Proportion of Searches`)) +
  scale_fill_gradient("Proportion of Searches Accounted For", low = "#ffeda0", high = "#f03b20",
                      labels = scales::percent_format()) +
  scale_x_discrete("Countries") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 0.4, 0.05), limits = c(0, 0.45)) +
  geom_text(aes(label = sprintf("%.1f%% of %s", 100*`Zero Results Rate`, polloi::compress(`Total Queries`, 1))),
            nudge_y = 0.05) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom") +
  ggtitle("Zero results rate in top 20 countries by volume of searches",
          subtitle = paste(fig_subtitle, "on", as.character(head(searches$date, 1), format = "%a (%d %b %Y)")))
print(p)
ggsave("zrr_by_country.png", p, path = fig_path, units = "in", dpi = 150, height = 10, width = 10)
rm(p)

# Zero results rate by language
p <- searches[, j = list(searches = .N), by = c("language", "zero_result")] %>%
  mutate(zero_result = ifelse(zero_result, "Zero Results", "Some Results")) %>%
  tidyr::spread(zero_result, searches, fill = 0) %>%
  mutate(`Total Queries` = `Zero Results` + `Some Results`,
         `Zero Results Rate` = `Zero Results`/`Total Queries`,
         `Proportion of Searches` = `Total Queries`/sum(`Total Queries`)) %>%
  dplyr::top_n(20, `Total Queries`) %>%
  ggplot(aes(x = reorder(language, -`Zero Results Rate`), y = `Zero Results Rate`)) +
  geom_bar(stat = "identity", aes(fill = `Proportion of Searches`)) +
  scale_fill_gradient("Proportion of Searches Accounted For", low = "#ffeda0", high = "#f03b20",
                      labels = scales::percent_format()) +
  scale_x_discrete("Language") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 0.40, 0.05), limits = c(0, 0.45)) +
  geom_text(aes(label = sprintf("%.1f%% of %s", 100*`Zero Results Rate`, polloi::compress(`Total Queries`, 1))),
            nudge_y = 0.05) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom") +
  ggtitle("Zero results rate in top 20 languages by volume of searches",
          subtitle = paste(fig_subtitle, "on", as.character(head(searches$date, 1), format = "%a (%d %b %Y)")))
print(p)
ggsave("zrr_by_language.png", p, path = fig_path, units = "in", dpi = 150, height = 10, width = 10)
rm(p)



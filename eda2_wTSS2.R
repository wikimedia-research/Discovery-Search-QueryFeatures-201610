# Answer questions in https://phabricator.wikimedia.org/T147216#2782783

temp <- searches
temp$n_feats <- as.integer(temp$n_feats)
temp <- temp %>% mutate(n_feats=ifelse(features_matrix$`is simple`, n_feats-1, n_feats)) %>% as.data.table()
temp[, j = list(searches = .N), by = c("query_type", "zero_result", "n_feats")] %>%
  rename(`Number of Features per Query` = n_feats, `Query Type` = query_type) %>%
  mutate(zero_result = ifelse(zero_result, "Queries with zero results", "Queries with some results")) %>%
  spread(zero_result, searches, fill=0) %>%
  mutate(Queries = `Queries with zero results` + `Queries with some results`,
         `Queries with zero results` = sprintf("%.1f%%", 100*`Queries with zero results`/Queries),
         `Queries with some results` = sprintf("%.1f%%", 100*`Queries with some results`/Queries),
         Queries = polloi::compress(Queries)) %>%
  knitr::kable(align = c("l", "r", "r", "r", "r", "r", "r"), format = "markdown")
rm(temp)

temp <- searches[, j = list(searches = .N), by = c("query_type", "zero_result", "n_terms")] %>%
  rename(`Number of Terms per Query` = n_terms, `Query Type` = query_type) %>%
  mutate(zero_result = ifelse(zero_result, "Queries with zero results", "Queries with some results")) %>%
  spread(zero_result, searches, fill=0) %>%
  mutate(Queries = `Queries with zero results` + `Queries with some results`,
         `Queries with zero results` = sprintf("%.1f%%", 100*`Queries with zero results`/Queries),
         `Queries with some results` = sprintf("%.1f%%", 100*`Queries with some results`/Queries),
         Queries = polloi::compress(Queries))
temp[,2] <- as.integer(temp[,2])
 temp %>% arrange(`Number of Terms per Query`) %>%
  knitr::kable(align = c("l", "r", "r", "r", "r", "r", "r"), format = "markdown")
rm(temp)

temp <- searches[, j = list(searches = .N), by = c("query_type", "zero_result", "n_chars")] %>%
  rename(`Number of Characters per Query` = n_chars, `Query Type` = query_type) %>%
  mutate(zero_result = ifelse(zero_result, "Queries with zero results", "Queries with some results")) %>%
  spread(zero_result, searches, fill=0) %>%
  mutate(Queries = `Queries with zero results` + `Queries with some results`,
         `Queries with zero results` = sprintf("%.1f%%", 100*`Queries with zero results`/Queries),
         `Queries with some results` = sprintf("%.1f%%", 100*`Queries with some results`/Queries),
         Queries = polloi::compress(Queries))
temp[,2] <- as.integer(temp[,2])
temp %>% arrange(`Number of Characters per Query`) %>%
  knitr::kable(align = c("l", "r", "r", "r", "r", "r", "r"), format = "markdown")
rm(temp)

#searches %>% group_by(project, language) %>% summarise(searches=n()) %>% ungroup %>% mutate(proportion=searches/sum(searches))
searches %>% group_by(project) %>% summarise(searches=n()) %>%
  knitr::kable(format = "markdown")
searches %>% group_by(language) %>% summarise(searches=n()) %>%
  knitr::kable(format = "markdown")

# Zero results rate by project
p <- searches[, j = list(searches = .N), by = c("project", "zero_result")] %>%
  mutate(zero_result = ifelse(zero_result, "Zero Results", "Some Results")) %>%
  tidyr::spread(zero_result, searches, fill = 0) %>%
  mutate(`Total Queries` = `Zero Results` + `Some Results`,
         `Zero Results Rate` = `Zero Results`/`Total Queries`,
         `Proportion of Searches` = `Total Queries`/sum(`Total Queries`)) %>%
  #dplyr::top_n(20, `Total Queries`) %>%
  ggplot(aes(x = reorder(project, -`Zero Results Rate`), y = `Zero Results Rate`)) +
  geom_bar(stat = "identity", aes(fill = `Proportion of Searches`)) +
  scale_fill_gradient("Proportion of Searches Accounted For", low = "#ffeda0", high = "#f03b20",
                      labels = scales::percent_format()) +
  scale_x_discrete("Project") +
  scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 0.7, 0.05), limits = c(0, 0.8)) +
  geom_text(aes(label = sprintf("%.1f%% of %s", 100*`Zero Results Rate`, polloi::compress(`Total Queries`, 1))),
            nudge_y = 0.05) +
  coord_flip() +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  theme(legend.position = "bottom") +
  ggtitle("Zero results rate by projects",
          subtitle = paste(fig_subtitle))
print(p)
ggsave("zrr_by_project.png", p, path = fig_path, units = "in", dpi = 150, height = 10, width = 10)
rm(p)


temp <- searches %>% filter(language =="English") %>% group_by(n_terms) %>% summarise(searches=n()) %>% ungroup()
temp$n_terms <- as.integer(temp$n_terms)
temp$n_terms[temp$n_terms >= 10] <- "10+"
p1 <- temp %>% group_by(n_terms) %>% summarise(searches=sum(searches)) %>% ungroup() %>%
  mutate(prop = searches/sum(searches)) %>%
  ggplot(aes(x = n_terms, y = prop)) +
  geom_bar(stat = "identity") +
  scale_y_continuous("Proportion of Searches", labels = scales::percent_format()) +
  scale_x_discrete("Number of Terms", limits = c(1:9, "10+")) +
  geom_text(aes(label = sprintf("%.1f%%", 100*prop)), nudge_y = 0.025) +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  ggtitle("Proportion of searches with N terms in English", subtitle = fig_subtitle)
temp <- searches %>% filter(language =="Chinese") %>% group_by(n_terms) %>% summarise(searches=n()) %>% ungroup()
temp$n_terms <- as.integer(temp$n_terms)
temp$n_terms[temp$n_terms >= 10] <- "10+"
p2 <- temp %>% group_by(n_terms) %>% summarise(searches=sum(searches)) %>% ungroup() %>%
  mutate(prop = searches/sum(searches)) %>%
  ggplot(aes(x = n_terms, y = prop)) +
  geom_bar(stat = "identity") +
  scale_y_continuous("Proportion of Searches", labels = scales::percent_format()) +
  scale_x_discrete("Number of Terms", limits = c(1:9, "10+")) +
  geom_text(aes(label = sprintf("%.1f%%", 100*prop)), nudge_y = 0.025) +
  ggthemes::theme_tufte(base_family = "Gill Sans", base_size = 14) +
  ggtitle("Proportion of searches with N terms in Chinese", subtitle = fig_subtitle)
p <- cowplot::plot_grid(p1, p2)
ggsave("searches_by_nterm_enzh.png", p, path = fig_path, units = "in", dpi = 150, height = 5, width = 12)
rm(temp, p)


temp <- searches[features_matrix$`has non-ASCII`, ]
temp[, j = list(searches = .N), by = c("query_type", "zero_result", "language")] %>%
  rename(Language = language, `Query Type` = query_type) %>%
  mutate(zero_result = ifelse(zero_result, "Queries with zero results", "Queries with some results")) %>%
  spread(zero_result, searches, fill=0) %>%
  mutate(Queries = `Queries with zero results` + `Queries with some results`,
         `Queries with zero results` = sprintf("%.1f%%", 100*`Queries with zero results`/Queries),
         `Queries with some results` = sprintf("%.1f%%", 100*`Queries with some results`/Queries)) %>% 
  arrange(desc(Queries)) %>%
  mutate(Queries = polloi::compress(Queries)) %>%
  knitr::kable(format = "markdown")
rm(temp)

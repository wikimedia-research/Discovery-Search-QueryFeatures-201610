queries <- queries[queries$ip!="164.132.183.164", , ]
lang_proj <- polloi::parse_wikiid(queries$wikiid)
inactive_lang <- names(table(lang_proj$language))[table(lang_proj$language) <= 50]
inactive_proj <- names(table(lang_proj$project))[table(lang_proj$project) <= 50]
queries <- cbind(queries, lang_proj)
queries <- queries[!is.na(queries$project), , ] %>% mutate(language = ifelse(language %in% inactive_lang|is.na(language), "Other", language), 
                                                           project = ifelse(project %in% inactive_proj, "Other", project)) %>% as.data.table()
rm(lang_proj,inactive_lang,inactive_proj)
queries <- queries %>% rename(cirrus_id = event_logs.cirrus_id)

events_queries <- left_join(events, queries, by = c("date", "event_id", "page_id", "cirrus_id"))
events_queries <- events_queries %>% filter(!(action=="SERP" & is.na(features)))

# Correct for when user uses pagination or uses back button to go back to SERP after visiting a result.
# Start by assigning the same page_id to different SERPs that have exactly the same query:
temp <- events_queries %>%
  filter(action == "SERP") %>%
  group_by(session_id, search_id, query) %>%
  mutate(new_page_id = min(page_id)) %>%
  ungroup %>%
  select(c(page_id, new_page_id)) %>%
  distinct
# We also need to do the same for associated click events:
events_queries <- left_join(events_queries, temp, by = "page_id"); rm(temp)
# Find out which SERPs are duplicated:
temp <- events_queries %>%
  filter(action == "SERP") %>%
  arrange(new_page_id, ts) %>%
  mutate(dupe = duplicated(new_page_id, fromLast = FALSE)) %>%
  select(c(event_id, dupe))
events_queries <- left_join(events_queries, temp, by = "event_id"); rm(temp)
events_queries$dupe[events_queries$action == "click"] <- FALSE
# Remove duplicate SERPs and re-sort:
events_queries <- events_queries[!events_queries$dupe & !is.na(events_queries$new_page_id), ] %>%
  select(-c(page_id, dupe)) %>%
  rename(page_id = new_page_id) %>%
  arrange(date, session_id, search_id, page_id, desc(action), ts)

# PaulScore Calculation
query_score <- function(positions, F) {
  if (length(positions) == 1 || all(is.na(positions))) {
    # no clicks were made
    return(0)
  } else {
    positions <- positions[!is.na(positions)] # when operating on 'events' dataset, SERP events won't have positions
    return(sum(F^positions))
  }
}

# Summarize on a page-by-page basis:
searches <- events_queries %>%
  group_by(session_id, search_id, page_id) %>%
  filter("SERP" %in% action) %>% # filter out searches where we have clicks but not SERP events
  summarize(ts = ts[1], query = query[1], identity = identity[1], country = country[1], n_terms=n_terms[1], query_type=query_type[1],
            n_chars=n_chars[1], n_feats=n_feats[1], features=features[1], language=language[1], project=project[1], zero_result=zero_result[1],
            results = ifelse(n_results_returned[1] > 0, "some", "zero"),
            clickthrough = "click" %in% action,
            `first clicked result's position` = ifelse(clickthrough, position_clicked[2], NA),
            `result page IDs` = result_pids[1],
            `Query score (F=0.1)` = query_score(position_clicked, 0.1),
            `Query score (F=0.5)` = query_score(position_clicked, 0.5),
            `Query score (F=0.9)` = query_score(position_clicked, 0.9)) %>%
  arrange(ts) %>% ungroup() %>% as.data.table()

## feature matrix
if (file.exists(file.path(data_root, paste0("features-matrix_", dataset_to_use)))) {
  features_matrix <- readr::read_rds(file.path(data_root, paste0("features-matrix_", dataset_to_use)))
} else {
  # What we're interested in is turning a comma-separated list of features into a vector of 1s and 0s
  #   (or rather TRUEs and FALSEs) so that when we work with the queries' features, we would actually
  #   dealing with a boolean matrix.
  library(magrittr)
  library(progress)
  pb <- progress_bar$new(total = nrow(searches))
  features_matrix <- searches$features %>%
    # head(200) %>%
    strsplit(", ") %>%
    lapply(function(feats) {
      x <- rep(TRUE, length(feats))
      names(x) <- feats
      pb$tick()
      return(as.data.frame(t(x)))
    }) %>%
    do.call(dplyr::bind_rows, .) %>%
    lapply(function(column) {
      return(replace(column, is.na(column), FALSE))
    }) %>%
    dplyr::as_data_frame()
  rm(pb)
  # Quick check to see that everything went as it should have:
  if (sum(rowSums(features_matrix) != searches$n_feats) == 0) {
    readr::write_rds(features_matrix, file.path(data_root, paste0("features-matrix_", dataset_to_use)), "gz")
  } else {
    message("Row sums did not match number of features!")
  }
}


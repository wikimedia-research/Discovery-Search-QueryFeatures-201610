# data(list=c("ISO_639_2","ISO_639_3"), package = "ISOcodes")
# queries$lang_name <- ISO_639_2$Name[match(queries$lang_code, ISO_639_2$Alpha_2)]
# queries$lang_name[is.na(queries$lang_name)] <- ISO_639_3$Name[match(queries$lang_code[is.na(queries$lang_name)], ISO_639_3$Id)]
# inactive_lang <- names(table(queries$lang_name))[table(queries$lang_name) <= 10]
# queries <- queries[!is.na(queries$lang_name),,] %>% mutate(lang_name = ifelse(lang_name %in% inactive_lang, "Other", lang_name))
# rm(ISO_639_2, ISO_639_3)
## OR
queries <- queries[queries$ip!="164.132.183.164", , ]
lang_proj <- polloi::parse_wikiid(queries$wikiid)
inactive_lang <- names(table(lang_proj$language))[table(lang_proj$language) <= 50]
inactive_proj <- names(table(lang_proj$project))[table(lang_proj$project) <= 50]
queries <- cbind(queries, lang_proj)
queries <- queries[!is.na(queries$project), , ] %>% mutate(language = ifelse(language %in% inactive_lang|is.na(language), "Other", language), 
                                                           project = ifelse(project %in% inactive_proj, "Other", project)) %>% as.data.table()
rm(lang_proj,inactive_lang,inactive_proj)

if (file.exists(file.path(data_root, paste0("features-matrix_", dataset_to_use)))) {
  features_matrix <- readr::read_rds(file.path(data_root, paste0("features-matrix_", dataset_to_use)))
} else {
  # What we're interested in is turning a comma-separated list of features into a vector of 1s and 0s
  #   (or rather TRUEs and FALSEs) so that when we work with the queries' features, we would actually
  #   dealing with a boolean matrix.
  library(magrittr)
  library(progress)
  pb <- progress_bar$new(total = nrow(queries))
  features_matrix <- queries$features %>%
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
  if (sum(rowSums(features_matrix) != queries$n_feats) == 0) {
    readr::write_rds(features_matrix, file.path(data_root, paste0("features-matrix_", dataset_to_use)), "gz")
  } else {
    message("Row sums did not match number of features!")
  }
}

# dim(features_matrix) = 8391456 queries x 24 possible features

## Regions might be easier to work with than countries...
# data(list = c("UN_M.49_Countries", "UN_M.49_Regions"), package = "ISOcodes")
# 
# UN_M.49_Countries$Name[!(tolower(UN_M.49_Countries$Name) %in% tolower(unique(queries$country)))]
# queries$country[queries$country == "United States"] <- "United States of America"
# queries$country[queries$country == "United Kingdom"] <- "United Kingdom of Great Britain and Northern Ireland"
# 
# codes <- unlist(strsplit(keep_where(UN_M.49_Regions, Name == "Northern America")$Children, ", "))
# tolower(keep_where(UN_M.49_Countries, Code %in% codes)$Name) %in% tolower(unique(queries$country))
# 
# rm(UN_M.49_Countries, UN_M.49_Regions)

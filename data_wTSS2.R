## Event logging to Hive
x <- character()
for (day in 1:23) {
  for (hour in 0:23) {
    x <- c(x, sprintf("ALTER TABLE TestSearchSatisfaction2
  ADD PARTITION (year=2016,month=10,day=%0.0f,hour=%0.0f)
  LOCATION '/wmf/data/raw/eventlogging/eventlogging_TestSearchSatisfaction2/hourly/2016/10/%02.0f/%02.0f';", day, hour, day, hour))
  }
}
cat(paste0(x, collapse = '\n'))

## Remotely: cirrussearchrequestset
end_date <- as.Date("2016-10-23")
start_date <- as.Date("2016-10-1")
queries <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching query features from", as.character(date), "\n")
  query <- paste0(
                 "ADD JAR hdfs:///wmf/refinery/current/artifacts/refinery-hive.jar;
                  CREATE TEMPORARY FUNCTION array_sum AS 'org.wikimedia.analytics.refinery.hive.ArraySumUDF';
                  CREATE TEMPORARY FUNCTION is_spider as 'org.wikimedia.analytics.refinery.hive.IsSpiderUDF';
                  CREATE TEMPORARY FUNCTION ua_parser as 'org.wikimedia.analytics.refinery.hive.UAParserUDF';
                  CREATE TEMPORARY FUNCTION deconstruct AS 'org.wikimedia.analytics.refinery.hive.DeconstructSearchQueryUDF';
                  CREATE TEMPORARY FUNCTION geocode_country as 'org.wikimedia.analytics.refinery.hive.GeocodedCountryUDF';
                  CREATE TEMPORARY FUNCTION country_name as 'org.wikimedia.analytics.refinery.hive.CountryNameUDF';
                  SELECT
                  event_logs.cirrus_id,
                  event_logs.page_id AS page_id,
                  event_logs.event_id AS event_id,
                  search_results.page_id AS result_pids,
                  identity, ip, wikiid, 
                  country_code, country, 
                  query_type, zero_result, n_terms, 
                  n_chars, n_feats, features
                  FROM (
                  SELECT
                    get_json_object(json_string, '$.event.searchToken') AS cirrus_id,
                    get_json_object(json_string, '$.event.pageViewId') AS page_id,
                    get_json_object(json_string, '$.event.uniqueId') AS event_id
                  FROM chelsyx.TestSearchSatisfaction2 ",
                  wmf::date_clause(date)$date_clause, 
                  " AND get_json_object(json_string, '$.event.source') = 'fulltext'
                    AND get_json_object(json_string, '$.event.action') = 'searchResultPage'
                  ) AS event_logs
                  LEFT JOIN (
                  SELECT
                  id, identity, ip, wikiid, page_id, country_code, country_name(country_code) AS country, 
                  query_type, zero_result, n_terms, n_chars,
                  SIZE(SPLIT(features, ', ')) AS n_feats,
                  SUBSTR(features, 2, LENGTH(features)-2) AS features
                  FROM (
                  SELECT
                  id, identity, ip, wikiid, geocode_country(ip) AS country_code,
                  CASE
                  WHEN array_sum(requests.hitstotal, -1) = 0 THEN NULL
                  ELSE hits.pageid
                  END AS page_id,
                  CASE
                  WHEN ((ua_parser(useragent)['device_family'] = 'Spider') OR
                  is_spider(useragent) OR ip = '127.0.0.1')
                  THEN 'TRUE' ELSE 'FALSE'
                  END AS is_automata,
                  array_sum(requests.hitstotal, -1) = 0 AS zero_result,
                  requests[size(requests)-1].querytype AS query_type,
                  deconstruct(requests.query[SIZE(requests.query)-1]) AS features,
                  SIZE(SPLIT(requests.query[SIZE(requests.query)-1], ' +')) AS n_terms,
                  LENGTH(requests.query[SIZE(requests.query)-1]) AS n_chars
                  FROM wmf_raw.cirrussearchrequestset ",
                  wmf::date_clause(date)$date_clause, 
                  " AND source = 'web'
                  AND requests[size(requests)-1].querytype IN('full_text', 'comp_suggest')
                  ) AS deconstructed_queries
                  WHERE is_automata = 'FALSE') AS search_results
                  ON (event_logs.cirrus_id = search_results.id)
                  ;")
  results <- wmf::query_hive(query)
  results <- results[results$query_type != "", ]
  results$result_pids[results$result_pids == "NULL"] <- NA
  results$result_pids[results$result_pids == "[]"] <- NA
  results <- cbind(date = date, results, stringsAsFactors = FALSE)
  results$zero_result <- results$zero_result == "true"
  return(results)
  }))

library(data.table)
queries <- as.data.table(queries, key = c("date"))

readr::write_rds(queries, "~/cirrus_query_features/web_excl-known-automata_with_logs.rds", "gz")

## Locally:
dir.create("data")
system("scp -r stat2:/home/chelsyx/cirrus_query_features/web_excl-known-automata_with_logs.rds data/")


## Remotely: TestSearchSatisfaction2_15700292
events <- wmf::mysql_read("
  SELECT
    LEFT(`timestamp`, 8) AS date,
    `timestamp` AS ts,
    event_uniqueId AS event_id,
    event_mwSessionId AS session_id,
    event_searchSessionId AS search_id,
    event_pageViewId AS page_id,
    event_searchToken AS cirrus_id,
    event_query AS query,
    event_hitsReturned AS n_results_returned,
    event_msToDisplayResults AS load_time,
    CASE WHEN event_action = 'searchResultPage' THEN 'SERP' ELSE 'click' END AS action,
    event_position AS position_clicked
  FROM TestSearchSatisfaction2_15700292
  WHERE
    LEFT(`timestamp`, 8) >= '20161001' AND LEFT(`timestamp`, 8) <= '20161023'
    AND event_source = 'fulltext'
    AND (
      (event_action = 'searchResultPage' AND event_hitsReturned IS NOT NULL AND event_msToDisplayResults IS NOT NULL)
      OR
      (event_action = 'click' AND event_position IS NOT NULL AND event_position > -1)
    );", "log")
# Fix dates & times:
events$date <- lubridate::ymd(events$date)
events$ts <- lubridate::ymd_hms(events$ts)
# Remove duplicate events:
events <- events[order(events$event_id, events$ts), ]
events <- events[!duplicated(events$event_id, fromLast = FALSE), ]
# Sort:
events <- events[order(events$date, events$session_id, events$ts), ]
# Save:
save(events, file = "tss2_oct.RData", ascii = FALSE, compress = "gzip")

## LOCALLY
system("scp stat2:/home/chelsyx/tss2_oct.RData data/")
# Query Features and Search Performance

This codebase contains the code used to investigate the relationship between query features and search performance measures, as noted in [T147216](https://phabricator.wikimedia.org/T147216).

- **Data Retrieval:** `data_wTSS2.R`, `eventlog_to_hive.hql`
- **Data Refining:** `refine_wTSS2.R`
- **Exploratory Data Analysis:** `eda_wTSS2.R`, `eda2_wTSS2.R`
- **Random Forest:** `wTSS2_RF.R`, `randomforest.R`
- **GLM with Elasticnet Penalty:** `wTSS2_reg.R`
- **Model Agreement:** `map_imp_coef.R`

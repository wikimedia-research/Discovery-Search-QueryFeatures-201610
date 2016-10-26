ADD JAR file:///usr/lib/hive-hcatalog/share/hcatalog/hive-hcatalog-core.jar;

-- Make sure you don't create tables in the default Hive database.
USE chelsyx;

-- Create a table with a single string field
CREATE EXTERNAL TABLE `TestSearchSatisfaction2` (
  `json_string` string
)
PARTITIONED BY (
  year int,
  month int,
  day int,
  hour int
)
STORED AS INPUTFORMAT
  'org.apache.hadoop.mapred.SequenceFileInputFormat'
OUTPUTFORMAT
  'org.apache.hadoop.hive.ql.io.HiveIgnoreKeyTextOutputFormat'
LOCATION 
  '/wmf/data/raw/eventlogging/eventlogging_TestSearchSatisfaction2';

-- Add a partition
ALTER TABLE TestSearchSatisfaction2
ADD PARTITION (year=2016, month=10, day=17, hour=23)
LOCATION '/wmf/data/raw/eventlogging/eventlogging_TestSearchSatisfaction2/hourly/2016/10/17/23';

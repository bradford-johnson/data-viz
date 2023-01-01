# load packages
library(tidyverse)
library(rtweet)
library(DBI)
library(RPostgres)

# connect rtweet package to twitter dev account
auth_setup_default()
auth_has_default()

# scrape twitter and collect tweets
resolutions <- search_tweets("new years resolution", n = 2000, include_rts = FALSE, lang = "en")

traditions <- search_tweets("new years traditions", n = 2000, include_rts = FALSE, lang = "en")

# make dataframes concise
resolutions <- resolutions |>
  select(created_at, id, full_text)

traditions <- traditions |>
  select(created_at, id, full_text)

# connect to postgresql database
con <- dbConnect(RPostgres::Postgres(),dbname = 'postgres',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'vannah')

# append tweets to tables
dbWriteTable(con, "resolutions", resolutions, append = TRUE)

dbWriteTable(con, "traditions", traditions, append = TRUE)

# disconnect from database
dbDisconnect(con)
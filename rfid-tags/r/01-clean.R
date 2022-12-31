# load packages
library(tidyverse)

# load data and clean names
rfid_df <- read_csv("rfid-tag-data.csv") |>
  janitor::clean_names()

# remove unwanted columns
## removing percents fields to calculate new and uniform fields
rfid_df <- rfid_df |>
  select(-c(venue, percent_edit, percent_no_tag, suppressed_tags, percent_wagsak))

# remove na records
rfid_df <- rfid_df |>
  drop_na()

# change date from char to date data type
rfid_df$date <- lubridate::mdy(rfid_df$date)

# calculate new percent fields, in decimal form
rfid_df <- rfid_df |>
  mutate(percent_edit = round(edited / dispensed, 6),
         percent_tag_error = round(no_tag / dispensed, 6),
         percent_wagsak = round(wagsak_total / dispensed, 6))

# export new cleaned .csv
write_csv(rfid_df, "rfid-tag-data-cleaned.csv")
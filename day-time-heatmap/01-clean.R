# load packages
library(tidyverse)
library(lubridate)
# library(skimr)

# load data
cobra <- read_csv("COBRA-2021.csv") |>
  janitor::clean_names()

# remove unwanted columns
cobra <- cobra |>
  select(-c(rpt_date, occur_day_num, poss_date, poss_time, location))

# remove "-" sep in uc2_literal
cobra$uc2_literal <- gsub("-", " ", cobra$uc2_literal)

# change uc2_literal to title case
cobra$uc2_literal <- str_to_title(cobra$uc2_literal, locale = "en")

# change occur_date from char to date
cobra$occur_date <- mdy(cobra$occur_date)

# inspect for missing records
# cobra |>
#   skim()

# remove missing npu records
cobra <- cobra |>
  drop_na(npu)

# export new cleaned .csv
write_csv(cobra, "cobra-2021-cleaned.csv")
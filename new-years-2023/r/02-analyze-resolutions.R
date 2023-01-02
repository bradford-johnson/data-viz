# load packages
library(tidyverse)
library(DBI)
library(RPostgres)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(showtext)
library(htmltools)

# connect to database
con <- dbConnect(RPostgres::Postgres(),dbname = 'postgres',
                 host = 'localhost',
                 port = 5432,
                 user = 'postgres',
                 password = 'vannah') 

# make and execute SQL query
## resolutions
res <- dbSendQuery(con, "
SELECT DISTINCT(id), full_text
FROM resolutions;
                   ")

resolutions_df <- dbFetch(res)

# clear query and disconnect from database
dbClearResult(res)
dbDisconnect(con)

# word correlations | unnest words and remove stop words
tweet_words <- resolutions_df |>
  unnest_tokens(output = word, input = full_text) |>
  anti_join(stop_words, by = "word") |>
  filter(str_detect(word, "[:alpha:]")) |>
  distinct()

# remove words
word <- c("https", "t.co", "ttv", "newyearresolution2023", "happynewyear", "newyearwishes",
          "i'm", "it's", "i've", "newyearseve", "amp", "steam", "bundle", "play", "games",
          "sale", "aicc", "findom", "send", "newyear", "game", "im", "gonna", "i’m", "i’ve",
          "i’ll", "haven’t", "year’s", "you’re", "year's", "newyear2023", "gaming", "misamigosplaygames")

remove_words <- data.frame(word)

tweet_words <- tweet_words |>
  anti_join(remove_words, by = "word")

# get word counts
tweets_that_mention_word <- tweet_words |>
  count(word, name = "number_of_tweets") |> 
  filter(number_of_tweets >=8)

# get word correlations
tweet_correlations <- tweet_words |> 
  semi_join(tweets_that_mention_word, by = "word") |>
  pairwise_cor(item = word, feature = id) |>
  filter(correlation >= 0.16)

tweet_correlations <- tweet_correlations |>
  mutate(Correlation = correlation)

tweet_correlations <- tweet_correlations |>
  select(-correlation)

# load fonts 
font_add(family = "Roboto",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Roboto-Regular.ttf")
font_add(family = "RobotoB",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Roboto-Bold.ttf")
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

# create caption
caption = paste0("<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:white;'>.</span>",
                 "<span style='font-family:sans;'>bradfordjohnson</span>")

# create word correlation visual
res_corr <- graph_from_data_frame(d = tweet_correlations,
                                         vertices = tweets_that_mention_word |>
                                           semi_join(tweet_correlations, by = c("word" = "item1"))) |>
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = Correlation)) + 
  geom_node_point() +
  geom_node_text(aes(color = number_of_tweets, label = name), repel = TRUE, check_overlap = TRUE, size = 11) + 
  labs(title = "Twitter's 2023 New Year's Resolutions",
       caption = caption) +
  scale_colour_gradientn(colours=c("#3e3b92", "#f44369")) +
  theme_void() +
  theme(text = element_text(family = "Roboto"),
        plot.title = element_text(family = "RobotoB", hjust = .5, vjust = 3.5, size = 48),
        plot.margin = unit(c(8,12,8,8), "pt"),
        legend.position = "right",
        legend.box = "vertical",
        legend.margin = margin(),
        legend.text.align = .5,
        legend.title.align = .5,
        legend.title = element_text(family = "RobotoB", size = 32),
        legend.text = element_text(family = "Roboto", size = 28),
        plot.caption = ggtext::element_textbox_simple(color="#444444",
                                                      size = 30),
        panel.background = element_rect(fill = "#f3f3f3", color = NA),
        plot.background = element_rect(fill = "#f3f3f3")) +
  guides(color = guide_legend(title = "Number of Tweets"))

# view plot
res_corr

ggsave("new-years-resolutions-v3.png", width = 9, height = 9)

# load packages
library(tidyverse)
library(lubridate)
library(showtext)
library(htmltools)

# load fonts 
font_add(family = "Roboto",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Roboto-Regular.ttf")
font_add(family = "RobotoB",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Roboto-Bold.ttf")
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

# load data
cobra_df <- read_csv("cobra-2021-cleaned.csv")

# keep only records that occurred in 2021
cobra_df <- cobra_df |>
  mutate(year = as.character(year(occur_date))) |>
  filter(year == "2021") |>
  select(-year)

# prepare data for visual
cobra_df$occur_time <- hour(cobra_df$occur_time)

caption = paste0("<span style='font-family:sans;'>Source: Atlanta PD</span><br>",
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:sans;color:white;'>.</span>",
  "<span style='font-family:sans;'>bradfordjohnson</span>")

cobra_df$occur_day <- factor(cobra_df$occur_day, levels = 
                               c("Monday", "Tuesday", "Wednesday",
                                 "Thursday", "Friday", "Saturday", "Sunday"))

cobra_df |>
  ggplot(aes(x = occur_day, y = occur_time)) +
  geom_bin2d() +
  scale_fill_continuous(high = "#004c6d", low = "#e0edf8") +
  scale_y_reverse(expand = c(0,.25), breaks = round(seq(min(cobra_df$occur_time), 
                                     max(cobra_df$occur_time), by = 1),1)) +
  scale_x_discrete(position = "top") +
  labs(title = "Atlanta's Crime 2021: Day / Time Heatmap",
       y = "Hour (24H)",
       fill = "Counts",
       caption = caption) +
  theme_void() +
  theme(text = element_text(family = "Roboto"),
        axis.text = element_text(family = "Roboto"),
        axis.text.x = element_text(family = "Roboto"),
        axis.text.y = element_text(family = "Roboto", size = 10),
        axis.title.y = element_text(family = "RobotoB", size = 10, angle = 90,
                                    hjust = .5, vjust = 1),
        plot.margin = unit(c(8,8,4,4), "pt"),
        legend.title.align = 0.5,
        plot.title = element_text(family = "RobotoB", hjust = .5, vjust = 4),
        legend.title = element_text(family = "RobotoB", size = 11),
        legend.text = element_text(size = 11),
        plot.caption = ggtext::element_textbox_simple(color="#444444",
                                                      size = 11)) +
  guides(fill=guide_colorbar(ticks.colour = NA))

# ggsave("weekly-heatmap-v3.png", bg="white", units="px", width= 1000, height=898)

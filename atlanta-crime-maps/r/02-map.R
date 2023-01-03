# load packages
library(tidyverse)
library(sf)
library(showtext)
library(htmltools)

# load data
crime <- read_csv("cobra-2021-cleaned.csv")

atlanta <- read_sf("Official_NPU.geojson")

# wrangle crime data
## only occured in 2021
crime <- crime |>
  mutate(year = as.character(lubridate::year(occur_date))) |>
  filter(year == "2021") |>
  select(-year)
  
# burglary df
burg <- crime |>
  filter(uc2_literal == "Burglary") |>
  group_by(npu) |>
  summarise(n=n())

# join atlanta shapefile and burglary df
atlanta_burg <- atlanta |>
  left_join(burg, by = c("NAME" = "npu"))

# load fonts 
font_add(family = "Roboto",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Roboto-Regular.ttf")
font_add(family = "RobotoB",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Roboto-Bold.ttf")
font_add(family = "fb",
         regular = "C:/Users/Bradf/AppData/Local/Microsoft/Windows/Fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

caption = paste0("<span style='font-family:sans;'>Source: Atlanta PD</span>  <b>|</b>  ",
                 "<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:sans;color:black;'>.</span>",
                 "<span style='font-family:sans;'>bradfordjohnson</span>")

ggplot(data = atlanta_burg) +
  geom_sf(aes(fill = n), color = "black") +
  geom_sf_label(aes(label = NAME), size = 15, family = "RobotoB", color = "black", fill = "white", fun.geometry = sf::st_centroid, alpha = .8) +
  scale_fill_gradient(low = "#e2d4d3", high = "#9e1b30") +
  labs(title = "Atlanta's Burglaries in 2021",
       subtitle = "by NPU",
       caption = caption,
       fill = "Burglary Count") +
  theme_void() +
  theme(title = element_text(family = "RobotoB", colour = "white", size = 52),
        plot.margin = unit(c(.3,.3,.3,1), "cm"),
        legend.title.align = .5,
        legend.title = element_text(colour = "white", family = "RobotoB", size = 36),
        legend.text = element_text(colour = "white", family = "RobotoB", size = 32),
        legend.margin = margin(),
        panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black"),
        legend.position = c(.01,.75),
        plot.caption = ggtext::element_textbox_simple(color="#B1B1B1", size = 28)
          )
ggsave("atl-burg-2021.png", width = 9, height = 9)

# agg assault df
agg_a <- crime |>
  filter(uc2_literal == "Agg Assault") |>
  group_by(npu) |>
  summarise(n=n())

# join atlanta shapefile and agg assault df
atlanta_agg <- atlanta |>
  left_join(agg_a, by = c("NAME" = "npu"))

ggplot(data = atlanta_agg) +
  geom_sf(aes(fill = n), color = "black") +
  geom_sf_label(aes(label = NAME), size = 15, family = "RobotoB", color = "black", fill = "white", fun.geometry = sf::st_centroid, alpha = .8) +
  scale_fill_gradient(low = "#e2d4d3", high = "#9e1b30") +
  labs(title = "Atlanta's Aggravated Assault in 2021",
       subtitle = "by NPU",
       caption = caption,
       fill = "Agg Assault Count") +
  theme_void() +
  theme(title = element_text(family = "RobotoB", colour = "white", size = 52),
        plot.margin = unit(c(.3,.3,.3,1), "cm"),
        legend.title.align = .5,
        legend.title = element_text(colour = "white", family = "RobotoB", size = 36),
        legend.text = element_text(colour = "white", family = "RobotoB", size = 32),
        legend.margin = margin(),
        panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black"),
        legend.position = c(.01,.75),
        plot.caption = ggtext::element_textbox_simple(color="#B1B1B1", size = 28)
  )
ggsave("atl-agg-2021.png", width = 9, height = 9)


# auto_theft df
auto_t <- crime |>
  filter(uc2_literal == "Auto Theft") |>
  group_by(npu) |>
  summarise(n=n())

# join atlanta shapefile and auto theft df
atlanta_auto <- atlanta |>
  left_join(auto_t, by = c("NAME" = "npu"))

ggplot(data = atlanta_auto) +
  geom_sf(aes(fill = n), color = "black") +
  geom_sf_label(aes(label = NAME), size = 15, family = "RobotoB", color = "black", fill = "white", fun.geometry = sf::st_centroid, alpha = .8) +
  scale_fill_gradient(low = "#e2d4d3", high = "#9e1b30") +
  labs(title = "Atlanta's Auto Theft in 2021",
       subtitle = "by NPU",
       caption = caption,
       fill = "Auto Theft Count") +
  theme_void() +
  theme(title = element_text(family = "RobotoB", colour = "white", size = 52),
        plot.margin = unit(c(.3,.3,.3,1), "cm"),
        legend.title.align = .5,
        legend.title = element_text(colour = "white", family = "RobotoB", size = 36),
        legend.text = element_text(colour = "white", family = "RobotoB", size = 32),
        legend.margin = margin(),
        panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black"),
        legend.position = c(.01,.75),
        plot.caption = ggtext::element_textbox_simple(color="#B1B1B1", size = 28)
  )
ggsave("atl-auto-2021.png", width = 9, height = 9)

# homicide df
homicide <- crime |>
  filter(uc2_literal == "Homicide") |>
  group_by(npu) |>
  summarise(n=n())

# join atlanta shapefile and auto theft df
atlanta_homicide <- atlanta |>
  left_join(homicide, by = c("NAME" = "npu"))

ggplot(data = atlanta_homicide) +
  geom_sf(aes(fill = n), color = "black") +
  geom_sf_label(aes(label = NAME), size = 15, family = "RobotoB", color = "black", fill = "white", fun.geometry = sf::st_centroid, alpha = .8) +
  scale_fill_gradient(low = "#e2d4d3", high = "#9e1b30") +
  labs(title = "Atlanta's Homicide in 2021",
       subtitle = "by NPU",
       caption = caption,
       fill = "Homicide Count") +
  theme_void() +
  theme(title = element_text(family = "RobotoB", colour = "white", size = 52),
        plot.margin = unit(c(.3,.3,.3,1), "cm"),
        legend.title.align = .5,
        legend.title = element_text(colour = "white", family = "RobotoB", size = 36),
        legend.text = element_text(colour = "white", family = "RobotoB", size = 32),
        legend.margin = margin(),
        panel.background = element_rect(fill = "black", color = NA),
        plot.background = element_rect(fill = "black"),
        legend.position = c(.01,.75),
        plot.caption = ggtext::element_textbox_simple(color="#B1B1B1", size = 28)
  )
ggsave("atl-homicide-2021.png", width = 9, height = 9)

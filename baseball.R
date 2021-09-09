library(tidyverse)
library(lubridate)
library(scales)
library(httr)
library(rio)
library(reticulate)

# get data ----
fivethirtyeight_data_url <- "https://projects.fivethirtyeight.com/mlb-api/mlb_elo_latest.csv"
fivethirtyeight_data <- rio::import(fivethirtyeight_data_url, format = "csv")
fivethirtyeight_data_chw <- fivethirtyeight_data %>%
  select(date, season, team1, team2, score1, score2) %>%
  arrange(date) %>%
  filter(team1 == "CHW" | team2 == "CHW") %>%
  mutate(win_status = if_else((team2 == "CHW"),if_else((score2 > score1),"W","L"),if_else((score1 > score2),"W","L"))) %>%
  mutate(win_value = if_else((win_status == "W"),1,-1,)) %>%
  mutate(game_n = row_number()) %>%
  mutate(    
    y = case_when(
      win_status == "L" ~ -1.33,
      win_status == "W" ~ 1.33,
      TRUE ~ 0.33),
    yend = case_when(
      win_status == "L" ~ 0.33,
      TRUE ~ -0.33))
    
ggplot(data = fivethirtyeight_data_chw) +
  # Win/draw/loss lines
  geom_segment(aes(x = game_n, xend = game_n, y = y, yend = yend, color = win_status), lineend = "round", size = 0.6) +
  scale_color_manual(values = c("darkred", "darkblue"),
                     #labels = c("Loss", "Draw", "Win"), 
                     guide = NULL) +
  xlab(NULL) +
  theme_minimal() +
  labs(title = "Chicago White Sox wins and losses") +
  theme(
    plot.background = element_rect(fill = "grey97", color = "white"),
    legend.position = c(0.45, 1.08),
    legend.direction = "horizontal",
    legend.key.size = unit(0.35, "line"),
    legend.text = element_text(color = "grey10", size = 8),
    legend.title = element_blank(),
    plot.margin = margin(20, 40, 10, 40),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "grey30", size = 7),
    axis.title.x = element_text(color = "grey10", size = 7, margin = margin(10, 0, 0, 0)),
    axis.ticks.x = element_line(color = "grey60", size = 0.25),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 8, 
                                 margin = margin(0, 0, 40, 0)),
    plot.caption = element_text(size = 5.5, color = "grey40", hjust = 0.5,
                                margin = margin(25, 0, 0, 0)),
    strip.text = element_blank()
  )

ggsave("plots/mlb_wins_losses.png", 
       width = 15, height = 1, dpi = 320)

ggplot(fivethirtyeight_data_chw, 
       aes(x = date,
           y = win_value)) + 
  geom_point(aes(color = win_value>0),
             shape = 15) +
  scale_color_manual(values = c("darkred", "darkblue"),
                     #labels = c("Loss", "Draw", "Win"), 
                     guide = NULL) +
  xlab(NULL) +
  theme_minimal() +
  labs(title = "Chicago White Sox wins and losses") +
  theme(
    plot.background = element_rect(fill = "grey97", color = "white"),
    legend.position = c(0.45, 1.08),
    legend.direction = "horizontal",
    legend.key.size = unit(0.35, "line"),
    legend.text = element_text(color = "grey10", size = 8),
    legend.title = element_blank(),
    plot.margin = margin(20, 40, 10, 40),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "grey30", size = 7),
    axis.title.x = element_text(color = "grey10", size = 7, margin = margin(10, 0, 0, 0)),
    axis.ticks.x = element_line(color = "grey60", size = 0.25),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 8, 
                                 margin = margin(0, 0, 40, 0)),
    plot.caption = element_text(size = 5.5, color = "grey40", hjust = 0.5,
                                margin = margin(25, 0, 0, 0)),
    strip.text = element_blank()
  )
#ggsave("plots/mlb_wins_losses.png", 
 #      width = 15, height = 1, dpi = 320)



web_text <- paste(
  "---
layout: page
title: Baseball
permalink: /charts/baseball/
---

## Chicago White Sox

![Season]({{ site.baseurl }}/plots/mlb_wins_losses.png)

",
sep = ""
)
write_lines(web_text,"charts/baseball.md")


  

#count(fivethirtyeight_data_chw, win)

# python stuff ----
# mlb_host_url <- "http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&teamId=145"
# path <- "schedule/games/"
# 
# #http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&date=03/06/2019
# games <- GET(url = mlb_host_url)
#              #path = path,
# 
# game <- content(games,"parsed")
# 
# game$teams[[145]]
# 
# 
# game[["dates"]][[1]][["games"]][[1]][["officialDate"]]
# 
# mlb_team_stats(2021, "pitching", "regular")
# 
# 
# standings <- get_reference_team_standings(2021)
# 
# 
# py_install("MLB-StatsAPI", pip = TRUE)
# py_run_string("import statsapi")
# py_run_file("mlb.py")
# 
# 
# wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
# 
# # Assign line y and yend depending on win_status
# winloss <- wwc_outcomes %>%
#   group_by(team) %>%
#   mutate(
#     game_n = row_number(),
#     y = case_when(
#       win_status == "Lost" ~ -1.33,
#       win_status == "Won" ~ 1.33,
#       TRUE ~ 0.33),
#     yend = case_when(
#       win_status == "Lost" ~ 0.33,
#       TRUE ~ -0.33)
#   )
# 

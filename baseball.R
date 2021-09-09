library(tidyverse)
library(lubridate)
library(scales)
library(httr)
library(rio)

# get data ----
fivethirtyeight_data_url <- "https://projects.fivethirtyeight.com/mlb-api/mlb_elo_latest.csv"
fivethirtyeight_data <- rio::import(fivethirtyeight_data_url, format = "csv") %>%
  arrange(date) 
get_team_records <- function(abbreviation) {
  records <- fivethirtyeight_data %>%
    select(date, season, team1, team2, score1, score2) %>%
    filter(team1 == abbreviation | team2 == abbreviation) %>%
    mutate(result = if_else((team2 == abbreviation),
                            if_else((score2 > score1),"W","L"),
                            if_else((score1 > score2),"W","L"))) %>%
    mutate(game_n = row_number()) %>%
    select(date, game_n, result) %>%
    mutate(win = if_else(result == "W",1,0)) %>%
    mutate(loss = if_else(result == "L",1,0)) %>%
    mutate(wins = cumsum(win)) %>%
    mutate(losses = cumsum(loss)) %>%
    mutate(win_pct = wins/row_number()) %>%
    mutate(net_wins = wins-losses) %>%
    mutate(team = abbreviation)
}

chw <- get_team_records("CHW")
cle <- get_team_records("CLE")
det <- get_team_records("DET")
kcr <- get_team_records("KCR")
min <- get_team_records("MIN")

al_central <- full_join(chw,cle) %>%
  full_join(det) %>%
  full_join(kcr) %>%
  full_join(min)


ggplot(al_central, aes(x = game_n,
                       y = net_wins,
                       color= team)) +
  geom_step(direction = "vh") +
  scale_y_continuous(position = "right") +
  scale_color_manual(values = c("#27251F","#E31937","#0C2340","#BD9B60","#002B5C")) +
  ylab(NULL) +
  xlab(NULL) +
  theme_minimal() +
  labs(title = "Games Above .500",
       caption = "Source: FiveThirtyEight") +
  theme(
    plot.background = element_rect(fill = "grey99", color = "white"),
    #plot.margin = margin(20, 40, 10, 40),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(colour = "grey91"),
    axis.text.y = element_text(color = "grey30", size = 7),
    axis.title.y = element_text(color = "grey10", size = 7, margin = margin(10, 0, 0, 0)),
    axis.ticks.y = element_line(color = "grey60", size = 0.25),
    plot.title = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 8, 
                                 margin = margin(0, 0, 40, 0)),
    plot.caption = element_text(size = 5.5, color = "grey40", #hjust = 0.5,
                                margin = margin(25, 0, 0, 0))
  )

ggsave("plots/al_central_wins_losses.png", 
       width = 8, height = 8*(628/1200), dpi = 320)

# web text ----
web_text <- paste(
  "---
layout: page
title: Baseball
permalink: /charts/baseball/
---

## AL Central

![CHW]({{ site.baseurl }}/plots/al_central_wins_losses.png)

Data updated hourly from [FiveThirtyEight](https://github.com/fivethirtyeight/data/tree/master/mlb-elo).

",
sep = ""
)
write_lines(web_text,"charts/baseball.md")


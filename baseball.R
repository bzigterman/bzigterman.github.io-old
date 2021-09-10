library(tidyverse)
library(lubridate)
library(scales)
library(httr)
library(rio)
library(gt)

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
    mutate(game_counter = if_else(result == "W",1,if_else(result == "L",1,NULL))) %>%
    mutate(wins = cumsum(win)) %>%
    mutate(losses = cumsum(loss)) %>%
    mutate(win_pct = wins/row_number()) %>%
    mutate(net_wins = wins-losses) %>%
    mutate(team = abbreviation) %>%
    mutate(games_played = cumsum(game_counter)) %>%
    mutate(games_remaining = 162-games_played) %>%
    mutate(team_label = if_else(games_played == max(na.omit(games_played)),team,NULL))  %>%
    mutate(result_arrow = if_else(result == "W","▲",
                                  if_else(result == "L","▼",""))) %>%
    mutate(last_ten = paste(lag(result_arrow,9),
                            lag(result_arrow,8),
                            lag(result_arrow,7),
                            lag(result_arrow,6),
                            lag(result_arrow,5),
                            lag(result_arrow,4),
                            lag(result_arrow,3),
                            lag(result_arrow,2),
                            lag(result_arrow),
                            result_arrow,
                            sep = "")) 
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

# standings ----
standings <- al_central %>%
  filter(!is.na(team_label)) %>%
  select(team_label, wins, losses, win_pct, games_remaining, last_ten)

standings_table <- standings %>%
  arrange(desc(win_pct)) %>%
  gt() %>%
  # tab_spanner(
  #   label = "Games",
  #   columns = c(games_played,games_remaining)
  # ) %>%
  fmt_number(
    columns = win_pct,
    decimals = 3
  ) %>%
  cols_label(
    team_label = md("**Team**"),
    wins = md("**W**"),
    losses = md("**L**"),
    win_pct = md("**Pct**"),
    games_remaining = md("**Left**"),
    last_ten = md("**Last 10**")
  ) %>%
  opt_table_font(font = c("menlo","monospace","verdana", "helvetica", "arial", "sans-serif")) %>%
  tab_options(
    table.width = pct(100),
    table.font.size = px(12)
  )  %>%
  opt_table_lines(extent = "none")

standings_table

standings_table_html <- as_raw_html(standings_table)


# plot ----
ggplot(al_central, aes(x = game_n,
                       y = net_wins,
                       color= team,
                       label = team_label)) +
  geom_hline(yintercept = 0,
             color = "grey10",
             size = .2) +
  geom_step(direction = "vh") +
  geom_text(aes(x = game_n + 5)) +
  scale_x_continuous(breaks = c(0,40, 81,121, 162)) +
  scale_y_continuous(position = "right") +
  scale_color_manual(values = c("#27251F","#E31937","#0C2340","#BD9B60","#002B5C"),
                     guide = NULL) +
  coord_cartesian(xlim = c(0,162)) +
  theme_minimal() +
  labs(title = "Games Above .500",
       caption = "Source: FiveThirtyEight",
       x = NULL,
       y = NULL) +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    #plot.margin = margin(20, 40, 10, 40),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    axis.ticks.x = element_line(color = "grey60", size = 0.25),
    panel.grid.major.y = element_line(colour = "grey93"),
    #axis.text.y = element_text(color = "grey30", size = 7),
    #axis.title.x = element_text(color = "grey10", size = 7),
    axis.ticks.y = element_line(color = "grey60"),
    plot.caption = element_text(color = "grey40")
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

",standings_table_html,"

![CHW]({{ site.baseurl }}/plots/al_central_wins_losses.png)

Data updated hourly from [FiveThirtyEight](https://github.com/fivethirtyeight/data/tree/master/mlb-elo).

",
sep = ""
)
write_lines(web_text,"charts/baseball.md")


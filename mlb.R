library(tidyverse)
library(lubridate)
library(scales)
library(httr)
library(rio)
library(reticulate)

# get data ----
fivethirtyeight_data_url <- "https://projects.fivethirtyeight.com/mlb-api/mlb_elo_latest.csv"
fivethirtyeight_data <- rio::import(fivethirtyeight_data_url, format = "csv")



# python stuff ----
mlb_host_url <- "http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&teamId=145"
path <- "schedule/games/"

#http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&date=03/06/2019
games <- GET(url = mlb_host_url)
             #path = path,

game <- content(games,"parsed")

game$teams[[145]]


game[["dates"]][[1]][["games"]][[1]][["officialDate"]]

mlb_team_stats(2021, "pitching", "regular")


standings <- get_reference_team_standings(2021)


py_install("MLB-StatsAPI", pip = TRUE)
py_run_string("import statsapi")
py_run_file("mlb.py")

library(tidyverse)
library(lubridate)
library(scales)
library(httr)
library(rio)

# get data
mlb_host_url <- "http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&teamId=145"
path <- "schedule/games/"

#http://statsapi.mlb.com/api/v1/schedule/games/?sportId=1&date=03/06/2019
games <- GET(url = mlb_host_url)
             #path = path,

game <- content(games,"parsed")

game$teams[[145]]


game[["dates"]][[1]][["games"]][[1]][["officialDate"]]

library(tidyverse)
library(Quandl)
library(lubridate)

twenty_years_ago <- ymd((today() - years(20)))

consumer_sentiment <- Quandl("UMICH/SOC1") 
recent_consumer_sentiment <- consumer_sentiment %>%
  filter(Date > twenty_years_ago) %>%
  mutate(short_date = paste(month(Date, label = TRUE, abbr = FALSE),
                            mday(Date))) 

ggplot(recent_consumer_sentiment, aes(x = as.Date(Date),
                 y = Index)) +
  geom_line() +
  labs(title = "University of Michigan Consumer Survey, Index of Consumer Sentiment",
       caption = paste("Source: Quandl. Data updated",
                       head(recent_consumer_sentiment$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     limits = c(0,max(recent_consumer_sentiment$Index)*1.05)) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("plots/consumer_sentiment.png", width = 8, height = 8*(628/1200), dpi = 320)

web_text <- paste(
  "---
layout: page
title: Economic Indicators
permalink: /economy/
---

Here are some economic indicators I like to keep track of:

University of Michigan Consumer Survey, Index of Consumer Sentiment

![Consumer Sentiment](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/consumer_sentiment.png)
",
sep = ""
)
write_lines(web_text,"economy.md")


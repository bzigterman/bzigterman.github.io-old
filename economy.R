library(tidyverse)
library(lubridate)
library(scales)
library(fredr)

fredr_set_key(Sys.getenv("FRED_API_KEY"))

twenty_years_ago <- ymd((today() - years(20)))

# unemployment rate ----
data <- fredr(series_id = "UNRATE")
recent_data <- data %>%
  filter(date > twenty_years_ago) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) 

ggplot(recent_data, aes(x = date,
                        y = value/100)) +
  geom_line() +
  labs(title = "Unemployment Rate",
       caption = paste("Source: FRED. Data updated",
                       tail(recent_data$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     labels = label_percent(),
                     limits = c(0,max(recent_data$value/100)*1.05)) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("plots/unemployment_rate.png", width = 8, height = 8*(628/1200), dpi = 320)

# employment -----
data <- fredr(series_id = "PAYEMS")
recent_data <- data %>%
  filter(date > twenty_years_ago) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) %>%
  mutate(change = value - lag(value))

ggplot(recent_data, aes(x = date,
                        y = value/1000)) +
  geom_line() +
  labs(title = "Total Nonfarm Payroll",
       caption = paste("Source: FRED. Data updated",
                       tail(recent_data$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     labels = label_comma(suffix = "M"),
                     limits = c(0,max(recent_data$value/1000)*1.05)) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("plots/employment.png", width = 8, height = 8*(628/1200), dpi = 320)

## employment change ----
ggplot(recent_data, aes(x = date,
                        y = change*1000)) +
  geom_col() +
  labs(title = "Change in Total Nonfarm Payroll",
       caption = paste("Source: FRED. Data updated",
                       tail(recent_data$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     labels = label_comma()) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("plots/employment_change.png", width = 8, height = 8*(628/1200), dpi = 320)

# real GPD ----

data <-fredr(series_id = "GDPC1")
recent_data <- data %>%
  filter(date > twenty_years_ago) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) %>%
  mutate(scaled_value = value/1000) 

ggplot(recent_data, aes(x = as.Date(date),
                        y = scaled_value)) +
  geom_line() +
  labs(title = "Real GDP",
       caption = paste("Source: FRED. Data updated",
                       tail(recent_data$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     labels = label_dollar(suffix = "T"),
                     limits = c(0,max(recent_data$scaled_value)*1.05)) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("plots/real_gdp.png", width = 8, height = 8*(628/1200), dpi = 320)

## real gdp growth ----
data <- fredr(series_id = "A191RL1Q225SBEA")
recent_data <- data %>%
  filter(date > twenty_years_ago) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) 

ggplot(recent_data, aes(x = as.Date(date),
                        y = value/100)) +
  geom_col() +
  labs(title = "Real GDP Growth",
       caption = paste("Source: FRED. Data updated",
                       tail(recent_data$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     labels = label_percent()) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("plots/real_gdp_growth.png", width = 8, height = 8*(628/1200), dpi = 320)

# consumer sentiment ----
consumer_sentiment <- fredr(series_id = "UMCSENT")
recent_consumer_sentiment <- consumer_sentiment %>%
  filter(date > twenty_years_ago) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) 

ggplot(recent_consumer_sentiment, aes(x = date,
                                      y = value)) +
  geom_line() +
  labs(title = "University of Michigan Consumer Survey, Index of Consumer Sentiment",
       caption = paste("Source: FRED. Data updated",
                       tail(recent_consumer_sentiment$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     limits = c(0,max(recent_consumer_sentiment$value)*1.05)) +
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
permalink: /charts/economy/
---

## United States

![Unemployment Rate](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/unemployment_rate.png)

![Total Nonfarm Payroll](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/employment.png)

![Total Nonfarm Payroll Change](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/employment_change.png)

![Real GDP](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/real_gdp.png)

![Real GDP Growth](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/real_gdp_growth.png)

![Consumer Sentiment](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/consumer_sentiment.png)
",
sep = ""
)
write_lines(web_text,"charts/economy.md")


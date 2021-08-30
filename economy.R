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
                        y = change/1000)) +
  geom_col() +
  labs(title = "Change in Total Nonfarm Payroll",
       caption = paste("Source: FRED. Data updated",
                       tail(recent_data$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     labels = label_comma(suffix = "M")) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("plots/employment_change.png", width = 8, height = 8*(628/1200), dpi = 320)

# median household income ----
data <-fredr(series_id = "MEHOINUSA672N")
recent_data <- data %>%
  filter(date > twenty_years_ago) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) 

ggplot(recent_data, aes(x = as.Date(date),
                        y = value)) +
  geom_line() +
  labs(title = "Real Median Household Income",
       caption = paste("Source: FRED. Data updated",
                       tail(recent_data$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     labels = label_dollar(),
                     limits = c(0,max(recent_data$value)*1.05)) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("plots/real_median_income.png", width = 8, height = 8*(628/1200), dpi = 320)

# real GDP ----
data <-fredr(series_id = "GDPC1")
recent_data <- data %>%
  filter(date > twenty_years_ago) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) 

ggplot(recent_data, aes(x = as.Date(date),
                        y = value/1000)) +
  geom_line() +
  labs(title = "Real GDP",
       caption = paste("Source: FRED. Data updated",
                       tail(recent_data$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     labels = label_dollar(suffix = "T"),
                     limits = c(0,max(recent_data$value/1000)*1.05)) +
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

# retail sales and durable goods ----
retail_sales <- fredr(series_id = "RSXFS")
durable_goods <- fredr(series_id = "DGORDER")
data <- full_join(retail_sales, durable_goods)
recent_data <- data %>%
  filter(date > twenty_years_ago) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) %>%
  mutate(names = recode(series_id,
                        "DGORDER" = "Durable Goods Orders",
                        "RSXFS" = "Retail Sales"))

ggplot(recent_data, aes(x = date,
                        y = value/1000)) +
  geom_line() +
  facet_wrap(~ names, ncol = 1,  scales = "free_y") +
  labs(caption = paste("Source: FRED. Data updated",
                       tail(recent_data$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     labels = label_dollar(suffix = "B")) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("plots/retail_sales_durable_goods.png", width = 8, height = 6, dpi = 320)

# gini index ----

data <- fredr(series_id = "SIPOVGINIUSA")
recent_data <- data %>%
  filter(date > twenty_years_ago) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) 

ggplot(recent_data, aes(x = date,
                        y = value)) +
  geom_line() +
  labs(title = "Gini Index of Inequality",
       subtitle = "0 represents perfect equality; 100 represents perfect inequality",
       caption = paste("Source: University of Michigan Consumer Survey and FRED. Data updated",
                       tail(recent_data$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     limits = c(0,max(recent_data$value)*1.05)) +
  theme(axis.text.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour = "grey93"),
        strip.text = element_text(size = 11),
        strip.background = element_blank(),
        plot.caption = element_text(colour = "grey40"))

ggsave("plots/gini_index.png", width = 8, height = 8*(628/1200), dpi = 320)

# consumer sentiment ----
data <- fredr(series_id = "UMCSENT")
recent_data <- data %>%
  filter(date > twenty_years_ago) %>%
  mutate(short_date = paste(month(date, label = TRUE, abbr = FALSE),
                            mday(date))) 

ggplot(recent_data, aes(x = date,
                                      y = value)) +
  geom_line() +
  labs(title = "Consumer Sentiment Index",
       caption = paste("Source: University of Michigan Consumer Survey and FRED. Data updated",
                       tail(recent_data$short_date,1))) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(expand = expansion(mult = c(0, .01))) +
  scale_y_continuous(position = "right",
                     limits = c(0,max(recent_data$value)*1.05)) +
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

![Real Median Income](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/real_median_income.png)

![Real GDP](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/real_gdp.png)

![Real GDP Growth](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/real_gdp_growth.png)

![Retail Sales and Durable Goods Orders](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/retail_sales_durable_goods.png)

![Gini Index](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/gini_index.png)

![Consumer Sentiment](https://raw.githubusercontent.com/bzigterman/bzigterman.github.io/master/plots/consumer_sentiment.png)
",
sep = ""
)
write_lines(web_text,"charts/economy.md")


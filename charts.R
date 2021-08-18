library(httr)
library(rio)
library(tidyverse)
library(lubridate)
library(zoo)

# make variables ----
## Champaign County ----
### get data ----
idph_cases_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Champaign",
                                    format = "json") 
idph_cases_champaign <- idph_cases_champaign$values %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(monthlydead = rollmean(new_deaths, k = 31, 
                                fill = NA, align = "right")*31)  %>%
  mutate(Date = ymd_hms(reportDate)) 

idph_vax_champaign <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Champaign",
                                  format = "csv") %>%
  mutate(Date = mdy_hms(Report_Date)) 

### variables ----
champaign_dead_last_month <- format(tail(idph_cases_champaign$monthlydead,1),big.mark=",")
champaign_avg_new_cases <- format(round(tail(idph_cases_champaign$avg_new_cases,1)),big.mark=",")
champaign_pct_fully_vaccinated <- round(100*tail(idph_vax_champaign$PctVaccinatedPopulation,1), digits = 1)
champaign_avg_new_vaccine_doses <- format(tail(idph_vax_champaign$AdministeredCountRollAvg,1),big.mark=",")
champaign_weekday <- wday(tail(idph_cases_champaign$Date,1), label = TRUE, abbr = FALSE)
champaign_month_ago_deaths <- format(tail(lag(idph_cases_champaign$monthlydead, 31),1),big.mark=",")
champaign_month_ago_cases <- format(round(tail(lag(idph_cases_champaign$avg_new_cases, 31),1)),big.mark=",")
champaign_month_ago_vaccinated <- round(100*tail(lag(idph_vax_champaign$PctVaccinatedPopulation,31),1), digits = 1)
champaign_month_ago_new_doses <- format(tail(lag(idph_vax_champaign$AdministeredCountRollAvg,31),1),big.mark=",")

### text ----

champaign_county_text <- paste(
  "As of ",champaign_weekday," in Champaign County (vs. a month ago):
  
  ",
  "- Average new cases: ",champaign_avg_new_cases," (vs. ",champaign_month_ago_cases,")
  ",
  "- Deaths in the past month: ",champaign_dead_last_month," (vs. ",champaign_month_ago_deaths,")
  ",
  "- Percent of Champaign County fully vaccinated: ",champaign_pct_fully_vaccinated,"% (vs. ",champaign_month_ago_vaccinated,"%)
  ",
  "- Average new vaccine doses: ",champaign_avg_new_vaccine_doses," (vs. ",champaign_month_ago_new_doses,")",
  "
",
sep = ""
)


## Illinois
## Champaign County ----
### get data ----
idph_cases_il <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVID/GetCountyHistorical?countyName=Illinois",
                                    format = "json") 
idph_cases_il <- idph_cases_il$values %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases)) %>%
  mutate(new_deaths = deaths - lag(deaths)) %>%
  mutate(avg_new_cases = rollmean(new_cases, k = 7, 
                                  fill = NA, align = "right")) %>%
  mutate(avg_new_deaths = rollmean(new_deaths, k = 7, 
                                fill = NA, align = "right"))  %>%
  mutate(Date = ymd_hms(reportDate)) 

idph_vax_il <- rio::import("https://idph.illinois.gov/DPHPublicInformation/api/COVIDExport/GetVaccineAdministration?format=csv&countyName=Illinois",
                                  format = "csv") %>%
  mutate(Date = mdy_hms(Report_Date)) 

### variables ----
il_avg_new_deaths <- format(round(tail(idph_cases_il$avg_new_deaths,1)),big.mark=",")
il_avg_new_cases <- format(round(tail(idph_cases_il$avg_new_cases,1)),big.mark=",")
il_pct_fully_vaccinated <- round(100*tail(idph_vax_il$PctVaccinatedPopulation,1), digits = 1)
il_avg_new_vaccine_doses <- format(tail(idph_vax_il$AdministeredCountRollAvg,1),big.mark=",")
il_weekday <- wday(tail(idph_cases_il$Date,1), label = TRUE, abbr = FALSE)
il_month_ago_avg_new_deaths <- format(round(tail(lag(idph_cases_il$avg_new_deaths, 31),1)),big.mark=",")
il_month_ago_cases <- format(round(tail(lag(idph_cases_il$avg_new_cases, 31),1)),big.mark=",")
il_month_ago_vaccinated <- round(100*tail(lag(idph_vax_il$PctVaccinatedPopulation,31),1), digits = 1)
il_month_ago_new_doses <- format(tail(lag(idph_vax_il$AdministeredCountRollAvg,31),1),big.mark=",")

### text ----

il_text <- paste(
  "As of ",il_weekday," in Illinois (vs. a month ago):
  
  ",
  "- Average new cases: ",il_avg_new_cases," (vs. ",il_month_ago_cases,")
  ",
  "- Average new deaths: ",il_avg_new_deaths," (vs. ",il_month_ago_avg_new_deaths,")
  ",
  "- Percent of Illinois fully vaccinated: ",il_pct_fully_vaccinated,"% (vs. ",il_month_ago_vaccinated,"%)
  ",
  "- Average new vaccine doses: ",il_avg_new_vaccine_doses," (vs. ",il_month_ago_new_doses,")",
  "
",
sep = ""
)





# make web text ----

web_text <- paste(
  "---
layout: page
title: Charts
permalink: /charts/
---

During the COVID-19 pandemic, I've been making charts with data from the [Champaign-Urbana Public Health District](https://www.c-uphd.org/champaign-urbana-illinois-coronavirus-information.html), the [University of Illinois](https://go.illinois.edu/COVIDTestingData), the [Illinois Department of Public Health](http://www.dph.illinois.gov/covid19), the [CDC](https://covid.cdc.gov/covid-data-tracker/), [Our World in Data](https://github.com/owid/covid-19-data/tree/master/public/data) and the [COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University](https://github.com/CSSEGISandData/COVID-19).

## Champaign County

",champaign_county_text,
"
![Champaign County Metrics](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/Champaign_facet.png)

Charts for Champaign County are posted weekdays on Twitter [@ChampaignCovid](https://twitter.com/ChampaignCovid).

## Illinois

",il_text,
"
![Illinois Metrics](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/IL_facet.png)

![Illinois CDC_vax_combined map](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/IL_vax_combined.png)

![IL CDC_cases_transmission_IL map](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/IL_cases_transmission.png)

Community transmission levels are calculated by the CDC based on new cases per capita in the past week and test positivity.

## United States

![USA Metrics](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/US_facet.png)

![USA fully vaccinated map](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/usa_vax_total.png)

![USA transmission levels map](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/usa_transmission.png)

## World

![World Metrics](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/world_facet.png)

## Case Acceleration

![Case Acceleration](https://raw.githubusercontent.com/bzigterman/CUcovid/main/gh_action/new_cases_change_facet.png)

This chart measures how quickly the average number of new cases is increasing, or roughly, the slope of the new-cases charts above. If the case acceleration is positive, then the average number of new cases is increasing. If it is negative, then the average number of new cases is decreasing. 

",
sep = ""
)


if (champaign_avg_new_cases >= 0 && 
    champaign_dead_last_month >= 0 && 
    champaign_pct_fully_vaccinated >= 0 &&
    champaign_pct_fully_vaccinated <= 100 &&
    champaign_avg_new_vaccine_doses >= 0 &&
    champaign_month_ago_cases >= 0 && 
    champaign_month_ago_deaths >= 0 && 
    champaign_month_ago_vaccinated >= 0 &&
    champaign_month_ago_vaccinated <= 100 &&
    champaign_month_ago_new_doses >= 0
) {
write_lines(web_text,"charts.md")
}
## R ----

setwd("/Users/graham/Dropbox/01_gitR/our_world_in_data")
## load packages ----
library(tidyverse)
library(lubridate)
library(qcc)

## import data ----

owid  <-  as_tibble(read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv"))

#add log2() columns
owid <- mutate(owid, log2tdpm = log2(total_deaths_per_million), log2tcpm = log2(total_cases_per_million))

## Reformat data to get country sets ----

#iso_code filters as vectors

eu <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", 
        "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", 
        "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")


eu_gbr <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", 
            "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", 
            "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
gbr_fra_deu_swe <- c("GBR", "FRA", "DEU", "SWE")

americas <- c("ARG", "BRA", "CAN","CHL", "USA", "MEX")

badperformers <- c("ITA", "ESP", "CHL", "USA", "GBR")

world <- c("GBR", "FRA", "USA", "ITA", "CHN")


## filtering by country ----

owid_gbr <- filter(owid, iso_code == "GBR")

owid_eu <- filter(owid, iso_code %in% eu)

owid_eu_gbr <- filter(owid, iso_code %in% eu_gbr)

owid_gbr_fra_deu_swe <- filter(owid, iso_code %in% gbr_fra_deu_swe)

owid_americas <- filter(owid, iso_code %in% americas)

owid_world <- filter(owid, iso_code %in% world)


## filtering by date ----

date_v <- seq(as.Date("2020-03-01"), as.Date("2020-06-23"), by = "days") %>% as.character() #need as.charactor %in% to work

## date range filter ----
date_range <- filter(owid_world, date %in% date_v)
date_range <- mutate(date_range, Date = as.Date(date)) #then back to date format


##plot new deaths or cases per million in the countries selected at date range filter ----
ggplot(date_range) +
  stat_smooth(mapping = aes(x = Date, y = new_cases_per_million, group = iso_code, colour = iso_code), span= 0.5, show.legend = TRUE) +
  geom_point(mapping =  aes(x = Date, y = new_cases_per_million, colour = iso_code, shape = iso_code), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("1 week"),
               labels = scales::label_date_short()) +
  #scale_y_continuous(name = "new cases per million", breaks = seq(0, 25, by = 5)) +
  ylim(0, 150) +
  ylab("new cases per million") +
  labs (title = "Covid-19 new cases per million in UK, France, USA, Italy & China",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = "March 1st to June 23rd 2020 https://github.com/LordGenome/covid_stats/covid_R")


##plot new deaths and cases per million in the countries selected at date range filter ----
ggplot(date_range) +
  stat_smooth(mapping = aes(x = Date, y = new_cases_per_million, group = iso_code), colour = "blue", span= 0.5, show.legend = TRUE) +
  geom_point(mapping =  aes(x = Date, y = new_cases_per_million, shape = iso_code,), colour = "blue", show.legend = TRUE) +
  #stat_smooth(mapping = aes(x = Date, y = 6*new_deaths_per_million, group = iso_code), span= 0.5, colour = "yellow", show.legend = FALSE) +
  #geom_point(mapping =  aes(x = Date, y = 6*new_deaths_per_million, shape = iso_code), colour = "yellow", show.legend = FALSE) 
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("1 week"),
               labels = scales::label_date_short()) +
  #scale_y_continuous(name = "new cases per million", breaks = seq(0, 25, by = 5)) +
  ylim(0, 125) +
  ylab("new cases per million") +
  labs (title = "Covid-19 new cases per million in UK, France, USA, Italy & China",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = "March 1st to June 23rd 2020 https://github.com/LordGenome/covid_R")



#ylim(0, 25) +
#ylab("new deaths per million") +
#labs (title = "Covid-19 new deaths per million in UK, France, USA, Italy & China"
#  subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
#        caption = "March 1st to June 12th 2020") +
#  scale_y_continuous(name = "new cases per million", breaks = seq(0, 150, by = 25))


## log2 plot of deaths and cases ---

ggplot(date_range) +
  geom_smooth(mapping = aes(x = Date, y = log2tcpm, group = iso_code, colour = iso_code), 
            linetype = "solid", se = FALSE, span= 0.1, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = log2tcpm, shape = iso_code, colour = iso_code), show.legend = TRUE) +
  stat_smooth(mapping = aes(x = Date, y = log2tdpm, group = iso_code, colour = iso_code), 
              linetype = "dashed", se = FALSE, span= 0.1, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = log2tdpm, shape = iso_code, colour = iso_code), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("1 week"),
               labels = scales::label_date_short()) +
  scale_y_continuous(name = "log2 per million", breaks = seq(-15, 15, by = 1))

##filtering by specific date ----
owid_one_date <- filter(owid, date == "2020-06-20")
owid_one_date <- mutate(owid_one_date, lethality = (100*total_deaths_per_million/total_cases_per_million))
owid_one_date <- filter(owid_one_date, lethality > 0)

ggplot(owid_one_date) + 
  geom_point(mapping = aes(x = life_expectancy, y = log2(total_deaths_per_million)), colour = "blue") +
  geom_smooth(method = "glm", mapping = aes(x = life_expectancy, y = log2(total_deaths_per_million)), colour = "blue", show.legend = FALSE) +
  geom_point(mapping = aes(x = life_expectancy, y = log2(total_cases_per_million)), colour = "orange") +
  geom_smooth(method = "glm", mapping = aes(x = life_expectancy, y = log2(total_cases_per_million)), colour = "orange", show.legend = FALSE) +
    theme_bw() +
  scale_y_continuous(name = "log2 per million", breaks = seq(-5, 15, by = 1)) +
  scale_x_continuous(name = "life expectancy", breaks = seq(50, 100, by = 1))

#output x interecpt and gradient co-efficients
fit <- lm(owid_one_date$life_expectancy, log2(owid_one_date$total_cases_per_million))

lfit <- line(owid_one_date$life_expectancy, log2(owid_one_date$total_cases_per_million))
summary(lfit)

summary(fit) #works
anova(fit) #doesn't work
coefficients(fit) #gives c and m from y=mx+c
effects(fit) #doesn't work
fitted.values(fit) #table for fitted values
residuals(fit) #another table
formula(fit) #error in formula.default(fit) : invalid formula

scores <- read.csv("/Users/graham/Documents/scores.csv")


## curve fitting ----
#https://rdrr.io/r/stats/line.html
require(graphics)
plot(cars)
(z <- line(cars))
abline(coef(z))
## Tukey-Anscombe Plot :
plot(residuals(z) ~ fitted(z), main = deparse(z$call))

#
d.AS <- data.frame(x = c(-4:3, 12), y = 3*c(rep(0,6), -5, 5, 1))
cAS <- with(d.AS, t(sapply(1:10,
                           function(it) line(x,y, iter=it)$coefficients)))
dimnames(cAS) <- list(paste("it =", format(1:10)), c("intercept", "slope"))
cAS


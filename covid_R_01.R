## R ----

setwd("/Users/graham/Dropbox/01_gitR/our_world_in_data")
## load packages ----
library(tidyverse)
library(lubridate)
library(qcc)
library(jsonlite)

## import data ----

owid_raw  <-  as_tibble(read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv"))

#add log2() columns
owid <- mutate(owid_raw, log2tdpm = log2(total_deaths_per_million), log2tcpm = log2(total_cases_per_million))
owid <- rename(owid, pc_vax = people_vaccinated_per_hundred, hospitalised_per_million = hosp_patients_per_million)

## Reformat data to get country sets ----

#iso_code filters as vectors

eu <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", 
        "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", 
        "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")


eu_gbr <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", 
            "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", 
            "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR")
gbr_fra_deu_swe <- c("GBR", "FRA", "DEU", "SWE")

gbr_fra_deu_swe_ita <- c("GBR", "FRA", "DEU", "SWE", "ITA")

gbr_fra_deu_usa_can <- c("GBR", "FRA", "DEU", "USA", "CAN")

americas <- c("ARG", "BRA", "CAN","CHL", "USA", "MEX")

badperformers <- c("ITA", "ESP", "CHL", "USA", "GBR")

world <- c("GBR", "FRA", "USA", "ITA", "DEU", "ESP", "SWE", "AUS")

mex_bra_usa_can <- c( "BRA", "CAN", "USA", "MEX")

mex_usa_can <- c( "CAN", "USA", "MEX")

mex_bra_usa_can_uk <- c( "BRA", "CAN", "USA", "MEX", "GBR")

## filtering by country ----

owid_gbr <- filter(owid, iso_code == "GBR")

owid_eu <- filter(owid, iso_code %in% eu)

owid_eu_gbr <- filter(owid, iso_code %in% eu_gbr)

owid_gbr_fra_deu_swe <- filter(owid, iso_code %in% gbr_fra_deu_swe)

owid_gbr_fra_deu_swe_ita <- filter(owid, iso_code %in% gbr_fra_deu_swe_ita)

owid_americas <- filter(owid, iso_code %in% americas)

owid_world <- filter(owid, iso_code %in% world)

owid_mex_bra_usa_can <- filter(owid, iso_code %in% mex_bra_usa_can)

owid_mex_usa_can <- filter(owid, iso_code %in% mex_usa_can)

owid_mex_bra_usa_can_uk <- filter(owid, iso_code %in% mex_bra_usa_can_uk)

owid_gbr_fra_deu_usa_can <- filter(owid, iso_code %in% gbr_fra_deu_usa_can)


## filtering by date ----

sys_date <- Sys.Date()
start_date <- as.Date("2020-03-01")
label_start_date <- format(start_date, "%d %b")
label_sys_date <- format(sys_date, "%d %b")
label_dates <- paste0(label_start_date," to ",label_sys_date, " https://github.com/LordGenome/our_world_in_data")


date_v <- seq(as.Date("2020-03-01"), as.Date(sys_date), by = "days") %>% as.character() #need as.charactor for %in% to work

## date range filter for mex_bra_usa_can_uk ----
americas <- filter(owid_mex_bra_usa_can_uk, date %in% date_v)
americas <- mutate(americas, Date = as.Date(date)) #then back to date format
image_name <- paste("images/deaths_mex_bra_usa_can_uk_",sys_date, ".png", sep="")


##plot new deaths per million in the countries selected at date range filter: mex_bra_usa_can_uk ----
image_name <- paste("images/deaths_mex_bra_usa_can_uk_",sys_date, ".png", sep="")
ggplot(americas) +
  stat_smooth(mapping = aes(x = Date, y = new_deaths_per_million, group = iso_code, colour = iso_code), span= 0.1, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = new_deaths_per_million, colour = iso_code, shape = iso_code), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 weeks"),
               labels = scales::label_date_short()) +
  #scale_y_continuous(name = "new cases per million", breaks = seq(0, 25, by = 5)) +
  scale_y_continuous(breaks = seq(0, 20, by = 5 ))  +
  ylab("new deaths per million") +
  labs (title = "Covid-19 new deaths per million in Brazil, Canada, Mexico, UK and USA",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates) #+
#ggsave(image_name)


##plot new cases per million in the countries selected at date range filter: mex_bra_usa_can_uk ----
image_name <- paste("images/cases_mex_bra_usa_can_uk_",sys_date, ".png", sep="")
ggplot(americas) +
  stat_smooth(mapping = aes(x = Date, y = new_cases_per_million, group = iso_code, colour = iso_code), span= 0.1, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = new_cases_per_million, colour = iso_code, shape = iso_code), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 weeks"),
               labels = scales::label_date_short()) +
  #scale_y_continuous(name = "new cases per million", breaks = seq(0, 25, by = 5)) +
  scale_y_continuous(breaks = seq(0, 1000, by = 100 ))  +
  ylab("new cases per million") +
  labs (title = "Covid-19 new cases per million in Brazil, Canada, Mexico, UK and USA",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates) #+
#ggsave(image_name)

##vaccination status
ggplot(americas) +
  #ggplot(europe) +
  stat_smooth(mapping = aes(x = Date, y = pc_vax, group = iso_code, colour = iso_code), span= 0.1, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = pc_vax, colour = iso_code, shape = iso_code), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 weeks"),
               labels = scales::label_date_short()) +
  #scale_y_continuous(name = "new cases per million", breaks = seq(0, 25, by = 5)) +
  scale_y_continuous(breaks = seq(0, 100, by = 5 ))  +
  ylab("percent population vaccinated") +
  labs (title = "Covid-19 vaccinations in Brazil, Canada, Mexico, UK and USA",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates)


## date range filter for gbr_fra_deu_swe_ita  ----
europe <- filter(owid_gbr_fra_deu_swe_ita, date %in% date_v)

europe <- mutate(europe, Date = as.Date(date)) #then back to date format ##############################

##plot new deaths per million in the countries selected at date range filter:eu_gbr ----
image_name <- paste("images/deaths_owid_gbr_fra_deu_swe",sys_date, ".png", sep="")
ggplot(europe) +
  stat_smooth(mapping = aes(x = Date, y = new_deaths_per_million, group = iso_code, colour = iso_code), span= 0.1, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = new_deaths_per_million, colour = iso_code, shape = iso_code), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 weeks"),
               labels = scales::label_date_short()) +
  #scale_y_continuous(name = "new cases per million", breaks = seq(0, 25, by = 5)) +
  scale_y_continuous(breaks = seq(0, 20, by = 5 ))  +
  ylab("new deaths per million") +
  labs (title = "Covid-19 new deaths per million in UK, France, Germany & Sweden",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates) #+
#ggsave(image_name)

##plot new cases per million in the countries selected at date range filter: gbr_fra_deu_swe ----
image_name <- paste("images/cases_gbr_fra_deu_swe",sys_date, ".png", sep="")
ggplot(europe) +
  stat_smooth(mapping = aes(x = Date, y = new_cases_per_million, group = iso_code, colour = iso_code), span= 0.1, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = new_cases_per_million, colour = iso_code, shape = iso_code), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 weeks"),
               labels = scales::label_date_short()) +
  #scale_y_continuous(name = "new cases per million", breaks = seq(0, 25, by = 5)) +
  #scale_y_continuous(name = "new cases per million", breaks = seq(0, 25, by = 5)) +
  scale_y_continuous(breaks = seq(0, 20000, by = 100 ))  +
  ylab("new cases per million") +
  labs (title = "Covid-19 new cases per million in UK, France, Germany & Sweden",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates) #+
#  ggsave(image_name)

###############################################################################################################################################################################
###############################################################################################################################################################################
###############################################################################################################################################################################
##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america
##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america
##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america
##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america
##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america
###############################################################################################################################################################################
###############################################################################################################################################################################
###############################################################################################################################################################################


europe_north_america <- filter(owid_gbr_fra_deu_usa_can, date %in% date_v)
europe_north_america <- mutate(owid_gbr_fra_deu_usa_can, Date = as.Date(date)) #then back to date format
#image_name <- paste("images/vaccinations_gbr_fra_deu_usa_can",sys_date, ".png", sep="")


ggplot(europe_north_america) +
  stat_smooth(mapping = aes(x = Date, y = pc_vax, group = iso_code, colour = iso_code), span= 0.1, show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 weeks"),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = seq(0, 100, by = 5 ))  +
  ylab("total vaccinated per hundred") +
  labs (title = "Covid-19 vaccinations in UK, France, Germany, USA & Canada",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates)



##plot new cases per million in the countries selected at date range filter: gbr_fra_deu_swe ----
#image_name <- paste("images/cases_gbr_fra_deu_swe",sys_date, ".png", sep="")
ggplot(europe_north_america) +
  stat_smooth(mapping = aes(x = Date, y = new_cases_per_million, group = iso_code, colour = iso_code), span= 0.1, show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 weeks"),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = seq(0, 20000, by = 100 ))  +
  ylab("new cases per million") +
  labs (title = "Covid-19 new cases per million in UK, France, Germany, USA & Canada",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates) #+



##plot new deaths per million in the countries selected at date range filter:eu_gbr ----
#image_name <- paste("images/deaths_owid_gbr_fra_deu_swe",sys_date, ".png", sep="")
ggplot(europe_north_america) +
  stat_smooth(mapping = aes(x = Date, y = new_deaths_per_million, group = iso_code, colour = iso_code), span= 0.1, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = new_deaths_per_million, colour = iso_code, shape = iso_code), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 weeks"),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = seq(0, 20, by = 5 ))  +
  ylab("new deaths per million") +
  labs (title = "Covid-19 new deaths per million inUK, France, Germany, USA & Canada",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates) #+
#ggsave(image_name)




##plot R value in the countries selected at date range filter:eu_gbr ----
#image_name <- paste("images/deaths_owid_gbr_fra_deu_swe",sys_date, ".png", sep="")
ggplot(europe_north_america) +
  stat_smooth(mapping = aes(x = Date, y = reproduction_rate, group = iso_code, colour = iso_code), span= 0.01, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = reproduction_rate, colour = iso_code, shape = iso_code), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 weeks"),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = seq(0, 5, by = 0.25 ))  +
  ylab("Reproduction Rate") +
  labs (title = "SARS-CoV-2 Reproduction Rate in UK, France, Germany, USA & Canada",
        #subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        subtitle = "after Arroyo-Marioli F, Bullano F, Kucinskas S, Rondón-Moreno C (2021) 
        Tracking of COVID-19: A new real-time estimation using the Kalman filter. PLoS ONE 16(1): e0244474.
        Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates) #+
#ggsave(image_name)


###############################################################################################################################################################################
###############################################################################################################################################################################
###############################################################################################################################################################################
##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america
##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america
##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america
##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america
##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america ##vaccination status europe & north america
###############################################################################################################################################################################
###############################################################################################################################################################################
###############################################################################################################################################################################


##plot new cases per 50,000 and deaths per million in GBR selected at date range filter: gb----
#image_name <- paste("images/cases_gbr_fra_deu_swe",sys_date, ".png", sep="")
## date range filter for gbr_fra_deu_swe  ----
gbr <- filter(owid_gbr, date %in% date_v)

gbr <- mutate(gbr, Date = as.Date(date)) #then back to date format ##############################


ggplot(gbr) +
  stat_smooth(mapping = aes(x = Date, y = (new_cases_per_million/50)), colour = "BLUE",
              linetype = "solid", se = TRUE, span= 0.05, show.legend = TRUE) +
  stat_smooth(mapping = aes(x = Date, y = (new_deaths_per_million) ), colour = "BLACK", 
              linetype = "dashed", se = TRUE, span= 0.05, show.legend = TRUE) +
  stat_smooth(mapping = aes(x = Date, y = (hospitalised_per_million/25) ), colour = "GREEN", 
              linetype = "dashed", se = TRUE, span= 0.05, show.legend = TRUE) +
  stat_smooth(mapping = aes(x = Date, y = (icu_patients_per_million/4) ), colour = "ORANGE", 
              linetype = "dashed", se = TRUE, span= 0.05, show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 weeks"),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = seq(-4, 100, by = 2 )) +
  ylab("new deaths per million") +
  labs (title = "UK Covid-19 new cases per 20,000 (blue), hospital admissons per 40,000 (green), new deaths per million (black),
        ICU cases per 250,000 (Orange)",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates)


########don't plot cases

ggplot(gbr) +
  #stat_smooth(mapping = aes(x = Date, y = (new_cases_per_million/50)), colour = "BLUE",
  #            linetype = "solid", se = TRUE, span= 0.05, show.legend = TRUE) +
  stat_smooth(mapping = aes(x = Date, y = (new_deaths_per_million) ), colour = "BLACK", 
              linetype = "dashed", se = TRUE, span= 0.05, show.legend = TRUE) +
  stat_smooth(mapping = aes(x = Date, y = (hospitalised_per_million/25) ), colour = "GREEN", 
              linetype = "dashed", se = TRUE, span= 0.05, show.legend = TRUE) +
  stat_smooth(mapping = aes(x = Date, y = (icu_patients_per_million/4) ), colour = "ORANGE", 
              linetype = "dashed", se = TRUE, span= 0.05, show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 weeks"),
               labels = scales::label_date_short()) +
  scale_y_continuous(breaks = seq(-4, 100, by = 2 )) +
  ylab("new deaths per million") +
  labs (title = "UK Covid-19 hospital admissons per 40,000 (green), new deaths per million (black), ICU cases per 250,000 (Orange)",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates)


##plot new deaths and cases per million in the countries selected at date range filter ----
ggplot(europe) +
  stat_smooth(mapping = aes(x = Date, y = new_deaths_per_million, group = iso_code, colour = iso_code), span= 0.7, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = new_deaths_per_million, shape = iso_code, colour = iso_code), show.legend = TRUE) +
  #stat_smooth(mapping = aes(x = Date, y = new_deaths_per_million, group = iso_code), span= 0.7, colour = "yellow", show.legend = FALSE) +
  #geom_point(mapping =  aes(x = Date, y = new_deaths_per_million, shape = iso_code), colour = "yellow", show.legend = FALSE) 
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("1 week"),
               labels = scales::label_date_short()) +
  #scale_y_continuous(name = "new cases per million", breaks = seq(0, 25, by = 5)) +
  ylim(0, 10) +
  ylab("new cases per million") +
  labs (title = "Covid-19 new deaths per million in Germany, France, UK, Italy & Sweden",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates)



#ylim(0, 25) +
#ylab("new deaths per million") +
#labs (title = "Covid-19 new deaths per million in UK, France, USA, Italy, China, Brazil & Germany"
#  subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
#        caption = "March 1st to June 25th 2020") +
#  scale_y_continuous(name = "new cases per million", breaks = seq(0, 150, by = 25))


## log2 plot of deaths and cases small series----
image_name <- paste("images/log2_deu_fra_swe_uk",sys_date, ".png", sep="")

ggplot(europe) +
  geom_smooth(mapping = aes(x = Date, y = log2tcpm, group = iso_code, colour = iso_code), 
            linetype = "solid", se = TRUE, span= 0.2, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = log2tcpm, shape = iso_code, colour = iso_code), show.legend = TRUE) +
  stat_smooth(mapping = aes(x = Date, y = log2tdpm, group = iso_code, colour = iso_code), 
              linetype = "dashed", se = TRUE, span= 0.2, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = log2tdpm, shape = iso_code, colour = iso_code), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 week"),
               labels = scales::label_date_short()) +
  scale_y_continuous(name = "log2 per million", breaks = seq(-16, 16, by = 1)) +
  labs (title = "Covid-19 total cases (solid) & deaths (hatched) per million Germany, France, UK & Sweden",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates) +
  ggsave(image_name)

## log2 plot of deaths and cases larger series----

    +
  ggsave(image_name)

## linear plot of cases ----
image_name <- paste("images/linear_deu_fra_ita_swe_uk",sys_date, ".png", sep="")

ggplot(europe) +
  geom_smooth(mapping = aes(x = Date, y = total_cases_per_million, group = iso_code, colour = iso_code), 
              linetype = "solid", se = TRUE, span= 0.02, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = y = total_cases_per_million, shape = iso_code, colour = iso_code), show.legend = TRUE) +
  #stat_smooth(mapping = aes(x = Date, y = log2tdpm, group = iso_code, colour = iso_code), 
             #linetype = "dashed", se = TRUE, span= 0.2, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = log2tdpm, shape = iso_code, colour = iso_code), show.legend = TRUE) +
  theme_bw() +
 scale_x_date(NULL,
             breaks = scales::breaks_width("2 weeks"),
              labels = scales::label_date_short()) +
 scale_y_continuous(name = "total cases per million", breaks = seq(0, 500000, by = 100000)) +
labs (title = "Covid-19 total cases per million Germany, France, UK & Sweden",
     subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
      caption = label_dates) +
  ggsave(image_name)

## linear plot of deaths ----
image_name <- paste("images/linear_deu_fra_ita_swe_uk_",sys_date, ".png", sep="")


ggplot(europe) +
  #geom_smooth(mapping = aes(x = Date, y = total_cases_per_million, group = iso_code, colour = iso_code), 
   #           linetype = "solid", se = TRUE, span= 0.02, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = y = total_cases_per_million, shape = iso_code, colour = iso_code), show.legend = TRUE) +
  stat_smooth(mapping = aes(x = Date, y = total_deaths_per_million, group = iso_code, colour = iso_code), 
  linetype = "solid", se = TRUE, span= 0.005, show.legend = TRUE) +
  #geom_point(mapping =  aes(x = Date, y = total_deaths_per_million, shape = iso_code, colour = iso_code), show.legend = TRUE) +
  theme_bw() +
  scale_x_date(NULL,
               breaks = scales::breaks_width("2 weeks"),
               labels = scales::label_date_short()) +
  scale_y_continuous(name = "total deaths per million", breaks = seq(0, 15000, by = 100)) +
  labs (title = "Covid-19 total deaths per million in Germany, France, UK & Sweden",
        subtitle = "Source: Our World in Data  (https://github.com/owid/covid-19-data/tree/master/public/data)",
        caption = label_dates) #+
  #ggsave(image_name)





##filtering by specific date ----
owid_one_date <- filter(owid, date == "2020-11-13")
owid_one_date <- mutate(owid_one_date, lethality = (100*total_deaths_per_million/total_cases_per_million))
owid_one_date <- filter(owid_one_date, lethality > 0)

ggplot(owid_one_date) + 
  geom_point(mapping = aes(x = life_expectancy, y = log2(total_deaths_per_million)), colour = "blue") +
  geom_smooth(method = "glm", mapping = aes(x = life_expectancy, y = log2(total_deaths_per_million)), colour = "blue", show.legend = FALSE) +
  geom_point(mapping = aes(x = life_expectancy, y = log2(total_cases_per_million)), colour = "orange") +
  geom_smooth(method = "glm", mapping = aes(x = life_expectancy, y = log2(total_cases_per_million)), colour = "orange", show.legend = FALSE) +
    theme_bw() +
  scale_y_continuous(name = "log2 per million", breaks = seq(-5, 15, by = 1)) +
  scale_x_continuous(name = "life expectancy", breaks = seq(50, 100, by = 1)) +
  ggsave(image_name)



ggplot(owid_one_date) + 
  #geom_point(mapping = aes(x = life_expectancy, y = total_deaths_per_million), colour = "blue") +
 #geom_smooth(method = "loess",span = 0.9, mapping = aes(x = life_expectancy, y = total_deaths_per_million), colour = "blue", show.legend = FALSE) +
  geom_point(mapping = aes(x = life_expectancy, y = total_cases_per_million), colour = "orange") +
  geom_smooth(method = "lm", formula = y ~ poly(x,4), mapping = aes(x = life_expectancy, y = total_cases_per_million), colour = "orange", show.legend = FALSE) +
  theme_bw() +
  scale_y_continuous(name = "cases/deaths per million", breaks = seq(-10000, 200000, by = 10000)) +
  scale_x_continuous(name = "life expectancy", breaks = seq(50, 100, by = 1))

#method = "lm", formula = y ~ x + I(x^2), size = 1


#output x interecpt and gradient co-efficients
#fit <- lm(owid_one_date$life_expectancy, log2(owid_one_date$total_cases_per_million))

lfit <- line(owid_one_date$life_expectancy, log2(owid_one_date$total_cases_per_million))
summary(lfit)

summary(lfit) #works
anova(lfit) #doesn't work
coefficients(lfit) #gives c and m from y=mx+c
effects(fit) #doesn't work
fitted.values(lfit) #table for fitted values
residuals(lfit) #another table
formula(lfit) #error in formula.default(fit) : invalid formula

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



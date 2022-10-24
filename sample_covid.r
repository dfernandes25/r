
## load libraries ##
if (!require("pacman"))
  install.packages("pacman")
pacman::p_load( dplyr, ggplot2, ggthemes, gridExtra, janitor, tidyverse, readxl, readr, anytime, lubridate)

setwd("C:/Users/cbc/Desktop/covid/csv")

# census bureau usc
regions     <- "https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv"

# ny times nyt
states      <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
counties    <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

# covidtracking.com ctc
us_hist     <- "https://covidtracking.com/api/v1/us/daily.csv"
us_curr     <- "https://covidtracking.com/api/v1/us/current.csv"
st_hist     <- "https://covidtracking.com/api/v1/states/daily.csv"
st_curr     <- "https://covidtracking.com/api/v1/states/current.csv"
us_rde      <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vR_xmYt4ACPDZCDJcY12kCiMiH0ODyx3E1ZvgOHB8ae1tRcjXbs_yWBOA4j4uoCEADVfC1PS2jYO68B/pub?gid=43720681&single=true&output=csv"

# hopkins hop #
proc.hopkins    <- function(x) {system2("powershell", args = c("-file", ("C:/Users/cbc/Desktop/covid/posh/pscovid-19.ps1")))}
proc.hopkins()
# read in csv from powershell #
ps_covid        <- "C:/Users/cbc/Desktop/covid/csv/pscovidtmp.csv"

## read data in and cleanup column names ##
nyt_cov_cty     <- read_csv(url(counties))    %>% clean_names()
nyt_cov_st      <- read_csv(url(states))      %>% clean_names()

ctc_cov_st_hist <- read_csv(url(st_hist))  %>% clean_names()
ctc_cov_st_curr <- read_csv(url(st_curr))  %>% clean_names()
ctc_cov_us_hist <- read_csv(url(us_hist))  %>% clean_names()
ctc_cov_us_curr <- read_csv(url(us_curr))  %>% clean_names()
ctc_cov_us_rde  <- read_csv(url(us_rde))   %>% clean_names()

usc_reg         <- read_csv(url(regions))     %>% clean_names()

hop_ps_covid           <- read_csv(ps_covid) %>% clean_names()
colnames(hop_ps_covid) <- c("country", "state", "county","last_update", "confirmed", "deaths", "recovered", "active", "latitude", "longitude")

 ctc_cov_st_curr$date   <- as.character(ctc_cov_st_curr$date)
 ctc_cov_st_curr$date   <- anydate(ctc_cov_st_curr$date)
 
 ctc_cov_st_hist$date   <- as.character(ctc_cov_st_hist$date)
 ctc_cov_st_hist$date   <- anydate(ctc_cov_st_hist$date)

 ctc_cov_us_curr$date   <- as.character(ctc_cov_us_curr$date)
 ctc_cov_us_curr$date   <- anydate(ctc_cov_us_curr$date)
 
 ctc_cov_us_hist$date   <- as.character(ctc_cov_us_hist$date)
 ctc_cov_us_hist$date   <- anydate(ctc_cov_us_hist$date)

usc_reg <- usc_reg %>% rename(stname = state)
usc_reg <- usc_reg %>% rename(state = state_code)

ctc_cov_st_curr <- right_join(usc_reg, ctc_cov_st_curr, by = "state")
ctc_cov_st_hist <- right_join(usc_reg, ctc_cov_st_hist, by = "state")


## get top as specified by x ##
x <- 10

top_st_deaths <- ctc_cov_st_curr %>% 
  arrange(desc(death)) %>% head(x) %>% 
  select(date, region, division, state, death)

top_st_deaths_i <- ctc_cov_st_curr %>% 
  arrange(desc(death_increase)) %>% head(x) %>% 
  select(date, region, division, state, death_increase)

top_st_cases <- ctc_cov_st_curr %>% 
  arrange(desc(positive)) %>% head(x) %>% 
  select(date, region, division, state, positive)

top_st_cases_i <- ctc_cov_st_curr %>% 
  arrange(desc(positive_increase)) %>% head(x) %>% 
  select(date, region, division, state, positive_increase)

## plots ##

 # us_7d <- ctc_cov_us_hist %>% select(date, death, death_increase, positive, positive_increase, hcum, hinc ) %>%
 #  filter(date >= today() - days(7))


us_7d <- ctc_cov_us_hist %>%
 filter(date >= today() - days(7)) %>%
 ggplot() +
 aes(x = date, y = death, label = death) +
 geom_line(size = 1L, colour = "#0c4c8a") +
 geom_point() +
 geom_text(size = 3, vjust = "inward", hjust = "inward",
           show.legend = FALSE) + #, position = position_stack(vjust = 0.5))
 labs(x = "date", y = "deaths", title = "Total US Deaths - 7 day moving") +
 theme_classic2()
 
us_7c <- ctc_cov_us_hist %>%
  filter(date >= today() - days(7)) %>%
  ggplot() +
  aes(x = date, y = positive, label = positive) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_point() +
  geom_text(size = 3, vjust = "inward", hjust = "inward",
            show.legend = FALSE) + #, position = position_stack(vjust = 0.5))
  labs(x = "date", y = "case count", title = "Total US Case Count - 7 day moving") +
  theme_classic2()


top_st_deaths_p <- ggplot(top_st_deaths) +
  aes(x = reorder(state, -death), y=death, label=death) +
  geom_bar(stat="identity", fill="gray") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(x = "state", y = "deaths", title = "top 10 cumulative death count") +
  theme_classic2() +
  theme(axis.text.x=element_text(angle =- 45, vjust = 0.5))

top_st_deaths_i_p <- ggplot(top_st_deaths_i) +
  aes(x = reorder(state, -death_increase), y=death_increase, label=death_increase) +
  geom_bar(stat="identity", fill="gray") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  labs(x = "state", y = "increased deaths", title = "top 10 day over day death increase") +
  theme_classic2() +
  theme(axis.text.x=element_text(angle =- 45, vjust = 0.5))



par(mfrow = c(2, 5))  # Set up a 2 x 2 plotting space
st_list <- unique(top_st_deaths$state)
for (i in seq_along(st_list)) {
  plotname <- paste(st_list[i])
  plotname.plot <- ctc_cov_st_hist %>%
    filter(date >= today() - days(7)) %>% filter(state == paste(st_list[i])) %>%
    ggplot() +
    aes(x = date, y = death, label = death) +
    geom_line(size = 1L, colour = "#0c4c8a") +
    geom_point() +
    geom_text(size = 3, vjust = "inward", hjust = "inward",
              show.legend = FALSE) + #, position = position_stack(vjust = 0.5))
    labs(x = "date", y = "death count") +
    ggtitle(paste(st_list[i], 'Death Count - 7 day Moving'))
  theme_classic2()
  
  print(plotname.plot)
  
  testit <- function(x)
  {
    p1 <- proc.time()
    Sys.sleep(x)
    proc.time() - p1 # The cpu usage should be negligible
  }
  testit(3.7)
  
}



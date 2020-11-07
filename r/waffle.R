library(ggplot2)
library(ggthemes)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(reshape2)
library(waffle)

parse_date <- function(s) {
  lubridate::with_tz(lubridate::mdy_hms(s), tzone="America/New_York")
}

# import data
activities <- read_csv("data/activities.csv")
names(activities) <- str_replace_all(names(activities), c(" "="_")) # remove spaces from header titles
activities$Activity_Date <- lubridate::as_datetime(unlist(purrr::map(activities$Activity_Date, parse_date)))
activities <- dplyr::filter(activities, Activity_Type == "Run")

# filter only runs from the new marathon plan
activities <- dplyr::filter(activities, Activity_Date >= lubridate::ymd("2020-04-01"))

# get date-based data
activities$wday <- lubridate::wday(activities$Activity_Date)
activities <- dplyr::arrange(activities, Activity_Date)
days_freq <- table(activities$wday)
days_km <- dplyr::summarise(dplyr::group_by(activities, wday), dist = sum(Distance))
days_time <- dplyr::summarise(dplyr::group_by(activities, wday), time = sum(Moving_Time/(60*60))) # hours
days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
by_day  <- dplyr::tibble(`day` = days, `freq` = days_freq, `dist` = days_km$dist, `moving_time` = days_time$time)
remove(days_freq, days_km, days_time, days)

# plot
by_day.m <- melt(by_day, id.vars="day")
ggplot2::ggplot(data=by_day.m, aes(day, value)) +
  geom_bar(aes(fill=variable), width = 0.4, position = position_dodge(width=0.5), stat = "identity")
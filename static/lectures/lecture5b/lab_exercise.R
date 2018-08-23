# Code for lab exercise
library(cricketdata)
library(tidyverse)
d1 <- fetch_cricinfo(matchtype = "t20",
                    sex = "men",
                    activity = "batting",
                    country = "australia")
d2 <- fetch_cricinfo(matchtype = "t20",
                     sex = "men",
                     activity = "batting",
                     country = "india")
d1 <- d1 %>% mutate(country = "Australia")
d2 <- d2 %>% mutate(country = "India")
d <- bind_rows(d1, d2)

ggplot(d, aes(x=country, y=Average)) + geom_boxplot()
ggplot(d, aes(x=country, y=StrikeRate)) + geom_boxplot()

# Scraping
library(rvest)
library(plotly)
site <- "https://www.icc-cricket.com/rankings/mens/team-rankings/odi"
raw_html <- read_html(site)
tables <- raw_html %>% html_table(fill=TRUE)
length(tables)
tables[[1]]
odi <- tables[[1]]
glimpse(odi)
odi <- odi %>%
  mutate(Points = as.numeric(sub("\\,", "", Points)))

site <- "https://www.icc-cricket.com/rankings/mens/team-rankings/t20i"
raw_html <- read_html(site)
tables <- raw_html %>% html_table(fill=TRUE)
length(tables)
tables[[1]]
t20 <- tables[[1]]
glimpse(t20)
t20 <- t20 %>%
  mutate(Points = as.numeric(sub("\\,", "", Points)))

odi_t20 <- full_join(odi, t20, by="Team")
ggplot(odi_t20, aes(x=Rating.x, y=Rating.y, label=Team)) +
  geom_abline(slope=1) +
  geom_point() +
  coord_equal() + xlim(c(0,135)) + ylim(c(0,135))
ggplotly()

# NBA
indexes <- c("", "41", "81", "121", "161", "201", "241")
nba <- NULL
for (i in 1:7) {
  site <- paste0("http://www.espn.com/nba/statistics/player/_/stat/scoring-per-game/sort/avgPoints/count/",indexes[i])
  raw_html <- read_html(site)
  temp <- raw_html %>% html_table(fill=TRUE, header=TRUE)
  nba <- bind_rows(nba, temp)
}
nba <- nba %>% filter(PLAYER != "PLAYER")
nba <- nba %>%
  separate(PLAYER, c("full_name", "position"), ",") %>%
  mutate(PTS = as.numeric(PTS))

ggplot(nba, aes(x=position, y=PTS)) + geom_boxplot()

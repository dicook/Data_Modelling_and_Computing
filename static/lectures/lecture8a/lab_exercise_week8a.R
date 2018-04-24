# lab exercises Week 8, class a
library(tidyverse)
library(broom)
library(readxl)
# read in data
fert <- read_xlsx("data/indicator undata total_fertility.xlsx") 
colnames(fert)[1] <- "country"
fert <- fert %>% gather(year, fert, -country) %>% 
  mutate(year = as.numeric(year)) %>%
  filter(year > 1950)

ggplot(fert, aes(x=year, y=fert, group=country)) +
  geom_line(alpha=0.1)


fert2 <- fert %>% mutate(year1950 = year-1950)

# Australian Fertility
oz_fert <- fert2 %>% filter(country=="Australia")

p1 <- ggplot(data=oz_fert, aes(x=year, y=fert)) + 
  geom_line() 
oz_fert_lm <- lm(fert~year1950, data=oz_fert)
tidy(oz_fert_lm)
oz_fert_mod <- augment(oz_fert_lm, oz_fert)
p2 <- ggplot(data=oz_fert_mod, aes(x=year, y=.fitted)) + 
  geom_line() 
p3 <- ggplot(data=oz_fert_mod, aes(x=year, y=.std.resid)) + 
  geom_hline(yintercept=0, colour="white", size=2) +
  geom_line() 
grid.arrange(p1, p2, p3, nrow=1)

# Combine with gapminder to join continent

library(gapminder)
country_continent <- gapminder %>% select(country, continent) %>% distinct

fert2 <- fert2 %>% 
  filter(!is.na(fert)) %>%
  mutate(country = case_when(
    country == "Korea, Rep." ~ "South Korea",
    country == "Yemen, Rep." ~ "Yemen",
    country == "Korea, Dem. Rep." ~ "North Korea",
    TRUE ~ as.character(country)
  )) %>%
  left_join(., country_continent)


# Modeling fertility
library("purrr")
by_country <- fert2  %>% 
  select(country, year1950, fert, continent) %>%
  group_by(country, continent) %>% 
  nest()
by_country <- by_country %>% 
  mutate(
    model = purrr::map(data, ~ lm(fert ~ year1950, 
                                  data = .))
  )

country_coefs <- by_country %>% 
  unnest(model %>% purrr::map(broom::tidy))
country_coefs <- country_coefs %>% 
  select(country, continent, term, estimate) %>% 
  spread(term, estimate) %>%
  rename(intercept = `(Intercept)`)
head(country_coefs)
country_coefs %>%
  filter(country == "Australia")


gp <-ggplot(country_coefs, aes(x=intercept, y=year1950, 
                               colour=continent, label=country)) +
  geom_point(alpha=0.5, size=2) +
  scale_color_brewer(palette = "Dark2")
library(plotly)
ggplotly(gp)

country_fit <- by_country %>% 
  unnest(model %>% 
           purrr::map(broom::glance))

ggplot(country_fit) + geom_histogram(aes(x=r.squared))
badfit <- country_fit %>% filter(r.squared <= 0.4)
fert2_sub <- fert2 %>% filter(country %in% badfit$country)
ggplot(data=fert2_sub, aes(x=year, y=fert)) + 
  geom_point() +
  facet_wrap(~country) +
  scale_x_continuous(breaks=seq(1950,2000,10), 
                     labels=c("1950", "60","70", "80","90","2000")) +
  geom_smooth(method="lm", se=FALSE)


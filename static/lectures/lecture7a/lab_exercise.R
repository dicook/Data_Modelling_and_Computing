# Lab solution code
library(tidyverse)
library(readxl)
fert <- read_xlsx("data/children_per_woman_total_fertility.xlsx") %>%
  rename(country = geo)
fert <- fert %>% gather(year, fert, -country) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year > 1950) %>%
  mutate(year1950 = year - 1950)
ggplot(fert, aes(x=year, y=fert, group=country)) +
  geom_line(alpha=0.1)

oz <- fert %>% filter(country=="Australia")
head(oz)
p1 <- ggplot(data=oz, aes(x=year, y=fert)) +
  geom_line()
oz_lm <- lm(fert~year1950, data=oz)
tidy(oz_lm)
oz_mod <- augment(oz_lm, oz)
p2 <- ggplot(data=oz_mod, aes(x=year, y=.fitted)) +
  geom_line()
p3 <- ggplot(data=oz_mod, aes(x=year, y=.std.resid)) +
  geom_hline(yintercept=0, colour="white", size=2) +
  geom_line()
grid.arrange(p1, p2, p3, ncol=3)

library(purrr)
by_country <- fert %>%
  select(country, year1950, fert) %>%
  group_by(country) %>%
  nest()
by_country <- by_country %>%
  mutate(
    model = purrr::map(data, ~ lm(fert ~ year1950,
                                  data = .))
  )
country_coefs <- by_country %>%
  unnest(model %>% purrr::map(broom::tidy))
country_coefs <- country_coefs %>%
  select(country, term, estimate) %>%
  spread(term, estimate) %>%
  rename(intercept = `(Intercept)`)
head(country_coefs)
country_coefs %>%
  filter(country == "Australia")
p <- ggplot(country_coefs, aes(x=intercept, y=year1950,
                               label=country)) +
  geom_point(alpha=0.5, size=2)
library(plotly)
ggplotly(p)

country_fit <- by_country %>%
  unnest(model %>%
           purrr::map(broom::glance))

ggplot(country_fit, aes(x=r.squared)) + geom_histogram()

badfit <- country_fit %>% filter(r.squared <= 0.15)
fert %>% filter(country %in% badfit$country) %>%
ggplot(aes(x=year, y=fert)) +
  geom_point() +
  facet_wrap(~country) +
  scale_x_continuous(breaks=seq(1950,2020,10),
                     labels=c("1950", "60","70", "80","90","2000", "10", "20")) +
  geom_smooth(method="lm", se=FALSE)

fert %>% filter(country=="Bangladesh") %>%
  ggplot(aes(x=year, y=fert)) +
  geom_line()

country_fit %>% filter(country=="Bangladesh")
country_coefs %>% filter(country=="Bangladesh")

library(tidyverse)
library(lubridate)
library(naniar)
library(visdat)
houses <- read_csv("data/Melbourne_housing_FULL.csv")
vis_dat(houses)
houses <- houses %>%
  mutate(Date = dmy(Date))

ggplot(houses, aes(x=Date, y=Price)) + geom_point() +
  geom_smooth(se=FALSE) + facet_wrap(~Type)
ggplot(filter(houses, Suburb=="Port Melbourne"),
       aes(x=Date, y=Price)) + geom_point() +
  geom_smooth(se=FALSE) + facet_wrap(~Type)

houses_sub <- houses %>% select(Price, Rooms, Type, Distance, Bedroom2, Bathroom)
vis_miss(houses_sub)

houses_sub <- houses_sub %>% bind_shadow()
ggplot(houses_sub, aes(x=Bedroom2, y=Bathroom,
                       colour=Bedroom2_NA)) +
  geom_point()
ggplot(houses_sub, aes(x=Bedroom2, y=Bathroom,
                       colour=Bathroom_NA)) +
  geom_point()

# Mean imputation
houses_sub <- houses %>% select(Price, Rooms, Type, Distance, Bedroom2, Bathroom)
houses_sub <- houses_sub %>% bind_shadow()
houses_sub <- houses_sub %>%
  mutate(Bedroom2 = impute_mean(Bedroom2),
         Bathroom = impute_mean(Bathroom))
ggplot(houses_sub, aes(x=Bedroom2, y=Bathroom,
                       colour=Bedroom2_NA)) +
  geom_point(alpha=0.5)
ggplot(houses_sub, aes(x=Bedroom2, y=Bathroom,
                       colour=Bathroom_NA)) +
  geom_point(alpha=0.5)

# Nearest neighbours doesn't work for this data
houses_bb <- houses_sub %>%
  select(Rooms, Bedroom2, Bathroom)
houses_bb <- impute.knn(as.matrix(houses_bb), 20)
houses_sub <- houses_sub %>%
  mutate(Bedroom2 = houses_bb$Bedroom2,
         Bathroom = houses_bb$Bathroom)
ggplot(houses_sub, aes(x=Bedroom2, y=Bathroom,
                       colour=Bedroom2_NA)) +
  geom_point(alpha=0.2)
ggplot(houses_sub, aes(x=Bedroom2, y=Bathroom,
                       colour=Bathroom_NA)) +
  geom_point(alpha=0.2)

# Regression model
houses_sub <- houses %>% select(Price, Rooms, Type, Distance, Bedroom2, Bathroom)
houses_sub <- houses_sub %>% bind_shadow()
br2 <- lm(Bedroom2~Rooms, data=houses_sub)
ba <- lm(Bathroom~Rooms, data=houses_sub)
houses_sub <- houses_sub %>%
  mutate(Bedroom2=ifelse(is.na(Bedroom2), predict(br2, new=houses_sub), Bedroom2),
         Bathroom=ifelse(is.na(Bathroom), predict(br2, new=houses_sub), Bathroom))
ggplot(houses_sub, aes(x=Bedroom2, y=Bathroom,
                       colour=Bedroom2_NA)) +
  geom_point(alpha=0.5)
ggplot(houses_sub, aes(x=Bedroom2, y=Bathroom,
                       colour=Bathroom_NA)) +
  geom_point(alpha=0.2)


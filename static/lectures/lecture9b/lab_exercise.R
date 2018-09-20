k <- 6
cntrs <- ru_sub_t[1:6,]
ru_km <- kmeans(ru_sub_t, cntrs, iter.max=50, nstart=5)
ru_cl <- tibble(curr=names(ru_km$cluster), cl=factor(ru_km$cluster))
ru_sub_long <- ru_sub %>%
  mutate(date=ru$date) %>%
  gather(curr, value, -date) %>%
  left_join(ru_cl, by="curr")
p <- ggplot(ru_sub_long, aes(x=date, y=value, group=curr, colour=cl, label=curr)) +
  geom_line() +
  facet_wrap(~cl)
library(plotly)
ggplotly(p)

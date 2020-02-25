proj_groups <- c("AHVCD",
  "Alfredo",
  "bomrang",
  "Bowerbird",
  "count",
  "drake",
  "ecc team",
  "fable",
  "free real estate",
  "gravitas",
  "HD_TEAM",
  "HDPLZ",
  "icon",
  "knit",
  "Lumos",
  "purrr",
  "rude recliners",
  "smurfs",
  "sugarbag",
  "suggrants",
  "tapian",
  "team analytica",
  "Team dyplr",
  "The Bobcats",
  "The Foodies",
  "visdat")

library(tidyverse)
set.seed(2019-10-17-2225)
talk_order <- 
tibble::tibble(proj_groups = sample(proj_groups),
               day =  rep(c("Wednesday", 
                            "Friday"),
                          times = length(proj_groups)/2,
                          out.length = length(proj_groups))) %>% 
  arrange(day)


talk_list <- talk_order %>% 
  split(.$day) %>% 
  map(~.["proj_groups"]) %>% 
  map(~ as.character(unlist(.x)))

talk_list$Friday

write_list <- function(input){
  paste0("-",input,collapse = "\n") %>% 
    clipr::write_clip()
}

write_list(talk_list$Wednesday)
write_list(talk_list$Friday)


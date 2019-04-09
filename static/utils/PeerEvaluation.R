library(googlesheets)
library(tidyverse)

gs_ls()
lss <- gs_key("1fpQO16KCB1aPyfM1LcaWySZuXia_S8UgeXP5kS9dgIU")
assignments <- gs_read(lss, ws=2)

# Match groups without self assignment
sampleGroups <- function(groups){
  match <- character(length(groups))
  remaining_groups <- groups
  for(i in seq_along(groups)){
    possible <- seq_along(remaining_groups)[!(remaining_groups %in% groups[i])]
    if(length(possible) == 0){ # Oops, this attempt failed, try again!
      return(sampleGroups(groups))
    }
    j <- sample(possible, 1)
    match[i] <- remaining_groups[j]
    remaining_groups <- remaining_groups[-j]
  }
  match
}

# Assign individuals to evaluate groups
assignments %>%
  filter(!is.na(GROUP)) %>%
  mutate(`Evaluation Group` = sampleGroups(GROUP)) %>%
  select(STUDENT_CODE, GROUP, `Evaluation Group`) %>%
  write_csv("evaluation_assignment.csv")

s <- read_csv("evaluation_assignment.csv")
s %>% count(`Evaluation Group`, sort=TRUE) %>% print(n=21)
s %>% count(`GROUP`, sort=TRUE) %>% print(n=21)

# Prepare submissions for peer evaluation (ZIP)

## Change to be where you would like your ZIPs to be placed
zippath <- "a2_groups/"
## List the directories that you'd like to ZIP
submissions <- list.dirs("Assignment 2 group submissions/", recursive = FALSE)

## Check that all groups have submitted assignments
assignments %>%
  mutate(match = Email %in% basename(submissions))%>%
  group_by(GROUP) %>%
  summarise(match = sum(match)) %>%
  print(n=22)

## Build the ZIPs
submissions %>%
  map(list.files, recursive = TRUE, full.names = TRUE) %>% # Find files to zip
  map2(submissions, function(...){
    # Find group name to be used for the zip file name
    group <- assignments %>% filter(Email == basename(..2)) %>% pull(GROUP)
    # Zip the files in the appropriate folder
    zip(file.path(zippath, group), files = ..1, extras = "-j")
  })

library(googlesheets)
library(tidyverse)

gs_ls()
lss <- gs_key("1bm0e4ctLbAsn72yDMKuT5V1iIV2wH_kD2bOWCZx0aOw")
assignments <- gs_read(lss)

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
  write_csv("~/github/Data_Modelling_and_Computing/static/evaluation_assignment.csv")

# Prepare submissions for peer evaluation (ZIP)

## Change to be where you would like your ZIPs to be placed
zippath <- "~/teaching/ETC1010/a3_groups/"
## List the directories that you'd like to ZIP
submissions <- list.dirs("~/teaching/ETC1010/Assignment 3 group submissions/", recursive = FALSE)

## Check that all groups have submitted assignments
assignments %>% 
  mutate(match = EMAIL_ADDRESS %in% basename(submissions))%>% 
  group_by(GROUP) %>% 
  summarise(any(match))

## Build the ZIPs
submissions %>% 
  map(list.files, recursive = TRUE, full.names = TRUE) %>% # Find files to zip
  map2(submissions, function(...){
    # Find group name to be used for the zip file name
    group <- assignments %>% filter(EMAIL_ADDRESS == basename(..2)) %>% pull(GROUP)
    # Zip the files in the appropriate folder
    zip(file.path(zippath, group), files = ..1, extras = "-j")
  })
library(googlesheets)
library(tidyverse)

gs_ls()
lss <- gs_key("KEYGOESHERE")
assignments <- gs_read(lss, ws = "assignment2") %>%
  select(STUDENT_CODE:group)

assignments %>%
  count(group) %>%
  count(n)

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
  
# tolower(assignments$GROUP) %>% clipr::write_clip()

# Assign individuals to evaluate groups
assignments %>%
  # replace NAs...
  # mutate(GROUP = str_replace_na(GROUP, replacement = "unallocated")) %>%
  filter(!is.na(group)) %>%
  mutate(`Evaluation Group` = sampleGroups(group)) %>%
  select(STUDENT_CODE, group, `Evaluation Group`) %>%
  write_csv("static/utils/evaluation_assignment_2.csv")

# Check that all groups have at leat two evaluators
s <- read_csv("static/utils/evaluation_assignment_2.csv")
s %>% count(`Evaluation Group`, sort=TRUE) %>% print(n = 30)
s %>% count(group, sort=TRUE) %>% print(n = 30)

# Prepare submissions for peer evaluation (ZIP)

## Change to be where you would like your ZIPs to be placed
zippath <- here::here(
  "static",
  "utils",
  "Assignment - ETC1010 Assignment 2, Semester 2 2019 submissions"
  )

## List the directories that you'd like to ZIP
# submissions <- list.dirs("Assignment 1 group submissions/", 
#                          recursive = FALSE)
submissions <- list.dirs(zippath,
                         recursive = FALSE)

## Check that all groups have submitted assignments
assignments %>%
  mutate(match = Email %in% basename(submissions))%>%
  group_by(GROUP) %>%
  summarise(match = sum(match)) %>%
  filter(match != 1) %>%
  arrange(match)

## Build the ZIPs
submissions %>%
  map(list.files, recursive = TRUE, full.names = TRUE) %>% # Find files to zip
  map2(submissions, function(...){
    # Find group name to be used for the zip file name
    group <- assignments %>% filter(Email == basename(..2)) %>% pull(GROUP)
    # Zip the files in the appropriate folder
    zip(file.path(zippath, group), files = ..1, extras = "-j")
  })

library(googlesheets)
library(tidyverse)

gs_ls()
lss <- gs_key("KEYGOESHERE")
assignment_2 <- gs_read(lss, ws = "assignment3") %>%
  select(STUDENT_CODE:group,
         -Title)

assignment_2 %>%
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
assignment_2 %>%
  # replace NAs...
  mutate(group = str_replace_na(group, replacement = "unallocated")) %>%
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
  "Assignment - Assignment 2 S2, 2019 submissions"
  )

## List the directories that you'd like to ZIP
# submissions <- list.dirs("Assignment 1 group submissions/", 
#                          recursive = FALSE)
submissions <- list.dirs(zippath,
                         recursive = FALSE)

## Check that all groups have submitted assignments
assignment_2 %>%
  mutate(match = Email %in% basename(submissions)) %>%
  group_by(group) %>%
  summarise(match = sum(match)) %>%
  filter(match != 1) %>%
  arrange(match)

## Build the ZIPs
submissions %>%
  map(list.files, 
      recursive = TRUE, 
      full.names = TRUE) %>% # Find files to zip
  map2(submissions, function(...){
    # Find group name to be used for the zip file name
    group <- assignment_2 %>% 
      filter(Email == basename(..2)) %>% 
      pull(group)
    # Zip the files in the appropriate folder
    zip(file.path(zippath, group), files = ..1, extras = "-j")
  })


## now prepare for rstudio.cloud

## List the directories ending in .zip
submissions_zip <- list.files(zippath,
                              pattern = "*.zip$",
                              recursive = FALSE,
                              full.names = TRUE)

# put these into a folder called "all submissions"
fs::dir_create(path = glue::glue("{zippath}/all_submissions"))
               
# move all zip files into all_submissions
library(glue)
map(.x = submissions_zip,
    .f = ~fs::file_move(path = .x,
                        new_path = glue(
                          "{zippath}/all_submissions/{basename(.x)}"
                          )
                        )
    )

all_zipped <- list.files(path = glue("{zippath}/all_submissions"),
                         full.names = TRUE)

# this didn't work for me, but you get the idea  
map(all_zipped, unzip)

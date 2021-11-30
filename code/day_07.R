# Day 7 in R

# Part 1 instructions ----
# How many bag colors can eventually contain at least one shiny gold bag? 
# (The list of rules is quite long; make sure you get all of it.)

# Input ----
library(tibble)
library(dplyr)
input <- readLines(here::here('input/day_07/input.txt'))
input <- tibble(x = input)

# Solve ----
# Each line is a rule
# Should we convert the rules into a directed network?

# source - first bag
# dest - all after "bags contain"

solution  <- input %>% 
  tidyr::separate(x, into=c("source","dest"), sep=" bags contain ") %>% 
  tidyr::separate_rows(dest, sep = ", ") %>% 
  mutate(dest = stringr::str_remove(" bags.| bags| bag.| bag", string = dest)) %>% 
  mutate(dest = stringr::str_remove("^[(0-9)] ", string = dest)) #%>%
  # mutate(target_dest = ifelse(dest == "shiny gold",TRUE,FALSE))

# Now find all the "source" with dest == "shiny gold"
all_source_bags <- c()
counter <- 1
current_dests <- c("shiny gold")
while (counter >0){
  current_sources <- solution$source[solution$dest %in% current_dests]
  all_source_bags <- c(all_source_bags, current_sources)
  counter <- length(current_sources)
  current_dests <- current_sources
}

length(unique(all_source_bags))
# 248 - CORRECT

# Part 2 ----
# How many individual bags are required inside your single shiny gold bag?

# need to add the numbers back in...
solution  <- input %>% 
  tidyr::separate(x, into=c("source","dest"), sep=" bags contain ") %>% 
  tidyr::separate_rows(dest, sep = ", ") %>% 
  mutate(dest = stringr::str_remove(" bags.| bags| bag.| bag", string = dest)) %>% 
  mutate(dest_n = stringr::str_extract("^[(0-9)] ", string = dest),
         dest_n = as.numeric(ifelse(is.na(dest_n), 0, dest_n))) %>% 
  mutate(dest = stringr::str_remove("^[(0-9)] ", string = dest)) %>% 
  filter(dest != "no other")

# Solve ----
all_dest_bags <- c()
all_dest_n <- 0
counter <- 1
current_sources <- c("shiny gold")
while (counter >0){
  # current_dests <- rep(solution$dest[solution$source %in% current_sources],
  #                      solution$dest_n[solution$source %in% current_sources])
  current_dests <- unlist(sapply(current_sources, function(x) 
    rep(solution$dest[solution$source == x],
                       solution$dest_n[solution$source == x])))
  # all_dest_n <- all_dest_n + 
  #   sum(solution$dest_n[solution$source %in% current_sources])
  all_dest_bags <- c(all_dest_bags, current_dests)
  counter <- length(current_dests)
  current_sources <- current_dests
}

length((all_dest_bags))

#57281 - correct!

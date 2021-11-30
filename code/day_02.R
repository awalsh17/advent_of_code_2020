# Day 2

# https://adventofcode.com/2020/day/2

# Part 1 instructions ----
# For example, suppose you have the following list:
#   
# 1-3 a: abcde
# 1-3 b: cdefg
# 2-9 c: ccccccccc
# Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.
# 
# In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.
# 
# How many passwords are valid according to their policies?

# Read input ----
input <- readr::read_delim(here::here('input/day_02/input.txt'), delim = " ",
                          col_names = c("num","letter","pw"))

# Solve ----

# Think we will need a check function
check_ps <- function(row){
  # Return TRUE/FALSE
  num <- row[1]
  letter <- row[2]
  pw <- row[3]
  pw_letter <- sub(":","",letter, fixed = T)
  num_split <- strsplit(num,"-",fixed = T)[[1]]
  min_l <- num_split[1]
  max_l <- num_split[2]
  pw_count <- stringr::str_count(pw, pw_letter)
  pw_count >=min_l & pw_count <=max_l
}
check_ps(c("1-3","a:","bcde"))

sum(apply(input, 1, check_ps))

# So that didn't work. Let's try adding columns
library(dplyr)

mod <- input %>% 
  tidyr::separate(num, into=c("min","max")) %>% 
  mutate(min = as.numeric(min),
         max = as.numeric(max),
         letter = sub(":","",letter)) %>% 
  mutate(count = stringr::str_count(pw, letter)) %>% 
  mutate(valid = count >= min & count <=max)

answer_part1 <- sum(mod$valid)

# The problem was not having numeric data types!

# Part 2 -----

# Each policy actually describes two positions in the password, where 1 means the first character, 2 means the second character, and so on. (Be careful; Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of these positions must contain the given letter. Other occurrences of the letter are irrelevant for the purposes of policy enforcement.
# 
# Given the same example list from above:
#   
# 1-3 a: abcde is valid: position 1 contains a and position 3 does not.
# 1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
# 2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.
# How many passwords are valid according to the new interpretation of the policies?

# Solve ----

mod_2 <- input %>% 
  tidyr::separate(num, into=c("min","max")) %>% 
  mutate(min = as.numeric(min),
         max = as.numeric(max),
         letter = sub(":","",letter)) %>% 
  mutate(check_first = substr(pw,min,min) == letter,
         check_second = substr(pw,max,max) == letter) %>% 
  mutate(valid = (check_first | check_second) & 
           !(check_first & check_second))

answer_part2 <- sum(mod_2$valid)

# Completed! ----
# Day 9 in R


# Input ----
input <- as.numeric(readLines(here::here('input/day_09/input.txt')))

# Next number is series must be sum of two of the prev. 25 numbers
# find the first number in the list (after the preamble) 
# which is not the sum of two of the 25 numbers before it. 
# What is the first number that does not have this property?

# Part 1 instructions ----
test_input <- c(
  35, 20,15, 25,47, 40,62, 55,65, 95,102,117,150,182,127,219,299,277,309,576)
# preamble of 5, considers prev 5 numbers
window <- c(0:4)
stop_signal <- TRUE
while (stop_signal){
  window <- window + 1
  current_n <- test_input[window[5]+1]
  all_sums <- tidyr::crossing(x=test_input[window],y=test_input[window])
  all_sums <- all_sums$x + all_sums$y
  if (!any(current_n %in% all_sums)){
    stop_signal <- FALSE
  }
}
current_n # answer value
  
# Solve ----

window <- c(0:24)
stop_signal <- TRUE
while (stop_signal){
  window <- window + 1
  current_n <- input[window[25]+1]
  all_sums <- tidyr::crossing(x=input[window],y=input[window])
  all_sums <- all_sums$x + all_sums$y
  if (!any(current_n %in% all_sums)){
    stop_signal <- FALSE
  }
}
answer_part1 <- current_n # answer value

# CORRECT

# Part 2 ----

# you must find a contiguous set of at least two numbers in your list which sum to the invalid number from step 1.

# To find the encryption weakness, add together the smallest and largest number in this contiguous range

# Solve ----

# could do this several ways
# try all possible windows, but what is most efficient way?

check_window_sum <- function(input, target_sum, window_length){
  window <- c(0:(window_length-1))
  stop_signal <- TRUE
  while (stop_signal){
    window <- window + 1
   
    if (window[window_length]>length(input)){
      # message("not correct window")
      stop_signal <- FALSE
      # get new number wanted
      return(NA)
    } else {
      current_sum <- sum(input[window])
      ## DEBUG
      # print(current_sum)
      ##
      if (current_sum==target_sum){
        stop_signal <- FALSE
        # get new number wanted
        message("correct window")
        return(max(input[window]) + min(input[window]))
      }
    }
    
  }
  
}
# test
check_window_sum(test_input, target_sum = 127, window_length = 4)

for (i in 12:22){
  print(i)
  check_window_sum(input, target_sum = answer_part1, window_length = i)
}

check_window_sum(input, target_sum = answer_part1, window_length = 17)
# I got this correct, but this was really sloppy and terrible

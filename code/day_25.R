# Day 25 in R

# Part 1 instructions ----


# Input ----
input <- c(17115212,3667832)
test_input <- c(5764801,17807724)

# Solve ----

door_transform <- function(loop_size, subject_n){
  value_i = 1
  for (i in 1:loop_size){
    value_i = value_i * subject_n
    value_i = value_i %% 20201227
  }
  value_i
}

find_loop <- function(target, subject_n){
  value_i = 1
  i = 0
  stop_cond <- TRUE
  while (stop_cond){
    i <- i + 1
    value_i = value_i * subject_n
    value_i = value_i %% 20201227
    if (value_i == target){
      # stop(paste("card loop size is:",i))
      stop_cond <- FALSE
    }
    
  }
  return(i)
}

#Tests
door_transform(8,7)
find_loop(test_input[1],7)

# Get loop size
find_loop(input[1],7)
# 8217635
find_loop(input[2],7)
# 20035918

# Get encryption key
door_transform(8217635,input[2])
door_transform(20035918,input[1])
# 15467093 - CORRECT

# Part 2 ----

# Solve ----


# Day 23 (R)

# Part 1 instructions ----

# Using your labeling, simulate 100 moves. 
# What are the labels on the cups after cup 1?
# Your puzzle input is 364297581.
# Input ----
library(tibble)
library(dplyr)

input <- as.character(389125467) #389125467 is test, real: 364297581
input_v <- as.numeric(strsplit(input,"")[[1]])
# Solve ----
current_cup_index <- 1
current_cup <- input_v[current_cup_index]
current_circle <- input_v
for (i in 1:100){ #UPDATE TO 100
  print(paste("current cup ", current_cup)) ## COMMENT
  print(current_circle) ## COMMENT
  
  # Temp make circle start at current cup
  if (current_cup_index != 1){
    temp_circle <- c(
      current_circle[current_cup_index:9],
      current_circle[1:(current_cup_index-1)]
    )
  } else {
    temp_circle <- current_circle
  }

  pick_up_index <- c(2:4)#rep(1:9,2)[seq(current_cup_index+1,current_cup_index+3)]
  picked_cups <- temp_circle[pick_up_index]
  print(picked_cups) ## COMMENT
  temp_circle <- temp_circle[-pick_up_index]
  # print(current_circle) ## COMMENT
  if (any(temp_circle<current_cup)){
    dest_cup_index <- which(temp_circle == max(
      temp_circle[temp_circle<current_cup]), 
                            arr.ind = T)
  } else {
    dest_cup_index <- which(temp_circle == max(temp_circle), arr.ind = T)
  }
  
  # print(dest_cup_index) ## COMMENT
  # If dest_cup_index is before current_cup_index
  c_l <- 6
  temp_circle <- c(temp_circle[1:dest_cup_index],
                        picked_cups, 
                   temp_circle[(dest_cup_index+1):c_l])
  temp_circle <- temp_circle[1:length(input_v)]
  # Re-translate to the regular circle
  if (current_cup_index != 1){
    current_circle <- c(
      temp_circle[(11-current_cup_index):9],
      temp_circle[1:(10-current_cup_index)]
      )
    } else {
      current_circle <- temp_circle
    }
  current_cup_index <- current_cup_index + 1
  if (current_cup_index>9){current_cup_index <- 1}
  print(paste("new cup index: ",current_cup_index)) ## COMMENT
  current_cup <- current_circle[current_cup_index]
}
current_circle
# re-arrange after the 1 to get the final answer
# 47382659 - correct!!

# Part 2 ----
# Lol - no way
# For example, if your labeling were 54321, 
# the cups would be numbered 5, 4, 3, 2, 1, 
# and then start counting up from 6 until one million is reached.

#the crab is going to do ten million (10000000) moves!

# two cups that will end up immediately clockwise of cup 1

# Need to not have the whole 1e6 vector... just use some math...

# Solve ----
input <- as.character(364297581) #389125467 is test, real: 364297581
input_v <- as.numeric(strsplit(input,"")[[1]])
# so digits 10 to 1e6 are just 10-1e6
input_v <- c(input_v, c(seq(10,1e5)))

current_cup_index <- 1
current_cup <- input_v[current_cup_index]
current_circle <- input_v
val_1 <- 0
val_2 <- 0
max_length <- 1e6
test_length <- length(input_v)
for (i in 1:50000){ #UPDATE?
  
  # print(paste("current cup ", current_cup)) ## COMMENT
  # print(current_circle) ## COMMENT
  
  # Temp make circle start at current cup
  if (current_cup_index != 1){
    temp_circle <- c(
      current_circle[current_cup_index:test_length],
      current_circle[1:(current_cup_index-1)]
    )
  } else {
    temp_circle <- current_circle
  }
  
  pick_up_index <- c(2:4)
  picked_cups <- temp_circle[pick_up_index]
  # print(picked_cups) ## COMMENT
  temp_circle <- temp_circle[-pick_up_index]

  if (any(temp_circle<current_cup)){
    dest_cup_index <- which(temp_circle == max(
      temp_circle[temp_circle<current_cup]), 
      arr.ind = T)
  } else {
    dest_cup_index <- which(temp_circle == max(temp_circle), arr.ind = T)
  }
  
  # print(dest_cup_index) ## COMMENT
  # If dest_cup_index is before current_cup_index
  c_l <- test_length - 3
  temp_circle <- c(temp_circle[1:dest_cup_index],
                   picked_cups, 
                   temp_circle[(dest_cup_index+1):c_l])
  temp_circle <- temp_circle[1:length(input_v)]
  # Re-translate to the regular circle
  if (current_cup_index != 1){
    current_circle <- c(
      temp_circle[((test_length+2)-current_cup_index):test_length],
      temp_circle[1:((test_length+1)-current_cup_index)]
    )
  } else {
    current_circle <- temp_circle
  }
  current_cup_index <- current_cup_index + 1
  if (current_cup_index>test_length){current_cup_index <- 1}
  # print(paste("new cup index: ",current_cup_index)) ## COMMENT
  current_cup <- current_circle[current_cup_index]
  
  index_1 <- which(current_circle==1, arr.ind = T)
  index_plus2 <- index_1+2
  if (index_1 == test_length) {
    index_1 <- 0
    index_plus2 <- 2
    }
  if (index_1 == (test_length-1)) {
    index_plus2 <- 1
    }
  if (current_circle[index_1+1] != val_1){
    val_1 <- current_circle[index_1+1]
  }
  if ((current_circle[index_plus2] != val_2)){
    val_2 <- current_circle[index_plus2]
    print (paste("round:", i)) ## COMMENT
    print(paste("after 1: ",
                paste(val_1, 
                      val_2, 
                      collapse = ", ")))
  }

  
}

# Test answer is 934001 * 159792 = 149245887792.

# real answer is ??

# OK -So this was a fail. Instead should implement a structure
# Where each value:location is stored and updated.

named_v <- c(4,5,1,7,6,8,9,2,3)
names(named_v) <- c(1:9)


current_cup_index <- 1
test_length <- length(input_v)
current_circle <- input_v
current_cup <- current_circle[current_cup_index]
for (i in 1:10){ #UPDATE?
  temp_circle <- c(current_circle,current_circle)
  
  # Get picked up cups
  pick_up_index <- c((current_cup_index+1): (current_cup_index+3))
  picked_cups <- temp_circle[pick_up_index]
  # print(picked_cups) ## COMMENT

  # Find destination
  if (any(temp_circle[-pick_up_index]<current_cup)){
    dest_cup_index <- which(temp_circle == max(
      temp_circle[temp_circle<current_cup]), 
      arr.ind = T)
  } else {
    dest_cup_index <- which(temp_circle == max(temp_circle), arr.ind = T)
  }
  
  # UPDATE
  
  
  # If dest_cup_index is before current_cup_index
  c_l <- test_length - 3
  temp_circle <- c(temp_circle[1:dest_cup_index],
                   picked_cups, 
                   temp_circle[(dest_cup_index+1):c_l])
  temp_circle <- temp_circle[1:length(input_v)]
  # Re-translate to the regular circle
  if (current_cup_index != 1){
    current_circle <- c(
      temp_circle[((test_length+2)-current_cup_index):test_length],
      temp_circle[1:((test_length+1)-current_cup_index)]
    )
  } else {
    current_circle <- temp_circle
  }
  current_cup_index <- current_cup_index + 1
  if (current_cup_index>test_length){current_cup_index <- 1}
  # print(paste("new cup index: ",current_cup_index)) ## COMMENT
  current_cup <- current_circle[current_cup_index]
  
  index_1 <- which(current_circle==1, arr.ind = T)
  index_plus2 <- index_1+2
  if (index_1 == test_length) {
    index_1 <- 0
    index_plus2 <- 2
  }
  if (index_1 == (test_length-1)) {
    index_plus2 <- 1
  }
  if (current_circle[index_1+1] != val_1){
    val_1 <- current_circle[index_1+1]
  }
  if ((current_circle[index_plus2] != val_2)){
    val_2 <- current_circle[index_plus2]
    print (paste("round:", i)) ## COMMENT
    print(paste("after 1: ",
                paste(val_1, 
                      val_2, 
                      collapse = ", ")))
  }
  
  
}
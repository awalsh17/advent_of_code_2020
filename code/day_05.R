# Day 5

# Part 1 instructions ----
# 128 rows on the plane (numbered 0 through 127)
# 8 columns of seats on the plane (numbered 0 through 7)
# Every seat also has a unique seat ID: multiply the row by 8, then add the column. In this example, the seat has ID 44 * 8 + 5 = 357.

# Input ----
input <- readLines(here::here('input/day_05/input.txt'))

# Solve ----
# Need a function
get_row <- function(data){
  this_row <- c(0:127)
  for (i in 1:7){
    current_char <- substr(data, i,i)
    if(current_char=="F"){
      this_row <- c(min(this_row):(floor(median(this_row))))
    } else{
      this_row <- c(ceiling(median(this_row)):max(this_row))
    }
    # print(i)
    # print(this_row)
  }
  this_row
}

get_col <- function(data){
  this_col <- c(0:7)
  for (i in 8:10){
    current_char <- substr(data, i,i)
    if(current_char=="L"){
      this_col <- c(min(this_col):(floor(median(this_col))))
    } else{
      this_col <- c(ceiling(median(this_col)):max(this_col))
    }
    # print(i)
    # print(this_col)
  }
  this_col
}

get_id <- function(row, col){row*8 + col}

get_id(get_row("BFFFBBFRRR"),get_col("BFFFBBFRRR"))

all_ids <- sapply(input, function(x) get_id(get_row(x),get_col(x)))

max(all_ids) # correct 994

# Part 2 ----
# Your seat wasn't at the very front or back, though; the seats with IDs +1 and -1 from yours will be in your list.

# What is the ID of your seat?

# Solve ----

setdiff(c(min(all_ids):max(all_ids)), all_ids)

#741 #Correct!

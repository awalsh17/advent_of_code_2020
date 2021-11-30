# Day 3

# Part 1 instructions ----
# Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?

# Input ----
input <- readLines(here::here('input/day_03/input.txt'))

# Solve ----
# First idea:
# Loop thru all the lines, 
# Track where you are
# count tree or no tree
n_tree <- 0
place <- 1
for (i in input){
  if (place >31){
    place <- place - 31
  }
  current_char <- substr(i,place,place)
  print(current_char)
  if (current_char == "#"){
    n_tree <- n_tree + 1
  }
  place <- place+3
}
n_tree
# Part 2 ----

# Right 1, down 1.
# Right 3, down 1. (This is the slope you already checked.)
# Right 5, down 1.
# Right 7, down 1.
# Right 1, down 2.
# In the above example, these slopes would find 2, 7, 3, 4, and 2 tree(s) respectively; multiplied together, these produce the answer 336.
# 
# What do you get if you multiply together the number of trees encountered on each of the listed slopes?

# Solve ----
# Ok seems like a fxn would be a good idea
count_trees <- function(input, right, down){
  n_tree <- 0
  place_across <- 1
  row_down <- 1
  while(row_down<=length(input)){
  # for (i in input){
    if (place_across >31){
      place_across <- place_across - 31
    }
    current_char <- substr(input[row_down],place_across,place_across)
    if (current_char == "#"){
      n_tree <- n_tree + 1
    }
    place_across <- place_across+right
    row_down <- row_down+down
  }
  return(n_tree)
}
# test
count_trees(input, 1, 1)
count_trees(input, 3, 1)
count_trees(input, 5, 1)
count_trees(input, 7, 1)
count_trees(input, 1, 2)

answer <- 65*237*59*61*38

# Day 1

# Instructions ----
# --- Day 1: Report Repair ---
# find the two entries that sum to 2020 and then multiply those two numbers together.
# 
# For example, suppose your expense report contained the following:
# 
# 1721
# 979
# 366
# 299
# 675
# 1456
# In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying them together produces 1721 * 299 = 514579, so the correct answer is 514579.
# 
# Of course, your expense report is much larger. Find the two entries that sum to 2020; what do you get if you multiply them together?

# Puzzle input ----
# https://adventofcode.com/2020/day/1/input

input <- as.numeric(readLines(here::here('input/day_01/input.txt')))

# Solve ----

# find all pairwise sums
# there is probably a much fancier way
sums <- matrix(data=rep(0, length(input)^2), nrow=length(input), ncol=length(input))
for (i in 1:length(input)){
  for (j in 1:length(input)){
    sums[i,j] <- input[i]+input[j]
  }
}
# find the positions
which(sums == 2020, arr.ind = T)
# Now multiply them
answer_part1 <- input[which(sums == 2020, arr.ind = T)[1,1]] * 
  input[which(sums == 2020, arr.ind = T)[1,2]]

answer_part1  # correct - 365619

# Part two ----
# In your expense report, what is the product of the three entries that sum to 2020?

# Solve ----
# So doing something general here with a fxn seems smart...

for (i in 1:length(input)){
  for (j in 1:length(input)){
    for (k in 1:length(input)){
    sum_ijk <- input[i]+input[j]+input[k]
    if (sum_ijk == 2020){
      desired_indices <- c(i,j,k)
    }
    }
  }
}

# find the positions
desired_indices

# Now multiply them
answer_part2 <- input[desired_indices[1]] * 
  input[desired_indices[2]] * 
  input[desired_indices[3]] 

answer_part2 # 236873508

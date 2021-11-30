# Day 1 - in Python

# Puzzle input ----
input_path = 'input/day_01/input.txt'
input = open(input_path,"r").read().splitlines()

# Solve----
for i in input:
  for j in input:
    if (int(i)+int(j)==2020):
      print(int(i)*int(j))
      break
    
   

# 365619

# Solve part 2 -----
for i in input:
  for j in input:
    for k in input:
      if (int(i)+int(j)+int(k)==2020):
        print(int(i)*int(j)*int(k))
        break

# 236873508 correct answer

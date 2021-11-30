# Day 2 - in Python

# Puzzle input ----
input_path = 'input/day_02/input.txt'
input = open(input_path,"r").read().splitlines()

# Solve----
tally_valid = 0
for line in input:
  line_sep = line.split(" ")
  min_l = int(line_sep[0].split("-")[0])
  max_l = int(line_sep[0].split("-")[1])
  lettr = line_sep[1].split(":")[0]
  pw = line_sep[2]
  if (pw.count(lettr) <=max_l) and (pw.count(lettr) >=min_l):
    tally_valid+=1

print(tally_valid)

# 582

# Part 2 ----

tally_valid = 0
for line in input:
  line_sep = line.split(" ")
  min_l = int(line_sep[0].split("-")[0])
  max_l = int(line_sep[0].split("-")[1])
  lettr = line_sep[1].split(":")[0]
  pw = line_sep[2]
  if (pw[min_l]==lettr or pw[max_l==lettr]) \
  and not (pw[min_l]==lettr and pw[max_l==lettr]):
    tally_valid+=1

print(tally_valid)



# Day 6 in Python

# Input
input_path = 'input/day_06/input.txt'
input = open(input_path,"r").read().splitlines()

# Solve -----
input.append('')
counter = 0
allqs = ''
n_qs = 0
for line in input:
  if line == "":
    counter+=n_qs
    print("counter is "+str(counter))
    allqs = ''
    n_qs = 0
  else:
    allqs+=line
    n_qs = len(''.join(set(allqs)))
    # print(n_qs)
  
# 6441 was the final answer - wrong (low)
# I think this missed the last line!!
# fixed by adding '' to end
# 6443 was correct!

# Part 2 ----
# You don't need to identify the questions to which \
# anyone answered "yes"; you need to identify the questions \
# to which everyone answered "yes"!

counter = 0
allqs = set('abcdefghijklmnopqrstuvwxyz')
n_qs = 0
for line in input:
  if line == "":
    counter+=n_qs
    print("counter is "+str(counter))
    allqs = set('abcdefghijklmnopqrstuvwxyz')
    n_qs = 0
  else:
    allqs = allqs.intersection(set(line))
    n_qs = len(allqs)

# 3232 - CORRECT!

  

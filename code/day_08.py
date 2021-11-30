# Day 8 in Python

# 
import copy

# Input
input_path = 'input/day_08/input.txt'
input = open(input_path,"r").read().splitlines()

# Test input
# input = ['nop +0','acc +1','jmp +4','acc +3','jmp -3','acc -99','acc +1','jmp -4','acc +6']

# Solve -----
# strange infinite loop in the boot code (your puzzle input)
# The boot code is represented as a text file with one instruction per line of text
# Each instruction consists of an operation (acc, jmp, or nop) 
# and an argument (a signed number like +4 or -20)

# Immediately before any instruction is executed a second time
# what value is in the accumulator?


accumulator = 0
all_commands = []
i = 0
while i < len(input):
	if i in all_commands:
		break
	elif input[i][0:3] == 'nop':
		# do nothing
		all_commands.append(i)
		i+=1
	elif input[i][0:3] == 'acc':
		all_commands.append(i)
		accumulator = accumulator + int(input[i][4:])
		i+=1
	elif input[i][0:3] == 'jmp':
		all_commands.append(i)
		i = i + int(input[i][4:])

	# print("accumulator is: " + str(accumulator)) ## DEBUG
	# print("i is: " + str(i)) ## DEBUG
	# print("n all_commands is: " + str(len(all_commands))) ## DEBUG

print(str(accumulator))
# 1816 - correct

# Part 2 ----
# Somewhere in the program, 
# either a jmp is supposed to be a nop, 
# or a nop is supposed to be a jmp.

# Fix the program so that it terminates normally 
# by changing exactly one jmp (to nop) or nop (to jmp). 
# What is the value of the accumulator after the program terminates?

# Create a run function
# then test all possible jmp to nop and nop to jmp
print("PART 2:\n")

def test_code(mod_input):
	accumulator = 0
	all_commands = []
	i = 0
	while i < len(mod_input):
		if i in all_commands:
			break
		elif mod_input[i][0:3] == 'nop':
			# do nothing
			all_commands.append(i)
			i+=1
		elif mod_input[i][0:3] == 'acc':
			all_commands.append(i)
			accumulator = accumulator + int(mod_input[i][4:])
			i+=1
		elif mod_input[i][0:3] == 'jmp':
			all_commands.append(i)
			i = i + int(mod_input[i][4:])
	if i < len(mod_input):
		accumulator = 'broken'
	return accumulator


for j in list(range(1,len(input)-1)):
	mod_input = copy.deepcopy(input)
	if (input[j][0:3] == 'nop'):
		mod_input[j] = 'jmp' + mod_input[j][4:]
		# RUN FUNCTION
		acc = test_code(mod_input)
		print(str(acc))
		if acc != 'broken':
			print(str(acc))
	elif (input[j][0:3] == 'jmp'):
		mod_input[j] = 'nop' + mod_input[j][4:]
		# RUN FUNCTION
		acc = test_code(mod_input)
		print(str(acc))
		if acc != 'broken':
			print(str(acc))

# EnD
# Part 2 was also correct!

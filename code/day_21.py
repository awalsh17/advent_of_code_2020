# Day 21 in Python

import itertools
flatten = itertools.chain.from_iterable
# Determine which ingredients cannot possibly contain any of the allergens in your list. 
# How many times do any of those ingredients appear?
# Input
input_path = 'input/day_21/input.txt'
input = open(input_path,"r").read().splitlines()

# Solve -----
# Dictionary of allergens to ingredient sets?
# allow us to map the ingredients to allergens 
# find the set with no allergens
# count those

test_input = ['mxmxvkd kfcds sqjhc nhms (contains dairy, fish)','trh fvjkl sbzzf mxmxvkd (contains dairy)','sqjhc fvjkl (contains soy)','sqjhc mxmxvkd sbzzf (contains fish)']

clean_input = [[set(line[0].split(' ')),line[1].rstrip(')').split(', ')] for line in [line.split(' (contains ') for line in input]]

all_allergens = [l[1] for l in clean_input]#['dairy','fish','soy']
all_allergens = set(list(flatten(all_allergens)))

all_ingredients = [l[0] for l in clean_input]
all_ingredients = set(list(flatten(all_ingredients)))

all_ingredients_int = dict()
for i in all_allergens:
	all_ingredients_int[i] = set([])
	for j in clean_input:
		if i in j[1]:
			if (len(all_ingredients_int[i])==0):
				all_ingredients_int[i] = j[0]
			else:
				all_ingredients_int[i] = all_ingredients_int[i].intersection(j[0])


while sum([len(value) for key,value in all_ingredients_int.items()])>len(all_ingredients_int):
	for i in all_allergens:
		if len(all_ingredients_int[i]) == 1:
			remove_ingredient = list(all_ingredients_int[i])
			for key,value in all_ingredients_int.items():
				if key !=i:
					value = value.discard(remove_ingredient[0])

bad_ingredients = [list(value)[0] for key,value in all_ingredients_int.items()]
print(all_ingredients_int)
print(bad_ingredients)

good_ingredients = all_ingredients.difference(set(bad_ingredients))
print(good_ingredients)

# Need to count the number of times any of these appear
answer_part1 = 0
for i in good_ingredients:
	for j in clean_input:
		if i in j[0]:
			answer_part1+=1

print(answer_part1)

# Correct!

############
# PART 2 ###
############

# Arrange the ingredients alphabetically by their allergen
# separate them by commas to produce your canonical dangerous ingredient list. 

for key in sorted(all_ingredients_int):
	print("%s," % (list(all_ingredients_int[key])[0]))
# END - correct

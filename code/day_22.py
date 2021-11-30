# Day 22 in Python

import numpy as np
import copy
# Input
input_path = 'input/day_22/input.txt'
input = open(input_path,"r").read().splitlines()
player = [[],[]]
player_n = -1
for line in input:
  if line[0:6] == "Player":
    player_n += 1
  elif line == "":
    print("blank")    
  else:
    player[player_n].append(int(line))

# Solve -----

# split the cards so each player has their own deck (your puzzle input). 
# Then, the game consists of a series of rounds: 
# both players draw their top card, 
# and the player with the higher-valued card wins the round. 
# The winner keeps both cards, 
# placing them on the bottom of their own deck 
# so that the winner's card is above the other card. 
# If this causes a player to have all of the cards, 
# they win, and the game ends.

deck = copy.deepcopy(player)
n_1 = len(deck[0])
n_2 = len(deck[1])
while(n_1>0 and n_2 >0):
  top_cards = [deck[0][0],deck[1][0]]
  winner = np.argmax(top_cards)
  loser = np.argmin(top_cards)
  top_cards.sort(reverse=True)
  # Update decks
  deck[0] = deck[0][1:]
  deck[1] = deck[1][1:]
  deck[winner] = deck[winner] + top_cards
  n_1 = len(deck[0])
  n_2 = len(deck[1])
  # print(str(n_1) + str(n_2))
  
# Now score - top =50, bottom=1
score_v = list(range(1,51))
score_v.reverse()
print("part 1 answer")

print(sum([x*y for x,y in zip(score_v,deck[0])]))

# Got 30138 - correct!



# Part 2 ----


def play_round(deck):
  n_1 = len(deck[0])
  n_2 = len(deck[1])
  i = 1
  all_decks = []
  while(n_1>0 and n_2 >0):
    
    ## DEBUG
    # print (str(i))
    # print("deck 1 size = ", str(n_1))
    # print("deck 1: ", deck[0])
    # print("deck 2: ", deck[1])
    # print("all decks: ", all_decks)
    ##
    # Check if this deck appeared
    
    if deck in all_decks:
      break
      print("player 1 wins by default")

    all_decks.append(copy.deepcopy(deck))

    top_cards = [deck[0][0],deck[1][0]]
    deck[0] = deck[0][1:]
    deck[1] = deck[1][1:]
  
    if (top_cards[0]<=len(deck[0]) and \
    top_cards[1]<=len(deck[1])):
      # print("trigger sub game")
      mini_deck = copy.deepcopy(deck)
      mini_deck[0] = mini_deck[0][0:top_cards[0]]
      mini_deck[1] = mini_deck[1][0:top_cards[1]]
      sub_deck = play_round(mini_deck)
      if (len(sub_deck[0])==0):
        winner = 1
        loser = 0
      else:
        winner = 0
        loser = 1
    else:
      winner = np.argmax(top_cards)
      loser = np.argmin(top_cards)
    
    top_cards = [top_cards[winner],top_cards[loser]]
  
    # Update decks
    deck[winner] = deck[winner] + top_cards
    n_1 = len(deck[0])
    n_2 = len(deck[1])
    
    i+=1

  return deck
  

##############
# Test input #
##############
test_deck = [[9, 2, 6, 3, 1],[5, 8, 4, 7, 10]]
final_deck = play_round(test_deck)

print(final_deck)

print("test deck answer (291)")
score_v = list(range(1,11))
score_v.reverse()
print(sum([x*y for x,y in zip(score_v,final_deck[1])]))


############
# PART 2 ###
############

# deck = [[43,19],[2,29,14]]
deck = copy.deepcopy(player)
final_deck = play_round(deck)

print(final_deck)

print("part 2 answer")

score_v = list(range(1,(1+len(final_deck[0]))))
score_v.reverse()
print(sum([x*y for x,y in zip(score_v,final_deck[0])]))

# 39439 - was too high!!
# 25742 - was too low
# 31587 correcrt
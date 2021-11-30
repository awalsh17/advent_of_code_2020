# Day 24 in R

library(dplyr)
# Part 1 instructions ----
# a list of the tiles that need to be flipped over (your puzzle input)
# They start with the white side facing up
# Each line in the list identifies a single tile that needs to be flipped
# by giving a series of steps starting from a reference tile in the very center of the room. (Every line starts from the same reference tile.)
# every tile has six neighbors: east, southeast, southwest, west, northwest, and northeast

# how many tiles are left with the black side up?

# Input ----
input <- readLines(here::here('input/day_24/input.txt'))

test <- c(
  "sesenwnenenewseeswwswswwnenewsewsw",
  "neeenesenwnwwswnenewnwwsewnenwseswesw",
  "seswneswswsenwwnwse",
  "nwnwneseeswswnenewneswwnewseswneseene",
  "swweswneswnenwsewnwneneseenw",
  "eesenwseswswnenwswnwnwsewwnwsene",
  "sewnenenenesenwsewnenwwwse",
  "wenwwweseeeweswwwnwwe",
  "wsweesenenewnwwnwsenewsenwwsesesenwne",
  "neeswseenwwswnwswswnw",
  "nenwswwsewswnenenewsenwsenwnesesenew",
  "enewnwewneswsewnwswenweswnenwsenwsw",
  "sweneswneswneneenwnewenewwneswswnese",
  "swwesenesewenwneswnwwneseswwne",
  "enesenwswwswneneswsenwnewswseenwsese",
  "wnwnesenesenenwwnenwsewesewsesesew",
  "nenewswnwewswnenesenwnesewesw",
  "eneswnwswnwsenenwnwnwwseeswneewsenese",
  "neswnwewnwnwseenwseesewsenwsweewe",
  "wseweeenwnesenwwwswnew"
)

# Solve ----
# No idea really
# make a function to find the location of each line
# Use cube coordinates c(x,y,z)
parse_line <- function(input){
  directions <- c("e", "se", "sw", "w", "nw", "ne")
  cur_line <- input
  cur_pos <- c(0,0,0)
  while (stringr::str_length(cur_line)>0){
    ## DEBUG
    # print(paste("length: ",stringr::str_length(cur_line)))
    # print(paste("line: ",cur_line))
    ##
    cur_len <- stringr::str_length(cur_line)
    #update line and pos
    if (substr(cur_line,1,1) == "e"){
      cur_pos[1] <- cur_pos[1]+1
      cur_pos[2] <- cur_pos[2]-1
      cur_line <- substr(cur_line,2,cur_len)
    } else if (substr(cur_line,1,1) == "w"){
      cur_pos[1] <- cur_pos[1]-1
      cur_pos[2] <- cur_pos[2]+1
      cur_line <- substr(cur_line,2,cur_len)
    } else if (substr(cur_line,1,2) == "se"){
      cur_pos[2] <- cur_pos[2]-1
      cur_pos[3] <- cur_pos[3]+1
      cur_line <- substr(cur_line,3,cur_len)
    } else if (substr(cur_line,1,2) == "sw"){
      cur_pos[1] <- cur_pos[1]-1
      cur_pos[3] <- cur_pos[3]+1
      cur_line <- substr(cur_line,3,cur_len)
    } else if (substr(cur_line,1,2) == "nw"){
      cur_pos[2] <- cur_pos[2]+1
      cur_pos[3] <- cur_pos[3]-1
      cur_line <- substr(cur_line,3,cur_len)
    } else if (substr(cur_line,1,2) == "ne"){
      cur_pos[1] <- cur_pos[1]+1
      cur_pos[3] <- cur_pos[3]-1
      cur_line <- substr(cur_line,3,cur_len)
    }
    
  }
  return(cur_pos)
}
# Test this one
parse_line("nwwswee")

# Test answer
answer = tibble::tibble(x = lapply(test, parse_line))
answer$y = unlist(lapply(answer$x, function(x) paste(x, collapse = "_")))

# Need to know how many tiles are in this list an odd or even number of times

answer %>% 
  count(y) %>% 
  count(n)

# CORRECT

# real answer
answer = tibble::tibble(x = lapply(input, parse_line))
answer$y = unlist(lapply(answer$x, function(x) paste(x, collapse = "_")))

# Need to know how many tiles are in this list an odd or even number of times

answer %>% 
  count(y) %>% 
  count(n)

# 473 -- CORRECT

# Part 2 ----
# How many tiles will be black after 100 days?

# Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
# Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.

# Solve ----
# Need to update the other one to also output color

# answer <-  tibble::tibble(coord = lapply(test, parse_line))
answer <-  tibble::tibble(coord = lapply(input, parse_line))
answer$y <-  unlist(lapply(answer$coord, function(x) paste(x, collapse = "_")))
answer <- answer %>% 
  add_count(y) %>% 
  mutate(color = ifelse(n==1,"black","white")) 

answer_2 <- answer %>% 
  distinct(y,color) %>%
  tidyr::separate(y, into=c("x","y","z"), sep = "_") %>% 
  mutate(x = as.numeric(x),
         y = as.numeric(y),
         z = as.numeric(z)) 
  # There could be some missing in the sequence
missing_x <- setdiff(seq(min(answer_2$x),max(answer_2$x)),answer_2$x)
missing_y <- setdiff(seq(min(answer_2$y),max(answer_2$y)),answer_2$y)
missing_z <- setdiff(seq(min(answer_2$z),max(answer_2$z)),answer_2$z)
answer_2 <- answer_2 %>% 
  bind_rows(tibble(x=0,y=missing_y,z=0,color="white")) %>% 
  mutate(color = dplyr::if_else(is.na(color),"white",color))
# Want to add in the n_white and n_black for each based on its neighbors  
  
# Function to flip
# Takes in answer df the coordinates as list
flip_line <- function(input, n_days){
  # for each tile - count the black tiles around it
  input$orig_color <- input$color
  for (j in 1:n_days){
    ##
    print(j)
    ##
    # Only need the black tiles
    input <- dplyr::filter(input, color=="black")
    # Before going through each tile - need to add all neighbor tiles!
    new_input <- input[,1:3]
    for (i in 1:nrow(input)){
      row <- input[i,]
      neighbors <- rbind(c(x=row$x + 1,y=row$y-1,z= row$z),
                         c(x=row$x + 1,y=row$y,z= row$z-1),
                         c(x=row$x,    y=row$y+1,z= row$z-1),
                         c(x=row$x - 1,y=row$y+1,z= row$z),
                         c(x=row$x - 1,y=row$y,z = row$z+1),
                         c(x=row$x,    y=row$y-1,z = row$z+1))
      new_input <- unique(rbind(new_input,neighbors))
    }
    # Update input
    input <- left_join(new_input, input, by=c("x","y","z"))
    input$color <- ifelse(is.na(input$color),"white",input$color)
    ##
    # print(dim(input))
    ##
    new_color_v <- rep(NA,nrow(input))
    for (i in 1:nrow(input)){
      row <- input[i,]

      neighbors <- rbind(filter(input,
                                x==row$x + 1,
                                y==row$y-1,
                                z== row$z),
                         filter(input,
                                x==row$x + 1,
                                y==row$y,
                                z== row$z-1),
                         filter(input,
                                x==row$x,
                                y==row$y+1,
                                z== row$z-1),
                         filter(input,
                                x==row$x - 1,
                                y==row$y+1,
                                z== row$z),
                         filter(input,
                                x==row$x - 1,
                                y==row$y,
                                z == row$z+1),
                         filter(input,
                                x==row$x ,
                                y==row$y-1,
                                z == row$z+1)
      )
      # print(neighbors)
      n_white <- sum(neighbors$color =="white")
      n_black <- sum(neighbors$color =="black")
      if (row$color=="black" & (n_black==0 | n_black>2)){
        new_color_v[i] <- "white"
      } else if (row$color=="white" & (n_black==2)){
        new_color_v[i] <- "black"
      } else {
        new_color_v[i] <- row$color
      }
      
    }
    # Update color
    input$color <- new_color_v
  }
  
  return(input)
}

final_floor <- flip_line(answer_2,n_days = 100)
sum(final_floor$color=="black")

# 2208 - test answer...
# 4070 final answer - CORRECT
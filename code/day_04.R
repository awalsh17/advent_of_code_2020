# Day 4

# Part 1 instructions ----
# The expected fields are as follows:
#   
# byr (Birth Year)
# iyr (Issue Year)
# eyr (Expiration Year)
# hgt (Height)
# hcl (Hair Color)
# ecl (Eye Color)
# pid (Passport ID)
# cid (Country ID)

# Count the number of valid passports - those that have all required fields. Treat cid as optional. In your batch file, how many passports are valid?

# Input ----
input <- readLines(here::here('input/day_04/input.txt'))

pass_list <- list()
id <- 1
for (i in input){
  if (i !=""){
    if (length(pass_list)< id){
      pass_list[[id]] <- i
    } else{
      pass_list[[id]] <- paste(pass_list[[id]], i)
    }
  } else{
    id <- id + 1
  }
}
# Solve ----
valid <- sapply(pass_list, function(x)
  grepl("byr:",x) & grepl("iyr:",x) & grepl("eyr:",x) & 
    grepl("hgt:",x) & grepl("hcl:",x) &grepl("ecl:",x) &grepl("pid",x)
)

sum(valid)

# Part 2 ----

# byr (Birth Year) - four digits; at least 1920 and at most 2002.
# iyr (Issue Year) - four digits; at least 2010 and at most 2020.
# eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
# hgt (Height) - a number followed by either cm or in:
#   If cm, the number must be at least 150 and at most 193.
# If in, the number must be at least 59 and at most 76.
# hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
# ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
# pid (Passport ID) - a nine-digit number, including leading zeroes.
# cid (Country ID) - ignored, missing or not.

# Solve ----
# Parse each passport to fields
fields <- c("byr","iyr","eyr","hgt","hcl","ecl","pid","cid")
pass_df <- matrix(rep(NA,length(fields)*length(pass_list)), ncol = length(fields))
pass_df <- as.data.frame(pass_df)
colnames(pass_df) <- fields

for (i in 1:length(pass_list)){
  entry <- pass_list[[i]]
  entry <- strsplit(entry, " ", fixed=T)[[1]]

  for (j in fields){
    test_str <- grep(j,entry)
    if (length(test_str)>0){
      pass_df[[j]][i] <- stringr::str_remove(entry[test_str], paste0(j,":"))
    }
  }
  
}

library(dplyr)
pass_df_mod <- pass_df %>% 
  mutate(hgt_unit = ifelse(grepl("cm",hgt),"cm","in"),
         hgt = as.numeric(stringr::str_remove(hgt,"cm|in"))) %>% 
  mutate(valid = ifelse(
    # byr (Birth Year) - four digits; at least 1920 and at most 2002.
    (!is.na(byr) & as.numeric(byr) >=1920 & as.numeric(byr) <=2002) &
    # iyr (Issue Year) - four digits; at least 2010 and at most 2020.
    (!is.na(iyr) &as.numeric(iyr) >=2010 & as.numeric(iyr) <=2020) &
    # eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
    (!is.na(eyr) &as.numeric(eyr) >=2020 & as.numeric(eyr) <=2030) &
    # hgt (Height) - a number followed by either cm or in:
    #   If cm, the number must be at least 150 and at most 193.
    # If in, the number must be at least 59 and at most 76.
      (!is.na(hgt)) & 
    (hgt_unit=="cm"&between(hgt,150,193) | hgt_unit=="in"&between(hgt,59,76)) &
    # hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
    (!is.na(hcl) &grepl("^#[(a-z|0-9)]{6}$", hcl)) & 
    # ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
    (!is.na(ecl) &ecl %in% c("amb", "blu", "brn" ,"gry" ,"grn" ,"hzl", "oth"))&
    # pid (Passport ID) - a nine-digit number, including leading zeroes.
      (!is.na(pid) &grepl("^[(0-9)]{9}$",pid))
    , TRUE, FALSE)
  )

sum(pass_df_mod$valid)

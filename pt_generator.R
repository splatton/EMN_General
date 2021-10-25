#This script will generate an artificial patient when called.

pt_generator <- function(){
  #Loading libraries
  library(dplyr)
  library(stringr)
  library(readr)
  
  temp_tibble <- tibble(Last.Name = NA, First.Name = NA, Sex = NA)
  working <- name_sex_gen(temp_tibble)
  working <- age_gen(working)
  working
}

#Name/sex generator
name_sex_gen <- function(temp){
  first_names <- read_csv("yob2019.txt", col_names = c('Name', 'Sex', 'n'))
  last_names <- read_csv("last_names.csv")
  first_sample <- sample(size = 1, 1:nrow(first_names), prob = first_names$n)
  second_sample <- sample(size = 1, 1:nrow(last_names), prob = last_names$prop100k)
  temp[1,'Last.Name'] <- last_names[second_sample,'name']
  temp[1,'First.Name'] <- first_names[first_sample,'Name']
  temp[1,'Sex'] <- first_names[first_sample,'Sex']
  return(temp)
}

#Age/age.cat generator
age_gen <- function(temp){
  age_cats <- c('Neonate', 'Infant', 'Toddler', 'Preschool', 'Grade School', 'Adolescent')
  first_sample <- sample(size = 1, 1:length(age_cats))
  temp[1,'Age.Cat'] <- age_cats[first_sample]
  
  #This next part of the code will use the age.cat to pick the numerical age. For readability, this numerical age will be translated into a string.
  if(first_sample == 1) {
    second_sample <- sample(0:28, size = 1)
    temp[1,'Age'] <- str_c(second_sample, 'd')
  }
  else if(first_sample == 2) {
    second_sample <- sample(1:11, size = 1)
    temp[1,'Age'] <- str_c(second_sample, 'm')
  }
  else if(first_sample == 3) {
    second_sample <- sample(1:2, size = 1)
    temp[1,'Age'] <- str_c(second_sample, 'y')
  }
  else if(first_sample == 4) {
    second_sample <- sample(3:5, size = 1)
    temp[1,'Age'] <- str_c(second_sample, 'y')
  }
  else if(first_sample == 5) {
    second_sample <- sample(6:11, size = 1)
    temp[1,'Age'] <- str_c(second_sample, 'y')
  }
  else if(first_sample == 6) {
    second_sample <- sample(12:18, size = 1)
    temp[1,'Age'] <- str_c(second_sample, 'y')
  }
  return(temp)
}
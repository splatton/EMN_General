#This script will generate an artificial patient when called.

pt_generator <- function(){
  #Loading libraries
  library(dplyr)
  library(stringr)
  library(readr)
  library(tibble)
  
  temp_tibble <- tibble(Last.Name = 'Test', First.Name = 'Test', Sex = 'Test')
  working <- name_sex_gen(temp_tibble)
  working <- age_gen(working)
  working <- vs_gen(working)
  working <- pmh_gen(working)
  working
}

#Name/sex generator
name_sex_gen <- function(temp){
  first_names <- read_csv("yob2019.txt", col_names = c('Name', 'Sex', 'n'), col_types = "cci")
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
    temp[1,'Num.Age'] <- second_sample/365
  }
  else if(first_sample == 2) {
    second_sample <- sample(1:11, size = 1)
    temp[1,'Age'] <- str_c(second_sample, 'm')
    temp[1,'Num.Age'] <- second_sample/12
  }
  else if(first_sample == 3) {
    second_sample <- sample(1:2, size = 1)
    temp[1,'Age'] <- str_c(second_sample, 'y')
    temp[1,'Num.Age'] <- second_sample
  }
  else if(first_sample == 4) {
    second_sample <- sample(3:5, size = 1)
    temp[1,'Age'] <- str_c(second_sample, 'y')
    temp[1,'Num.Age'] <- second_sample
  }
  else if(first_sample == 5) {
    second_sample <- sample(6:11, size = 1)
    temp[1,'Age'] <- str_c(second_sample, 'y')
    temp[1,'Num.Age'] <- second_sample
  }
  else if(first_sample == 6) {
    second_sample <- sample(12:18, size = 1)
    temp[1,'Age'] <- str_c(second_sample, 'y')
    temp[1,'Num.Age'] <- second_sample
  }
  return(temp)
}

#This function generates vitals based on a normal distribution. We will intentionally make the normal distribution 1 on either side to get more abnormal results.
vs_gen <- function(temp) {
  age_cats <- c('Neonate', 'Infant', 'Toddler', 'Preschool', 'Grade School', 'Adolescent')
  
  
  #This next part of the code will use the age.cat to pick the numerical age. For readability, this numerical age will be translated into a string.
  if(temp[1,'Age.Cat'] == age_cats[1]) {
    temp[1,'Temp.C'] <- round(rnorm(1, mean = 37.5, sd = 1), digits = 1)
    temp[1,'Temp.F'] <- round((temp[1,'Temp.C']*(9/5)+32), digits = 1)
    temp_adj <- as.double(ifelse(temp[1,'Temp.C'] > 38, temp[[1,'Temp.C']] - 38, 0))
    temp[1,'HR'] <- round(rnorm(1, mean = 150 + temp_adj*8, sd = 35))
    temp[1,'RR'] <- round(rnorm(1, mean = 45 + temp_adj, sd = 10))
    temp[1,'Systolic.BP'] <- round(rnorm(1, mean = 75, sd = 10))
    temp_pulse_ox <- round(rnorm(1, mean = 99, sd = 6))
    temp[1,'Pulse.Ox'] <- ifelse(temp_pulse_ox > 100, 100, temp_pulse_ox)
  }
  else if(temp[1,'Age.Cat'] == age_cats[2]) {
    temp[1,'Temp.C'] <- round(rnorm(1, mean = 37.5, sd = 1), digits = 1)
    temp[1,'Temp.F'] <- round((temp[1,'Temp.C']*(9/5)+32), digits = 1)
    temp_adj <- as.double(ifelse(temp[1,'Temp.C'] > 38, temp[[1,'Temp.C']] - 38, 0))
    temp[1,'HR'] <- round(rnorm(1, mean = 140 + temp_adj*8, sd = 35))
    temp[1,'RR'] <- round(rnorm(1, mean = 30 + temp_adj, sd = 10))
    temp[1,'Systolic.BP'] <- round(rnorm(1, mean = 90, sd = 10))
    temp_pulse_ox <- round(rnorm(1, mean = 99, sd = 6))
    temp[1,'Pulse.Ox'] <- ifelse(temp_pulse_ox > 100, 100, temp_pulse_ox)
  }
  else if(temp[1,'Age.Cat'] == age_cats[3]) {
    temp[1,'Temp.C'] <- round(rnorm(1, mean = 37.5, sd = 1), digits = 1)
    temp[1,'Temp.F'] <- round((temp[1,'Temp.C']*(9/5)+32), digits = 1)
    temp_adj <- as.double(ifelse(temp[1,'Temp.C'] > 38, temp[[1,'Temp.C']] - 38, 0))
    temp[1,'HR'] <- round(rnorm(1, mean = 120 + temp_adj*8, sd = 35))
    temp[1,'RR'] <- round(rnorm(1, mean = 25 + temp_adj, sd = 8))
    temp[1,'Systolic.BP'] <- round(rnorm(1, mean = 90, sd = 15))
    temp_pulse_ox <- round(rnorm(1, mean = 99, sd = 6))
    temp[1,'Pulse.Ox'] <- ifelse(temp_pulse_ox > 100, 100, temp_pulse_ox)
  }
  else if(temp[1,'Age.Cat'] == age_cats[4]) {
    temp[1,'Temp.C'] <- round(rnorm(1, mean = 37.5, sd = 1), digits = 1)
    temp[1,'Temp.F'] <- round((temp[1,'Temp.C']*(9/5)+32), digits = 1)
    temp_adj <- as.double(ifelse(temp[1,'Temp.C'] > 38, temp[[1,'Temp.C']] - 38, 0))
    temp[1,'HR'] <- round(rnorm(1, mean = 100 + temp_adj*8, sd = 20))
    temp[1,'RR'] <- round(rnorm(1, mean = 23 + temp_adj, sd = 6))
    temp[1,'Systolic.BP'] <- round(rnorm(1, mean = 95, sd = 15))
    temp_pulse_ox <- round(rnorm(1, mean = 99, sd = 6))
    temp[1,'Pulse.Ox'] <- ifelse(temp_pulse_ox > 100, 100, temp_pulse_ox)
  }
  else if(temp[1,'Age.Cat'] == age_cats[5]) {
    temp[1,'Temp.C'] <- round(rnorm(1, mean = 37.5, sd = 1), digits = 1)
    temp[1,'Temp.F'] <- round((temp[1,'Temp.C']*(9/5)+32), digits = 1)
    temp_adj <- as.double(ifelse(temp[1,'Temp.C'] > 38, temp[[1,'Temp.C']] - 38, 0))
    temp[1,'HR'] <- round(rnorm(1, mean = 90 + temp_adj*8, sd = 20))
    temp[1,'RR'] <- round(rnorm(1, mean = 20 + temp_adj, sd = 6))
    temp[1,'Systolic.BP'] <- round(rnorm(1, mean = 100, sd = 15))
    temp_pulse_ox <- round(rnorm(1, mean = 99, sd = 6))
    temp[1,'Pulse.Ox'] <- ifelse(temp_pulse_ox > 100, 100, temp_pulse_ox)
  }
  else if(temp[1,'Age.Cat'] == age_cats[6]) {
    temp[1,'Temp.C'] <- round(rnorm(1, mean = 37.5, sd = 1), digits = 1)
    temp[1,'Temp.F'] <- round((temp[1,'Temp.C']*(9/5)+32), digits = 1)
    temp_adj <- as.double(ifelse(temp[1,'Temp.C'] > 38, temp[[1,'Temp.C']] - 38, 0))
    temp[1,'HR'] <- round(rnorm(1, mean = 80 + temp_adj*8, sd = 20))
    temp[1,'RR'] <- round(rnorm(1, mean = 18 + temp_adj, sd = 6))
    temp[1,'Systolic.BP'] <- round(rnorm(1, mean = 120, sd = 20))
    temp_pulse_ox <- round(rnorm(1, mean = 99, sd = 6))
    temp[1,'Pulse.Ox'] <- ifelse(temp_pulse_ox > 100, 100, temp_pulse_ox)
  }
  return(temp)
}

pmh_gen <- function(temp) {
  temp[1,'Prematurity'] <- sample(c(TRUE, FALSE), size = 1, prob = c((0.05 + 0.1/(round(temp[1,'Num.Age']) + 1)),0.9))
  temp[1,'Reactive.Airways'] <- sample(c(TRUE, FALSE), size = 1, prob = c((0.05 + 0.1*temp[1,'Prematurity']),0.9))
  temp[1,'CHD'] <- sample(c(TRUE, FALSE), size = 1, prob = c((0.05 + 0.1*temp[1,'Prematurity']),0.9))
  temp[1,'Pneumonia'] <- sample(c(TRUE, FALSE), size = 1, prob = c((0.05 + 0.1*temp[1,'Prematurity']),0.9))
  temp[1,'Bronchiolitis'] <- sample(c(TRUE, FALSE), size = 1, prob = c((0.05 + 0.1*temp[1,'Prematurity']),0.9))
  return(temp)
}
#This script will generate an artificial patient when called.

#First we will generate constants for the VS.
neonate_hr_mean <- 150
neonate_hr_sd <- 35
neonate_hr_adj <- 15
neonate_rr_mean <- 45
neonate_rr_sd <- 10
neonate_sbp_mean <- 75
neonate_sbp_sd <- 10

infant_hr_mean <- 140
infant_hr_sd <- 35
infant_hr_adj <- 12
infant_rr_mean <- 30
infant_rr_sd <- 10
infant_sbp_mean <- 90
infant_sbp_sd <- 10

toddler_hr_mean <- 120
toddler_hr_sd <- 35
toddler_hr_adj <- 10
toddler_rr_mean <- 25
toddler_rr_sd <- 8
toddler_sbp_mean <- 90
toddler_sbp_sd <- 15

preschool_hr_mean <- 100
preschool_hr_sd <- 20
preschool_hr_adj <- 10
preschool_rr_mean <- 23
preschool_rr_sd <- 6
preschool_sbp_mean <- 95
preschool_sbp_sd <- 15

gradeschool_hr_mean <- 90
gradeschool_hr_sd <- 20
gradeschool_hr_adj <- 10
gradeschool_rr_mean <- 20
gradeschool_rr_sd <- 6
gradeschool_sbp_mean <- 100
gradeschool_sbp_sd <- 15

adolescent_hr_mean <- 80
adolescent_hr_sd <- 20
adolescent_hr_adj <- 10
adolescent_rr_mean <- 18
adolescent_rr_sd <- 6
adolescent_sbp_mean <- 120
adolescent_sbp_sd <- 20

pulse_ox_sd <- 5

#Other constants

age_cats <- c('Neonate', 'Infant', 'Toddler', 'Preschool', 'Grade School', 'Adolescent')

cc_vec <- c('Cough', 'Runny Nose', 'Fever', 'Wheezing', 'Shortness of Breath', 'Congestion', 'Sore Throat', 'Lethargy', 'Chest Pain')

ros_vec <- c('Cough', 'Runny Nose', 'Fever', 'Shortness of Breath', 'Congestion', 'Sore Throat', 'Lethargy', 'Chest Pain', 'Vomiting', 'Wheezing', 'Rash')


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
  working <- cc_hpi_gen(working)
  working <- ros_gen(working)
  working <- fever_duration_gen(working)
  working <- phys_exam_gen(working)
  working
}

#Name/sex generator
name_sex_gen <- function(temp){
  first_sample <- sample(size = 1, 1:nrow(first_names), prob = first_names$n)
  second_sample <- sample(size = 1, 1:nrow(last_names), prob = last_names$prop100k)
  temp[1,'Last.Name'] <- last_names[second_sample,'name']
  temp[1,'First.Name'] <- first_names[first_sample,'Name']
  temp[1,'Sex'] <- first_names[first_sample,'Sex']
  return(temp)
}

#Age/age.cat generator
age_gen <- function(temp){
  first_sample <- sample(size = 1, 1:length(age_cats), prob = c(0.5,1,1,1,1,1))
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
  
  #Neonate VS
  
  if(temp[1,'Age.Cat'] == age_cats[1]) {
    temp[1,'Temp.C'] <- round(rnorm(1, mean = 37.5, sd = 1), digits = 1)
    temp[1,'Temp.F'] <- round((temp[1,'Temp.C']*(9/5)+32), digits = 1)
    temp_adj <- as.double(ifelse(temp[1,'Temp.C'] > 38, temp[[1,'Temp.C']] - 38, 0))
    temp[1,'RR'] <- round(rnorm(1, mean = neonate_rr_mean + temp_adj*3, sd = neonate_rr_sd))
    temp[1,'RR.Z'] <- (temp[1,'RR'] - neonate_rr_mean)/neonate_rr_sd
    temp[1,'HR'] <- round(rnorm(1, mean = neonate_hr_mean + temp_adj*neonate_hr_adj + temp[[1,'RR.Z']]*2, sd = neonate_hr_sd))
    temp[1,'HR.Z'] <- (temp[1,'HR'] - neonate_hr_mean)/neonate_hr_sd
    temp[1,'Systolic.BP'] <- round(rnorm(1, mean = neonate_sbp_mean, sd = neonate_sbp_sd))
    temp[1,'SBP.Z'] <- (temp[1,'Systolic.BP'] - neonate_sbp_mean)/neonate_sbp_sd
    temp_pulse_ox <- round(rnorm(1, mean = 99 - temp[[1,'RR.Z']], sd = pulse_ox_sd))
    temp[1,'Pulse.Ox'] <- ifelse(temp_pulse_ox > 100, 100, temp_pulse_ox)
  }
  
  #Infant VS
  
  else if(temp[1,'Age.Cat'] == age_cats[2]) {
    temp[1,'Temp.C'] <- round(rnorm(1, mean = 37.5, sd = 1), digits = 1)
    temp[1,'Temp.F'] <- round((temp[1,'Temp.C']*(9/5)+32), digits = 1)
    temp_adj <- as.double(ifelse(temp[1,'Temp.C'] > 38, temp[[1,'Temp.C']] - 38, 0))
    temp[1,'RR'] <- round(rnorm(1, mean = infant_rr_mean + temp_adj*3, sd = infant_rr_sd))
    temp[1,'RR.Z'] <- (temp[1,'RR'] - infant_rr_mean)/infant_rr_sd
    temp[1,'HR'] <- round(rnorm(1, mean = infant_hr_mean + temp_adj*infant_hr_adj + temp[[1,'RR.Z']]*2, sd = infant_hr_sd))
    temp[1,'HR.Z'] <- (temp[1,'HR'] - infant_hr_mean)/infant_hr_sd
    temp[1,'Systolic.BP'] <- round(rnorm(1, mean = infant_sbp_mean, sd = infant_sbp_sd))
    temp[1,'SBP.Z'] <- (temp[1,'Systolic.BP'] - infant_sbp_mean)/infant_sbp_sd
    temp_pulse_ox <- round(rnorm(1, mean = 99 - temp[[1,'RR.Z']], sd = pulse_ox_sd))
    temp[1,'Pulse.Ox'] <- ifelse(temp_pulse_ox > 100, 100, temp_pulse_ox)
  }
  
  #Toddler VS
  
  else if(temp[1,'Age.Cat'] == age_cats[3]) {
    temp[1,'Temp.C'] <- round(rnorm(1, mean = 37.5, sd = 1), digits = 1)
    temp[1,'Temp.F'] <- round((temp[1,'Temp.C']*(9/5)+32), digits = 1)
    temp_adj <- as.double(ifelse(temp[1,'Temp.C'] > 38, temp[[1,'Temp.C']] - 38, 0))
    temp[1,'RR'] <- round(rnorm(1, mean = toddler_rr_mean + temp_adj*3, sd = toddler_rr_sd))
    temp[1,'RR.Z'] <- (temp[1,'RR'] - toddler_rr_mean)/toddler_rr_sd
    temp[1,'HR'] <- round(rnorm(1, mean = toddler_hr_mean + temp_adj*toddler_hr_adj + temp[[1,'RR.Z']]*2, sd = toddler_hr_sd))
    temp[1,'HR.Z'] <- (temp[1,'HR'] - toddler_hr_mean)/toddler_hr_sd
    temp[1,'Systolic.BP'] <- round(rnorm(1, mean = toddler_sbp_mean, sd = toddler_sbp_sd))
    temp[1,'SBP.Z'] <- (temp[1,'Systolic.BP'] - toddler_sbp_mean)/toddler_sbp_sd
    temp_pulse_ox <- round(rnorm(1, mean = 99 - temp[[1,'RR.Z']], sd = pulse_ox_sd))
    temp[1,'Pulse.Ox'] <- ifelse(temp_pulse_ox > 100, 100, temp_pulse_ox)
  }
  
  #Preschool VS
  
  else if(temp[1,'Age.Cat'] == age_cats[4]) {
    temp[1,'Temp.C'] <- round(rnorm(1, mean = 37.5, sd = 1), digits = 1)
    temp[1,'Temp.F'] <- round((temp[1,'Temp.C']*(9/5)+32), digits = 1)
    temp_adj <- as.double(ifelse(temp[1,'Temp.C'] > 38, temp[[1,'Temp.C']] - 38, 0))
    temp[1,'RR'] <- round(rnorm(1, mean = preschool_rr_mean + temp_adj*3, sd = preschool_rr_sd))
    temp[1,'RR.Z'] <- (temp[1,'RR'] - preschool_rr_mean)/preschool_rr_sd
    temp[1,'HR'] <- round(rnorm(1, mean = preschool_hr_mean + temp_adj*preschool_hr_adj + temp[[1,'RR.Z']]*2, sd = preschool_hr_sd))
    temp[1,'HR.Z'] <- (temp[1,'HR'] - preschool_hr_mean)/preschool_hr_sd
    temp[1,'Systolic.BP'] <- round(rnorm(1, mean = preschool_sbp_mean, sd = preschool_sbp_sd))
    temp[1,'SBP.Z'] <- (temp[1,'Systolic.BP'] - preschool_sbp_mean)/preschool_sbp_sd
    temp_pulse_ox <- round(rnorm(1, mean = 99 - temp[[1,'RR.Z']], sd = pulse_ox_sd))
    temp[1,'Pulse.Ox'] <- ifelse(temp_pulse_ox > 100, 100, temp_pulse_ox)
  }
  
  #Gradeschool VS
  
  else if(temp[1,'Age.Cat'] == age_cats[5]) {
    temp[1,'Temp.C'] <- round(rnorm(1, mean = 37.5, sd = 1), digits = 1)
    temp[1,'Temp.F'] <- round((temp[1,'Temp.C']*(9/5)+32), digits = 1)
    temp_adj <- as.double(ifelse(temp[1,'Temp.C'] > 38, temp[[1,'Temp.C']] - 38, 0))
    temp[1,'RR'] <- round(rnorm(1, mean = gradeschool_rr_mean + temp_adj*3, sd = gradeschool_rr_sd))
    temp[1,'RR.Z'] <- (temp[1,'RR'] - gradeschool_rr_mean)/gradeschool_rr_sd
    temp[1,'HR'] <- round(rnorm(1, mean = gradeschool_hr_mean + temp_adj*gradeschool_hr_adj + temp[[1,'RR.Z']]*2, sd = gradeschool_hr_sd))
    temp[1,'HR.Z'] <- (temp[1,'HR'] - gradeschool_hr_mean)/gradeschool_hr_sd
    temp[1,'Systolic.BP'] <- round(rnorm(1, mean = gradeschool_sbp_mean, sd = gradeschool_sbp_sd))
    temp[1,'SBP.Z'] <- (temp[1,'Systolic.BP'] - gradeschool_sbp_mean)/gradeschool_sbp_sd
    temp_pulse_ox <- round(rnorm(1, mean = 99 - temp[[1,'RR.Z']], sd = pulse_ox_sd))
    temp[1,'Pulse.Ox'] <- ifelse(temp_pulse_ox > 100, 100, temp_pulse_ox)
  }
  
  #Adolescent VS
  
  else if(temp[1,'Age.Cat'] == age_cats[6]) {
    temp[1,'Temp.C'] <- round(rnorm(1, mean = 37.5, sd = 1), digits = 1)
    temp[1,'Temp.F'] <- round((temp[1,'Temp.C']*(9/5)+32), digits = 1)
    temp_adj <- as.double(ifelse(temp[1,'Temp.C'] > 38, temp[[1,'Temp.C']] - 38, 0))
    temp[1,'RR'] <- round(rnorm(1, mean = adolescent_rr_mean + temp_adj*3, sd = adolescent_rr_sd))
    temp[1,'RR.Z'] <- (temp[1,'RR'] - adolescent_rr_mean)/adolescent_rr_sd
    temp[1,'HR'] <- round(rnorm(1, mean = adolescent_hr_mean + temp_adj*adolescent_hr_adj + temp[[1,'RR.Z']]*2, sd = adolescent_hr_sd))
    temp[1,'HR.Z'] <- (temp[1,'HR'] - adolescent_hr_mean)/adolescent_hr_sd
    temp[1,'Systolic.BP'] <- round(rnorm(1, mean = adolescent_sbp_mean, sd = adolescent_sbp_sd))
    temp[1,'SBP.Z'] <- (temp[1,'Systolic.BP'] - adolescent_sbp_mean)/adolescent_sbp_sd
    temp_pulse_ox <- round(rnorm(1, mean = 99 - temp[[1,'RR.Z']], sd = pulse_ox_sd))
    temp[1,'Pulse.Ox'] <- ifelse(temp_pulse_ox > 100, 100, temp_pulse_ox)
  }
  return(temp)
}

pmh_gen <- function(temp) {
  temp[1,'Prematurity'] <- sample(c(TRUE, FALSE), size = 1, prob = c((0.03 + 0.1/(round(temp[1,'Num.Age']) + 1)),0.9))
  temp[1,'Downs'] <- sample(c(TRUE, FALSE), size = 1, prob = c(0.002,0.998))
  temp[1,'Reactive.Airways'] <- sample(c(TRUE, FALSE), size = 1, prob = c((0.07 + 0.1*temp[1,'Prematurity']),0.9))
  temp[1,'CHD'] <- sample(c(TRUE, FALSE), size = 1, prob = c((0.05 + 0.1*temp[1,'Prematurity'] + 0.1*temp[1,'Downs']),0.9))
  temp[1,'Bronchiolitis'] <- sample(c(TRUE, FALSE), size = 1, prob = c((0.05 + 0.1*temp[1,'Prematurity']),0.9))
  temp[1,'Sickle.Cell'] <- sample(c(TRUE, FALSE), size = 1, prob = c(0.005,0.995))
  temp[1,'BPD'] <- sample(c(TRUE, FALSE), size = 1, prob = c((0.001 + 0.1*temp[1,'Prematurity']),0.999))
  temp[1,'Cystic.Fibrosis'] <- sample(c(TRUE, FALSE), size = 1, prob = c(0.001,0.999))
  temp[1,'Pneumonia'] <- sample(c(TRUE, FALSE), size = 1, prob = c((0.05 + 0.1*temp[1,'Prematurity'] + 0.5*temp[1,'Cystic.Fibrosis']),0.9))
  return(temp)
}

cc_hpi_gen <- function(temp) {
  
  temp[1,'Chief.Complaint'] <- sample(cc_vec, size = 1, prob = c(10, 2, 5, 5, 4, 4, 3, 2, 0.1*temp[[1,'Num.Age']]))
  temp_duration <- round(rnorm(1, mean = 2, sd = 3))
  temp_duration <- ifelse(temp_duration < 0, -1 * temp_duration, temp_duration)
  temp[1,'Duration'] <- ifelse(temp_duration > temp[1,'Num.Age']*365, 1, temp_duration)
  temp[1,'Course'] <- sample(c('staying the same', 'worsening', 'improving'), size = 1)
  temp[1,'Onset'] <- sample(c('suddenly', 'gradually'), size = 1, prob = c(3, 7))
  temp[1,'Severity'] <- sample(c('mild', 'moderate', 'severe'), size = 1, prob = c(5,3,1))
  temp[1,'Prior.Episodes'] <- sample(c(TRUE, FALSE), size = 1, prob = c(4,6))
  return(temp)
}

ros_gen <- function(temp) {
  
  for (i in 1:length(ros_vec)) {
    if((ros_vec[i] == 'Wheezing') & temp[[1,'Reactive.Airways']] & (temp[[1,'Chief.Complaint']] != 'Wheezing')){
      temp[1,ros_vec[i]] <- sample(c(TRUE, FALSE), size = 1, prob = c(0.7, 0.3))
    }
    else if((ros_vec[i] != temp[1,'Chief.Complaint']) & (ros_vec[i]== 'Chest Pain')) {
      temp[1,ros_vec[i]] <- sample(c(TRUE, FALSE), size = 1, prob = c((0.08*(temp[[1,'Num.Age']]*0.1)), 0.9))
    }
    else if(ros_vec[i] != temp[1,'Chief.Complaint']) {
      temp[1,ros_vec[i]] <- sample(c(TRUE, FALSE), size = 1, prob = c(0.08, 0.9))
    }
    else {
      temp[1,ros_vec[i]] <- TRUE
    }
  }
  return(temp)
}

fever_duration_gen <- function(temp) {
  if(temp[1,'Fever'] | (temp[1,'Chief.Complaint'] == 'Fever')) {
    temp[1,'Fever.Duration'] <- sample(0:temp[[1,'Duration']], size = 1)
  }
  else {
    temp[1,'Fever.Duration'] <- 0
  }
  return(temp)
}

phys_exam_gen <- function(temp) {
  #These are the pulmonary exam findings.
  temp[1,'PE.Dyspnea'] <- sample(c(FALSE,TRUE), size = 1, prob = c(80,(4^temp[[1,'RR.Z']])*((temp[[1,'Pulse.Ox']]-98)/3)^4)*(2^temp[[1,'HR.Z']]))
  
  temp[1,'PE.Wheezing'] <- sample(c(FALSE,TRUE), size = 1, prob = c(100,(((4^temp[[1,'RR.Z']])*((temp[[1,'Pulse.Ox']]-98)/3)^4)*(2^temp[[1,'HR.Z']]) + (temp[[1,'Reactive.Airways']] | temp[[1,'Wheezing']])*70)))
  
  temp[1,'PE.RespDistress'] <- sample(c(FALSE,TRUE), size = 1, prob = c(150,(4^temp[[1,'RR.Z']])*((temp[[1,'Pulse.Ox']]-98)/3)^4)*(2^temp[[1,'HR.Z']]))
  
  temp[1,'PE.Retractions'] <- sample(c(FALSE,TRUE), size = 1, prob = c(80,(4^temp[[1,'RR.Z']])*((temp[[1,'Pulse.Ox']]-98)/3)^4)*(2^temp[[1,'HR.Z']]))
  
  temp[1,'PE.Grunting'] <- sample(c(FALSE,TRUE), size = 1, prob = c(150,(4^temp[[1,'RR.Z']])*((temp[[1,'Pulse.Ox']]-98)/3)^4)*(2^temp[[1,'HR.Z']]))
  
  #This physical exam finding is more related to infectious type findings.
  temp[1,'PE.FocalDecrBS'] <- sample(c(FALSE, TRUE), size = 1, prob = c(40,(2^temp[[1,'RR.Z']])*((temp[[1,'Pulse.Ox']]-98)/3)^2)*((temp[[1,'Fever']] | (temp[[1,'Temp.C']] > 38))+1)^2)
  
  #The rales finding is more common than focal rales. Both will go up with fever and multiplicatively as pulse ox decreases below 94%.
  temp[1,'PE.Rales'] <- sample(c(FALSE, TRUE), size = 1, prob = c(20,(2^temp[[1,'RR.Z']])*((temp[[1,'Pulse.Ox']]-98)/3)^2)*((temp[[1,'Fever']] | (temp[[1,'Temp.C']] > 38))+1)^2)
  
  temp[1,'PE.FocalRales'] <- sample(c(FALSE, TRUE), size = 1, prob = c(40,(2^temp[[1,'RR.Z']])*((temp[[1,'Pulse.Ox']]-98)/3)^2)*((temp[[1,'Fever']] | (temp[[1,'Temp.C']] > 38))+1)^2)
  
  temp[1,'PE.MDry'] <- sample(c(FALSE, TRUE), size = 1, prob = c(40,(4^temp[[1,'HR.Z']])))
  
  temp[1,'PE.Murmur'] <- sample(c(TRUE,FALSE), size = 1, prob = c(0.01, 0.99))
  
  temp[1,'PE.AbdDistension'] <- sample(c(TRUE, FALSE), size = 1, prob = c(0.005, 0.995))
  
  temp[1,'PE.Lethargy'] <- sample(c(FALSE,TRUE), size = 1, prob = c(400,(2^temp[[1,'RR.Z']])*((temp[[1,'Pulse.Ox']]-98)/3)^4)*(2^temp[[1,'HR.Z']])*(4^(-temp[[1,'SBP.Z']])))
  
  temp[1,'PE.Pale'] <- sample(c(FALSE, TRUE), size = 1, prob = c(80,(4^temp[[1,'HR.Z']])))
  
  return(temp)
}

pt_gen_loop <- function(num_loops) {
  library(parallel)
  library(parallelMap)
  parallelStartSocket(cpus = detectCores())
  temp <- data.frame()
  for (i in 1:num_loops) {
    temp <- rbind(temp,pt_generator())
  }
  parallelStop()
  return(temp)
}
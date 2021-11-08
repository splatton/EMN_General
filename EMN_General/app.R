#
# This web application will generate fake pediatric respiratory patients and allow physicians to play with them.
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(googlesheets4)
library(gargle)
library(googledrive)

drive_auth(
    cache = ".secrets",
    email = "spltt.tlb@gmail.com"
)


#This section will be for defining constants and reading in initial data.

#CONSTANTS

#Vital Signs

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

#Reading in data

first_names <- read_csv("yob2019.txt", col_names = c('Name', 'Sex', 'n'), col_types = "cci")

last_names <- read_csv("last_names.csv")

#Defining static functions

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
    
    #This next part of the code will use the age.cat to choose the correct vital sign distributions.
    
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

#This set of functions will generate the patient narrative using patient data.

narrative_gen <- function(patient) {
    
    temp_ros_vec <- ros_vec[ros_vec != patient[[1,'Chief.Complaint']]]
    
    working <- str_c(patient[1,'First.Name'], ' ', patient[1,'Last.Name'], ' is a ')
    working <- str_c(working, patient[1,'Age'], ' old ')
    working <- str_c(working, ifelse(patient[1,'Sex'] == 'F', 'female ', 'male '))
    working <- str_c(working, 'who presents with ', tolower(patient[1,'Chief.Complaint']), '. ')
    working <- str_c(working, 'The ', tolower(patient[1,'Chief.Complaint']), ' started ')
    working <- str_c(working, patient[1,'Onset'], ' ')
    working <- str_c(working, ifelse(patient[[1,'Duration']] == 0, 'today ', str_c(patient[1,'Duration'], ' days ago. ')))
    working <- str_c(working, 'The ', tolower(patient[1,'Chief.Complaint']), ' is currently ', patient[1,'Severity'], ' in severity ')
    working <- str_c(working, 'and has been ', patient[1,'Course'], '.')
    working <- str_c(working, ' It is associated with ')
    ros_string <- ''
    for (i in 1:length(temp_ros_vec)) {
        temp <- temp_ros_vec[i]
        if(patient[[1,temp_ros_vec[i]]] & (str_length(ros_string) == 0)) {
            ros_string <- str_c(ros_string, tolower(temp_ros_vec[i]))
        }
        else if(patient[[1,temp_ros_vec[i]]]) {
            ros_string <- str_c(ros_string, " and ", tolower(temp_ros_vec[i]))
        }
    }
    if(str_length(ros_string) == 0) {
        ros_string <- 'nothing'
    }
    working <- str_c(working, ros_string)
    
    if((patient[1,'Temp.C'] >= 38) & (patient[1,'Chief.Complaint'] != 'Fever') & !(patient[1,'Fever'])) {
        working <- str_c(working, '. The patient has not been febrile before coming to the ER')
    }
    if(((patient[1,'Chief.Complaint'] != 'Fever') & (patient[1,'Fever'])) & (patient[[1,'Fever.Duration']] == 0)) {
        working <- str_c(working, '. The patient has had a fever today only')
    }
    if(((patient[1,'Chief.Complaint'] != 'Fever') & (patient[1,'Fever'])) & (patient[[1,'Fever.Duration']] > 0)) {
        working <- str_c(working, '. The patient has had a fever for ', patient[[1,'Fever.Duration']], ' days')
    }
    
    working <- str_c(working, '. The patient has ', ifelse(patient[1,'Prior.Episodes'], '', 'never '), 'had prior similar episodes. ')
    return(working)
}

pt_generator <- function(){
    
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

pt_gen_loop <- function(num_loops) {
    temp <- data.frame()
    for (i in 1:num_loops) {
        temp <- rbind(temp,pt_generator())
    }
    return(temp)
}

# Defines the UI for the patient generation application
ui <- fluidPage(theme = shinytheme("cosmo"),
                useShinyjs(),
                useSweetAlert(),

    # Application title
    titlePanel("Welcome to EMN General!"),
    
    tabsetPanel(
        tabPanel("See Patients",

    # Sidebar allows for user login and downloading of procedurally-generated patient data. 
    sidebarLayout(
        sidebarPanel(
            verticalLayout(
            #Login information here
            div(id = "login_div",
                helpText(tags$b("Enter your username and and email address for the leaderboard.")),
                helpText("Remember: the winner gets 20 bucks. Your email address will not be shared; I just need it to contact you in case you win."),
            textInput(inputId = "user_inpt", label = "Username"),
            textInput(inputId = "email_inpt", label = "E-Mail Address"),
            actionButton(inputId = "login_btn", label = "Log In")
            ),
            shinyjs::hidden(
                div(id = "logged_in_div",
                    tags$b(textOutput("user_text")),
                    hr(),
                    textOutput("pts_seen")
                    )
            ),
            hr(),
            tags$a(href = "cnn.com", "RAW DATASET"),
            tags$a(href = "mailto:spltt.tlb@gmail.com", "EMAIL ME")
            ) 
        ),

        # The main panel shows the patient information and allows the user to make their selections.
        mainPanel(
            verticalLayout(
                wellPanel(
                    verticalLayout(
                        tags$h2("HPI"),
                        textOutput("pt_narrative"),
                        hr(),
                        tags$h2("PMHx"),
                        textOutput("pmhx")
                    )
                ),
                wellPanel(
                    verticalLayout(
                        tags$h2("Physical Exam"),
                        #This next part of the layout will show the vital signs.
                        hr(),
                        flowLayout(
                            textOutput("temp"),
                            textOutput("pulse"),
                            textOutput("resps"),
                            textOutput("bloodpressure"),
                            textOutput("pulseox")
                        ),
                        br(),
                        verticalLayout(
                            textOutput("pe_resp"),
                            textOutput("pe_cards"),
                            textOutput("pe_abd"),
                            textOutput("pe_heent"),
                            textOutput("pe_skin"),
                            textOutput("pe_neuro")
                        )
                    )
                ),
                wellPanel(
                    verticalLayout(
                    tags$h2("Decision Time!"),
                    br(),
                    helpText("Would you recommend a chest X-ray for this patient?"),
                    hr(),
                    div(id = "cxr_div",
                        flowLayout(
                            actionButton(inputId = "yes_cxr",
                                         "Recommend a Chest X-Ray",
                                         icon = icon("thumbs-up")),
                            actionButton(inputId = "no_cxr",
                                         "Recommend No Imaging",
                                         icon = icon("thumbs-down"))
                        )
                        ),
                    shinyjs::hidden(
                    div(id = "thanks",
                        helpText("Thanks! Your next patient is waiting for you!")
                    )
                        ),
                    br()
                    #hr(),
                    #actionButton(inputId = "new_patient", label = "New Patient")
                )
                )
        )
        )
    )
    ),
    tabPanel("Leaderboard",
             wellPanel(
                 helpText(tags$b("Scoring Algorithm: "), "Your score is your accuracy squared times the number of questions answered. You are rewarded for every correct answer, but an incorrect answer will penalize you!"),
                 helpText("The leaderboard will refresh periodically; check back often because your standings may change as others input their data."),
                 hr(),
                 helpText("The leaderboard will populate once enough people have tried the application.")
             )
             ),
    tabPanel("Test a Patient",
             wellPanel(
                 helpText("This area will allow a person to input data on a hypothetical patient to see if an electronic consensus of doctors would recommend a chest X-ray. It will populate once enough fake patients have been treated!"),
                 tags$b("Please do not use this on real patients at this time.")
             ))
    )
)

# Server logic
server <- function(input, output, session) {
    
    v <- reactiveValues()
    
    v$username <- ''
    v$user_email <- ''
    
    v$user_set <- as_tibble(unclass(read_sheet("https://docs.google.com/spreadsheets/d/1sQXS3FBIujmrGp0HWT7Qh0dcolkz583SDaCzXBHAMmI/edit#gid=0")), stringsAsFactors = TRUE)
    
    v$patient <- pt_generator()
    
    v$data_sheet <- as_tibble(unclass(read_sheet("https://docs.google.com/spreadsheets/d/1dD3uXgwLvEZDbXNUch99ytemNBijieprxWzwqI2Cm5Q/edit#gid=0")), stringsAsFactors = TRUE)
    v$num_treated <- 0
    
    #User management functions
    
    observeEvent(input$login_btn, {
        if(input$user_inpt %in% v$user_set$User) {
            if(input$email_inpt == v$user_set[[which(v$user_set$User == input$user_inpt), 'Email']]) {
                v$username <- input$user_inpt
                shinyjs::hide(id = "login_div")
                shinyjs::show(id = "logged_in_div")
                v$num_treated <- sum(v$data_sheet$User == v$username, na.rm = TRUE)
            }
            else {
                sendSweetAlert(
                    session = session,
                    title = "Login Error",
                    text = "Wrong username/e-mail combination. Please try again. Email me with any questions.",
                    type = "error"
                )
            }
        }
        else {
            v$username <- input$user_inpt
            v$user_email <- input$email_inpt
            shinyjs::hide(id = "login_div")
            shinyjs::show(id = "logged_in_div")
            v$num_treated <- 0 #Sheet filtered
            temp_frame <- data.frame(User = v$username, Email = v$user_email)
            sheet_append("https://docs.google.com/spreadsheets/d/1sQXS3FBIujmrGp0HWT7Qh0dcolkz583SDaCzXBHAMmI/edit#gid=0", temp_frame[1,])
        }
    })
    
    output$user_text <- renderText({
        str_c("Dr. ", str_to_title(v$username))
    })
    
    output$pts_seen <- renderText({
        str_c("Number of patients treated: ", v$num_treated)
    })
    
    #Patient info functions
    
    output$pt_narrative <- renderText({
        narrative_gen(v$patient)
    })
    
    output$pmhx <- renderText({
        pmhx_string <- str_c(ifelse(v$patient[[1,'Prematurity']], 'Prematurity, ', ''),
                             ifelse(v$patient[[1,'Downs']], 'Downs Syndrome, ', ''),
                             ifelse(v$patient[[1,'Reactive.Airways']], 'Reactive Airways, ', ''),
                             ifelse(v$patient[[1,'CHD']], 'Congenital Heart Disease, ', ''),
                             ifelse(v$patient[[1,'Bronchiolitis']], 'Bronchiolitis, ', ''),
                             ifelse(v$patient[[1,'Sickle.Cell']], 'Sickle Cell Disease, ', ''),
                             ifelse(v$patient[[1,'BPD']], 'Bronchopulmonary Dysplasia, ', ''),
                             ifelse(v$patient[[1,'Cystic.Fibrosis']], 'Cystic Fibrosis, ', ''),
                             ifelse(v$patient[[1,'Pneumonia']], 'Pneumonia, ', '')
                             )
        if(pmhx_string == '') {
            pmhx_string <- 'None'
        }
        if(str_detect(pmhx_string, ',')) {
            pmhx_string <- str_sub(pmhx_string, end = -3L)
        }
        pmhx_string
    })
    
    #This part generates the text for vital signs
    
    output$temp <- renderText({
        str_c("Temp: ", v$patient[[1,'Temp.C']], "C / ", v$patient[[1,'Temp.F']], "F")
    })
    
    output$pulse <- renderText({
        str_c("P: ", v$patient[[1,'HR']])
    })
    
    output$resps <- renderText({
        str_c("RR: ", v$patient[[1,'RR']])
    })
    
    output$bloodpressure <- renderText({
        str_c("SBP: ", v$patient[[1,'Systolic.BP']])
    })
    
    output$pulseox <- renderText({
        str_c("Pulse Ox: ", v$patient[[1,'Pulse.Ox']], "%")
    })
    
    #This part generates physical exam findings
    
    output$pe_resp <- renderText({
        abnl_string <- str_c(ifelse(v$patient[[1,'PE.Dyspnea']], 'dyspneic, ', ''),
                             ifelse(v$patient[[1,'PE.Wheezing']], 'wheezing, ', ''),
                             ifelse(v$patient[[1,'PE.RespDistress']], 'respiratory distress, ', ''),
                             ifelse(v$patient[[1,'PE.Retractions']], 'retracting, ', ''),
                             ifelse(v$patient[[1,'PE.Grunting']], 'grunting, ', ''),
                             ifelse(v$patient[[1,'PE.FocalDecrBS']], 'focal decreased breath sounds, ', ''),
                             ifelse(v$patient[[1,'PE.Rales']], 'rales, ', ''),
                             ifelse(v$patient[[1,'PE.FocalRales']], 'worse rales focally, ', ''))
        
        #This part cleanes the outputs for printing
        if(abnl_string == '') {
            temp_string <- 'Normal breath sounds, no distress'
        }
        else {
            temp_string <- str_sub(abnl_string, end = -3L)
            temp_string <- str_to_sentence(temp_string)
        }
        str_c("Respiratory: ", temp_string)
    })
    
    output$pe_cards <- renderText({
        temp_string <- ''
        if(v$patient[[1,'PE.Murmur']]) {
            temp_string <- "murmur"
        }
        else {
            temp_string <- "no murmurs"
        }
        str_c("Cardiac: Regular rhythm, ", temp_string)
    })
    
    output$pe_abd <- renderText({
        temp_string <- ''
        if(v$patient[[1,'PE.AbdDistension']]) {
            temp_string <- "Distended abdomen"
        }
        else {
            temp_string <- "Normal"
        }
        str_c("Abdomen: ", temp_string)
    })
    
    output$pe_heent <- renderText({
        temp_string <- ''
        if(v$patient[[1,'PE.MDry']]) {
            temp_string <- "Dry mucous membranes"
        }
        else {
            temp_string <- "Moist mucous membranes"
        }
        str_c("HEENT: ", temp_string)
    })
    
    output$pe_skin <- renderText({
        temp_string <- ''
        if(v$patient[[1,'PE.Pale']]) {
            temp_string <- "Pale"
        }
        else {
            temp_string <- "Normal"
        }
        str_c("Skin: ", temp_string)
    })
    
    output$pe_neuro <- renderText({
        temp_string <- ''
        if(v$patient[[1,'PE.Lethargy']]) {
            temp_string <- "Lethargic"
        }
        else {
            temp_string <- "Awake, alert"
        }
        str_c("Neuro: ", temp_string)
    })
    
    #CXR Buttons
    
    observeEvent(input$yes_cxr, {
        shinyjs::hide(id = "cxr_div")
        shinyjs::show(id = "thanks")
        v$num_treated <- v$num_treated + 1
        v$patient <- pt_generator()
        sendSweetAlert(
            session = session,
            title = "Great job!",
            text = "Time to see your next patient!",
            type = "success"
        )
        shinyjs::show(id = "cxr_div")
        shinyjs::hide(id = "thanks")
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    
    observeEvent(input$no_cxr, {
        shinyjs::hide(id = "cxr_div")
        shinyjs::show(id = "thanks")
        v$num_treated <- v$num_treated + 1
        v$patient <- pt_generator()
        sendSweetAlert(
            session = session,
            title = "Great job!",
            text = "Time to see your next patient!",
            type = "success"
        )
        shinyjs::show(id = "cxr_div")
        shinyjs::hide(id = "thanks")
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    
    #New patient button
    observeEvent(input$new_patient, {
        v$patient <- pt_generator()
        shinyjs::show(id = "cxr_div")
        shinyjs::hide(id = "thanks")
        shinyjs::runjs("window.scrollTo(0, 0)")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

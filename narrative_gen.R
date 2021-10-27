#This set of functions will generate the patient narrative using patient data.

narrative_gen <- function(patient) {
  ros_vec <- c('Cough', 'Runny Nose', 'Fever', 'Shortness of Breath', 'Congestion', 'Sore Throat', 'Lethargy', 'Chest Pain', 'Vomiting', 'Wheezing', 'Rash')
  working <- "The patient is a "
  working <- str_c(working, patient[1,'Age'], ' old ')
  working <- str_c(working, ifelse(patient[1,'Sex'] == 'F', 'female ', 'male '))
  working <- str_c(working, 'who presents with ', tolower(patient[1,'Chief.Complaint']), '. ')
  working <- str_c(working, 'The ', tolower(patient[1,'Chief.Complaint']), ' started ')
  working <- str_c(working, patient[1,'Onset'], ' ')
  working <- str_c(working, ifelse(patient[1,'Duration'] == 0, 'today ', str_c(patient[1,'Duration'], ' days ago. ')))
  working <- str_c(working, 'The ', tolower(patient[1,'Chief.Complaint']), ' is currently ', patient[1,'Severity'], ' in severity ')
  working <- str_c(working, 'and has been ', patient[1,'Course'], '.')
  working <- str_c(working, ' It is associated with ')
  ros_string <- ''
  for (i in 1:length(ros_vec)) {
    if(patient[[1,ros_vec[i]]] & (length(ros_string) == 1)) {
      ros_string <- str_c(ros_string, ros_vec)
    }
    else if(patient[[1,ros_vec[i]]]) {
      ros_string <- str_c(ros_string, " and ", ros_vec)
    }
  }
  if(length(ros_string) == 1) {
    ros_string <- 'nothing. '
  }
  working <- str_c(working, ' The patient has ', ifelse(patient[1,'Prior.Episodes'], '', 'never '), 'had prior similar episodes. ')
  return(working)
}
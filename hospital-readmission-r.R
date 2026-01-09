#############################################
# MAM02: Assignment 1
# Parul Nagar & Pamela Sneekes
# 06/10/2024
#############################################
# Note: 
# Observations regarding the final results
# The values we are getting are not exactly the same as the ones given in the table
# However, they are close to the results. 
# We were able to reproduce most of the table. The exception was the mean, median, and first and third quartile
# with respect to age. The age in the data set is mentioned as a range. 
# However, to calculate these values, we would require a single number. 
# We did calculate the time in hospital, but did not add this to the main dataframe,
# because the data types are not equivalent. The mean is in the same column as the number of encounters,
# and when you add them to the same column, the integers of the number of encounters would transform to float,
# and these numbers now appear as a decimal, which is not what we want.
# So that's why we have two dataframes which can be visualized.
# Table3 is the dataframe which contains most of the values in the table of the article,
# and time is the dataframe which consists of only the time in hospital.

#############################################
# Libraries
#############################################
library(dplyr)
library(kableExtra)
library(ggplot2)

#############################################
# Functions
#############################################
# check if variable is a table
check_table <- function(variable) {
  if (is.table(variable)) {
    variable <- sum(variable, na.rm=TRUE)
  }
  
  return(variable)
}

# calculate a percentage
calc_perc <- function(numerator, denominator) {
  return(round(numerator / denominator * 100, 1))
}

# add a list of values to a dataframe
add_df <- function(df, values) {
  df[nrow(df) + 1, ] <- values
  
  return(df)
}

# calculate the numbers for a specific value of a category
calc_val <- function(df1, df2, cat_value, col_name, group) {
  # calculate number of encounters and percentage of population
  n_encounter <- check_table(table(df2[[col_name]])[group])
  pop_size <- dim(data_diabetic_selected)[1]
  perc_pop <- calc_perc(n_encounter, pop_size)
  
  # calculate readmitted number of encounters and percentage of group
  readmit_n_encounter <- check_table(table(df2[df2$readmitted == "<30",][[col_name]])[group])
  readmit_perc_group <- calc_perc(readmit_n_encounter, n_encounter)
  
  # add values to df and return
  df1 <- add_df(df1, list(cat_value, n_encounter, perc_pop, readmit_n_encounter, readmit_perc_group))
  
  return(df1)
}

# calculate the values for all the values of a category
calc_cat <- function(table, data, values, col_name) {
  for (val in values) {
    # check if another dataframe was given as argument
    correct_df <- if ("df" %in% names(val)) val$df else data
    # calculate the numbers for this value of the category
    table3 <- calc_val(table3, correct_df, val$cat_val, col_name, val$group)
  }
  
  return(table3)
}


#############################################
# Import the data
#############################################
# Set your working directory to change to the folder where you downloaded the data file
# Thus replace the pathname H:/Downloads/ with the folder containing the data file
# Note that in Windows you have to use '/' instead of the normal '\' in the folder pathname
setwd("C:/Users/pamel/OneDrive/Documenten/Medische Informatiekunde/Master/MAM2/Assignment1")

# Import the data file
data_diabetic <- read.csv(file="diabetic_data_initial.csv", header=TRUE, sep=",")

# Let's quickly check the first few rows of the data
head(data_diabetic)

# Let's check the names of the variables (i.e., column names)
names(data_diabetic)

# How many rows are there?
dim(data_diabetic)[1]
# your answer: 101766

# Are these all unique encounters?
length(unique(data_diabetic$encounter_id))
# your answer: 101766, it's the same number as the number of rows, so they are all unique encounters

# And are they all unique patients?
length(unique(data_diabetic$patient_nbr))
# your answer: 71518, so multiple rows can represent the same patients


#############################################
# Select data
#############################################
# We have to remove some data according to the paper (read section 2.3 carefully)
# "... used only one encounter per patient ..."
# I found the way to do this on stackoverflow.com (remember this site because you can find a lot of answers on R)
# https://stackoverflow.com/questions/19944334/extract-rows-for-the-first-occurrence-of-a-variable-in-a-data-frame#19944458
data_diabetic_first_encounters <- data_diabetic[match(unique(data_diabetic$patient_nbr), data_diabetic$patient_nbr),]
# "... removed all encounters that resulted in either discharge to a hospice or patient death ..."
# The discharge_disposition_id column contains info on the discharge
# In id_mapping.csv we find discharge IDs 11 (Expired), 13 (Hospice / home), 14 (Hospice / medical facility)
names_removed_discharge_disposition_id <- c(11,13,14)
# Thus we remove these rows from the data
data_diabetic_selected = filter(data_diabetic_first_encounters, !data_diabetic_first_encounters$discharge_disposition_id %in% names_removed_discharge_disposition_id)

# Do we now have 69,984 encounters as mentioned in the paper?
dim(data_diabetic_selected)[1]
# your answer: No, we have 69980, it's 4 encounters less as mentioned in the paper.

# And let's quickly check if we now only have unique patients
length(unique(data_diabetic_selected$patient_nbr))
# your answer: 69980, it's same number as encounters, so we only have unique patients


#############################################
# Calculate descriptive statistics
#############################################
# create dataframe for table and rename dataframe used
table3 <- data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("Cat_val", "1", "2", "3", "4"))))
data <- data_diabetic_selected

# Create a list for each of the variables (e.g. HbA1C) with the possible values (cat_val) 
# with accompanying value in category (group) and adjusted data (df) if needed
HbA1c <- list(
  list(cat_val = "No test was performed", group = 3),
  list(cat_val = "Result was high but the diabetic medication was changed", group = 2, df = data[data$change == "Ch",]),
  list(cat_val = "Result was high but the diabetic medication was not changed", group = 2, df = data[data$change == "No",]),
  list(cat_val = "Normal result of the test", group = -c(2:3)))

# Gender
gender <- list(
  list(cat_val = "Female", group = 1),   list(cat_val = "Male", group = 2))

# Discharge disposition
discharge <- list(
  list(cat_val = "Discharged to home", group = 1),   list(cat_val = "Otherwise", group = -1)
)

# Admission source
admission <- list(
  list(cat_val = "Admitted from emergency room", group = 7),
  list(cat_val = "Admitted from because of physician/clinic referral", group = 1:2),
  list(cat_val = "Otherwise", group = -c(1, 2, 7)))

# Specialty of the admitting physician
# find all specialties that start with surg
surg_val <- unique(data[grep("Surg", data$medical_specialty),]$medical_specialty)
# combine the surgeon specialties with the specialties appearing in table3
combined <- c(surg_val, "?", "InternalMedicine", "Cardiology", "Family/GeneralPractice")
# get the remaining values of specialties for the last specialty 'Other'
remain <- setdiff(unique(data$medical_specialty), c(surg_val, combined))

specialty <- list(
  list(cat_val = "Internal Medicine", group = "InternalMedicine"), list(cat_val = "Cardiology", group = "Cardiology"),
  list(cat_val = "Surgery", group = surg_val), list(cat_val = "Family/general practice", group = "Family/GeneralPractice"),
  list(cat_val = "Missing or unknown", group = "?"), list(cat_val = "Other", group = remain)
)

# Primary diagnosis | # diabetes missing
# find all values representing diabetes that start with 250.
diab_val <- unique(data[grep("250.", data$diag_1),]$diag_1)
# find not used values which represents other
remain <- setdiff(unique(data$diag_1), c(diab_val, 140:239, 390:629, 710:739, 785:788, 800:999))

diagnosis <- list(
  list(cat_val = "A disease of the circulatory system", group = as.character(c(390:459, 785))),
  list(cat_val = "Diabetes", group = diab_val),
  list(cat_val = "A disease of the respiratory system", group = as.character(c(460:519, 786))),
  list(cat_val = "Diseases of the digestive system", group = as.character(c(520:579, 787))),
  list(cat_val = "Injury and poisoning", group = as.character(c(800:999))),
  list(cat_val = "Diseases of the musculoskeletal system and connective tissue", group = as.character(c(710:739))),
  list(cat_val = "Diseases of the genitourinary system", group = as.character(c(580:629, 788))),
  list(cat_val = "Neoplasms", group = as.character(c(140:239))),
  list(cat_val = "Other", group = remain)
)

# Race
race <- list(
  list(cat_val = "African American", group = 2), list(cat_val = "Caucasian", group = 4),
  list(cat_val = "Other", group = -c(1, 2, 4)), list(cat_val = "Missing", group = 1))

# Age
age <- list(
  list(cat_val = "30 years old or younger", group = 1:3), list(cat_val = "30-60 years old", group = 3:6),
  list(cat_val = "Older than 60", group = 7:10))

# Time
time_mean <- round(mean(data_diabetic_selected$time_in_hospital),1)
time_median <- median(data_diabetic_selected$time_in_hospital)
time_Q1 <- quantile(data_diabetic_selected$time_in_hospital)[2]
time_Q3 <- quantile(data_diabetic_selected$time_in_hospital)[4]

# calculate the numbers for all the categories using the list created above 
# and add the numbers to the table
table3 <- calc_cat(table3, data, HbA1c, "A1Cresult")
table3 <- calc_cat(table3, data, gender, "gender")
table3 <- calc_cat(table3, data, discharge, "discharge_disposition_id")
table3 <- calc_cat(table3, data, admission, "admission_source_id")
table3 <- calc_cat(table3, data, specialty, "medical_specialty")
table3 <- calc_cat(table3, data, diagnosis, "diag_1")
table3 <- calc_cat(table3, data, race, "race")
table3 <- calc_cat(table3, data, age, "age")

# create new dataframe for the time, otherwise the integers in table 3 will become decimals
time <- data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("Cat_val", "1", "2", "3", "4"))))
time <- add_df(time, list("Days between admission and discharge (1–14)", time_mean, time_median, time_Q1, time_Q3))

# Visualize the two dataframes
kbl(table3, col.names = c("Variable", "Number of encounters", "% of the population", "Number of encounters", "% in group"), caption = "Table 3: Distribution of variable values and readmissions (population size is 69,980).") %>%
  add_header_above(c(" " = 3, "Readmitted" = 2)) %>%
  kable_paper(full_width = F) %>%
  pack_rows(index = c("HbA1c" = 4, "Gender" = 2, "Discharge disposition" = 2, "Admission source" = 3, "Specialty of the admitting physician" = 6, "Primary diagnosis" = 9, "Race" = 4, "Age" = 3))

# time
kbl(time, col.names = c(" ", "Mean", "Median", "Q1", "Q3")) %>%
  kable_paper(full_width = F) %>%
  pack_rows(index = c("Time in hospital" = 1))

#Readmission and HbA1c
#to show the proportions of readmissions with relation to HbA1c testing
data_diabetic_selected$A1Cresult <- as.factor(data_diabetic_selected$A1Cresult)
levels(data_diabetic_selected$A1Cresult)
data_diabetic_selected$A1Cresult <- factor(data_diabetic_selected$A1Cresult, levels = c("Normal", ">7", ">8"))
table(data_diabetic_selected$A1Cresult, data_diabetic_selected$readmitted)
ggplot(data, aes(x = A1Cresult, fill = as.factor(readmitted))) +
  geom_bar(position = "fill") +
  labs(title = "Readmission Rate by HbA1c Category", x = "HbA1c Category", 
       y = "Proportion", fill = "Readmission")
#While some differences can be seen (the proportions of no admissions is the lowest in No testing)
#in this plot, a more thorough analysis would be required to get conclusive results regarding the relationship
#between HbA1C testing and readmission rates. 
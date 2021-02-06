"
Author: Arran J. Davis
Email: arran.davis@anthro.ox.ac.uk | davis.arran@gmail.com
Affiliation: Social Body Lab, Institute of Cognitive and Evolutionary Anthropology, University of Oxford
Date: 26/01/2021
"

#clean environment
rm(list= ls())

#set current working directory to the one this script is in (when in RStudio)
code_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(code_dir)

################################################################################################################################################

### THIS WILL CREATE A DATA FRAME OF THE VOLTAGE DATA (THE MEASURE OR PARTICIPANTS' PERFORMANCE ON THE EXERCISE TASK) ###

library(R.matlab)

#folder containing voltage data
datfolder = "../data/matlab_results/"

#this takes all the subfolders in the results folder 
subfolders = list.dirs(datfolder)
l = (1 + length(subfolders))
subfolders = subfolders[2:l]

#skip the last subfolder (NA)
subfolders = subfolders[1:(length(subfolders) - 1)]

#counter for the for loop below
count = 1

#list for all the numbers of voltage readings (the minumum per session)
totreadings = list()

#this will get the minimum number of voltage readings for a given trial in each experimental session
for (i in subfolders) {
  
  #track progress
  print(paste0("CURRENT FOLDER: ", i))
  
  #get the data 
  sub1 = subfolders[count]
  files1 = list.files(sub1)
  
  #this gets the MATLAB file with the voltage data
  D = readMat(paste(sub1, files1[1], sep = '/'))
  
  #this gets the voltage data (its a list of 42, since there are 42 trials)
  voltage = D$record.V
  
  #this for loop will get the number of voltage readings for each trial
  
  #this creates a list to see the minimum amount of voltage readings in the sessions (so the smallest number for the 42 trials)
  sess_readings = list()
  
  #counter for the for loop below
  count2 = 1
  
  #this will go through all the trials and get the number of readings
  for (i in voltage) {
    
    cur_trial = voltage[count2]
    
    trial_readings = ncol(as.data.frame(cur_trial))
    
    sess_readings = append(sess_readings, trial_readings)
    
    count2 = count2 +1
  }
  
  #flips 'sess_readings' so that its a matrix with one column and 42 rows
  sess_readings = t(sess_readings)
  
  #makes it a data frame
  p = as.data.frame(sess_readings)
  
  #flips it so that it has only one column
  p = t(p)
  
  #make data frame again
  p = as.data.frame(p)
  
  #convert from a list to numeric and get the minimum value (for the variable 'V1,' which is the only column)
  minimum_trial_readings = min(as.numeric(p$V1))
  
  #now add the minumum for this session to the list of all the minimum readings for all the sessions
  totreadings = append(totreadings, minimum_trial_readings)
  
  count = count + 1
}

#this gets the minimum reading from all trials from all the sessions for all participants
q = as.data.frame(totreadings)
q = t(q)
q = as.data.frame(q)
grand_minimum = min(as.numeric(q$V1))

################################################################################################################################################

### RESAMPLE ALL TRIALS SO THAT EACH TRIAL HAS THE GRAND MINIMUM NUMBER OF VOLTAGE READINGS (160 OR 20 READINGS PER SECOND) ###

#this creates an empty data frame to be filled with the resampled (160) voltage readings from each trial for each participant
total_data = data.frame()

#counter for the for loop below
count3 = 1

#this for loop will go through all the trials for each session for all the participants and resample the voltage readings
for (i in subfolders) {
  
  #set the seed for random sampling
  set.seed(count3)
  
  #track progress
  print(paste0("CURRENT FOLDER: ", i))
  
  #get the data
  sub1 = subfolders[count3]
  files1 = list.files(sub1)
  
  #this gets the MATLAB file with the voltage data
  D = readMat(paste(sub1, files1[1], sep = '/'))
  
  #this gets the MATLAB file with the other data (condition, etc)
  E = readMat(paste(sub1, files1[2], sep = '/'))
  
  tempdat = as.data.frame(E$record.dat[,1:21])
  tempdat$participant = E$subDetails[1]
  tempdat$placebo = E$subDetails[5]
  
  #variable names
  headings = c('trial', #1
               'supp_cont', #2 - 1 equals the supportive face, 2 equals the control face
               'start_time', #3
               'NAN1', #4
               'Q1_start_time', #5
               'Q2_start_time', #6
               'blank1', #7
               'q1_response_time', #8
               'q2_response_time', #9
               'blank2', #10
               'NAN2', #11
               'Q1_start_pos', #12
               'Q2_start_pos', #13
               'blank3', #14
               'Q1_ans_pos', #15
               'Q2_ans_pos', #16
               'blank4', #17
               'Q1_ans_perc', #18
               'Q2_ans_perc', #19
               'blank5', #20
               'trial_difficulty', #21
               'participant', #22
               'placebo') #23
  
  colnames(x = tempdat) = headings
  
  #this gets the current participant's number
  participant_number = as.character(tempdat$participant[1:1])
  
  #this gets the current participant's placebo condition
  participant_placebo = as.character(tempdat$placebo[1:1])
  
  #this gets the current participant's experimental session number (first or second)
  session_number = as.numeric(D$subDetails[4])
  
  #this gets the current participant's maximum grip strength reading (during the grip strength test)
  max_grip = as.numeric(D$subDetails[6])
  
  #this gets the current participant's minimum grip reading (during the grip strength test)
  min_grip = as.numeric(D$subDetails[7])
  
  #this gets the current participant's grip strength
  grip_strength = (min_grip - max_grip)
  
  #this gets the voltage data (its a list of 42, since there are 42 trials)
  voltage = D$record.V
  
  #counter for the for loop below
  count4 = 1
  
  #this for loop will go through all the trials in the current session and resample the voltage readings so that there are 160
  #it will then add these 160 readings to a data set along with the participant number and condition
  #there are 42 lists in voltage
  for (i in voltage) {
    
    #this gets the trial difficulty of the current trial (i will range from 1 to 42 - the number of trials per session)
    trial_diff = as.numeric(tempdat$trial_difficulty[count4])
    
    #this gets the face (support or control) for the current trial (it will range from 1 to 42 - the number of trials per session)
    support_control = as.numeric(tempdat$supp_cont[count4])
    
    #this gets the voltage data for the current trial
    current_dat = as.data.frame(voltage[count4])
    
    #this makes the data frame one variable
    current_dat = t(current_dat)
    
    #this renames the row names from "X1" to "1"
    row.names(current_dat) = sub("X","", row.names(current_dat))
    
    #this samples 160 voltage readings (they will not be in order in 'sampled_volt')
    sample_volt = as.data.frame(current_dat[sample(nrow(current_dat), 160), ])
    
    #this changes the name of the column
    colnames(sample_volt) = 'voltage'
    
    #this creates a variable that is the same as the row names, and then sorts by this variable (same as sorting by row)
    sample_volt$reading_number = as.numeric(row.names(sample_volt))
    ordered_sample_volt = sample_volt[order(sample_volt$reading_number), ]
    
    #this creates a variable that is 1 - 160 ('reading_number' will vary from 1 - 240)
    x = row(ordered_sample_volt)
    ordered_sample_volt["sample_reading_number"] = x
    
    #now add all the other variables for the particular trial
    
    #this adds the trial difficulty
    ordered_sample_volt["trial_difficulty"] = trial_diff
    
    #this adds if the trial was with a support or control face
    ordered_sample_volt["support_or_control"] = support_control
    
    #this adds whether the trial was in the placebo or control condition
    ordered_sample_volt["placebo_condition"] = participant_placebo
    
    #this adds the experimental session number of the trial (first or second)
    ordered_sample_volt["session_number"] = session_number
    
    #this adds the trial number
    ordered_sample_volt["trial_number"] = count4
    
    #this adds the maximum grip 
    ordered_sample_volt["max_grip"] = max_grip
    
    #this adds the minimum grip
    ordered_sample_volt["min_grip"] = min_grip
    
    #this adds the grip strength
    ordered_sample_volt["grip_strength"] = grip_strength
    
    #this adds the participant number (making it the first column)
    ordered_sample_volt = cbind(participant = participant_number, ordered_sample_volt)
    
    #add to counter
    count4 = count4 + 1
    
    #this adds the sample of voltage readings from the current trial to the data set with all samples from all the trials
    total_data = rbind(total_data, ordered_sample_volt)
    
    #clears the data from the last trial
    rm(current_dat)
  }
  
  count3 = count3 + 1
}

#some trials are labled 'OFF' instead of 'off' for 'placebo_condition' - this fixes the labeling
total_data$placebo_condition <- ifelse(total_data$placebo_condition == "OFF", "off", ifelse(total_data$placebo_condition == "on", "on", ifelse(total_data$placebo_condition == "off", "off", NA)))

#'total_data$sample_reading_number' is a matrix with two columns; extract the first column
total_data$sample_reading_number = total_data$sample_reading_number[,1]

################################################################################################################################################

### IDENTIFY ERRONEOUS VOLTAGE DATA ###

library(dplyr)

#create voltage range data for all participants
voltage_ranges = total_data %>% 
                 group_by(participant) %>%
                 summarise(volt_range = max(voltage) - min(voltage))

#examine the voltage ranges
hist(voltage_ranges$volt_range, breaks = 100)

#get the mean and SD of the voltage ranges
vr_mean = mean(voltage_ranges$volt_range)
vr_sd = sd(voltage_ranges$volt_range)

#find the outlying participant
outlier = subset(voltage_ranges, voltage_ranges$volt_range > (vr_mean + (vr_sd*2)))
print(paste0("OUTLIER VOLTAGE RANGE: ", mean(outlier$volt_range)))
print(paste0("VOLTAGE RANGE MEAN: ", vr_mean))
print(paste0("VOLTAGE SD: ", vr_sd))


#remove the outlying participant from the dataset
total_data = subset(total_data, participant != as.character(outlier$participant))
total_data$participant = droplevels(total_data$participant)

################################################################################################################################################

### CONVERT THE VOLTAGE DATA TO PERCENTAGE OF MAXIMUM GRIP STRENGTH ###

#'bar_unit' is a variable in the MATLAB script (lines 191-193) that is involved in converting voltage data for the pixel display; convert it back
total_data$bar_unit_easy = (300 * (50/40)) / (total_data$min_grip - total_data$max_grip)
total_data$bar_unit_medium = (300 / (total_data$min_grip - total_data$max_grip))
total_data$bar_unit_hard= (300 * (50/60)) / (total_data$min_grip - total_data$max_grip)

#'Grip_px' is a variable in the MATLAB script (line 303) that is involved in converting voltage data for the pixel display; convert it back
total_data$grip_px = 750 - total_data$voltage

#'Grip_dV' is a variable in the MATLAB script (line 320) that is involved in converting voltage data for the pixel display; convert it back
total_data$trial_difficulty = as.numeric(total_data$trial_difficulty)
total_data$grip_dv = ifelse(total_data$trial_difficulty == 1, (total_data$grip_px / total_data$bar_unit_easy), ifelse(total_data$trial_difficulty == 2, (total_data$grip_px / total_data$bar_unit_medium), ifelse(total_data$trial_difficulty == 3, (total_data$grip_px / total_data$bar_unit_hard), NA)))

#get the actual voltage reading (instead of the pixel at which the bar is displayed)
total_data$real_voltage = -1 * (total_data$grip_dv - total_data$min_grip)

#get the percentage of each participant's maximum grip strength for the current reading (i.e., the percent of their total grip strength they are currently performing at)
total_data$percent_of_maximum = (total_data$min_grip - total_data$real_voltage) / (total_data$min_grip - total_data$max_grip)

################################################################################################################################################

### MERGE THE MATLAB DATA WITH THE QUALTRICS DATA (THE PRE AND POST-EXPERIMENT QUESTIONS THAT THE PARTICIPANTS ANSWERED) ###

library(anchors)

#load the Qualtrics data
survey_dat = read.csv("../data/Qualtrics_data_processed/qualtrics_data.csv", sep = ",")

### BIG-FIVE INVENTORY (BFI) PERSONALITY TEST ###

#recode all answers to the BFI so that they are numeric (note that some questions are reversed scored)
survey_dat$talkative <- ifelse(survey_dat$talkative == "Disagree strongly", 1, ifelse(survey_dat$talkative == "Disagree a little", 2, ifelse(survey_dat$talkative == "Neither agree nor disagree", 3, ifelse(survey_dat$talkative == "Agree a little", 4, ifelse(survey_dat$talkative == "Strongly agree", 5, NA)))))
survey_dat$fault_with_others <- ifelse(survey_dat$fault_with_others == "Disagree strongly", 5, ifelse(survey_dat$fault_with_others == "Disagree a little", 4, ifelse(survey_dat$fault_with_others == "Neither agree nor disagree", 3, ifelse(survey_dat$fault_with_others == "Agree a little", 2, ifelse(survey_dat$fault_with_others == "Strongly agree", 1, NA)))))
survey_dat$thorough_job <- ifelse(survey_dat$thorough_job == "Disagree strongly", 1, ifelse(survey_dat$thorough_job == "Disagree a little", 2, ifelse(survey_dat$thorough_job == "Neither agree nor disagree", 3, ifelse(survey_dat$thorough_job == "Agree a little", 4, ifelse(survey_dat$thorough_job == "Strongly agree", 5, NA)))))
survey_dat$depressed <- ifelse(survey_dat$depressed == "Disagree strongly", 1, ifelse(survey_dat$depressed == "Disagree a little", 2, ifelse(survey_dat$depressed == "Neither agree nor disagree", 3, ifelse(survey_dat$depressed == "Agree a little", 4, ifelse(survey_dat$depressed == "Strongly agree", 5, NA)))))
survey_dat$original <- ifelse(survey_dat$original == "Disagree strongly", 1, ifelse(survey_dat$original == "Disagree a little", 2, ifelse(survey_dat$original == "Neither agree nor disagree", 3, ifelse(survey_dat$original == "Agree a little", 4, ifelse(survey_dat$original == "Strongly agree", 5, NA)))))
survey_dat$reserved <- ifelse(survey_dat$reserved == "Disagree strongly", 5, ifelse(survey_dat$reserved == "Disagree a little", 4, ifelse(survey_dat$reserved == "Neither agree nor disagree", 3, ifelse(survey_dat$reserved == "Agree a little", 2, ifelse(survey_dat$reserved == "Strongly agree", 1, NA)))))
survey_dat$helpful <- ifelse(survey_dat$helpful == "Disagree strongly", 1, ifelse(survey_dat$helpful == "Disagree a little", 2, ifelse(survey_dat$helpful == "Neither agree nor disagree", 3, ifelse(survey_dat$helpful == "Agree a little", 4, ifelse(survey_dat$helpful == "Strongly agree", 5, NA)))))
survey_dat$careless <- ifelse(survey_dat$careless == "Disagree strongly", 5, ifelse(survey_dat$careless == "Disagree a little", 4, ifelse(survey_dat$careless == "Neither agree nor disagree", 3, ifelse(survey_dat$careless == "Agree a little", 2, ifelse(survey_dat$careless == "Strongly agree", 1, NA)))))
survey_dat$relaxed <- ifelse(survey_dat$relaxed == "Disagree strongly", 5, ifelse(survey_dat$relaxed == "Disagree a little", 4, ifelse(survey_dat$relaxed == "Neither agree nor disagree", 3, ifelse(survey_dat$relaxed == "Agree a little", 2, ifelse(survey_dat$relaxed == "Strongly agree", 1, NA)))))
survey_dat$curious <- ifelse(survey_dat$curious == "Disagree strongly", 1, ifelse(survey_dat$curious == "Disagree a little", 2, ifelse(survey_dat$curious == "Neither agree nor disagree", 3, ifelse(survey_dat$curious == "Agree a little", 4, ifelse(survey_dat$curious == "Strongly agree", 5, NA)))))
survey_dat$energy <- ifelse(survey_dat$energy == "Disagree strongly", 1, ifelse(survey_dat$energy == "Disagree a little", 2, ifelse(survey_dat$energy == "Neither agree nor disagree", 3, ifelse(survey_dat$energy == "Agree a little", 4, ifelse(survey_dat$energy == "Strongly agree", 5, NA)))))
survey_dat$quarrels <- ifelse(survey_dat$quarrels == "Disagree strongly", 5, ifelse(survey_dat$quarrels == "Disagree a little", 4, ifelse(survey_dat$quarrels == "Neither agree nor disagree", 3, ifelse(survey_dat$quarrels == "Agree a little", 2, ifelse(survey_dat$quarrels == "Strongly agree", 1, NA)))))
survey_dat$reliable <- ifelse(survey_dat$reliable == "Disagree strongly", 1, ifelse(survey_dat$reliable == "Disagree a little", 2, ifelse(survey_dat$reliable == "Neither agree nor disagree", 3, ifelse(survey_dat$reliable == "Agree a little", 4, ifelse(survey_dat$reliable == "Strongly agree", 5, NA)))))
survey_dat$tense <- ifelse(survey_dat$tense == "Disagree strongly", 1, ifelse(survey_dat$tense == "Disagree a little", 2, ifelse(survey_dat$tense == "Neither agree nor disagree", 3, ifelse(survey_dat$tense == "Agree a little", 4, ifelse(survey_dat$tense == "Strongly agree", 5, NA)))))
survey_dat$ingenious <- ifelse(survey_dat$ingenious == "Disagree strongly", 1, ifelse(survey_dat$ingenious == "Disagree a little", 2, ifelse(survey_dat$ingenious == "Neither agree nor disagree", 3, ifelse(survey_dat$ingenious == "Agree a little", 4, ifelse(survey_dat$ingenious == "Strongly agree", 5, NA)))))
survey_dat$enthusiam <- ifelse(survey_dat$enthusiam == "Disagree strongly", 1, ifelse(survey_dat$enthusiam == "Disagree a little", 2, ifelse(survey_dat$enthusiam == "Neither agree nor disagree", 3, ifelse(survey_dat$enthusiam == "Agree a little", 4, ifelse(survey_dat$enthusiam == "Strongly agree", 5, NA)))))
survey_dat$forgiving <- ifelse(survey_dat$forgiving == "Disagree strongly", 1, ifelse(survey_dat$forgiving == "Disagree a little", 2, ifelse(survey_dat$forgiving == "Neither agree nor disagree", 3, ifelse(survey_dat$forgiving == "Agree a little", 4, ifelse(survey_dat$forgiving == "Strongly agree", 5, NA)))))
survey_dat$disorganised <- ifelse(survey_dat$disorganised == "Disagree strongly", 5, ifelse(survey_dat$disorganised == "Disagree a little", 4, ifelse(survey_dat$disorganised == "Neither agree nor disagree", 3, ifelse(survey_dat$disorganised == "Agree a little", 2, ifelse(survey_dat$disorganised == "Strongly agree", 1, NA)))))
survey_dat$worries <- ifelse(survey_dat$worries == "Disagree strongly", 1, ifelse(survey_dat$worries == "Disagree a little", 2, ifelse(survey_dat$worries == "Neither agree nor disagree", 3, ifelse(survey_dat$worries == "Agree a little", 4, ifelse(survey_dat$worries == "Strongly agree", 5, NA)))))
survey_dat$active_imagination <- ifelse(survey_dat$active_imagination == "Disagree strongly", 1, ifelse(survey_dat$active_imagination == "Disagree a little", 2, ifelse(survey_dat$active_imagination == "Neither agree nor disagree", 3, ifelse(survey_dat$active_imagination == "Agree a little", 4, ifelse(survey_dat$active_imagination == "Strongly agree", 5, NA)))))
survey_dat$quiet <- ifelse(survey_dat$quiet == "Disagree strongly", 5, ifelse(survey_dat$quiet == "Disagree a little", 4, ifelse(survey_dat$quiet == "Neither agree nor disagree", 3, ifelse(survey_dat$quiet == "Agree a little", 2, ifelse(survey_dat$quiet == "Strongly agree", 1, NA)))))
survey_dat$trusting <- ifelse(survey_dat$trusting == "Disagree strongly", 1, ifelse(survey_dat$trusting == "Disagree a little", 2, ifelse(survey_dat$trusting == "Neither agree nor disagree", 3, ifelse(survey_dat$trusting == "Agree a little", 4, ifelse(survey_dat$trusting == "Strongly agree", 5, NA)))))
survey_dat$lazy <- ifelse(survey_dat$lazy == "Disagree strongly", 5, ifelse(survey_dat$lazy == "Disagree a little", 4, ifelse(survey_dat$lazy == "Neither agree nor disagree", 3, ifelse(survey_dat$lazy == "Agree a little", 2, ifelse(survey_dat$lazy == "Strongly agree", 1, NA)))))
survey_dat$emotionally_stable <- ifelse(survey_dat$emotionally_stable == "Disagree strongly", 5, ifelse(survey_dat$emotionally_stable == "Disagree a little", 4, ifelse(survey_dat$emotionally_stable == "Neither agree nor disagree", 3, ifelse(survey_dat$emotionally_stable == "Agree a little", 2, ifelse(survey_dat$emotionally_stable == "Strongly agree", 1, NA)))))
survey_dat$inventive <- ifelse(survey_dat$inventive == "Disagree strongly", 1, ifelse(survey_dat$inventive == "Disagree a little", 2, ifelse(survey_dat$inventive == "Neither agree nor disagree", 3, ifelse(survey_dat$inventive == "Agree a little", 4, ifelse(survey_dat$inventive == "Strongly agree", 5, NA)))))
survey_dat$assertive <- ifelse(survey_dat$assertive == "Disagree strongly", 1, ifelse(survey_dat$assertive == "Disagree a little", 2, ifelse(survey_dat$assertive == "Neither agree nor disagree", 3, ifelse(survey_dat$assertive == "Agree a little", 4, ifelse(survey_dat$assertive == "Strongly agree", 5, NA)))))
survey_dat$cold <- ifelse(survey_dat$cold == "Disagree strongly", 5, ifelse(survey_dat$cold == "Disagree a little", 4, ifelse(survey_dat$cold == "Neither agree nor disagree", 3, ifelse(survey_dat$cold == "Agree a little", 2, ifelse(survey_dat$cold == "Strongly agree", 1, NA)))))
survey_dat$perserveres <- ifelse(survey_dat$perserveres == "Disagree strongly", 1, ifelse(survey_dat$perserveres == "Disagree a little", 2, ifelse(survey_dat$perserveres == "Neither agree nor disagree", 3, ifelse(survey_dat$perserveres == "Agree a little", 4, ifelse(survey_dat$perserveres == "Strongly agree", 5, NA)))))
survey_dat$moody <- ifelse(survey_dat$moody == "Disagree strongly", 1, ifelse(survey_dat$moody == "Disagree a little", 2, ifelse(survey_dat$moody == "Neither agree nor disagree", 3, ifelse(survey_dat$moody == "Agree a little", 4, ifelse(survey_dat$moody == "Strongly agree", 5, NA)))))
survey_dat$values_art <- ifelse(survey_dat$values_art == "Disagree strongly", 1, ifelse(survey_dat$values_art == "Disagree a little", 2, ifelse(survey_dat$values_art == "Neither agree nor disagree", 3, ifelse(survey_dat$values_art == "Agree a little", 4, ifelse(survey_dat$values_art == "Strongly agree", 5, NA)))))
survey_dat$shy <- ifelse(survey_dat$shy == "Disagree strongly", 5, ifelse(survey_dat$shy == "Disagree a little", 4, ifelse(survey_dat$shy == "Neither agree nor disagree", 3, ifelse(survey_dat$shy == "Agree a little", 2, ifelse(survey_dat$shy == "Strongly agree", 1, NA)))))
survey_dat$considerate <- ifelse(survey_dat$considerate == "Disagree strongly", 1, ifelse(survey_dat$considerate == "Disagree a little", 2, ifelse(survey_dat$considerate == "Neither agree nor disagree", 3, ifelse(survey_dat$considerate == "Agree a little", 4, ifelse(survey_dat$considerate == "Strongly agree", 5, NA)))))
survey_dat$efficient <- ifelse(survey_dat$efficient == "Disagree strongly", 1, ifelse(survey_dat$efficient == "Disagree a little", 2, ifelse(survey_dat$efficient == "Neither agree nor disagree", 3, ifelse(survey_dat$efficient == "Agree a little", 4, ifelse(survey_dat$efficient == "Strongly agree", 5, NA)))))
survey_dat$calm <- ifelse(survey_dat$calm == "Disagree strongly", 5, ifelse(survey_dat$calm == "Disagree a little", 4, ifelse(survey_dat$calm == "Neither agree nor disagree", 3, ifelse(survey_dat$calm == "Agree a little", 2, ifelse(survey_dat$calm == "Strongly agree", 1, NA)))))
survey_dat$prefers_routine_work <- ifelse(survey_dat$prefers_routine_work == "Disagree strongly", 5, ifelse(survey_dat$prefers_routine_work == "Disagree a little", 4, ifelse(survey_dat$prefers_routine_work == "Neither agree nor disagree", 3, ifelse(survey_dat$prefers_routine_work == "Agree a little", 2, ifelse(survey_dat$prefers_routine_work == "Strongly agree", 1, NA)))))
survey_dat$outgoing <- ifelse(survey_dat$outgoing == "Disagree strongly", 1, ifelse(survey_dat$outgoing == "Disagree a little", 2, ifelse(survey_dat$outgoing == "Neither agree nor disagree", 3, ifelse(survey_dat$outgoing == "Agree a little", 4, ifelse(survey_dat$outgoing == "Strongly agree", 5, NA)))))
survey_dat$rude <- ifelse(survey_dat$rude == "Disagree strongly", 5, ifelse(survey_dat$rude == "Disagree a little", 4, ifelse(survey_dat$rude == "Neither agree nor disagree", 3, ifelse(survey_dat$rude == "Agree a little", 2, ifelse(survey_dat$rude == "Strongly agree", 1, NA)))))
survey_dat$makes_plans <- ifelse(survey_dat$makes_plans == "Disagree strongly", 1, ifelse(survey_dat$makes_plans == "Disagree a little", 2, ifelse(survey_dat$makes_plans == "Neither agree nor disagree", 3, ifelse(survey_dat$makes_plans == "Agree a little", 4, ifelse(survey_dat$makes_plans == "Strongly agree", 5, NA)))))
survey_dat$get_nervous <- ifelse(survey_dat$get_nervous == "Disagree strongly", 1, ifelse(survey_dat$get_nervous == "Disagree a little", 2, ifelse(survey_dat$get_nervous == "Neither agree nor disagree", 3, ifelse(survey_dat$get_nervous == "Agree a little", 4, ifelse(survey_dat$get_nervous == "Strongly agree", 5, NA)))))
survey_dat$likes_to_reflect <- ifelse(survey_dat$likes_to_reflect == "Disagree strongly", 1, ifelse(survey_dat$likes_to_reflect == "Disagree a little", 2, ifelse(survey_dat$likes_to_reflect == "Neither agree nor disagree", 3, ifelse(survey_dat$likes_to_reflect == "Agree a little", 4, ifelse(survey_dat$likes_to_reflect == "Strongly agree", 5, NA)))))
survey_dat$few_artistic_interests <- ifelse(survey_dat$few_artistic_interests == "Disagree strongly", 5, ifelse(survey_dat$few_artistic_interests == "Disagree a little", 4, ifelse(survey_dat$few_artistic_interests == "Neither agree nor disagree", 3, ifelse(survey_dat$few_artistic_interests == "Agree a little", 2, ifelse(survey_dat$few_artistic_interests == "Strongly agree", 1, NA)))))
survey_dat$cooperative <- ifelse(survey_dat$cooperative == "Disagree strongly", 1, ifelse(survey_dat$cooperative == "Disagree a little", 2, ifelse(survey_dat$cooperative == "Neither agree nor disagree", 3, ifelse(survey_dat$cooperative == "Agree a little", 4, ifelse(survey_dat$cooperative == "Strongly agree", 5, NA)))))
survey_dat$distracted <- ifelse(survey_dat$distracted == "Disagree strongly", 5, ifelse(survey_dat$distracted == "Disagree a little", 4, ifelse(survey_dat$distracted == "Neither agree nor disagree", 3, ifelse(survey_dat$distracted == "Agree a little", 2, ifelse(survey_dat$distracted == "Strongly agree", 1, NA)))))
survey_dat$sophisticated <- ifelse(survey_dat$sophisticated == "Disagree strongly", 1, ifelse(survey_dat$sophisticated == "Disagree a little", 2, ifelse(survey_dat$sophisticated == "Neither agree nor disagree", 3, ifelse(survey_dat$sophisticated == "Agree a little", 4, ifelse(survey_dat$sophisticated == "Strongly agree", 5, NA)))))

#this will create the participants' extraversion score
survey_dat$extraversion = (survey_dat$talkative + survey_dat$reserved + survey_dat$energy + survey_dat$enthusiam + survey_dat$quiet + survey_dat$assertive + survey_dat$shy + survey_dat$outgoing)/8

#this will create the participants' agreeableness score
survey_dat$agreeableness = (survey_dat$fault_with_others + survey_dat$helpful + survey_dat$quarrels + survey_dat$forgiving + survey_dat$trusting + survey_dat$cold + survey_dat$considerate + survey_dat$rude + survey_dat$cooperative)/9

#this will create the participants' conscientiousness score
survey_dat$conscientiousness = (survey_dat$thorough_job + survey_dat$careless + survey_dat$reliable + survey_dat$disorganised + survey_dat$lazy + survey_dat$perserveres + survey_dat$efficient + survey_dat$makes_plans + survey_dat$distracted)/9

#this will create the participants' neuroticism score
survey_dat$neuroticism = (survey_dat$depressed + survey_dat$relaxed + survey_dat$tense + survey_dat$worries + survey_dat$emotionally_stable + survey_dat$moody + survey_dat$calm + survey_dat$get_nervous)/8

#this will create the participants' openness score
survey_dat$openness = (survey_dat$original + survey_dat$curious + survey_dat$ingenious + survey_dat$active_imagination + survey_dat$inventive + survey_dat$values_art + survey_dat$prefers_routine_work + survey_dat$likes_to_reflect + survey_dat$few_artistic_interests + survey_dat$sophisticated)/10

### REVISED ADULT ATTACHMENT SCALE (RAAS) ###

#recode all answers to the RAAS so that they are numeric (note that some questions are reversed scored)
survey_dat$easy_to_get_close <- ifelse(survey_dat$easy_to_get_close == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$easy_to_get_close == "-2", 2, ifelse(survey_dat$easy_to_get_close == "-3", 3, ifelse(survey_dat$easy_to_get_close == "-4", 4, ifelse(survey_dat$easy_to_get_close == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$difficult_depending_on_others <- ifelse(survey_dat$difficult_depending_on_others == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$difficult_depending_on_others == "-2", 4, ifelse(survey_dat$difficult_depending_on_others == "-3", 3, ifelse(survey_dat$difficult_depending_on_others == "-4", 2, ifelse(survey_dat$difficult_depending_on_others == "Very characteristic of me\n(5)\n", 1, NA)))))
survey_dat$worry_people_dont_love_me <- ifelse(survey_dat$worry_people_dont_love_me == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$worry_people_dont_love_me == "-2", 2, ifelse(survey_dat$worry_people_dont_love_me == "-3", 3, ifelse(survey_dat$worry_people_dont_love_me == "-4", 4, ifelse(survey_dat$worry_people_dont_love_me == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$others_reluctant_to_get_close <- ifelse(survey_dat$others_reluctant_to_get_close == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$others_reluctant_to_get_close == "-2", 2, ifelse(survey_dat$others_reluctant_to_get_close == "-3", 3, ifelse(survey_dat$others_reluctant_to_get_close == "-4", 4, ifelse(survey_dat$others_reluctant_to_get_close == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$comfortable_depending_on_others <- ifelse(survey_dat$comfortable_depending_on_others == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$comfortable_depending_on_others == "-2", 2, ifelse(survey_dat$comfortable_depending_on_others == "-3", 3, ifelse(survey_dat$comfortable_depending_on_others == "-4", 4, ifelse(survey_dat$comfortable_depending_on_others == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$dont_worry_about_people_getting_too_close <- ifelse(survey_dat$dont_worry_about_people_getting_too_close == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$dont_worry_about_people_getting_too_close == "-2", 2, ifelse(survey_dat$dont_worry_about_people_getting_too_close == "-3", 3, ifelse(survey_dat$dont_worry_about_people_getting_too_close == "-4", 4, ifelse(survey_dat$dont_worry_about_people_getting_too_close == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$people_never_there_when_needed <- ifelse(survey_dat$people_never_there_when_needed == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$people_never_there_when_needed == "-2", 4, ifelse(survey_dat$people_never_there_when_needed == "-3", 3, ifelse(survey_dat$people_never_there_when_needed == "-4", 2, ifelse(survey_dat$people_never_there_when_needed == "Very characteristic of me\n(5)\n", 1, NA)))))
survey_dat$uncomfortable_being_close_to_others <- ifelse(survey_dat$uncomfortable_being_close_to_others == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$uncomfortable_being_close_to_others == "-2", 4, ifelse(survey_dat$uncomfortable_being_close_to_others == "-3", 3, ifelse(survey_dat$uncomfortable_being_close_to_others == "-4", 2, ifelse(survey_dat$uncomfortable_being_close_to_others == "Very characteristic of me\n(5)\n", 1, NA)))))
survey_dat$worry_people_wont_stay_with_me <- ifelse(survey_dat$worry_people_wont_stay_with_me == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$worry_people_wont_stay_with_me == "-2", 2, ifelse(survey_dat$worry_people_wont_stay_with_me == "-3", 3, ifelse(survey_dat$worry_people_wont_stay_with_me == "-4", 4, ifelse(survey_dat$worry_people_wont_stay_with_me == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$afraid_feelings_not_reciprocated <- ifelse(survey_dat$afraid_feelings_not_reciprocated == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$afraid_feelings_not_reciprocated == "-2", 2, ifelse(survey_dat$afraid_feelings_not_reciprocated == "-3", 3, ifelse(survey_dat$afraid_feelings_not_reciprocated == "-4", 4, ifelse(survey_dat$afraid_feelings_not_reciprocated == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$wonder_whether_people_really_care <- ifelse(survey_dat$wonder_whether_people_really_care == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$wonder_whether_people_really_care == "-2", 2, ifelse(survey_dat$wonder_whether_people_really_care == "-3", 3, ifelse(survey_dat$wonder_whether_people_really_care == "-4", 4, ifelse(survey_dat$wonder_whether_people_really_care == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$comfortable_developing_close_relationships <- ifelse(survey_dat$comfortable_developing_close_relationships == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$comfortable_developing_close_relationships == "-2", 2, ifelse(survey_dat$comfortable_developing_close_relationships == "-3", 3, ifelse(survey_dat$comfortable_developing_close_relationships == "-4", 4, ifelse(survey_dat$comfortable_developing_close_relationships == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$uncomfortable_with_people_getting_too_emotionally_close <- ifelse(survey_dat$uncomfortable_with_people_getting_too_emotionally_close == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$uncomfortable_with_people_getting_too_emotionally_close == "-2", 4, ifelse(survey_dat$uncomfortable_with_people_getting_too_emotionally_close == "-3", 3, ifelse(survey_dat$uncomfortable_with_people_getting_too_emotionally_close == "-4", 2, ifelse(survey_dat$uncomfortable_with_people_getting_too_emotionally_close == "Very characteristic of me\n(5)\n", 1, NA)))))
survey_dat$people_there_when_needed <- ifelse(survey_dat$people_there_when_needed == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$people_there_when_needed == "-2", 2, ifelse(survey_dat$people_there_when_needed == "-3", 3, ifelse(survey_dat$people_there_when_needed == "-4", 4, ifelse(survey_dat$people_there_when_needed == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$worry_about_being_hurt <- ifelse(survey_dat$worry_about_being_hurt == "Not at all characteristic of me\n(1)\n", 1, ifelse(survey_dat$worry_about_being_hurt == "-2", 2, ifelse(survey_dat$worry_about_being_hurt == "-3", 3, ifelse(survey_dat$worry_about_being_hurt == "-4", 4, ifelse(survey_dat$worry_about_being_hurt == "Very characteristic of me\n(5)\n", 5, NA)))))
survey_dat$difficult_to_trust_others <- ifelse(survey_dat$difficult_to_trust_others == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$difficult_to_trust_others == "-2", 4, ifelse(survey_dat$difficult_to_trust_others == "-3", 3, ifelse(survey_dat$difficult_to_trust_others == "-4", 2, ifelse(survey_dat$difficult_to_trust_others == "Very characteristic of me\n(5)\n", 1, NA)))))
survey_dat$people_want_to_be_closer_than_wanted <- ifelse(survey_dat$people_want_to_be_closer_than_wanted == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$people_want_to_be_closer_than_wanted == "-2", 4, ifelse(survey_dat$people_want_to_be_closer_than_wanted == "-3", 3, ifelse(survey_dat$people_want_to_be_closer_than_wanted == "-4", 2, ifelse(survey_dat$people_want_to_be_closer_than_wanted == "Very characteristic of me\n(5)\n", 1, NA)))))
survey_dat$not_sure_if_people_can_be_depended_on <- ifelse(survey_dat$not_sure_if_people_can_be_depended_on == "Not at all characteristic of me\n(1)\n", 5, ifelse(survey_dat$not_sure_if_people_can_be_depended_on == "-2", 4, ifelse(survey_dat$not_sure_if_people_can_be_depended_on == "-3", 3, ifelse(survey_dat$not_sure_if_people_can_be_depended_on == "-4", 2, ifelse(survey_dat$not_sure_if_people_can_be_depended_on == "Very characteristic of me\n(5)\n", 1, NA)))))

#this will create the participants' score on the 'CLOSE' subscale
survey_dat$close = (survey_dat$easy_to_get_close + survey_dat$dont_worry_about_people_getting_too_close + survey_dat$uncomfortable_being_close_to_others + survey_dat$comfortable_developing_close_relationships + survey_dat$uncomfortable_with_people_getting_too_emotionally_close + survey_dat$people_want_to_be_closer_than_wanted)/6

#this will create the participants' score on the 'DEPEND' subscale
survey_dat$depend = (survey_dat$difficult_depending_on_others + survey_dat$comfortable_depending_on_others + survey_dat$people_never_there_when_needed + survey_dat$people_there_when_needed + survey_dat$difficult_to_trust_others + survey_dat$not_sure_if_people_can_be_depended_on)/6

#this will create the participants' score on the 'ANXIETY' subscale
survey_dat$anxiety = (survey_dat$worry_people_dont_love_me + survey_dat$others_reluctant_to_get_close + survey_dat$worry_people_wont_stay_with_me + survey_dat$afraid_feelings_not_reciprocated + survey_dat$wonder_whether_people_really_care + survey_dat$worry_about_being_hurt)/6

### FEAR OF PAIN QUESTIONNAIRE (FPQ) ###

#recode all answers to the FPQ so that they are numeric (the questionnare was modified to include a tenth item related to muscle cramps)
survey_dat$breaking_arm = ifelse(survey_dat$breaking_arm == "Not at all", 1, ifelse(survey_dat$breaking_arm == "A little", 2, ifelse(survey_dat$breaking_arm == "A fair amount", 3, ifelse(survey_dat$breaking_arm == "Very much", 4, ifelse(survey_dat$breaking_arm == "Extreme", 5, NA)))))
survey_dat$wart_removal = ifelse(survey_dat$wart_removal == "Not at all", 1, ifelse(survey_dat$wart_removal == "A little", 2, ifelse(survey_dat$wart_removal == "A fair amount", 3, ifelse(survey_dat$wart_removal == "Very much", 4, ifelse(survey_dat$wart_removal == "Extreme", 5, NA)))))
survey_dat$paper_cut = ifelse(survey_dat$paper_cut == "Not at all", 1, ifelse(survey_dat$paper_cut == "A little", 2, ifelse(survey_dat$paper_cut == "A fair amount", 3, ifelse(survey_dat$paper_cut == "Very much", 4, ifelse(survey_dat$paper_cut == "Extreme", 5, NA)))))
survey_dat$injection_in_mouth = ifelse(survey_dat$injection_in_mouth == "Not at all", 1, ifelse(survey_dat$injection_in_mouth == "A little", 2, ifelse(survey_dat$injection_in_mouth == "A fair amount", 3, ifelse(survey_dat$injection_in_mouth == "Very much", 4, ifelse(survey_dat$injection_in_mouth == "Extreme", 5, NA)))))
survey_dat$soap_in_eyes = ifelse(survey_dat$soap_in_eyes == "Not at all", 1, ifelse(survey_dat$soap_in_eyes == "A little", 2, ifelse(survey_dat$soap_in_eyes == "A fair amount", 3, ifelse(survey_dat$soap_in_eyes == "Very much", 4, ifelse(survey_dat$soap_in_eyes == "Extreme", 5, NA)))))
survey_dat$slam_door_on_hand = ifelse(survey_dat$slam_door_on_hand == "Not at all", 1, ifelse(survey_dat$slam_door_on_hand == "A little", 2, ifelse(survey_dat$slam_door_on_hand == "A fair amount", 3, ifelse(survey_dat$slam_door_on_hand == "Very much", 4, ifelse(survey_dat$slam_door_on_hand == "Extreme", 5, NA)))))
survey_dat$gulping_hot_drink = ifelse(survey_dat$gulping_hot_drink == "Not at all", 1, ifelse(survey_dat$gulping_hot_drink == "A little", 2, ifelse(survey_dat$gulping_hot_drink == "A fair amount", 3, ifelse(survey_dat$gulping_hot_drink == "Very much", 4, ifelse(survey_dat$gulping_hot_drink == "Extreme", 5, NA)))))
survey_dat$injection_in_hip = ifelse(survey_dat$injection_in_hip == "Not at all", 1, ifelse(survey_dat$injection_in_hip == "A little", 2, ifelse(survey_dat$injection_in_hip == "A fair amount", 3, ifelse(survey_dat$injection_in_hip == "Very much", 4, ifelse(survey_dat$injection_in_hip == "Extreme", 5, NA)))))
survey_dat$falling_down = ifelse(survey_dat$falling_down == "Not at all", 1, ifelse(survey_dat$falling_down == "A little", 2, ifelse(survey_dat$falling_down == "A fair amount", 3, ifelse(survey_dat$falling_down == "Very much", 4, ifelse(survey_dat$falling_down == "Extreme", 5, NA)))))
survey_dat$cramp = ifelse(survey_dat$cramp == "Not at all", 1, ifelse(survey_dat$cramp == "A little", 2, ifelse(survey_dat$cramp == "A fair amount", 3, ifelse(survey_dat$cramp == "Very much", 4, ifelse(survey_dat$cramp == "Extreme", 5, NA)))))

#this will create the participants' total score for the FPQ
survey_dat$fear_of_pain = (survey_dat$breaking_arm + survey_dat$wart_removal + survey_dat$paper_cut + survey_dat$injection_in_mouth + survey_dat$soap_in_eyes + survey_dat$slam_door_on_hand + survey_dat$gulping_hot_drink + survey_dat$injection_in_hip + survey_dat$falling_down + survey_dat$cramp)/10

### EXPERIMENT-SPECIFIC QUESTIONS ###

#creates score for participants' answer to "I felt close to my friend, family member, or partner when I saw their photo during the exercise trials."
survey_dat$close_to_support_figure_during_exercise = ifelse(survey_dat$close_to_support_figure_during_exercise == "Strongly disagree", 1, ifelse(survey_dat$close_to_support_figure_during_exercise == "Disagree a little", 2, ifelse(survey_dat$close_to_support_figure_during_exercise == "Neither agree nor disagree", 3, ifelse(survey_dat$close_to_support_figure_during_exercise == "Agree a little", 4, ifelse(survey_dat$close_to_support_figure_during_exercise == "Strongly agree", 5, NA)))))

#creates score for participants' answer to "I felt close to the other person whose photo I saw during the exercise trials."
survey_dat$close_to_stranger_during_exercise = ifelse(survey_dat$close_to_stranger_during_exercise == "Strongly disagree", 1, ifelse(survey_dat$close_to_stranger_during_exercise == "Disagree a little", 2, ifelse(survey_dat$close_to_stranger_during_exercise == "Neither agree nor disagree", 3, ifelse(survey_dat$close_to_stranger_during_exercise == "Agree a little", 4, ifelse(survey_dat$close_to_stranger_during_exercise == "Strongly agree", 5, NA)))))

#the Qualtrics script did not force a response for this question; NA's assumed to be 50, the starting point of the scale (and where it would remain if unmoved)
table(survey_dat$beta_alanine_performance_effect, useNA = "ifany")

#this will replace all NA's with 50
survey_dat = replace.value(survey_dat, c("beta_alanine_performance_effect"), from = NA, to = as.integer(50), verbose = FALSE)

#convert the reversed scored question "What effect did the beta-alanine have on how hard the exercise trials were? (0 = "It made them easier, 50 = "No effect", 100 = "It made them harder")
survey_dat$beta_alanine_difficulty_effect = (100 - survey_dat$beta_alanine_difficulty_effect)

#this will make a variable that is how long participants have known their support figure (in months)
survey_dat$months_known = (survey_dat$years * 12) + survey_dat$months

### SOCIAL ASSURANCE SCALE (SAS) ###

#create scale scores (1 = "Disagree", 7 = "Agree")
survey_dat$SAS = (survey_dat$comfortable_when_constantly_with_someone + survey_dat$at_ease_doing_things_with_others + survey_dat$prefer_work_side_by_side + survey_dat$life_incomplete_without_buddy + survey_dat$hard_to_use_skills_without_partner + survey_dat$stick_to_friends + survey_dat$join_groups_for_friends + survey_dat$wish_to_be_with_someone_always)/8

### DROP UNUSED VARIABLES FROM THE SURVEY DATA SET ###

#this gives an untruncated list of all the variables in 'survey_dat'
str(survey_dat, list.len=ncol(survey_dat))

#the variables to be retained
columns = names(survey_dat)
keepers = c(columns[1], columns[8:9], columns[12], columns[86:89], columns[99], columns[105:115]) 

#this returns 'survey_dat' without the dropped variables
survey_dat = survey_dat[keepers]

### PREPARE THE TWO DATA SETS TO BE MERGED ###

#add 'sub_' to all participant numbers (to match the 'participant' variable in 'total_data'), and coerce an integer value input into a character value output
survey_dat$participant.char = sub("^", "sub_", survey_dat$Participant)

#merge the two data sets and then drop the columns with "sub_" and keep only the "Participant" column
merged_data = merge(total_data, survey_dat, by.x="participant", by.y="participant.char")

#drop the column containing the old participant classification system (the one with "sub_" as the prefix)
drops = c("participant")
merged_data = merged_data[ , !(names(merged_data) %in% drops)]

#this will move the other 'Participant' variable to the first column in the data set
merged_data = merged_data %>% select(Participant, everything())

################################################################################################################################################

### MERGE MATLAB DATA ON PARTICIPANTS' ANSWERS TO THE QUESTIONS ABOUT EXERCISE EFFORT AND DIFFICULTY WITH THE REST OF THE MATLAB AND QUALTRICS DATA ###

library(R.matlab)
library(ggplot2)
library(stringr)

#get all the subfolders in the results folder (exclude the first item, as it is the parent folder)
subfolders = list.dirs(datfolder)[-1]

#exclude items 35 and 36, which belong to the excluded participant with erroneous voltage data)
subfolders = subfolders[-c(35, 36)]

#counter for the for loop
count = 1

#this creates an empty data frame
matdat = data.frame(matrix(NA, nrow = 0, ncol = 23))

#this will go through all the subfolders
for (i in subfolders) {
  
  #track progress
  print(paste0("CURRENT FOLDER: ", i))
  
  #get the data
  sub1 = subfolders[count]
  files1 = list.files(sub1)
  
  D = readMat(paste(sub1, files1[2], sep = '/'))
  
  #this gets the first 21 columns from the MATLAB data set
  tempdat = as.data.frame(D$record.dat[,1:21])
  
  #this gives participant data that is in a 42 item-long list
  tempdat$participant = D$subDetails[1]
  
  #this unlists the first item in the list (they are all the same, the participant number) so that it becomes a character vector
  tempdat$participant = sapply(tempdat$participant[1], paste0, collapse="")
  tempdat$participant = tempdat$participant[1]
  
  #this gets the placebo condition
  tempdat$placebo = D$subDetails[5]
  
  #this gets the experimental condition ('sub1' is a string and this takes the last character, which is the experimental session) and converts it to numeric
  tempdat$experimental_session = as.numeric(sub(".*_sess", "", sub1))
  
  #this creates a new data frame with both sessions from the participant
  matdat = rbind(matdat, tempdat)
  
  count = count + 1
}

#'matdat' variable names
headings = c('trial', #1
             'supp_cont', #2 - 1 equals the supportive face, 2 equals the control face
             'start_time', #3
             'NAN1', #4
             'Q1_start_time', #5 Q1 is "How much effort did you put in to keep the ball above the line?"
             'Q2_start_time', #6 Q2 is "How hard was it to keep the ball above the line?"
             'blank1', #7
             'q1_response_time', #8
             'q2_response_time', #9
             'blank2', #10
             'NAN2', #11
             'Q1_start_pos', #12
             'Q2_start_pos', #13
             'blank3', #14
             'Q1_ans_pos', #15
             'Q2_ans_pos', #16
             'blank4', #17
             'Q1_ans_perc', #18
             'Q2_ans_perc', #19
             'blank5', #20
             'trial_difficulty', #21
             'participant', #22
             'placebo', #23
             'experimental_session') #24

#set the column names
colnames(x = matdat) = headings

#there is one participant with "OFF" instead of "off" for the 'placebo' variable; fix this
matdat$placebo = ifelse(matdat$placebo == "OFF", "off", ifelse(matdat$placebo == "on", "on", ifelse(matdat$placebo == "off", "off", NA)))

#make the placebo variable a factor
matdat$placebo = as.factor(matdat$placebo)

### MERGE THE DATA AND THEN CLEAN IT  ###

#drop the unwanted columns from 'matdat'
matdat.drops = c("start_time", "NAN1", "Q1_start_time", "Q2_start_time", "blank1", "q1_response_time", "q2_response_time", "blank2", "NAN2", "Q1_start_pos", "Q2_start_pos", "blank3", "Q1_ans_pos", "Q2_ans_pos", "blank4", "blank5")
matdat = matdat[ , !(names(matdat) %in% matdat.drops)]

#add 'sub_' to all participant numbers (to match the 'participant' variable in 'matdat')
merged_data$participant.char = sub("^", "sub_", merged_data$Participant)

#merges two data sets with non-matching column names
final_data = merge(merged_data, matdat, by.x= c("participant.char", "session_number", "trial_number"), by.y= c("participant", "experimental_session", "trial"))

#this will drop the column containing the old participant classification system (the one with "sub_" as the prefix), and other unneeded or repetitive variables
drops = c("participant.char", "trial_difficulty.y", "supp_cont", "placebo", "bar_unit_easy", "bar_unit_medium", "bar_unit_hard", "grip_px", "grip_dv")
final_data = final_data[ , !(names(final_data) %in% drops)]

#this will move the other 'Participant' variable to the first column in the data set
final_data =  final_data %>% select(Participant, everything())

#reorder and rename the rest of the variables
names(final_data)[names(final_data) == 'trial_difficulty.x'] = 'trial_difficulty'

#the 'percent_of_maximum' variable is actually a decimal, multiply it by 100 to make it a perentage
final_data$percent_of_maximum = final_data$percent_of_maximum * 100

#make clear what the social support conditions are (it is now numeric)
final_data$support_or_control = ifelse(final_data$support_or_control == 1, 'support', ifelse(final_data$support_or_control == 2, 'control', NA))

#create a variable that represents the 2*2 placebo-support conditions
final_data$placeb_by_support_condition = ifelse(final_data$support_or_control == "support" & final_data$placebo_condition == "on", "placebo, support",
                                                ifelse(final_data$support_or_control == "control" & final_data$placebo_condition == "on", "placebo, no support",
                                                       ifelse(final_data$support_or_control == "support" & final_data$placebo_condition == "off", "no placebo, support",
                                                              ifelse(final_data$support_or_control == "control" & final_data$placebo_condition == "off", "no placebo, no support", NA))))

#list of all the variables in the order they should appear in the final dataset
vars = c("Participant", "age", "sex", "placebo_condition", "support_or_control", "placeb_by_support_condition", "trial_difficulty", "session_number", "trial_number",
         "sample_reading_number", "reading_number", "real_voltage", "percent_of_maximum", "grip_strength", "min_grip", "max_grip", "voltage", "Q1_ans_perc", "Q2_ans_perc",
         "does_support_figure_match_description", "months_known", "close_to_support_figure_during_exercise", "close_to_stranger_during_exercise",
         "beta_alanine_performance_effect", "beta_alanine_difficulty_effect", "heard_of_beta_alanine", "extraversion", "agreeableness", "conscientiousness",
         "neuroticism", "openness", "close", "depend", "anxiety", "fear_of_pain", "SAS")

#reorder the columns of the data frame
final_data = final_data[vars]

#this will output the 'total_data' data frame to .csv
write.csv(final_data, file = "../data/total_combined_data.csv", row.names=FALSE)

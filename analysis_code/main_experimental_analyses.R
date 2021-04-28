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

#loads the final data set
total_data = read.csv("../data/total_combined_data.csv", sep = ",")

################################################################################################################################################

### REMOVAL OF PARTICIPANTS WHO DID NOT FOLLOW EXPERIMENTAL INSTRUCTIONS ###

#create a list of all participants with data
participants = as.numeric(as.character(unique(total_data$Participant)))

#find participants whose voltage data was not recorded (due to equipment malfunctions)
all_participant_numbers = min(participants):max(participants)
no_volt_participants = all_participant_numbers[!all_participant_numbers %in% participants]
print(paste0("PARTICIPANTS NOT INCLUDED IN ANALYSES DUE TO MISSING HANDGRIP VOLTAGE (STRENGTH) DATA: ", paste( unlist(no_volt_participants), collapse=' '),
             " (", length(no_volt_participants), " IN TOTAL)"))

### ### ###

#make the 'Participant' variable a factor
total_data$Participant = as.factor(total_data$Participant)

#a list of participants who were not following the experimental instructions
slacking = c()

#go through all the participants and calculate average percentage of total grip strength for each difficulty level
for (i in participants) {
  
  #subsets data by participant and for each difficulty level
  trial_diff_1 = subset(total_data, Participant == i & trial_difficulty == 1)
  trial_diff_2 = subset(total_data, Participant == i & trial_difficulty == 2)
  trial_diff_3 = subset(total_data, Participant == i & trial_difficulty == 3)
  
  #gets participant i's mean percentage of maximum grip strength for each difficulty level
  mean_td1 = mean(trial_diff_1$percent_of_maximum)
  mean_td2 = mean(trial_diff_2$percent_of_maximum) 
  mean_td3 = mean(trial_diff_3$percent_of_maximum)
  
  #identify any participant whose average grip strength does not increase with trial difficulty (indicating they did not try to meet their target grip strengths)
  if (mean_td1 < mean_td2 & mean_td2 < mean_td3){
  } else {
    slacking = append(slacking, i)
  }
}

#remove all data from participants who did not follow the experimental instructions to increase grip strength with trial difficulty
slacker_data = total_data[total_data$Participant %in% slacking, ]
total_data = total_data[ ! total_data$Participant %in% slacking, ]
total_data$Participant = droplevels(total_data$Participant)
print(paste0("PARTICIPANTS NOT INCLUDED IN ANALYSES DUE TO NOT FOLLOWING THE EXPERIMENTAL INSTRUCTIONS: ", paste( unlist(slacking), collapse=' '),
             " (", length(slacking), " IN TOTAL)"))

#find all participants whose data is not included in analyses
final_participants = unique(total_data$Participant)
not_included = all_participant_numbers[!all_participant_numbers %in% final_participants]
print(paste0("TOTAL PARTICIPANTS NOT INCLUDED IN ANALYSES: ", paste( unlist(not_included), collapse=' '),
             " (", length(not_included), " IN TOTAL)"))

################################################################################################################################################

### PARTICIPANT DESCRIPTIVE STATISTICS ###

#get the total number of participants (after exclusions)
length(unique(total_data$Participant))

#get the number of female participants
(table(total_data$sex)[1] / (sum(table(total_data$sex)))) * length(unique(total_data$Participant))

#get the percentage of female participants
table(total_data$sex)[1] / (sum(table(total_data$sex)))

#getdescriptives of participants' age
mean(total_data$age)
sd(total_data$age)
range(total_data$age)

################################################################################################################################################

### DESCRIPTIVES AND TESTS OF POST-EXERCISE QUESTIONS ###

#these analyses need to be run on a data set that has just one row per trial, since each question is answered only once per trial (the first row and the last row will be the same - only the voltage / handgrip data changes)
question_data = subset(total_data, sample_reading_number == 1)

#get one row per participant (this will get the data for the pre- and post-experiment questions)
post_experiment_data = subset(question_data, trial_number == 1 & session_number == 1)

### ### ###

#number of participants who had heard of beta-alanine
table(post_experiment_data$heard_of_beta_alanine)[3]

#percentage of participants who had heard of beta-alanine
(table(post_experiment_data$heard_of_beta_alanine)[3] / sum(table(post_experiment_data$heard_of_beta_alanine))) * 100

### ### ##

#get the mean, sd, and range of responses to the question about whether participants felt that the beta-alanine (i.e., the placebo) significantly improved their performance (add 50 to get actual scale values)
mean(post_experiment_data$beta_alanine_performance_effect) + 50
sd(post_experiment_data$beta_alanine_performance_effect)
range(post_experiment_data$beta_alanine_performance_effect) + 50

#one sample t-test to see if scores differ from 0 (make 50 or "no difference" equal 0)
post_experiment_data$beta_alanine_performance_effect = post_experiment_data$beta_alanine_performance_effect + 50
t.test(post_experiment_data$beta_alanine_performance_effect)

### ### ###

#get the mean, sd, and range of of responses to the question about whether participants felt that the beta-alanine (i.e., the placebo) made the exercise trials easier (add 50 to get actual scale values)
mean(post_experiment_data$beta_alanine_difficulty_effect) + 50
sd(post_experiment_data$beta_alanine_difficulty_effect)
range(post_experiment_data$beta_alanine_difficulty_effect) + 50

#one sample t-test to see if scores differ from 0 (make 50 or "no difference" equal 0)
post_experiment_data$beta_alanine_difficulty_effect = post_experiment_data$beta_alanine_difficulty_effect + 50
t.test(post_experiment_data$beta_alanine_difficulty_effect)

### ### ###

#get the mean, sd, and range of responses to the question on how close participants felt to their support figure whose photo they saw during the experiment
mean(post_experiment_data$close_to_support_figure_during_exercise)
sd(post_experiment_data$close_to_support_figure_during_exercise)
range(post_experiment_data$close_to_support_figure_during_exercise)

#get the mean, sd, and range of responses to the question on how close participants felt to the stranger whose photo they saw during the experiment
mean(post_experiment_data$close_to_stranger_during_exercise)
sd(post_experiment_data$close_to_stranger_during_exercise)
range(post_experiment_data$close_to_stranger_during_exercise)

#do a Wilcoxon rank-sum test (paired samples, since measures are from the same participant)
wilcox.test(post_experiment_data$close_to_support_figure_during_exercise, post_experiment_data$close_to_stranger_during_exercise, paired = TRUE)

### ### ###

#get the mean, sd, and range of responses to the question about how well participants' support figures met the description of “someone you feel you have a close connection with and that you can depend on in times of need"
mean(post_experiment_data$does_support_figure_match_description)
sd(post_experiment_data$does_support_figure_match_description)
range(post_experiment_data$does_support_figure_match_description)

#count and percentage of participants who answered the question with a 6 or 7 ("Very much")
sum(table(post_experiment_data$does_support_figure_match_description)[c(3,4)])
(sum(table(post_experiment_data$does_support_figure_match_description)[c(3,4)]) / sum(table(post_experiment_data$does_support_figure_match_description))) * 100

#time (in months) participants reported knowing their support figures
mean(post_experiment_data$months_known) / 12
sd(post_experiment_data$months_known) / 12
range(post_experiment_data$months_known) / 12

### ### ###

#mean, sd, and range of scores on social assurance scale
mean(post_experiment_data$SAS)
sd(post_experiment_data$SAS)
range(post_experiment_data$SAS)

#mean, sd, and range of scores on ability to depend upon others measure
mean(post_experiment_data$depend)
sd(post_experiment_data$depend)
range(post_experiment_data$depend)

#mean, sd, and range of scores on neuroticism measure
mean(post_experiment_data$neuroticism)
sd(post_experiment_data$neuroticism)
range(post_experiment_data$neuroticism)

#mean, sd, and range of ratios of felt closeness to support figure v. felt closeness to stranger
post_experiment_data$support.to.stranger.ratio = post_experiment_data$close_to_support_figure_during_exercise / post_experiment_data$close_to_stranger_during_exercise
mean(post_experiment_data$support.to.stranger.ratio)
sd(post_experiment_data$support.to.stranger.ratio)
range(post_experiment_data$support.to.stranger.ratio)

#mean, sd, and range of scores on fear of pain questionnaire
mean(post_experiment_data$fear_of_pain)
sd(post_experiment_data$fear_of_pain)
range(post_experiment_data$fear_of_pain)

################################################################################################################################################

### HANDGRIP OUTPUTS, PERCEIVED DIFFICULTY, AND REPORTED EFFORT LEVELS ###

library(ggplot2)
library(dplyr)

#handgrip strength by trial difficulty for all the participants in the main analyses
group_by(total_data, trial_difficulty) %>% summarise(grip_mean = mean(percent_of_maximum, na.rm = TRUE),
                                                     grip_sd = sd(percent_of_maximum, na.rm = TRUE))

#handgrip strength by trial difficulty for all the participants removed from the main analyses for not following the experimental instructions
group_by(slacker_data, trial_difficulty) %>% summarise(grip_mean = mean(percent_of_maximum, na.rm = TRUE),
                                                       grip_sd = sd(percent_of_maximum, na.rm = TRUE))

#handgrip strength by each experimental condition
group_by(total_data, support_or_control, placebo_condition, trial_difficulty) %>% summarise(volt_mean = mean(percent_of_maximum, na.rm = TRUE),
                                                                                            grip_sd = sd(percent_of_maximum, na.rm = TRUE))

#handgrip strength by social support and placebo condition
group_by(total_data, support_or_control, placebo_condition) %>% summarise(volt_mean = mean(percent_of_maximum, na.rm = TRUE),
                                                                                           grip_sd = sd(percent_of_maximum, na.rm = TRUE))

#handgrip strength by social support condition
group_by(total_data, support_or_control) %>% summarise(volt_mean = mean(percent_of_maximum, na.rm = TRUE),
                                                       grip_sd = sd(percent_of_maximum, na.rm = TRUE))

#handgrip strength by placebo condition
group_by(total_data, placebo_condition) %>% summarise(volt_mean = mean(percent_of_maximum, na.rm = TRUE),
                                                      grip_sd = sd(percent_of_maximum, na.rm = TRUE))

#handgrip strength by exercise block
group_by(total_data, session_number) %>% summarise(volt_mean = mean(percent_of_maximum, na.rm = TRUE),
                                                       grip_sd = sd(percent_of_maximum, na.rm = TRUE))

#handgrip strength by exercise block and placebo condition
group_by(total_data, session_number, placebo_condition) %>% summarise(volt_mean = mean(percent_of_maximum, na.rm = TRUE),
                                                                      grip_sd = sd(percent_of_maximum, na.rm = TRUE))

#percentage of handgrip strength readings greater than 100% of the participant's maximum
high_reading = subset(total_data, percent_of_maximum > 100)
length(unique(high_reading$Participant))
nrow(high_reading)
(nrow(high_reading) / nrow(total_data)) * 100

### ### ###

#perceived difficulty by trial target difficulty
group_by(question_data, trial_difficulty) %>% summarise(diff = mean(Q2_ans_perc, na.rm = TRUE),
                                                        sd = sd(Q2_ans_perc, na.rm = TRUE))

#perceived difficulty by each experimental condition
group_by(question_data, support_or_control, placebo_condition, trial_difficulty) %>% summarise(diff = mean(Q2_ans_perc, na.rm = TRUE),
                                                                                               sd = sd(Q2_ans_perc, na.rm = TRUE))

#perceived difficulty by social support and placebo condition
group_by(question_data, support_or_control, placebo_condition) %>% summarise(diff = mean(Q2_ans_perc, na.rm = TRUE),
                                                                             sd = sd(Q2_ans_perc, na.rm = TRUE))

#perceived difficulty by placebo condition
group_by(question_data, placebo_condition ) %>% summarise(diff = mean(Q2_ans_perc, na.rm = TRUE),
                                                          sd = sd(Q2_ans_perc, na.rm = TRUE))

#perceived difficulty by social support condition
group_by(question_data, placebo_condition ) %>% summarise(diff = mean(Q2_ans_perc, na.rm = TRUE),
                                                          sd = sd(Q2_ans_perc, na.rm = TRUE))

#perceived difficulty by exercise block
group_by(question_data, session_number ) %>% summarise(diff = mean(Q2_ans_perc, na.rm = TRUE),
                                                       sd = sd(Q2_ans_perc, na.rm = TRUE))

### ### ###

#reported effort levels by trial target difficulty
group_by(question_data, trial_difficulty) %>% summarise(effort = mean(Q1_ans_perc, na.rm = TRUE),
                                                        sd = sd(Q1_ans_perc, na.rm = TRUE))

#reported effort levels by each experimental condition
group_by(question_data, support_or_control, placebo_condition, trial_difficulty) %>% summarise(effort = mean(Q1_ans_perc, na.rm = TRUE),
                                                                                               sd = sd(Q1_ans_perc, na.rm = TRUE))

#reported effort levels by social support and placebo condition
group_by(question_data, support_or_control, placebo_condition) %>% summarise(effort = mean(Q1_ans_perc, na.rm = TRUE),
                                                                             sd = sd(Q1_ans_perc, na.rm = TRUE))

#reported effort levels by placebo condition
group_by(question_data, placebo_condition) %>% summarise(effort = mean(Q1_ans_perc, na.rm = TRUE),
                                                         sd = sd(Q1_ans_perc, na.rm = TRUE))

#reported effort levels by social support
group_by(question_data, support_or_control) %>% summarise(effort = mean(Q1_ans_perc, na.rm = TRUE),
                                                          sd = sd(Q1_ans_perc, na.rm = TRUE))

#reported effort levels by exercise block
group_by(question_data, session_number) %>% summarise(effort = mean(Q1_ans_perc, na.rm = TRUE), 
                                                      sd = sd(Q1_ans_perc, na.rm = TRUE))

#compare reported effort levels between each social support condition and each placebo condition
t.test(subset(question_data, question_data$support_or_control == "support")[, c("Q1_ans_perc")], subset(question_data, question_data$support_or_control == "control")[, c("Q1_ans_perc")])
t.test(subset(question_data, question_data$placebo_condition == "on")[, c("Q1_ans_perc")], subset(question_data, question_data$placebo_condition == "off")[, c("Q1_ans_perc")])

### ### ###

#handgrip strength by trial difficulty 
grip_dat = group_by(total_data, trial_difficulty) %>% 
            summarise(grip_mean = mean(percent_of_maximum, na.rm = TRUE),
              grip_sd = sd(percent_of_maximum, na.rm = TRUE))

#mean handgrip strength for each trial difficulty 
mean_40 = as.numeric(grip_dat[1,2])
mean_50 = as.numeric(grip_dat[2,2])
mean_60 = as.numeric(grip_dat[3,2])

#linearity of trial target difficulty effect on handgrip outputs (a 'prettier' version of this plot is saved below)
trial_diff_handgrip_lm = lm(percent_of_maximum ~ trial_difficulty, data = total_data)

trial_difficulty_handgrip_outputs = ggplot(total_data, aes(x = as.factor(trial_difficulty), y = percent_of_maximum)) + 
                                      geom_violin() + 
                                      geom_boxplot() +
                                      geom_point(aes(x = "1", y = mean_40), colour="grey", size = 3) + 
                                      geom_point(aes(x = "2", y = mean_50), colour="grey", size = 3) + 
                                      geom_point(aes(x = "3", y = mean_60), colour="grey", size = 3) +
                                      geom_abline(slope = coef(trial_diff_handgrip_lm)[[2]], intercept = coef(trial_diff_handgrip_lm)[[1]]) +
                                      scale_x_discrete(labels = c("40%", "50%", "60%")) + 
                                      scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%", "125%")) +
                                      xlab("Trial target difficulty\n(percentage of participant's maximum handgrip strength)") +
                                      ylab("Handgrip output (as percentage of participant's maximum)")

#preview the plot
trial_difficulty_handgrip_outputs

### ### ###

#perceived difficulty by trial difficulty 
diff_dat = group_by(question_data, trial_difficulty) %>% 
  summarise(diff_mean = mean(Q2_ans_perc, na.rm = TRUE),
            diff_sd = sd(Q2_ans_perc, na.rm = TRUE))

#mean perceived difficulty for each trial difficulty 
mean_40 = as.numeric(diff_dat[1,2])
mean_50 = as.numeric(diff_dat[2,2])
mean_60 = as.numeric(diff_dat[3,2])

#linearity of trial target difficulty effect on perceived difficulty (a 'prettier' version of this plot is saved below)
trial_diff_difficulty_lm = lm(Q2_ans_perc ~ trial_difficulty, data = question_data)

trial_difficulty_perceived_difficulty = ggplot(question_data, aes(x = as.factor(trial_difficulty), y = Q2_ans_perc)) + 
                                          geom_violin() + 
                                          geom_boxplot() +
                                          geom_point(aes(x = "1", y = mean_40), colour="grey", size = 3) + 
                                          geom_point(aes(x = "2", y = mean_50), colour="grey", size = 3) + 
                                          geom_point(aes(x = "3", y = mean_60), colour="grey", size = 3) +
                                          geom_abline(slope = coef(trial_diff_difficulty_lm)[[2]], intercept = coef(trial_diff_difficulty_lm)[[1]]) +
                                          scale_x_discrete(labels = c("40%", "50%", "60%")) + 
                                          scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), labels = c("0%", "25%", "50%", "75%", "100%")) +
                                          xlab("Trial target difficulty\n(percentage of participant's maximum handgrip strength)") +
                                          ylab("Perceived difficulty\n(as percentage of sliding scale)")

#preview the plot
trial_difficulty_perceived_difficulty

### ### ###

#show that the 'trial_number' variable is crossed with trial difficulty (so it won't be linearly related to the outcome)
trial_dat = total_data %>% group_by(trial_number) %>% summarise(volt_mean = mean(percent_of_maximum, na.rm = TRUE))
plot(trial_dat$trial_number, trial_dat$volt_mean)

#convert the 'trial_number' variable to a sub-block variable (there are 14 sub-blocks of trials, each with three trials), which will be more linearly related to the outcome
trial_dat$group = ifelse(trial_dat$volt_mean < 45, "low", ifelse(trial_dat$volt_mean > 45 & trial_dat$volt_mean < 53, "med", ifelse(trial_dat$volt_mean > 53, "high", NA)))
trial_dat = trial_dat[order(trial_dat[,3]),]
trial_dat$sub_block = rep(seq(1,14), 3)
total_data$sub_block =  with(trial_dat, sub_block[match(total_data$trial_number, trial_number)])

#handgrip outputs by exercise sub-block
sub_block_dat = total_data %>% group_by(sub_block) %>% summarise(voltage_ave = mean(percent_of_maximum, na.rm = TRUE),
                                                                 voltage_se = sd(percent_of_maximum, na.rm = TRUE) / sqrt(n()))

#linearity of exercise sub-block effect on handgrip outputs (a 'prettier' version of this plot is saved below)
sub_b_handgrip_lm = lm(percent_of_maximum ~ sub_block, data  = total_data)

sub_block_handgrip_outputs = ggplot(total_data, aes(x = as.factor(sub_block), y = percent_of_maximum)) + 
                               geom_violin() + 
                               geom_boxplot() +
                               geom_point(aes(x = 1, y = sub_block_dat$voltage_ave[1]), colour="grey", size = 2) + 
                               geom_point(aes(x = 2, y = sub_block_dat$voltage_ave[2]), colour="grey", size = 2) + 
                               geom_point(aes(x = 3, y = sub_block_dat$voltage_ave[3]), colour="grey", size = 2) +
                               geom_point(aes(x = 4, y = sub_block_dat$voltage_ave[4]), colour="grey", size = 2) + 
                               geom_point(aes(x = 5, y = sub_block_dat$voltage_ave[5]), colour="grey", size = 2) + 
                               geom_point(aes(x = 6, y = sub_block_dat$voltage_ave[6]), colour="grey", size = 2) +
                               geom_point(aes(x = 7, y = sub_block_dat$voltage_ave[7]), colour="grey", size = 2) + 
                               geom_point(aes(x = 8, y = sub_block_dat$voltage_ave[8]), colour="grey", size = 2) + 
                               geom_point(aes(x = 9, y = sub_block_dat$voltage_ave[9]), colour="grey", size = 2) +
                               geom_point(aes(x = 10, y = sub_block_dat$voltage_ave[10]), colour="grey", size = 2) + 
                               geom_point(aes(x = 11, y = sub_block_dat$voltage_ave[11]), colour="grey", size = 2) + 
                               geom_point(aes(x = 12, y = sub_block_dat$voltage_ave[12]), colour="grey", size = 2) +
                               geom_point(aes(x = 13, y = sub_block_dat$voltage_ave[13]), colour="grey", size = 2) + 
                               geom_point(aes(x = 14, y = sub_block_dat$voltage_ave[14]), colour="grey", size = 2) + 
                               geom_abline(slope = coef(sub_b_handgrip_lm)[[2]], intercept = coef(sub_b_handgrip_lm)[[1]]) +
                               scale_y_continuous(limits = c(0, 125), breaks = c(seq(0, 125, 25)),
                                                  labels = c("0%", "25%", "50%", "75%", "100%", "125%")) +
                               xlab("Exercise sub-block") +
                               ylab("Handgrip output (as percentage of participant's maximum)")  

#preview the plot
sub_block_handgrip_outputs

### ### ###

#these analyses need to be run on a data set that has just one row per trial, since each question is answered only once per trial
question_data = subset(total_data, sample_reading_number == 1)

#perceived difficulty by exercise sub-block
sub_block_dat = question_data %>% group_by(sub_block) %>% summarise(diff_ave = mean(Q2_ans_perc, na.rm = TRUE),
                                                                    diff_se = sd(Q2_ans_perc, na.rm = TRUE) / sqrt(n()))

#linearity of exercise sub-block effect on perceived difficulty (a 'prettier' version of this plot is saved below)
sub_b_p_difficulty_lm = lm(Q2_ans_perc ~ sub_block, data = question_data)

sub_block_perceived_difficulty = ggplot(question_data, aes(x = as.factor(sub_block), y = Q2_ans_perc)) + 
                                   geom_violin() + 
                                   geom_boxplot() +
                                   geom_point(aes(x = 1, y = sub_block_dat$diff_ave[1]), colour="grey", size = 2) + 
                                   geom_point(aes(x = 2, y = sub_block_dat$diff_ave[2]), colour="grey", size = 2) + 
                                   geom_point(aes(x = 3, y = sub_block_dat$diff_ave[3]), colour="grey", size = 2) +
                                   geom_point(aes(x = 4, y = sub_block_dat$diff_ave[4]), colour="grey", size = 2) + 
                                   geom_point(aes(x = 5, y = sub_block_dat$diff_ave[5]), colour="grey", size = 2) + 
                                   geom_point(aes(x = 6, y = sub_block_dat$diff_ave[6]), colour="grey", size = 2) +
                                   geom_point(aes(x = 7, y = sub_block_dat$diff_ave[7]), colour="grey", size = 2) + 
                                   geom_point(aes(x = 8, y = sub_block_dat$diff_ave[8]), colour="grey", size = 2) + 
                                   geom_point(aes(x = 9, y = sub_block_dat$diff_ave[9]), colour="grey", size = 2) +
                                   geom_point(aes(x = 10, y = sub_block_dat$diff_ave[10]), colour="grey", size = 2) + 
                                   geom_point(aes(x = 11, y = sub_block_dat$diff_ave[11]), colour="grey", size = 2) + 
                                   geom_point(aes(x = 12, y = sub_block_dat$diff_ave[12]), colour="grey", size = 2) +
                                   geom_point(aes(x = 13, y = sub_block_dat$diff_ave[13]), colour="grey", size = 2) + 
                                   geom_point(aes(x = 14, y = sub_block_dat$diff_ave[14]), colour="grey", size = 2) +
                                   geom_abline(slope = coef(sub_b_p_difficulty_lm)[[2]], intercept = coef(sub_b_p_difficulty_lm)[[1]]) +
                                   scale_y_continuous(limits = c(0, 100), breaks = c(seq(0, 100, 10)),
                                                      labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
                                   xlab("Exercise sub-block") +
                                   ylab("Perceived difficulty\n(as percentage of sliding scale)")

#preview the plot
sub_block_perceived_difficulty

### ### ###

#correlation beteen participants' answers to the two questions post-trial questions on perceived difficulty and effort
cor(question_data$Q1_ans_perc, question_data$Q2_ans_perc)

#check question censoring for the two questions (a 'prettier' version of the 'effort' plot is saved below)
hist(question_data$Q1_ans_perc, main = "", xlab = "Percent of sliding scale (0 = 'no effort at all', 100 = 'maximum effort')", ylim = c(0, 800), breaks = 101, cex=2.5)
hist(question_data$Q2_ans_perc, main = "", xlab = "Percent of sliding scale (0 = 'not hard at all', 100 = 'extremely hard')",  ylim = c(0, 800), breaks = 101)

#get the percentage of answers at the maximum of the sliding scale
nrow(subset(question_data, Q2_ans_perc == 100)) / nrow(question_data) * 100
nrow(subset(question_data, Q1_ans_perc == 100)) / nrow(question_data) * 100

#a function for finding the mode of a vector
getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#get the mode of both answers
getmode(question_data$Q2_ans_perc)
getmode(question_data$Q1_ans_perc)

################################################################################################################################################

### MULTILEVEL MODEL ON PARTICIPANTS' HANDGRIP OUTPUTS ###

library(lme4)
library(lmerTest)
library(optimx)
library(nloptr)
library(dfoptim)
library(RCurl)
library(MuMIn)
library(emmeans)
library(ggplot2)

#sum contrast the social support and placebo factors
total_data$support_or_control = ordered(total_data$support_or_control, levels = c("support", "control"))
contrasts(total_data$support_or_control) = contr.sum(2)

total_data$placebo_condition = ordered(total_data$placebo_condition, levels = c("on", "off"))
contrasts(total_data$placebo_condition) = contr.sum(2)

#make the mean 0 for the 'trial_difficulty' variable, and scale / zero sum code the 'session_number' variable (for when it is used as a covariate)
total_data$trial_difficulty_scaled = total_data$trial_difficulty - 2
total_data$session_number_scaled = ifelse(total_data$session_number == 1, -1, ifelse(total_data$session_number == 2, 1, NA))

#scale the 'sub_block' and 'Q1_ans_perc' variables
total_data$sub_block_scaled = scale(total_data$sub_block)
total_data$Q1_ans_perc_scaled = scale(total_data$Q1_ans_perc)

### ### ###

#test the four and three-way interactions (which are not of theoretical interest); this is the most complex random effects structure that allows for convergence (the non-significant participant sex variable and the lowest variance random effect, social support condition, were dropped to allow for model convergence)
full_interaction_model_1 = lmer(percent_of_maximum ~ 1 + trial_difficulty_scaled*placebo_condition*support_or_control*session_number_scaled + sub_block_scaled + session_number_scaled + Q1_ans_perc_scaled + sex + (1 + placebo_condition + support_or_control | Participant), data = total_data)
summary(full_interaction_model_1)

full_interaction_model_2 = lmer(percent_of_maximum ~ 1 + trial_difficulty_scaled*placebo_condition*support_or_control*session_number_scaled + sub_block_scaled + session_number_scaled + Q1_ans_perc_scaled + sex + (1 + placebo_condition | Participant), data = total_data)
summary(full_interaction_model_2)

full_interaction_model_final = lmer(percent_of_maximum ~ 1 + trial_difficulty_scaled*placebo_condition*support_or_control*session_number_scaled + sub_block_scaled + session_number_scaled + Q1_ans_perc_scaled + (1 + placebo_condition | Participant), data = total_data)
summary(full_interaction_model_final)
r.squaredGLMM(full_interaction_model_final)

#drop the non-significant four and three-way interactions (along with their constituent interactions) and the non-significant 'sex' predictor; this is the most complex model that allows for convergence
final_handgrip_model = lmer(percent_of_maximum ~ 1 + support_or_control*placebo_condition + support_or_control*trial_difficulty_scaled + placebo_condition*trial_difficulty_scaled +
                                                 sub_block_scaled + session_number_scaled + Q1_ans_perc_scaled + (1 + placebo_condition | Participant), data = total_data)
summary(final_handgrip_model)

### ### ###

#create a 95% confidence interval for the social support effect (since the variable is sum coded, the beta coefficient is the difference between the grand mean and the support condition; multiply by two)
int_est_sup = summary(final_handgrip_model)$coefficients[2,1]
int_se_sup = summary(final_handgrip_model)$coefficients[2,2]
print(paste("SOCIAL SUPPORT UPPER 95% CI:", (int_est_sup + 1.96*int_se_sup) * 2))
print(paste("SOCIAL SUPPORT LOWER 95% CI:", (int_est_sup - 1.96*int_se_sup) * 2))

#create a 95% confidence interval for the social support by trial difficulty interaction
int_est_sup_dif = summary(final_handgrip_model)$coefficients[9,1]
int_se_sup_dif = summary(final_handgrip_model)$coefficients[9,2]
print(paste("SOCIAL SUPPORT BY TRIAL DIFFICULTY UPPER 95% CI:", int_est_sup_dif + 1.96*int_se_sup_dif))
print(paste("SOCIAL SUPPORT BY TRIAL DIFFICULTY LOWER 95% CI:", int_est_sup_dif - 1.96*int_se_sup_dif))

### ### ###

#model R-squared values
r.squaredGLMM(final_handgrip_model)

### ### ###

#the 'emmeans' package requires factored variables for post-hoc contrasts - create a model which is the same as the 'final_handgrip_model', except with an ordinal version of the 'trial_difficulty' variable and the 'sub_block_scaled' variable removed to allow for convergence
total_data$trial_difficulty_o = ordered(total_data$trial_difficulty)

final_handgrip_model_emmeans = lmer(percent_of_maximum ~ 1 + support_or_control*placebo_condition + support_or_control*trial_difficulty_o + placebo_condition*trial_difficulty_o +
                                                         session_number_scaled + Q1_ans_perc_scaled + (1 + placebo_condition | Participant), data = total_data)
summary(final_handgrip_model_emmeans)

#test post-hoc contrasts 
contrasts = as.data.frame(summary(emmeans(final_handgrip_model_emmeans, data = total_data, pairwise ~ support_or_control | trial_difficulty_o)))

#increase the SE to account for degree of freedom estimates being disabled (estimated SEs based on SEs calculated with sufficient RAM for degree of freedom calculations)
contrasts$contrasts.SE.adjusted = contrasts$contrasts.SE * 2.812

#adjust Z-scores, p-values, and 95% CIs (making them more conservative)
contrasts$contrasts.z.ratio.adjusted = contrasts$contrasts.estimate / contrasts$contrasts.SE.adjusted
contrasts$contrasts.p.value.adjusted = 2 * pnorm(contrasts$contrasts.z.ratio.adjusted, lower.tail = FALSE)
contrasts$lower.95CI.contrasts.estimate.adjusted = contrasts$contrasts.estimate - (1.96*contrasts$contrasts.SE.adjusted)
contrasts$upper.95CI.contrasts.estimate.adjusted = contrasts$contrasts.estimate + (1.96*contrasts$contrasts.SE.adjusted)

#print results
print(paste0("SUPPORT - CONTROl AT 40% TARGET HANDGRIP STRENGTH:", 
            " estimate = ", round(contrasts$contrasts.estimate[1], 3),
            ", SE = ", round(contrasts$contrasts.SE.adjusted[1], 3),
            ", Z = ", round(contrasts$contrasts.z.ratio.adjusted[1], 3),
            ", p = ", round(contrasts$contrasts.p.value.adjusted[1], 4),
            ", 95% CI = [", round(contrasts$lower.95CI.contrasts.estimate.adjusted[1], 3), ", ", round(contrasts$upper.95CI.contrasts.estimate.adjusted[1], 3), "]"))

print(paste0("SUPPORT - CONTROl AT 50% TARGET HANDGRIP STRENGTH:", 
             " estimate = ", round(contrasts$contrasts.estimate[2], 3),
             ", SE = ", round(contrasts$contrasts.SE.adjusted[2], 3),
             ", Z = ", round(contrasts$contrasts.z.ratio.adjusted[2], 3),
             ", p = ", round(contrasts$contrasts.p.value.adjusted[2], 4),
             ", 95% CI = [", round(contrasts$lower.95CI.contrasts.estimate.adjusted[2], 3), ", ", round(contrasts$upper.95CI.contrasts.estimate.adjusted[2], 3), "]"))

print(paste0("SUPPORT - CONTROl AT 60% TARGET HANDGRIP STRENGTH:", 
             " estimate = ", round(contrasts$contrasts.estimate[3], 3),
             ", SE = ", round(contrasts$contrasts.SE.adjusted[3], 3),
             ", Z = ", round(contrasts$contrasts.z.ratio.adjusted[3], 3),
             ", p = ", round(contrasts$contrasts.p.value.adjusted[3], 4),
             ", 95% CI = [", round(contrasts$lower.95CI.contrasts.estimate.adjusted[3], 3), ", ", round(contrasts$upper.95CI.contrasts.estimate.adjusted[3], 3), "]"))

################################################################################################################################################

### MULTILEVEL CENSORED REGRESSION MODEL ON POST EXERCISE TRIAL QUESTION ON PERCEIVED DIFFUCULTY ###

library(censReg)
library(plm)

#these analyses need to be run on a data set that has just one row per trial, since each question is answered only once per trial (the first row and the last row will be the same - only the voltage / handgrip data changes)
question_data = subset(total_data, sample_reading_number == 1)

#create data frame that will later need to be transformed to a 'pdata.frame' object)
rData = data.frame(question_data$Participant)

#add the main predictor variables
rData$placebo_condition = question_data$placebo_condition
rData$support_or_control = question_data$support_or_control
rData$trial_difficulty_scaled = question_data$trial_difficulty_scaled
rData$session_number_scaled = question_data$session_number_scaled

#sum contrast the social support, placebo, and exercise blcok factors,
rData$support_or_control = ordered(rData$support_or_control, levels = c("control", "support"))
contrasts(rData$support_or_control) = contr.sum(2)

rData$placebo_condition = ordered(rData$placebo_condition, levels = c("off", "on"))
contrasts(rData$placebo_condition) = contr.sum(2)

rData$session_number_scaled = ordered(question_data$session_number, levels = c(1, 2))
contrasts(rData$session_number_scaled) = contr.sum(2)

#add other covariates
rData$trial_number = question_data$trial_number
rData$sub_block_scaled = question_data$sub_block_scaled
rData$session_number = question_data$session_number

#create a time variable
rData$time = ifelse(rData$session_number == 1, rData$trial_number, ifelse(rData$session_number == 2, 42 + rData$trial_number, NA ))

#add the variable on perceived difficulty ('Q2_ans_perc') and effort ('Q1_ans_perc')
rData$y.Q2 = question_data$Q2_ans_perc
rData$y.Q1.scaled = scale(question_data$Q1_ans_perc)

#convert data frame to class 'pdata.frame' based on the id and time variables
rData = pdata.frame( rData, c( "question_data.Participant", "time" ) )

### ### ###

#test the four and three-way interactions (which are not of theoretical interest); this is the most complex random effects structure that allows for convergence
cens_reg_interactions = censReg( y.Q2 ~ trial_difficulty_scaled*placebo_condition*support_or_control*session_number_scaled + sub_block_scaled + y.Q1.scaled, left = 0, right = 100, data = rData, method = "BHHH" ) 
summary(cens_reg_interactions)

#keep the significant four-way interactions (along with its constituent interactions); this is the most complex model that allows for convergence
final_cens_reg_model = cens_reg_interactions
summary(final_cens_reg_model)

### ### ###

#create a 95% confidence interval for the social support effect (since the variable is sum coded, the beta coefficient is the difference between the grand mean and the support condition; multiply by two)
est_sup = as.data.frame(summary(final_cens_reg_model)["estimate"])[4,1]
se_sup = as.data.frame(summary(final_cens_reg_model)["estimate"])[4,2]
print(paste("SOCIAL SUPPORT ESTIMATE:", est_sup * 2))
print(paste("SOCIAL SUPPORT LOWER 95% CI:", (est_sup - 1.96*se_sup) * 2))
print(paste("SOCIAL SUPPORT UPPER 95% CI:", (est_sup + 1.96*se_sup) * 2))

#create a 95% confidence interval for the placebo effect
est_plac = as.data.frame(summary(final_cens_reg_model)["estimate"])[3,1]
se_plac = as.data.frame(summary(final_cens_reg_model)["estimate"])[3,2]
print(paste("PLACEBO ESTIMATE:", est_plac * 2))
print(paste("LOWER 95% CI:", est_plac - 1.96*se_plac))
print(paste("UPPER 95% CI:", est_plac + 1.96*se_plac))

#create a 95% confidence interval for the placebo by exercise block interaction
est_plac_eb = as.data.frame(summary(final_cens_reg_model)["estimate"])[12,1]
se_plac_eb = as.data.frame(summary(final_cens_reg_model)["estimate"])[12,2]
print(paste("LOWER 95% CI:", est_plac_eb - 1.96*se_plac_eb))
print(paste("UPPER 95% CI:", est_plac_eb + 1.96*se_plac_eb))

### ### ###

#this type of model ('censReg') does not produce estimates by participant or residuals; to compare the interaction between placebo and exercise block (session) t-tests will be used
no.placebo.block1 = subset(question_data,  placebo_condition == "off" & session_number == 1)
no.placebo.block2 = subset(question_data,  placebo_condition == "off" & session_number == 2)
placebo.block1 = subset(question_data,  placebo_condition == "on" & session_number == 1)
placebo.block2 = subset(question_data,  placebo_condition == "on" & session_number == 2)

#compare mean difficulty ratings by exercise block and placebo condition
mean(no.placebo.block1$Q2_ans_perc)
sd(no.placebo.block1$Q2_ans_perc)

mean(placebo.block1$Q2_ans_perc)
sd(placebo.block1$Q2_ans_perc)

mean(no.placebo.block2$Q2_ans_perc)
sd(no.placebo.block2$Q2_ans_perc)

mean(placebo.block2$Q2_ans_perc)
sd(placebo.block2$Q2_ans_perc)

#compare difficutly ratings between participants who received no placebo before the first exercise block to the ratings of participants who received the placebo before the first exercise block
block1_test_diff = t.test(placebo.block1$Q2_ans_perc, no.placebo.block1$Q2_ans_perc)
block1_test_diff
print(paste("MEAN DIFFERENCE ESTIMATE:", (block1_test_diff[4][1]$conf.int[1] + block1_test_diff[4][1]$conf.int[2])/2))
print(paste("LOWER 95% CI:", block1_test_diff[4][1]$conf.int[1]))
print(paste("UPPER 95% CI:", block1_test_diff[4][1]$conf.int[2]))

#compare difficutly ratings between participants who received no placebo before the second exercise block to the ratings of participants who received the placebo before the second exercise block
block2_test_diff = t.test(placebo.block2$Q2_ans_perc, no.placebo.block2$Q2_ans_perc)
block2_test_diff
print(paste("MEAN DIFFERENCE ESTIMATE:", (block2_test_diff[4][1]$conf.int[1] + block2_test_diff[4][1]$conf.int[2])/2))
print(paste("LOWER 95% CI:", block2_test_diff[4][1]$conf.int[1]))
print(paste("UPPER 95% CI:", block2_test_diff[4][1]$conf.int[2]))

### ### ###

#get get hand grip outputs for the four placebo by exercise block conditions
no.placebo.block1.hg = subset(total_data,  placebo_condition == "off" & session_number == 1)
no.placebo.block2.hg = subset(total_data,  placebo_condition == "off" & session_number == 2)
placebo.block1.hg = subset(total_data,  placebo_condition == "on" & session_number == 1)
placebo.block2.hg = subset(total_data,  placebo_condition == "on" & session_number == 2)

#compare mean hand grip outputs by exercise block and placebo condition
mean(no.placebo.block1.hg$percent_of_maximum)
sd(no.placebo.block1.hg$percent_of_maximum)

mean(placebo.block1.hg$percent_of_maximum)
sd(placebo.block1.hg$percent_of_maximum)

mean(no.placebo.block2.hg$percent_of_maximum)
sd(no.placebo.block2.hg$percent_of_maximum)

mean(placebo.block2.hg$percent_of_maximum)
sd(placebo.block2.hg$percent_of_maximum)

################################################################################################################################################

### PREDICTORS OF SOCIAL SUPPORT EFFECTS ON HANDGRIP OUTPUTS ###

library(dplyr)

#create the social support effect variable
support_control_data = total_data %>% 
                        group_by(Participant, support_or_control) %>% 
                          dplyr::summarise(mean_percent_maximum = mean(percent_of_maximum),
                                    sex = first(sex),
                                    sas = first(SAS),
                                    neuroticism = first(neuroticism),
                                    depend = first(depend),
                                    support_to_stranger_ratio = first(close_to_support_figure_during_exercise) / first(close_to_stranger_during_exercise))

control_dat = subset(support_control_data, support_control_data$support_or_control == "control")
support_data = subset(support_control_data, support_control_data$support_or_control == "support")

support_data$social_support_effect = support_data$mean_percent_maximum - control_dat$mean_percent_maximum

### ### ###

#test the effects of need for social assurance on social support effects on handgrip outputs
sas_model = lm(social_support_effect ~ sas + sex, data = support_data)
summary(sas_model)

#test the effects of neuroticism on social support effects on handgrip outputs
neuroticism_model = lm(social_support_effect ~ neuroticism + sex, data = support_data)
summary(neuroticism_model)

#test the effects of ability to depend on others on social support effects on handgrip outputs
depend_model = lm(social_support_effect ~ depend + sex, data = support_data)
summary(depend_model)

#test the effects of closeness to support figure (v. stranger) on social support effects on handgrip outputs
support_to_stranger_ratio_model = lm(social_support_effect ~ support_to_stranger_ratio + sex, data = support_data)
summary(support_to_stranger_ratio_model)

################################################################################################################################################

### PLOT SETTINGS ###

#fonts
quartzFonts(helv  = c('Helvetica-Bold',
                      'Helvetica-Bold',
                      'Helvetica-Oblique',
                      'Helvetica-BoldOblique'))

#theme settings
point_size = 1
header_size = 12
axis_title_size = 12
axis_size = 10

original_theme = theme(text=element_text(size=header_size,family='helv'),
                       title = element_text(size = header_size),
                       axis.text.x = element_text(color='black',size=axis_size),
                       axis.text.y = element_text(color='black',size=axis_size),
                       axis.title.x = element_text(size = axis_title_size, margin = margin(t = 10, r = 0, b = 0, l = 0)),
                       axis.title.y = element_text(size = axis_title_size, margin = margin(t = 0, r = 10, b = 0, l = 0)),
                       panel.background = element_rect(colour = '#FFFFFF',fill='#FFFFFF'),
                       panel.grid.major = element_line(color = '#e7e7e7'),
                       panel.grid.minor = element_blank(),
                       panel.grid.major.x = element_blank(), plot.margin = margin(1, 1, 1, 1, 'cm')) 

#colors for trial difficulty (colorblind friendly)
diff_cols_g_y_r = c("#009E73", "#F0E442", "#D55E00")
diff_cols_r_y_g = c("#D55E00", "#F0E442", "#009E73")

#colors for placebo
placebo_cols = c("grey75", "grey25")

### ### ###

#create a directory for the plots, set the current directory to this directory
cur_dir = code_dir
new_dir = paste0('../outputs/plots/')
dir.create(file.path(cur_dir, new_dir))
setwd(new_dir)

################################################################################################################################################

### HANDGRIP OUTPUTS (AS A PERCENTAGE OF MAXIMUM) BY TRIAL TARGET DIFFICULTY  ###

#handgrip strength by trial difficulty 
grip_dat = group_by(total_data, trial_difficulty) %>% 
  summarise(grip_mean = mean(percent_of_maximum, na.rm = TRUE),
            grip_sd = sd(percent_of_maximum, na.rm = TRUE))

#mean handgrip strength for each trial difficulty 
mean_40 = as.numeric(grip_dat[1,2])
mean_50 = as.numeric(grip_dat[2,2])
mean_60 = as.numeric(grip_dat[3,2])

#linearity of trial target difficulty effect on handgrip outputs
trial_diff_handgrip_lm = lm(percent_of_maximum ~ trial_difficulty, data = total_data)

grip_by_difficulty = ggplot(total_data, aes(x = as.factor(trial_difficulty), y = percent_of_maximum)) + 
                       geom_violin() + 
                       geom_boxplot() +
                       geom_point(aes(x = "1", y = mean_40), colour="grey", size = 3) + 
                       geom_point(aes(x = "2", y = mean_50), colour="grey", size = 3) + 
                       geom_point(aes(x = "3", y = mean_60), colour="grey", size = 3) +
                       geom_abline(slope = coef(trial_diff_handgrip_lm)[[2]], intercept = coef(trial_diff_handgrip_lm)[[1]]) +
                       scale_x_discrete(labels = c("40%", "50%", "60%")) + 
                       scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%", "125%")) +
                       xlab("Trial target difficulty\n(percentage of participant's maximum handgrip strength)") +
                       ylab("Handgrip output (as percentage of participant's maximum)") + 
                       original_theme

#save the plot
ggsave("handgrip_outputs_by_trial_target_difficulty.jpeg", grip_by_difficulty, width = 10, height = 7.5)

################################################################################################################################################

### PERCEIVED DIFFICULTY (AS A PERCENTAGE OF SLIDING SCALE) BY TRIAL TARGET DIFFICULTY  ###

#perceived difficulty by trial difficulty 
diff_dat = group_by(question_data, trial_difficulty) %>% 
  summarise(diff_mean = mean(Q2_ans_perc, na.rm = TRUE),
            diff_sd = sd(Q2_ans_perc, na.rm = TRUE))

#mean perceived difficulty for each trial difficulty 
mean_40 = as.numeric(diff_dat[1,2])
mean_50 = as.numeric(diff_dat[2,2])
mean_60 = as.numeric(diff_dat[3,2])

#linearity of trial target difficulty effect on perceived difficulty
trial_diff_difficulty_lm = lm(Q2_ans_perc ~ trial_difficulty, data = question_data)

q2_by_difficulty = ggplot(question_data, aes(x = as.factor(trial_difficulty), y = Q2_ans_perc)) + 
                     geom_violin() + 
                     geom_boxplot() +
                     geom_point(aes(x = "1", y = mean_40), colour="grey", size = 3) + 
                     geom_point(aes(x = "2", y = mean_50), colour="grey", size = 3) + 
                     geom_point(aes(x = "3", y = mean_60), colour="grey", size = 3) +
                     geom_abline(slope = coef(trial_diff_difficulty_lm)[[2]], intercept = coef(trial_diff_difficulty_lm)[[1]]) +
                     scale_x_discrete(labels = c("40%", "50%", "60%")) + 
                     scale_y_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), labels = c("0%", "25%", "50%", "75%", "100%")) +
                     xlab("Trial target difficulty\n(percentage of participant's maximum handgrip strength)") +
                     ylab("Perceived difficulty\n(as percentage of sliding scale)") +
                     original_theme

#save the plot
ggsave("perceived_difficulty_by_trial_target_difficulty.jpeg", q2_by_difficulty, width = 10, height = 7.5)

################################################################################################################################################

### HANDGRIP OUTPUTS (AS A PERCENTAGE OF MAXIMUM) BY EXERCISE SUB-BLOCK ###

#handgrip outputs by exercise sub-block
sub_block_dat = total_data %>% group_by(sub_block) %>% summarise(voltage_ave = mean(percent_of_maximum, na.rm = TRUE),
                                                                 voltage_se = sd(percent_of_maximum, na.rm = TRUE) / sqrt(n()))

#linearity of exercise sub-block effect on handgrip outputs
sub_b_handgrip_lm = lm(percent_of_maximum ~ sub_block, data  = total_data)

grip_by_sub_block = ggplot(total_data, aes(x = as.factor(sub_block), y = percent_of_maximum)) + 
                      geom_violin() + 
                      geom_boxplot() +
                      geom_point(aes(x = 1, y = sub_block_dat$voltage_ave[1]), colour="grey", size = 2) + 
                      geom_point(aes(x = 2, y = sub_block_dat$voltage_ave[2]), colour="grey", size = 2) + 
                      geom_point(aes(x = 3, y = sub_block_dat$voltage_ave[3]), colour="grey", size = 2) +
                      geom_point(aes(x = 4, y = sub_block_dat$voltage_ave[4]), colour="grey", size = 2) + 
                      geom_point(aes(x = 5, y = sub_block_dat$voltage_ave[5]), colour="grey", size = 2) + 
                      geom_point(aes(x = 6, y = sub_block_dat$voltage_ave[6]), colour="grey", size = 2) +
                      geom_point(aes(x = 7, y = sub_block_dat$voltage_ave[7]), colour="grey", size = 2) + 
                      geom_point(aes(x = 8, y = sub_block_dat$voltage_ave[8]), colour="grey", size = 2) + 
                      geom_point(aes(x = 9, y = sub_block_dat$voltage_ave[9]), colour="grey", size = 2) +
                      geom_point(aes(x = 10, y = sub_block_dat$voltage_ave[10]), colour="grey", size = 2) + 
                      geom_point(aes(x = 11, y = sub_block_dat$voltage_ave[11]), colour="grey", size = 2) + 
                      geom_point(aes(x = 12, y = sub_block_dat$voltage_ave[12]), colour="grey", size = 2) +
                      geom_point(aes(x = 13, y = sub_block_dat$voltage_ave[13]), colour="grey", size = 2) + 
                      geom_point(aes(x = 14, y = sub_block_dat$voltage_ave[14]), colour="grey", size = 2) + 
                      geom_abline(slope = coef(sub_b_handgrip_lm)[[2]], intercept = coef(sub_b_handgrip_lm)[[1]]) +
                      scale_y_continuous(limits = c(0, 125), breaks = c(seq(0, 125, 25)),
                                         labels = c("0%", "25%", "50%", "75%", "100%", "125%")) +
                      xlab("Exercise sub-block") +
                      ylab("Handgrip output (as percentage of participant's maximum)") +
                      original_theme

#save the plot
ggsave("handgrip_outputs_by_exercise_sub_block.jpeg", grip_by_sub_block, width = 10, height = 7.5)

################################################################################################################################################

### PERCEIVED DIFFICULTY (AS A PERCENTAGE OF SLIDING SCALE) BY EXERCISE SUB-BLOCK ###

#these analyses need to be run on a data set that has just one row per trial, since each question is answered only once per trial
question_data = subset(total_data, sample_reading_number == 1)

#perceived difficulty by exercise sub-block
sub_block_dat = question_data %>% group_by(sub_block) %>% summarise(diff_ave = mean(Q2_ans_perc, na.rm = TRUE),
                                                                    diff_se = sd(Q2_ans_perc, na.rm = TRUE) / sqrt(n()))

#linearity of exercise sub-block effect on perceived difficulty
sub_b_p_difficulty_lm = lm(Q2_ans_perc ~ sub_block, data = question_data)

q2_by_sub_block = ggplot(question_data, aes(x = as.factor(sub_block), y = Q2_ans_perc)) + 
                    geom_violin() + 
                    geom_boxplot() +
                    geom_point(aes(x = 1, y = sub_block_dat$diff_ave[1]), colour="grey", size = 2) + 
                    geom_point(aes(x = 2, y = sub_block_dat$diff_ave[2]), colour="grey", size = 2) + 
                    geom_point(aes(x = 3, y = sub_block_dat$diff_ave[3]), colour="grey", size = 2) +
                    geom_point(aes(x = 4, y = sub_block_dat$diff_ave[4]), colour="grey", size = 2) + 
                    geom_point(aes(x = 5, y = sub_block_dat$diff_ave[5]), colour="grey", size = 2) + 
                    geom_point(aes(x = 6, y = sub_block_dat$diff_ave[6]), colour="grey", size = 2) +
                    geom_point(aes(x = 7, y = sub_block_dat$diff_ave[7]), colour="grey", size = 2) + 
                    geom_point(aes(x = 8, y = sub_block_dat$diff_ave[8]), colour="grey", size = 2) + 
                    geom_point(aes(x = 9, y = sub_block_dat$diff_ave[9]), colour="grey", size = 2) +
                    geom_point(aes(x = 10, y = sub_block_dat$diff_ave[10]), colour="grey", size = 2) + 
                    geom_point(aes(x = 11, y = sub_block_dat$diff_ave[11]), colour="grey", size = 2) + 
                    geom_point(aes(x = 12, y = sub_block_dat$diff_ave[12]), colour="grey", size = 2) +
                    geom_point(aes(x = 13, y = sub_block_dat$diff_ave[13]), colour="grey", size = 2) + 
                    geom_point(aes(x = 14, y = sub_block_dat$diff_ave[14]), colour="grey", size = 2) +
                    geom_abline(slope = coef(sub_b_p_difficulty_lm)[[2]], intercept = coef(sub_b_p_difficulty_lm)[[1]]) +
                    scale_y_continuous(limits = c(0, 100), breaks = c(seq(0, 100, 10)),
                                       labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
                    xlab("Exercise sub-block") +
                    ylab("Perceived difficulty\n(as percentage of sliding scale)") +
                    original_theme

#save the plot
ggsave("perceived_difficulty_by_exercise_sub_block.jpeg", q2_by_sub_block, width = 10, height = 7.5)

################################################################################################################################################

### MEAN HANDGRIP OUTPUTS (AS A PERCENTAGE OF MAXIMUM) OVER TIME BY TRIAL TARGET DIFFICULTY  ###

#this creates the mean for of the trial difficulty levels
mean_data = group_by(total_data, sample_reading_number, trial_difficulty) %>%
              summarise(volt_mean = mean(percent_of_maximum, na.rm = TRUE))

#rename trial difficulty variable
mean_data$trial_difficulty = ifelse(mean_data$trial_difficulty == 1, "40% of maximum", 
                                    ifelse(mean_data$trial_difficulty == 2, "50% of maximum", 
                                           ifelse(mean_data$trial_difficulty == 3, "60% of maximum", NA)))

#reorder the factor for plot interpretation
mean_data$trial_difficulty = factor(mean_data$trial_difficulty, levels = c("60% of maximum", "50% of maximum", "40% of maximum"))

#plot the data
grip_by_time_by_difficulty = ggplot(na.omit(mean_data), aes(x = sample_reading_number, y = volt_mean, colour = trial_difficulty)) + 
                               geom_point() + geom_line() + 
                               scale_color_manual(values = diff_cols_r_y_g) +
                               ylab("Mean percentage of participant maximum handgrip strength") + xlab("Time (handgrip strength reading number)") + labs(color = "Target handgrip strength") +
                               geom_hline(yintercept=c(40, 50, 60), linetype="dotted", color = diff_cols_g_y_r) + 
                               scale_y_continuous(breaks=c(0,10,20,30,40,50,60), labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%")) +
                               scale_x_continuous(breaks = seq(0,160,10)) +
                               original_theme + theme(legend.key=element_blank())

#save the plot
ggsave("mean_handgrip_outputs_over_time_by_trial_target_difficulty.jpeg", grip_by_time_by_difficulty, width = 10, height = 7.5)

################################################################################################################################################

### HISTOGRAM OF PARTICIPANTS' ANSWERS TO THE POST-TRIAL QUESTION ON HOW HARD THE PREVIOUS TRIAL WAS ###

#plot the data
perceived_difficulty_hist = ggplot(data = question_data, aes(x = Q2_ans_perc)) + 
                            geom_histogram(binwidth=1, color = "black", fill = "grey") + 
                            ylab("Count") + xlab("Percent of sliding scale") + 
                            scale_x_continuous(breaks = seq(0,100,10), 
                                               labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
                            original_theme

#save the plot
ggsave("trial_perceived_difficulty_histogram.jpeg", perceived_difficulty_hist, width = 10, height = 5)

################################################################################################################################################

### MEAN HANDGRIP OUTPUTS BY TRIAL TARGET DIFFICULTY AND SOCIAL SUPPORT ###

#create a difference variable (difference between support figure and stranger face by trial difficulty for hand grip outputs)
difference_dat_hang_grip = data.frame(matrix(NA, nrow = 0, ncol = 3))

#create data for each participant
for (p in unique(total_data$Participant)){
  
  #subset the data for each target difficulty
  low = total_data[(total_data$Participant == p & total_data$trial_difficulty == 1), ]
  med = total_data[(total_data$Participant == p & total_data$trial_difficulty == 2), ]
  high = total_data[(total_data$Participant == p & total_data$trial_difficulty == 3), ]
  
  #get the hand grip outputs for each target difficulty
  low_support = low[(low$support_or_control == "support"), ][c("percent_of_maximum", "trial_number", "sample_reading_number", "trial_difficulty")]
  low_stranger = low[(low$support_or_control == "control"), ][c("percent_of_maximum", "trial_number", "sample_reading_number", "trial_difficulty")]
  
  med_support = med[(med$support_or_control == "support"), ][c("percent_of_maximum", "trial_number", "sample_reading_number", "trial_difficulty")]
  med_stranger = med[(med$support_or_control == "control"), ][c("percent_of_maximum", "trial_number", "sample_reading_number", "trial_difficulty")]
  
  high_support = high[(high$support_or_control == "support"), ][c("percent_of_maximum", "trial_number", "sample_reading_number", "trial_difficulty")]
  high_stranger = high[(high$support_or_control == "control"), ][c("percent_of_maximum", "trial_number", "sample_reading_number", "trial_difficulty")]
  
  #get the differences for each target difficulty (first create empty dataframes)
  low_diff = data.frame(matrix(NA, nrow =2240, ncol = 3))
  colnames(low_diff) = c("participant", "output_difference", "trial_difficulty")
  low_diff$participant = p
  low_diff$trial_difficulty = "40%"
  low_diff$output_difference = low_support$percent_of_maximum - low_stranger$percent_of_maximum
  low_diff$support_percent_maximum = low_support$percent_of_maximum
  low_diff$stranger_percent_maximum = low_stranger$percent_of_maximum
  low_diff$support_percent_increase = ((low_support$percent_of_maximum - low_stranger$percent_of_maximum) / low_stranger$percent_of_maximum)*100
  
  med_diff = data.frame(matrix(NA, nrow =2240, ncol = 3))
  colnames(med_diff) = c("participant", "output_difference", "trial_difficulty")
  med_diff$participant = p
  med_diff$trial_difficulty = "50%"
  med_diff$output_difference = med_support$percent_of_maximum - med_stranger$percent_of_maximum
  med_diff$support_percent_maximum = med_support$percent_of_maximum
  med_diff$stranger_percent_maximum = med_stranger$percent_of_maximum
  med_diff$support_percent_increase = ((med_support$percent_of_maximum - med_stranger$percent_of_maximum) / med_stranger$percent_of_maximum)*100
  
  high_diff = data.frame(matrix(NA, nrow =2240, ncol = 3))
  colnames(high_diff) = c("participant", "output_difference", "trial_difficulty")
  high_diff$participant = p
  high_diff$trial_difficulty = "60%"
  high_diff$output_difference = high_support$percent_of_maximum - high_stranger$percent_of_maximum
  high_diff$support_percent_maximum = high_support$percent_of_maximum
  high_diff$stranger_percent_maximum = high_stranger$percent_of_maximum
  high_diff$support_percent_increase = ((high_support$percent_of_maximum - high_stranger$percent_of_maximum) / high_stranger$percent_of_maximum)*100
  
  #add to the total data frame
  difference_dat_hang_grip = rbind(difference_dat_hang_grip, low_diff)
  difference_dat_hang_grip = rbind(difference_dat_hang_grip, med_diff)
  difference_dat_hang_grip = rbind(difference_dat_hang_grip, high_diff)
  
}

#get the average difference for each participant
average_dat_outputs = difference_dat_hang_grip %>% group_by(participant, trial_difficulty) %>% summarise(mean_diff = mean(output_difference))

#get the mean differences for each condition
average_dat_outputs %>% group_by(trial_difficulty) %>% summarise(mean_percent_difference = mean(mean_diff), sd_percent_difference = sd(mean_diff))

#plot the data
avp_outputs = ggplot(average_dat_outputs, aes(x=trial_difficulty, y=mean_diff)) + 
                geom_violin(aes(fill = trial_difficulty)) + 
                geom_boxplot(width = 0.1) +
                geom_hline(yintercept = 0, linetype = "dashed") + 
                scale_y_continuous(limits = c(-7,10), breaks = c(-5, 0, 5, 10), labels = c("-5%", "0%\n(no effect)", "+5%", "+10%")) +
                xlab("Trial target difficulty\n(percentage of participant's maximum handgrip strength)") +
                ylab("Mean social support effect (by participant) on handgrip outputs") + 
                coord_flip() + 
                scale_fill_manual(values = diff_cols_g_y_r) +
                theme(legend.position="none") +
                original_theme 

#save the plot
ggsave("average_social_support_effect_on_handgrip_outputs_by_trial_target_difficulty.jpeg", avp_outputs, width = 10, height = 7.5)

### ### ###

#get participant social support percent increase means for each level of trial target difficulty
percent_by_participant = difference_dat_hang_grip %>% group_by(participant, trial_difficulty) %>% summarise(mean_support = mean(support_percent_maximum),mean_control = mean(stranger_percent_maximum))
percent_by_participant$percent_increase = ((percent_by_participant$mean_support - percent_by_participant$mean_control) / percent_by_participant$mean_control) * 100
percent_by_participant %>% group_by(trial_difficulty) %>% summarise(mean_percent_increase = mean(percent_increase))

################################################################################################################################################

### PERCEIVED DIFFICULTY BY PLACEBO AND EXERCISE BLOCK ###

#get the difficulty rating for each participant
average_dat_difficulty_plac_eb = question_data %>% group_by(Participant, session_number, placebo_condition) %>% summarise(mean_difficulty = mean(Q2_ans_perc))
average_dat_difficulty_plac_eb$placebo_condition = ifelse(average_dat_difficulty_plac_eb$placebo_condition == "on", "Placebo",
                                                   ifelse(average_dat_difficulty_plac_eb$placebo_condition == "off", "Control", NA))

#create a variable that 
dodge = position_dodge(width = 1)
pl_eb_difficulty = ggplot(average_dat_difficulty_plac_eb, aes(x = as.factor(session_number), y = mean_difficulty)) +
                     geom_violin(aes(fill = placebo_condition), position = dodge) +
                     geom_boxplot(aes(group = interaction(placebo_condition, as.factor(session_number))), width=0.3, fill="white", position=dodge,outlier.shape=NA) + 
                     scale_y_continuous(limits = c(20,100), breaks = c(seq(20, 100, 10)), labels = c("20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
                     scale_x_discrete(labels = c("First", "Second")) +
                     xlab("Exercise block") +
                     ylab("Perceived difficulty\n(as percentage of sliding scale)") + 
                     scale_fill_manual(name = "Placebo\ncondition", values = placebo_cols, labels = c("Control", "Placebo")) +
                     original_theme

#save the plot
ggsave("perceived_difficulty_by_exercise_block_and_placebo_condition.jpeg", pl_eb_difficulty, width = 7.5, height = 7.5, units = "in")

################################################################################################################################################

### ASSUMPTION CHECKS ###

library(nlme)
library(AER)

#load local functions adapted from the R script provided by Snijders & Bosker (2012); https://www.stats.ox.ac.uk/~snijders/ch10.r
source("../../analysis_code/assumption_checks.R")

#create a directory for the assumption check results, set the current directory to this directory
cur_dir = code_dir
new_dir = paste0('../outputs/assumption_checks/')
dir.create(file.path(cur_dir, new_dir))
setwd(file.path(cur_dir, new_dir))

################################################################################################################################################

### MULTILEVEL MODEL ON PARTICIPANTS' HANDGRIP OUTPUTS ###

#create a directory for the assumption check results for this model, set the current directory to this directory
dir.create('./multilevel_regression_model_on_handgrip_outputs/')
setwd('./multilevel_regression_model_on_handgrip_outputs/')

################################################################################################################################################

### MULTILEVEL MODEL ON PARTICIPANTS' HANDGRIP OUTPUTS - LEVEL-ONE HOMOSCEDASTICITY ###

#regressions at each level-two unit ('Participant')
olselist = lmList(percent_of_maximum ~ 1 + support_or_control*placebo_condition + support_or_control*trial_difficulty_scaled + placebo_condition*trial_difficulty_scaled +
                                       sub_block_scaled + session_number_scaled + Q1_ans_perc_scaled | Participant, data = total_data)

#run a multilevel model on all the data with 'Participant' as the level-two unit (this is the same as the 'final_model', except is estimated with the 'nlme' instead of 'lme4' package)
final_model_nlme = nlme::lme(percent_of_maximum ~ 1 + support_or_control*placebo_condition + support_or_control*trial_difficulty_scaled + placebo_condition*trial_difficulty_scaled +
                                                  sub_block_scaled + session_number_scaled + Q1_ans_perc_scaled, random = ~ placebo_condition | Participant, control = lmeControl(opt = 'optim'), data = total_data)

#compare the estimates from the participant regression models to those from the main multilevel model
olse_coefs = coef(olselist)
mlm_coefs = coef(final_model_nlme)
comp.models = compareFits(olse_coefs, mlm_coefs)

#intercept coefficients from olselist
frame1_int_olselist = as.data.frame(comp.models[, 1, 1])
colnames(frame1_int_olselist)[1] = "Estimate"
frame1_int_olselist$coefficient = "Intercept"
frame1_int_olselist$Model = "OLS regression"
frame1_int_olselist$level_two_unit = rownames(frame1_int_olselist)

#intercept coefficients from lme
frame1_int_lme = as.data.frame(comp.models[, 2, 1])
colnames(frame1_int_lme)[1] = "Estimate" 
frame1_int_lme$coefficient = "Intercept"
frame1_int_lme$Model = "Multilevel regression"
frame1_int_lme$level_two_unit = rownames(frame1_int_lme)

#predictor coefficients from olselist (subtract mean of other variable predictions for plotting)
frame2_pred_olselist = as.data.frame(comp.models[, 1, "placebo_condition1"])
colnames(frame2_pred_olselist)[1] = "Estimate"
frame2_pred_olselist$coefficient = "Predictor"
frame2_pred_olselist$Model = "OLS regression"
frame2_pred_olselist$level_two_unit = rownames(frame2_pred_olselist)

#predictor coefficients from lme; drop the first column (which is olselist coefficients)
frame2_pred_lme = as.data.frame(comp.models[, , "placebo_condition1"])
colnames(frame2_pred_lme)[2] = "Estimate" 
frame2_pred_lme$coefficient = "Predictor"
frame2_pred_lme$Model = "Multilevel regression"
frame2_pred_lme$level_two_unit = rownames(frame2_pred_lme)
frame2_pred_lme = frame2_pred_lme[ , c(2,3,4,5)]

#add the random intercept to the random slope estimates
frame2_pred_lme$Estimate = frame2_pred_lme$Estimate + frame1_int_lme$Estimate

#combine and create row name variable
new_frame = do.call("rbind", list(frame1_int_olselist, frame1_int_lme, frame2_pred_olselist, frame2_pred_lme)) 

#create list and function from renaming predictor variable
variable_names = list('Intercept'="Intercept",'Predictor'= "Placebo condition") 
variable_labeller = function(variable,value){return(variable_names[value])}

### ### ###

#create random effects estimates that take into account the random slope estimate
ols_intercept = subset(new_frame, new_frame$coefficient == "Intercept" & new_frame$Model == "OLS regression")
ols_predictor = subset(new_frame, new_frame$coefficient == "Predictor" & new_frame$Model == "OLS regression")
mlm_intercept = subset(new_frame, new_frame$coefficient == "Intercept" & new_frame$Model == "Multilevel regression")
mlm_predictor = subset(new_frame, new_frame$coefficient == "Predictor" & new_frame$Model == "Multilevel regression")
mlm_predictor$Estimate = mlm_predictor$Estimate - mlm_intercept$Estimate

### ### ###

#make the data to plot
hom_dat = do.call("rbind", list(ols_intercept, ols_predictor, mlm_intercept, mlm_predictor))

#make the plot
l_1_homoscedasticity = ggplot(hom_dat, aes(x=Estimate, y=level_two_unit, group=Model)) + 
                       geom_point(aes(colour=Model), na.rm = TRUE) + 
                       suppressWarnings(facet_grid(. ~ coefficient, scales = 'free', labeller=variable_labeller)) +
                       ylab("Level-two unit") +
                       theme(legend.key = element_rect(fill = "transparent", colour = "transparent"),
                             axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1), margin = margin(t = 10, r = 0, b = 0, l = 0)),
                             axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1), margin = margin(t = 0, r = 5, b = 0, l = 0)),
                             axis.text.y = element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.text.x = element_text(family="Arial", colour="black", size=rel(1)),
                             legend.title = element_text(family="Arial", face="bold", colour="black", size=rel(1)),
                             axis.line = element_line(colour = "black"),
                             panel.border = element_blank(), 
                             panel.background = element_blank(),
                             strip.background = element_blank(),
                             strip.text = element_text(family="Arial", face="bold", colour="black", size=rel(1)))

#save the plot
ggsave("visualisation_for_level_one_heteroscedasticity.jpg", l_1_homoscedasticity, width = 15, height = 10, units = "in")

### ### ###

#test heteroscedasticity (first drop any infinite or NA values)
df_res = sapply(olselist, function(x){x$df.residual})
sigma2_res = suppressWarnings(sapply(olselist, function(x){(summary(x)$sigma)^2}))
parts = (df_res*log(sigma2_res))
parts = parts[!is.na(parts) & !is.infinite(parts)]

ls_tot = sum(parts)/sum(df_res)
d = (sqrt(0.5*df_res))*(log(sigma2_res) - ls_tot)
d = d[!is.na(d) & !is.infinite(d)]

#convert d to a dataframe
d_dat = as.data.frame(d)
d_dat$id = rownames(d_dat)

#get d scores from all participants
d_list = c()
counter = 1
for (i in d_dat$id){
  d = as.numeric(d_dat[ which(d_dat$id == i), ]['d'])
  d_list[[counter]] = d
  counter = counter + 1
}

#test score
H = round(sum(d_list^2), digits = 2)
df = sum(length(d_list))-1

#the associated p-value is:
p_val = round(1-pchisq(H, df), digits = 7)
cat("LEVEL-ONE HOMOSCEDASTICITY", file = 'assumption_tests.txt', append=TRUE, sep = "\n")
cat(paste("Homoscedasticity test statistic (H):", H), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
cat(paste("Homoscedasticity test degrees of freedom:", df), file = 'assumption_tests.txt', append=TRUE, sep = "\n")
cat(paste("Homoscedasticity test p-value (significance indicates heteroscedasticity):", p_val), file = 'assumption_tests.txt', append=TRUE, sep = "\n")

#this will plot d, which is Gausian if there is level-one homoscedasticity
jpeg('d_normality_for_level_one_homoscedasticity.jpg', width = 5, height = 5, units = "in", res = 300, pointsize = 12, quality = 100)
qqnorm(d_list, main = "")
qqline(d_list)
dev.off()

################################################################################################################################################

### MAIN MULTILEVEL MODEL ON PARTICIPANTS' HANDGRIP OUTPUTS - LEVEL-ONE LINEARITY ###

#this is the level-one variable to check linearity with
level_one_variable1 = "trial_difficulty_scaled"
level_one_variable2 = "Q1_ans_perc_scaled"
level_one_variable3 = "sub_block_scaled"

#get within-group OLS residuals
resi = residuals(olselist) 

#make the plots
jpeg('level_one_linearity_target_trial_difficulty.jpg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
plot(total_data[, level_one_variable1], resi, xlab = "Target trial difficulty\n(percentage of participant's maximum handgrip strength)", ylab = "Residual", xaxt = "n")
lines(lowess(total_data[, level_one_variable1], resi))
axis(1, at = seq(-1, 1, 0.5), labels = c("40%", "", "50%", "", "60%"))
dev.off()

jpeg('level_one_linearity_perceived_difficulty.jpg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
plot(total_data[, level_one_variable2], resi, xlab = expression("Perceived difficulty (scaled)"), ylab = "Residual")
lines(lowess(total_data[, level_one_variable2], resi), col = "dodgerblue1")
dev.off()

jpeg('level_one_linearity_exercise_sub_block.jpg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
sub_block_scaled_list = c(-1.6124507, -1.3643814, -1.1163120, -0.8682427, -0.6201734, -0.3721040, -0.1240347, 0.1240347, 0.3721040, 0.620173352502387, 0.8682427, 1.1163120345043, 1.3643814, 1.61245071650621)
plot(total_data[, level_one_variable3], resi, xlab = "Exercise sub-block", ylab = "Residual", xaxt = "n")
lines(lowess(total_data[, level_one_variable3], resi), col = "dodgerblue1")
axis(side = 1, at = sub_block_scaled_list, labels = FALSE)
text(sub_block_scaled_list, labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"), par("usr")[3], offset = 1, pos = 1, xpd = TRUE)
dev.off()

################################################################################################################################################

### MAIN MULTILEVEL MODEL ON PARTICIPANTS' HANDGRIP OUTPUTS - LEVEL-ONE RESIDUAL NORMALITY ###

#compute within-participant studentized OLS residuals
resi_st = by(total_data, total_data[,"Participant"], function(total_data) rstudent(lm(percent_of_maximum ~ 1 + trial_difficulty_scaled*placebo_condition*support_or_control + sub_block_scaled + session_number_scaled + Q1_ans_perc_scaled, data = total_data)))

#unlist
rs = unlist(resi_st)

#make a QQ plot
jpeg('ols_residual_normality.jpg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
qqnorm(rs, main = "")
qqline(rs)
dev.off()

################################################################################################################################################

### MAIN MULTILEVEL MODEL ON PARTICIPANTS' HANDGRIP OUTPUTS - MULTICOLLINEARITY ###

#get the variance inflation factor (VIF) mean
cat("\nLEVEL-ONE MULTICOLLINEARITY", file = 'assumption_tests.txt', append=TRUE, sep = "\n")
cat(paste("Variance inflation factor (VIF) mean:", round(mean(vif(final_handgrip_model)[1:6]), digits = 2)), file = 'assumption_tests.txt', append=TRUE)

#get the variance inflation factor (VIF) range
cat(paste0("\nVIF range: ", round(range(vif(final_handgrip_model)[1:6])[1], digits = 2), ", ", 
           round(range(vif(final_handgrip_model)[1:6])[2], digits = 2)), file = 'assumption_tests.txt', append=TRUE)

#get the VIF for each predictor
cat(paste("\nVIF for the social support condition predictor:", round(vif(final_handgrip_model)[1], digits = 2)), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for the placebo condition predictor:", round(vif(final_handgrip_model)[2], digits = 2)), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for the trial target difficulty predictor:", round(vif(final_handgrip_model)[3], digits = 2)), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for the exercise block predictor:", round(vif(final_handgrip_model)[5], digits = 2)), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for the exercise sub-block predictor:", round(vif(final_handgrip_model)[4], digits = 2)), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for the reported effort predictor:", round(vif(final_handgrip_model)[6], digits = 2)), file = 'assumption_tests.txt', append=TRUE)

################################################################################################################################################

### MAIN MULTILEVEL MODEL ON PARTICIPANTS' HANDGRIP OUTPUTS - LEVEL-TWO RESIDUAL NORMALITY ###

#get the random effects for the model
mod_re = lme4::ranef(final_handgrip_model, condVar=TRUE)

#get the posterior means for random intercepts for each level-two unit
postmean_int =  as.data.frame(mod_re["Participant"])[, 1]

#this will get the posterior variances for the random intercepts
postmeanvar_int = attr(mod_re[["Participant"]],'postVar')[1,1,]

#calculate diagnostic variances and posterior means for the random intercept
diagmeanvar_int = VarCorr(final_handgrip_model)[["Participant"]][1,1] - postmeanvar_int
postmean_stand_int = postmean_int/sqrt(diagmeanvar_int)

#get the posterior means for random slopes for each level-two unit
postmean_slope = as.data.frame(mod_re["Participant"])[, 2] 
  
#this will get the posterior variances for the random slopes
postmeanvar_slope = attr(mod_re[["Participant"]],'postVar')[2,2,] 
  
#calculate diagnostic variances and posterior means for the random slopes
diagmeanvar_slope = VarCorr(final_handgrip_model)[["Participant"]][2,2] - postmeanvar_slope 
postmean_stand_slope = postmean_slope/sqrt(diagmeanvar_slope)
  
#make plots for the intercepts and slopes
jpeg('level_two_residual_normality_intercept.jpeg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
qqnorm(postmean_stand_int, main = "")
qqline(postmean_stand_int)
dev.off()

jpeg('level_two_residual_normality_slope.jpeg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
qqnorm(postmean_stand_slope, main = "")
qqline(postmean_stand_slope)
dev.off()

################################################################################################################################################

### MAIN MULTILEVEL MODEL ON PARTICIPANTS' HANDGRIP OUTPUTS - LEVEL-TWO UNIT INFLUENCE ###

library(influence.ME)

#a function for checking the influence of level-two units (participants) using Cook's distances
check_influence(final_handgrip_model, "Participant")

#estimate the model removing one participant at a time so that their effects on the overall results can be assessed
alt.est = influence(final_handgrip_model, group = "Participant")

#the sigtest function tests whether excluding a particular level-two unit changes the statistical significance (at 1.96) of a model’s predictor variables (Nieuwenhuis et al. 2012)
exclusions = sigtest(alt.est)
sig.test.dataframe = as.data.frame(exclusions)

#use only columns for predictor variables (first three are about the intercept) that are about changes in significance
subset = sig.test.dataframe[, c(4:ncol(sig.test.dataframe))]

#check whether the predictor variable(s) changed significance
changed_sig_support = subset[, grepl(paste("support_or_control1",".Changed.Sig", sep = ""), names(subset))]
changed_sig_support_difficulty = subset[, grepl(paste("support_or_control1.trial_difficulty_scaled",".Changed.Sig", sep = ""), names(subset))]

trues_support = table(changed_sig_support)["TRUE"]
trues_support = if (is.na(trues_support) == TRUE) 0 else trues_support
trues_support_difficulty = table(changed_sig_support_difficulty)["TRUE"]
trues_support_difficulty = if (is.na(trues_support_difficulty) == TRUE) 0 else trues_support_difficulty

falses_support = table(changed_sig_support)["FALSE"]
falses_support = if (is.na(falses_support) == TRUE) 0 else falses_support
falses_support_difficulty = table(changed_sig_support_difficulty)["FALSE"]
falses_support_difficulty = if (is.na(falses_support_difficulty) == TRUE) 0 else falses_support_difficulty

percent_true_support = round(as.integer((trues_support / (trues_support + falses_support)) * 100), digits = 2)
percent_true_support_difficulty = round(as.integer((trues_support_difficulty / (trues_support_difficulty + falses_support_difficulty)) * 100), digits = 2)

cat(paste0("Percentage of level-two units that, if removed from the model, would change the significance of the 'main effect' of social support: ", percent_true_support, "%", "\n"), file = 'assumption_tests.txt', append=TRUE)
cat(paste0("Percentage of level-two units that, if removed from the model, would change the significance of the social support by trial target difficulty condition interaction: ", percent_true_support_difficulty, "%"), file = 'assumption_tests.txt', append=TRUE)

################################################################################################################################################

### MULTILEVEL CENSORED REGRESSION MODEL ON POST EXERCISE TRIAL QUESTION ON PERCEIVED DIFFUCULTY ###

library(AER)
library(lme4)
library(lmerTest)

#create a directory for the assumption check results for this model, set the current directory to this directory
dir.create('../multilevel_censored_regression_model_on_perceived_difficulty/')
setwd('../multilevel_censored_regression_model_on_perceived_difficulty/')

### ### ###

#contrast sum the exercise block variable
question_data$session_number_scaled = ordered(question_data$session_number, levels = c(1, 2))
contrasts(question_data$session_number_scaled) = contr.sum(2)

#the 'censReg' function does not produce residuals, so a censored regression (with 'Participant' as a factor) and multilevel model are estimated on participants' perceived difficulty scores (four-way interaction removed to avoid singularity)
tobit_Q2 = tobit(Q2_ans_perc ~ trial_difficulty_scaled*placebo_condition*support_or_control + session_number_scaled + sub_block_scaled + Q1_ans_perc_scaled + Participant, left = 0, right = 100, data = question_data)
summary(tobit_Q2)

Q2_results_full_model = lmer(Q2_ans_perc ~ 1 + trial_difficulty_scaled*placebo_condition*support_or_control*session_number_scaled + sub_block_scaled + Q1_ans_perc_scaled + (1 | Participant), data = question_data)
summary(Q2_results_full_model)

################################################################################################################################################

### MULTILEVEL CENSORED REGRESSION MODEL ON POST EXERCISE TRIAL QUESTION ON PERCEIVED DIFFUCULTY - LEVEL-ONE LINEARITY AND HOMOSCEDASTICITY ###

#plot fitted values v. residual values
jpeg('fitted_v_residual_plot.jpeg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
plot(fitted(tobit_Q2), resid(tobit_Q2), ylab = "Residuals", xlab = "Fitted values")
dev.off()

################################################################################################################################################

### MULTILEVEL CENSORED REGRESSION MODEL ON POST EXERCISE TRIAL QUESTION ON PERCEIVED DIFFUCULTY - LEVEL-ONE RESIDUAL NORMALITY ###

#make a qq plot of the model residuals
jpeg('level_one_residual_qq_plot.jpeg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
qqnorm(residuals(tobit_Q2), main = "")
qqline(residuals(tobit_Q2))
dev.off()

################################################################################################################################################

### MULTILEVEL CENSORED REGRESSION MODEL ON POST EXERCISE TRIAL QUESTION ON PERCEIVED DIFFUCULTY - MULTICOLLINEARITY ###

#get the variance inflation factor (VIF) mean
cat("LEVEL-ONE MULTICOLLINEARITY", file = 'assumption_tests.txt', append=TRUE, sep = "\n")
cat(paste("Variance inflation factor (VIF) mean:", round(mean(vif(tobit_Q2)[c(1:6), 1]), digits = 2)), file = 'assumption_tests.txt', append=TRUE)

#get the variance inflation factor (VIF) range
cat(paste0("\nVIF range: ", round(range(vif(tobit_Q2)[c(1:6), 1])[1], digits = 2), ", ", round(range(vif(tobit_Q2)[c(1:6), 1])[2], digits = 2)), file = 'assumption_tests.txt', append=TRUE)

#get the VIF for each predictor
cat(paste("\nVIF for the social support condition predictor:", round(vif(tobit_Q2)[c(1:6), 1][3], digits = 2)), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for the placebo condition predictor:", round(vif(tobit_Q2)[c(1:6), 1][2], digits = 2)), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for the trial target difficulty predictor:", round(vif(tobit_Q2)[c(1:6), 1][1], digits = 2)), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for the exercise block predictor:", round(vif(tobit_Q2)[c(1:6), 1][4], digits = 2)), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for the exercise sub-block predictor:", round(vif(tobit_Q2)[c(1:6), 1][5], digits = 2)), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for the reported effort predictor:", round(vif(tobit_Q2)[c(1:6), 1][6], digits = 2)), file = 'assumption_tests.txt', append=TRUE)
          
################################################################################################################################################

### MULTILEVEL CENSORED REGRESSION MODEL ON POST EXERCISE TRIAL QUESTION ON PERCEIVED DIFFUCULTY - LEVEL-TWO RESIDUAL NORMALITY ###

#get the random intercepts for the model
mod_re = ranef(Q2_results_full_model, condVar = TRUE, standard = TRUE)

#get a dataset of all non-standardized Empirical Bayes (EB) residuals (use this for level-two fixed effects; Snijders & Bosker, 2012; p. 64)
postmean = mod_re$Participant[,1]

#posterior variances
postmeanvar = attr(mod_re$Participant, 'postVar')[1,1,]

#diagnostic variances
diagmeanvar = VarCorr(Q2_results_full_model)$Participant[1,1] - postmeanvar

#standardised level-two random intercept residuals
postmean_stand = postmean/sqrt(diagmeanvar)

#plot standardised level-two random intercept residual normality
jpeg('level_two_residual_normality_intercepts.jpeg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
qqnorm(postmean_stand, main = "")
qqline(postmean_stand)
dev.off()

#perform a Shapiro-Wilks test
sw_w = round(as.numeric(shapiro.test(postmean_stand)[1]), digits = 2)
sw_p = as.numeric(shapiro.test(postmean_stand)[2])
cat("\n", "LEVEL-TWO RESIDUAL NORMALITY", file = 'assumption_tests.txt', append=TRUE, sep = "\n")
cat(paste0("Shapiro-Wilks test of level-two residual normality: W = ", sw_w, ", p = ", sw_p), file = 'assumption_tests.txt', append=TRUE)

################################################################################################################################################

### MULTILEVEL CENSORED REGRESSION MODEL ON POST EXERCISE TRIAL QUESTION ON PERCEIVED DIFFUCULTY - LEVEL-TWO OUTLIERS ###

#a function for checking the influence of level-two units (participants) using Cook's distances
check_influence(Q2_results_full_model, "Participant")

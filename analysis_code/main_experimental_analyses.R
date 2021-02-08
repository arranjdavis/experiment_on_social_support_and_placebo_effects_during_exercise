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

#get the mean, sd, and range of responses to the question about whether participants felt that the beta-alanine (i.e., the placebo) significantly improved their performance
mean(post_experiment_data$beta_alanine_performance_effect)
sd(post_experiment_data$beta_alanine_performance_effect)
range(post_experiment_data$beta_alanine_performance_effect)

#one sample t-test to see if scores differ from 0 (make 50 or "no difference" equal 0)
post_experiment_data$beta_alanine_performance_effect = post_experiment_data$beta_alanine_performance_effect - 50
t.test(post_experiment_data$beta_alanine_performance_effect)

### ### ###

#get the mean, sd, and range of of responses to the question about whether participants felt that the beta-alanine (i.e., the placebo) made the exercise trials easier
mean(post_experiment_data$beta_alanine_difficulty_effect)
sd(post_experiment_data$beta_alanine_difficulty_effect)
range(post_experiment_data$beta_alanine_difficulty_effect)

#one sample t-test to see if scores differ from 0 (make 50 or "no difference" equal 0)
post_experiment_data$beta_alanine_difficulty_effect = post_experiment_data$beta_alanine_difficulty_effect - 50
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

### HANDGRIP STRENGTH, PERCEIVED DIFFICULTY, AND EFFORT MEASURES ###

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
group_by(total_data, trial_difficulty) %>% summarise(diff = mean(Q2_ans_perc, na.rm = TRUE),
                                                     sd = sd(Q2_ans_perc, na.rm = TRUE))

#perceived difficulty by each experimental condition
group_by(total_data, support_or_control, placebo_condition, trial_difficulty) %>% summarise(diff = mean(Q2_ans_perc, na.rm = TRUE),
                                                                                            sd = sd(Q2_ans_perc, na.rm = TRUE))

#perceived difficulty by social support and placebo condition
group_by(total_data, support_or_control, placebo_condition) %>% summarise(diff = mean(Q2_ans_perc, na.rm = TRUE),
                                                                                      sd = sd(Q2_ans_perc, na.rm = TRUE))

#perceived difficulty by social support condition
group_by(total_data, support_or_control ) %>% summarise(diff = mean(Q2_ans_perc, na.rm = TRUE),
                                                        sd = sd(Q2_ans_perc, na.rm = TRUE))

#perceived difficulty by social support condition
group_by(total_data, placebo_condition ) %>% summarise(diff = mean(Q2_ans_perc, na.rm = TRUE),
                                                        sd = sd(Q2_ans_perc, na.rm = TRUE))

#perceived difficulty by exercise block
group_by(total_data, session_number ) %>% summarise(diff = mean(Q2_ans_perc, na.rm = TRUE),
                                                        sd = sd(Q2_ans_perc, na.rm = TRUE))

### ### ###

#effort by trial target difficulty
group_by(total_data, trial_difficulty) %>% summarise(effort = mean(Q1_ans_perc, na.rm = TRUE))

#effort by exercise block
group_by(total_data, session_number) %>% summarise(effort = mean(Q1_ans_perc, na.rm = TRUE))

### ### ###

#correlation beteen participants' answers to the two questions post-trial questions on perceived difficulty and effort
cor(question_data$Q1_ans_perc, question_data$Q2_ans_perc)

#check question censoring for the two questions
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

#this will change the appropriate variables to factors
total_data$placebo_condition = as.factor(total_data$placebo_condition)
total_data$Participant = as.factor(total_data$Participant)
total_data$trial_difficulty.f = as.factor(total_data$trial_difficulty)

#this will change the appropriate variables to integers
total_data$session_number = as.integer(total_data$session_number)
total_data$trial_number = as.integer(total_data$trial_number)
total_data$trial_difficulty = as.integer(total_data$trial_difficulty)

#make the 'trial_difficulty' variable an ordered factor
total_data$trial_difficulty_o = ordered(total_data$trial_difficulty)

### ### ###

#test the interaction between placebo and exercise block; this is the most complex random effects structure that allows for convergence
placebo_exercise_block_interaction = lmer(percent_of_maximum ~ placebo_condition*session_number + trial_number + sex + Q1_ans_perc + (1 + placebo_condition | Participant), data = total_data)
summary(placebo_exercise_block_interaction)

#test the trial difficulty, placebo, and social support interaction, drop the non-significant 'sex' variable and placebo by exercise block interaction, and the lowest variance random effect (social support condition); this is the most complex model that allows for convergence
difficulty_placebo_support_interaction = lmer(percent_of_maximum ~ 1 + trial_difficulty_o*placebo_condition*support_or_control + trial_number + session_number + Q1_ans_perc + (1 + placebo_condition | Participant), data = total_data)
summary(difficulty_placebo_support_interaction)

#keep the three-way interaction (the constituent difficulty by social support interaction is significant); this is the most complex model that allows for convergence
final_model = lmer(percent_of_maximum ~ 1 + trial_difficulty_o*placebo_condition*support_or_control + trial_number + session_number + Q1_ans_perc + (1 + placebo_condition | Participant), data = total_data)
summary(final_model)

#create a 95% confidence interval for the social support by trial difficulty interaction
int_est = summary(final_model)$coefficients[11,1]
int_se = summary(final_model)$coefficients[11,2]
print(paste("UPPER 95% CI:", int_est + 1.96*int_se))
print(paste("LOWER 95% CI:", int_est - 1.96*int_se))

#model R-squared values
r.squaredGLMM(final_model)

#test post-hoc contrasts 
contrasts = as.data.frame(summary(emmeans(final_model, data = total_data, pairwise ~ support_or_control | trial_difficulty_o)))

#multiply the comparison estimate by -1 to get 'support - control' comparisons
contrasts$contrasts.estimate.reversed = contrasts$contrasts.estimate * -1

#increase the SE to account for degree of freedom estimates being disabled (estimated SEs based on SEs calculated with sufficient RAM for df calculations)
contrasts$contrasts.SE.adjusted = contrasts$contrasts.SE * 2.812

#adjust Z-scores, p-values, and 95% CIs (making them more conservative)
contrasts$contrasts.z.ratio.adjusted = contrasts$contrasts.estimate.reversed / contrasts$contrasts.SE.adjusted
contrasts$contrasts.p.value.adjusted = 2 * pnorm(contrasts$contrasts.z.ratio.adjusted, lower.tail = FALSE)
contrasts$lower.95CI.contrasts.estimate.adjusted = contrasts$contrasts.estimate.reversed - (1.96*contrasts$contrasts.SE.adjusted)
contrasts$upper.95CI.contrasts.estimate.adjusted = contrasts$contrasts.estimate.reversed + (1.96*contrasts$contrasts.SE.adjusted)

#print results
print(paste0("SUPPORT - CONTROl AT 40% TARGET HANDGRIP STRENGTH:", 
            " estimate = ", round(contrasts$contrasts.estimate.reversed[1], 3),
            ", SE = ", round(contrasts$contrasts.SE.adjusted[1], 3),
            ", Z = ", round(contrasts$contrasts.z.ratio.adjusted[1], 3),
            ", p = ", round(contrasts$contrasts.p.value.adjusted[1], 4),
            ", 95% CI = [", round(contrasts$lower.95CI.contrasts.estimate.adjusted[1], 3), ", ", round(contrasts$upper.95CI.contrasts.estimate.adjusted[1], 3), "]"))

print(paste0("SUPPORT - CONTROl AT 50% TARGET HANDGRIP STRENGTH:", 
             " estimate = ", round(contrasts$contrasts.estimate.reversed[2], 3),
             ", SE = ", round(contrasts$contrasts.SE.adjusted[2], 3),
             ", Z = ", round(contrasts$contrasts.z.ratio.adjusted[2], 3),
             ", p = ", round(contrasts$contrasts.p.value.adjusted[2], 4),
             ", 95% CI = [", round(contrasts$lower.95CI.contrasts.estimate.adjusted[2], 3), ", ", round(contrasts$upper.95CI.contrasts.estimate.adjusted[2], 3), "]"))

print(paste0("SUPPORT - CONTROl AT 60% TARGET HANDGRIP STRENGTH:", 
             " estimate = ", round(contrasts$contrasts.estimate.reversed[3], 3),
             ", SE = ", round(contrasts$contrasts.SE.adjusted[3], 3),
             ", Z = ", round(contrasts$contrasts.z.ratio.adjusted[3], 3),
             ", p = ", round(contrasts$contrasts.p.value.adjusted[3], 4),
             ", 95% CI = [", round(contrasts$lower.95CI.contrasts.estimate.adjusted[3], 3), ", ", round(contrasts$upper.95CI.contrasts.estimate.adjusted[3], 3), "]"))

################################################################################################################################################

### MULTILEVEL CENSORED REGRESSION MODEL ON POST EXERCISE TRIAL QUESTION ON PERCEIVED DIFFUCULTY ###

library(censReg)
library(plm)

#make the 'trial_difficulty' variable an ordered factor
question_data$trial_difficulty_o = ordered(question_data$trial_difficulty)

#create data frame that will later need to be transformed to a 'pdata.frame' object)
rData = data.frame(question_data$Participant)

#add the main predictor variables ('support_or_control' needs to be made a factor)
rData$placebo = question_data$placebo_condition
rData$support = question_data$support_or_control

#add other covariates
rData$sex = question_data$sex
rData$trial_number = question_data$trial_number
rData$session = question_data$session_number
rData$trial_difficulty_o = question_data$trial_difficulty_o

#create a time variable
rData$time = ifelse(rData$session == 1, rData$trial_number, ifelse(rData$session == 2, 42 + rData$trial_number, NA ))

#add the variable on trial difficulty ('Q2_ans_perc') and effort ('Q1_ans_perc')
rData$y.Q2 = question_data$Q2_ans_perc
rData$y.Q1 = question_data$Q1_ans_perc

#convert data frame to class 'pdata.frame' based on the id and time variables
rData = pdata.frame( rData, c( "question_data.Participant", "time" ) )

#convert variables to factors
rData$session = as.factor(rData$session)

### ### ###

#test the interaction between placebo and exercise block
cens_reg_session_placebo_interaction = censReg( y.Q2 ~ placebo*session + trial_number + y.Q1, left = 0, right = 100, data = rData, method = "BHHH" )
summary(cens_reg_session_placebo_interaction)

### ### ###

#keep the interaction between placebo and exercise block, thus creating a four-way interaction between trial difficulty, placebo, social support, and exercise block
cens_reg_final_model = censReg( y.Q2 ~ trial_difficulty_o*placebo*support*session + trial_number + y.Q1, left = 0, right = 100, data = rData, method = "BHHH" ) 
summary(cens_reg_final_model)

#create a 95% confidence interval for the placebo by exercise block interaction
int_est = as.data.frame(summary(cens_reg_final_model)["estimate"])[16,1]
int_se = as.data.frame(summary(cens_reg_final_model)["estimate"])[16,2]
print(paste("UPPER 95% CI:", int_est + 1.96*int_se))
print(paste("LOWER 95% CI:", int_est - 1.96*int_se))

#create a 95% confidence interval for the social support effect
int_est = as.data.frame(summary(cens_reg_final_model)["estimate"])[5,1]
int_se = as.data.frame(summary(cens_reg_final_model)["estimate"])[5,2]
print(paste("UPPER 95% CI:", int_est + 1.96*int_se))
print(paste("LOWER 95% CI:", int_est - 1.96*int_se))

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

#compare difficutly ratings between those who received no placebo before the first exercise block to the ratings of those who received the placebo before the first exercise block
t.test(no.placebo.block1$Q2_ans_perc, placebo.block1$Q2_ans_perc)

#compare difficutly ratings between those who received no placebo before the second exercise block to the ratings of those who received the placebo before the second exercise block
t.test(no.placebo.block2$Q2_ans_perc, placebo.block2$Q2_ans_perc)

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
                          summarise(mean_percent_maximum = mean(percent_of_maximum),
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

### PLOT THEMES ###

library(ggplot2)

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

#create a directory for the plot, set the current directory to this directory
cur_dir = code_dir
new_dir = paste0('../outputs/plots/')
dir.create(file.path(cur_dir, new_dir))
setwd(new_dir)

################################################################################################################################################

### MEAN HANDGRUP OUTPUTS (AS A PERCENTAGE OF MAXIMUM) OVER TIME BY TRIAL TARGET DIFFICULTY  ###

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
grip_by_difficulty = ggplot(na.omit(mean_data), aes(x = sample_reading_number, y = volt_mean, colour = trial_difficulty)) + 
                     geom_point() + geom_line() + 
                     scale_color_manual(values = diff_cols_r_y_g) +
                     ylab("Mean percentage of participant maximum handgrip strength") + xlab("Time (handgrip strength reading number)") + labs(color = "Target handgrip strength") +
                     geom_hline(yintercept=c(40, 50, 60), linetype="dotted", color = diff_cols_g_y_r) + 
                     scale_y_continuous(breaks=c(0,10,20,30,40,50,60), labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%")) +
                     scale_x_continuous(breaks = seq(0,160,10)) +
                     original_theme

#save the plot
ggsave("mean_grip_strength_by_trial_difficulty_target.jpeg", grip_by_difficulty, width = 10, height = 7.5)

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
  med_diff
  
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

#get the average distance for each participant
average_dat_outputs = difference_dat_hang_grip %>% group_by(participant, trial_difficulty) %>% summarise(mean_diff = mean(output_difference))

### ### ###

#plot the data
avp_outputs = ggplot(average_dat_outputs, aes(x=trial_difficulty, y=mean_diff)) + 
              geom_violin(aes(fill = trial_difficulty)) + 
              geom_boxplot(width = 0.1) +
              geom_hline(yintercept = 0, linetype = "dashed") + 
              scale_y_continuous(limits = c(-7,10), breaks = c(-5, 0, 5, 10), labels = c("-5%", "0%\n(no effect)", "+5%", "+10%")) +
              xlab("Trial target difficulty\n(percentage of participant's maximum handgrip strength)") +
              ylab("Mean effect of support figure face (versus stranger face) on handgrip outputs") + 
              coord_flip() + 
              scale_fill_manual(values = diff_cols_g_y_r) +
              theme(legend.position="none") +
              original_theme 

#save the plot
ggsave("average_social_support_effect_by_trial_difficulty_target.jpg", avp_outputs, width = 10, height = 7.5)

################################################################################################################################################

### PERCEIVED DIFFICULTY BY PLACEBO AND EXERCISE BLOCK ###

#get the difficulty rating for each participant
average_dat_difficulty = question_data %>% group_by(Participant, session_number, placebo_condition) %>% summarise(mean_difficulty = mean(Q2_ans_perc))

#create a variable that 
dodge = position_dodge(width = 1)
vp_difficulty = ggplot(average_dat_difficulty, aes(x = as.factor(session_number), y = mean_difficulty)) +
                geom_violin(aes(fill = placebo_condition), position = dodge) +
                geom_boxplot(aes(group = interaction(placebo_condition, as.factor(session_number))), width=0.3, fill="white", position=dodge,outlier.shape=NA) + 
                scale_y_continuous(limits = c(20,100), breaks = c(seq(20, 100, 10)), labels = c("20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
                scale_x_discrete(labels = c("First", "Second")) +
                xlab("Exercise block") +
                ylab("Mean perceived difficulty\n(percent of sliding scale)") + 
                scale_fill_manual(name = "Placebo\ncondition", values = placebo_cols, labels = c("Control", "Placebo")) +
                original_theme

#save the plot
ggsave("perceived_difficulty_by_exercise_block_and_placebo_condition.jpg", vp_difficulty, width = 10, height = 7.5, units = "in")

################################################################################################################################################

### ASSUMPTION CHECKS ###

library(nlme)

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
dir.create('./multilevel_model_on_hand_grip_outputs/')
setwd('./multilevel_model_on_hand_grip_outputs/')

################################################################################################################################################

### MULTILEVEL MODEL ON PARTICIPANTS' HANDGRIP OUTPUTS - LEVEL-ONE HOMOSCEDASTICITY ###

#regressions at each level-two unit ('Participant')
olselist = lmList(percent_of_maximum ~ 1 + trial_difficulty_o*placebo_condition*support_or_control + trial_number + session_number + Q1_ans_perc | Participant, data = total_data)

#run a multilevel model on all the data with 'Participant' as the level-two unit (this is the same as the 'final_model', except is estimated with the 'nlme' instead of 'lme4' package)
final_model_nlme = nlme::lme(percent_of_maximum ~ 1 + trial_difficulty_o*placebo_condition*support_or_control + trial_number + session_number + Q1_ans_perc, random = ~ placebo_condition | Participant,
                             control = lmeControl(opt = 'optim'), data = total_data)

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
frame1_int_lme$Model = "Multilevel model"
frame1_int_lme$level_two_unit = rownames(frame1_int_lme)

#predictor coefficients from olselist (subtract mean of other variable predictions for plotting)
frame2_pred_olselist = as.data.frame(comp.models[, 1, "placebo_conditionon"])
colnames(frame2_pred_olselist)[1] = "Estimate"
frame2_pred_olselist$coefficient = "Predictor"
frame2_pred_olselist$Model = "OLS regression"
frame2_pred_olselist$level_two_unit = rownames(frame2_pred_olselist)

#predictor coefficients from lme; drop the first column (which is olselist coefficients)
frame2_pred_lme = as.data.frame(comp.models[, , "placebo_conditionon"])
colnames(frame2_pred_lme)[2] = "Estimate" 
frame2_pred_lme$coefficient = "Predictor"
frame2_pred_lme$Model = "Multilevel model"
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
mlm_intercept = subset(new_frame, new_frame$coefficient == "Intercept" & new_frame$Model == "Multilevel model")
mlm_predictor = subset(new_frame, new_frame$coefficient == "Predictor" & new_frame$Model == "Multilevel model")
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
H = round(sum(d_list^2), digits = 3)
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
level_one_variable1 = "trial_difficulty"
level_one_variable2 = "Q1_ans_perc"

#get within-group OLS residuals
resi = residuals(olselist) 

#make the plots
jpeg('level_one_linearity_target_trial_difficulty.jpg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
plot(total_data[, level_one_variable1], resi, xlab = expression("Target trial difficulty"), ylab = "Residual")
lines(lowess(total_data[, level_one_variable1], resi))
dev.off()

jpeg('level_one_linearity_perceived_difficulty.jpg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
plot(total_data[, level_one_variable2], resi, xlab = expression("Perceived difficulty"), ylab = "Residual")
lines(lowess(total_data[, level_one_variable2], resi), col = "dodgerblue1")
dev.off()

################################################################################################################################################

### MAIN MULTILEVEL MODEL ON PARTICIPANTS' HANDGRIP OUTPUTS - LEVEL-ONE RESIDUAL NORMALITY ###

#compute within-participant studentized OLS residuals
resi_st = by(total_data, total_data[,"Participant"], function(total_data) rstudent(lm(percent_of_maximum ~ 1 + trial_difficulty_o*placebo_condition*support_or_control + trial_number + session_number + Q1_ans_perc, data = total_data)))

#unlist
rs = unlist(resi_st)

#make a QQ plot
jpeg('ols_residual_normality.jpg', width = 5, height = 5, units = "in",res = 300, pointsize = 12, quality = 100)
qqnorm(rs, main = "")
qqline(rs)
dev.off()

################################################################################################################################################

### MAIN MULTILEVEL MODEL ON PARTICIPANTS' HANDGRIP OUTPUTS - LEVEL-TWO RESIDUAL NORMALITY ###

#get the random effects for the model
mod_re = lme4::ranef(final_model, condVar=TRUE)

#get the posterior means for random intercepts for each level-two unit
postmean_int =  as.data.frame(mod_re["Participant"])[, 1]

#this will get the posterior variances for the random intercepts
postmeanvar_int = attr(mod_re[["Participant"]],'postVar')[1,1,]

#calculate diagnostic variances and posterior means for the random intercept
diagmeanvar_int = VarCorr(final_model)[["Participant"]][1,1] - postmeanvar_int
postmean_stand_int = postmean_int/sqrt(diagmeanvar_int)

#get the posterior means for random slopes for each level-two unit
postmean_slope = as.data.frame(mod_re["Participant"])[, 2] 
  
#this will get the posterior variances for the random slopes
postmeanvar_slope = attr(mod_re[["Participant"]],'postVar')[2,2,] 
  
#calculate diagnostic variances and posterior means for the random slopes
diagmeanvar_slope = VarCorr(final_model)[["Participant"]][2,2] - postmeanvar_slope 
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
check_influence(final_model, "Participant")

#estimate the model removing one participant at a time so that their effects on the overall results can be assessed
alt.est = influence(final_model, group = "Participant")

#the sigtest function tests whether excluding a particular level-two unit changes the statistical significance (at 1.96) of a model’s predictor variables (Nieuwenhuis et al. 2012)
exclusions = sigtest(alt.est)
sig.test.dataframe = as.data.frame(exclusions)

#use only columns for predictor variables (first three are about the intercept) that are about changes in significance
subset = sig.test.dataframe[, c(4:ncol(sig.test.dataframe))]

#get whether the predictor variable(s) changed significance
changed_sig = subset[, grepl(paste("trial_difficulty_o.L.support_or_controlsupport",".Changed.Sig", sep = ""), names(subset))]

trues = table(changed_sig)["TRUE"]
trues = if (is.na(trues) == TRUE) 0 else trues

falses = table(changed_sig)["FALSE"]
falses = if (is.na(falses) == TRUE) 0 else falses

percent_true = round(as.integer((trues / (trues + falses)) * 100), digits = 2)
cat(paste("Percentage of level-two units that, if removed from the model, would change the significance of the target trial difficulty by social support condition interaction:", percent_true), file = 'assumption_tests.txt', append=TRUE)

################################################################################################################################################

### MULTILEVEL CENSORED REGRESSION MODEL ON POST EXERCISE TRIAL QUESTION ON PERCEIVED DIFFUCULTY ###

library(AER)
library(lme4)
library(lmerTest)

#create a directory for the assumption check results for this model, set the current directory to this directory
dir.create('../multilevel_censored_regression_model_on_perceived_difficulty/')
setwd('../multilevel_censored_regression_model_on_perceived_difficulty/')

### ### ###

#the 'censReg' function does not produce residuals, so estimate a censored regression (with 'Participant' as a factor) and a multilevel model on participants' perceived difficulty scores
tobit_Q2 = tobit(Q2_ans_perc ~ support_or_control*placebo_condition*trial_difficulty_o + session_number + session_number + trial_number + Participant + Q1_ans_perc, left = 0, right = 100, data = question_data)
summary(tobit_Q2)

Q2_results_full_model = lmer(Q2_ans_perc ~ 1 + placebo_condition*support_or_control*trial_difficulty_o*session_number + trial_number + Q1_ans_perc + (1 | Participant), data = question_data)
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
cat(paste("Variance inflation factor (VIF) mean:", mean(vif(tobit_Q2)[c(1:6), 1])), file = 'assumption_tests.txt', append=TRUE)

#get the variance inflation factor (VIF) range
cat(paste0("\nVIF range: ", range(vif(tobit_Q2)[c(1:6), 1])[1], ", ", range(vif(tobit_Q2)[c(1:6), 1])[2]), file = 'assumption_tests.txt', append=TRUE)

#get the VIF for each predictor
cat(paste("\nVIF for social support condition predictor:", vif(tobit_Q2)[c(1:6), 1][1]), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for placebo condition predictor:", vif(tobit_Q2)[c(1:6), 1][2]), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for target trial difficulty condition predictor:", vif(tobit_Q2)[c(1:6), 1][3]), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for exercise block predictor:", vif(tobit_Q2)[c(1:6), 1][4]), file = 'assumption_tests.txt', append=TRUE)
cat(paste("\nVIF for exercise trial number predictor:", vif(tobit_Q2)[c(1:6), 1][5]), file = 'assumption_tests.txt', append=TRUE)

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
sw_w = as.numeric(shapiro.test(postmean_stand)[1])
sw_p = as.numeric(shapiro.test(postmean_stand)[2])
cat("\n", "LEVEL-TWO RESIDUAL NORMALITY", file = 'assumption_tests.txt', append=TRUE, sep = "\n")
cat(paste0("Shapiro-Wilks test of level-two residual normality: W = ", sw_w, ", p = ", sw_p), file = 'assumption_tests.txt', append=TRUE)

################################################################################################################################################

### MULTILEVEL CENSORED REGRESSION MODEL ON POST EXERCISE TRIAL QUESTION ON PERCEIVED DIFFUCULTY - LEVEL-TWO OUTLIERS ###

#a function for checking the influence of level-two units (participants) using Cook's distances
check_influence(Q2_results_full_model, "Participant")

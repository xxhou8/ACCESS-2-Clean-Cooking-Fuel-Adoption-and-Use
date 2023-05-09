rm(list = ls())

#packages for models
#install.packages("pscl")
#install.packages("mhurdle")
#install.packages("oglmx")
#install.packages("lmtest")

library(readstata13)
library("dplyr")
library("pscl")
library("mhurdle")
library("ordinal")
library(foreign)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(tidyr)
library(plyr)

setwd("~/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/data")
access<-read.dta13("access2018_2018Dec31.dta")
remove<-read.csv("remove_hhs_access.csv")
forest<-read.csv(file="shrug_vcf_wide.csv")

###### Remove outliers from analysis ######
# This part is to remove the outliers due to unreasonable answers

remove$remove<-1

access<-merge(access, remove, by=c("finalhhid","year"), all.x=T)
access$remove[is.na(access$remove)]<-0

access<-subset(access, access$remove=="0")


###### Create indicators and test #####
###Region ID
# access$id = paste(access$m1_q8_state_code, access$m1_q9_district_code, 
#                   access$m1_q11_village_code, sep="-")

###Adoption of LPG
access["LPG"] <- ifelse(access$m4_q103_lpg == "Yes", 1,0)



access$fuel_stack<-NA #fuel stacking
access$fuel_stack[access$LPG==0]<-1 #no adoption
access$fuel_stack[(access$LPG==1) &
                    (access$m4_q109_firewood=="No"& 
                       ((access$m4_q113_dungcake=="No") & 
                          ((access$m4_q114_agro=="No") & 
                             (access$m4_q115_other_fuel=="No"))))]<-4 #exclusive use

access$fuel_stack[is.na(access$fuel_stack)]<-ifelse(access$m5_q118_main_cookfuel[is.na(access$fuel_stack)]=="LPG", 3, 2)
# label family who use LPG as primary fuel but also stack fuels 2
# label family who do not use LPG as primary fuel but as one of the stack fuels 3

access$LPG_exclusive_use<-ifelse(access$fuel_stack==4,1,0)
access$Fuel_stacking_with_LPG_as_Primary<-ifelse(access$fuel_stack==3,1,0)
access$Fuel_stacking_with_Other_Fuels_as_Primary<-ifelse(access$fuel_stack==2,1,0)
access$No_Adoption_of_LPG<-ifelse(access$fuel_stack==1,1,0)

# test for # of observations of ele_stove/kero-stove
unique(access$m4_q115_other_fuel_s)
access["ele_stove"]<-ifelse(access$m4_q115_other_fuel_s=="INDUCTION STOVE",1,0)
sum(access$ele_stove)

# access["kero_stove"]<-ifelse(access$m4_q115_other_fuel_s=="KEROSENE STOVE"|
#                                access$m4_q115_other_fuel_s=="KEROSENE"|
#                                access$m4_q115_other_fuel_s=="KEROSENE OIL"|
#                                access$m4_q115_other_fuel_s=="KEROSENE OIL FOR STOVE"|
#                                access$m4_q115_other_fuel_s=="HEATER AND KEROSENE STOVE",1,0)
# sum(access$kero_stove)

###Calculate monthly LPG consumption
access$m4_q103_4_lpg_lcyl[is.na(access$m4_q103_4_lpg_lcyl)]<-0
access$m4_q103_7_lpg_scyl[is.na(access$m4_q103_7_lpg_scyl)]<-0
access$lpg_consumption<-(access$m4_q103_4_lpg_lcyl*14200+access$m4_q103_7_lpg_scyl*5000)/12
access$lpg_log<-log(access$lpg_consumption+1)
#LARGE LPG CYLINDER IS 14.2 KG; SMALL LPG CYLINDER IS 5 KG

#take out NAs in expenditure
#access<-subset(access, !is.na(access$m1_q32_month_expenditure))


###Log transform of monthly expenditure
access$month_expenditure_log<-log(access$m1_q32_month_expenditure + 1)

###Create caste indicators
access["Caste_SC"] <- ifelse(access$m1_q25_caste == 1, 1, 0)
access["Caste_ST"] <- ifelse(access$m1_q25_caste == 2, 1, 0)
access["Caste_OBC"]<-ifelse(access$m1_q25_caste == 3, 1, 0)
access["Caste_General"] <- ifelse(access$m1_q25_caste == 4|access$m1_q25_caste == 5, 1, 0)

access$Caste[access$m1_q25_caste == 1]<- 4
access$Caste[access$m1_q25_caste == 2]<- 3
access$Caste[access$m1_q25_caste == 3]<- 2
access$Caste[access$m1_q25_caste == 4|access$m1_q25_caste == 5]<- 1

access$Caste<-as.factor(access$Caste)

###Create education indicators
access["Edu_NoFormalSchooling"] <- ifelse(access$m1_q23_edu == "No Formal Schooling", 1, 0)
access["Edu_UpTo5thStandard"] <- ifelse(access$m1_q23_edu == "Up to 5th Standard", 1, 0)
access["Edu_MoreThan5thStandard"] <- ifelse(access$m1_q23_edu == "Up to 10th Standard" | 
                                               access$m1_q23_edu == "12th Standard or Diploma" |
                                               access$m1_q23_edu == "Graduate and Above", 1, 0)

access$Education[access$m1_q23_edu == "No Formal Schooling"] <- 1
access$Education[access$m1_q23_edu == "Up to 5th Standard"] <- 2

access$Education[access$m1_q23_edu == "Up to 10th Standard" | 
                   access$m1_q23_edu == "12th Standard or Diploma" |
                   access$m1_q23_edu == "Graduate and Above"] <- 3

access$Education<-access$Education
  
###Create religion indicators
access["Religion_Hindu"] <- ifelse(access$m1_q24_religion == 1, 1, 0)
access["Religion_Other"] <- ifelse(access$m1_q24_religion == 2 | access$m1_q24_religion == 3, 1, 0)

access["Religion"] <- ifelse(access$m1_q24_religion == 1, 2, 1)

###Create household size
access$Numpers_house <- access$m1_q27_no_adults + access$m1_q29_no_children

###Create Gender Effect
access["Decision_MaleHouseholdHead"] <- ifelse(access$m1_q38_decision_maker=="Male Household Head", 1, 0)
access["Decision_FemaleHouseholdHead"] <- ifelse(access$m1_q38_decision_maker=="Female Household Head", 1, 0)
access["Decision_Both"] <- ifelse(access$Decision_MaleHouseholdHead==0 & access$Decision_FemaleHouseholdHead==0, 1, 0)

# access$Decision_Maker <- ifelse(access$m1_q38_decision_maker=="Male Household Head",1,
#                                 ifelse(access$m1_q38_decision_maker=="Female Household Head",2,3))

access$Decision_Maker <- ifelse(access$m1_q38_decision_maker=="Male Household Head" |
                                  access$m1_q38_decision_maker_other=="Brother" |
                                  access$m1_q38_decision_maker_other=="BROTHER" |
                                  access$m1_q38_decision_maker_other=="Brother of HH Head" |
                                  access$m1_q38_decision_maker_other=="Elder brother" |
                                  access$m1_q38_decision_maker_other=="Father of HH head" |
                                  access$m1_q38_decision_maker_other=="Grand father" |
                                  access$m1_q38_decision_maker_other=="GRAND FATHER" |
                                  access$m1_q38_decision_maker_other=="Grand son" |
                                  access$m1_q38_decision_maker_other=="GRAND SON" |
                                  access$m1_q38_decision_maker_other=="Nephew" |
                                  access$m1_q38_decision_maker_other=="Son" |
                                  access$m1_q38_decision_maker_other=="SON" |
                                  access$m1_q38_decision_maker_other=="Son-in-law" |
                                  access$m1_q38_decision_maker_other=="Sons" |
                                  access$m1_q38_decision_maker_other=="Uncle", 1, 
                                ifelse(access$m1_q38_decision_maker=="Female Household Head" |
                                         access$m1_q38_decision_maker_other=="AUNTI" |
                                         access$m1_q38_decision_maker_other=="Daughter" |
                                         access$m1_q38_decision_maker_other=="DAUGHTER" |
                                         access$m1_q38_decision_maker_other=="DAUGHTER IN LAW" |
                                         access$m1_q38_decision_maker_other=="Daughter-in-law" |
                                         access$m1_q38_decision_maker_other=="Mother" |
                                         access$m1_q38_decision_maker_other=="MOTHER" |
                                         access$m1_q38_decision_maker_other=="Mother of HH head" |
                                         access$m1_q38_decision_maker_other=="NIECE" |
                                         access$m1_q38_decision_maker_other=="Sister-in-law", 2, 3))

###State FE
access["State_UttarPradesh"]<-ifelse(access$m1_q8_state=="UTTAR PRADESH", 1, 0)
access["State_Bihar"]<-ifelse(access$m1_q8_state=="BIHAR", 1, 0)
access["State_WestBengal"]<-ifelse(access$m1_q8_state=="WEST BENGAL", 1, 0)
access["State_Jharkhand"]<-ifelse(access$m1_q8_state=="JHARKHAND", 1, 0)
access["State_Odisha"]<-ifelse(access$m1_q8_state=="ODISHA", 1, 0)
access["State_MadhyaPradesh"]<-ifelse(access$m1_q8_state=="MADHYA PRADESH", 1, 0)

###Time FE
access["Round2"]<-ifelse(access$year=="2018", 1, 0)
access["Round1"]<-ifelse(access$year=="2015", 1, 0)
access$round<-ifelse(access$year=="2018",1,0)

### PMUY scheme
access$pmuy<-ifelse(access$m4_3n1_ujjwala_beneficiary=="Yes",1,0)
access$pmuy<-ifelse(is.na(access$pmuy)&access$round==0,0,access$pmuy)



### forest coverage

forest<-separate(forest,shrid, c("A","B","C"),"-")
forest$B<-as.numeric(forest$B)
forest$C<-as.numeric(forest$C)
forest$avgforest<-forest$total_forest2014/forest$num_cells
forest14<-forest[,c("B","C","avgforest")]
forest14<-rename(forest14, c("B"="m1_q8_state_code", "C"="m1_q11_village_code"))

access_fr<-join(access, forest14, type="left")
# dt<-merge(data_nona, forest14, by.x=c("m1_q8_state_code", "m1_q11_village_code"), 
#       by.y=c("B", "C"), type="left")

access_fr$m4_q103_1_lpg_year<-ifelse(is.na(access_fr$m4_q103_1_lpg_year)&access_fr$LPG==0,0,access_fr$m4_q103_1_lpg_year)

access_nona<-subset(access_fr, !is.na(access$m1_q32_month_expenditure))
# 64 NAs in 2018


###### Sebset of targeted data ######
# data<- dplyr:: select(access,m1_q3_hhid,year,m1_q8_state_code,m1_q11_village_code, LPG, lpg_log,
#               lpg_consumption, m4_q109_firewood,m4_q113_dungcake,m4_q114_agro,m4_q115_other_fuel,
#               fuel_stack, m1_q32_month_expenditure,month_expenditure_log, Numpers_house, 
#               Caste,Caste_SC,Caste_ST,Caste_OBC,Caste_General,
#               Education,Edu_NoFormalSchooling,Edu_UpTo5thStandard,Edu_MoreThan5thStandard,
#               Religion, Religion_Hindu,Religion_Other,m1_q19_age, 
#               Decision_Maker, Decision_MaleHouseholdHead, Decision_FemaleHouseholdHead, Decision_Both, 
#               LPG_exclusive_use, Fuel_stacking_with_LPG_as_Primary,
#               Fuel_stacking_with_Other_Fuels_as_Primary,No_Adoption_of_LPG,
#               State_UttarPradesh,Round2,Round1,round,m1_q8_state, weight,
#               State_Bihar,State_WestBengal,State_Jharkhand,State_Odisha,State_MadhyaPradesh, pmuy)

data_nona<-dplyr:: select(access_nona,m1_q3_hhid,year,m1_q8_state_code,m1_q11_village_code, LPG, lpg_log,
                   lpg_consumption, m4_q109_firewood,m4_q113_dungcake,m4_q114_agro,m4_q115_other_fuel,
                   fuel_stack, m1_q32_month_expenditure,month_expenditure_log, Numpers_house, 
                   Caste,Caste_SC,Caste_ST,Caste_OBC,Caste_General,
                   Education,Edu_NoFormalSchooling,Edu_UpTo5thStandard,Edu_MoreThan5thStandard,
                   Religion, Religion_Hindu,Religion_Other,m1_q19_age, 
                   Decision_Maker, Decision_MaleHouseholdHead, Decision_FemaleHouseholdHead, Decision_Both, 
                   LPG_exclusive_use, Fuel_stacking_with_LPG_as_Primary,
                   Fuel_stacking_with_Other_Fuels_as_Primary,No_Adoption_of_LPG,
                   State_UttarPradesh,Round2,Round1,round,m1_q8_state, weight,
                   State_Bihar,State_WestBengal,State_Jharkhand,State_Odisha,State_MadhyaPradesh, 
                   pmuy, avgforest, m4_q103_1_lpg_year)

write.dta(data_nona, "data.dta") ### Save dataset to be used in STATA analysis


######################## Descrptive Statistics ######################

data1<-subset(data,data$round==0)
write.dta(data1, "data1.dta")
data2<-subset(data,data$round==1)
write.dta(data2, "data2.dta")
# 2015 dependent variables
sum_stats_2015 <- cbind(data1$LPG, 
                        data1$LPG_exclusive_use,
                        data1$Fuel_stacking_with_LPG_as_Primary, 
                        data1$Fuel_stacking_with_Other_Fuels_as_Primary, 
                        data1$No_Adoption_of_LPG
                        )


sum_stats_output_2015 <- data.frame(matrix(data = NA,
                                           nrow = 5,
                                           ncol = 5))
sum_stats_output_2015[,5] = rep(nrow(sum_stats_2015), 5)
for(i in 1:5) {
  #mean; sd; min and max
  sum_stats_output_2015[i,1] = signif(mean(sum_stats_2015[,i]), 5)
  sum_stats_output_2015[i,2] = signif(sd(sum_stats_2015[,i]), 5)
  sum_stats_output_2015[i,3] = signif(min(sum_stats_2015[,i]), 5)
  sum_stats_output_2015[i,4] = signif(max(sum_stats_2015[,i]), 5)
}
sum_stats_output_2015[,5] = format(sum_stats_output_2015[,5], big.mark=",")
colnames(sum_stats_output_2015) = c("Mean", "SD", "Min", "Max", "Observations")
rownames(sum_stats_output_2015) = c("LPG (1 = yes, 0 = no)", 
                                    "LPG Exclusive Use", 
                                    "Fuel Stacking with LPG as Primary",
                                    "Fuel Stacking with Other Fuels as Primary","No Adoption of LPG"
                                    )
sum_stats_output_2015
stargazer(sum_stats_output_2015, summary=FALSE, float=FALSE,
          out = "C:/Users/59634/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/Manuscript/Tables/sum_dependent_2015.tex")


# 2018 dependent variables
sum_stats_2018 <- cbind(data2$LPG, 
                        data2$LPG_exclusive_use,
                        data2$Fuel_stacking_with_LPG_as_Primary, 
                        data2$Fuel_stacking_with_Other_Fuels_as_Primary, 
                        data2$No_Adoption_of_LPG
)


sum_stats_output_2018 <- data.frame(matrix(data = NA,
                                           nrow = 5,
                                           ncol = 5))
sum_stats_output_2018[,5] = rep(nrow(sum_stats_2018), 5)
for(i in 1:5) {
  #mean; sd; min and max
  sum_stats_output_2018[i,1] = signif(mean(sum_stats_2018[,i]), 5)
  sum_stats_output_2018[i,2] = signif(sd(sum_stats_2018[,i]), 5)
  sum_stats_output_2018[i,3] = signif(min(sum_stats_2018[,i]), 5)
  sum_stats_output_2018[i,4] = signif(max(sum_stats_2018[,i]), 5)
}
sum_stats_output_2018[,5] = format(sum_stats_output_2018[,5], big.mark=",")
colnames(sum_stats_output_2018) = c("Mean", "SD", "Min", "Max", "Observations")
rownames(sum_stats_output_2018) = c("LPG (1 = yes, 0 = no)", 
                                    "LPG Exclusive Use", 
                                    "Fuel Stacking with LPG as Primary",
                                    "Fuel Stacking with Other Fuels as Primary","No Adoption of LPG"
)
sum_stats_output_2018
stargazer(sum_stats_output_2018, summary=FALSE, float=FALSE,
          out = "C:/Users/59634/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/Manuscript/Tables/sum_dependent_2018.tex")




# 2015 control variables
sum_stats_indep_2015 <- data1[,c("month_expenditure_log",
                                 "m1_q32_month_expenditure",
                                 "Numpers_house",
                                 "Caste_SC",
                                 "Caste_ST",
                                 "Caste_OBC",
                                 "Caste_General",
                                 "Edu_NoFormalSchooling",
                                 "Edu_UpTo5thStandard",
                                 "Edu_MoreThan5thStandard",
                                 "Religion_Hindu",
                                 "Religion_Other",
                                 "m1_q19_age",
                                 "Decision_MaleHouseholdHead",
                                 "Decision_FemaleHouseholdHead",
                                 "Decision_Both"
                                 )]


sum_stats_indep_output_2015 <- data.frame(matrix(data = NA,
                                                    nrow = 16,
                                                    ncol = 5))
for(i in 1:16) {
  #n
  sum_stats_indep_output_2015[,5] = rep(nrow(sum_stats_indep_2015), 16)
  #mean; sd; min and max
  sum_stats_indep_output_2015[i,1] = signif(mean(as.numeric(sum_stats_indep_2015[,i])), 3)
  sum_stats_indep_output_2015[i,2] = signif(sd(as.numeric(sum_stats_indep_2015[,i])),3)
  sum_stats_indep_output_2015[i,3] = signif(min(as.numeric(sum_stats_indep_2015[,i])), 0)
  sum_stats_indep_output_2015[i,4] = signif(max(as.numeric(sum_stats_indep_2015[,i])), 0)
}


sum_stats_indep_output_2015[,5] = format(sum_stats_indep_output_2015[,5], big.mark=",")
colnames(sum_stats_indep_output_2015) = c("Mean", "SD", "Min", "Max", "Observations")
rownames(sum_stats_indep_output_2015) = c("log (Monthly Expenditure)",
                                          "Monthly Expenditure",
                                          "Household Size",
                                          "Caste: SC",
                                          "Caste: ST",
                                          "Caste: OBC",
                                          "Caste: General",
                                          "Edu: NoFormalSchooling",
                                          "Edu:UpTo5thStandard",
                                          "Edu:MoreThan5thStandard",
                                          "Religion:Hindu",
                                          "Religion:Other",
                                          "Decision Maker Age",
                                          "Man Decision Maker",
                                          "Women Decision Maker",
                                          "Both Gender"
)
sum_stats_indep_output_2015
stargazer(sum_stats_indep_output_2015, summary=FALSE, float=FALSE,
          out = "C:/Users/59634/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/Manuscript/Tables/sum_stats_indep_2015.tex")



# 2018 control variables
sum_stats_indep_2018 <- data2[,c("month_expenditure_log",
                                 "m1_q32_month_expenditure",
                                 "Numpers_house",
                                 "Caste_SC",
                                 "Caste_ST",
                                 "Caste_OBC",
                                 "Caste_General",
                                 "Edu_NoFormalSchooling",
                                 "Edu_UpTo5thStandard",
                                 "Edu_MoreThan5thStandard",
                                 "Religion_Hindu",
                                 "Religion_Other",
                                 "m1_q19_age",
                                 "Decision_MaleHouseholdHead",
                                 "Decision_FemaleHouseholdHead",
                                 "Decision_Both"
)]
sum_stats_indep_output_2018 <- data.frame(matrix(data = NA,
                                                 nrow = 16,
                                                 ncol = 6))
for(i in 1:16) {
  #n
  sum_stats_indep_output_2018[,6] = rep(nrow(sum_stats_indep_2018), 16)
  #mean; sd; min and max
  sum_stats_indep_output_2018[i,1] = signif(mean(sum_stats_indep_2018[,i]), 4)
  sum_stats_indep_output_2018[i,2] = signif(sd(sum_stats_indep_2018[,i]),4)
  sum_stats_indep_output_2018[i,3] = signif(min(sum_stats_indep_2018[,i]), 0)
  sum_stats_indep_output_2018[i,4] = signif(max(sum_stats_indep_2018[,i]), 0)
  sum_stats_indep_output_2018[i,5] = sum(is.na(sum_stats_indep_2018[,i]))
}

sum_stats_indep_output_2018[,6] = format(sum_stats_indep_output_2018[,6], big.mark=",")

for(i in 1:2) {
  #mean; sd; min and max
  sum_stats_indep_output_2018[i,1] = signif(mean(sum_stats_indep_2018[,i],na.rm=T),4)
  sum_stats_indep_output_2018[i,2] = signif(sd(sum_stats_indep_2018[,i], na.rm=T),4)
  sum_stats_indep_output_2018[i,3] = signif(min(sum_stats_indep_2018[,i], na.rm=T), 0)
  sum_stats_indep_output_2018[i,4] = signif(max(sum_stats_indep_2018[,i], na.rm=T), 0)
}

sum_stats_indep_output_2018[1,1]=mean(data2$month_expenditure_log, na.rm=T)
sum_stats_indep_output_2018[2,1]=mean(data2$m1_q32_month_expenditure, na.rm=T)
sum_stats_indep_output_2018[1,2]=sd(data2$month_expenditure_log, na.rm=T)
sum_stats_indep_output_2018[2,2]=mean(data2$month_expenditure_log, na.rm=T)
sum_stats_indep_output_2018[1,1]=mean(data2$month_expenditure_log, na.rm=T)
sum_stats_indep_output_2018[1,1]=mean(data2$month_expenditure_log, na.rm=T)


colnames(sum_stats_indep_output_2018) = c("Mean", "SD", "Min", "Max","NA", "Observations")
rownames(sum_stats_indep_output_2018) = c("log (Monthly Expenditure)",
                                          "Monthly Expenditure",
                                          "Household Size",
                                          "Caste: SC",
                                          "Caste: ST",
                                          "Caste: OBC",
                                          "Caste: General",
                                          "Edu: NoFormalSchooling",
                                          "Edu:UpTo5thStandard",
                                          "Edu:MoreThan5thStandard",
                                          "Religion:Hindu",
                                          "Religion:Other",
                                          "Decision Maker Age",
                                          "Man Decision Maker",
                                          "Women Decision Maker",
                                          "Both Gender"
)
sum_stats_indep_output_2018
stargazer(sum_stats_indep_output_2018, summary=FALSE, float=FALSE,
          out = "C:/Users/59634/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/Manuscript/Tables/sum_stats_indep_2018.tex")

################### plot figures #######################
pdf("C:/Users/59634/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/Manuscript/Figures/Descriptive_Statistics/distribution_lpg_log.pdf")
ggplot(data, aes(lpg_consumption+1))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+ 
  geom_histogram(aes(y = ..density..), bins=12, alpha = 0.4)+
  geom_line(aes(y = ..density..), stat = 'density')+
  theme(legend.position = "none")+
  labs(x="LPG consumption in grams", y="Density")+
  theme_minimal() 

dev.off()

##### margins plot

# This part use outputs from Stata analysis

consum_caste<-read.csv("consum_caste.csv")
select_caste<-read.csv("select_caste.csv")
consum_edu<-read.csv("consum_edu.csv")
select_edu<-read.csv("select_edu.csv")
consum_gender<-read.csv("consum_gender.csv")
select_gender<-read.csv("select_gender.csv")
consum_house<-read.csv("consum_house.csv")
select_house<-read.csv("select_house.csv")
consum_exp<-read.csv("consum_exp.csv")
select_exp<-read.csv("select_exp.csv")
consum_age<-read.csv("consum_age.csv")
select_age<-read.csv("select_age.csv")

pd <- position_dodge(0.1) # move them .05 to the left and right
### hurdle_caste

g1_1<-ggplot(consum_caste, aes(x=Caste, y=AAP, colour=Caste, group=Caste)) + 
  geom_point(position=pd, size=3, shape=21, fill="white") +
  geom_errorbar(data=consum_caste, aes(ymin=lower, ymax=upper, 
                                     color=Caste), width=.1, position=pd)+
  scale_y_continuous(limits=c(5000,10500))+
  ylab("")+
  ggtitle("") +
  scale_color_discrete(guide=F)+
  theme_bw()+ 
  theme(plot.margin=unit(c(-0.5,0.5,0,-0.5), "cm"))

g1_2<-ggplot(select_caste, aes(x=Caste, y=AME, colour=Caste, group=Caste)) + 
  geom_point(position=pd, size=3, shape=21, fill="white") +
  geom_errorbar(data=select_caste, aes(ymin=lower, ymax=upper, 
                                     color=Caste), width=.1, position=pd)+
  scale_y_continuous(limits=c(0,1))+
  ylab("")+
  ggtitle("") +
  scale_color_discrete(guide=F)+
  theme_bw()+ 
  theme(plot.margin=unit(c(-0.5,0.5,0,-0.5), "cm"))

### hurdle_education

consum_edu$Education <- factor(consum_edu$Education, levels=c("No Formal", "Up to 5th ", "More than 5th"))
select_edu$Education <- factor(select_edu$Education, levels=c("No Formal", "Up to 5th", "More than 5th"))

g2_1<-ggplot(consum_edu, aes(x=Education, y=AAP, colour=Education, group=Education)) + 
  geom_point(position=pd, size=3, shape=21, fill="white") +
  geom_errorbar(data=consum_edu, aes(ymin=lower, ymax=upper, 
                                     color=Education), width=.1, position=pd)+
  scale_y_continuous(limits=c(5000,10500))+
  ylab("")+
  ggtitle("") +
  scale_color_discrete(guide=F)+
  theme_bw()+ 
  theme(plot.margin=unit(c(-0.5,0.5,0,-0.5), "cm"))
  #+theme(axis.text.x=element_text(angle = 30,hjust=1))

g2_2<-ggplot(select_edu, aes(x=Education, y=AME, colour=Education, group=Education)) + 
  geom_point(position=pd, size=3, shape=21, fill="white") +
  geom_errorbar(data=select_edu, aes(ymin=lower, ymax=upper, 
                                     color=Education), width=.1, position=pd)+
  scale_y_continuous(limits=c(0,1))+
  ylab("")+
  ggtitle("") +
  scale_color_discrete(guide=F)+
  theme_bw()+ 
  theme(plot.margin=unit(c(-0.5,0.5,0,-0.5), "cm"))
#+theme(axis.text.x=element_text(angle = 30,hjust=1))


### hurdle_gender

consum_gender$Decision.Maker <- factor(consum_gender$Decision.Maker, levels=c("Man", "Woman", "Both"))
select_gender$Decision.Maker <- factor(select_gender$Decision.Maker, levels=c("Man", "Woman", "Both"))

g3_1<-ggplot(consum_gender, aes(x=Decision.Maker, y=AAP, colour=Decision.Maker, group=Decision.Maker)) + 
  geom_point(position=pd, size=3, shape=21, fill="white") +
  geom_errorbar(data=consum_gender, aes(ymin=lower, ymax=upper, 
                                     color=Decision.Maker), width=.1, position=pd)+
  scale_y_continuous(limits=c(5000,10500))+
  xlab("Decision Maker")+
  ylab("")+
  ggtitle("") +
  scale_color_discrete(guide=F)+
  theme_bw()+ 
  theme(plot.margin=unit(c(-0.5,0.5,0,-0.5), "cm"))

g3_2<-ggplot(select_gender, aes(x=Decision.Maker, y=AME, colour=Decision.Maker, group=Decision.Maker)) + 
  geom_point(position=pd, size=3, shape=21, fill="white") +
  geom_errorbar(data=select_gender, aes(ymin=lower, ymax=upper, 
                                        color=Decision.Maker), width=.1, position=pd)+
  scale_y_continuous(limits=c(0,1))+
  xlab("Decision Maker")+
  ylab("")+
  ggtitle("") +
  scale_color_discrete(guide=F)+
  theme_bw()+ 
  theme(plot.margin=unit(c(-0.5,0.5,0,-0.5), "cm"))


### hurdle_house

g4_1<-ggplot(data=consum_house, aes(x=Household.Size, y=AAP)) + 
  geom_line()+
  geom_ribbon(aes(ymin=consum_house$lower, ymax=consum_house$upper), linetype=2, alpha=0.1)+
  ggtitle("") +
  xlab("Household Size")+
  ylab("")+
  scale_x_continuous(limits=c(0,25))+
  scale_y_continuous(limits=c(5000,10500))+
  theme_bw()+ 
  theme(plot.margin=unit(c(-0.5,0.5,0,-0.5), "cm"))


g4_2<-ggplot(data=select_house, aes(x=Household.Size, y=AME)) + 
  geom_line()+
  geom_ribbon(aes(ymin=select_house$lower, ymax=select_house$upper), linetype=2, alpha=0.1)+
  ggtitle("") +
  xlab("Household Size")+
  ylab("")+
  scale_x_continuous(limits=c(0,25))+
  scale_y_continuous(limits=c(0,1))+
  theme_bw()+ 
  theme(plot.margin=unit(c(-0.5,0.5,0,-0.5), "cm"))

### hurdle_exp

g5_1<-ggplot(data=consum_exp, aes(x=Expenditure, y=AAP)) + 
  geom_line()+
  geom_ribbon(aes(ymin=consum_exp$lower, ymax=consum_exp$upper), linetype=2, alpha=0.1)+
  ggtitle("") +
  xlab("Monthly Expenditure")+
  ylab("")+
  scale_x_continuous(limits=c(500,60000))+
  scale_y_continuous(limits=c(5000,10500))+
  theme_bw()+ 
  theme(plot.margin=unit(c(-0.5,0.5,0,-0.5), "cm"))


g5_2<-ggplot(data=select_exp, aes(x=Monthly.Expenditure, y=AME)) + 
  geom_line()+
  geom_ribbon(aes(ymin=select_exp$lower, ymax=select_exp$upper), linetype=2, alpha=0.1)+
  ggtitle("") +
  xlab("Monthly Expenditure")+
  scale_x_continuous(limits=c(500,60000))+
  ylab("")+
  scale_y_continuous(limits=c(0,1))+
  theme_bw()+ 
  theme(plot.margin=unit(c(-0.5,0.5,0,-0.5), "cm"))

### hurdle_age

g6_1<-ggplot(data=consum_age, aes(x=Age, y=AAP)) + 
  geom_line()+
  geom_ribbon(aes(ymin=consum_age$lower, ymax=consum_age$upper), linetype=2, alpha=0.1)+
  ggtitle("") +
  xlab("Household Head Age")+
  ylab("")+
  scale_x_continuous(limits=c(18,80))+
  scale_y_continuous(limits=c(5000,10500))+
  theme_bw()+ 
  theme(plot.margin=unit(c(-0.5,0.5,0,-0.5), "cm"))

g6_2<-ggplot(data=select_age, aes(x=Age, y=AME)) + 
  geom_line()+
  geom_ribbon(aes(ymin=select_age$lower, ymax=select_age$upper), linetype=2, alpha=0.1)+
  ggtitle("") +
  xlab("Household Head Age")+
  ylab("")+
  scale_x_continuous(limits=c(18,80))+
  scale_y_continuous(limits=c(0,1))+
  theme_bw()+ 
  theme(plot.margin=unit(c(-0.5,0.5,0,-0.5), "cm"))

# pdf("C:/Users/59634/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/Manuscript/Figures/churdle_ME1.pdf")
# grid.arrange(g1_1, g1_2, g2_1, g2_2, g3_1, g3_2, nrow=3,
#              top = textGrob("The Averge Marginal Effect with 95% of Confidence Interval",gp=gpar(fontsize=15,font=3)))
# dev.off()
# 
# pdf("C:/Users/59634/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/Manuscript/Figures/churdle_ME2.pdf")
# grid.arrange(g4_1, g4_2, g5_1, g5_2, g6_1, g6_2, nrow=3, 
#              top = textGrob("The Averge Marginal Effect with 95% of Confidence Interval",gp=gpar(fontsize=15,font=3)))
# dev.off()

pdf("C:/Users/59634/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/Manuscript/Figures/churdle_ME.pdf")
grid.arrange(g1_2, g1_1, g2_2, g2_1, g3_2, g3_1, g4_2, g4_1, g5_2, g5_1, g6_2, g6_1, nrow=6,
             top = textGrob("The Average Adjusted Predictions with 95% of Confidence Interval",gp=gpar(fontsize=15,font=3)))
dev.off()


########################## APPENDIX ##############################

names(access_nona)[names(access_nona) == "m1_q11_village_code"] <- "village_code"


# Use 2011 census data
dta<-read.dta13("2011census.dta")

village = data.frame(
       village_code = dta$c11_2011_vill_code,
       c11_2011_total_pop = dta$c11_2011_total_pop,
       c01_2001_dist_town = dta$c01_2001_dist_town
   )

colnames(village) = c("village_code",
                   "population_log",
                   "village_town_distance_log"
                   )

# # 1. village level covariate: number of households
# village<-read.dta13("RawDataVillage-1.dta")
# 
# village$v_q21_2_elec_hab1_total<-as.numeric(village$v_q21_2_elec_hab1_total)
# village$v_q21_2_elec_hab2_total<-as.numeric(village$v_q21_2_elec_hab2_total)
# village$v_q21_2_elec_hab3_total<-as.numeric(village$v_q21_2_elec_hab3_total)
# village$v_q21_2_elec_hab4_total<-as.numeric(village$v_q21_2_elec_hab4_total)
# village$v_q21_2_elec_hab5_total<-as.numeric(village$v_q21_2_elec_hab5_total)
# village$v_q21_2_elec_hab6_total<-as.numeric(village$v_q21_2_elec_hab6_total)
# village$v_q21_2_elec_hab7_total<-as.numeric(village$v_q21_2_elec_hab7_total)
# village$v_q21_2_elec_hab8_total<-as.numeric(village$v_q21_2_elec_hab8_total)
# village$v_q21_2_elec_hab9_total<-as.numeric(village$v_q21_2_elec_hab9_total)
# village$v_q21_2_elec_hab10_total<-as.numeric(village$v_q21_2_elec_hab10_total)
# village$v_q21_2_elec_hab11_total<-as.numeric(village$v_q21_2_elec_hab11_total)
# village$v_q21_2_elec_hab12_total<-as.numeric(village$v_q21_2_elec_hab12_total)
# village$v_q21_2_elec_hab13_total<-as.numeric(village$v_q21_2_elec_hab13_total)
# village$v_q21_2_elec_hab14_total<-as.numeric(village$v_q21_2_elec_hab14_total)
# village$v_q21_2_elec_hab15_total<-as.numeric(village$v_q21_2_elec_hab15_total)
# village$v_q21_2_elec_hab16_total<-as.numeric(village$v_q21_2_elec_hab16_total)
# village$v_q21_2_elec_hab17_total<-as.numeric(village$v_q21_2_elec_hab17_total)
# 
# village$hh<-village$v_q21_2_elec_hab1_total+village$v_q21_2_elec_hab2_total+
#   village$v_q21_2_elec_hab3_total+village$v_q21_2_elec_hab4_total+
#   village$v_q21_2_elec_hab5_total+village$v_q21_2_elec_hab6_total+
#   village$v_q21_2_elec_hab7_total+village$v_q21_2_elec_hab8_total+
#   village$v_q21_2_elec_hab9_total+village$v_q21_2_elec_hab10_total+
#   village$v_q21_2_elec_hab11_total+village$v_q21_2_elec_hab12_total+
#   village$v_q21_2_elec_hab13_total+village$v_q21_2_elec_hab14_total+
#   village$v_q21_2_elec_hab15_total+village$v_q21_2_elec_hab16_total+
#   village$v_q21_2_elec_hab17_total
# 
# 
# 

# 2. village level covariate: distance to buy LPG cylinders among LPG users

access_nona$m4_q103_15_lpg_distance<-
  ifelse(access_nona$m2_q55_grid=="No", NA, access_nona$m4_q103_15_lpg_distance)
# change lpg distance to NA if hh does not use LPG

access_nona$village_lpg_distance<-
  ave(access_nona$m4_q103_15_lpg_distance, access_nona$village_code, 
      FUN=function(x)mean(x, na.rm = T))
# Mssing lpg distance of 1171 HHs in 53 villages



# # 3. village level covariate: distance to town, use ditance to buy firewood to represent
# 
# access_nona$m4_q111_2_firewood_mkt_dist<- 
#   ifelse(access_nona$m4_q111_2_firewood_mkt_dist=="0" & access_nona$m4_q109_3_firewood_mkt=="0", 
#          NA, access_nona$m4_q111_2_firewood_mkt_dist)
# # change 0 to NA because sometimes 0 is recorded when no firewood bought from mkt, i.e. NA misrecorded as o
# 
# access_nona$village_town_distance<-
#   ave(access_nona$m4_q111_2_firewood_mkt_dist, access_nona$village_code, 
#       FUN=function(x)mean(x, na.rm = T))
# # ***Missing town distance of 5313 HHs in 232 villages

# merge HH survey with village survey
access_app<-merge(access_nona, village, by="village_code")

summary(access_app$village_lpg_distance) #1123 NA
village_lpg_distance<-unique(access_app[,c("village_lpg_distance", "village_code")])
write.csv(village_lpg_distance, "village_lpg_distance.csv")

summary(access_app$population_log) 
population_log<-unique(access_app[,c("population_log", "village_code")])
write.csv(population_log, "population_log.csv")

summary(access_app$village_town_distance_log) # 356 NA in 15 villages
# Missing data for ID: 130191 130442 130445 130581 146788 152284 152521 154834 
# 155433 157764 158006 174282 176601 187171 452586
village_town_distance_log<-unique(access_app[,c("village_town_distance_log", "village_code")])
write.csv(village_town_distance_log, "village_town_distance_log.csv")

summary(access_app$avgforest) 
avgforest<-unique(access_app[,c("avgforest", "village_code")])
write.csv(avgforest, "avgforest.csv")


# unique(access_nona$village_code)
# unique(village$village_code)
# # village ID: 401793 402791 402163 401901 427697 403081 401060 427751 427706 427199 427664 400530 401951 402230 401641 427374 400762 400555 427215 402491 427432 427722 427223 401861 402944 427846 427513 401172 403285 403129 403107 400819 401827 402596 427332 401159 403381 402370 402876 402686 427702 403030
# # missing village part survey (total 504 HHs, 42 villages of 2018 survey)

data_app<-dplyr:: select(access_app,m1_q3_hhid,year,m1_q8_state_code,village_code, LPG, lpg_log,
                         lpg_consumption, m4_q109_firewood,m4_q113_dungcake,m4_q114_agro,m4_q115_other_fuel,
                         fuel_stack, m1_q32_month_expenditure,month_expenditure_log, Numpers_house, 
                         Caste,Caste_SC,Caste_ST,Caste_OBC,Caste_General,
                         Education,Edu_NoFormalSchooling,Edu_UpTo5thStandard,Edu_MoreThan5thStandard,
                         Religion, Religion_Hindu,Religion_Other,m1_q19_age, 
                         Decision_Maker, Decision_MaleHouseholdHead, Decision_FemaleHouseholdHead, Decision_Both, 
                         LPG_exclusive_use, Fuel_stacking_with_LPG_as_Primary,
                         Fuel_stacking_with_Other_Fuels_as_Primary,No_Adoption_of_LPG,
                         State_UttarPradesh,Round2,Round1,round,m1_q8_state,
                         State_Bihar,State_WestBengal,State_Jharkhand,State_Odisha,State_MadhyaPradesh,
                         pmuy,avgforest,m4_q103_1_lpg_year,
                         population_log, village_lpg_distance, village_town_distance_log)

data_app_nona<-na.omit(data_app)
# Left with 16092 HHs if remove all NAs

write.dta(data_app, "data_app.dta")




##### Other analysis

#################### This section is only for ID test ########################
access$ID<-group_indices(access,finalhhid)
access$id<-1
access$round<-ifelse(access$year=="2018",1,0)
access1<-subset(access,access$round=="0") # 8563 HH in round 2015
access2<-subset(access,access$round=="1") # 9072 HH in round 2018
# caluculate sum of round and id identifiers for each HH ID
ID_rounds<-aggregate(access[c("round","id")],
                     by=list(access$ID), 
                     sum,na.rm=TRUE, na.action=NULL)
ID_rounds<-dplyr::rename(ID_rounds, "ID"=Group.1,"sum_round"=round, "observations"=id)
access<-merge(access,ID_rounds, by="ID")
# Subset observations appear in both round
data<-subset(access, access$sum_round=="1" & access$observations=="2")
data1<-subset(data,data$round=="0") # 8318 HH in both rounds
data2<-subset(data,data$round=="1")

observe1<-subset(access, access$observations=="1") 
length(unique(observe1$finalhhid))

obs1<-cbind(observe1$ID,observe1$m1_q8_state, observe1$round)

# A problem of repeated HHID for 3 different HH


b<-as.data.frame(data1$ID-data2$ID)
b$id<-1:nrow(b)

c<-subset(b,!(b$value=="0"))

access$finalID<-group_indices(access,finalhhid)
# caluculate sum of round and id identifiers for each HH ID
ID_rounds_2<-aggregate(access[c("round","id")],
                       by=list(access$finalID), 
                       sum,na.rm=TRUE, na.action=NULL)
ID_rounds_2<-dplyr::rename(ID_rounds_2, "finalID"=Group.1,"sum_round"=round, "observations"=id)
access_2<-merge(access,ID_rounds_2, by="finalID")

data_2<-subset(access_2, access_2$sum_round=="1" & access_2$observations=="2")




#### gologit plot ####

gologit_edu<-read.csv("gologit_edu.csv")
gologit_caste<-read.csv("gologit_caste.csv")
gologit_gender<-read.csv("gologit_gender.csv")
gologit_house<-read.csv("gologit_house.csv")
gologit_age<-read.csv("gologit_age.csv")
gologit_exp<-read.csv("gologit_exp.csv")

# gologit_edu

a1 <- cbind(gologit_edu[, 2], 1, c(1:3))
b1 <- cbind(gologit_edu[, 3], 2, c(1:3))
c1 <- cbind(gologit_edu[, 4], 3, c(1:3))
d1 <- cbind(gologit_edu[, 5], 4, c(1:3))

#c("No Formal Schooling","Up to 5th Standard","More than 5th Standard"))

gologit_edu <- as.data.frame(rbind(a1, b1, c1, d1))
colnames(gologit_edu) <-c("Prob", "Outcome", "Group")
gologit_edu$Group<-factor(gologit_edu$Group,labels=c("No Formal Schooling","Up to 5th", "More than 5th"))
gologit_edu$Outcome<-factor(gologit_edu$Outcome,labels=c("Exclusive LPG", "LPG as Primary","LPG as Secondary","No LPG"))

g1_3<-ggplot(gologit_edu, aes(x=Group, y=Prob, fill=Outcome)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=gologit_edu$Outcome),position=position_stack(0.5), size=1.5)+
  xlab("Education")+
  ylab("")+
  scale_fill_manual(values=c("#00CCFF", "#0099FF", "#0066FF", "#0033FF"))+
  theme_bw()+
  theme(legend.position = "none")


#+scale_y_continuous(labels = dollar_format(suffix = "%", prefix = ""))+
#geom_text(data=gologit_edu, aes(x=Group, y=Prob, label = paste0(Prob*100,"%")),
#          colour="black", family="Tahoma", size=4)



# gologit_caste

a2 <- cbind(gologit_caste[, 2], 1, c(1:4))
b2 <- cbind(gologit_caste[, 3], 2, c(1:4))
c2 <- cbind(gologit_caste[, 4], 3, c(1:4))
d2 <- cbind(gologit_caste[, 5], 4, c(1:4))

gologit_caste <- as.data.frame(rbind(a2, b2, c2, d2))
colnames(gologit_caste) <-c("Prob", "Outcome", "Group")
gologit_caste$Group<-factor(gologit_caste$Group,labels=c("General","OBC", "ST","SC"))
gologit_caste$Outcome<-factor(gologit_caste$Outcome,labels=c("Exclusive LPG", "LPG as Primary","LPG as Secondary","No LPG"))

g2_3<-ggplot(gologit_caste, aes(x=Group, y=Prob, fill=Outcome)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=gologit_caste$Outcome),position=position_stack(0.5), size=1.5)+
  xlab("Caste")+
  ylab("")+
  scale_fill_manual(values=c("#00CCFF", "#0099FF", "#0066FF", "#0033FF"))+
  theme_bw()+
  theme(legend.position = "none")


# gologit_gender

a3 <- cbind(gologit_gender[, 2], 1, c(1:3))
b3 <- cbind(gologit_gender[, 3], 2, c(1:3))
c3 <- cbind(gologit_gender[, 4], 3, c(1:3))
d3 <- cbind(gologit_gender[, 5], 4, c(1:3))

gologit_gender <- as.data.frame(rbind(a3, b3, c3, d3))
colnames(gologit_gender) <-c("Prob", "Outcome", "Group")
gologit_gender$Group<-factor(gologit_gender$Group,labels=c("Man","Woman", "Both"))
gologit_gender$Outcome<-factor(gologit_gender$Outcome,labels=c("Exclusive LPG", "LPG as Primary","LPG as Secondary","No LPG"))

g3_3<-ggplot(gologit_gender, aes(x=Group, y=Prob, fill=Outcome)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=gologit_gender$Outcome),position=position_stack(0.5), size=1.5)+
  xlab("Decision Maker")+
  ylab("")+
  scale_fill_manual(values=c("#00CCFF", "#0099FF", "#0066FF", "#0033FF"))+
  theme_bw()+
  theme(legend.position = "none")



# gologit_exp

g4_3<-ggplot(gologit_exp, aes(x=Expenditure, y=LPG..Exclusive.Use))+
  geom_line(aes(x=Expenditure, y=LPG..Exclusive.Use), col="#0066CC", size=1)+
  geom_line(aes(x=Expenditure, y=Fuel.Stack..LPG.), col="#336600", size=1)+
  geom_line(aes(x=Expenditure, y=Fuel.Stack..Other.), col="#FF9933", size=1)+
  geom_line(aes(x=Expenditure, y=No.LPG.Adoption), col="#FF0000", size=1)+
  scale_x_continuous(limits=c(500,60000))+
  scale_y_continuous(limits=c(0,1))+
  annotate("text", x=58000, y=0.28, label= "No LPG",size=2) +
  annotate("text", x=56000, y=0.34, label= "Exclusive LPG",size=2) +
  annotate("text", x=55000, y=0.19, label= "LPG as Secondary",size=2) +
  annotate("text", x=54000, y=0.24, label= "LPG as Primary",size=2) +
  ylab("")+
  theme_bw()


# gologit_age

g5_3<-ggplot(gologit_age, aes(x=Age, y=LPG..Exclusive.Use))+
  geom_line(aes(x=Age, y=LPG..Exclusive.Use), col="#0066CC", size=1)+
  geom_line(aes(x=Age, y=Fuel.Stack..LPG.), col="#336600", size=1)+
  geom_line(aes(x=Age, y=Fuel.Stack..Other.), col="#FF9933", size=1)+
  geom_line(aes(x=Age, y=No.LPG.Adoption), col="#FF0000", size=1)+
  scale_x_continuous(limits=c(18,80))+
  scale_y_continuous(limits=c(0,1))+
  annotate("text", x=76, y=0.10, label= "Exclusive LPG",size=2) +
  annotate("text", x=76, y=0.20, label= "LPG as Primary",size=2) +
  annotate("text", x=74.8, y=0.15, label= "LPG as Secondary",size=2) +
  annotate("text", x=79, y=0.61, label= "No LPG",size=2) +
  ylab("")+
  theme_bw()

# gologit_house

g6_3<-ggplot(gologit_house, aes(x=Household.Size, y=LPG..Exclusive.Use))+
  geom_line(aes(x=Household.Size, y=LPG..Exclusive.Use), col="#0066CC", size=1)+
  geom_line(aes(x=Household.Size, y=Fuel.Stack..LPG.), col="#336600", size=1)+
  geom_line(aes(x=Household.Size, y=Fuel.Stack..Other.), col="#FF9933", size=1)+
  geom_line(aes(x=Household.Size, y=No.LPG.Adoption), col="#FF0000", size=1)+
  annotate("text", x=23, y=0.01, label= "Exclusive LPG",size=2) +
  annotate("text", x=23, y=0.09, label= "LPG as Primary",size=2) +
  annotate("text", x=22.7, y=0.20,label= "LPG as Secondary",size=2) +
  annotate("text", x=24, y=0.73, label= "No LPG",size=2) +
  scale_x_continuous(limits=c(0,25))+
  scale_y_continuous(limits=c(0,1))+
  xlab("Household Size")+
  ylab("")+
  theme_bw()

pdf("C:/Users/59634/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/Manuscript/Figures/gologit.pdf")
grid.arrange(g1_3, g4_3, g2_3, g5_3, g3_3, g6_3, nrow=3,
             top = textGrob("The Predicted Probabilities of LPG Adoption",gp=gpar(fontsize=15,font=3)))
dev.off()



###################################################################



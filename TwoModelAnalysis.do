ssc install estout, replace
ssc install gologit2, replace
ssc install fre, replace
ssc install margeff, replace
ssc install spost13_ado
ssc install regplot
ssc install prtab

clear all
set more off

*	************************************************************************
* 	File-Name: 	0410_Run.do
*	Log-file:	na
*	Date:  		03/01/2019
*	Author: 	Xiaoxue Hou	
*	Data Used:  data.dta
*	Output		Model Results Table and Marginal Effects Tables
*	Purpose:   	.do file to replicate the findings in " "
*
*	************************************************************************

*	************************************************************************
* 	0. Setting up the data
*	************************************************************************

* Change this link to your working directory where the dataset is located.
use "/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/Data/data.dta"
* Change this link to your working directory where you want to work.
cd "/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/Manuscript/Tables"	

set scheme s1mono

*	************************************************************************
* 	1. Build Model
*	************************************************************************
rename Decision_Maker gender

churdle linear lpg_log i.pmuy month_expenditure_log Numpers_house i.Education i.Caste i.gender i.Religion m1_q19_age m4_q103_1_lpg_year State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round, select(month_expenditure_log Numpers_house i.Education i.Caste i.gender i.Religion m1_q19_age State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round) vce(cluster m1_q11_village_code) baselevels ll(0)
est sto m1


*	************************************************************************
* 	2. Marginal Effect (Second-Stage)
*	************************************************************************


*******************************************************************
* Using AAPs - Average Adjusted Predictions

*******************************************************************

* Education
margins Education if lpg_log>0, expression(exp(predict(xb)))
est sto consumption_Education 
outreg2  using \Dropbox\ACCESS-2 Clean Cooking Fuel Adoption and Use\Data\ME output\consum_Education.txt

*PMUY
margins pmuy if lpg_log>0, expression(exp(predict(xb)))

*LPG years
margins if lpg_log>0, at(m4_q103_1_lpg_year=(0 (0.5) 8)) expression(exp(predict(xb)))

* Caste
margins Caste if lpg_log>0, expression(exp(predict(xb)))

* Decision Maker
margins gender if lpg_log>0, expression(exp(predict(xb)))
*est sto consum_gender
*outreg2 consum_gender using "C:\Users\59634\Dropbox\ACCESS-2 Clean Cooking Fuel Adoption and Use\Data\ME output\consum_gender.txt"

* Religion
margins Religion if lpg_log>0, expression(exp(predict(xb)))
*est sto consum_religion
*outreg2 consum_religion using "C:\Users\59634\Dropbox\ACCESS-2 Clean Cooking Fuel Adoption and Use\Data\ME output\consum_religion.txt"

* Expenditure
margins if lpg_log>0, at(month_expenditure_log=(0 (0.5) 11.29)) expression(exp(predict(xb)))
est sto consum_exp

* Household Size
margins if lpg_log>0, at(Numpers_house=(0 (4) 98)) expression(exp(predict(xb)))
est sto consum_house


* Age

margins if lpg_log>0, at(m1_q19_age=(18 (4) 98)) expression(exp(predict(xb)))
est sto consum_age

**** village covariates
margins if lpg_log>0, at(population_log=(4 (0.5) 11)) expression(exp(predict(xb)))
margins if lpg_log>0, at(village_lpg_distance=(0 (2) 44)) expression(exp(predict(xb)))
margins if lpg_log>0, at(village_town_distance_log=(0 (0.2) 5)) expression(exp(predict(xb)))
margins if lpg_log>0, at(avgforest=(0 (5) 70)) expression(exp(predict(xb)))


*	************************************************************************
* 	3. Marginal Effect (First-Stage)
*	************************************************************************

quietly probit lpg_log month_expenditure_log Numpers_house i.Education i.Caste i.gender i.Religion m1_q19_age m4_q103_1_lpg_year State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round, vce(cluster m1_q11_village_code)

* Education
margins Education 

* Caste
margins Caste 
marginsplot, recast(dot) yscale(r(0 0.8)) xlabel(1 "General" 2 "OBC" 3 "ST" 4 "SC") ylabel(0 (0.1) 0.8) title("") ytitle("Probability of LPG Selection") plotregion(fcolor(white)) graphregion(fcolor(white))
graph rename Graph churdle_selection_Caste

* Decision Maker
margins gender 
marginsplot, recast(dot) yscale(r(0 0.8)) xlabel(1 "Man" 2 "Women" 3 "Both") ylabel(0 (0.1) 0.8) title("") ytitle("Probability of LPG Selection") xtitle("Gender of Decision Maker") plotregion(fcolor(white)) graphregion(fcolor(white))
graph rename Graph churdle_selection_DM

* Religion
margins Religion 
marginsplot, recast(dot) yscale(r(0 0.8)) xlabel(1 "Other" 2 "Hindu") ylabel(0 (0.1) 0.8) title("") ytitle("Probability of LPG Selection") plotregion(fcolor(white)) graphregion(fcolor(white))
graph rename Graph churdle_selection_Religion

* Expenditure
margins, at(month_expenditure_log=(0 (0.5) 11.29))

* Household Size
margins, at(Numpers_house=(0 (4) 98))

* Age
margins, at(m1_q19_age=(18 (4) 98))

* village covariates
margins, at(population_log=(4 (0.5) 11)) 
margins, at(village_lpg_distance=(0 (2) 44)) 
margins, at(village_town_distance_log=(0 (0.2) 5)) 
margins, at(avgforest=(0 (5) 70)) 


*************************************************************
************************* gologit2 **************************
*************************************************************

 label define Education 1 "No formal schooling" 2 "Up to 5th grade" 3 "More than 5th grade"
 label define Caste 1 "General" 2 "OBC" 3 "ST" 4 "SC"
 label define Decision_Maker 1 "Man" 2 "Woman" 3 "Both"


xi: gologit2 fuel_stack month_expenditure_log Numpers_house Edu_UpTo5thStandard Edu_MoreThan5thStandard Caste_SC Caste_ST Caste_OBC Decision_MaleHouseholdHead Religion_Hindu m1_q19_age State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round, cluster(m1_q11_village_code) or autofit gsvy force


*******************************************************************
mtable, at(Numpers_house=(0 (4) 98))
mchange black female age, stats(change start end) dec(5) delta(10)

mtable, at(_IEducation_2=0 _IEducation_3=0)
mtable, at(_IEducation_2=1 _IEducation_3=0)
mtable, at(_IEducation_2=0 _IEducation_3=1)
* Education *
margins, at(_IEducation_2=0 _IEducation_3=0)  predict(outcome(#1))
margins, at(_IEducation_2=1 _IEducation_3=0)  predict(outcome(#1))
margins, at(_IEducation_2=0 _IEducation_3=1)  predict(outcome(#1))

margins, at(_IEducation_2=0 _IEducation_3=0)  predict(outcome(#2))
margins, at(_IEducation_2=1 _IEducation_3=0)  predict(outcome(#2))
margins, at(_IEducation_2=0 _IEducation_3=1)  predict(outcome(#2))

margins, at(_IEducation_2=0 _IEducation_3=0)  predict(outcome(#3))
margins, at(_IEducation_2=1 _IEducation_3=0)  predict(outcome(#3))
margins, at(_IEducation_2=0 _IEducation_3=1)  predict(outcome(#3))

margins, at(_IEducation_2=0 _IEducation_3=0)  predict(outcome(#4))
margins, at(_IEducation_2=1 _IEducation_3=0)  predict(outcome(#4))
margins, at(_IEducation_2=0 _IEducation_3=1)  predict(outcome(#4))


* Household Size *
margins, at(Numpers_house=(0 (4) 98)) predict(outcome(#1))

margins, at(Numpers_house=(0 (4) 98)) predict(outcome(#2))

margins, at(Numpers_house=(0 (4) 98)) predict(outcome(#3))

margins, at(Numpers_house=(0 (4) 98)) predict(outcome(#4))

mtable, at(Numpers_house=(0 (4) 98))

* Caste

margins, at(_ICaste_2=0 _ICaste_3=0 _ICaste_4=0)  predict(outcome(#1))
margins, at(_ICaste_2=1 _ICaste_3=0 _ICaste_4=0)  predict(outcome(#1))
margins, at(_ICaste_2=0 _ICaste_3=1 _ICaste_4=0)  predict(outcome(#1))
margins, at(_ICaste_2=0 _ICaste_3=0 _ICaste_4=1)  predict(outcome(#1))

margins, at(_ICaste_2=0 _ICaste_3=0 _ICaste_4=0)  predict(outcome(#2))
margins, at(_ICaste_2=1 _ICaste_3=0 _ICaste_4=0)  predict(outcome(#2))
margins, at(_ICaste_2=0 _ICaste_3=1 _ICaste_4=0)  predict(outcome(#2))
margins, at(_ICaste_2=0 _ICaste_3=0 _ICaste_4=1)  predict(outcome(#2))

margins, at(_ICaste_2=0 _ICaste_3=0 _ICaste_4=0)  predict(outcome(#3))
margins, at(_ICaste_2=1 _ICaste_3=0 _ICaste_4=0)  predict(outcome(#3))
margins, at(_ICaste_2=0 _ICaste_3=1 _ICaste_4=0)  predict(outcome(#3))
margins, at(_ICaste_2=0 _ICaste_3=0 _ICaste_4=1)  predict(outcome(#3))

margins, at(_ICaste_2=0 _ICaste_3=0 _ICaste_4=0)  predict(outcome(#4))
margins, at(_ICaste_2=1 _ICaste_3=0 _ICaste_4=0)  predict(outcome(#4))
margins, at(_ICaste_2=0 _ICaste_3=1 _ICaste_4=0)  predict(outcome(#4))
margins, at(_ICaste_2=0 _ICaste_3=0 _ICaste_4=1)  predict(outcome(#4))

mtable, at(_ICaste_2=0 _ICaste_3=0 _ICaste_4=0)
mtable, at(_ICaste_1=1)
mtable, at(_ICaste_2=1 _ICaste_3=0 _ICaste_4=0)
mtable, at(_ICaste_2=0 _ICaste_3=1 _ICaste_4=0)
mtable, at(_ICaste_2=0 _ICaste_3=0 _ICaste_4=1)

* age *

margins, at(m1_q19_age=(18 (4) 98)) predict(outcome(#1))

margins, at(m1_q19_age=(18 (4) 98)) predict(outcome(#2))

margins, at(m1_q19_age=(18 (4) 98)) predict(outcome(#3))

margins, at(m1_q19_age=(18 (4) 98)) predict(outcome(#4))

mtable, at(m1_q19_age=(18 (4) 98))

* gender *

margins, at(_Igender_2=0 _Igender_3=0)  predict(outcome(#1))
margins, at(_Igender_2=1 _Igender_3=0)  predict(outcome(#1))
margins, at(_Igender_2=0 _Igender_3=1)  predict(outcome(#1))

margins, at(_Igender_2=0 _Igender_3=0)  predict(outcome(#2))
margins, at(_Igender_2=1 _Igender_3=0)  predict(outcome(#2))
margins, at(_Igender_2=0 _Igender_3=1)  predict(outcome(#2))

margins, at(_Igender_2=0 _Igender_3=0)  predict(outcome(#3))
margins, at(_Igender_2=1 _Igender_3=0)  predict(outcome(#3))
margins, at(_Igender_2=0 _Igender_3=1)  predict(outcome(#3))

margins, at(_Igender_2=0 _Igender_3=0)  predict(outcome(#4))
margins, at(_Igender_2=1 _Igender_3=0)  predict(outcome(#4))
margins, at(_Igender_2=0 _Igender_3=1)  predict(outcome(#4))


mtable, at(_Igender_2=0 _Igender_3=0)
mtable, at(_Igender_2=1 _Igender_3=0)
mtable, at(_Igender_2=0 _Igender_3=1)

* Expenditure *

margins, at(month_expenditure_log=(0 (0.5) 11.29)) predict(outcome(#1))
margins, at(month_expenditure_log=(0 (0.5) 11.29)) predict(outcome(#2))
margins, at(month_expenditure_log=(0 (0.5) 11.29)) predict(outcome(#3))
margins, at(month_expenditure_log=(0 (0.5) 11.29)) predict(outcome(#4))


mtable, at(month_expenditure_log=(0 (0.5) 11.29))


* village covariates


* Population_log
margins, at(population_log=(4 (0.5) 11)) predict(outcome(#1))
margins, at(population_log=(4 (0.5) 11)) predict(outcome(#2))
margins, at(population_log=(4 (0.5) 11)) predict(outcome(#3))
margins, at(population_log=(4 (0.5) 11)) predict(outcome(#4))

* lpg distance
margins, at(village_lpg_distance=(0 (2) 44)) predict(outcome(#1))
margins, at(village_lpg_distance=(0 (2) 44)) predict(outcome(#2))
margins, at(village_lpg_distance=(0 (2) 44)) predict(outcome(#3))
margins, at(village_lpg_distance=(0 (2) 44)) predict(outcome(#4))

* town distance
margins, at(village_town_distance_log=(0 (0.2) 5)) predict(outcome(#1))
margins, at(village_town_distance_log=(0 (0.2) 5)) predict(outcome(#2))
margins, at(village_town_distance_log=(0 (0.2) 5)) predict(outcome(#3))
margins, at(village_town_distance_log=(0 (0.2) 5)) predict(outcome(#4))

* average forest
margins, at(avgforest=(0 (5) 70)) predict(outcome(#1))
margins, at(avgforest=(0 (5) 70)) predict(outcome(#2))
margins, at(avgforest=(0 (5) 70)) predict(outcome(#3))
margins, at(avgforest=(0 (5) 70)) predict(outcome(#4))



********************************* APPENDIX ********************************
use "/Dropbox/ACCESS-2 Clean Cooking Fuel Adoption and Use/Data/data_app.dta"


churdle linear lpg_log i.pmuy month_expenditure_log Numpers_house i.Education i.Caste i.gender i.Religion m1_q19_age m4_q103_1_lpg_year State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round, select(month_expenditure_log Numpers_house i.Education i.Caste i.gender i.Religion m1_q19_age State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round) vce(cluster m1_q11_village_code) baselevels ll(0)
est sto m1

xi: gologit2 fuel_stack month_expenditure_log Numpers_house i.Education i.Caste i.gender i.Religion m1_q19_age State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round, cluster(m1_q11_village_code) baselevels or autofit gsvy force
est sto g1

esttab m1 g1 using "/Desktop/appendix_run.tex", eform(0 1) se ar2 unstack drop(State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round _cons) label mlabels("two-stage model" "generalized logit model")  collabels("consumption" "selection")


** Add village indicators
churdle linear lpg_log i.pmuy month_expenditure_log Numpers_house i.Education i.Caste i.gender i.Religion m1_q19_age m4_q103_1_lpg_year population_log avgforest village_lpg_distance village_town_distance_log State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round, select(month_expenditure_log Numpers_house i.Education i.Caste i.gender i.Religion m1_q19_age population_log avgforest village_lpg_distance village_town_distance_log State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round) vce(cluster village_code) baselevels ll(0)
est sto m2

quietly probit lpg_log month_expenditure_log Numpers_house i.Education i.Caste i.gender i.Religion m1_q19_age population_log avgforest village_lpg_distance village_town_distance_log State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round, vce(cluster village_code)

xi: gologit2 fuel_stack month_expenditure_log Numpers_house i.Education i.Caste i.gender i.Religion m1_q19_age population_log avgforest village_lpg_distance village_town_distance_log State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round, cluster(village_code) baselevels or autofit gsvy force 
est sto g2

esttab m2 g2 using "/Desktop/appendix_run.tex", eform(0 1) se ar2 unstack drop(State_Bihar State_WestBengal State_Jharkhand State_Odisha State_MadhyaPradesh round _cons) label mlabels("two-stage model" "generalized logit model")  collabels("consumption" "selection")



/******************************************************************
Nutritional Epidemiology Final Data Analysis Project
Elsbeth Sites
11.13.18
**********************************************************************/ 

/* Begin with merged dataset: 
NHANES Demographics, Consumer Behavior Questionnaires, Physical Functioning, 2007-2008 */

use "/Users/Elsbeth/Desktop/Fall 2018/Nutritional Epi/DEMO_CBQ_PFQ_08.dta"

/*********** Data cleaning ****************/

/* 
E: CBD150 time to get to grocery store, minutes
	missing - 99999 */
replace cbd150=. if cbd150==99999
replace cbd150=. if cbd150==77777
drop if cbd150>300	

/* Create tertiles based on average kcal intake */
xtile cbd150_quint=cbd150, nq(5) /* create quintiles */ 
tab cbd150_quint	



/*	
O: CBD160 # of times someone cooked dinner at home
	"During the past 7 days, how many times did 
	{you or someone else in your family/you} cook food for dinner or supper at home"
	Don't know - 777
	Refused - 999
*/
replace cbd160=. if cbd160==999
replace cbd160=. if cbd160==777
*delete outlier?
drop if cbd160>7

save "/Users/Elsbeth/Desktop/Fall 2018/Nutritional Epi/DEMO_CBQ_PFQ_08_svy_sample_outlierdropped.dta", replace

/*
Potential confounder:
PFQ059 - Physical, mental, emotional limitations - 
	refused - 7
	don't know - 9
	missing - .
*/
replace pfq059=. if pfq059==7 
replace pfq059=. if pfq059==9
label define phys_func_label 1 "Yes" 2 "No"
label values pfq059 phys_func_label


/*
Other variables of interest: 
INDFMIN2 - Annual Family Income
		refused - 77
		don't know - 99
		missing - . */

replace indfmin2=. if indfmin2==99
replace indfmin2=. if indfmin2==77

/* INDFMPIR - Ratio of family income to poverty */
gen indfmpir_cat =1
replace indfmpir_cat=2 if indfmpir >1 & indfmpir <=2
replace indfmpir_cat=3 if indfmpir >2 & indfmpir <=3
replace indfmpir_cat=4 if indfmpir >3 & indfmpir <=4
replace indfmpir_cat=5 if indfmpir >4 & indfmpir <=5
label variable indfmpir_cat "Categorical income:poverty ratio"
		
/*RIDRETH1 - Race/ethnicity recode
	missing - .
 */
label define eth_label 1 "Mexican American" 2 "Other Hispanic" 3 "Non-hispanic white" 4 "Non-hispanic black" 5 "Other race, including multi-racial" 
label values ridreth1 eth_label 
 
 
 /* RIAGENDR - Gender */
label define gender 1 "Male" 2 "Female"
label values riagendr gender 
 
/* create categorical age variable */
gen agecat =2
replace agecat = 3 if ridageyr>=30
replace agecat = 4 if ridageyr>=40
replace agecat = 5 if ridageyr>=50
replace agecat = 6 if ridageyr>=60
label define agecat_label 2 "18-29" 3 "30-39" 4 "40-49" 5 "50-59" 6 ">=60"
label values agecat agecat_label

/*********** Create sample of adults who have both exposure and outcome data ****************/
gen sample=0
replace sample=1 if ridageyr>=18
replace sample=0 if cbd150==.
replace sample=0 if cbd160==.
replace sample=0 if pfq059==.
label variable sample "Sample population of adults (>=18) only"
tabulate sample 
* sample size = 4,247

/*********** Set survey design ****************/
svyset sdmvpsu [pw = wtint2yr], strata(sdmvstra) singleunit(centered) 

*Check the survey data features
svydescribe

save "/Users/Elsbeth/Desktop/Fall 2018/Nutritional Epi/DEMO_CBQ_PFQ_08_svy_sample.dta" 

/*********** Model results ****************/

use "/Users/Elsbeth/Desktop/Fall 2018/Nutritional Epi/DEMO_CBQ_PFQ_08_svy_sample.dta"

 /* 1) Describe exposure and outcome variables (means, SDs, proportions, etc) */

 // EXPOSURE
	*A.	For the entire population
	svy, subpop(sample): mean cbd150 
	estat sd
		* in the sample, mean minutes to grocery store = 14.27, sd = 18.02
	
	*B.	By: gender
	svy, subpop(sample): mean cbd150, over(riagendr)
	estat sd 
	*age categories  
	svy, subpop(sample): mean cbd150, over(agecat)
	estat sd 
	*race/ethnicity
	svy, subpop(sample): mean cbd150, over(ridreth1)
	estat sd 
	*income:poverty ratio
	svy, subpop(sample): mean cbd150, over (indfmpir_cat)
	estat sd 
	*disability
	svy, subpop(sample): mean cbd150, over (pfq059)
	estat sd 

// OUTCOME
	*A.	For the entire population
	svy, subpop(sample): mean cbd160 
	estat sd 
		*in the sample, mean # meals prepared at home = 4.88, sd = 1.80
		
	*B.	By: gender
	svy, subpop(sample): mean cbd160, over(riagendr)
	estat sd 
	*age categories  
	svy, subpop(sample): mean cbd160, over(agecat)
	estat sd 
	*race/ethnicity
	svy, subpop(sample): mean cbd160, over(ridreth1)
	estat sd 	
	*income:poverty ratio
	svy, subpop(sample): mean cbd160, over (indfmpir_cat)
	estat sd 
	*disability
	svy, subpop(sample): mean cbd160, over (pfq059)
	estat sd 
			
/* 2) Describe bivariate association between exposure and outcome. */

svy, subpop(sample): reg cbd160 cbd150 // * R^2 is .0089
//coefficient is .0093734 percent

/* 3) regress continuous outcome on selected exposure and confounders */

svy, subpop(sample): reg cbd160 cbd150 i.indfmpir_cat i.pfq059 i.ridreth1

/* 4) Creating tables and graphs */
twoway (scatter cbd160 cbd150) (lfit cbd160 cbd150) if sample==1 


hist cbd160 if sample==1, percent
graph box cbd150, over (cbd160)

library(dplyr)
library(readr)

"
Variables of interest in `adult.dat`
1-5:	SEQN, Respondent identification number
13: DMARACER, Race
	1: White
	2: Black
	3: Other
	8: Mexican-American of unknown race
14: DMAETHNR, Ethnicity
	1: Mexican-American
	2: Other Hispanic
	3: Not Hispanic
15:		HSSEX, Sex
	1: Male
	2: Female
43:		SDPPSU6, Total NHANES III pseudo-PSU
44-5:	SDPSTRA6, Total NHANES III pseudo-stratum
61-9:	WTPFEX6, Total MEC-examined sample final weight
70-8:	WTPFHX6, Final MEC+home examination weight
1237-40:MXPAXTMR, Age in months at MEC exam
1245-8: HXPAXTMR, Age in months at home exam
1256-7:	HFA8R, Highest grade or yr of school completed
	00: Never attended or kindergarten only
	01-17
	88: Blank but applicable
	99: Don't know
1961-63:HAM10S, Up to present time,most ever weighed-lbs
	888: Blank but applicable
	999: Don't know
2281:	HAR1, Have you smoked 100+ cigarettes in life
2311:	HAR14, Ever used chewing tabacco or snuff
2330:	HAR23, Ever smoke at least 20 cigars in life
2334:	HAR26, Ever smoke 20 pipes of tobacco in life
	1: Yes
	2: No
	8: Blank but applicable
"

data_adult = read_fwf('NHANES III/adult.dat',
	fwf_positions(c(1, 13, 14, 15, 43, 44, 70, 1237, 1245, 1256, 1961, 2281, 2311, 2330, 2334), 
				  c(5, 13, 14, 15, 43, 45, 78, 1240, 1248, 1257, 1963, 2281, 2311, 2330, 2334), 
				  col_names = c('id', 'race', 'ethnicity', 'sex', 'psu', 'stratum', 
				  			  'sample.weight', 'age.mec', 'age.home', 'education.yrs', 
				  			  'weight.max', 'cigarettes', 'chew', 'cigars', 'pipe'))) %>%
		transmute(id = as.numeric(id), 
				  sex = factor(sex, labels = c('male', 'female')),
				  age.months = ifelse(!is.na(age.mec), 
				  					as.numeric(age.mec), 
				  					as.numeric(age.home)),
				  age.years = (age.months - 12) %/% 12,
				  race = factor(race, labels = c('White', 'Black', 'Other', 
				  							   'Mexican American')), 
				  ethnicity = factor(ethnicity, labels = c('Mexican American', 
				  										 'Other Hispanic', 
				  										 'Not Hispanic')), 
				  education.yrs = as.numeric(education.yrs),
				  weight.max = as.numeric(weight.max),
				  cigarettes = cigarettes, pipe = pipe, 
				  cigars = cigars, chew = chew,
				  nhanes.cycle = 0,
				  psu = factor(paste('iii', psu)), 
				  stratum = factor(paste('iii ', stratum)), 
				  sample.weight = as.numeric(sample.weight))

data_adult$race.ethnicity = 
	with(data_adult, 
		ifelse(race == 'White' & ethnicity == 'Not Hispanic', 
			   'Non-Hispanic White', 
		ifelse(race == 'Black' & ethnicity == 'Not Hispanic', 
			   'Non-Hispanic Black', 
		ifelse(race == 'Mexican American' | ethnicity == 'Mexican American' | ethnicity == 'Other Hispanic', 
			   'Hispanic', 
		ifelse(race == 'Other', 
			   'Non-Hispanic Other', 
			   '???'
		)))))
data_adult$race.ethnicity = factor(data_adult$race.ethnicity,
								   levels = c('Non-Hispanic White',
								   		   'Non-Hispanic Black', 
								   		   'Hispanic', 
								   		   'Non-Hispanic Other'))
data_adult = data_adult %>% select(-one_of('race', 'ethnicity'))

code_ed.yrs = function (ed.yrs) {
	if (ed.yrs == '88' | ed.yrs == '99' | is.na(ed.yrs)) {
		return(NA)
	} else if (ed.yrs < 9) {
		return('Less than High School')
	} else if (ed.yrs < 12) {
		return('Some High School')
	} else if (ed.yrs == 12) {
		return('High School')
	} else if (ed.yrs < 16) {
		return('Some College')
	} else {
		return('College')
	}
}
data_adult$education = sapply(data_adult$education.yrs, code_ed.yrs)
catch_error_values = function (x) {
	if (x == 888 | x == 999 | is.na(x)) {
		return(NA)
	} else {
		return(x)
	}
}
data_adult$weight.max = sapply(data_adult$weight.max, catch_error_values) 
data_adult$weight.max = data_adult$weight.max * .454

code_tobacco = function (x) {
	x = as.character(x)
	if (x == ' ' | x == '8' | x == '9' | is.na(x)) {
		return(NA)
	} else if (x == '1') {
		return(TRUE)
	} else if (x == '2') {
		return(FALSE)
	} else {
		stop('Unrecognized tobacco code')
	}
}
data_adult$cigarettes = sapply(data_adult$cigarettes, code_tobacco)
data_adult$pipe = sapply(data_adult$pipe, code_tobacco)
data_adult$cigars = sapply(data_adult$cigars, code_tobacco)
data_adult$chew = sapply(data_adult$chew, code_tobacco)
data_adult$smoker = data_adult$cigarettes #| data_adult$pipe | data_adult$cigars | data_adult$chew

data_adult = data_adult %>% select(-c(education.yrs, cigarettes, pipe, cigars, chew))


"
Variables of interest in `exam.dat`
1-5:	SEQN, Respondent identification number
1508-13:BMPWT, Weight (kg)
1528-32:BMPHT, Standing height (cm)
1524-7:BMPBMI, Body mass index
	NB need to calculate for home examinees? 

11:		DMPSTAT, Examination/interview status
	2: Interviewed, MEC-examined
	3: Interviewed, home-examined
"
data_exam = read_fwf('NHANES III/exam.dat', 
					 fwf_positions(c(1, 11, 1508, 1524, 1528), 
					 			  c(5,  11, 1513, 1527, 1532), 
					 	col_names = c('id', 'mec.home', 
					 				  'weight', 'bmi', 'height'))) %>%
	transmute(id = as.numeric(id), 
			  mec.home = factor(mec.home, levels = c(2,3), 
			  				  labels = c('mec', 'home')),
			  weight = as.numeric(weight), 
			  height = as.numeric(height), 
			  bmi = as.numeric(bmi))
catch_errors_weight = function (x) {
	if (x == 888888 | is.na(x)) {
		return(NA)
	} else {
		return(x)
	}
}
data_exam$weight = sapply(data_exam$weight, catch_errors_weight)
catch_errors_height = function (x) {
	if (x == 88888 | is.na(x)) {
		return(NA)
	} else {
		return(x)
	}
}
data_exam$height = sapply(data_exam$height, catch_errors_height)
catch_errors_bmi = function (x) {
	if (x == 8888 | is.na(x)) {
		return(NA)
	} else {
		return(x)
	}
}
data_exam$bmi = sapply(data_exam$bmi, catch_errors_bmi)



## Public-use Linked Mortality Files
## http://www.cdc.gov/nchs/data_access/data_linkage/mortality/data_files_data_dictionaries.htm
## Variables of interest:  
##
## In the legacy data file:
## Variables of interest:
## Col. 1-5:	NHANES Respondent Sequence Number
## Col. 7: 		Final Mortality Status
## Col. 15-7:	Person Months of Follow-up from MEC/Home
data_mort_2006 = read_fwf('mortality 2006/NHANES3_MORT_PUBLIC_USE_2010.DAT',  
					  fwf_positions(c(1, 7, 15),
					  			    c(5, 7, 17),
					  			  col_names = c('id', 'mort.status', 
					  			  			  'follow.months')),
					  na = c('.', ' ')) %>%
	transmute(id = as.numeric(id),
			  mort.2006 = factor(mort.status, labels = c('alive', 'deceased')), 
			  follow.months.2006 = as.numeric(follow.months))
## Col. 1-5: 	NHANES Respondent Sequence Number
## Col. 16:		Final Morality Status
##	0: Assumed alive
##	1: Assumed deceased
##  Blank: Ineligible or under age 18
## Col. 47-9:	Person Months of Follow-up from MEC/Exam Date
data_mort_2011 = read_fwf('mortality/NHANES_III_MORT_2011_PUBLIC.DAT', 
						  fwf_positions(c(1, 16, 47), 
						  				c(5, 16, 49), 
						  				col_names = c('id', 'mort.status', 
						  							  'follow.months')), 
						  na = c('.', ' ')) %>%
	transmute(id = as.numeric(id), 
			  mort.2011 = factor(mort.status, labels = c('alive', 'deceased')), 
			  follow.months.2011 = as.numeric(follow.months))

data_mort = full_join(data_mort_2006, data_mort_2011, by = 'id')

## Catch duplicate mortality results
# data_mort = data_mort %>%
# 	dcast(id + follow.months ~ mort.status) %>%
# 	mutate(mort.status = ifelse(!is.na(deceased),
# 									 'deceased',
# 									 ifelse(!is.na(alive),
# 									 	   'alive',
# 									 	   NA))) %>%
# 	mutate(mort.status = as.factor(mort.status)) %>%
# 	select(id, mort.status, follow.months)

dataf_iii = left_join(data_adult, data_exam) %>%
	left_join(data_mort) %>% 
	mutate(bmi.max = pmax(weight.max / (height/100)**2, bmi))



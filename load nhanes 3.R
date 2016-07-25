library(dplyr)
library(readr)

"
Variables of interest in `adult.dat`
1-5:	SEQN, Respondent identification number
15:		HSSEX, Sex
	1: Male
	2: Female
1237-40:MXPAXTMR, Age in months at MEC exam
12:		DMARETHN, Race-ethnicity
	1: Non-Hispanic White
	2: Non-Hispanic Black
	3: Mexican-American
	4: Other
1256-7:	HFA8R, Highest grade or yr of school completed
	00: Never attended or kindergarten only
	01-17
	88: Blank but applicable
	99: Don't know
43:		SDPPSU6, Total NHANES III pseudo-PSU
44-5:	SDPSTRA6, Total NHANES III pseudo-stratum
61-9:	WTPFEX6, Total MEC-examined sample final weight
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

system.time({
	data_adult = read_fwf('NHANES III/adult.dat', n_max = -1, 
						  fwf_positions(c(1, 15, 1237, 12, 1256, 1961, 
						  					2281, 2334, 2330, 2311, 43, 44, 
						  					61, 1000), 
						  				c(5, 15, 1240, 12, 1257, 1963, 
						  				  	2281, 2334, 2330, 2311, 43, 45, 
						  				  	69, 1000), 
						  				col_names = 
						  					c('id', 'sex', 'age.months', 
						  					  'race.ethnicity', 
						  					  'education.yrs', 
						  					  'weight.max', 
						  					  'cigarettes', 'pipe', 
						  					  'cigars', 'chew', 
						  					  'psu', 'stratum', 'sample.weight', 
						  					  'dummy'))) %>%
		transmute(id = as.numeric(id), 
				  sex = factor(sex, labels = c('male', 'female')),
				  age.months = as.numeric(age.months), 
				  age.years = age.months %/% 12,
				  race.ethnicity = factor(race.ethnicity, 
				  						labels = c('Non-Hispanic White', 
				  								   'Non-Hispanic Black', 
				  								   'Mexican American', 
				  								   'Other, Multiracial')),
				  education.yrs = as.numeric(education.yrs),
				  weight.max = as.numeric(weight.max),
				  cigarettes = cigarettes, pipe = pipe, 
				  cigars = cigars, chew = chew,
				  nhanes.cycle = 0,
				  psu = factor(paste('iii', psu)), 
				  stratum = factor(paste('iii ', stratum)), 
				  sample.weight = as.numeric(sample.weight))
})

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
data_adult$smoker = data_adult$cigarettes | data_adult$pipe | data_adult$cigars | data_adult$chew

data_adult = data_adult %>% select(-c(education.yrs, cigarettes, pipe, cigars, chew))


"
Variables of interest in `exam.dat`
1-5:	SEQN, Respondent identification number
1508-13:BMPWT, Weight (kg)
1528-32:BMPHT, Standing height (cm)
1524-7:BMPBMI, Body mass index
"

system.time({
	data_exam = read_fwf('NHANES III/exam.dat', 
						 fwf_positions(c(1, 1508, 1528, 1524, 1533), 
						 			  c(5,  1513, 1532, 1527, 1533), 
						 	col_names = c('id', 'weight', 'height', 'bmi', 'dummy')),
						 n_max = -1) %>%
		transmute(id = as.numeric(id), 
				  weight = as.numeric(weight), 
				  height = as.numeric(height), 
				  bmi = as.numeric(bmi))
})
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
## Col. 1-5: 	NHANES Respondent Sequence Number
## Col. 16:		Final Morality Status
##	0: Assumed alive
##	1: Assumed deceased
##  Blank: Ineligible or under age 18
## Col. 47-9:	Person Months of Follow-up from MEC/Exam Date
## 
## In the legacy data file:
## Variables of interest:
## Col. 1-5:	NHANES Respondent Sequence Number
## Col. 7: 		Final Mortality Status

system.time({
	data_mort = read_fwf('mortality/NHANES_III_MORT_2011_PUBLIC.dat', 
						 fwf_positions(c(1, 16, 17), 
						 			   c(5, 16, 17), 
						 			  col_names = c('id', 'mort.status', 'dummy')),
						 n_max = -1) %>%
		transmute(id = as.numeric(id), 
				  mort.status = factor(mort.status, labels = c('alive', 'deceased')))
})

dataf_iii = left_join(data_adult, data_exam) %>%
	left_join(data_mort) %>% 
	mutate(bmi.max = pmax(weight.max / (height/100)**2, bmi))



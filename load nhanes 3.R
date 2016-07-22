library(dplyr)

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

data_adult = data.frame(id = c(), sex = c(), age.months = c(), 
						race.ethnicity = c(), 
						education.yrs = c(),
						psu = c(),
						stratum = c(),
						sample.weight = c(),
						weight.max = c(), 
						cigarettes = c(), 
						pipe = c(), 
						cigars = c(), 
						chew = c())
unparsed = readLines('NHANES III/adult.dat')
unparsed = strsplit(unparsed, '\n')
paste(length(unparsed), 'entries in adult.dat')
for (entry in unparsed) {
	print(nrow(data_adult))
	this_id = substr(entry, 1, 5)
	this_sex = substr(entry, 15, 15)
	this_age = substr(entry, 1237, 1240)
	this_race.ethnicity = substr(entry, 12, 12)
	this_education = substr(entry, 1256, 1257)
	this_psu = substr(entry, 43, 43)
	this_stratum = substr(entry, 44, 45)
	this_sample.weight = substr(entry, 61, 69)
	this_weight.max = substr(entry, 1961, 1963)
	this_cigarettes = substr(entry, 2281, 2281)
	this_pipe = substr(entry, 2334, 2334)
	this_cigars = substr(entry, 2330, 2330)
	this_chew = substr(entry, 2311, 2311)
	new_line = data.frame(id = this_id, sex = this_sex, 
						  age.months = this_age, 
						  race.ethnicity = this_race.ethnicity, 
						  education.yrs = this_education, 
						  psu = this_psu, 
						  stratum = this_stratum,
						  sample.weight = this_sample.weight, 
						  weight.max = this_weight.max, 
						  cigarettes = this_cigarettes, 
						  pipe = this_pipe, 
						  cigars = this_cigars, 
						  chew = this_chew)
	data_adult = rbind(data_adult, new_line)
}
data_adult$id = as.numeric(as.character(data_adult$id))
data_adult$sex = factor(as.numeric(data_adult$sex), 
						labels = c('male', 'female'))
data_adult$age.months = as.numeric(as.character(data_adult$age.months))
data_adult$race.ethnicity = factor(as.numeric(data_adult$race.ethnicity), 
									  labels = c('Non-Hispanic White',
									  			'Non-Hispanic Black',
									  			'Mexican-American',
									  			'Other'))
code_ed.yrs = function (ed.yrs) {
	ed.yrs = as.numeric(as.character(ed.yrs))
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
data_adult$psu = factor(paste('iii', data_adult$psu))
data_adult$stratum = factor(paste('iii', data_adult$stratum))

data_adult$sample.weight = as.numeric(as.character(data_adult$sample.weight))
catch_error_values = function (x) {
	if (x == 888 | x == 999 | is.na(x)) {
		return(NA)
	} else {
		return(x)
	}
}
data_adult$weight.max = as.numeric(as.character(data_adult$weight.max))
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



"
Variables of interest in `exam.dat`
1-5:	SEQN, Respondent identification number
1508-13:BMPWT, Weight (kg)
1528-32:BMPHT, Standing height (cm)
1524-7:BMPBMI, Body mass index
"

data_exam = data.frame(id = c(), 
					   weight = c(), height = c(), bmi = c())
unparsed = readLines('NHANES III/exam.dat')
unparsed = strsplit(unparsed, '\n')
paste(length(unparsed), 'entries in exam.dat')
for (entry in unparsed) {
	print(nrow(data_exam))
	this_id = substr(entry, 1, 5)
	this_weight = substr(entry, 1508, 1513)
	this_height = substr(entry, 1528, 1532)
	this_bmi = substr(entry, 1524, 1527)
	
	new_line = data.frame(id = this_id, weight = this_weight, 
						  height = this_height, bmi = this_bmi)
	data_exam = rbind(data_exam, new_line)
}
data_exam$id = as.numeric(as.character(data_exam$id))

data_exam$weight = as.numeric(as.character(data_exam$weight))
data_exam$weight = sapply(data_exam$weight, catch_error_values)
data_exam$height = as.numeric(as.character(data_exam$height))
data_exam$height = sapply(data_exam$height, catch_error_values)
data_exam$bmi = as.numeric(as.character(data_exam$bmi))


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

data_mort_file = 'data_mort_III.Rdata'
if (file.exists(data_mort_file)) {
	## The parsing code in the other branch is slow, so we save parsed data
	load(data_mort_file)
} else {
	## For the most recent data release
	files = 'mortality/NHANES_III_MORT_2011_PUBLIC.dat'
	## For the legacy data release
	# files = paste('mortality 2006/NHANES',
	# 			  c('99_00', '01_02', '03_04'),
	# 			  '_MORT_PUBLIC_USE_2010.DAT', sep = '')
	data_mort = data.frame(id = numeric(), mort.status = numeric(), 
						   followup.m = numeric())
	for (file in files) {
		unparsed = readLines(file)
		unparsed = strsplit(unparsed, '\n')
		print(paste(length(unparsed), 'entries in mortality data'))
		for (entry in unparsed) {
			print(nrow(data_mort))
			this_id = substr(entry, 1, 5)
			## In the current release
			this_mort_status = substr(entry, 16, 16)
			## In the legacy data
			#this_mort_status = substr(entry, 7, 7)
			## In the current release
			this_followup = substr(entry, 47, 49)
			## In the legacy data
			#this_followup = substr(entry, 13, 15)
			new_line = data.frame(id = as.numeric(this_id), 
								  mort.status = as.numeric(this_mort_status), 
								  followup.m = as.numeric(this_followup))
			data_mort = rbind(data_mort, new_line)
			#print(new_line)
		}
	}
	data_mort$mort.status = factor(data_mort$mort.status, 
								   labels = c('alive', 'deceased'))
	save(data_mort, file = data_mort_file)
}


dataf = inner_join(data_adult, data_exam) %>%
	left_join(data_mort) %>%
	mutate(bmi.max = pmax(weight.max / (height/100)**2, bmi))

#TODO: combine with continuous NHANES


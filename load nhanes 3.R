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
"

data_adult = data.frame(id = c(), sex = c(), age.months = c(), 
						race.ethnicity = c(), 
						education.yrs = c(),
						psu = c()
						stratum = c()
						sample.weight = c()
						weight.max = c())
unparsed = readLines('NHANES III/adult.dat')
unparsed = strsplit(unparsed, '\n')
for (entry in unparsed) {
	this_id = strsplit(entry, 1, 5)
	this_sex = strsplit(entry, 15, 15)
	this_age = strsplit(entry, 1237, 1240)
	this_race.ethnicity = strsplit(entry, 12)
	this_education = strsplit(entry, 1256, 1257)
	this_psu = strsplit(entry, 43, 43)
	this_stratum = strsplit(entry, 44, 45)
	this_sample.weight = strsplit(entry, 61, 69)
	this_weight.max = strsplit(entry, 1961, 1963)
	new_line = data.frame(id = this_id, sex = this_sex, 
						  age.months = this_age, 
						  race.ethnicity = this_race.ethnicity, 
						  education.yrs = this_education, 
						  psu = this_psu, 
						  stratum = this_stratum,
						  sample.weight = this_sample.weight, 
						  weight.max = this_weight.max)
	data_adult = rbind(data_adult, new_line)
}
data_adult$id = as.numeric(data_adult$id)
data_adult$sex = factor(as.numeric(data_adult$sex), 
						labels = c('male', 'female'))
data_adult$age.months = as.numeric(data_adult$age.months)
data_adult$race.ethnicity = as.factor(as.numeric(data_adult$race.ethnicity), 
									  labels = c('Non-Hispanic White',
									  			'Non-Hispanic Black',
									  			'Mexican-American',
									  			'Other'))
code_ed.yrs = function (ed.yrs) {
	ed.yrs = as.numeric(ed.yrs)
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
# TODO: convert psu, stratum to numerics; add a column to indicate NHANES III; 
#	interact psu w/ stratum so that design doesn't confuse the two surveys
# TODO: convert max weight


"
Variables of interest in `exam.dat`
1-5:	SEQN, Respondent identification number
4518:	MYPB3, Have you smoked 100+ cigarettes in life
	1: Yes
	2: No
	8: Blank but applicable
	9: Don't know
4542:	MYPB17, Used 5+ containers chew tobacco or snuff
	1: Yes
	2: No
	8: Blank but applicable
- [no questions about pipes or cigars]
1508-13:BMPWT, Weight (kg)
1528-32:BMPHT, Standing height (cm)
1524-7:BMPBMI, Body mass index
"


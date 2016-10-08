#library(cowplot)
library(dplyr)
library(foreign)
library(readr)
library(reshape2)
library(lubridate)

source('load nhanes 3.R')

## TODO: https://cran.r-project.org/web/packages/nhanesA/vignettes/Introducing_nhanesA.html

suffixes = c('', '_B', '_C')

## Demographic Variables & Sample Weights
## http://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics
## Variables of interest: 
## SEQN: 	Respondent sequence number
## RIAGENDR:Gender of the sample person
##	1: Male [sic]
##	2: Female [sic]
## RIDAGEEX:Best age in months at date of examination for individuals under 80 years of age at the time of MEC exam.
##	NB It's not clear whether Stokes 2014 used age in months or integer years
## RIDRETH1:Race/Ethnicity
##  1:	Mexican American
##  2:	Other Hispanic
##	3:	Non-Hispanic White
##	4:	Non-Hispanic Black
##	5:	Other Rce - Including Multi-Racial
## DMDEDUC2:Education Level - Adults 20+
##	NB It's not clear which variable Stokes 2014 used to control for education
##	1:	Less than 9th Grade
##	2:	9th-11th Grade
##	3:	High School Grad/GED or Equivalent
##	4:	Some College or AA Degree
##	5:	College Graduate or Above
##	7:	Refused
##	9:	Don't Know
## RIDEXMON: Six month time period when examination was performed
##	1:	November 1 through April 30
##	2:	May 1 through October 31
## SDDSRVYR:Data Release Number
##	1:	1999-2000
##	2:	2001-2002
##	3:	2003-2004
## WTMEC4YR:4-Year Sample Weights
## WTMEC2YR:2-Year Sample Weights
files = paste('NHANES/DEMO', suffixes, '.XPT', sep = '')
data_demo = data.frame(id = c(), sex = c(), age.months = c(), race.ethnicity = c())
for (file in files) {
	readdata = read.xport(file)
	readdata = readdata %>%
		transmute(id = SEQN, 
				  sex = factor(RIAGENDR, labels = c('male', 'female')),
				  age.months = RIDAGEEX,
				  age.years = (age.months - 12) %/% 12,
				  race.ethnicity = factor(RIDRETH1, 
				  						labels = c('Hispanic',
				  								   'Hispanic', 
				  								   'Non-Hispanic White', 
				  								   'Non-Hispanic Black', 
				  								   'Non-Hispanic Other')),
				  education = factor(DMDEDUC2, 
				  				   labels = c('Less than High School', 
				  				   		   		'Some High School', 
				  				   		   		'High School', 
				  				   		   		'Some College',
				  				   		   		'College',
				  				   		   		NA, 
				  				   		   		NA), ordered = FALSE),
				  nhanes.cycle = SDDSRVYR,
				  psu = SDMVPSU,
				  stratum = SDMVSTRA,
				  ## Combined sample weight instructions here:  
				  ##  https://www.cdc.gov/nchs/tutorials/NHANES/SurveyDesign/Weighting/Task2.htm
				  ##  Search in page for "6 years of data from 1999-2004"
				  #WTMEC4YR = WTMEC4YR, 
				  #WTMEC2YR = WTMEC2YR,
				  sample.weight = ifelse(SDDSRVYR %in% c(1,2), 
						  		  	2/3 * WTMEC4YR, 
						  		  ifelse(SDDSRVYR %in% c(3), 
						  		  	1/3 * WTMEC2YR, 
						  		  NA))
		)
	data_demo = rbind(data_demo, readdata)
}
data_demo$race.ethnicity = relevel(data_demo$race.ethnicity, 'Non-Hispanic White')
data_demo$education = relevel(data_demo$education, 'High School')


## Smoking Data
## http://wwwn.cdc.gov/Nchs/Nhanes/Search/DataPage.aspx?Component=Questionnaire
## Variables of interest: 
## SEQN: 	Respondent sequence number
## SMQ020: 	Smoked at least 100 cigarettes in life
## SMQ120: 	Smoked a pipe at least 20 times in life
## SMQ150: 	Smoked cigars at least 20 times in life
## SMQ180:  Used snuff at least 20 times in life
## SMQ210:	Used chewing tobacco 20 times in life
##	NB Stokes 2014 doesn't identify exactly which columns were used to define non-smokers
##	1: Yes
##	2: No
##	7: Refused
##	9: Don't know
files = paste('NHANES/SMQ', suffixes, '.XPT', sep = '')
data_smq = data.frame(id = c(), smoker = c())
for (file in files) {
	readdata = read.xport(file)
	readdata = readdata %>%
		transmute(id = SEQN, cigarettes = SMQ020, pipe = SMQ120, 
				  cigars = SMQ150, snuff = SMQ180, chew = SMQ210)
	readdata[readdata == 7] = NA
	readdata[readdata == 9] = NA
	readdata = readdata %>%
		transmute(id = id, smoker = (cigarettes == 1) #| 
				  					# (pipe == 1) | 
				  					# (cigars == 1) | 
				  					# (snuff == 1) |
				  					# (chew == 1)
				  )
	data_smq = rbind(data_smq, readdata)
}



## Body Measures
## http://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Examination
## Variables of interest: 
## SEQN: 	Respondent sequence number
## BMXWT:	Weight (kg)
## BMXHT:	Standing Height (cm)
## BMXBMI:	Body Mass Index (kg/m**2)
files = paste('NHANES/BMX', suffixes, '.XPT', sep = '')
data_bmx = data.frame(id = c(), weight = c(), height = c(), bmi = c())
for (file in files) {
	readdata = read.xport(file)
	readdata = readdata %>%
		transmute(id = SEQN, weight = BMXWT, height = BMXHT, bmi = BMXBMI)
	data_bmx = rbind(data_bmx, readdata)
}


## Weight History
## http://wwwn.cdc.gov/Nchs/Nhanes/Search/DataPage.aspx?Component=Questionnaire
## Variables of interest:
## SEQN:	Respondent sequence number
## WHD140:	Self-reported greatest weight (pounds)
files = paste('NHANES/WHQ', suffixes, '.XPT', sep = '')
data_whq = data.frame(id = c(), weight.max = c())
for (file in files) {
	readdata = read.xport(file)
	## Catch and replace two error codes
	readdata[readdata == 7777] = NA
	readdata[readdata == 77777] = NA
	readdata[readdata == 9999] = NA
	readdata[readdata == 99999] = NA
	readdata = readdata %>%
		transmute(id = SEQN, weight.max = WHD140 * .454)
	data_whq = rbind(data_whq, readdata)
}


## Public-use Linked Mortality Files
## http://www.cdc.gov/nchs/data_access/data_linkage/mortality/data_files_data_dictionaries.htm

## For the legacy data release
## Col. 1-5:	NHANES Respondent Sequence Number
## Col. 7: 		Final Mortality Status
## Col. 13-5:	Person Months of Follow-up from MEC Exam Date
files = paste('mortality 2006/NHANES',
			  c('99_00', '01_02', '03_04'),
			  '_MORT_PUBLIC_USE_2010.DAT', sep = '')
data_mort_2006 = data.frame(id = c(), mort.status = c())
for (file in files) {
	data_mort_temp = read_fwf(file,  n_max = Inf,
							  fwf_positions(c(1, 7, 13),
							  			    c(5, 7, 15),
							  			  col_names = c('id', 'mort.status', 
							  			  			  'follow.months')), 
							  na = c('.', ' ')) %>%
		transmute(id = as.numeric(id),
				  mort.2006 = factor(mort.status, labels = c('alive', 'deceased')), 
				  follow.months.2006 = as.numeric(follow.months))
	data_mort_2006 = rbind(data_mort_2006, data_mort_temp)
}

## For the most recent data release
## Col. 1-5: 	NHANES Respondent Sequence Number
## Col. 16:		Final Morality Status
##	0: Assumed alive
##	1: Assumed deceased
##  Blank: Ineligible or under age 18
## Col. 47-9:	Person Months of Follow-up from MEC/Exam Date
files = paste('mortality/NHANES_',
			  c('1999_2000', '2001_2002', '2003_2004'),
			  '_MORT_2011_PUBLIC.dat', sep = '')
data_mort_2011 = data.frame(id = c(), mort.status = c())
for (file in files) {
	data_mort_temp = read_fwf(file,  n_max = Inf,
			 fwf_positions(c(1, 16, 47),
			 			   c(5, 16, 49),
			 			  col_names = c('id', 'mort.status', 
			 			  			    'follow.months'))
			) %>%
	transmute(id = as.numeric(id),
			  mort.2011 = factor(mort.status, labels = c('alive', 'deceased')),
			  follow.months.2011 = as.numeric(follow.months))
	data_mort_2011 = rbind(data_mort_2011, data_mort_temp)
}
data_mort = full_join(data_mort_2006, data_mort_2011, by = 'id')

## Catch duplicate mortality results
# data_mort = data_mort %>%
# 	dcast(id + follow.months ~ mort.status) %>%
# 	mutate(mort.status = ifelse(!is.na(deceased),
# 								'deceased',
# 								ifelse(!is.na(alive),
# 									   'alive',
# 									   NA))) %>%
# 	mutate(mort.status = as.factor(mort.status)) %>%
# 	select(id, mort.status, follow.months)


## Combine the datasets
dataf = full_join(data_demo, data_bmx) %>% 
	full_join(data_smq) %>%
	full_join(data_whq) %>%
	## Calculate maximum BMI
	##  NB If observed BMI is greater than recalled, use observed BMI
	mutate(bmi.max = pmax(weight.max / (height/100)**2, bmi)) %>%
	left_join(data_mort)
dataf = dataf %>% mutate(mec.home = NA) %>%
	rbind(dataf_iii)

## Define BMI categories
bmi_breaks = c(0, 18.5, 25, 30, 35, Inf)
bmi_labels = c('underweight', 'normal', 'overweight', 'obese I', 'obese II')

dataf = dataf %>% 
	mutate(bmi.cat = {cut(bmi, breaks = bmi_breaks, labels = bmi_labels) %>%
						C(treatment, base = 2)},
		   bmi.max.cat = {cut(bmi.max, breaks = bmi_breaks, labels = bmi_labels) %>%
		   				C(treatment, base = 2)})

save(dataf, bmi_breaks, file = paste(Sys.Date(), '.Rdata', sep = ''))

# ## No. cases where current BMI category is greater than "maximum" BMI category
#dataf %>% filter(bmi.cat > bmi.max.cat) %>% nrow
# 
# ## Plots of BMI against maximum BMI
# ggplot(data = dataf, aes(x = bmi, y = bmi.max)) + geom_point()
# ggplot(data = {dataf %>% group_by(bmi.cat, bmi.max.cat) %>% summarize(n = n())},
# 	   aes(x = bmi.cat, y = bmi.max.cat, fill = n)) + geom_tile()
# ## ECdataf of difference
#ggplot(data = dataf, aes(bmi.max - bmi, color = bmi.cat)) + stat_ecdataf() + xlim(0, 20)
# 
# ## Histograms
# bmi_hist = ggplot(data = dataf, aes(x = bmi, fill = race.ethnicity)) +
# 	geom_histogram(bins = 100) +
# 	geom_vline(xintercept = bmi_breaks) +
# 	scale_fill_brewer(palette = 'Set1', guide = FALSE)
# bmi_cat_hist = ggplot(data = dataf, aes(x = bmi.cat, fill = race.ethnicity)) +
# 	geom_bar() +
# 	scale_fill_brewer(palette = 'Set1')
# plot_grid(bmi_hist, bmi_cat_hist, labels = 'AUTO', rel_widths = c(1, 1.2))
# 
# 

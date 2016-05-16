library(cowplot)
library(dplyr)
library(foreign)

## TODO: load NHANES III data

suffixes = c('', '_B', '_C')

## Demographic Variables & Sample Weights
## http://wwwn.cdc.gov/nchs/nhanes/search/datapage.aspx?Component=Demographics
## Variables of interest: 
## SEQN: 	Respondent sequence number
## RIAGENDR:Gender of the sample person
##	1: Male [sic]
##	2: Female [sic]
## RIDAGEEX:Best age in months at date of examination for individuals under 80 years of age at the time of MEC exam.
## RIDRETH1:Recode of reported race and ethnicity information.
##	1: Mexican American
##	2: Other Hispanic
##	3: Non-Hispanic White
## 	4: Non-Hispanic Black
##	5: Other Race - Including Multi-Racial
files = paste('NHANES/DEMO', suffixes, '.XPT', sep = '')
data_demo = data.frame(id = c(), sex = c(), age.months = c(), race.ethnicity = c())
for (file in files) {
	readdata = read.xport(file)
	readdata = readdata %>%
		transmute(id = SEQN, 
				  sex = factor(RIAGENDR, labels = c('male', 'female')),
				  age.months = RIDAGEEX,
				  race.ethnicity = factor(RIDRETH1, 
				  						labels = c('Mexican American', 
				  								   'Other Hispanic', 
				  								   'Non-Hispanic White',
				  								   'Non-Hispanic Black', 
				  								   'Other, Multiracial')))
	data_demo = rbind(data_demo, readdata)
}


## Smoking Data
## http://wwwn.cdc.gov/Nchs/Nhanes/Search/DataPage.aspx?Component=Questionnaire
## Variables of interest: 
## SEQN: 	Respondent sequence number
## SMQ020: 	Smoked at least 100 cigarettes in life
##	1: Yes
##	2: No
##	7: Refused
##	9: Don't know
files = paste('NHANES/SMQ', suffixes, '.XPT', sep = '')
data_smq = data.frame(id = c(), smoker = c())
for (file in files) {
	readdata = read.xport(file)
	readdata = readdata %>%
		transmute(id = SEQN, smoker = SMQ020)
	readdata[readdata == 7] = NA
	readdata[readdata == 9] = NA
	data_smq = rbind(data_smq, readdata)
}
data_smq$smoker = data_smq$smoker == 1


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
## Variables of interest:  
## Col. 1-5: 	NHANES Respondent Sequence Number
## Col. 16:		Final Morality Status
##	0: Assumed alive
##	1: Assumed decreased
##  Blank: Ineligible or under age 18
files = paste('mortality/NHANES_', c('1999_2000', '2001_2002', '2003_2004'), 
			  '_MORT_2011_PUBLIC.dat', sep = '')
data_mort = data.frame(id = numeric(), mort.status = numeric())
for (file in files) {
	unparsed = readLines(file)
	unparsed = strsplit(unparsed, '\n')
	for (entry in unparsed) {
		this_id = substr(entry, 1, 5)
		this_mort_status = substr(entry, 16, 16)
		new_line = data.frame(id = as.numeric(this_id), 
							  mort.status = as.numeric(this_mort_status))
		data_mort = rbind(data_mort, new_line)
	}
}
data_mort$mort.status = factor(data_mort$mort.status, 
							   labels = c('alive', 'deceased'))

## Combine the datasets
df = full_join(data_demo, data_bmx) %>% 
	full_join(data_smq) %>%
	full_join(data_whq) %>%
	## Calculate maximum BMI
	mutate(bmi.max = weight.max / (height/100)**2) %>%
	full_join(data_mort)
## Filter down to complete cases
df = df[complete.cases(df), ]
## Filter down to non-smokers between 50 and 85
df = df %>% filter(!smoker, age.months >= 50*12, age.months <= 85*12)

## ----------
## Define BMI categories
bmi_breaks = c(18.5, 25, 30, 35, Inf)
names(bmi_breaks) = c('underweight', 'normal', 'overweight', 'obese I', 'obese II')

bmi_classify = function (bmi) {
	return(names(which(bmi < bmi_breaks))[1])
}

df$bmi.cat = sapply(df$bmi, bmi_classify) %>% factor(levels = names(bmi_breaks), ordered = TRUE)
df$bmi.max.cat = sapply(df$bmi.max, bmi_classify) %>% factor(levels = names(bmi_breaks), ordered = TRUE)

# 
# ## No. cases where current BMI category is greater than "maximum" BMI category
df %>% filter(bmi.cat > bmi.max.cat) %>% nrow
# 
# ## Plots of BMI against maximum BMI
ggplot(data = df, aes(x = bmi, y = bmi.max)) + geom_point()
ggplot(data = {df %>% group_by(bmi.cat, bmi.max.cat) %>% summarize(n = n())},
	   aes(x = bmi.cat, y = bmi.max.cat, fill = n)) + geom_tile()
# 
# ## Histograms
# bmi_hist = ggplot(data = df, aes(x = bmi, fill = race.ethnicity)) +
# 	geom_histogram(bins = 100) +
# 	geom_vline(xintercept = bmi_breaks) +
# 	scale_fill_brewer(palette = 'Set1', guide = FALSE)
# bmi_cat_hist = ggplot(data = df, aes(x = bmi.cat, fill = race.ethnicity)) + 
# 	geom_bar() +
# 	scale_fill_brewer(palette = 'Set1')
# plot_grid(bmi_hist, bmi_cat_hist, labels = 'AUTO', rel_widths = c(1, 1.2))
# 
# 
# ## ----------
ggplot(data = df, aes(x = bmi, y = as.numeric(mort.status) - 1, 
					  color = race.ethnicity, 
					  shape = sex, linetype = sex)) +
	geom_point(position = position_jitter(height = .25), alpha = .1) +
	stat_smooth(alpha = .2) +
	geom_vline(xintercept = bmi_breaks, color = 'grey') +
	scale_y_continuous(name = 'mortality', breaks = c(0,1), 
					   labels = c('alive', 'deceased')) +
	scale_x_continuous(limits = c(NA, 50)) +
	facet_wrap(~ race.ethnicity)
	# stat_smooth(method = 'glm',
	# 			formula = mort.status ~ sex + age.months + race.ethnicity + bmi)

fit1 = glm(mort.status ~ bmi,
		  family = binomial,
		  data = df)
summary(fit1)

fit2 = glm(mort.status ~ bmi + sex + age.months + race.ethnicity, 
		  family = binomial, 
		  data = df)
summary(fit2)

fit3 = glm(mort.status ~ bmi + bmi.max + sex + age.months + race.ethnicity, 
		   family = binomial, 
		   data = df)
summary(fit3)
exp(fit3$coefficients)

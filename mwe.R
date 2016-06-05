library(dplyr)
library(survey)
## https://cran.r-project.org/web/packages/srvyr/vignettes/srvyr-vs-survey.html
library(survival)

datafile = '2016-05-24.Rdata'
if (!file.exists(datafile)) {
	download.file('https://github.com/dhicks/obesity/blob/mwe/2016-05-24.Rdata?raw=true',
				  datafile)
}
load(datafile)

df = df[complete.cases(df),]

## ----------
## W/o sampling weights
df.working = df %>% filter(!smoker, age.months >= 50*12, age.months < 85*12)
coxfit.unweighted = coxph(Surv(age.months, mort.status == 'deceased') ~ 
						  	bmi.cat * bmi.max.cat,
				   data = df.working)
summary(coxfit.unweighted)


## ----------
## W/ sampling weights
## cf the svydesign call in the docs for survey::nhanes
df$bmi.cat.inter = interaction(df$bmi.cat, df$bmi.max.cat, drop = TRUE)
design = svydesign(id = ~ psu, strata = ~ stratum, weights = ~ sample.weight, 
				   nest = TRUE, 
				   data = {df %>% filter(!smoker, age.months >= 50*12, age.months < 85*12)})
coxfit.weighted = svycoxph(Surv(age.months, mort.status == 'deceased') ~ 
						   	bmi.cat.inter, 
				 design = design)
summary(coxfit.weighted)

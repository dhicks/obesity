## Data ##

We used data from the National Health and Nutrition Examination Survey (NHANES), an ongoing health survey conducted by the US National Institutes of Health.  NHANES collects clinical, behavioral, demographic, and socioeconomic data on respondents across the United States; is anonymously linked to mortality data curated by the Centers for Disease Control and Prevention (CDC); uses a complex survey design to permit better statistical inferences about both the US population as a whole and smaller subpopulations (e.g., racial and ethnic minorities); and is freely available to the public.  For these reasons, NHANES is widely used in epidemiology and public health.  For example, the original "obesity paradox" paper by Flegal et al *[cite] is based on NHANES data.  

NHANES was initially conducted as a series of independent surveys; NHANES III, the third such survey, was conducted from 1988-1994.  Beginning in 1999, NHANES moved to a continuous cycle format, with new data released every two years. *[https://wwwn.cdc.gov/nchs/nhanes/Default.aspx]  Here we use NHANES III and cycles 1-3 of continuous NHANES, covering 1999-2004.  Followup mortality data were collected in 2011 *[https://www.cdc.gov/nchs/data-linkage/mortality-public.htm].  Both NHANES III and continuous NHANES use a complex survey design that oversamples "black and Mexican-American," to allow reliable inferences about these minority groups  *[https://wwwn.cdc.gov/nchs/data/series/sr02_113.pdf 1].  Analyzing these complex survey data requires specific methods *[survey package]; these methods are standard in the epidemiology literature, and we used them as well in all of our analyses here.  

Following studies in the "obesity paradox" literature *[cites], we focus our analysis on the association between BMI and all-cause mortality, incorporating several social and demographic variables into our models as controls: dichotomous sex, race/ethnicity *[note probs w/ cite to Sean], education level.  For the Cox proportional hazards model, age at followup was used as survival time (see below); for the other models, age at followup was included in the covariates as another control.  

Further, also following the lead of studies in the "obesity paradox" literature, we filter the NHANES sample, including only individuals who (1) report never smoking cigarettes, (2) *[have BMI less than something, greater than something?], *[and?] (3) were at least *[n] years old when they participated in NHANES and were no more than *[m] years old when their mortality status (i.e., whether they were dead or alive) was determined in 2011.  

All together, the dataset we use includes *[n] individuals

Clearly both the choice of variables and the use of filtering criteria introduce what we might call *data collection uncertainty*.  Note that this uncertainty is not due to limited precision in measurement — that is, *measurement uncertainty* — or questions about the extent to which the sample accurately represents a target population — *inductive uncertainty*.  These three kinds of uncertainty are also significant and worth further analysis; however, since our focus is on model-related uncertainty we do not discuss them further here.  

## Models ##

We focus here on three sources of model uncertainty, which we call *covariate specification*, *model specification*, and *model selection*.  To examine the effects of these sources of uncertainty, we systematically consider several variations under each source.  These variations are summarized in table 1.  

| **Covariate specification**  | **Model Specification** | **Model Selection** |
|:-----------------------------|:------------------------|:--------------------|
| Continuous BMI               | Linear
| Binned or discrete BMI       | Logistic
| 4-knot spline                | Poisson
| 6-knot spline                | Cox PH
|:-----------------------------|:------------------------|
Table 1: Variations used for each source of model uncertainty.  The columns are read independently, giving a total of 16 models (for the first two columns) that are evaluated using 4 different model selection statistics.  

### Covariate Specification ###

By *covariate specification*, we refer to the way the independent variables in a statistical model are specified.  Consider a seemingly-simple univariate linear regression model:  *[match symbology]

Y = \beta X + \beta_0

Even given this model specification, there may be several different ways of specifying the independent variable or covariate $X$.  These different specifications of $X$ may lead to different conclusions about the relationship between $X$ and $Y$.  

In the obesity literature, it is common to use a binned or discretized version of BMI.  Since BMI is the quotient of two continuous-valued variables, it is itself continuous-valued.  But this continuous value is conventionally divided into several bins, shown in table 2.  Thus, our first two variations for covariate specification are (1) continuous BMI and (2) discrete BMI.  

| BMI bin name | BMI range    |
|:-------------|:-------------|
| underweight  | $<$ 18.5     |
| normal weight| 18.5 - 25    |
| overweight   | 25 - 30      |
| obese I      | 30 - 35      |
| obese II     | $>$ 35       |
|--------------|--------------|
Table 2: Conventional bins for BMI values 

The use of discrete BMI may strike some readers as an implausible oversimplification.  While individuals with BMI 25 (on the low end of the "overweight" bin) may be similar to individuals with BMI 30 (on the high end of the "overweight" bin), it is plausible that they are more similar to individuals with BMI 24 (in the "normal weight" bin).  These cutoffs coincide very neatly with our base-10 number system, which may make us suspect that they do not correspond to any real biological differences.  

However, the use of discrete BMI can be justified by a suspicion that the relationship between BMI and mortality is not linear.  In the univariate linear model above, increasing the value of $X$ by one unit corresponds to an increase in $Y$ of $\beta$ units.  This is the case whether the starting value of $X$ is large or small.  In terms of BMI, increasing BMI from 19 to 23 would have exactly the same effect as increasing BMI from 33 to 37.  But it is plausible that these two changes in BMI might have different effects.  With discrete BMI, we can estimate effects relative to a baseline level (in this case, "normal weight") without assuming a linear relationship.  This approach will not let us make claims about changes in continuous BMI, e.g., the effect of a decrease from 37 to 33.  But it will let us make claims about whether, say, "obese II" individuals are at greater or lower risk than "normal weight" individuals.  

*[splines allow us to treat BMI as a continuous variable while also allowing the relationship to vary non-linearly]

## Model Specification ##

By *model specification*, we refer to the mathematical structure of the statistical model other than the covariate specification.  One of the most common statistical model specifications is the standard linear regression, where the response $y$ is represented as a linear combination of the covariates $x_1, x_2, \ldots$:  

\[ y = \beta_0 + \beta_1 x_1 + \beta_2 x_2 + \cdots.\]

However, this model specification does not fit easily with our response variable $y$, all-cause mortality.  $y$ can take exactly two values:  alive or dead.  These two values can be represented numerically as 0 or 1.  But predicted values $\hat y$ — estimates of $y$ produced using the model — are not limited to these two values.  When the predicted values of $\hat y$ are between 0 and 1, they might be interpreted as probabilities — for example, a predicted value $\hat y = .25$ can be interpreted as a 25% chance of being dead.  But, even then, the standard linear regression does not place bounds on the range of $\hat y$.  So for sufficient large (positive or negative) values of the covariates, $\hat y$ will be less than 0 or greater than 1.  Obviously these values cannot be interpreted as probabilities.  

*Generalized linear models* were developed to build models that better fit response variables like our $y$.  The observed values of $y$ are modeled as samples drawn from some probability distribution $\mathbf{Y}$.  For example, our observations of $y$ — alive or dead — might be modeled as a series of Bernoulli trials.  Informally, a Bernoulli trial corresponds to a single flip of a biased coin; the bias $\theta$ is the probability that the coin lands heads.  That is, we might model the series of observed values of $y$ as samples drawn from a Bernoulli distribution $\mathbf{Y}$ with unknown bias $\theta$.  Conceptually, this fits all-cause mortality much better than the continous, unbounded response variable in the standard linear regression.  (Since a series of Bernoulli trials follows the binomial distribution, this way of modeling the response is often called *binomial regression*.)  

To connect the distribution $\mathbf{Y}$ to the covariates $x_1, \ldots$, we assume the existence of an invertible *link function* $g$:  

\[ E[\mathbf{Y}] = g^{-1}(\beta_0 + \beta_1 x_1 + \beta_2 x_2 + \cdots)\]

where $E[\mathbf{Y}]$ is the expected value of $\mathbf{Y}$.  For the Bernoulli distribution with bias $\theta$, $E[\mathbf{Y}] = \theta$.  So the covariates do not directly predict the observed values of the response variable $y$.  Instead, they predict a key property of the distribution that generates those observations — namely, the unobserved bias in the "coin flip."  

For binomial regression, a standard link function is the logit function

\[ g(p) = \log\frac{p}{1-p}. \]

The inverse $g^{-1}(x) = \frac{e^x}{1+e^x}$ is called the logistic function; so a binomial regression with a logit link is typically called simply *logistic regression*.  Note that the logit takes a probability $p$ to the logarithm of its odds, or log-odds.  So logistic regression connects the covariates to the log-odds of the bias $\theta$ of the "coin flips" generating the observed response values $y$.  Further, 

\begin{align} 
    odds(\theta) &= e^{\beta_0 + \beta_1 x_1 + \beta_2 x_2 + \cdots\\
        &= e^{\beta_0} \cdot e^{\beta_1 x_1} \cdot e^{\beta_2 x_2} \cdots\\
        &= e^{\beta_0} \cdot (e^{\beta_1})^{x_1} \cdot (e^{\beta_2})^{x_2} \cdots.
\end{align}

In the standard linear regression, for a given covariate, the ceteris paribus effect of that covariate (that is, keeping the values of all other covariates fixed) is linear.  And these effects are added together to predict the response.  By contrast, in logistic regression, the ceteris paribus effect of a covariate is exponential, and these effects are multiplied together to predict the response.  This means that the coefficients in the two model specifications — the $\beta$s — are incommensurable.  We cannot compare $\beta_1$ in a standard linear regression to $\beta_1$ in a logistic regression and ask whether their values are "the same" or "different."  

The first two model specifications that we consider here are a standard linear regression and logistic regression.  Logistic regression clearly has a much better conceptual fit; but in actual scientific practice standard linear regressions are widely used even when logistic regressions provide a better conceptual fit.  *[example?]

Our response variable — alive or dead — can also be represented as a discrete count variable — the "number of deaths" that occur for the given individual between their participation in NHANES and the CDC mortality followup.  The Poisson distribution is commonly used to represent discrete count variables, and so could be used to construct an alternative generalized linear model.  The Poisson distribution is described by a single positive real-valued parameter $\lambda$, gives the expected value, i.e., the expected number of events.  The natural logarithm is the standard link function for a Poisson regression.  This gives us our third model specification.  

Our fourth model specification takes a conceptually different approach to the response variable.  Rather than treating it as a binary alive or dead outcome, *survival analysis* treats the response variable as *survival time*, or the amount of time between the beginning of the study and an event (namely, death).  More precisely, survival analysis is interested in modeling a hazard function $\lambda(t)$, which gives the probability of an event occurring at time $t$.  In the context of a regression, we model the hazard function in terms of covariates $x_1, x_2, \ldots$ and a baseline hazard function $\lambda_0$:  

\[ \lambda(t|x_1, x_2, \ldots) = \lambda_0(t) e^{\beta_1 x_1 + \beta_2 x_2 + \cdots}.\]

Different approaches to hazard analysis use different methods to estimate the baseline hazard function $\lambda_0$.  The statistician David Cox developed an important method, now called *Cox proportional hazards regression* or sometimes simply *Cox regression*, that avoids estimating the baseline hazard function, and instead focuses on estimating the relative hazard or risk:  

\[\frac{\lambda(t | x_1, x_2, \ldots)}{\lambda_0(t)} = e^{\beta_1 x_1 + \beta_2 x_2 + \cdots}.\]

Here the response variable is not the hazard at time $t$, but instead the hazard at time $t$ for one group of individuals (characterized by the particular values of the covariates) relative to the baseline group (when all of the covariates are equal to 0).  Cox proportional hazards is widely used in epidemiology.  Indeed, *[cites to obesity paradox literature]

All together, with four ways of specifying the key covariate, BMI, and four ways of specifying the other features of the statistical model, we have 16 distinct regression models of the relationship between BMI and all-cause mortality.  

## Model Selection ##

We compare these 16 models using four different *model selection* or goodness-of-fit statistics.  *[ref to disc of accuracy in model-based science section]



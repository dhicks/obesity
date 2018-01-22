# Model Construction #

Our primary argument for structural uncertainty in statistical modeling is empirical:  we give an example of this uncertainty in a realistic case study.  Specifically, in this section we construct regression models of the health impacts of obesity.  By identifying distinct sources of model uncertainty, we construct a total of *[16]* different models, which can be evaluated (for model selection) using *[4]* different fidelity statistics.  We use real data, from a survey that is widely used by epidemiologists and public health officials; and — other than constructing multiple models — we use standard and recommended statistical methods.  In the next section, we show how these models give disparate indications of the health impacts of obesity.  

## Data ##

We used data from the National Health and Nutrition Examination Survey (NHANES), an ongoing health survey conducted by the US National Institutes of Health *[cite]*.  NHANES collects clinical, behavioral, demographic, and socioeconomic data on respondents across the United States; is anonymously linked to mortality data curated by the Centers for Disease Control and Prevention (CDC); uses a complex survey design to permit better statistical inferences about both the US population as a whole and smaller subpopulations (e.g., racial and ethnic minorities); and is freely available to the public.  For these reasons, NHANES is widely used in epidemiology and public health.  For example, the original "obesity paradox" paper by Flegal et al *[cite] is based on NHANES data.  

NHANES was initially conducted as a series of independent surveys; NHANES III, the third such survey, was conducted from 1988-1994.  Beginning in 1999, NHANES moved to a continuous cycle format, with new data released every two years. *[https://wwwn.cdc.gov/nchs/nhanes/Default.aspx]  Here we use NHANES III and cycles 1-3 of continuous NHANES, covering 1999-2004.  Followup mortality data were collected in 2011 *[https://www.cdc.gov/nchs/data-linkage/mortality-public.htm].  Both NHANES III and continuous NHANES use a complex survey design that oversamples "black and Mexican-American" people, to allow reliable inferences about these minority groups  *[https://wwwn.cdc.gov/nchs/data/series/sr02_113.pdf 1; @Lumley:Complex].  

Following studies in the "obesity paradox" literature *[cites], we focus our analysis on the association between BMI and all-cause mortality, incorporating several social and demographic variables into our models as controls: dichotomous sex, race/ethnicity *[note probs w/ cite to Sean], education level.  Individuals were included if they were between 50 and 85 years old when they participated in NHANES, and "aged out" of the study at 85.  This means that we consider a dead individual to be "alive" so long as they died after their 85th birthday.  For the Cox proportional hazards model (see below), age at followup (either age at death or age in 2011) was used as survival time; for the other models, age at followup was included in the covariates as another control.  

This age filtering introduces a potential for bias in several of our models.  To understand this bias, consider an overweight individual who was 75 and alive during followup in 2011, but who died in 2012.  Incorrectly treating this individual as alive will tend to make the estimates for the overweight group slightly more optimistic than the true value.  This phenomenon is called \technical{censoring}; while survival models (such as the Cox proportional hazards model) are designed to take it into account, the other models we use are not.  However, as we discuss below, survival models are also designed around a different kind of endpoint or response — relative survival time rather than whether an individual is alive or dead — which may make them inappropriate for certain studies.  Censoring can be avoided using a different study design.  For example, we might include only individuals who were 50 years old when they participated in NHANES and would have been at least 85 in 2011.  While *[our analysis file] can construct and analyze this kind of \technical{cohort sample}, using NHANES data it produces small samples and highly uncertain estimates.  This is NHANES is only a few decades old — not many participants have had a chance to age from 50 to 85.  Censoring means that our generalized linear models also have *[x] uncertainty — we are uncertain whether or to what extent the estimates produced by these models are biased.  

We apply additional filtering to the NHANES sample, again following the lead of studies in the "obesity paradox" literature.  Specifically, we include only individuals who (1) report never smoking cigarettes, (2) have BMI less than 75 (for a person 5'6"/1.67m tall, this corresponds to a cutoff of about 460 lbs/210 kg), (3) survived at least one month after participating in NHANES, and (5) have data for BMI, education level, and followup mortality status.  All together, the dataset we use includes 5,677 individuals.  Summary statistics of this dataset are included in the *[supplemental file].  

Clearly, both the choice of variables and the use of filtering criteria introduce what we might call \technical{data collection uncertainty}.  Note that this uncertainty is not due to limited precision in measurement — that is, \technical{measurement uncertainty} — or questions about the extent to which the sample accurately represents a target population — \technical{inductive uncertainty}.  These three kinds of uncertainty are also significant and worth further analysis; however, since our focus is on model-related uncertainty we do not discuss them further here.  

## Models ##

We focus here on three sources of model uncertainty, which we call \technical{covariate specification}, \technical{model specification}, and \technical{predictive fidelity}.  To examine the effects of these sources of uncertainty, we systematically consider several variations under each source.  *[These variations are summarized in table 1.] 

| **Covariate Specification**  | **Model Specification** | **Predictive Fidelity** |
|:-----------------------------|:------------------------|:--------------------|
| Binned or discrete BMI       | Linear                  | Accuracy            |
| Continuous BMI               | Logistic                | Precision           |
| Square BMI                   | Poisson                 | Recall              |
| 4-knot spline                | Cox PH                  | $F_1$               |
||                                                       | AIC                 |
||                                                       | AUROC               |

Table 1: Variations used for each source of model uncertainty.  The columns are read independently, giving a total of 16 models (for the first two columns) that are evaluated using 6 different predictive fidelity statistics.  

Before discussing these sources of uncertainty, note that we do not consider interactions between covariates, which are another source of structural uncertainty for statistical models.  In the context of human genetics, James Tabery argues that gene-environment interactions are highly variable:  they are substantial in some contexts, but not others *[cite].  From a statistical model-building perspective, including interactions will generally increase predictive fidelity, but may or may not be generatively faithful.  Since our case study uses one key predictor (BMI), and the other covariates are regarded as control variables, we do not consider any interactions here.  However, interactions would be highly appropriate if and insofar as we suspect, for example, that the effect of BMI on all-cause mortality depends on sex, race, or socio-economic status.  *[In our examination of the "obesity paradox" literature, we found that statistical models generally did not include interactions.]  

### Covariate Specification ###

By \technical{covariate specification}, we refer to the way the independent variables in a statistical model are specified.  Consider a seemingly-simple univariate linear regression model:  

$$ Y = \beta X + \beta_0 $$

Even given this model specification, there may be several different ways of specifying the independent variable or covariate $X$.  These different specifications of $X$ may lead to different conclusions about the relationship between $X$ and $Y$.  

In the obesity literature, it is common to use a binned or discretized version of BMI.  Since BMI is the quotient of two continuous-valued variables, it is itself continuous-valued.  But this continuous value is conventionally divided into several bins, shown in table 2.  Thus, our first two variations for covariate specification are (1) continuous BMI and (2) discrete BMI.  

| BMI bin name | BMI range    |
|:-------------|:-------------|
| underweight  | $<$ 18.5     |
| normal weight| 18.5 - 25    |
| overweight   | 25 - 30      |
| obese I      | 30 - 35      |
| obese II     | $>$ 35       |

Table 2: Conventional bins for BMI values 

The use of discrete BMI may strike some readers as an implausible oversimplification.  While individuals with BMI 25 (on the low end of the "overweight" bin) may be similar to individuals with BMI 30 (on the high end of the "overweight" bin), it is plausible that they are more similar to individuals with BMI 24 (in the "normal weight" bin).  These cutoffs coincide very neatly with our base-10 number system, which may make us suspect that they do not correspond to any real biological differences.  That is, we may suspect that discrete BMI has low generative fidelity.  

On the other hand, the use of discrete BMI can be justified by a suspicion that the relationship between BMI and mortality is not linear.  In the univariate linear model above, increasing the value of $X$ by one unit corresponds to an increase in $Y$ of $\beta$ units.  This is the case whether the starting value of $X$ is large or small.  In terms of BMI, increasing BMI from 19 to 23 would have exactly the same effect as increasing BMI from 33 to 37.  But it is plausible that these two changes in BMI might have different effects.  With discrete BMI, we can estimate effects relative to a baseline level (in this case, "normal weight") without assuming a linear relationship.  This approach will not let us make claims about changes in continuous BMI, e.g., the effect of a decrease from 37 to 33.  But it will let us make claims about whether, say, "obese II" individuals are at greater or lower risk than "normal weight" individuals.  This can be seen as a tradeoff between generative and predictive fidelity, or between realism and flexibility *[cite Weisberg]:  continuous BMI is more realistic or generatively faithful; while discrete BMI might be more flexible in ways that facilitate better predictive fidelity.  

Non-linear covariate specifications give us another way to balance generative and predictive fidelity.  One common approach is to add exponentiated terms — squares, cubes, square roots, etc. — of covariates of interest.  Our third covariate specification includes both continuous BMI and its square, $X + X^2$.  

Another family of non-linear covariate specifications uses \technical{splines}, or piecewise polynomials. *[cite]  Suppose a given covariate $X$ takes values in the interval $(0,1)$.  This interval can be divided into $k+1$ sub-intervals by setting $k$ breakpoints or *knots* within this interval:  $t_0 = 0 < t_1 < t_2 < \cdots < t_k < 1 = t_{k+1}$.  Polynomial functions $p_j(x)$ (typically with a fixed degree, say $d=3$ for \technical{cubic splines}) are constructed for each subinterval $[t_j, t_{j+1}]$ such that $p_j(t_{j+1}) = p_{j+1}(t_{j+1})$.  That is, at each knot point $t_j$, the two polynomials agree, ensuring that the combined function over the entire interval $(0,1)$ is continuous.  

Splines allow us to treat BMI as a continuous variable while also allowing the relationship between all-cause mortality and BMI to vary non-linearly.  Our fourth covariate specification uses a 4-knot spline for BMI with cubic splines.  To set the knots, we first estimate the distribution of BMI values for the population (not just the survey sample), then set the knots at the 20th, 40th, 60th, and 80th percentiles for this distribution.  A 4-knot specification roughly corresponds to the structure of conventional discrete BMI:  discrete BMI has 5 bins, with 4 breakpoints.  However, the location of the knots do not necessarily correspond to these breakpoints.  (Indeed, the knots are at about BMI 24, 26, 29, and 33.)  

## Model Specification ##

By \technical{model specification}, we refer to the mathematical structure of the statistical model other than the covariate specification.  Perhaps the most widely-used statistical model specification is the standard linear regression, where the response $Y$ is represented as a linear combination of the covariates:  

$$ Y = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \cdots.$$

However, this model specification does not fit easily with our response variable $Y$, all-cause mortality — that is, there are serious limits to its fidelity to the data-generating process.  $Y$ can take exactly two values:  alive or dead.  These two values can be represented numerically as 0 or 1.  But predicted values $\hat y$ — estimates of the individual state $y$ produced using the model — are not limited to these two values.  When the predicted values $\hat y$ are between 0 and 1, they might be interpreted as probabilities — for example, a predicted value $\hat y = .25$ can be interpreted as a 25% chance of being dead.  But, even then, the standard linear regression does not place bounds on the range of $\hat y$.  So for sufficiently large (positive or negative) values of the covariates, $\hat y$ will be less than 0 or greater than 1.  Obviously these values cannot be interpreted as probabilities.  

*Generalized linear models* were developed to build models that better fit response variables like our $Y$.  The observed values $y$ are modeled as samples drawn from some probability distribution $Y$.  For example, our observations $y$ — alive or dead — might be modeled as a series of Bernoulli trials, that is, single flips of a biased coin; the bias $\theta$ is the probability that the coin lands heads.  (Note that this use of "bias" shouldn't be confused with biased or unbiased estimates.)  That is, we might model the series of observed values $y$ as samples drawn from a Bernoulli distribution $Y$ with unknown bias $\theta$: $\y \sim Y(\theta)$.  In terms of fidelity to the data-generating process, this fits all-cause mortality much better than the continuous, unbounded response variable in the standard linear regression.  (Since a series of Bernoulli trials follows the binomial distribution, this way of modeling the response is often called \technical{binomial regression}.)  

To connect the response distribution $Y$ to the covariates $X$, we use an invertible \technical{link function} $g$:  

$$ E[Y] = g^{-1}(\beta_0 + \beta_1 X_1 + \beta_2 X_2 + \cdots)$$

where $E[Y]$ is the expected value of $Y$.  For the Bernoulli distribution with bias $\theta$, $E[Y] = \theta$; different covariate values correspond to different values of $\theta$ via the link function $g$.  That is, the covariates determine a key property of the distribution that generates the observations — the unobserved bias in the "coin flip."  

For binomial regression, a standard link function is the logit function

$$ g(p) = \log\frac{p}{1-p}. $$

The inverse $g^{-1}(x) = \frac{e^x}{1+e^x}$ is called the logistic function; so a binomial regression with a logit link is often called simply \technical{logistic regression}.  Note that the logit takes a probability $p$ to the logarithm of its odds, or log-odds.  So logistic regression connects the covariates to the log-odds of the bias $\theta$ of the "coin flips" generating the observed response values $y$.  Further, 

\begin{align} 
    odds(\theta) &= e^{\beta_0 + \beta_1 X_1 + \beta_2 X_2 + \cdots\\
        &= e^{\beta_0} \cdot e^{\beta_1 X_1} \cdot e^{\beta_2 X_2} \cdots\\
        &= e^{\beta_0} \cdot (e^{\beta_1})^{X_1} \cdot (e^{\beta_2})^{X_2} \cdots.
\end{align}

In the standard linear regression, for a given covariate, the ceteris paribus effect of that covariate (that is, keeping the values of all other covariates fixed) is linear.  And these effects are added together to predict the response.  By contrast, in logistic regression, the ceteris paribus effect of a covariate is exponential, and these effects are multiplied together to predict the response.  This means that the coefficients in the two model specifications — the $\beta$s — are incommensurable.  We cannot compare $\beta_1$ in a standard linear regression to $\beta_1$ in a logistic regression and ask whether their values are "the same" or "different."  

The first two model specifications that we consider here are a standard linear regression and logistic regression.  Logistic regression clearly is much more faithful to the data-generating process; but in actual scientific practice standard linear regressions are widely used even when logistic regressions would be more faithful.  On the other hand, standard linear regressions are more familiar to researchers and, perhaps for this reason, can be easier for researchers to interpret.  Further, and most importantly for our argument here, using generalized linear regression models introduces further sources of structural uncertainty, as different link functions can be used for a given way of modeling the response.  For example, besides the logit link, binomial regression can be used with the probit link — the probit is the inverse of the cumulative distribution function for the standard Gaussian ("normal") distribution — or the complementary log-log link:  
$$ g(p) = \log(-\log(1-p)) .$$

Our response variable — alive or dead — can also be represented as a discrete count variable — the "number of deaths" that occur for the given individual between their participation in NHANES and the CDC mortality followup.  The Poisson distribution is commonly used to represent discrete count variables, and so can be used to construct an alternative generalized linear model.  The Poisson distribution is described by a single positive real-valued parameter $\lambda$, gives the expected value, i.e., the expected number of events.  The natural logarithm is the standard link function for a \technical{Poisson regression}.  This gives us our third model specification.  

It might be objected that Poisson regression is clearly not faithful to the data-generating process, and so should not be used when we assemble our set of models.  However, biostatistician Guangyong Zhou has argued that a modified version of Poisson regression can be used with binary response variables.  Briefly, Zhou argues that, for a given covariate $X$ and its coefficient $\beta$ in a Poisson regression, the relative risk (difference in the response) associated with an increase in $X$ is simply $e^\beta$.    *[https://academic.oup.com/aje/article/159/7/702/71883/A-Modified-Poisson-Regression-Approach-to; http://journals.sagepub.com/doi/abs/10.1177/0962280211427759]  Zhou's regression is modified by using a different calculation of variance, which changes the standard errors of the coefficients but not their point-estimates.  That is, off-the-shelf Poisson regression incorrectly estimates the parameter uncertainty for Zhou's modified Poisson regression; but gives the same point-estimates.  

Our fourth model specification takes a conceptually different approach to the response variable.  Rather than treating it as a discrete alive or dead outcome, \technical{survival analysis} treats the response variable as \technical{survival time}, or the amount of time it takes for an event to occur to event (in our case, age at death).  More precisely, survival analysis is interested in modeling a hazard function $\lambda(t)$, which gives the probability of an event occurring at time $t$.  In the context of a regression, we model the hazard function in terms of covariates $X_1, X_2, \ldots$ and a baseline hazard function $\lambda_0$:  

$$ \lambda(t) = \lambda_0(t) e^{\beta_1 X_1 + \beta_2 X_2 + \cdots}.$$

Different approaches to hazard analysis use different methods to estimate the baseline hazard function $\lambda_0$; these introduce further structural uncertainty.  To avoid this structural uncertainty, the statistician David Cox developed a method, now called \technical{Cox proportional hazards regression} or sometimes simply \technical{Cox regression}, that instead focuses on estimating the relative hazard or risk:  

$$\frac{\lambda(t)}{\lambda_0(t)} = e^{\beta_1 X_1 + \beta_2 X_2 + \cdots}.$$

Here the response variable is not the hazard at time $t$, but instead the hazard at time $t$ relative to the baseline hazard (i.e., when all of the covariates are equal to 0).  Thus, while the right-hand side of the Cox model is similar to the right-hand sides of the logistic and Poisson regression, coefficients between these models cannot be compared:  for a given covariate $X_1$, it does not make sense to ask whether its coefficient $\beta_1$ in one model is greater, or less, than its coefficient in a different model.  

Cox proportional hazards is widely used in epidemiology.  Indeed, *[cites to obesity paradox literature]

All together, with four ways of specifying the key covariate, BMI, and four ways of specifying the other features of the statistical model, we have 16 distinct regression models of the relationship between BMI and all-cause mortality.  

## Model Selection ##

We compare these 16 models using six different *model selection* or goodness-of-fit statistics:  accuracy, AIC, AUROC, $F_1$, precision, and recall.  Since we have already discussed these fidelity criteria for statistical models above *[xref], we do not discuss them in detail here.  


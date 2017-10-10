# Model Analysis #

In this section, we analyze our 16 models in terms of generative fidelity, by calculating our different predictive fidelity or model selection statistics, and by generating relative risk predictions.  In each case, we argue that there is significant ambiguity or incommensurability.  

## Generative Fidelity ##

In terms of generative fidelity, it is highly plausible that the logistic regression and Cox proportional hazard models are the most faithful to the data-generating process.  As discussed above, linear regression represents binary mortality status as an unbounded, continuous variable; and Poisson regression represents mortality status as a count variable, with no upper bound.  

Because none our models include interactions, the covariates are represented as independent contributors to all-cause mortality.  It is at least reasonable to think that this assumption is suspect, and thus that all of our models have the same limitations on their generative fidelity.  On the other hand, from a model selection perspective, this point does not help us choose between the available models.  

Generative fidelity is much more ambiguous for the different covariate specifications.  It is highly plausible that discrete BMI is the least generatively faithful.  And because it requires a linear or uniform effect, it is plausible that continuous BMI also has low generative fidelity.  This suggests that, among the four available options, the spline specifications have the highest generative fidelity.  But splines are at once both highly complex and highly simple.  On the one hand, they are highly complex, in that specifying splines requires specifying the number and location of knot points and the polynomial degree.  It is plausible that it would be extremely difficult to get good empirical estimates of the correct values for these parameters.  On the other hand, splines are highly simple, in that they are piecewise polynomials.  While polynomials are good at approximating other functions[^dense], and extremely convenient for human- and machine-accessible representations, they are analytically unusual, and in this sense "simple."  Why should we think that the true relationship between BMI and all-cause mortality can be expressed using a "simple" function?  This question points us towards the deep debate over the role of simplicity in scientific reasoning *[cites].  

[^dense]: The Weierstrass approximation theorem states that polynomials are dense in the space of continuous functions on a closed real interval $[a,b]$.  


## Predictive Fidelity ##

In line with our discussion in *[xref], all of the models produce continuous-valued predictions $\hat y$, and calculating accuracy and $F_1$ requires setting a discrimination threshold to map these predictions into the set $\{alive, dead\}$.  Across all models, a threshold $\alpha = .33$ gives approximately the same number of predicted deaths as actually observed deaths.  In what follows we use this threshold for accuracy and $F_1$.  AIC and AUROC do not require choosing this threshold.  

Figure 1 shows the values of each model selection statistic for all 16 models.  In each panel of this plot, points located higher on the y-axis correspond to better scores (note that AIC is reversed) and x-axis position is not meaningful.  *[In the online supplement, Table S1 gives numerical values from this plot in a sortable list.]  

![Figure 1. Predictive fidelity statistics for all 16 models.  In each panel, higher values indicate greater predictive fidelity.](01_pred_fit.png)  

The Cox model performs worst under all four metrics.  For the two accuracy statistics (accuracy itself and AUROC), the other three models perform roughly equally well; covariate specification seems to be more important than model specification, with continuous BMI performing slightly worse by AUROC than the other variables across all three model specifications.  $F_1$ generally finds continuous variables and linear models perform worse than alternatives; logistic and Poisson models with spline variables seem to perform the best, presumably because these models are more flexible for fitting the non-linear relationship between BMI and mortality.  

AIC should be interpreted with care.  For the linear and generalized regression models, the response variable is whether or not the survey respondent died.  On the other hand, for the Cox models, the response is the amount of time the respondent survived until either dying, being lost to followup, or aging out of the study.  So it may not be legitimate to compare the AIC of the Cox model to the AIC of the other three model specifications.  AICs for those other specifications should be comparable.  

Setting side the Cox model, the distribution of AICs contrast sharply with those of the other statistics.  Variable seems to matter little or not at all; the plot gives the impression that the AIC values are the same for a given specification, though *[Table S1] indicates small differences.  Specifically, linear models clearly dominate the other specifications (in the game-theoretic sense) according to AIC.  By contrast, linear models had roughly the same accuracy and AUROC scores, and tended to have the second-worst F1 scores.  

All together, these statistics underdetermine model selection.  Logistic and poisson models with spline variables tend to perform slightly better with accuracy, AUROC, and $F_1$, while AIC strongly favors linear models.  Further, these statistics introduce incommensurability, insofar as the Cox model AIC cannot be compared to those of the other three model specifications.  Thus, while *[Sober and coauthor] might prefer AIC over accuracy statistics — because of the way AIC incorporates considerations of simplicity — we cannot use it to choose between linear and Cox models.  And since there is very little difference in AIC between the various covariance specifications given, say, a linear model, arguably AIC is not informative on this choice either.  


## Relative Risk Predictions ##

Figures 2 and 3 each show relative risk predictions for all 16 models across a range of BMI values, with all other covariates held fixed.  These plots thus show the statistical effect of changes in BMI, according to the various models.  Risk is measured relative to a baseline BMI of 22; that is, the risk for BMI = 22 is stipulated to be 1.0; relative risk values less than 1 indicate deceased risk relative to BMI = 22, while values greater than 1 indicate increased risk.  

![Figure 2. Relative risk predictions for all 16 models.  All covariates other than BMI are held fixed.  Predictions are grouped by covariate specifications. Ribbons give 95% confidence intervals.  All predictions are the same as in figure 3.](02_rr_preds.png)

![Figure 3. Relative risk predictions for all 16 models.  All covariates other than BMI are held fixed.  Predictions are grouped by model specifications.  Ribbons give 95% confidence intervals.  All predictions are the same as in figure 2.](03_rr_preds.png)


All 16 sets of predictions are given in both figure 2 and figure 3; these predictions are grouped differently to aid interpretation.  In figure 2, predictions are grouped by covariate specification, allowing us to compare across model specifications; while predictions in figure 3 are grouped by model specification, allowing us to compare across covariate specifications.  

Figure 2 indicates that the Cox model consistently generates predictions of greater risk than the other specifications, especially for BMI values greater than 25 (for binned and continuous BMI) or between 25 and about 40 (for the two spline specifications).  Recall that the lower bound for overweight is 25.  Thus, the region for which the Cox model indicates greater risk (relative to the other model specifications) is the region for overweight and obesity.  The other three model specifications agree with each other quite closely across the whole range of plotted BMI values.  

Furthermore, the other three model specifications generally point to the "obesity paradox."  For both spline covariate specifications, the point estimates are below 1 until BMI is greater than 35; and for binned BMI, relative risk for overweight is below 1 for all models except Cox.  There is no indication of the "obesity paradox" with continuous BMI; but since the "obesity paradox" is a non-linear trend, a continuous model cannot show it.  

Figure 3 shows some qualitative agreement between the non-continous covariate specifications.  That is, there is decreased risk across the overweight range, slightly decreased or increased risk across the obese I range, and greater increases in risk only in the obese II range, where BMI > 35.  As with the Cox model, continuous BMI tends to produce higher estimates of risk than the other covariate specifications.  This may be needed to accommodate very high increases for BMI somewhat greater than 35.  Note as well that binned BMI tends to give higher risk estimates than splined BMI across the overweight and obese I ranges.  

However, the proceeding paragraphs all suggest a "vote-counting" approach to synthesizing the findings from these models, which is widely regarded as inappropriate.  If our set of models include may highly flawed models, a vote-counting approach is likely to lead to incorrect conclusions.  Better approaches would be to use only the "most faithful" model, or to use a weighted combination of models, where the weights are based on fidelity criteria.  However, as we saw above, both generative and predictive fidelity criteria are ambiguous or incommensurable on our set of models.  While they do allow us to make some comparisons, they do not identify one unambiguous most faithful model.  In particular, while the Cox model specification does well in terms of generative fidelity, in terms of predictive fidelity it either makes the worst predictions or is incommensurable with the other specifications.  Thus the fidelity criteria used here does not let us use only the "most faithful" model.  Further, while the predictive criteria are quantitative, and therefore might be used to produce quantitative weights on the various models (setting aside the incommensurability issue), the generative criteria are not quantitative, and it is far from obvious how to use them to produce model weights.  




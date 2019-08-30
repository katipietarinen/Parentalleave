

regressiondata = read.csv("regressiondatadads.csv", stringsAsFactors = FALSE)

regressionbusinessdads2 = lm(dadsonlymonths ~ Scandinavia. + German.speaking. + English.speaking. + GenderGapScore + GDPPerCapita + Mean.age.at.first.birth.2016 + Ferility + 
                               Welfare.regime.no + SuffrageYear + 
                               + Youth.dependency.ratio.2015 + Socialist.postsocialist. , data = regressiondata, na.action = "na.omit")
summary(regressionbusinessdads2)

# Stepwise Regression
library(MASS)
step = stepAIC(regressionbusinessdads2, direction="both")

step$anova # display results 
# But actually this is what gives the real result - it suggests a final model to use. Here it is: 

dadsfinal2 = lm( dadsonlymonths ~ German.speaking. + English.speaking. + GenderGapScore + 
                   Ferility + Youth.dependency.ratio.2015 + Socialist.postsocialist. , data = regressiondata, na.action = "na.omit")
summary(dadsfinal2)
# These variables do not explain variation. 

# Predicted value of paternity leave as fully paid months in country x by the model 
predictedpatleave2 = fitted(dadsfinal2) # here one does not need to use scaled variables

# Plot these predicted values and the actual values - here also use the dataframe where the variables are not scaled, so the values are meaningful (months)

plot(predictedpatleave2, regressiondata$dadsonlymonhts,  main = "True vs predicted leave", xlab = "Predicted", ylab = "True", col = "red")
# This is a diagnostic plot for linear regression. One can see that the trend is correct, but the predictions are quite far off from reality.





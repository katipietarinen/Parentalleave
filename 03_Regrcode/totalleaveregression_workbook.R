setwd("/home/kati/Documents/Birmingham_City_University/Lopputyo/rdata")

# So this is a regression analysis of maternity + paternity leave in all, as fully paid months

regressiondataboth = read.csv("regressiondataboth.csv", stringsAsFactors = FALSE)
regressiondataboth$allleaveasfullypaidm = regressiondataboth$totalmotherleaveasfullpaidm + regressiondataboth$dadsonlyfullypaidm

# This is multiple regression just as it is

regressionbusinessboth = lm(allleaveasfullypaidm ~ Scandinavia. + German.speaking. + English.speaking. + GenderGapScore + GDPPerCapita + Mean.age.at.first.birth.2016 + Ferility + 
                               Welfare.regime.no + SuffrageYear + 
                               + Youth.dependency.ratio.2015 + Socialist.postsocialist. , data = regressiondataboth, na.action = "na.omit")
summary(regressionbusinessboth)

# Stepwise Regression
library(MASS)
step = stepAIC(regressionbusinessboth, direction="both")

step$anova # display results 
# But actually this is what gives the real result - it suggests a final model to use. Here it is: 

finalboth = lm(allleaveasfullypaidm ~ Scandinavia. + GenderGapScore + Ferility + 
  SuffrageYear + Youth.dependency.ratio.2015 + Socialist.postsocialist., data = regressiondataboth, na.action = "na.omit")
summary(finalboth)

# Quick analysis - compared to only mothers, if fathers are also included, postsocialism lengthens leave less, whereas being a Scandinavian country
# expands it more. A high youth dependency ratio shortens leave, as does a higher Gender Gap Score. These variables explain 64% of variation between countries. 

# Predicted value of paternity leave as fully paid months in country x by the model 
predictedallleave = fitted(finalboth) # here one does not need to use scaled variables

# Plot these predicted values and the actual values - here also use the dataframe where the variables are not scaled, so the values are meaningful (months)

plot(predictedallleave, regressiondataboth$allleaveasyfullypaidm,  main = "True vs predicted leave", xlab = "Predicted", ylab = "True", col = "red")
# This is a diagnostic plot for linear regression. One can see that the trend is correct, but the predictions are quite far off from reality.

# Diagnostic plots - these are the same things as earlier but repeated unnecessarily because they look cool or something? 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(finalboth)

# Residual (true - predicted value) vs fitted (=predicted value) - it should be straight line, but it's not. So model doesn't fit very well. Independent variables not able to explain. 

# Shiny app preparation

regressiondatanumbers = regressiondataboth[-c(1,2)]
scaledregression = as.data.frame(scale(regressiondatanumbers))

scaledfinalmodel = lm(allleaveasfullypaidm ~ Scandinavia. + GenderGapScore + Ferility + 
                        SuffrageYear + Youth.dependency.ratio.2015 + Socialist.postsocialist., data = scaledregression, na.action = "na.omit")
summary(scaledfinalmodel)
coefs = (coefficients(scaledfinalmodel))
varnames = (variable.names(scaledfinalmodel))
coefdataboth = data.frame("Explanations" = varnames, "ImportanceForBothParents"= coefs)
coefdataboth$Explanations = as.character(coefdataboth$Explanations)
coefdataboth = coefdataboth [-(1),] 


# Change variable names so they make sense for visualisation in shiny app
coefdataboth$Explanations[1] = paste("Scandinavian", " country", sep = " ")
coefdataboth$Explanations[2] = paste("High"," gender", " parity", sep = " ")
coefdataboth$Explanations[3] = paste("High", " fertility", sep = " ")
coefdataboth$Explanations[4]= paste("Late", " female", " suffrage", sep = " ")
coefdataboth$Explanations[5] = paste("Many", " young", " people", " v ", " working-age", sep = " ")
coefdataboth$Explanations[6] = paste("Post-socialist", "country", sep = " ")

write.csv(coefdataboth,"coefdataboth.csv")


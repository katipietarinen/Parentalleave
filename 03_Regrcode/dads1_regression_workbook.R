setwd("/home/kati/Documents/Birmingham_City_University/Lopputyo/rdata")

# MULTIPLE REGRESSION

regressiondata = read.csv("regressiondatadads.csv", stringsAsFactors = FALSE)

regressionbusinessdads1 = lm(dadsonlyfullypaidm ~ Scandinavia. + German.speaking. + English.speaking. + GenderGapScore + GDPPerCapita + Mean.age.at.first.birth.2016 + Ferility + 
                               Welfare.regime.no + SuffrageYear + 
                         + Youth.dependency.ratio.2015 + Socialist.postsocialist. , data = regressiondata, na.action = "na.omit")
summary(regressionbusinessdads1)

# Stepwise Regression
library(MASS)
step = stepAIC(regressionbusinessdads1, direction="both")

step$anova # display results 
# But actually this is what gives the real result - it suggests a final model to use. Here it is: 

dadsfinal1 = lm(dadsonlyfullypaidm ~ Scandinavia. + German.speaking. + GenderGapScore + 
  Ferility + Youth.dependency.ratio.2015 + Socialist.postsocialist. , data = regressiondata, na.action = "na.omit")
summary(dadsfinal1)

# The model fits very badly. These variables do not explain difference in paternity leave as fully paid months. 


# Predicted value of paternity leave as fully paid months in country x by the model 
predictedpatleave = fitted(dadsfinal1) # here one does not need to use scaled variables

# Plot these predicted values and the actual values - here also use the dataframe where the variables are not scaled, so the values are meaningful (months)

plot(predictedpatleave, regressiondata$dadsonlyfullypaidm,  main = "True vs predicted leave", xlab = "Predicted", ylab = "True", col = "red")
# This is a diagnostic plot for linear regression. One can see that the trend is correct, but the predictions are quite far off from reality.

# Diagnostic plots - these are the same things as earlier but repeated unnecessarily because they look cool or something? 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(dadsfinal1)
# here only check normal q-q- - it shows outliers are especially Greece, Japan and Estonia. The trend on other data points is not explained by anything. 

# Prepare for shiny app by scaling regression

regressiondatanumbers = regressiondata[-c(1,2)]
scaledregression = as.data.frame(scale(regressiondatanumbers))

scaledfinalmodel = lm(dadsonlyfullypaidm ~ Scandinavia. + German.speaking. + GenderGapScore + 
                        Ferility + Youth.dependency.ratio.2015 + Socialist.postsocialist., data = scaledregression, na.action = "na.omit")
summary(scaledfinalmodel)

#Create dataframe
coefs = (coefficients(scaledfinalmodel))
varnames = (variable.names(scaledfinalmodel))
coefdatadads = data.frame("Explanations" = varnames, "ImportanceForDads"= coefs)
coefdatadads$Explanations = as.character(coefdatadads$Explanations)
coefdatadads = coefdatadads [-(1),] 

# Change variable names so they make sense for visualisation in shiny app
coefdatadads$Explanations[1] = paste("Scandinavian", " country", sep = " ")
coefdatadads$Explanations[2]= paste("German-speaking", " country", sep = " ")
coefdatadads$Explanations[3] = paste("High"," gender", " parity", sep = " ")
coefdatadads$Explanations[4] = paste("High", " fertility", sep = " ")
coefdatadads$Explanations[5] = paste("Many", " young", " people", " v ", " working-age", sep = " ")
coefdatadads$Explanations[6] = paste("Post-socialist", "country", sep = " ")

write.csv(coefdatadads, "coefdatadads.csv")


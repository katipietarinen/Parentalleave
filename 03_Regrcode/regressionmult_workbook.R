setwd("/home/kati/Documents/Birmingham_City_University/Lopputyo/rdata")

# MULTIPLE REGRESSION

regressiondata = read.csv("regressiondata.csv", stringsAsFactors = FALSE)

#Multiple regression without standardisation

regressionbusiness = lm(totalmotherleaveasfullpaidm ~ Scandinavia. + German.speaking. + English.speaking. + GenderGapScore + GDPPerCapita + Mean.age.at.first.birth.2016 + Ferility + 
                          Welfare.regime.no + SuffrageYear + 
                          CollectiveBargainCoverage + Youth.dependency.ratio.2015 + Socialist.postsocialist. , data = regressiondata, na.action = "na.omit")
summary(regressionbusiness)

# Multiple regression with standardisation - standardisation is only needed to analyse coefficients. Otherwise, the unstandardised data SHOULD be used. 

regressiondatanumbers = regressiondata[-c(1,2)]
scaledregression = as.data.frame(scale(regressiondatanumbers))

scaledregressionbusiness = lm(totalmotherleaveasfullpaidm ~ Scandinavia. + German.speaking. + English.speaking. + GenderGapScore + GDPPerCapita + Mean.age.at.first.birth.2016 + Ferility + 
                          Welfare.regime.no + SuffrageYear + 
                          CollectiveBargainCoverage + Youth.dependency.ratio.2015 + Socialist.postsocialist. , data = scaledregression, na.action = "na.omit")
summary(scaledregressionbusiness)

# The relevant parts to check are the following: under Coefficients check Estimate - the bigger numbers are more relevant (but pay attention to whether
# it's e-01 or e-02) - note that this is only valid if values are standardised! 
# Adjusted R-squarerd (which goes between 0-1) is also relevant - it shows how well the model fits the data. 0.6304 means it's ok but not good. 
# One way to improve it, would be variable selection - i.e. chose those that fit best. 

# Now we're displaying the estimates of the coefficients 

coefficients(scaledregressionbusiness) # this is the same list as earlier in summary estimates

# Predicted value of maternity leave as fully paid months in country x by the model 
predictedmatleave = fitted(regressionbusiness) # here one does not need to use scaled variables

# Plot these predicted values and the actual values - here also use the dataframe where the variables are not scaled, so the values are meaningful (months)

plot(predictedmatleave, regressiondata$totalmotherleaveasfullpaidm,  main = "True vs predicted leave", xlab = "Predicted", ylab = "True", col = "red")
# This is a diagnostic plot for linear regression. One can see that the trend is correct, but the predictions are quite far off from reality.

# Residuals = subrtact true value from predicted. This tells us how wrong the model is - the error in prediction. 
error = residuals(regressionbusiness)

#Plot histogram
hist(error, xlim = c(-6,6), breaks = 15, main = "Error in predicting maternity leave months", xlab = "Error", ylab = "Number of countries") 
# This shows that the error is symmetric around zero, which is cool, as it shows it's basically a random error. But the two outliers show that the model cannot explain everything. 

# Diagnostic plots - these are the same things as earlier but repeated unnecessarily because they look cool or something? 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressionbusiness)
# here only check normal q-q- - it shows that same outliers as in the histogram plot but that the model is otherwise fairly ok. 

# Next we try to find a subset of variables that explains the independent variable best and gives the best model. This is called model/variable selection.

# Stepwise Regression
library(MASS)
step = stepAIC(regressionbusiness, direction="both")
# So this system shows you the 10 best models, the best is first. Has highest AIC (Akaike Information Criterion). For example the best one is this model,
# which is at the top of the list, but totally counterlogically, the best variables are at the bottom of this list of variables, under <none>. 

#Start:  AIC=55.84
# totalmotherleaveasfullpaidm ~ Scandinavia. + German.speaking. + 
#  English.speaking. + GenderGapScore + GDPPerCapita + Mean.age.at.first.birth.2016 + 
#  Ferility + Welfare.regime.no + SuffrageYear + CollectiveBargainCoverage + 
#  Youth.dependency.ratio.2015 + Socialist.postsocialist.

#                               Df Sum of Sq     RSS    AIC
#- CollectiveBargainCoverage     1     0.018  84.175 53.840
#- GDPPerCapita                  1     0.283  84.440 53.913
#- Ferility                      1     0.907  85.065 54.082
#- Youth.dependency.ratio.2015   1     1.142  85.299 54.145
#- English.speaking.             1     1.465  85.622 54.232
#- Mean.age.at.first.birth.2016  1     2.022  86.179 54.382
#- German.speaking.              1     2.657  86.815 54.551
#- Welfare.regime.no             1     4.621  88.778 55.065
#<none>                                       84.158 55.836
#- SuffrageYear                  1    20.210 104.367 58.786
#- Socialist.postsocialist.      1    26.372 110.530 60.105
#- GenderGapScore                1    30.084 114.242 60.865
#- Scandinavia.                  1    42.745 126.903 63.282

step$anova # display results 
# But actually this is what gives the real result - it suggests a final model to use. Here it is: 

#Final Model:
#  totalmotherleaveasfullpaidm ~ Scandinavia. + German.speaking. + 
#  GenderGapScore + SuffrageYear + Socialist.postsocialist.

# So, le'ts try the final model out!!!!

finalmodel = lm(totalmotherleaveasfullpaidm ~ Scandinavia. + German.speaking. +  GenderGapScore + SuffrageYear + Socialist.postsocialist., data = regressiondata, na.action = "na.omit")
summary(finalmodel)

# Analysis: adjusted r-squared 0.7516 is clearly better than before, and now even p-values (starts) show that these aspects are relevant. 

# Predicted value of maternity leave as fully paid months in country x by the model 
predictedmatleavefinal = fitted(finalmodel) # here one does not need to use scaled variables

# Plot these predicted values and the actual values - here also use the dataframe where the variables are not scaled, so the values are meaningful (months)

plot(predictedmatleavefinal, regressiondata$totalmotherleaveasfullpaidm,  main = "True vs predicted leave", xlab = "Predicted", ylab = "True", col = "red")
# This is a diagnostic plot for linear regression. One can see that the trend is correct, but the predictions are quite far off from reality.

# Residuals = subrtact true value from predicted. This tells us how wrong the model is - the error in prediction. 
error = residuals(finalmodel)

#Plot histogram
hist(error, xlim = c(-6,6), breaks = 15, main = "Error in predicting maternity leave months", xlab = "Error", ylab = "Number of countries") 
# This shows that the error is symmetric around zero, which is cool, as it shows it's basically a random error. But the two outliers show that the model cannot explain everything. 

#For shiny app visualisation, compute scaled final model
scaledfinalmodel = lm(totalmotherleaveasfullpaidm ~ Scandinavia. + German.speaking. +  GenderGapScore + SuffrageYear + Socialist.postsocialist., data = scaledregression, na.action = "na.omit")
summary(scaledfinalmodel)

# We need to create a dataframe with the coefficients and variables
coefsmat = (coefficients(scaledfinalmodel))
varnamesmat = (variable.names(scaledfinalmodel))
coefdatamat = data.frame("Explanations" = varnamesmat, "ImportanceForMothers"= coefsmat)
coefdatamat$Explanations = as.character(coefdatamat$Explanations)

# Remove intercept (first row)
coefdatamat = coefdatamat [-(1),] 

#Rename variables for visualisation
coefdatamat$Explanations[1] = paste("Scandinavian", " country", sep = " ")
coefdatamat$Explanations [2] = paste("German-speaking", " country", sep= " ")
coefdatamat$Explanations[3] = paste("High"," gender", " parity", sep = " ")
coefdatamat$Explanations[4]= paste("Late", " female", " suffrage", sep = " ")
coefdatamat$Explanations[5] = paste("Post-socialist", "country", sep = " ")

write.csv(coefdatamat,"coefdatamoms.csv")

# More detailed analyss: so what is most relevant is whether a country has had a socialist past - so countries with 
# socialist pasts have longest paid leave. Secondly, whether it's a scandinavian country - if it's Scandinavian, it will have a long leave, but not 
# so long. If it's a German-speaking country, leave is longer than others. The later the country enacted female suffrage 
# the shorter the leave is. Also - the better the country does on the Gender Gap Index (the better the country is generally for women) - the shorter the 
# leave as paid months. This is explained partially at least by the fact that postsocialist countries don't have super amazing ourcomes for women -
# especially Hungary. 
 


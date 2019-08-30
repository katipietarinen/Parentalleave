setwd("/home/kati/Documents/Birmingham_City_University/Lopputyo/rdata")

# MULTIPLE REGRESSION

regressiondata = read.csv("regressiondata.csv", stringsAsFactors = FALSE)

#Multiple regression without standardisation

regressionbusinessmoms2 = lm(totalpaidmotherleave ~ Scandinavia. + German.speaking. + English.speaking. + GenderGapScore + GDPPerCapita + Mean.age.at.first.birth.2016 + Ferility + 
                          Welfare.regime.no + SuffrageYear + 
                          CollectiveBargainCoverage + Youth.dependency.ratio.2015 + Socialist.postsocialist. , data = regressiondata, na.action = "na.omit")
summary(regressionbusinessmoms2)

# Stepwise Regression
library(MASS)
step = stepAIC(regressionbusinessmoms2, direction="both")

step$anova # display results 
# But actually this is what gives the real result - it suggests a final model to use. Here it is: 

finalmodelmoms2 = lm(totalpaidmotherleave ~ Scandinavia. + German.speaking. + English.speaking. + 
  GenderGapScore + GDPPerCapita + Youth.dependency.ratio.2015 + 
  Socialist.postsocialist. , data = regressiondata, na.action = "na.omit")
summary(finalmodelmoms2)

# Here the model really doesn't fit very well. It explains under half of the variation between countries. So something else should explain instead. Here based on p-value (of t-test for each coefficient) the most important are socialist past, german-speaking, and then Scandinvia and Gender Gap Score. 
#### MULTIPLE REGRESSION PROJECT ON MATERNITY AND PATERNITY LEAVE LENGTHS #####
# I'm interested in what factors can exmplain the length of parental leave given to mothers and fathers in different countries. 

setwd("/home/kati/Documents/Birmingham_University/Lopputyo/rdata")

library(ggplot2)

#install.packages("dplyr")
library(dplyr)

# 1. First I'll try LINEAR REGRESSION with 2 datasets - maternity leave (all leave for mothers as fully paid weeks) & fertility rate 2017, based on OECD data.

# 1.1 THIS IS DATA READING & CLEANING 

# Open csv files (add stringsAsFactors=F to export data as characters not factors, that mess everything up)
leavedata = read.csv("matleavedata.csv", stringsAsFactors=FALSE)  # read csv file
fertdata = read.csv("fertdata.csv", stringsAsFactors=FALSE)  # read csv file)
names(fertdata)[2] = "Ferility"

# If you want to check whether for some fucking ridiculous reason part of numerics are not numerics
typeof(leavefert$fertility)

#Then change characters to numerics in Total.leave.as.full.rate.months (because it's stupidly as characters)
leavedata$Total.leave.as.full.rate.months = as.numeric(leavedata$Total.leave.as.full.rate.months)

#Change headers to shorter names
names(leavedata)[4] = "totalpaidmotherleave"
names(leavedata)[5] = "totalmotherleaveasfullpaidm"
allmotherleavemonths = dplyr::select(leavedata,"Country","totalpaidmotherleave")
motherleavefullpaidmonths = dplyr::select(leavedata,"Country","totalmotherleaveasfullpaidm")

# Merge csv files
leavefert <- merge(leavedata, fertdata, by="Country")

#Check header names
names(leavefert)

#[1] "Country"                                
#[2] "Paid.maternity.leave.in.months"         
#[3] "Paid.maternity.leave.as.ful.rate.months"
#[4] "Total.paid.leave.for.mothers.in.months" 
#[5] "Total.leave.as.full.rate.months"        
#[6] "X2017"   

# Change super long or stupid column names
colnames(leavefert)[5]<-"totalfull"
colnames(leavefert)[6]<- "fertility"

# 2. THESE ARE DESCRIPTIVE STATISTICS

#Check stats
summary(leavefert$totalfull)

#Check standard deviation (sq root of avereage squared deviation from mean) - remove N/A value for Romania while computing
sd(leavefert$totalfull,  na.rm = TRUE)

#Plot histogram
hist(leavefert$totalfull, xlim = c(0,20), breaks = 15, main = "Leave for mothers as fully paid months", xlab = "Months", ylab = "Number of countries")

#Make a boxplot to check if there are outliers
boxplot(leavefert$totalfull, main = "Leave for mothers as fully paid months", ylab = "Months")

#There are no outliers, but the data is negatively skewed (long tail) - 
# i.e. the median is quite short leave, but there are some countries with very long leaves, but they are not outliers, 
# as there is a continuous trend

#Make a bar plot
ggplot(leavefert, aes(x = reorder(Country, totalfull), y = totalfull)) + geom_bar(stat = "identity", fill="darkgreen", position = "dodge")+ coord_flip() + geom_text(aes(label=totalfull), vjust=-7, size=2)+
  theme_minimal() + labs(title="Leave for mothers as fully paid months", x="Countries", y = "Months")

# Empty Romania is here again, figure out how to get rid of it, maybe higher up in the code 

#Plot with scatter.smooth
scatter.smooth(x=leavefert$totalfull, y=leavefert$fertility, main="totalfull ~ fertility") 

# Or do it like this - x-axis independent variable, y-axis dependent variable 
plot(leavefert$fertility, leavefert$totalfull,  main = "Leave and fertility", xlab = "Fertility", ylab = "Leave in fully paid months", col = "red")

#Use lm to fit linear regression line through, make this an object with the name model1
model1 = lm(totalfull ~ fertility , data = leavefert)

#Print model1 - it will give the estimated coefficients of linear regression
print(model1)

#Plot model1 (the line)
abline(model1, col = "darkgreen")

#Check correlation (between 1 and -1 - here it's very low)
cor(leavefert$totalfull, leavefert$fertility, use = "complete.obs")

# Check summary statistics for linear regression  
summary(model1)

#They result is:
# Call:
#lm(formula = totalfull ~ fertility + fertility^2, data = leavefert)

#Residuals:
# Min     1Q Median     3Q    Max 
#-6.47  -3.26  -1.70   2.68  12.45 

#Coefficients:
# Estimate Std. Error t value Pr(>|t|)   
# (Intercept)    12.18       3.73    3.26   0.0023 **
#  fertility      -3.23       2.25   -1.44   0.1593   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 4.57 on 39 degrees of freedom
#(1 observation deleted due to missingness)
#Multiple R-squared:  0.0502,	Adjusted R-squared:  0.0258 
#F-statistic: 2.06 on 1 and 39 DF,  p-value: 0.159

# Build linear regression model
linearMod <- lm(totalfull ~ fertility, data=leavefert)  # build linear regression model on full data
print(linearMod)

# 3. THIS IS MULTIPLE REGRESSION

# So here I'm interested the factors explaining the differences in length of leave given to both mothers and fathers. 

#3.1. But first one needs to bring in the rest of the datasets and do some cleaning

#Remove leavefert as not needed anymore
rm(leavefert)

# Bring in the other datasets, change unclear headers, filter & make sure numbers are read as numerals, not factors or characters - if they are, change the category

#3.1.1. Dependent variables, leave length for mothers and fathers

# Data on leave length for mothers (OECD data on all paid leave for mothers in months and on leave for mothers converted to fully paid months) is already here, as they were uploaded earlier

# Data on leave only available for fathers both as months and converted to fully-paid months
dadsonlymonthsdata = read.csv("OECD_Patleave .csv", stringsAsFactors = FALSE) #OECD data on length of paid parental leave only available for fathers in months
names(dadsonlymonthsdata)[2] = "dadsonlymonths"
dadsonlymonthsdata = dplyr::select(dadsonlymonthsdata, "Country", "dadsonlymonths")

dadsonlyasfullpaidmonthsdata = read.csv("OECD_Patleave .csv", stringsAsFactors = FALSE) # OECD data on length of parental leave only available for fathers converted to fully paid months 
names(dadsonlyasfullpaidmonthsdata)[3] = "dadsonlyfullypaidm"
dadsonlyasfullpaidmonthsdata = dplyr::select(dadsonlyasfullpaidmonthsdata, "Country", "dadsonlyfullypaidm") 

#3.1.2. Independent variables: Data on the economy

gdpdata = read.csv("OECD_GDP_USD_CAP_2018.csv", stringsAsFactors=FALSE) # OECD data on GDP per capita in 2018
names(gdpdata)[2] = "GDPPerCapita"

#3.1.3. Independent variables: Data related to womens' rights

femalevotedata = read.csv("Wikipedia_female_suffrage - Sheet1.csv", stringsAsFactors = FALSE) # Wikipedia table on year women got equal suffrage in national elections (or local, if no nat)
names(femalevotedata)[2] = "SuffrageYear"
femalevotedata$SuffrageYear = as.numeric(femalevotedata$SuffrageYear)
femalevote = dplyr::select(femalevotedata,"Country","SuffrageYear")
rm(femalevotedata)

genderscoredata = read.csv("WEF_Global_Gender_Gap_Index_2018 - WEF_Global_Gender_Gap_Index_2018.csv", stringsAsFactors = FALSE) # WEF Global Gender Gap Index Scores (0-1) from 2018

#3.1.4. Independent variables: Data on country groups

socialistpast = read.csv("Wikipedia_Socialist_states - All_countries.csv", stringsAsFactors = FALSE) # Based on Wikipedia table of current and post socialist states and own knowledge

scandinaviadata = read.csv("Country_groups - Sheet1.csv", stringsAsFactors = FALSE) # Whether a  country is considered part of Scandinacia (politically)
scandinaviadata = dplyr::select(scandinaviadata,"Country","Scandinavia.")

englishdata = read.csv("Country_groups - Sheet1.csv", stringsAsFactors = FALSE) #Whether a country is majority English-speaking (based on UK gov guidelines)
englishdata = dplyr::select(englishdata, "Country", "English.speaking.")

germandata = read.csv("Country_groups - Sheet1.csv", stringsAsFactors = FALSE) # Whether a country is majority German-speaking
germandata = dplyr::select(germandata, "Country", "German.speaking.")

espingadata = read.csv("Ebbinghaus_Welfare_regimes_EspingA - Ebbinghaus_Welfare_regimes_EspingA.csv") # Countries grouped as welfare regimes based on Esping-Andersen's famous theory, table extracted from paper by Bernhard Ebbinghaus, 2012 "Comparing Welfare State Regimes:  
# Are Typologies an Ideal or Realistic Strategy?
espingtype = dplyr::select(espingadata, "Country","Welfare.regime.no")
rm(espingadata)

#3.1.5. Independent variables: Data on demographics

# fertdata already exists on fertility

agefirstbirthdata = read.csv("OECD_Mean_age_first_birth_2016.csv", stringsAsFactors = FALSE) # OECD data on Mean age of women at first birth 2016 or latest available

youthdepdata = read.csv("OECD_Youth_dependency_ratio_2015 .csv", stringsAsFactors = FALSE) # “Estimated number of children and young people (aged 0-20) per one hundred people of working age (aged 20-64) in 2015”

idealkidsdata = read.csv("OECD_Ideal_family.csv", stringsAsFactors=FALSE) # OECD data from 2011 on mean ultimately intended number of children, women aged 25-39 

#3.1.6 Independent variables: Data on working mothers & preschool for kids

preschool02data = read.csv("OECD_02yo_preschool_rates.csv", stringsAsFactors=FALSE) # OECD data on percentage of 0-2 year olds in formal preschool
names(preschool02data)[2] = "2yoPreschoolRate"

fulltimemoms014 = read.csv("OECD_Maternal_Employment _Fulltime employment_rate 0-14 moms.csv", stringsAsFactors = FALSE) # OECD data on fulltime employment rate of mothers (15-64 year olds) with at least one child 0-14 year old kids

employedmoms014 = read.csv("OECD_Maternal_Employment_Employment rate 0-14 moms.csv", stringsAsFactors = FALSE) # OECD data employment rate of mothers (15-64 year olds) with at least one child 0-14 year old kids

employmomskidsage = read.csv("OECD_Maternal_Employment _youngest_child_age.csv",stringsAsFactors = FALSE) # OECD data on maternal employment by age of younget child

#Use select to find specific columns needed
employedmoms02 = select(employmomskidsage, Country, Youngest.child.aged.0.2) 

employedmoms35 = select(employmomskidsage, Country, Youngest.child.aged.3.5)

employedmoms614 = select(employmomskidsage, Country, Youngest.child.aged.6.14)

#3.1.7. Independent variables: Data on political system

#leftyears upcoming

colbargdata = read.csv("OECD_Collectivebargaincover.csv", stringsAsFactors=FALSE) # OECD data on percentage of workers covered by collective agreements (of those who could be)

# Filter relevant parts of colbargdata, I need the values for the last year available for each country
# First, create a list of the countries named Countries in colbargdata using unique
Countries = unique(colbargdata$Country) 

#This is an example of how to access the data about the first country in Countries (and in colbargdata), Australia
colbargdatatemp = colbargdata %>% filter(Country== Countries[1])

#This is an example of how to access the last value (latest year) of a certain country
last(colbargdatatemp$Value)

#Create for-loop to filter the values per last year available for each country and store them in a list called lastcolbarg
lastcolbarg = c()

for (countryname in Countries) {
  colbargdatatemp = colbargdata %>% filter(Country==countryname)
  lastcolbarg <- c(lastcolbarg , last(colbargdatatemp$Value))
}

# Then one needs to unite the lastcolbarg-list and the countries it refers to in a dataframe. They are stored in lastcolbarg list in the same order.
lastcolbargdata <- data.frame("Country" = Countries, "CollectiveBargainCoverage" = lastcolbarg) 

#So now finally lastcolbargdata has the relevant information needed for using in regression related to collective bargaining coverage

#Remove unnecessary earlier colbarg-versions
rm("colbargdata","colbargdatatemp")

# 3.2. More cleaning and preparations

#Make sure all datasets have a column called Country
names(agefirstbirthdata)[1] = "Country"

# Merge all dataframes into a new one - as one cannot use merge on more than 2 variables, one needs to either manually merge first two, and then add 2 more etc. like this:
leavegdp = merge(leavedata, gdpdata,  by="Country")
preschoolleavegdp = merge(leavegdp,preschool02data, by="Country")

# Or there is a function like this online that one can use to merge a longer list of variables, but I don't really understand what it's doing: 

regressiondata = Reduce(function(x,y) merge(x = x, y = y, by = "Country"), 
                        list(agefirstbirthdata, youthdepdata, fertdata,
                             socialistpast, lastcolbargdata, genderscoredata, scandinaviadata, germandata, englishdata,espingtype,
                             allmotherleavemonths, motherleavefullpaidmonths, gdpdata, femalevote))

# Save regressiondata as dataframe that can be worked on without loading everything, this is only for mothers
write.csv(regressiondata, "regressiondata.csv")

# As idealkids had only 27 countries, it was first included but then removed, because with the merge the number of countries dropped; so was femalevote. Try this regression later on seperately? 

# For later analysis (in separate workbook, create regressiondata for leaves for dads)
regressiondatadads = Reduce(function(x,y) merge(x = x, y = y, by = "Country"), 
                         list(agefirstbirthdata, youthdepdata, fertdata,
                              socialistpast, genderscoredata, scandinaviadata, germandata, englishdata,espingtype,
                             dadsonlyasfullpaidmonthsdata, dadsonlymonthsdata, gdpdata, femalevote))
write.csv(regressiondatadads, "regressiondatadads.csv")

# And for both moms and dads
regressiondataboth = Reduce(function(x,y) merge(x = x, y = y, by = "Country"), 
                            list(agefirstbirthdata, youthdepdata, fertdata,
                                 socialistpast, genderscoredata, scandinaviadata, germandata, englishdata,espingtype,
                                 dadsonlyasfullpaidmonthsdata, dadsonlymonthsdata, gdpdata, femalevote,  allmotherleavemonths, motherleavefullpaidmonths))
write.csv(regressiondataboth, "regressiondataboth.csv")


#Plot the regressionsdata

#plot(na.omit(regressiondata), pch=1, col = "orange", main = "Relationship between variables affecting the length of leave for moms")

# This is not working due to NAs in the data. So skip plotting for now. And go on to..

# MULTIPLE REGRESSION

regressiondata = read.csv("regressiondata.csv", stringsAsFactors = FALSE)

# Check correlations first

cor(regressiondata$totalmotherleaveasfullpaidm, regressiondata$Youth.dependency.ratio.2015, use = "complete.obs")

# regarding leave as full-paid months
regressionbusiness = lm(totalmotherleaveasfullpaidm ~ + Scandinavia. + German.speaking. + English.speaking. + GenderGapScore + GDPPerCapita + Mean.age.at.first.birth.2016 + CollectiveBargainCoverage + Youth.dependency.ratio.2015 + Socialist.postsocialist. , data = regressiondata, na.action = "na.omit")
summary(regressionbusiness)

# regarding length of paid leave
regressionbusiness2 = lm(totalpaidmotherleave ~ Employment.rate + GDPPerCapita + Mean.age.at.first.birth.2016 + Youth.dependency.ratio.2015 + Socialist.postsocialist. + Full.time.employment + Youngest.child.aged.0.2  + Youngest.child.aged.3.5 + Youngest.child.aged.6.14, data = regressiondata, na.action = "na.omit")
summary(regressionbusiness2)


# - so this is not really relevant at all

# Let's see how it works with just those that were relevant and an additional female suffreage year, regarding leave as full-paid months
regressionbusiness3 = lm(totalmotherleaveasfullpaidm ~ Employment.rate + Youth.dependency.ratio.2015 + Socialist.postsocialist. + SuffrageYear, data = regressiondata2, na.action = "na.omit")
summary(regressionbusiness3)

regressionbusiness3 = lm(totalmotherleaveasfullpaidm ~ Employment.rate + Youth.dependency.ratio.2015 + Socialist.postsocialist., data = regressiondata, na.action = "na.omit")
summary(regressionbusiness3)

# lastcolbarg <- array(numeric(),c(length(Countries),0))
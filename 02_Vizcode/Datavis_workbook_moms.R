#### VISUALIZATIONS of MATERNITY and PATERNITY leave lengths ####

setwd("/home/kati/Documents/Birmingham_City_University/Lopputyo/rdata")

library(ggplot2)

install.packages("dplyr")
library(dplyr)

library(wesanderson)

# 1.1 THIS IS DATA READING & CLEANING 

# Open and clean maternity data
leavedata = read.csv("matleavedata.csv", stringsAsFactors=FALSE)  # read csv file

#Then change characters to numerics in Total.leave.as.full.rate.months (because it's stupidly as characters)
leavedata$Total.leave.as.full.rate.months = as.numeric(leavedata$Total.leave.as.full.rate.months)

#Change headers to shorter names
names(leavedata)[4] = "totalpaidmotherleave"
names(leavedata)[5] = "totalmotherleaveasfullpaidm"
allmotherleavemonths = select(leavedata,"Country","totalpaidmotherleave")
allmotherleavemonths = na.omit(allmotherleavemonths)
motherleavefullpaidmonths = select(leavedata,"Country","totalmotherleaveasfullpaidm")
motherleavefullpaidmonths = na.omit(motherleavefullpaidmonths)

# 2. THIS IS VISUALIZATION

#2.1.2. Make a bar chart about maternity with both months and fully paid months (dodged)

#Give a common name to column about leave
names(motherleavefullpaidmonths)[2] = "leave"
names(allmotherleavemonths)[2]= "leave"

# create an grp column for each of the above data.frames (months, fully paid months)
motherleavefullpaidmonths$LeaveType = "Fully_paid_months"
allmotherleavemonths$LeaveType = "Length"
allmotherleavemonths = allmotherleavemonths [-(36),] 
allmotherleavemonths = allmotherleavemonths [-(44),]
allmotherleavemonths = allmotherleavemonths [-(43),]

# merge data together
motherleaves = rbind(allmotherleavemonths, motherleavefullpaidmonths)

# set levels for grp column - which one should be displayed first within the group
# here, mothers followed by fathers

motherleaves$LeaveType = factor(motherleaves$LeaveType, levels=c("Fully_paid_months", "Length" ))

# make sure country is a factor (reorder levels if you have to)

motherleaves$Country = factor(motherleaves$Country)

# plot using ggplot

ggplot(motherleaves, aes(x = reorder(Country, leave), y = leave)) + geom_bar(stat = "identity", width= 1, aes(fill=LeaveType), position = position_dodge(width=0.5)) + coord_flip() +
  theme_minimal() + labs(title = "Paid leave available to mothers", x="Countries", y = "Months")


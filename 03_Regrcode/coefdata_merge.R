setwd("/home/kati/Documents/Birmingham_City_University/Lopputyo/rdata")

#Merging coefdatasets into one for use in shiny app

#MOms
coefmoms = read.csv("coefdatamoms.csv")
coefmoms = coefmoms [-c(1)]
coefmoms$Explanations = as.character(coefmoms$Explanations)

# Dads
coefdads = read.csv("coefdatadads.csv")
coefdads = coefdads [-c(1)]
coefdads$Explanations = as.character(coefdads$Explanations)

# In all
coefboth = read.csv("coefdataboth.csv")
coefboth = coefboth [-c(1)]
coefboth$Explanations = as.character(coefboth$Explanations)

merge1 = merge(coefmoms, coefdads, all = TRUE)
coeffinal = merge(merge1, coefboth, all = TRUE)

coeffinal[is.na(coeffinal)] = 0

write.csv(coeffinal,"coeffinal.csv")

# Parentalleave
Data journalism Pg Cert final project on parental leave

This is a repository for the data and code I used for my final project at Birmingham City University, Postgraduate Certificate Programme in Data Journalism. The project was an investigation into family leave policies in the developed world. The articles I wrote are available at https://medium.com/p/5d73c88db970/ . 

Folder 1: Origdata

This folder contains the original versions of the datasets I used for the project, both for visualizations and linear regression.  

They are: 

English-speaking countries, based on https://www.gov.uk/tier-4-general-visa/knowledge-of-english

Female suffrage year, based on Wikipedia website https://en.wikipedia.org/wiki/Women%27s_suffrage 

OECD: Fertility rates, http://www.oecd.org/els/soc/SF_2_1_Fertility_rates.xlsx

OECD: GDP, https://data.oecd.org/gdp/gross-domestic-product-gdp.htm

OECD: Mean age of women at childbirth http://www.oecd.org/els/soc/SF_2_3_Age_mothers_childbirth.xlsx (Mean age of women at first birth 2016 or latest available)

OECD: Mean ultimately intended family size, http://www.oecd.org/els/family/SF_2_2-Ideal-actual-number-children.xls (Mean average ultimately intended number of children of women aged 25-39, 2011) 

OECD: Parental leave systems, http://www.oecd.org/els/soc/PF2_1_Parental_leave_systems.xlsx

OECD: Trade union membership and collective bargain coverage: https://stats.oecd.org/Index.aspx?DataSetCode=TUD

OECD: Youth Dependency Ratio, http://www.oecd.org/els/soc/SF_1_4_Population_age_children_youth_dependency_ratio.xlsx (“Estimated number of children and young people (aged 0-20) per one hundred people of working age (aged 20-64) in 2015”)

Socialist and Post-socialist countries. Based on Wikipedia website https://en.wikipedia.org/wiki/List_of_socialist_states

Scandinavian counties, based on Wikipedia page https://en.wikipedia.org/wiki/Scandinavia

Score in World Economic Forum global gender gap report: http://www3.weforum.org/docs/WEF_GGGR_2018.pdf (Converted from pdf to csv in Tabula.) 

Welfare regime, based on table in Ebbinghaus, Bernard 2012. Comparing Welfare State Regimes: Are Typologies an Ideal or Realistic Strategy? http://www.cas.ed.ac.uk/__data/assets/pdf_file/0005/89033/Ebbinghaus_-_Stream_2.pdf (Converted from pdf to csv in Tabula)


Folder 2: Vizcode

This folder contains the R code I used for three data visualizations on leave for mother and fathers in ggplot, and the cleaned datasets (cleaned in Google sheets) that I used. 

Folder 3: Regrcode

This folder contains the R code I used for linear regression, in order to determine the factors that influence the length of leave in different countries (visualized in the Shiny app). I collaborated with a statistician, who advised me to use AIC (Akaike Information Criterion) to determine which variables to select. The folder also contains the cleaned datasets (cleaned in Google sheets) that I used. 

Folder 4: Shiny app

This folder contains the R code I used for creating a Shiny app about the factors influencing the length of leave. It also includes the dataset on coefficients that I used. 


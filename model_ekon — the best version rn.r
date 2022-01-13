library("readxl")
library("sandwich")
library("knitr")
library("tidyverse")

options(scipen = 999)

#LOAD DATA
df <- read_excel("baza_ekon.xlsx") 
View(df)

#DELETE NA
df1<-na.omit(df)
str(df1) #TYPES OF VARIABLES

pairs(df1[,0:10]) 

library(corrplot)
kor<-cor(df1)
corrplot(kor, method="circle")

###################################################################################
########################### DEPENDENT VARIABLE CHECKING ###########################
#DEPENDENT VARIABLE - BIRTH RATE

library(rcompanion)

par(mfrow=c(1,2))  
plotNormalHistogram(df1$birth_rate, prob = FALSE,
                    main = "birth_rate and normal distribution",
                    linecol = "red",
                    length = 1000)

df1$ln_birth_rate = log(df1$birth_rate)

plotNormalHistogram(df1$ln_birth_rate, prob = FALSE,
                    main = "ln_birth_rate and normal distribution",
                    linecol = "red",
                    length = 1000)

library("tseries")
jarque.bera.test(df1$birth_rate) #p-value < 0.05, I reject H0 about normal distribution of birth_rate
jarque.bera.test(df1$ln_birth_rate) #p-value > 0.05; no grounds for rejecting H0 about normal distribution of ln_birth_rate

##########################################################################
########################### VARIABLES CHECKING ###########################
#VARIABLE COVID19_CASES

plotNormalHistogram(df1$covid19_cases, prob = FALSE,
                    main = "covid19_cases and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_covid19_cases = log(df1$covid19_cases)

plotNormalHistogram(df1$ln_covid19_cases, prob = FALSE,
                    main = "ln_covid19_cases and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_covid19_cases<-ggplot(df1, aes(x=birth_rate, y=ln_covid19_cases)) +geom_point(color="red")
g_ln_covid19_cases

jarque.bera.test(df1$covid19_cases) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_covid19_cases) #p-value < 0.05, I reject H0 about normal distribution

#ln_covid19_cases appears to have a distribution more similar to the normal distribution

########################### VARIABLES CHECKING ###########################
#VARIABLE COVID19_DEATHS

plotNormalHistogram(df1$covid19_deaths, prob = FALSE,
                    main = "covid19_deaths and normal distribution",
                    linecol = "red",
                    length = 1000) 

#covid19_deaths has values equal to 0, so I can't use the logarithm, because I get infinity values (log from 0 is equal to inf)

########################### VARIABLES CHECKING ###########################
#VARIABLE COVID19_QUARANTINE

plotNormalHistogram(df1$covid19_quarantine, prob = FALSE,
                    main = "covid19_quarantine and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_covid19_quarantine = log(df1$covid19_quarantine)

plotNormalHistogram(df1$ln_covid19_quarantine, prob = FALSE,
                    main = "ln_covid19_quarantine and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_covid19_quarantine<-ggplot(df1, aes(x=birth_rate, y=ln_covid19_quarantine)) +geom_point(color="red")
g_ln_covid19_quarantine

jarque.bera.test(df1$covid19_quarantine) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_covid19_quarantine) #p-value < 0.05, so I reject H0 about normal distribution, but p-value is higher
#than in previous test

########################### VARIABLES CHECKING ###########################
#VARIABLE MARRIAGE_RATE

plotNormalHistogram(df1$marriage_rate, prob = FALSE,
                    main = "marriage_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_marriage_rate = log(df1$marriage_rate)

plotNormalHistogram(df1$ln_marriage_rate, prob = FALSE,
                    main = "ln_covid19_cases and normal distribution",
                    linecol = "red",
                    length = 1000) 

jarque.bera.test(df1$marriage_rate) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_marriage_rate) #p-value > 0.05

########################### VARIABLES CHECKING ###########################
#VARIABLE DIVORCE_RATE

plotNormalHistogram(df1$divorce_rate, prob = FALSE,
                    main = "divorce_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_divorce_rate = log(df1$divorce_rate)

plotNormalHistogram(df1$ln_divorce_rate, prob = FALSE,
                    main = "ln_divorce_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

jarque.bera.test(df1$divorce_rate) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_divorce_rate) #p-value <0.05

#divorce_rate will be placed in model

########################### VARIABLES CHECKING ###########################
# VARIABLE BUDGET_REVE_PC
plotNormalHistogram(df1$budget_reve_pc, prob = FALSE,
                    main = "budget_reve_pc and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_budget_reve_pc = log(df1$budget_reve_pc)

plotNormalHistogram(df1$ln_budget_reve_pc, prob = FALSE,
                    main = "ln_budget_reve_pc and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_budget_reve_pc<-ggplot(df1, aes(x=birth_rate, y=ln_budget_reve_pc)) +geom_point(color="red")
g_ln_budget_reve_pc

jarque.bera.test(df1$budget_reve_pc) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_budget_reve_pc) #p-value <0.05

#budget_reve_pc with log and without log looks similar, I decide to model normal version of this variable

########################### VARIABLES CHECKING ###########################
#VARIABLE UNEMPLOYMENT_RATE

plotNormalHistogram(df1$unemployment_rate, prob = FALSE,
                    main = "unemployment_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_unemployment_rate = log(df1$unemployment_rate)

plotNormalHistogram(df1$ln_unemployment_rate, prob = FALSE,
                    main = "ln_unemployment_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

jarque.bera.test(df1$unemployment_rate) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_unemployment_rate) #p-value <0.05, but higher than in previous case

#ln_unemployment_rate will be placed in my model

########################### VARIABLES CHECKING ###########################
#VARIABLE WOMEN_REPRODUCTIVE

jarque.bera.test(df1$women_reproductive) #p-value > 0.05

########################### VARIABLES CHECKING ###########################
#VARIABLE FEMININITY_RATIO

g_femininity_ratio<-ggplot(df1, aes(x=birth_rate, y=femininity_ratio)) +geom_point(color="red")
g_femininity_ratio

plotNormalHistogram(df1$femininity_ratio, prob = FALSE,
                    main = "femininity_ratio and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_femininity_ratio = log(df1$femininity_ratio)

plotNormalHistogram(df1$ln_femininity_ratio, prob = FALSE,
                    main = "ln_femininity_ratio and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_femininity_ratio<-ggplot(df1, aes(x=birth_rate, y=ln_femininity_ratio)) +geom_point(color="red")
g_ln_femininity_ratio

jarque.bera.test(df1$femininity_ratio) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_femininity_ratio) #p-value <0.05, but higher than in previous case

#ln_femininity_ratio will be placed in model

########################### VARIABLES CHECKING ###########################
#VARIABLE AVG_SALARY

g_avg_salary<-ggplot(df1, aes(x=birth_rate, y=avg_salary)) +geom_point(color="red")
g_avg_salary

plotNormalHistogram(df1$avg_salary, prob = FALSE,
                    main = "avg_salary and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_avg_salary = log(df1$avg_salary)

plotNormalHistogram(df1$ln_avg_salary, prob = FALSE,
                    main = "ln_avg_salary and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_avg_salary<-ggplot(df1, aes(x=birth_rate, y=ln_avg_salary)) +geom_point(color="red")
g_ln_avg_salary

jarque.bera.test(df1$avg_salary) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_avg_salary) #p-value <0.05

#avg_salary will be placed in model

########################### VARIABLES CHECKING ###########################
#VARIABLE WOMEN_WORKING
plotNormalHistogram(df1$women_working, prob = FALSE,
                    main = "women_working and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_women_working = log(df1$women_working)

plotNormalHistogram(df1$ln_women_working, prob = FALSE,
                    main = "ln_women_working and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_women_working<-ggplot(df1, aes(x=birth_rate, y=ln_women_working)) +geom_point(color="red")
g_ln_women_working

jarque.bera.test(df1$women_working) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_women_working) #p-value > 0.05

########################### VARIABLES CHECKING ###########################
#VARIABLE MEN_WORKING
plotNormalHistogram(df1$men_working, prob = FALSE,
                    main = "men_working and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_men_working = log(df1$men_working)

plotNormalHistogram(df1$ln_men_working, prob = FALSE,
                    main = "ln_women_working and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_men_working<-ggplot(df1, aes(x=birth_rate, y=ln_men_working)) +geom_point(color="red")
g_ln_men_working

jarque.bera.test(df1$men_working) #p-value < 0.05, I reject H0 about normal distribution
jarque.bera.test(df1$ln_men_working) #p-value > 0.05

########################### VARIABLES CHECKING ###########################
#VARIABLE MEDIAN_HOUSE_PRICE

jarque.bera.test(df1$median_house_price) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_median_house_price = log(df1$median_house_price)
jarque.bera.test(df1$ln_median_house_price) #p-value < 0.05, but higher than in previous case

plotNormalHistogram(df1$median_house_price, prob = FALSE,
                    main = "median_house_price and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_median_house_price, prob = FALSE,
                    main = "ln_median_house_price and normal distribution",
                    linecol = "red",
                    length = 1000)

#ln_median_house_price will be placed in model

########################### VARIABLES CHECKING ###########################
#VARIABLE HOUSE_RATIO

jarque.bera.test(df1$house_ratio) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_house_ratio = log(df1$house_ratio)
jarque.bera.test(df1$ln_house_ratio) #p-value < 0.05

plotNormalHistogram(df1$house_ratio, prob = FALSE,
                    main = "house_ratio and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_house_ratio, prob = FALSE,
                    main = "ln_house_ratio and normal distribution",
                    linecol = "red",
                    length = 1000) 

#house_ratio will be placed in model

########################### VARIABLES CHECKING ###########################
#VARIABLE HOUSES_AREA_PC

jarque.bera.test(df1$houses_area_pc) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_houses_area_pc = log(df1$houses_area_pc)
jarque.bera.test(df1$ln_houses_area_pc) #p-value < 0.05

plotNormalHistogram(df1$houses_area_pc, prob = FALSE,
                    main = "houses_area_pc and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_houses_area_pc, prob = FALSE,
                    main = "ln_houses_area_pc and normal distribution",
                    linecol = "red",
                    length = 1000) 

#i decided to place in model houses_area_pc variable

########################### VARIABLES CHECKING ###########################
#VARIABLE AVG_PEOPLE_PER_HOUSE

jarque.bera.test(df1$avg_people_per_house) #p-value > 0.05

########################### VARIABLES CHECKING ###########################
#VARIABLE EDUCATION_EXPENDITURE

jarque.bera.test(df1$education_expenditure) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_education_expenditure = log(df1$education_expenditure)
jarque.bera.test(df1$ln_education_expenditure) #p-value < 0.05

plotNormalHistogram(df1$education_expenditure, prob = FALSE,
                    main = "education_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_education_expenditure, prob = FALSE,
                    main = "ln_education_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 

#variable with or without log looks similar, so teh basic version will be placed in model

########################### VARIABLES CHECKING ###########################
#VARIABLE HEALTH_EXPENDITURE

jarque.bera.test(df1$health_expenditure) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_health_expenditure = log(df1$health_expenditure)
jarque.bera.test(df1$ln_health_expenditure) #p-value < 0.05

plotNormalHistogram(df1$health_expenditure, prob = FALSE,
                    main = "health_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_health_expenditure, prob = FALSE,
                    main = "ln_health_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 

#health_expenditure will be placed in model

########################### VARIABLES CHECKING ###########################
#VARIABLE SOCIAL_EXPENDITURE

jarque.bera.test(df1$social_expenditure) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_social_expenditure = log(df1$social_expenditure)
jarque.bera.test(df1$ln_social_expenditure) #p-value < 0.05, but p-value is higher

plotNormalHistogram(df1$social_expenditure, prob = FALSE,
                    main = "social_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_social_expenditure, prob = FALSE,
                    main = "ln_social_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 

#ln_social_expenditure will be placed in model

########################### VARIABLES CHECKING ###########################
#VARIABLE FAMILY_EXPENDITURE

jarque.bera.test(df1$family_expenditure) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_family_expenditure = log(df1$family_expenditure)
jarque.bera.test(df1$ln_family_expenditure) #p-value < 0.05

plotNormalHistogram(df1$family_expenditure, prob = FALSE,
                    main = "family_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_family_expenditure, prob = FALSE,
                    main = "ln_family_expenditure and normal distribution",
                    linecol = "red",
                    length = 1000) 

#family_expenditure will be placed in model

########################### VARIABLES CHECKING ###########################
#VARIABLE CHILDREN

jarque.bera.test(df1$children) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_children = log(df1$children)
jarque.bera.test(df1$ln_children) #p-value < 0.05, but p-value is higher

plotNormalHistogram(df1$children, prob = FALSE,
                    main = "children and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_children, prob = FALSE,
                    main = "ln_children and normal distribution",
                    linecol = "red",
                    length = 1000) 

#i decided to use basic version of thos variable, because i want to create interaction this variable with other

########################### VARIABLES CHECKING ###########################
#VARIABLE NURSERY_PLACES

jarque.bera.test(df1$nursery_places) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_nursery_places = log(df1$nursery_places)
jarque.bera.test(df1$ln_nursery_places) #p-value < 0.05, but p-value is higher

# nursery_places has values equal to 0, so I can't use the logarithm, because I get infinity values (log from 0 is equal to inf)

########################### VARIABLES CHECKING ###########################
#VARIABLE DOCTORS

jarque.bera.test(df1$doctors) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_doctors = log(df1$doctors)
jarque.bera.test(df1$ln_doctors) #p-value < 0.05, but p-value is higher

plotNormalHistogram(df1$doctors, prob = FALSE,
                    main = "doctors and normal distribution",
                    linecol = "red",
                    length = 1000) 

plotNormalHistogram(df1$ln_doctors,prob = FALSE,
                    main = "ln_doctors and normal distribution",
                    linecol = "red",
                    length = 1000) 

#ln_doctors will be placed in model

########################### VARIABLES CHECKING ###########################
#VARIABLE URBANISATION_RATE

jarque.bera.test(df1$urbanisation_rate) #p-value < 0.05, but p-value is higher than in the next case
df1$ln_urbanisation_rate = log(df1$urbanisation_rate)
jarque.bera.test(df1$ln_urbanisation_rate) #p-value < 0.05

g_urbanisation_rate<-ggplot(df1, aes(x=birth_rate, y=urbanisation_rate)) +geom_point(color="red")
g_urbanisation_rate

plotNormalHistogram(df1$urbanisation_rate, prob = FALSE,
                    main = "urbanisation_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

#urbanisation_rate without log appears to have a distribution more similar to the normal distribution

########################### VARIABLES CHECKING ###########################
#VARIABLE BUS STOPS

jarque.bera.test(df1$bus_stops) #p-value < 0.05, I reject H0 about normal distribution
df1$ln_bus_stops = log(df1$bus_stops)
jarque.bera.test(df1$ln_bus_stops) #p-value < 0.05, but p-value is higher

#ln_bus_stops will be placed in model

############################################################################################
########################### CREATE INTERACTION BETWEEN VARIABLES ###########################

df1$nurseryxchildren=df1$nursery_places*df1$children
df1$education_expenditurexchildren=df1$education_expenditure*df1$children

df1$houses_area_pc_2 = df1$houses_area_pc**2
df1$avg_people_per_house_2 = df1$avg_people_per_house**2
df1$house_ratio_2 = df1$house_ratio**2

###################################################################
########################### FIRST MODEL ###########################
#MODEL 1
model1=lm(ln_birth_rate~ln_covid19_cases + covid19_deaths + ln_covid19_quarantine + ln_marriage_rate +
            divorce_rate + budget_reve_pc + ln_unemployment_rate + women_reproductive + ln_femininity_ratio + avg_salary +
            ln_women_working + ln_men_working + ln_median_house_price + house_ratio + house_ratio_2 + houses_area_pc +
            houses_area_pc_2 + avg_people_per_house +avg_people_per_house_2+ education_expenditure + health_expenditure +
            ln_social_expenditure + family_expenditure + children + nursery_places + ln_doctors+ urbanisation_rate +
            ln_bus_stops + nurseryxchildren + education_expenditurexchildren, data=df1)
summary(model1)

#R^2 = 0.8022
#adj R^2 = 0.7852

##################################################################
########################### RESET TEST ###########################

library("lmtest")
library("foreign")

resettest(model1, power=2:3, type="fitted")
# p-value = 0.9149 and it's more than 0.05, OK

resettest(model1, power=2:3, type="regressor")
#p-value < 0.05, but the fitted version is OK

############################################################
########################### GETS ###########################

library("car")
linearHypothesis(model=model1, c("ln_covid19_quarantine=0")) #p-value is almost equal to 1, so variable is insignificant

linearHypothesis(model=model1, c("ln_covid19_quarantine=0", "houses_area_pc_2=0")) #p-value is almost equal to 1
#so variables are jointly insignificant

linearHypothesis(model=model1, c("ln_covid19_quarantine=0", "houses_area_pc_2=0", "houses_area_pc=0"))
#p-value is equal to 0.8873, so variables are jointly insignificant

linearHypothesis(model=model1, c("ln_covid19_quarantine=0", "houses_area_pc_2=0", "houses_area_pc=0", "ln_women_working=0"))
#p-value is equal to 0.9365, so variables are jointly insignificant

linearHypothesis(model=model1, c("ln_covid19_quarantine=0", "houses_area_pc_2=0", "houses_area_pc=0", "ln_women_working=0",
                                 "children=0"))
#p-value is equal to 0.9475, so variables are jointly insignificant

linearHypothesis(model=model1, c("ln_covid19_quarantine=0", "houses_area_pc_2=0", "houses_area_pc=0", "ln_women_working=0",
                                 "children=0", "budget_reve_pc=0"))
#p-value is equal to 0.9472, so variables are jointly insignificant

linearHypothesis(model=model1, c("ln_covid19_quarantine=0", "houses_area_pc_2=0", "houses_area_pc=0", "ln_women_working=0",
                                 "children=0", "budget_reve_pc=0", "nursery_places=0"))
#p-value is equal to 0.9533, so variables are jointly insignificant

linearHypothesis(model=model1, c("ln_covid19_quarantine=0", "houses_area_pc_2=0", "houses_area_pc=0", "ln_women_working=0",
                                 "children=0", "budget_reve_pc=0", "nursery_places=0", "education_expenditure=0"))
#p-value is equal to 0.7947, so variables are jointly insignificant

linearHypothesis(model=model1, c("ln_covid19_quarantine=0", "houses_area_pc_2=0", "houses_area_pc=0", "ln_women_working=0",
                                 "children=0", "budget_reve_pc=0", "nursery_places=0", "education_expenditure=0",
                                 "urbanisation_rate=0"))
#p-value is equal to 0.5255, so variables are jointly insignificant

linearHypothesis(model=model1, c("ln_covid19_quarantine=0", "houses_area_pc_2=0", "houses_area_pc=0", "ln_women_working=0",
                                 "children=0", "budget_reve_pc=0", "nursery_places=0", "education_expenditure=0",
                                 "urbanisation_rate=0", "health_expenditure=0"))
#p-value is equal to 0.4745, so variables are jointly insignificant

linearHypothesis(model=model1, c("ln_covid19_quarantine=0", "houses_area_pc_2=0", "houses_area_pc=0", "ln_women_working=0",
                                 "children=0", "budget_reve_pc=0", "nursery_places=0", "education_expenditure=0",
                                 "urbanisation_rate=0", "health_expenditure=0", "avg_people_per_house=0"))
#p-value is equal to 0.2057, so variables are jointly insignificant

linearHypothesis(model=model1, c("ln_covid19_quarantine=0", "houses_area_pc_2=0", "houses_area_pc=0", "ln_women_working=0",
                                 "children=0", "budget_reve_pc=0", "nursery_places=0", "education_expenditure=0",
                                 "urbanisation_rate=0", "health_expenditure=0", "avg_people_per_house=0",
                                 "ln_femininity_ratio=0"))
#p-value is equal to 0.06553, so variables are jointly insignificant

linearHypothesis(model=model1, c("ln_covid19_quarantine=0", "houses_area_pc_2=0", "houses_area_pc=0", "ln_women_working=0",
                                 "children=0", "budget_reve_pc=0", "nursery_places=0", "education_expenditure=0",
                                 "urbanisation_rate=0", "health_expenditure=0", "avg_people_per_house=0",
                                 "ln_femininity_ratio=0", "avg_people_per_house_2=0"))
#p-value is equal to 0.08579, so variables are jointly insignificant

#i remove variables which are jointly insignificant

####################################################################
########################### SECOND MODEL ###########################
#MODEL 2
model2=lm(ln_birth_rate~ln_covid19_cases + covid19_deaths + ln_marriage_rate + divorce_rate + ln_unemployment_rate +
            women_reproductive + avg_salary + ln_men_working + ln_median_house_price + house_ratio + house_ratio_2 +
            ln_social_expenditure + family_expenditure + ln_doctors + ln_bus_stops + nurseryxchildren +
            education_expenditurexchildren, data=df1)
summary(model2)

#R^2 = 0.7905
#adj R^2 = 0.7806

#variable ln_median_house_price is the one which is insignificant

linearHypothesis(model=model2, c("ln_median_house_price=0"))
#p-value of F test is 0.2069, so I remove variable from model

##################################################################
########################### RESET TEST ###########################

resettest(model2, power=2:3, type="fitted")
# OK, because p-value is more than 0.05

resettest(model2, power=2:3, type="regressor")
#p-value < 0.05, but the fitted version is OK

###################################################################
########################### THIRD MODEL ###########################

#MODEL 3
model3=lm(ln_birth_rate~ln_covid19_cases + covid19_deaths + ln_marriage_rate + divorce_rate + ln_unemployment_rate +
            women_reproductive + avg_salary + ln_men_working + house_ratio + house_ratio_2 + ln_social_expenditure +
            family_expenditure + ln_doctors + ln_bus_stops + nurseryxchildren + education_expenditurexchildren, data=df1)
summary(model3)

#R^2 = 0.7895
#adj R^2 = 0.7803 (so third model is a little bit worse than second)
#every variable is statistically significant

##################################################################
########################### RESET TEST ###########################

resettest(model3, power=2:3, type="fitted")
# OK, because p-value = 0.5948

resettest(model3, power=2:3, type="regressor")
#p-value < 0.05, but the fitted version is OK

# the functional form of the model is correct

##########################################################################
########################### BREUSCH PAGAN TEST ###########################

bptest(model3, studentize=TRUE)
#p-value = 0.6162 > 0.05; OK

########################################################################
########################### JARQUE-BERA TEST ###########################

jarque.bera.test(model3$residuals)
#p-value = 0.6282 > 0.05; OK

########################################################################
########################### DURBIN-WATSON TEST ###########################

dwtest(model3)


#breuscha godfreya
bgtest(model3)

#durbin watson





install.packages("stargazer")
library("stargazer")
stargazer(model2, model8,model9, model10, type="text", align= TRUE, style="default", df=FALSE)

stargazer(model2, model8,model9, model10, type="text", align= TRUE, style="default", df=FALSE, star.cutoffs=c(0.05, 0.01,0.001),keep.stat = c(adj.rsq,rsq))


summary(model10)$coefficients

plot(model7)

odporna1=coeftest(model10, vcov=vcovHC(model10, type="HC0"))
show(odporna1)
macierz_odporna=vcovHC(model10,type="HC0")
#porownanie oszacowan
stargazer(model10, odporna1, type="text", df=FALSE,
          column.labels = c("model10", "white"))


#procedura od ogolu do szczegolu
model7=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Ćwiczenie + Czy_duzo 
          + Budowa + Zdrowe_jedzenie + Tycie_n+ Miasto_250 + Miasto_100 + Miasto_20 
          + Wieś+ Wszystko + Jarska + Wegetarianizm +Wegańska + Średnie + Student + Podstawowe, data=waga2)
summary(model7)

odporna2=coeftest(model7, vcov=vcovHC(model7, type="HC0"))
show(odporna2)
macierz_odporna2=vcovHC(model7,type="HC0")

#czy zmienne nieistotne sa nieistotne 
linearHypothesis(model=model7, c("Wszystko=0","Jarska=0","Wegetarianizm=0","Wegańska=0", "Ćwiczenie=0","Zdrowe_jedzenie=0","Miasto_250=0","Miasto_100=0","Miasto_20=0","Wieś=0", "Średnie=0","Student=0","Podstawowe=0"), vcov. = macierz_odporna2)

#najbardziej nieistotna
model7_1=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Ćwiczenie + Czy_duzo 
          + Budowa + Zdrowe_jedzenie + Tycie_n+ Miasto_250 + Miasto_100 + Miasto_20 
          + Wieś+ Jarska + Wegetarianizm +Wegańska + Średnie + Student + Podstawowe, data=waga2)
summary(model7_1)
odporna2_1=coeftest(model7_1, vcov=vcovHC(model7_1, type="HC0"))
show(odporna2_1)
macierz_odporna2_1=vcovHC(model7_1,type="HC0")

#czy zmienne nieistotne sa nieistotne 2
linearHypothesis(model=model7, c("Wszystko=0", "Zdrowe_jedzenie=0"), vcov. = macierz_odporna2)
#0.985


#najbardziej nieistotna 2
model7_2=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Ćwiczenie + Czy_duzo 
            + Budowa + Tycie_n+ Miasto_250 + Miasto_100 + Miasto_20 
            + Wieś+ Jarska + Wegetarianizm +Wegańska + Średnie + Student + Podstawowe, data=waga2)
summary(model7_2)
odporna2_2=coeftest(model7_2, vcov=vcovHC(model7_2, type="HC0"))
show(odporna2_2)
macierz_odporna2_2=vcovHC(model7_2,type="HC0")

#czy zmienne nieistotne sa nieistotne 3
linearHypothesis(model=model7, c("Wszystko=0", "Zdrowe_jedzenie=0", "Miasto_100=0"), vcov. = macierz_odporna2)
#0.9964

#najbardziej nieistotna 3
model7_3=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Ćwiczenie + Czy_duzo 
            + Budowa + Tycie_n+ Miasto_250 + Miasto_20 
            + Wieś+ Jarska + Wegetarianizm +Wegańska + Średnie + Student + Podstawowe, data=waga2)
summary(model7_3)
odporna2_3=coeftest(model7_3, vcov=vcovHC(model7_3, type="HC0"))
show(odporna2_3)
macierz_odporna2_3=vcovHC(model7_3,type="HC0")

#czy zmienne nieistotne sa nieistotne 4
linearHypothesis(model=model7, c("Wszystko=0", "Zdrowe_jedzenie=0", "Miasto_100=0", "Wieś=0"), vcov. = macierz_odporna2)
#0.9974
#najbardziej nieistotna 3
model7_3=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Ćwiczenie + Czy_duzo 
            + Budowa + Tycie_n+ Miasto_250 + Miasto_20 
            + Jarska + Wegetarianizm +Wegańska + Średnie + Student + Podstawowe, data=waga2)
summary(model7_3)
odporna2_3=coeftest(model7_3, vcov=vcovHC(model7_3, type="HC0"))
show(odporna2_3)
macierz_odporna2_3=vcovHC(model7_3,type="HC0") 

#czy zmienne nieistotne sa nieistotne 5
linearHypothesis(model=model7, c("Wszystko=0", "Zdrowe_jedzenie=0", "Miasto_100=0", "Wieś=0", "Średnie=0"), vcov. = macierz_odporna2)
#0.9985

#najbardziej nieistotna 4
model7_4=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Ćwiczenie + Czy_duzo 
            + Budowa + Tycie_n+ Miasto_250 + Miasto_20 
            + Jarska + Wegetarianizm +Wegańska +Student + Podstawowe, data=waga2)
summary(model7_4)
odporna2_4=coeftest(model7_4, vcov=vcovHC(model7_4, type="HC0"))
show(odporna2_4)
macierz_odporna2_4=vcovHC(model7_4,type="HC0") 

#czy zmienne nieistotne sa nieistotne 5
linearHypothesis(model=model7, c("Wszystko=0", "Zdrowe_jedzenie=0", "Miasto_100=0", "Wieś=0", "Średnie=0", "Wegetarianizm=0"), vcov. = macierz_odporna2)
#0.9956

#najbardziej nieistotna 4
model7_5=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Ćwiczenie + Czy_duzo 
            + Budowa + Tycie_n+ Miasto_250 + Miasto_20 
            + Jarska + Wegańska +Student + Podstawowe, data=waga2)
summary(model7_5)
odporna2_5=coeftest(model7_5, vcov=vcovHC(model7_5, type="HC0"))
show(odporna2_5)
macierz_odporna2_5=vcovHC(model7_5,type="HC0") 

#czy zmienne nieistotne sa nieistotne 5
linearHypothesis(model=model7, c("Wszystko=0", "Zdrowe_jedzenie=0", "Miasto_100=0", "Wieś=0", "Średnie=0", "Wegetarianizm=0", "Jarska=0"), vcov. = macierz_odporna2)
#0.9941

#najbardziej nieistotna 4
model7_6=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Ćwiczenie + Czy_duzo 
            + Budowa + Tycie_n+ Miasto_250 + Miasto_20 
            + Wegańska +Student + Podstawowe, data=waga2)
summary(model7_6)
odporna2_6=coeftest(model7_6, vcov=vcovHC(model7_6, type="HC0"))
show(odporna2_6)
macierz_odporna2_6=vcovHC(model7_6,type="HC0") 

#czy zmienne nieistotne sa nieistotne 5"
linearHypothesis(model=model7, c("Wszystko=0", "Zdrowe_jedzenie=0", "Miasto_100=0", "Wieś=0", "Średnie=0", "Wegetarianizm=0", "Jarska=0", "Podstawowe=0"), vcov. = macierz_odporna2)
#0.9902

#najbardziej nieistotna 4
model7_7=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Ćwiczenie + Czy_duzo 
            + Budowa + Tycie_n+ Miasto_250 + Miasto_20 
            + Wegańska +Student, data=waga2)
summary(model7_7)
odporna2_7=coeftest(model7_7, vcov=vcovHC(model7_7, type="HC0"))
show(odporna2_7)
macierz_odporna2_7=vcovHC(model7_7,type="HC0") 

#czy zmienne nieistotne sa nieistotne 5"
linearHypothesis(model=model7, c("Wszystko=0", "Zdrowe_jedzenie=0", "Miasto_100=0", "Wieś=0", "Średnie=0", "Wegetarianizm=0", "Jarska=0", "Podstawowe=0", "Ćwiczenie=0"), vcov. = macierz_odporna2)
#0.9742

#najbardziej nieistotna 4
model7_8=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Czy_duzo 
            + Budowa + Tycie_n+ Miasto_250 + Miasto_20 
            + Wegańska +Student, data=waga2)
summary(model7_8)
odporna2_8=coeftest(model7_8, vcov=vcovHC(model7_8, type="HC0"))
show(odporna2_8)
macierz_odporna2_8=vcovHC(model7_8,type="HC0") 

#czy zmienne nieistotne sa nieistotne 5"
linearHypothesis(model=model7, c("Wszystko=0", "Zdrowe_jedzenie=0", "Miasto_100=0", "Wieś=0", "Średnie=0", "Wegetarianizm=0", "Jarska=0", "Podstawowe=0", "Ćwiczenie=0", "Miasto_250=0"), vcov. = macierz_odporna2)
#0.829

#najbardziej nieistotna 4
model7_9=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Czy_duzo 
            + Budowa + Tycie_n+ Miasto_20 
            + Wegańska +Student, data=waga2)
summary(model7_9)
odporna2_9=coeftest(model7_9, vcov=vcovHC(model7_9, type="HC0"))
show(odporna2_9)
macierz_odporna2_9=vcovHC(model7_9,type="HC0") 

#czy zmienne nieistotne sa nieistotne 5"
linearHypothesis(model=model7, c("Wszystko=0", "Zdrowe_jedzenie=0", "Miasto_100=0", "Wieś=0", "Średnie=0", "Wegetarianizm=0", "Jarska=0", "Podstawowe=0", "Ćwiczenie=0", "Miasto_250=0", "Miasto_20=0"), vcov. = macierz_odporna2)
#0.766

#najbardziej nieistotna 4
model7_10=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Czy_duzo 
            + Budowa + Tycie_n
            + Wegańska +Student, data=waga2)
summary(model7_10)
odporna2_10=coeftest(model7_10, vcov=vcovHC(model7_10, type="HC0"))
show(odporna2_10)
macierz_odporna2_10=vcovHC(model7_10,type="HC0") 

#wszystkie istotne

model11=lm(Waga~Wiek + Wzrost + Czy_kobieta + Budowa +Czy_duzo+Tycie_n+Wegańska+Student, data=waga2)
summary(model11)

plot(model11)

summary(model11)$coefficients

plot(model11)
bptest(model11)
#nie ma homoskedastycznosci

summary(model11)

#jak starczy czasu dodac metody MNK UMNK itp
plot(waga2$Wiek, model11$residuals)

WLS = lm(Waga~Wiek + Wzrost + Czy_kobieta + Budowa +Czy_duzo+Tycie_n+Wegańska+Student, data=waga2, weights=1/sqrt(Wiek))

library("stargazer")
stargazer(WLS, model11, type="text", df=FALSE, dep.var.labels.include = FALSE )

plot(waga2$Wiek, WLS$residuals)

#wykresy reszt z obydwu modeli

plot(waga2$Wiek, model11$residuals)
points(waga2$Wiek, WLS$residuals, col="red")

bptest(model11, ~Wzrost,data=waga2)
gqtest(waga2$Wzrost~)
# p-value = 0.8533 nie ma heteroskedastycznosci ze zmienna Wzrost

bptest(model11, ~Wiek,data=waga2)
# p-value=0.5475





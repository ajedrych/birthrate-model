install.packages("lmtest")
install.packages("sandwich")
install.packages("knitr")
install.packages("tidyverse")
library("readxl")
library("lmtest")
library("sandwich")
library("knitr")
library("tidyverse")
library("lmtest")
library("foreign")

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

########################### VARIABLES CHECKING ###########################
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

#birth_rate with log appears to have distribution more similar to the normal distribution

########################### CREATE INTERACTION BETWEEN VARIABLES ###########################

df1$nurseryxchildren=df1$nursery_places*df1$children
df1$education_expenditurexchildren=df1$education_expenditure*df1$children

df1$avg_salary_2 = df1$avg_salary**2

########################### FIRST MODEL ###########################
#MODEL 1
model1=lm(ln_birth_rate~covid19_cases + covid19_deaths + covid19_quarantine + marriage_rate + divorce_rate + 
            budget_reve_pc + unemployment_rate + women_reproductive + femininity_ratio +avg_salary_2+ women_working +
            men_working + median_house_price + house_ratio + houses_area_pc + avg_people_per_house + education_expenditurexchildren +
            health_expenditure + social_expenditure + family_expenditure + nurseryxchildren + doctors  +
            urbanisation_rate + bus_stops, data=df1)
summary(model1)

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

#covid19_quarantine with log appears to have a distribution more similar to the normal distribution

########################### SECOND MODEL ###########################
#MODEL 2
model2=lm(ln_birth_rate~covid19_cases + covid19_deaths + ln_covid19_quarantine + marriage_rate + divorce_rate + 
            budget_reve_pc + unemployment_rate + women_reproductive + femininity_ratio + avg_salary_2 + women_working +
            men_working + median_house_price + house_ratio + houses_area_pc + avg_people_per_house + education_expenditurexchildren +
            health_expenditure + social_expenditure + family_expenditure + nurseryxchildren + doctors  +
            urbanisation_rate + bus_stops, data=df1)
summary(model2)

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

library("car")
linearHypothesis(model=model2, c("budget_reve_pc=0"))

# p-value of F test is higher than 0.05, so budget_reve_pc is insignificant
# remove variable budget_reve_pc from model

########################### THIRD MODEL ###########################
# MODEL 3
model3=lm(ln_birth_rate~covid19_cases + covid19_deaths + ln_covid19_quarantine + marriage_rate + divorce_rate + 
            unemployment_rate + women_reproductive + femininity_ratio + avg_salary_2 + women_working +
            men_working + median_house_price + house_ratio + houses_area_pc + avg_people_per_house + education_expenditurexchildren +
            health_expenditure + social_expenditure + family_expenditure + nurseryxchildren + doctors  +
            urbanisation_rate + bus_stops, data=df1)
summary(model3)

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


#women_working with log appears to have a distribution more similar to the normal distribution, but if we change women_working
#to ln_women_working in model, we get a little bit worse r^2 statistics and p-value of variable is still the highest

linearHypothesis(model=model2, c("budget_reve_pc=0", "women_working=0"))

# p-value of F test is higher than 0.05, so budget_reve_pc and women_working are jointly insignificant
# remove variable women_working from model

########################### FOURTH MODEL ###########################
#MODEL 4
model4=lm(ln_birth_rate~covid19_cases + covid19_deaths + ln_covid19_quarantine + marriage_rate + divorce_rate + 
            unemployment_rate + women_reproductive + femininity_ratio + avg_salary_2 +
            men_working + median_house_price + house_ratio + houses_area_pc + avg_people_per_house + education_expenditurexchildren +
            health_expenditure + social_expenditure + family_expenditure + nurseryxchildren + doctors  +
            urbanisation_rate + bus_stops, data=df1)
summary(model4)

########################### VARIABLES CHECKING ###########################
#VARIABLE AVG_PEOPLE_PER_HOUSE
g_avg_people_per_house<-ggplot(df1, aes(x=birth_rate, y=avg_people_per_house)) +geom_point(color="red")
g_avg_people_per_house

plotNormalHistogram(df1$avg_people_per_house, prob = FALSE,
                    main = "women_working and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_avg_people_per_house = log(df1$avg_people_per_house)

plotNormalHistogram(df1$ln_avg_people_per_house, prob = FALSE,
                    main = "ln_avg_people_per_house and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_avg_people_per_house<-ggplot(df1, aes(x=birth_rate, y=ln_avg_people_per_house)) +geom_point(color="red")
g_ln_avg_people_per_house

df1$avg_people_per_house_2 = df1$avg_people_per_house**2

g_avg_people_per_house_2<-ggplot(df1, aes(x=birth_rate, y=avg_people_per_house_2)) +geom_point(color="red")
g_avg_people_per_house_2

plotNormalHistogram(df1$avg_people_per_house_2, prob = FALSE,
                    main = "women_working and normal distribution",
                    linecol = "red",
                    length = 1000)

#avg_people_per_house without log appears to have a distribution more similar to the normal distribution, but if we try to change avg_people_per_house
#to ln_avg_people_per_house in model, we get a little bit worse r^2 statistics and p-value of variable is still the highest
#the same situation if I try to square the variable

linearHypothesis(model=model2, c("budget_reve_pc=0", "women_working=0", "avg_people_per_house=0"))

# p-value of F test is higher than 0.05, so variables are jointly insignificant
# remove variable avg_people_per_house from model

########################### FIFTH MODEL ###########################
#MODEL 5
model5=lm(ln_birth_rate~covid19_cases + covid19_deaths + ln_covid19_quarantine + marriage_rate + divorce_rate + 
            unemployment_rate + women_reproductive + femininity_ratio + avg_salary_2 +
            men_working + median_house_price + house_ratio + houses_area_pc + education_expenditurexchildren +
            health_expenditure + social_expenditure + family_expenditure + nurseryxchildren + doctors  +
            urbanisation_rate + bus_stops, data=df1)
summary(model5)

########################### VARIABLES CHECKING ###########################
#VARIABLE URBANISATION_RATE
g_urbanisation_rate<-ggplot(df1, aes(x=birth_rate, y=urbanisation_rate)) +geom_point(color="red")
g_urbanisation_rate

plotNormalHistogram(df1$urbanisation_rate, prob = FALSE,
                    main = "urbanisation_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_urbanisation_rate = log(df1$urbanisation_rate)

plotNormalHistogram(df1$ln_urbanisation_rate, prob = FALSE,
                    main = "ln_urbanisation_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_ln_urbanisation_rate<-ggplot(df1, aes(x=birth_rate, y=ln_urbanisation_rate)) +geom_point(color="red")
g_ln_urbanisation_rate

#urbanisation_rate without log appears to have a distribution more similar to the normal distribution, but if we try to change urbanisation_rate
#to ln_urbanisation_rate in model, we get a little bit worse r^2 statistics and p-value of variable is still the highest

linearHypothesis(model=model2, c("budget_reve_pc=0", "women_working=0", "avg_people_per_house=0", "urbanisation_rate=0"))

# p-value of F test is higher than 0.05, so variables are jointly insignificant
# remove variable avg_people_per_house from model

########################### SIXTH MODEL ###########################
#MODEL 6
model6=lm(ln_birth_rate~covid19_cases + covid19_deaths + ln_covid19_quarantine + marriage_rate + divorce_rate + 
            unemployment_rate + women_reproductive + femininity_ratio + avg_salary_2 +
            men_working + median_house_price + house_ratio + houses_area_pc + education_expenditurexchildren +
            health_expenditure + social_expenditure + family_expenditure + nurseryxchildren + doctors + bus_stops, data=df1)
summary(model6)

#the only statistically insignificant variable is ln_covid19_quarantine, which we have already logarithmised,

linearHypothesis(model=model2, c("budget_reve_pc=0", "women_working=0", "avg_people_per_house=0", "urbanisation_rate=0",
                                 "ln_covid19_quarantine=0"))

# p-value of F test is higher than 0.05, so variables are jointly insignificant
# remove variable ln_covid19_quarantine from model


########################### SEVENTH MODEL ###########################
#MODEL 7
model7=lm(ln_birth_rate~covid19_cases + covid19_deaths + marriage_rate + divorce_rate + 
            unemployment_rate + women_reproductive + femininity_ratio + avg_salary_2 +
            men_working + median_house_price + house_ratio + houses_area_pc + education_expenditurexchildren +
            health_expenditure + social_expenditure + family_expenditure + nurseryxchildren + doctors + bus_stops, data=df1)
summary(model7)

########################### VARIABLES CHECKING ###########################
#VARIABLE FEMININITY_RATE
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


#femininity_ratio without log appears to have a distribution more similar to the normal distribution, but if we try to change femininity_ratio
#to ln_femininity_ratio in model, we get a little bit worse r^2 statistics and p-value of variable is still the highest

linearHypothesis(model=model2, c("budget_reve_pc=0", "women_working=0", "avg_people_per_house=0", "urbanisation_rate=0",
                                 "ln_covid19_quarantine=0", "femininity_ratio=0"))

# p-value of F test is higher than 0.05, so variables are jointly insignificant
# remove variable femininity_ratio from model

########################### EIGHTH MODEL ###########################
#MODEL 8
model8=lm(ln_birth_rate~covid19_cases + covid19_deaths + marriage_rate + divorce_rate + 
            unemployment_rate + women_reproductive + avg_salary_2 +
            men_working + median_house_price + house_ratio + houses_area_pc + education_expenditurexchildren +
            health_expenditure + social_expenditure + family_expenditure + nurseryxchildren + doctors + bus_stops, data=df1)
summary(model8)


########################### TESTS ###########################

########################### RESET TEST ###########################

library("lmtest")
library("foreign")

resettest(model8, power=2:3, type="fitted")

# OK, because p-value is more than 0.05

resettest(model8, power=2:3, type="regressor")
#p-value < 0.05, but the fitted version is OK





















plotNormalHistogram(df1$marriage_rate, prob = FALSE,
                    main = "marriage_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_marriage_rate = log(df1$marriage_rate)

plotNormalHistogram(df1$ln_marriage_rate, prob = FALSE,
                    main = "ln_marriage_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_marriage<-ggplot(df1, aes(x=birth_rate, y=marriage_rate)) +geom_point(color="red")
g_marriage

#marriage_rate without log appears to have a distribution more similar to the normal distribution

#UNEMPLOYMENT RATE
plotNormalHistogram(df1$unemployment_rate, prob = FALSE,
                    main = "unemployment_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

df1$ln_unemployment_rate = log(df1$unemployment_rate)

plotNormalHistogram(df1$ln_unemployment_rate, prob = FALSE,
                    main = "unemployment_rate and normal distribution",
                    linecol = "red",
                    length = 1000) 

g_unemployment<-ggplot(df1, aes(x=birth_rate, y=unemployment_rate)) + geom_point(color="red")
g_unemployment

g_ln_unemployment<-ggplot(df1, aes(x=birth_rate, y=ln_unemployment_rate)) + geom_point(color="red")
g_ln_unemployment

#log from unemployment_rate looks better

########################### ADD variablle  ###########################

df1$budget_reve_pc_2 = df1$budget_reve_pc**2
df1$women_reproductive_2 = df1$women_reproductive**2

########################### MODEL 2 ###########################

#MODEL2
model2=lm(ln_birth_rate~covid19_cases + covid19_deaths + covid19_quarantine + marriage_rate + divorce_rate + 
            budget_reve_pc + ln_unemployment_rate + women_reproductive + femininity_ratio + avg_salary + women_working +
            men_working + median_house_price + house_ratio + houses_area_pc + avg_people_per_house + education_expenditurexchildren +
            health_expenditure + social_expenditure + family_expenditure + nurseryxchildren + doctors + family_benefits +
            urbanisation_rate + bus_stops, data=df1)
summary(model2)

########################### VARIABLES CHECKING ###########################






#MODEL 1
model1=lm(ln_birth_rate~covid19_cases + covid19_deaths + covid19_quarantine + marriage_rate + divorce_rate + 
            budget_reve_pc + ln_unemployment_rate + women_reproductive + femininity_ratio + avg_salary + women_working +
            men_working + median_house_price + house_ratio + houses_area_pc + avg_people_per_house + education_expenditurexchildren +
            health_expenditure + social_expenditure + family_expenditure + nurseryxchildren + doctors + family_benefits +
            urbanisation_rate + bus_stops, data=df1)
summary(model1)

#test reset
resettest(model4, power=2:3, type="fitted")


#MODEL 2
model1=lm(ln_birth_rate~covid19_cases + covid19_deaths + covid19_quarantine + marriage_rate + divorce_rate + 
            budget_reve_pc + unemployment_rate + women_reproductive + femininity_ratio + avg_salary + women_working +
            men_working + median_house_price + house_ratio + houses_area_pc + avg_people_per_house + education_expenditure +
            health_expenditure + social_expenditure + family_expenditure + children + nursery_places + doctors + family_benefits +
            population + urbanisation_rate + bus_stops, data=df1)
summary(model1)

#AVERAGE SALARY
g_avg_salary<-ggplot(df1, aes(x=birth_rate, y=avg_salary)) + geom_point(color="red")
g_avg_salary

df1$ln_avg_salary = log(df1$avg_salary)
hist(df1$avg_salary)
hist(df1$ln_avg_salary) #log from avg_salary looks better

g_ln_avg_salary<-ggplot(df1, aes(x=birth_rate, y=ln_avg_salary)) + geom_point(color="red")
g_ln_avg_salary

#INTERACTION
df1$nurseryxchildren=df1$nursery_places*df1$children
df1$education_expenditurexchildren=df1$education_expenditure*df1$children

df1$urbanisation_rate_2 = df1$urbanisation_rate*df1$urbanisation_rate




#DIVORCE RATE
hist(df1$divorce_rate) 
df1$ln_divorce_rate = log(df1$divorce_rate)
hist(df1$ln_divorce_rate) #divorce_rate with log appears to have a distribution more similar to the normal distribution

g_divorce<-ggplot(df1, aes(x=birth_rate, y=divorce_rate)) +geom_point(color="red")
g_divorce

g_ln_divorce<-ggplot(df1, aes(x=birth_rate, y=ln_divorce_rate)) +geom_point(color="red")
g_ln_divorce



#WOMEN_WORKING
hist(df1$women_working) 
df1$ln_women_working = log(df1$women_working)
hist(df1$ln_women_working) #women_working with log appears to have a distribution more similar to the normal distribution

#MODEL 3
model3=lm(ln_birth_rate~if_university + covid19_cases + covid19_deaths + covid19_quarantine + marriage_rate + ln_divorce_rate + 
            budget_reve_pc + ln_unemployment_rate + women_reproductive + femininity_ratio + avg_salary + ln_women_working +
            women_working_all + median_house_price + house_ratio + health_expenditure + social_expenditure +
            family_expenditure + doctors + urbanisation_rate + bus_stops + nurseryxchildren, data=df1)
summary(model3)

resettest(model6, power=2:3, type="fitted")

hist(df1$covid19_cases)
df1$ln_covid19_cases = log(df1$covid19_cases)
hist(df1$ln_covid19_cases)

hist(df1$covid19_deaths)
df1$ln_covid19_cases = log(df1$covid19_deaths)
hist(df1$ln_covid19_cases)



model2=lm(birth_rate~covid19_cases+covid19_deaths+covid19_quarantine+marriage_rate+divorce_rate+budget_reve_pc+unemployment_rate+women_reproductive+femininity_ratio+avg_salary+women_working+
            median_house_price+house_ratio, data=df1)
summary(model2)
resettest(model2, power=2:3, type="fitted")
#test reset
library("lmtest")
library("foreign")

#p-value < 0.05 czyli odrzucamy h0 o prawidłowej formie funkcyjnej

resettest(model8, power=2:3, type="regressor")


model3=lm(birth_rate~covid19_cases+covid19_deaths+covid19_quarantine+marriage_rate+divorce_rate+budget_reve_pc+unemployment_rate+women_reproductive+femininity_ratio+avg_salary+women_working+
            median_house_price+house_ratio+education_expenditure+health_expenditure+social_expenditure+family_expenditure, data=df1)
summary(model3)

resettest(model2, power=2:3, type="fitted")

model3=lm(lnWaga~Wiek + Wzrost + Dieta_n + Ćwiczenie + Czy_duzo + Średnie + Student + Podstawowe, data=waga2)
summary(model3)

model4=lm(lnWaga~Wiek + Wzrost + Dieta_n + Ćwiczenie + Czy_duzo + Budowa, data=waga2)
summary(model4)

model5=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Dieta_n + Ćwiczenie + Czy_duzo + Budowa, data=waga2)
summary(model5)

model6=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Dieta_n + Ćwiczenie + Czy_duzo + Budowa + Zdrowe_jedzenie, data=waga2)
summary(model6)

model7=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Dieta_n + Ćwiczenie + Czy_duzo + Budowa + Zdrowe_jedzenie + Tycie_n++  Miasto_250 + Miasto_100 + Miasto_20 + Wieś+ Wszystko + Jarska + Wegetarianizm +Wegańska + Średnie + Student + Podstawowe, data=waga2)
summary(model7)

model8=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Dieta_n + Ćwiczenie + Czy_duzo + Budowa + Zdrowe_jedzenie + Tycie_n + Czy_choroba_n+ Wszystko + Jarska + Wegetarianizm +Wegańska, data=waga2)
summary(model8)

model9=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Dieta_n + Ćwiczenie + Czy_duzo + Budowa + Zdrowe_jedzenie + Tycie_n + Czy_choroba_n + Czy_ogranicza_m, data=waga2)
summary(model9)

model10=lm(lnWaga~Wiek + Czy_kobieta+ Wzrost + Czy_duzo + Budowa + Tycie_n + Czy_choroba_n,  data=waga2)
summary(model10)

install.packages("stargazer")
library("stargazer")
stargazer(model2, model8,model9, model10, type="text", align= TRUE, style="default", df=FALSE)

stargazer(model2, model8,model9, model10, type="text", align= TRUE, style="default", df=FALSE, star.cutoffs=c(0.05, 0.01,0.001),keep.stat = c(adj.rsq,rsq))

install.packages("car")
library("car")
linearHypothesis(model=model7, c("Wszystko=0","Jarska=0","Wegetarianizm=0","Wegańska=0"))

linearHypothesis(model=model7, c( "Miasto_250=0","Miasto_100=0","Miasto_20=0","Wieś=0"))

linearHypothesis(model=model3, c( "Średnie=0","Student=0","Podstawowe=0"))

linearHypothesis(model=model10, c( "Czy_choroba_n=0"))


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





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

#loading data
df <- read_excel("baza_ekon.xlsx") 
View(df)

#delete NA
df1<-na.omit(df)
str(df1) #types of variables

pairs(df1[,0:10]) 


library(corrplot)
kor<-cor(df1)
corrplot(kor, method="circle")

#DEPENDENT VARIABLE - BIRTH RATE
par(mfrow=c(1,2))  

h <- hist(df1$irth_rate)
xfit <- seq(min(df1$birth_rate), max(df1$birth_rate), length = 40) 
yfit <- dnorm(xfit, mean = mean(df1$birth_rate), sd = sd(df1$birth_rate)) 
yfit <- yfit * diff(h$mids[1:2]) * length(df1$birth_rate) 
lines(xfit, yfit, col = "red", lwd = 2)

df1$ln_birth_rate = log(df1$birth_rate)

h <- hist(df1$ln_birth_rate)
xfit <- seq(min(df1$ln_birth_rate), max(df1$ln_birth_rate), length = 40) 
yfit <- dnorm(xfit, mean = mean(df1$ln_birth_rate), sd = sd(df1$ln_birth_rate)) 
yfit <- yfit * diff(h$mids[1:2]) * length(df1$ln_birth_rate) 
lines(xfit, yfit, col = "red", lwd = 2)

#birth_rate with log appears to have distribution more similar to the normal distribution

#MODEL 1
model1=lm(ln_birth_rate~covid19_cases + covid19_deaths + covid19_quarantine + marriage_rate + divorce_rate + 
            budget_reve_pc + unemployment_rate + women_reproductive + femininity_ratio + avg_salary + women_working +
            men_working+ median_house_price + education_expenditure + health_expenditure + social_expenditure +
            family_expenditure +children+nursery_places+ doctors+family_benefits +population+houses+houses_area+ urbanisation_rate + bus_stops, data=df1)
summary(model1)

#test reset
resettest(model1, power=2:3, type="fitted")

#VARIABLE MARRIAGE RATE
hist(df1$marriage_rate) #marriage_rate without log appears to have a distribution more similar to the normal distribution
df1$ln_marriage_rate = log(df1$marriage_rate)
hist(df1$ln_marriage_rate)

g_marriage<-ggplot(df1, aes(x=birth_rate, y=marriage_rate)) +geom_point(color="red")
g_marriage

#UNEMPLOYMENT RATE
hist(df1$unemployment_rate)

df1$ln_unemployment_rate = log(df1$unemployment_rate)

hist(df1$ln_unemployment_rate) #log from unemployment_rate looks better

h <- hist(df1$unemployment_rate)
xfit <- seq(min(df1$unemployment_rate), max(df1$unemployment_rate), length = 40) 
yfit <- dnorm(xfit, mean = mean(df1$unemployment_rate), sd = sd(df1$unemployment_rate)) 
yfit <- yfit * diff(h$mids[1:2]) * length(df1$unemployment_rate) 
lines(xfit, yfit, col = "red", lwd = 2)

h <- hist(df1$ln_unemployment_rate)
xfit <- seq(min(df1$ln_unemployment_rate), max(df1$ln_unemployment_rate), length = 40) 
yfit <- dnorm(xfit, mean = mean(df1$ln_unemployment_rate), sd = sd(df1$ln_unemployment_rate)) 
yfit <- yfit * diff(h$mids[1:2]) * length(df1$ln_unemployment_rate) 
lines(xfit, yfit, col = "red", lwd = 2)

g_unemployment<-ggplot(df1, aes(x=birth_rate, y=unemployment_rate)) + geom_point(color="red")
g_unemployment

g_ln_unemployment<-ggplot(df1, aes(x=birth_rate, y=ln_unemployment_rate)) + geom_point(color="red")
g_ln_unemployment

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

resettest(model1, power=2:3, type="fitted")

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

resettest(model5, power=2:3, type="regressor")


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





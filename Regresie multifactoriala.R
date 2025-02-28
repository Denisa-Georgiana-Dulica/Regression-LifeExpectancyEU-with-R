# Setarea mediului de lucru
rm(list = ls()) 
directory <- "C:/Users/User/R_project/Proiect_econometrie/"

# Instalarea si activarea pachetelor
PackageNames <- c("tidyverse", "stargazer", "magrittr", "car","ICglm")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}
install.packages("dplyr")
library(dplyr)
install.packages("stargazer")
library(stargazer)

global_health <- read.csv(paste0(directory, "global_health_cross_section.csv"), sep = ";")

#================================================================================================================================
#Definirea indicatorilor utilizati
global_health %>% 
  select(Life_Expectancy,Sanitary_Expense_Per_Capita, Water_Access_Percent, Unemployment_Rate,Fertility_Rate )%>%
  str #42 observatii si 4 variabile

#Sanitary_Expense_Per_Capita=cheltuielile sanitare (legate de sănătate) pe cap de locuitor
#                           =suma medie cheltuită pentru sănătate pentru fiecare individ dintr-o țară
#                           -> DOLARI
#Water_Access_Percent=Procentul populatiei cu acces la apa potabila -> PROCENT DIN POPULATIE
#Fertility_Rate=Rata fertilității
#              =numărul mediu de copii născuti de o femeie de-a lungul vietii într-o țară
#               -> NR COPII
#Unemployment_Rate= Rata șomajului
#                 = procentul din forța de muncă activă care nu are un loc de muncă
#                 -> PROCENT DIN FORTA DE MUNCA
#Life Expentancy =  reprezintă numărul mediu de ani pe care o persoană se așteaptă să îi trăiască de la naștere, presupunând că ratele de mortalitate curente rămân constante pe durata vieții
#                 -> ANI

#Aceste variabile sunt recunoscute ca character, trebuie să o convertim în numeric.
global_health$Sanitary_Expense_Per_Capita <- as.numeric(
  gsub(",", ".", global_health$Sanitary_Expense_Per_Capita)
)
global_health$Life_Expectancy <- as.numeric(
  gsub(",", ".", global_health$Life_Expectancy)
)
global_health$Water_Access_Percent <- as.numeric(
  gsub(",", ".", global_health$Water_Access_Percent)
)
global_health$Fertility_Rate <- as.numeric(
  gsub(",", ".", global_health$Fertility_Rate)
)
global_health$Unemployment_Rate <- as.numeric(
  gsub(",", ".", global_health$Unemployment_Rate)
)

#imputeazam valorile lipsă (NA) din coloana Water_Access_Percent cu mediana valorilor existente (o singura valoare lipsa)
global_health$Water_Access_Percent[is.na(global_health$Water_Access_Percent)] <- 
  median(global_health$Water_Access_Percent, na.rm = TRUE)

#==========================================================================================================================================================================
#STATISTICI DESCRIPTIVE
global_health %>% 
  select(Life_Expectancy,Sanitary_Expense_Per_Capita, Water_Access_Percent, Unemployment_Rate,Fertility_Rate) %>%
  stargazer(type = "text")

#media:
#Fertility_Rate -> În medie, o femeie are 1.59 copii de-a lungul vieții 
#Water_Access_Percent -> În medie, 92.597% din populație are acces la apă curată
#Unemployment_Rate -> În medie, aproximativ 7.379% din populația activă este șomeră
#Sanitary_Expense_Per_Capita -> în medie, țările din eșantion cheltuiesc aproximativ 3305 USD pe cap de locuitor pentru sănătate

#abaterea standard:
#Water_Access_Percent: 9.401. Procentul de acces la apă pentru țările analizate este apropiat de media de 92.597%
#Sanitary_Expense_Per_Capita: 2879,21 USD. Deviație standard mare care sugerează că există diferențe mari între țări în ceea ce privește cheltuielile pentru sănătate
#Unemployment_Rate: 3.803. Abaterea standard este aproape jumătate din media șomajului, iar acest lucru arată că diferențele între țări sunt destul de mari.
#Fertility_Rate:  0.340. Variabilitate redusă, ceea ce spune că majoritatea țărilor au rate similare de fertilitate


# Histogramă pentru fiecare variabilă
hist(global_health$Life_Expectancy, 
     main = "Distribution Life_Expectancy", 
     xlab = "Life_Expectancy", 
     col = "blue", 
     breaks = 20)
#Graficul arată o distribuție neuniformă a speranței de viață, cu două grupuri principale de 68-78 de ani și altul peste 80> de ani. 
#Lipsa valorilor sugerează o separare între țările cu speranță de viață moderată (țări în dezvoltare) și ridicată (țări dezvoltate), frecvența cea mai mare fiind pentru speranța de viață în jurul valorii 80 ani în țările dezvoltate.

hist(global_health$Sanitary_Expense_Per_Capita, 
     main = "Distribution Sanitary_Expense_Per_Capita", 
     xlab = "Sanitary_Expense_Per_Capita", 
     col = "blue", 
     breaks = 20)

#Distribuție asimetrică (pozitivă). Există câteva valori foarte mari (la capătul din dreapta), ceea ce sugerează că există câteva observații extreme
#Intervalul 0-2000 este dominant, ceea ce arată că majoritatea țărilor investesc puțin în cheltuielile sanitare per capita

hist(global_health$Water_Access_Percent,
     main = "Distribution Water_Access_Percent", 
     xlab = "Water_Access_Percent", 
     col = "green", 
     breaks = 20)
#Majoritatea țărilor au un procent foarte ridicat de acces la apă curată, situându-se în intervalul 95-100%, dar există câteva țări cu acces mai scăzut la apă, în intervalele 70-90%

hist(global_health$Unemployment_Rate, 
     main = "Distribution Unemployment_Rate", 
     xlab = "Unemployment_Rate", 
     col = "red", 
     breaks = 20)

#Majoritatea țărilor sunt concentrate în intervalul 4-8%, rata șomajului este relativ mică.
#Există câteva valori izolate în jurul 15% -> în anumite cazuri ratele șomajului sunt mult mai mari decât media

#======================================================================================================================================================================
#Incep cu un model de regresie simpla cu o variabila independenta si adaug pe rand variabilele celelalte independente

#In cazurile următoare se îndeplinește prima ipoteză fundamentală legată de LINIARITATEA MODELELOR DE REGRESIE 

# Regresia simpla (regresie unifactoriala 1 variabila dependenta depinde de 1 variabila independenta)
# global_health = beta0 + beta1*Sanitary_Expense_Per_Capita + u
model_simple <- lm(Life_Expectancy ~ Sanitary_Expense_Per_Capita, data = global_health) #var dependenta~var independenta
summary(model_simple)

#Termenul de interceptare < 2e-16 e semnificativ statistic pt ca am valoarea < 5% =0.05
#Valoarea p este 1.55e-11 = 1.55 * 10^-11, ceea ce indică faptul că această variabilă independentă Sanitary_Expense_Per_Capita  este semnificativă statistic (p < 0.05), deci influențează în mod semnificativ variabila dependentă Life_Expectancy.
#Dacă Sanitary_Expense_Per_Capita crește cu o unitate (1 dolar), atunci Life_Expectancy crește, în medie, cu 1.383e-03 ani (sau 0.001383 ani). 
#Asta înseamnă că pentru fiecare dolar suplimentar cheltuit pe cap de locuitor pentru sănătate, speranța de viață crește cu aproximativ 0.5 zile (0.001383 ani × 365 zile ≈ 0.5 zile).

#Interpretare bonitate model: R^2=0.6832 si R^2 ajustat=0.6753
#R-squared = măsoară proporția din variabilitatea variabilei dependente care este explicată de variabilele independente din model.
#68.32% din variația în Life Expectancy este explicată de Sanitary_Expense_Per_Capita
#R² ajustat fiind apropiat de R² sugerează că modelul nu este supraajustat și că variabila explicativă aleasă este relevantă.

#Testarea validitatii modelului (cu testul F)
#p-value: 1.552e-11 (valoare foarte mica < 0.01 --> resping ipoteza nulă) --> putem afirma cu o probabilitate de 99% ca modelul este valid din punct de vedere statistic


# Regresie multipla --------------------------------------------------------------------------------------------------------------------------------------------
#  Life_Expectancy = beta0 + beta1*Sanitary_Expense_Per_Capita + beta2*Water_Access_Percent + u

model_multiple1 <- lm(Life_Expectancy ~ Sanitary_Expense_Per_Capita+Water_Access_Percent, data = global_health)
summary(model_multiple1)
#Termenul de interceptare reprezintă valoarea estimată a Life Expectancy (Speranța de viață) atunci când Sanitary Expense Per Capita (Cheltuielile sanitare pe cap de locuitor) și Water Access Percent (Procentul de acces la apă) sunt egale cu zero.
#Dacă Sanitary_Expense_Per_Capita și Water_Access_Percent sunt amândouă zero (adică nu există cheltuieli sanitare și acces la apa 0%), atunci Life_Expectancy este estimată să fie 59.18 ani (Estimate).
#Având în vedere că p-value 1.5e-15 < 0.01, termenul de interceptare este semnificativ statistic

#Daca Sanitary_Expense_Per_Capita creste cu o unitate (1 dolar), speranța de viață (Life Expectancy) crește cu 0.001053 ani. (Estimate)
#p-value este 4.6e-07< 0.01, coeficientul este semnificativ statistic-->relație semnificativă între cheltuielile sanitare și speranța de viață.

# Daca Water Access Percent creștere cu 1% din procentului de acces la apă, speranța de viață crește cu 0.1612 ani.
# p-value este 0.00445 < 0.01, coeficientul este semnificativ statistic--> accesul la apă influențează semnificativ speranța de viață.

#Interpretare bonitate model:
#Adjusted R-squared (0.7301) indică  faptul că aproape 73.01% din variabilitatea speranței de viață este explicată de modelul. Ne axăm pe interpretarea R^2 ajustat, deoarece R^2 creste cand adaugam v.i chiar daca acesti nu au o semnificatie statictica, iar R^2 ajustat penalizează modelele cu v.i care nu sunt semnificative
#observam ca R-squared ajustat crește de la 0.6753 la 0.7301, iar acest lucru sugerează că variabila Water_Access_Percent aduce o îmbunătățire reală a modelului, nu doar o creștere artificială a R-squared

#Testarea validitatii modelului (cu testul F)
#p-value =3.054e-12<0.01, ceea ce înseamnă că resping ipoteza nulă (care ar spune că modelul nu este semnificativ) și spunem că modelul este semnificativ din punct de vedere statistic


# IPOTEZA PRIVIND LINIARITATEA MODELULUI DE REGRESIE ESTE VERIFICATA  --> poate fi scris ca o functie liniara: Life_Expectancy = beta0 + beta1*Sanitary_Expense_Per_Capita + beta2*Water_Access_Percent + beta3*Unemployment_Rate+ u
model_multiple3 <- lm(Life_Expectancy ~ Sanitary_Expense_Per_Capita+Water_Access_Percent+Unemployment_Rate, data = global_health)
summary(model_multiple3)
#Termenul de interceptare (Estimate):dacă nu există cheltuieli sanitare, acces la apă și șomaj, speranța de viață ar fi de 54.45 ani.
# p-value=1.31e-14 <0.01 --> interceptul este semnificativ din punct de vedere statistic, pentru un nivel de semnificatie de 0.01

#Daca Sanitary_Expense_Per_Capita creste cu o unitate (1 dolar), speranța de viață (Life Expectancy) crește cu  0.001132 ani (Estimate), menținând ceilalți factori constanți.
#p-value este 2.27e-08< 0.01, coeficientul este semnificativ statistic-->relație semnificativă între cheltuielile sanitare și speranța de viață, pentru un nivel de semnificatie de 0.01

# Daca Water Access Percent creștere cu 1% din procentului de acces la apă, speranța de viață crește cu 0.1855 ani, menținând ceilalți factori constanți.
# p-value este 0.00057 < 0.01, coeficientul este semnificativ statistic, pentru un nivel de semnificatie de 0.01--> accesul la apă influențează semnificativ speranța de viață

#Daca Unemployment_Rate creste cu 1%, speranța de viață crește cu 0.3004 ani, menținând ceilalți factori constanți.
#Din studii putem observa ca rata șomajului poate fi corelată cu o îmbunătățire a sperantei de viata.
#p-value este 0.00485<0.01 --> rata șomajului are o relație semnificativă cu speranța de viață, pentru un nivel de semnificatie de 0.01

#Interpretare bonitate model:
#Adjusted R-squared (0.7758) indică faptul că aproape 77.58% din variabilitatea speranței de viață este explicată de model, Adjusted R-squared fiind corectat pentru numărul de variabile independente din model
#observam ca R-squared ajustat crește de la 0.7301 la 0.7758, iar acest lucru sugerează că variabila Unemployment_Rate aduce o îmbunătățire reală a modelului, nu doar o creștere artificială a R-squared
#se poate vedea o îmbunătățire semnificativă față de modelul anterior si putem spune că includerea ratei șomajului aduce o valoare semnificativă.

#Testarea validitatii modelului (cu testul F)
#p-value =4.864e-13<0.01, ceea ce înseamnă că resping ipoteza nulă și accept ipoteza alternativa care spune că modelul este semnificativ din punct de vedere statistic

#Intervalul de încredere pentru coeficientii Sanitary_Expense_Per_Capita, Water_Access_Percent si Unemployment_Rate la un nivel de încredere de 99%

# Afisarea coeficientilor
(coefficient_sanitary <- coef(model_multiple3)["Sanitary_Expense_Per_Capita"]) # 0.001131574 
(coefficient_water<- coef(model_multiple3)["Water_Access_Percent"]) # 0.1855494
(coefficient_unemployment<- coef(model_multiple3)["Unemployment_Rate"]) #0.3004315 

(df_r <- model_multiple3$df.residual)
t_critic<-qt(p = 0.99, df = df_r) #T CRITIC= 2.428568

#eroarea standard
(se_sanitary <- vcov(model_multiple3) %>% 
    diag %>% 
    sqrt %>% 
    .["Sanitary_Expense_Per_Capita"]) # = 0.0001610762 

(se_water <- vcov(model_multiple3) %>% 
    diag %>% 
    sqrt %>% 
    .["Water_Access_Percent"]) # =0.04933601 

(se_unemployment <- vcov(model_multiple3) %>% 
    diag %>% 
    sqrt %>% 
    .["Unemployment_Rate"]) # =0.1004081 


# Valoarea minima 99% nivel incredere
coefficient_sanitary - t_critic * se_sanitary # = 0.0007403895 
# Valoarea maxima 99% nivel incredere
coefficient_sanitary + t_critic * se_sanitary # =0.001522758 

coefficient_water - t_critic * se_water # = 0.06573354  
coefficient_water + t_critic * se_water # = 0.3053652  

coefficient_unemployment - t_critic * se_unemployment # = 0.05658358   
coefficient_unemployment + t_critic * se_unemployment # = 0.5442793  
#Sanitary_Expense_Per_Capita [0.0007403895; 0.001522758], Water_Access_Percent [0.06573354;  0.3053652 ] si Unemployment_Rate [0.05658358; 0.5442793 ] sunt semnificative din punct de vedere statistic pentru a explica Life_Expectancy, deoarece intervalele lor de încredere nu conțin valoarea 0

#================================================================================================================================================================================================================== 
#Deoarece suntem limitati din cauza numarului de observatii si ne dorim sa pastram 3 variabile independente incercam sa imbunatatim modelul inlocuid cea mai putin semnificativa variabila independenta 

#Obesity_Rate_Percent-> proporția populației care este considerată obeză, calculată ca un procent din totalul populației

#din chr se transformă în numeric
global_health$Obesity_Rate_Percent <- as.numeric(
  gsub(",", ".", global_health$Obesity_Rate_Percent)
)

#salvăm vechile valori
original_global_health <- global_health 

#imputeazam valorile lipsă (NA) utilizând mediana
global_health$Obesity_Rate_Percent[is.na(global_health$Obesity_Rate_Percent)] <- 
  median(global_health$Obesity_Rate_Percent, na.rm = TRUE)

#global_health <- original_global_health  # Restaurează datele inițiale

model_multiple4 <- lm(Life_Expectancy ~ Sanitary_Expense_Per_Capita+Water_Access_Percent+Obesity_Rate_Percent, data = global_health)
summary(model_multiple4)
#Coeficientul=0.61985 nu este semnificativ din punct de vedere statistic (p-valoarea este 0.61985 > 0.1), ceea ce sugerează că această variabilă nu are un efect semnificativ asupra Life_Expectancy în contextul modelului


#Immunization_Rate -> procentul populației care a fost vaccinată împotriva unei boli specifice
model_multiple5 <- lm(Life_Expectancy ~ Sanitary_Expense_Per_Capita+Water_Access_Percent+Immunization_Rate, data = global_health)
summary(model_multiple5)
#Coeficientul nu este semnificativ din punct de vedere statistic (p-valoarea este  0.2161  > 0.1), ceea ce sugerează că această variabilă nu are un efect semnificativ asupra Life_Expectancy în contextul modelului


global_health$Fertility_Rate <- as.numeric(
  gsub(",", ".", global_health$Fertility_Rate)
)
model_multiple6 <- lm(Life_Expectancy ~ Sanitary_Expense_Per_Capita+Water_Access_Percent+Fertility_Rate, data = global_health)
summary(model_multiple6)
#Fertility_Rate sugerează o legătură inversă semnificativă între rata fertilității și celelalte variabile independente, inclusiv speranța de viață la naștere. Acest coeficient negativ (-2.61) indică faptul că, pe măsură ce fertilitatea scade, speranța de viață poate crește.
#Termenul de interceptare este semnificativ(p-value < 0.01), indicând că, în absența tuturor variabilelor independente, speranța de viață ar fi 64.71 ani 

#Daca Sanitary_Expense_Per_Capita creste cu o unitate (1 dolar), speranța de viață crește cu  0.0011 ani, menținând ceilalți factori constanți.
#p-value este 1.54e-07< 0.01, coeficientul este semnificativ statistic-->relație semnificativă între cheltuielile sanitare și speranța de viață

#Daca Water Access Percent creștere cu 1% din procentului de acces la apă, speranța de viață crește cu 0.1461 ani, menținând ceilalți factori constanți.
# p-value este 0.00662 < 0.01, coeficientul este semnificativ statistic--> accesul la apă influențează semnificativ speranța de viață.

#Fertility_Rate are un efect negativ semnificativ: o creștere cu 1 unitate a ratei fertilității reduce speranța de viață cu 2.61 ani (p-value < 0.05), menținând ceilalți factori constanți.

#Interpretare bonitate model:
#Adjusted R-squared (0.7588) indică faptul că aproape 75.88% din variabilitatea speranței de viață este explicată de model
#observam ca R-squared ajustat intre model_multiple3 si model_multiple6 scade de la 0.7758158 la 0.7588, iar acest lucru sugerează că variabila Fertility_Rate nu aduce o îmbunătățire reală a modelului fata de primul

#===================================================================================================================================================================================================================================
# Bonitatea modelului (R-squared si R-squared ajustat) 
#Ce procent din variatie este explicat de model?

# Regresie simpla cu 1 regresor
summary(model_simple)
summary(model_simple)$r.squared #R-squared=0.6832401
summary(model_simple)$adj.r.squared #R-squared ajustat=0.6753211

# Regresie multipla cu 2 regresori
summary(model_multiple1)
summary(model_multiple1)$r.squared #R-squared= 0.7432671
summary(model_multiple1)$adj.r.squared #R-squared ajustat=0.7301013

# Regresie multipla cu 3 regresori
summary(model_multiple3)
summary(model_multiple3)$r.squared #R-squared=0.7922195
summary(model_multiple3)$adj.r.squared #R-squared ajustat=0.7758158

# Regresie multipla2 cu 3 regresori
summary(model_multiple6)
summary(model_multiple6)$r.squared #R-squared=0.7764943
summary(model_multiple6)$adj.r.squared #R-squared ajustat=0.7588491

# modelul 3 explică mai multă variabilitate decât modelul 6
#Observam ca adaugand mai multe variabile independente R-squared ajustat creste, si putem spune ca regresia multipla cu 3 regresori este mai buna decat celelalte si explica mai mult din variabila dependenta

#===================================================================================================================================================================================================================================
# Calitatea ajustarii (dorim sa comparam mai multe modele de regresie intre ele)

# Criteriul Akaike pentru modelul cu Unemployment_Rate
aic <- AIC(model_multiple3)
cat("AIC (Akaike):", aic, "/n") #194.2438

# Criteriul Schwarz pentru modelul cu Unemployment_Rate
bic <- BIC(model_multiple3)
cat("BIC (Schwarz):", bic, "/n") #202.9322

# Criteriul Hannan - Quinn pentru modelul cu Unemployment_Rate
hqic <- HQIC(model_multiple3)
cat("HQIC (Hannan-Quinn):", hqic, "/n") #197.4285

# Criteriul Akaike pentru modelul cu Fertility_Rate
aic <- AIC(model_multiple6)
cat("AIC (Akaike):", aic, "/n") #197.3079

# Criteriul Schwarz pentru modelul cu Fertility_Rate
bic <- BIC(model_multiple6)
cat("BIC (Schwarz):", bic, "/n") #205.9963

# Criteriul Hannan - Quinn pentru modelul cu Fertility_Rate
hqic <- HQIC(model_multiple6)
cat("HQIC (Hannan-Quinn):", hqic, "/n") #200.4926

#Modelul 3 indica o potrivire mai bună și mai puțin complexă comparativ cu modelul 6, deoarece se poate observa faptul ca valorile criteriilor informationale sunt mai mici pentru modelul 3
#AIC: Explică mai bine datele, permite modele complexe.
#BIC: Penalizează complexitatea mai sever, preferă modele simple.
#HQ: Abordare intermediară între AIC și BIC.
# cât de bine explică modelul datele observate, dar penalizează complexitatea excesivă.

#Ipotezele fundamentale de regresie pentru modelul optim==========================================================================================================================

# Valorile previzionate si reziduurile 
# cream 2 variabile in global_health: estimate_values - pentru valorile estimate ale variabilei dependente / residual -rezidurile modelului de regresie multifactorial
global_health %<>% mutate(estimate_values = fitted(model_multiple3),
                          residual = residuals(model_multiple3))

global_health %>% 
  select(Life_Expectancy, estimate_values, residual) %>% 
  head(10)

#Rezidurile modelului=valoarea reala a variabilei independente Life_Expentancy - valoarea estimata folosind regresia

#Statistici descriptive
global_health %>% 
  select(Life_Expectancy, estimate_values, residual) %>%
  stargazer(type = "text")
mean(model_multiple3$residuals)
#media valorilor reale pentru variabila dependenta Life_Expectancy este egala cu media valorilor estimate - estimate_values--> mean(Life_Expectancy) = mean(estimate_values)
#SE VERIFICA UNA DINTRE IPOTEZELE FUNDAMENTALE ALE UNUI MODEL DE REGRESIE: media reziduurilor este 0 --> mean(residual) = 0
# Life_Expectancy = estimate_values + residual

# => ipoteza este indeplinita
#=======================================================================================================================================================================================================
# Multicoliniaritate folosind VIF (ipoteza fundamentala pentru modelul de regresie legată de MULTICOLINIARITATE este verificată)
# Multicoliniaritatea este intalnita atunci cand variabilele independente sunt puternic corelate intre ele

vif(model_multiple3)
# VIF<10 --> nu avem multicoliniaritate --> nu este necesar să eliminăm nicio variabilă din model 
#Sanitary_Expense_Per_Capita        Water_Access_Percent           Unemployment_Rate 
#         1.695343                       1.695696                       1.149201 

# => ipoteza este indeplinita
#======================================================================================================================================================================================================= 
install.packages("tseries")
library(tseries)
install.packages("e1071")
library(e1071)
install.packages("olsrr")
library(olsrr)

# Verific ipoteza de normalitate a reziduurilor (REZIDUURILE TREBUIE SA URMEZE O DISTRIBUTIE NORMALA)
# Este variabila Life_Expectancy normal distribuită? 

#1. Graficul 'Residuals vs Fitted' --> OX - valorile estimate pentru Life_Expectancy / OY - reziduurile
plot(model_multiple3)
    # linia nu se află în dreptul valorii 0 (prezintă fluctuații) și nu este || cu OX --> reziduurile nu sunt normal distribuite

#2. Graficul 'Q-Q plot' --> OX - cuantile teoretice ale distribuției normale(valorile așteptate pentru datele care ar urma o distribuție normală) / OY - reziduurile modelului
plot(model_multiple3)
    # nu se formeaza o linie diagonala si exista cateva valori extreme de exemplu: 41, 27, 1 --> reziduurile nu sunt normal distribuite (distributie asimetrica)

#3. Histograma reziduurilor -> împărțirea întregului domeniu de valori în intervale si numărarea observațiilor care se încadrează în fiecare interval
ggplot(data = global_health) +
  theme_bw() +
  geom_histogram(mapping = aes(x = residual), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element_text(hjust = 0.5))
    #Distribuție aproximativ simetrică în jurul valorii 0 --> reziduurile sunt centrate pe 0 (valori extreme pentru valorile reziduurilor care depășesc ±3)

skewness(global_health$residual) # 0.3357509 >0 distributia asimetrică spre dreapta (pozitiva)
kurtosis(global_health$residual) #-0.2270098 <3 distributie platicurtica (aplatizarea distributiei)

#4. Grafic de tip Boxplot
boxplot(model_multiple3$residuals, main="Box Plot reziduuri")
    #se pot observa valori extreme situate deasupra valorii de 4 care depasesc limita superioară

#5. Testarea normalitatii cu ajutorul testelor specifice acestei ipoteze

ols_test_normality(model_multiple3)
    #Shapiro-Wilk, Kolmogorov-Smirnov, Anderson-Darling sugerează că nu există suficiente dovezi pentru a respinge ipoteza nula --> reziduurile sunt distribuite normal

#Deoarece testele sugereaza ca avem de-a face cu distributie normala --> reziduurile sunt distribuite normal, nu este nevoie sa aplicam corectii
#Aceasta este singura ipoteza care se accepta a fi incalcata deoarece ea afecteaza doar intervalele de incredere pentru coeficientii de regresie

#=> ipoteza este indeplinita

#==================================================================================================================================================================================================
#Ipoteza de homoscedasticitate

# Testam heteroscedasticitatea pentru modelul model_multiple3

#1. ridicam la patrat reziduurile si valorile estimate
global_health_h <- global_health %>%
  mutate(estimate_valuesq = estimate_values^2,
         residualsq = residual^2)

#2.
# Testul Breusch-Pagan

#reziduurile devin variabila dependenta in functie de variabilele independente din model
model_BP <- lm(residualsq ~ Sanitary_Expense_Per_Capita+Water_Access_Percent+Unemployment_Rate, global_health_h)
summary(model_BP)

#Coeficienti de regresie sunt semnificativ statistic in acelasi timp?
#Aplic testul F si multiplicatorul Lagrange pentru testarea semnificatiei coeficientilor (sunt coef semnificativ de 0 sau nu)

(k1 <- model_BP$rank - 1)# numarul de variabile independente din model
(r2 <- summary(model_BP)$r.squared) #R^2 = 0.06759482
(n <- nobs(model_BP))  # nr observatii = 42

# H0: erorile sunt homoscedastice (dispersia reziduurilor nu depinde de variabilele independente)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)

(F_stat <- (r2 / k1) / ((1 - r2) / (n - k1 - 1))) # 0.9182714 (F-statistic)
(F_pval <- pf(F_stat, k1, n - k1 - 1, lower.tail = FALSE)) # p-value = 0.4412567 > 0.05 => 
# nu putem respinge ipoteza nulă H0: rezidurile sunt homoscedastice (Variabilele independente nu explică semnificativ dispersia reziduurilor)

(LM_stat <- n * r2) #2.838982 (LM-statistic)
(LM_pval <- pchisq(LM_stat, df = k1, lower.tail = FALSE)) # p-value = 0.4171229 > 0.05 => nu putem respinge ipoteza nulă H0: rezidurile sunt homoscedastice

library(lmtest)
bptest(model_BP)
# deoarece p-value = 0.4701 > 0.05 => reziduurile sunt homoscedastice deci se confirma rezultatele testelor F si LM=>
# => ipoteza este indeplinita

#===================================================================================================================================================================================================
#IPOTEZA DE NONCORELARE A REZIDUURILOR

#Autocorelare = reziduurile sunt corelate cu ele însuși (valoarea curenta este dependenta de valorile anterioare)

# Testul Durbin-Watson
install.packages("lmtest")
library(lmtest) 
dwtest(model_multiple3) # p-value= 0.06174 > 0.05 => nu există suficiente dovezi pentru a respinge ipoteza nula --> reziduurile nu sunt autocorelate

bgtest(model_multiple3) # p-value = 0.2157 > 0.1 => nu există suficiente dovezi pentru a respinge ipoteza nula --> reziduurile nu sunt autocorelate

# => ipoteza este indeplinita
#=========================================================================================================================================================================================================
#IPOTEZA DE NECORELARE A REZIDUURILOR CU VARIABILELE INDEPENDENTE
cor.test(global_health$Sanitary_Expense_Per_Capita, model_multiple3$residual) # p-value = 1 > 0.1 => nu sunt corelate
cor.test(global_health$Water_Access_Percent, model_multiple3$residual) # p-value = 1 > 0.1 => nu sunt corelate
cor.test(global_health$Unemployment_Rate,model_multiple3$residual) # p-value = 1 > 0.1 => nu sunt corelate

# => ipoteza este indeplinita
#=========================================================================================================================================================================================================
#IPOTEZA PRIVIND NUMARUL DE OBSERVATII SI NUMARUL VARIABILELOR INDEPENDENTE 
nobs(model_multiple3) > (model_multiple3$rank - 1) # Nr de observatii > nr variabile independente -> TRUE

#======================================================================================================================================================
# IPOTEZA PRIVIND VARIABILITATEA V. INDEPENDENTE Variabilitatea in x este pozitiva (mai mare decat 0)
var(global_health$Sanitary_Expense_Per_Capita)
var(global_health$Water_Access_Percent)
var(global_health$Unemployment_Rate) 
# variabilitatea in x trebuie sa fie pozitiva -> toate valorile > 0

# => ipoteza este indeplinita
#==============================================================================================================================================================================
#   Îmbunătățesc modelul de regresie multipla prin adoptarea altei forme functionale

# Utilizam modelul de tip log-log
global_health <- global_health %>%
  mutate(
    Life_Expectancy_log = log(Life_Expectancy),
    Sanitary_Expense_Per_Capita_log = log(Sanitary_Expense_Per_Capita)
  )

#nu folosim logaritm pe aceste variabile Unemployment_Rate, Water_Access_Percent, deoarece acestea sunt deja exprimate in %
model_log_log <- lm(Life_Expectancy_log ~ Sanitary_Expense_Per_Capita_log + Unemployment_Rate +Water_Access_Percent, global_health)
summary(model_log_log)

#observam ca variabila independenta Water_Access_Percent nu mai este semnificativ statistic p_value = 0.95758 > 0.1 ==> eliminam variabila din model
model_log_log <- lm(Life_Expectancy_log ~ Sanitary_Expense_Per_Capita_log + Unemployment_Rate , global_health)
summary(model_log_log)

#Sanitary_Expense_Per_Capita_log --> semnificativ statistic pentru un nivel de incredere de 99% p-value =  < 2e-16 <0.01
#Unemployment_Rate --> semnificativ statistic pentru un nivel de incredere de 99% p-value = 0.00251 < 0.01
#O creștere cu 1% a cheltuielilor sanitare este asociată cu o creștere de 0.056% în speranța de viață când celelalte variabile rămân constante
#O creștere cu 1% a ratei somajului este asociată cu o creștere de 0.00318% în speranța de viață când celelalte variabile rămân constante

summary(model_log_log)$r.squared #R-squared=0.8731711
summary(model_log_log)$adj.r.squared #R-squared ajustat=0.8666671 --> 86,66% din variabilitatea speranței de viață este explicată de model

# Criteriul Akaike 
aic <- AIC(model_log_log)
cat("AIC (Akaike):", aic, "/n") #-193.0229

# Criteriul Schwarz 
bic <- BIC(model_log_log)
cat("BIC (Schwarz):", bic, "/n") #-186.0722

# Criteriul Hannan - Quinn 
hqic <- HQIC(model_log_log)
cat("HQIC (Hannan-Quinn):", hqic, "/n") #-186.0722

#============================================================================================================================================================================================================================================================================================
#   Regresia cu variabile dummy 

#(adaug o variabilă dummy la modelul inițial -> țările din estul Europei ==> 1, țările din vestul Europei ==> 0 
# Creez variabila dummy 
global_health$EU_East <- ifelse(global_health$Country %in% c('Albania', 'Armenia', 'Azerbaijan', 'Belarus', 'Bosnia and Herzegovina',
                                                             'Bulgaria', 'Croatia', 'Czech Republic', 'Estonia', 'Georgia', 
                                                             'Hungary', 'Kazakhstan', 'Kyrgyzstan', 'Latvia', 'Lithuania', 
                                                             'Moldova', 'Montenegro', 'Poland', 'Romania', 'Russia', 'Serbia', 
                                                             'Slovakia', 'Slovenia', 'Ukraine', 'North Macedonia', 'Kosovo'), 1, 0)

lm(Life_Expectancy_log ~ EU_East, global_health) %>% summary
#Speranța de viață este cu aproximativ cu 10,92% mai mică în țările din Estul Europei, decât cu țările din Vestul Europei
#p-value = 1.55e-14 < 0.1 --> EU_East este semnificativ statistic deci există dovezi care justifică diferențele dintre țările din Estul și Vestul Europei în ceea ce privește speranța de viață

#construim un nou model care include și variabila dummy EU_East
model_dummy <- lm(Life_Expectancy_log ~ Sanitary_Expense_Per_Capita_log + Unemployment_Rate + EU_East, data = global_health)
summary(model_dummy)

#Termenul de interceptare p-value= < 2e-16  <0.01 --> interceptul este semnificativ din punct de vedere statistic, pentru un nivel de semnificatie de 0.01
#Termenul de interceptare: dacă nu există cheltuieli sanitare (=0), rata șomajului este 0 și țara se găsește în vestul Europei, speranța de viață ar fi de 57.03 ani (exponentiala pentru transformare)

#Daca cheltuielile sanitare per capita cresc cu 1%, speranța de viață crește cu 3,99%, menținând ceilalți factori constanți
#p-value este  4.39e-08< 0.01, coeficientul este semnificativ statistic-->relație semnificativă între cheltuielile sanitare și speranța de viață, pentru un nivel de semnificatie de 0.01

#Speranța de viață în țările din Estul Europei este cu aproximativ 3.97% mai mică comparativ cu țările din Vestul Europei
#p-value este 0.00251 < 0.01, coeficientul este semnificativ statistic, pentru un nivel de semnificatie de 0.01

#Daca Unemployment_Rate creste cu 1%, speranța de viață crește cu 0,27%, menținând ceilalți factori constanți.
#p-value este  0.00374<0.01 --> rata șomajului are o relație semnificativă cu speranța de viață, pentru un nivel de semnificatie de 0.01

#Interpretare bonitate model:
#Adjusted R-squared (0.8927) indică faptul că variația variabilei dependente Life_Expentancy este explicată în proporție de 89.27% de variația variabilelor independente luate în considerare

#Testarea validitatii modelului (testul F)
#p-value = < 2.2e-16 < 0.01, ceea ce înseamnă că resping ipoteza nulă și accept ipoteza alternativa care spune că modelul este semnificativ din punct de vedere statistic

#   Testarea ipotezelor modelului

# 1.LINIARITATEA MODELELOR DE REGRESIE 
# Life_Expectancy_log= 4.0443020 + 0.0399780 * Sanitary_Expense_Per_Capita_log -0.0396866 * EU_East + 0.0027629 * Unemployment_Rate 
# ==>ipoteza verificată

# 2.IPOTEZA PRIVIND NUMARUL DE OBSERVATII SI NUMARUL VARIABILELOR INDEPENDENTE 
nobs(model_dummy) > (model_dummy$rank - 1) # TRUE
# ==>ipoteza verificată

# 3.IPOTEZA PRIVIND VARIABILITATEA V. INDEPENDENTE 
var(global_health$EU_East) #0.2560976
var(global_health$Sanitary_Expense_Per_Capita_log)#1.186527
var(global_health$Unemployment_Rate)#14.46141
# variabilitatea in x trebuie sa fie pozitiva -> toate valorile > 0
# ==>ipoteza verificată

# 3.IPOTEZA PRIVIND MEDIA REZIDUURILOR = 0
#cream 2 variabile noi in global_health: estimate_values_dummy  / residual_dummy
global_health %<>% mutate(estimate_values_dummy = fitted(model_dummy),
                          residual_dummy = residuals(model_dummy))

#Statistici descriptive
global_health %>% 
  select(Life_Expectancy_log, estimate_values_dummy, residual_dummy) %>%
  stargazer(type = "text")

#media reziduurilor este 0 --> mean(residual) = 0
# ==>ipoteza verificată

# 4.IPOTEZA PRIVIND MULTICOLINIARITATEA
vif(model_dummy) # nu avem valori pentru VIF > 10 
# ==>ipoteza verificată

# 5.IPOTEZA DE NECORELARE A REZIDUURILOR CU VARIABILELE INDEPENDENTE
cor.test(global_health$EU_East, global_health$residual_dummy)# p-value > 0.1 => nu sunt corelate
cor.test(global_health$Sanitary_Expense_Per_Capita_log, global_health$residual_dummy) # p-value > 0.1 => nu sunt corelate
cor.test(global_health$Unemployment_Rate, global_health$residual_dummy) # p-value > 0.1 => nu sunt corelate
# => ipoteza acceptata

# 6.Ipoteza de homoscedasticitate

#1. ridicam la patrat reziduurile si valorile estimate
global_health <- global_health %>%
  mutate(estimate_values_dummyq = estimate_values_dummy^2,
         residual_dummyq = residual_dummy^2)

#2.
# Testul Breusch-Pagan

#reziduurile devin variabila dependenta in functie de variabilele independente din model
model_BP_dummy <- lm(residual_dummyq ~ Sanitary_Expense_Per_Capita_log + Unemployment_Rate + EU_East, global_health)
summary(model_BP_dummy)

#Coeficienti de regresie sunt semnificativ statistic in acelasi timp?
#Aplic testul F si multiplicatorul Lagrange pentru testarea semnificatiei coeficientilor

(k1 <- model_BP_dummy$rank - 1)# numarul de variabile independente din model =3
(r2 <- summary(model_BP_dummy)$r.squared) #R^2 = 0.1542301
(n <- nobs(model_BP_dummy))  # nr observatii = 42

# H0: erorile sunt homoscedastice (dispersia reziduurilor nu depinde de variabilele independente)
# H1: erorile sunt heteroscedastice (reziduurile nu sunt distribuite cu varianta egala)

(F_stat <- (r2 / k1) / ((1 - r2) / (n - k1 - 1))) # 2.309826 (F-statistic)
(F_pval <- pf(F_stat, k1, n - k1 - 1, lower.tail = FALSE)) # p-value = 0.09177703 > 0.05 => 
# nu putem respinge ipoteza nulă H0: rezidurile sunt homoscedastice (Variabilele independente nu explică semnificativ dispersia reziduurilor)

(LM_stat <- n * r2) #6.477665 (LM-statistic)
(LM_pval <- pchisq(LM_stat, df = k1, lower.tail = FALSE)) # p-value = 0.09054753 > 0.05 => nu putem respinge ipoteza nulă H0: rezidurile sunt homoscedastice

library(lmtest)
bptest(model_BP_dummy)
# deoarece p-value = 0.3026 > 0.05 => reziduurile sunt homoscedastice deci se confirma rezultatele testelor F si LM=>
# => ipoteza este indeplinita

# 7.IPOTEZA DE NONCORELARE A REZIDUURILOR

# Testul Durbin-Watson
dwtest(model_dummy) # p-value= 0.2686 > 0.05 => nu există suficiente dovezi pentru a respinge ipoteza nula --> reziduurile nu sunt autocorelate

bgtest(model_dummy) # p-value = 0.9243 > 0.1 => nu există suficiente dovezi pentru a respinge ipoteza nula --> reziduurile nu sunt autocorelate

# => ipoteza este indeplinita

#IPOTEZA PENTRU REZIDUURILE NORMAL DISTRIBUITE(cu ajutorul testelor specifice)

ols_test_normality(model_dummy) 
#respingem ipoteza nulă și acceptăm ipoteza alternativă conform căreia reziduurile nu sunt normal distribuite
#reziduurile nu sunt normal distribuite, dar un număr limitat de observații pentru numărul de variabile independente nu recurgem la metoda de corectare prin eliminarea de observații

# DORIM SA VEDEM DACĂ NOUL MODEL DE REGRESIE ESTE MAI BUN DECÂT CEL INIȚIAL
summary(model_multiple3)
summary(model_multiple3)$r.squared #R-squared=0.7922195
summary(model_multiple3)$adj.r.squared #R-squared ajustat=0.7758158

summary(model_dummy)
summary(model_dummy)$r.squared #R-squared=0.9005876
summary(model_dummy)$adj.r.squared #R-squared ajustat=0.8927392

#R-squared ajustat pentru model_dummy > R-squared ajustat pentru model_multiple3
#Modelul model_dummy reușește să explice o parte mai mare din variația speranței de viață, ajustând în același timp rezultatul în funcție de variabilele independete

# Comparam cele 2 modele de regresie între ele folosind calitatea ajustarii

# Criteriul Akaike pentru modelul model_multiple3
aic <- AIC(model_multiple3)
cat("AIC (Akaike):", aic, "/n") #194.2438

# Criteriul Schwarz pentru modelul model_multiple3
bic <- BIC(model_multiple3)
cat("BIC (Schwarz):", bic, "/n") #202.9322

# Criteriul Hannan - Quinn pentru modelul model_multiple3
hqic <- HQIC(model_multiple3)
cat("HQIC (Hannan-Quinn):", hqic, "/n") #197.4285

# Criteriul Akaike pentru modelul model_dummy
aic <- AIC(model_dummy)
cat("AIC (Akaike):", aic, "/n") #-201.2525

# Criteriul Schwarz pentru modelul model_dummy
bic <- BIC(model_dummy)
cat("BIC (Schwarz):", bic, "/n") #-192.5642

# Criteriul Hannan - Quinn pentru modelul model_dummy
hqic <- HQIC(model_dummy)
cat("HQIC (Hannan-Quinn):", hqic, "/n") #-198.0679

#Modelul model_dummy indica o potrivire mai bună și mai puțin complexă comparativ cu model_multiple3 , deoarece se poate observa faptul ca valorile criteriilor informationale sunt mai mici pentru model_dummy
#=======================================================================================================================================================================================================================================
global_health %<>% mutate(sanitaryXunemployment = Sanitary_Expense_Per_Capita_log*Unemployment_Rate,
                  sanitaryXeast = Sanitary_Expense_Per_Capita_log*EU_East,
                  unemploymentXeast = Unemployment_Rate*EU_East)

#dupa ce am testat toți termenii de interacțiune, am eliminat variabilele independete care au devenit nesemnificative sau care prezentau multicoliniaritate am ajuns la acest model de regresie

model_interactiune <- lm(Life_Expectancy_log ~ Sanitary_Expense_Per_Capita_log + unemploymentXeast +  Unemployment_Rate, data = global_health)
summary(model_interactiune)
#Această interacțiune explorează dacă efectul cheltuielilor sanitare variază între țările est-europene și celelalte.

#În țările din estul Europei impactul ratei șomajului asupra speranței de viață este mai mic cu 0.003655 comparativ cu Vestul Europei

#Bonitatea modelului
summary(model_interactiune)
summary(model_interactiune)$r.squared #R-squared=0.8987707
summary(model_interactiune)$adj.r.squared #R-squared ajustat=0.890779

# Criteriul Akaike pentru modelul model_interactiune
aic <- AIC(model_interactiune)
cat("AIC (Akaike):", aic, "/n") #-200.4919

# Criteriul Schwarz pentru modelul model_interactiune
bic <- BIC(model_interactiune)
cat("BIC (Schwarz):", bic, "/n") # -191.8035

# Criteriul Hannan - Quinn pentru modelul model_interactiune
hqic <- HQIC(model_interactiune)
cat("HQIC (Hannan-Quinn):", hqic, "/n") # -197.3072

# Criteriul Akaike pentru modelul model_dummy
aic <- AIC(model_dummy)
cat("AIC (Akaike):", aic, "/n") #-201.2525

# Criteriul Schwarz pentru modelul model_dummy
bic <- BIC(model_dummy)
cat("BIC (Schwarz):", bic, "/n") #-192.5642

# Criteriul Hannan - Quinn pentru modelul model_dummy
hqic <- HQIC(model_dummy)
cat("HQIC (Hannan-Quinn):", hqic, "/n") #-198.0679

#Modelul model_dummy oferă o ajustare mai bună a datelor, având un R-squared ajustat mai mare, iar în același timp, este mai eficient și mai simplu conform criteriilor AIC, BIC și HQIC

# ==> se pot face prognoze pe baza modelului

#=========================================================================================================================================================================================================================================
# SCENARIU DE PROGNOZA
# Impartirea setului de date in setul de antrenare (train.data-->80%- regresia) si setul de testare (test.data-->20% - prognoza si acuratetea)
install.packages("mltools")
install.packages("MLmetrics")
install.packages("caret")
library(mltools)
library(MLmetrics)
library(caret)

set.seed(123) 
training.samples <- global_health$Life_Expectancy %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- global_health[training.samples, ]
test.data <- global_health[-training.samples, ] 

summary(model_dummy)

model_train <- lm(Life_Expectancy_log ~ Sanitary_Expense_Per_Capita_log + Unemployment_Rate + EU_East, data = train.data) 
summary(model_train)

# Facem predictia modelului pe setul de testare (PREDICTII IN SAMPLE)
y_pred <- predict(model_train, newdata = test.data)
y_pred
exp(y_pred)

#Evaluam acuratețea modelului de regresie folosind următorii indicatori de acuratețe
# RMSE - Root Mean Squared Error (compar valoarea reala a variable dependente din setul de testare cu valoarea previzionata folosind modelul de regresie)
RMSE(y_pred, test.data$Life_Expectancy_log) #RMSE = 0.0232141 < 1 , apropiat de 0 => predicție bună

# MAE - Mean Absolute Error => (toate diferentele individuale sunt ponderate in mod egal in medie)
MAE(y_pred, test.data$Life_Expectancy_log) #MAE = 0.0203376 < 1 , apropiat de 0 => predicție bună

# MSE - Mean Squared Error => (masoara in unitati care este patratul variabilei tinta si penalizeaza mai mult erorile mai mari) 
mse(y_pred, test.data$Life_Expectancy_log) #MSE = 0.0005388943 < 1 , apropiat de 0 => predicție bună

# MAPE - Mean Absolute Percentage Error => (masura ce indica media dispersiei dintre valorile previzionate si cele reale)
MAPE(y_pred, test.data$Life_Expectancy_log) #MAPE = 0.004689214 < 1 , apropiat de 0 => predicție bună

#PREDICTII OUT OF SAMPLE (aleg 3 scenarii out of sample)
out_of_sample <- data.frame(Sanitary_Expense_Per_Capita = c(1200, 4000, 1500),  
                            Unemployment_Rate = c(8.5, 4.0, 7.0),          
                            EU_East = c(1, 0, 1))

#primul șcenariu: Țară cu cheltuieli sanitare moderate, șomaj mediu, din estul Europei
#scenariul al 2-lea: Țară cu cheltuieli sanitare ridicate, șomaj scăzut, din vestul Europei
#scenariul al 3-lea: Țară cu cheltuieli sanitare moderate, șomaj mediu, din estul Europei

#deoarece regresia inițial este de tip log-log o să logaritmăm
out_of_sample_log <- out_of_sample %>%
  mutate(Sanitary_Expense_Per_Capita_log = log(Sanitary_Expense_Per_Capita))
out_of_sample_log

#nu mai avem nevoie de variabila originală și o logaritmăm
out_of_sample_log <- out_of_sample_log %>%
  select(-Sanitary_Expense_Per_Capita)

#obținem predicțiile
y_pred_outsample <- predict(model_dummy, newdata = out_of_sample_log)
y_pred_outsample
# Inversăm transformarea logaritmică pentru a obține predicțiile pe scala originală
exp(y_pred_outsample) #speranțele de viață considerând aceste 3 scenarii de prognoză

#Pentru cheltuieli sanitare pe cap de locuitor de 1200 de dolari, o rată a somajului de 8.5%, țara aflându-se în Europa de Est-->speranța de viață este de 74.55 ani
#Pentru cheltuieli sanitare pe cap de locuitor de 4000 de dolari, o rată a somajului de 4.0%, țara aflându-se în Europa de Vest-->speranța de viață este de 80.39 ani
#Pentru cheltuieli sanitare pe cap de locuitor de 1500 de dolari, o rată a somajului de 7.0%, țara aflându-se în Europa de Est-->speranța de viață este de 74.91 ani
#==================================================================================================================================================================================================================================================
#Metode de regularizare (verificăm dacă obținem un model mai robust/generalizat, în care coeficienții nu sunt exagerați)
install.packages("glmnet")
library(glmnet)

#Regresia RIDGE
#modelul initial
summary(model_dummy)

#Prognoza pentru modelul inițial
prognoza <- data.frame(Sanitary_Expense_Per_Capita = c(1500),
                       Unemployment_Rate = c(5.5),
                       EU_East=c(1))
prognoza_new <- prognoza %>%
  mutate(Sanitary_Expense_Per_Capita_log = log(Sanitary_Expense_Per_Capita))
prognoza_new <- prognoza_new %>%
  select(-Sanitary_Expense_Per_Capita)
prognoza_new

y_pred_scenariu <- predict(model_dummy, newdata = prognoza_new)
y_pred_scenariu
exp(y_pred_scenariu)
#Pentru cheltuieli sanitare pe cap de locuitor de 1500 de dolari, o rată a somajului de 5.5%, țara aflându-se în Europa de Est-->speranța de viață este de 74.60288  ani

#Pentru a aplica tehnicile de regularizare o să ne salvăm variabila dependenta într-o variabilă separată y
y <- global_health$Life_Expectancy_log

#Variabilele independete trebuie scrise sub formă matriceală 
x <- data.matrix(global_health[, c('Sanitary_Expense_Per_Capita_log', 'Unemployment_Rate', 'EU_East')])

# Estimam modelul ridge (alpha = 0 pentru ridge)
model <- glmnet(x, y, alpha = 0)
summary(model)

# Pentru un model clasic de regresie (metoda celor mai mici patrate) încercăm să minimizăm suma patratelor reziduurilor ( SSR = sum((y-yfit)^2) )
# Pentru ridge incercam același lucru. dar în plus mai apare lambda*sum(beta^2)
# Trebuie sa obțin valoarea optimă a lui lambda pentru care suma ( SSR + lambda*sum(beta^2) ) este minimă

#Cross validation pentru a găsi lambda optim
#Cum funcționează: eșantionul e împărțit în subeșantioane, se estimează modelul pe fiecare subesantion și se obține acel eșantion pentru care lambda e optim 
cv_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model$lambda.min
best_lambda # 0.005685543 
plot(cv_model)

# Reimplementăm modelul cu valoarea lamda optimă
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model) # coeficienții penalizați ai variabilelor

# Diagrama Trace - cum coeficienții variabilelor se modifică pe măsură ce crește lambda ?
plot(model, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

#Sanitary_Expense_Per_Capita_log: La valori mici de lambda coeficientul este cel mai mare dintre variabile, indicând o influență puternică asupra variabilei dependente
#Unemployment_Rate: Coeficientul este aproape constant și foarte aproape de 0 pe întreg intervalul lambda ceea ce sugerează că această variabilă are o influență slabă 
#EU_East: Pe măsură ce lambda crește, coeficientul crește treptat, dar se apropie de 0 la valori mari de lambda, ceea ce indică faptul că influența variabilei este diminuată de regularizare

#valorile preconizate ale lui y (Life_Expentancy_log) pentru observațiile din setul de date inițial
y_predicted <- predict(model, s = best_lambda, newx = x)
y_predicted
# Progoza out-of-sample folosind noul model Ridge cu aceleași valori pentru a vedea cum se comportă modelul într-un scenariu similar
new <- matrix(c(1500,5.5,1), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new) #54.71047 ani

# calcularea lui R2 pentru a vedea acuratețea
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # R^2 = 89,61%
 
#Model inițial de regresie este mai bun decât modelul Ridge regularizat, deoarece R^2 este mai mare

#Regresia LASSO
#LASSO are avantajul că poate elimina complet variabilele irelevante (=0) care sunt nesemnificative, făcând modelul mai simplu
#Lasso încearcă să minimizeze SSR + lambda*sum(|beta|)

y <- global_health$Life_Expectancy_log
x <- data.matrix(global_health[, c('Sanitary_Expense_Per_Capita_log', 'Unemployment_Rate', 'EU_East')])
# Estimam modelul LASSO
model <- glmnet(x, y, alpha = 1) #alpha = 1 pentru LASSO

#Cross validation pentru a găsi lambda optim
cv_model <- cv.glmnet(x, y, alpha = 1)

# Valoarea optimă a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 0.0002349274
plot(cv_model) 

# Reimplementăm modelul cu valoarea lamda optimă
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 

# Diagrama Trace
plot(model, xvar = "lambda",label=T)
legend("bottomri ght", lwd = 1, col = 1:6, legend = colnames(x), cex = .7)

#Sanitary_Expense_Per_Capita_log: Pe măsură ce lambda crește, coeficientul scade ușor, dar nu ajunge la 0 și asta înseamnă că variabila rămâne semnificativă indiferent de nivelul de regularizare.
#Unemployment_Rate: Variabila are o influență slabă asupra variabilei dependente deoarece este aproape de 0 
#EU_East: Pe măsură ce lambda crește, coeficientul se apropie rapid de 0 --> influența acestei variabile este penalizată de Lasso

#valorile preconizate ale lui Life_Expentancy_log pentru observațiile din setul de date inițial
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Progoza out-of-sample folosind noul model Lasso cu aceleași valori pentru a vedea cum se comportă modelul într-un scenariu similar
new <- matrix(c(1500,5.5,1), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new) #63.61635 ani

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq #R^2 = 90.05477%

#Regresia ELASTIC NET
# aici se adaugă ambele penalități SSR + lambda*sum(beta^2) + lambda*sum(|beta|)

y <- global_health$Life_Expectancy_log
x <- data.matrix(global_health[, c('Sanitary_Expense_Per_Capita_log', 'Unemployment_Rate', 'EU_East')])
# Estimam modelul ELASTIC NET
model <- cv.glmnet(x, y, alpha = 0.5) #alpha = 0.5 pentru ELASTIC NET

#Cross validation pentru a găsi lambda optim
cv_model <- cv.glmnet(x, y, alpha = 0.5)

# Valoarea optimă a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 0.0008210845
plot(cv_model) 

# Reimplementăm modelul cu valoarea lamda optimă
best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model) 

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Progoza out-of-sample folosind noul model Ridge cu aceleași valori pentru a vedea cum se comportă modelul într-un scenariu similar
new <- matrix(c(1500,5.5,1), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new) #62.34485 ani

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq #R^2 = 90.03304%

#Model inițial de regresie este mai bun decât modelul Elastic Net regularizat, deoarece R^2 este cu puțin mai mare

#Comparând cele 3 modele regresia inițială, Ridge, Lasso și Elastic Net se observă :
#Modelul inițial are cel mai mare R^2= 90,06%, indicând o performanță mai bună, dar include toate variabilele indiferent de relevanța lor
#Ridge regularizează coeficienții, reducând impactul variabilelor mai puțin importante (nu le elimină complet), dar ca performanță avem R^2 = 89,61% < decât în modelul inițial
#Lasso este asemănător cu Ridge, dar el aduce la 0 coeficienții irelevanți și avem R^2 = 90,05% < decât în modelul inițial
#Elastic Net combină avantajele Ridge și Lasso (penalizează coeficienții mari + elimină variabilele mai puțin relevante) cu un R^2 = 90,03%.

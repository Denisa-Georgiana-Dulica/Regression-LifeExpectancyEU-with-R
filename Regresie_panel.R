#Aplicatia 2 - Modele de regresie folosind date de tip panel

#Import pachetele si definesc setul de date de tip panel pentru 42 de tari din Europa pe 10 ani 2012-2021
PackageNames <- c("tidyverse","gplots","plm","readxl","foreign","lmtest","car")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

panel_data <- read_excel("C:/Users/User/R_project/Proiect_econometrie/global_health_Europe.xlsx")
pd.df <- pdata.frame(panel_data, index = c("Country","Year"), drop.index = TRUE)

#Facem o analiza exploratorie a datelor unde vom observa evolutia sperantei de viata pentru fiecare tara 
coplot(Life_Expectancy ~ Year|Country, type="l", data=panel_data) 
# Putem observa ca exista tari care prezinta o crestere mai pronuntata a sperantei de viata intre 2012-2021, in timp ce 
#alte tari au o crestere constanta sau chiar nu prezinta cresteri. Sunt tari care arata un declin temporar,iar acest lucru
#ar putea indica de exemplu prezenta unei crize economice in acea perioada. Se observa si faptul ca anumite tari au o speranta 
#de viata mult mai ridicata fata de alte tari, indicand diferenta dintre tarile din Europa de Vest (tari dezvoltate) si Est (tari in curs de dezvoltare).
#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Heterogeneitatea transversala 

#Analizam cum difera speranta la viata in tarile studiate (graficul arata intervalul de incredere pentru media sperantei de viata a fiecarei tari)
plotmeans(Life_Expectancy ~ Country, main = 'Heterogeneitate transversala', data = panel_data)
# Tarile precum Italia, Islanda, Spania au valoarea medie a sperantei de viata peste 80 de ani (ridicata) 
#si tot pentru aceste tari observam variatia scazuta (valori stabile) in timp din cauza intervalelor de incredere inguste.
# Tarile precum Ucraina, Serbia au valoarea medie a sperantei de viata intre 70-75 ani, iar variatia sperantei de viata este mai mare pentru acestea.
# Avem diferente semnificative pentru tarile incluse in esantion ==> avem heterogeneitatea transversala 


#Heterogeneitatea temporala

#Analizam cum difera speranta la viata in anii studiati
plotmeans(Life_Expectancy ~ Year, main = 'Heterogeneitate temporala', data = panel_data)
# Observam ca in perioada 2012-2019, speranta de viata a crescut constant, iar odata cu 2019 
#se observa o scadere semnificativa a mediei sperantei de viata din cauza pandemiei de COVID-19 
#care a dus la cresterea mortalitatii in tarile din Europa.
# Pana in anul 2019 variabilitatea intre tari a fost stabila, iar in perioada pandemiei 2020-2021 
#aceasta a crescut ceea ce indica faptul ca impactul COVID-19 nu a fost la fel in toate tarile.
#Valorile sperantei de viata variaza de la an la an ==> avem heterogeneitatea temporala

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Estimez un model de regresie OLS care nu ia in calcul heterogeneitatea transversala si heterogeneitatea temporala

#adaug o variabila dummy (Tari din Europa de Est =1 /Tari din Europa de Vest =0) pentru a imbunatatii modelul 
panel_data$EU_East <- ifelse(panel_data$Country %in% c('Albania', 'Armenia', 'Azerbaijan', 'Belarus', 'Bosnia and Herzegovina',
                                                             'Bulgaria', 'Croatia', 'Czech Republic', 'Estonia', 'Georgia', 
                                                             'Hungary', 'Kazakhstan', 'Kyrgyzstan', 'Latvia', 'Lithuania', 
                                                             'Moldova', 'Montenegro', 'Poland', 'Romania', 'Russia', 'Serbia', 
                                                             'Slovakia', 'Slovenia', 'Ukraine', 'North Macedonia', 'Kosovo'), 1, 0)
ols <- lm(Life_Expectancy ~ Sanitary_Expense_Per_Capita+Unemployment_Rate+EU_East+Water_Access_Percent, data = panel_data)
summary(ols)
#Termenul de interceptare (Estimate):dacă toate variabilele explicative sunt egale cu 0, speranța de viață ar fi de 68,39 ani.
# p-value= <2e-16 < 0.05 --> interceptul este semnificativ din punct de vedere statistic, pentru un nivel de semnificatie de 0.01

#Daca cheltuielile sanitare pe cap de locuitor cresc cu o unitate (1 dolar), speranța de viață (Life Expectancy) crește cu  0.0004954 ani (Estimate), menținând ceilalți factori constanți.
#p-value este 6.37e-15< 0.05, coeficientul este semnificativ statistic-->relație semnificativă între cheltuielile sanitare și speranța de viață, pentru un nivel de semnificatie de 0.01

#Daca accesul la apa potabila creste cu 1%, speranța de viață crește cu 0.1002 ani, menținând ceilalți factori constanți.
#p-value este 1.00e-13 <0.05 --> accesul la apa are o relație semnificativă cu speranța de viață, pentru un nivel de semnificatie de 0.01

#Daca rata somajului creste cu 1%, speranța de viață crește cu 0.1430 ani, menținând ceilalți factori constanți.
#p-value este < 2e-16 <0.05 --> rata șomajului are o relație semnificativă cu speranța de viață, pentru un nivel de semnificatie de 0.01

#Tarile din Europa de Est au in medie o speranta de viata mai mica cu 3,975 ani fata de cele din Europa de Vest.
#p-value este < 2e-16<0.05 --> coeficientul are o relație semnificativă cu speranța de viață, pentru un nivel de semnificatie de 0.01

#Interpretare bonitate model:
#Adjusted R-squared (0.8091) indică faptul că aproape 80,91% din variatia variabilei dependente speranța de viață este explicată de variatia variabilelor independente

#Testarea validitatii modelului (cu testul F)
#p-value =< 2.2e-16<0.05, ceea ce înseamnă că resping ipoteza nulă și accept ipoteza alternativa care spune că modelul este valid din punct de vedere statistic

yhat <- ols$fitted # valorile estimate ale variabilei dependente

#Reprezentam grafic legatura dintre valorile estimate ale variabilei dependente si variabila independenta Sanitary_Expense_Per_Capita
ggplot(panel_data, aes(x=Sanitary_Expense_Per_Capita, y=Life_Expectancy))+ 
  geom_point()+
  geom_smooth(method='lm', se=FALSE)+
  theme_bw()
#Dreapta de regresie este orientata spre dreapta ==> legatura pozitiva intre Life_Expectancy si Sanitary_Expense_Per_Capita ==> pe masura ce cheltuielile sanitare cresc, speranta la viata creste 
#Incercam sa gasim un model mai bun

#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Model cu efecte fixe (FE - fixed effects) 
#Acest model are scopul de a controla efectele, care nu sunt variate in timp, la nivelul observatiilor. Acesta este corelat cu variabilele explicative.

fe <- plm(Life_Expectancy ~ Sanitary_Expense_Per_Capita+Unemployment_Rate+EU_East+Water_Access_Percent, panel_data, index = c('Country','Year'),
          model = 'within')#specific FE
summary(fe)
#variabila EU_East a fost eliminata din model, deoarece este o variabila constanta in timp pentru fiecare tara
#Eliminam variabilele independente care au devenit nesemnificative (>0.05) - Sanitary_Expense_Per_Capita/Unemployment_Rate si estimam din nou modelul
fe <- plm(Life_Expectancy ~ Water_Access_Percent, panel_data, index = c('Country','Year'),
          model = 'within')#specific FE
summary(fe)
#Daca accesul la apa potabila creste cu 1%, speranța de viață crește cu 0.12 ani
#p-value este 0.0001016  <0.05 --> accesul la apa are o relație semnificativă cu speranța de viață, pentru un nivel de semnificatie de 0.01
#Adj. R-Squared: -0.066655 ==> Modelul nu adauga suficienta valoare explicativa peste efectele fixe
#Testul F: p-value: 0.00010164<0.05, ceea ce înseamnă că resping ipoteza nulă și accept ipoteza alternativa care spune că modelul este valid din punct de vedere statistic

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Vrem sa alegem cea mai adecvata varianta de model ==> comparam regresiile OLS si FE
#Ipoteza nula: OSL Ipoteza alternativa: FE
pFtest(fe, ols) #p-value < 2.2e-16 < 0.05 ==> Resping ipoteza nula si accept ipoteza alternativa conform careia se recomanda un model cu efecte fixe FE
#=> efectele fixe in spatiu (caracteristici specifice fiecarei tari, constante in timp) sunt semnificative
  
#incercam sa gasim un model mai bun

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Model cu efecte aleatorii RE (random effects) 
#Acest model presupune ca diferentele intre tari sunt aleatorii si ca aceste efecte pot fi luate in considerare ca parte a erorii modelului
#Acest model nu e corelat cu variabilele independente
#se foloseste variatia in timp (cum se schimba variabilele in aceeasi tara) si variatia intre unitati (diferentele intre tari)

re <- plm(Life_Expectancy ~ Water_Access_Percent, panel_data, index = c('Country','Year'),
          model = 'between')
summary(re)
#Daca accesul la apa potabila creste cu 1%, speranța de viață crește cu 0.304 ani
#p-value este 7.904e-08  <0.05 --> accesul la apa are o relație semnificativă cu speranța de viață, pentru un nivel de semnificatie de 0.01
#Adj. R-Squared: 0.51463 ==>indică faptul că aproape 51,46% din variatia variabilei dependente speranța de viață este explicată de variatia variabilei independente
#Testul F: p-value: 7.9039e-08<0.05, ceea ce înseamnă că resping ipoteza nulă și accept ipoteza alternativa care spune că modelul este valid din punct de vedere statistic

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Testul Hausmann
#Pentru a vedea care este cel mai potrivit model dintre FE si RE
#Ipoteza nula: RE Ipoteza alternativa: FE
phtest(fe,re) #p-value = 0.0008304 < 0.05 ==> Resping ipoteza nula si accept ipoteza alternativa conform careia se recomanda un model cu efecte fixe FE

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Vrem sa vedem daca trebuie sa folosim efectele fixe in timp ->  variază între perioade de timp, dar rămân constanți pentru toate unitățile de observație

# ipoteza nula:  nu avem nevoie de efectele fixe in timp  / ipoteza alternativa: avem nevoie de efectele fixe in timp
fixed.time <- plm(Life_Expectancy ~ Water_Access_Percent + factor(Year), data=panel_data, index=c("Country","Year"), model="within")
summary(fixed.time)
pFtest(fixed.time, fe) # p-value= < 2.2e-16 < 0.05 => modelul se poate imbunatatii prin utilizarea efectelor fixe in timp
plmtest(fe, c('time'), type = 'bp') # p-value = 0.7874 >0.05  => rezultat opus fata de testul anterior, dar alegem sa nu folosim efectele fixe in timp pentru ca rezultatele sunt inconclusive

#Vrem sa vedem daca trebuie sa folosim efectele aleatorii(diferențele dintre unități sunt tratate ca fiind aleatorii, nu fixe)

#Testarea efectelor aleatorii cu Breusch-Pagan Lagrange Multiplier (decidem intre RE si OLS)
# ipoteza nula: variatiile in timp sunt 0 - OSL  /  ipoteza alternativa: variatiile in timp sunt diferite de 0 (efectele aleatorii sunt adecvate) - RE
pool <- plm(Life_Expectancy ~ Water_Access_Percent, data=panel_data, index=c("Country", "Year"), model="pooling")
summary(pool)
plmtest(pool, type=c("bp")) # p-value < 2.2e-16 <0.05 ==> Resping ipoteza nula si accept ipoteza alternativa conform careia se recomanda un model cu efecte aleatorii RE
#==> efectele aleatorii sunt adecvate, pentru ca exista diferente semnificative intre tari

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Ipoteza fundamentala: Testarea dependentei transversale (Sunt reziduurile dintr-o tara corelate cu reziduurile din alta tara?)

#Pentru ambele teste: H0: reziduurile intre tari nu sunt corelate / H1: reziduurile intre entitati sunt corelate
#Testul Breusch-Pagan LM  
pcdtest(fe, test = 'lm') # p-value < 2.2e-16 <0.05 => Se respinge ipoteza nula si se accepta ipoteza alternativa conform careia avem dependenta transversala
#Testul Parasan CD
pcdtest(fe, test = 'cd') #p-value < 2.2e-16 <0.5 => Se respinge ipoteza nula si se accepta ipoteza alternativa conform careia avem dependenta transversala

#Nu corectam deoarece avem sub 40 de perioade de timp (doar 10 ani) -> avem micro panel
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Ipoteza fundamentala: Testarea autocorelarii - Breusch-Godfrey/Wooldridge test 

# H0: reziduurile nu sunt autocorelate /  H1: reziduurile sunt autocorelate
pbgtest(fe) #p-value < 2.2e-16 < 0.05 => Se respinge ipoteza nula si se accepta ipoteza alternativa conform careia avem autocorelare

#Deoarece avem putine perioade de timp si autocorelatia apare greu, vom ignora ipoteza
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Ipoteza fundamentala: Testarea heteroschedasticitatii cu testul Breusch-Pagan

# H0: avem homoschedasticitate / H1: avem heteroschedasticitate
bptest(Life_Expectancy ~ Water_Access_Percent + factor(Country), data = panel_data, studentize=F)
# p-value < 2.2e-16 <0.05 => Se respinge ipoteza nula si se accepta ipoteza alternativa conform careia avem heteroschedasticitate

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Testarea efectelor random 

# ipoteza nula:  nu avem nevoie de efectele random  / ipoteza alternativa: avem nevoie de efectele random
pFtest(re, ols) # p-value=1 > 0.05 => Se respinge ipoteza alternativa si se accepta ipoteza nula conform careia nu se recomanda efecte random
plmtest(re, c('time'), type = 'bp') # p-value = 0.7874 > 0.05 => Se respinge ipoteza alternativa si se accepta ipoteza nula conform careia nu se recomanda efecte random

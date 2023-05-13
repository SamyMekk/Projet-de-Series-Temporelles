
#Import des données

serie=read.csv("C:\\Users\\samym\\Desktop\\Séries Temporelles Linéaires\\Série2.csv")
View(serie)

# On remet la série dans le bon ordre


serie1=serie$Valeurs[122:1] #Nous ne conservons que les données de Janvier 1990 à Février 2002 ce qui correspond
#aux 122 premières données

plot(serie1,type="l") # on la plot pour voir à quoi elle ressemble 

# On voit une tendance linéaire dans notre série. On va tester une différenciation à l'ordre 1



#Mise de la série au format Zoo
serie2=zoo(serie1)
plot(serie2)


#Différenciation à l'ordre 1 de notre série pour la rendre stationnaire

Trend=serie2-lag(serie2,-1)
plot(Trend)

# Notre série Trend semble être stationnaire après cette différenciation à l'ordre 1


# On rend notre série de moyenne nulle par soucis de simplification
Trend=Trend-mean(Trend)

# Tests de Stationnarité
adf.test(Trend) # Test de Dickey-Fuller Augmenté
pp.test(Trend) # Test de Phillips-Perron
kpss.test(Trend) # Test KPSS

# Autocorrélogramme et Autocorrélogramme partiel pour voir les ordres maximaux de p et q


acf(Trend) # ( On conserve les valeurs pour 0<=q<=1)
pacf(Trend) # (On garde les valeurs pour 0<=p<=1 )





#On va tester les modèles pour p<=1 et q<=1

model1=arma(Trend,order=c(0,0))
summary(model1)

model2=arma(Trend,order=c(0,1))
summary(model2)

model3=arma(Trend,order=c(1,0))
summary(model3)

model4=arma(Trend,order=c(1,1))
summary(model4)


# On voit que le modèle 2 est celui qui minimise le critère AIC


#On vérifie que le ARMA(0,1) est  le meilleur modèle avec la fonction auto.arima de R

auto.arima(Trend,seasonal=FALSE)

# Elle redonne bien le modèle ARMA(0,1) comme meilleur modèle ARMA de notre série

# On va prédire à horizon 2 ( h=2) les valeurs de notre série avec un intervalle de confiance à 95%

mod=auto.arima(Trend,seasonal=FALSE)

pred_forecast=forecast(mod,h = 2,level=95)
plot(pred_forecast) # On plot  à horizon h=2 les 2 valeurs prédites par le modèle ainsi que les intervalles 
# de confiance à 95%






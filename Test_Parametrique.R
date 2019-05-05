fct_t <- function(X,moy){
  res = abs( (moy- mean(X)))/(sd(X)/sqrt(length(X)) ) 
  res
}

fct_t2 <- function(X1,X2){
  moy = abs(mean(X1) - mean(X2))
  sig = ( (sd(X1)^2)/length(X1) ) + ( (sd(X2)^2)/length(X2) )
  res = moy / sqrt(sig)
}

{
  Database <- read.table("/home/fabrice/Documents/Polytech/AnalyseDonnees/data2TP1.txt", header = TRUE)
  t_marseille=fct_t(Database$Marseille,19)
  t_mars_aix=fct_t2(Database$Marseille,Database$Aix)
}


 # 5) On calcul un score t = 2.177369
 # On regarde sur la t table le seuil a ne pas depasser pour alpha=5%
 # et un degres de liberte = 14 , seuil = 2.145
 # On constate que t > seuil on a donc depasse le seuil
 # Cela veut dire qu'on rejette donc l'hypothese nulle H0 qui est que
 # l'inflation 2010-2019 n'a pas affecte le coût de la vie à Marseille
 # on rejette cette hypothese avec 5% de chance de se tromper en rejettant H0


 # 6) On calcul un score t = 2.321494
 # On regarde sur la t table le seuil a ne pas depasser pour alpha=5%
 # et un degres de liberte = 15+15-2=28 , seuil = 2.048
 # t > seuil on rejette alors l'hypothese H0 on peut alors dire 
 # qu'il existe une dépendance significative entre Marseille et Aix-en-Provence
 # avec 5% de chance se tromper
 #
 # Si on prend alpha=2% on a un seuil=2.468
 # Cette fois-ci on a t < seuil 
 # Ceci veut dire que l'on ne peut pas rejetter l'hypothese nulle H0 en 
 # ayant 2% de chance de se tromper
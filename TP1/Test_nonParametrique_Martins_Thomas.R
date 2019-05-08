khi_deux <- function(th, obs){
  k = length(th)
  somme=0
  
  for(i in 1:k){
    somme = somme + ( (obs[i]-th[i])^2 )/th[i]  
  }
  somme
}

v_theorique <- function(matrice){
  l=nrow(matrice)
  k=ncol(matrice)
  res = matrix(nrow=l, ncol=k)
  
  for( i in 1:l ){
    for( j in 1:k ){
      res[i,j]= ( sum(matrice[,j])*sum(matrice[i,]) ) / sum(matrice)
    }
  }
  
  res
}

{
  # 7)
  # On definit ici 2 hypothèses :
  # H0 : Les valeurs de ratios sont correctes
  # H1 : Les valeurs de rations sont fausses
  ratio = c(9,3,3,1)
  obs = c(1528,106,117,381)
  
  violon_th = ( ratio[1]/sum(ratio) ) * sum(obs)
  vioron_th = ( ratio[2]/sum(ratio) ) * sum(obs)
  roulon_th = ( ratio[3]/sum(ratio) ) * sum(obs)
  rouron_th = ( ratio[4]/sum(ratio) ) * sum(obs)
  
  theorique = c(violon_th,vioron_th,roulon_th,rouron_th)
  khid=khi_deux(theorique,obs)
  
  # On a un score du khi-2 = 966,6 
  # Avec alpha = 5% et un degres de liberte = 3, on a un seuil = 7.81
  # on a alors la valeur calculé khi-2 bien superieur au seuil
  # On peut en conclure qu'on peut rejeter avec certitude l'hypothese H0
  # Ce qui signifie que les valeurs des ratios sont incorrectes
  
  
  # 8)
  forme = rbind( c(29, 5, 46), c(40,32,8), c(18,22,0))
  color = rbind( c(20,60), c(29,51), c(12,28) )
  
  color_th = v_theorique(color)
  forme_th = v_theorique(forme)
  
  khid_color = khi_deux(color_th,color)
  khid_form = khi_deux(forme_th,forme)
  
  # H0 = Les 2 variables testees sont independantes
  
  # Pour la couleur avec alpha = 5% et df = 2 on a 
  # un seuil = 5.99
  # On obtient la valeur du khi-2 = 2.39
  # valeur khi-2 < seuil on ne peut donc pas rejetter 
  # l'hyptothese H0 => On ne detecte pas le melanome a la couleur
  
  # Pour la forme avec alpha = 5% et df = 4 on a 
  # un seuil = 9.49
  # On obtient la valeur du khi-2 = 75.16 
  # Cette fois on a valeur khi-2 > seuil
  # On peut donc rejeter l'hypothese H0 => Le melanome se
  # detecte a la forme
  
  
  #Conclusion
  # 9 )
  # Pour realiser le test t Student, la repartition des donnees 
  # observees doit suivre une loi normal et avoir des variances
  # egales. Ce test est dit parametrique car la formule utilisee
  # pour faire le calcul depend de 2 parametres (la moyenne et l'ecart
  # type).
  #
  # Le test du Khi-2 lui en revanche ne necessite pas de distribution
  # statistique des donnees observees et ne prend pas en parametre la
  # moyenne ou l'ecart type c'est pour cela qu il est dit non parametrique
  #
  # Le test t student base ses calculs sur des donnees quantitative et
  # ne peut pas fonctionner avec des donnees qualitative car les donnees
  # doivent etre mesurable, quantifiable.
  
  # 10)
  # Pour etablir une correlation entre 2 variable il faut verifier si les
  # valeurs de 2 variables evolue ensemble, il faut donc que ces valeurs 
  # soient mesurables, on ne peut donc pas appliquer le coefficient de Spearman
  # ou de Pearson sur des donnees qualitative.
}

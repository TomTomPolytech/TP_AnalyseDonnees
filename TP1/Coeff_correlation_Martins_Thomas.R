 # AUTEUR : THOMAS MARTINS INFO4A - InSI

# fonction question 2)
coeff_r<-function(X, Y){
  cov=cov(X,Y)
  res=cov/(sd(X)*sd(Y))
  res
}

# fonction question 3)
spearmn<-function(X,Y){
  rankX=floor(rank(X))
  rankY=floor(rank(Y))
  sum=0
  
  for(i in 1:length(X)){
    sum=sum+ (rankX[i]-rankY[i])^2
  }
  res = 1 - ( (6*sum)/ (length(X)^3-length(X)) )
  res
}

{
  # 1)
  Database <- read.table("/home/fabrice/Documents/Polytech/AnalyseDonnees/data1TP1.txt", header = TRUE)
  par(mfrow = c(2,3))
  plot(Database$A, Database$Y)
  plot(Database$B, Database$Y)
  plot(Database$C, Database$Y)
  plot(Database$D, Database$Y)
  plot(Database$E, Database$Y)
  
  #On observe les nuages de points et on observe que les points des graphes :
  # A et B sont parfaitement lineaire et monotone  
  # D sont parfaitement monotones mais pas tres lineaire
  # C pas tres lineaire et ni tres monotone
  # E ni lineaire ni monotone
  
  # 2)
  rA=coeff_r(Database$A,Database$Y)
  rB=coeff_r(Database$B,Database$Y)
  rC=coeff_r(Database$C,Database$Y)
  rD=coeff_r(Database$D,Database$Y)
  rE=coeff_r(Database$E,Database$Y)
  
  # La variable avec la plus petite correlation est E car ses
  # valeurs ne sont pas lineaires
  
  rA_fct = cor(Database$A,Database$Y)
  rB_fct = cor(Database$B,Database$Y)
  rC_fct = cor(Database$C,Database$Y)
  rD_fct = cor(Database$D,Database$Y)
  rE_fct = cor(Database$E,Database$Y)
  
  # On verifie ensuite avec la fonction cor et on trouve bien les 
  # memes coefficients
  
  
  # 3)
  coeff_pA=spearmn(Database$A,Database$Y)
  coeff_pB=spearmn(Database$B,Database$Y)
  coeff_pC=spearmn(Database$C,Database$Y)
  coeff_pD=spearmn(Database$D,Database$Y)
  coeff_pE=spearmn(Database$E,Database$Y)
  
  # ici la difference avec les coefficient trouves precedemment 
  # avec la methode de Pearson et que ceux trouve avec la methode
  # de Spearman sont plus eleves pour les points qui sont monotone
  # Par exemple les points de D qui ont un coefficient de 1 sont 
  # parfaitement monotones
  
  coeff_pA_fct=cor(Database$A,Database$Y,method="spearman")
  coeff_pB_fct=cor(Database$B,Database$Y,method="spearman")
  coeff_pC_fct=cor(Database$C,Database$Y,method="spearman")
  coeff_pD_fct=cor(Database$D,Database$Y,method="spearman")
  coeff_pE_fct=cor(Database$E,Database$Y,method="spearman")
  
  # On compare avec la fonction cor Spearman et on trouve des resultat
  # identique a la 3eme decimale pres
  
  # 4)
  # On peut voir visuellement que qu'il y a bien
  # une correlation entre les donnees E et Y mais cela ne peut pas se 
  # savoir si on regarde le resultat des coefficients de Pearson et de
  # Spearman
  # Une solution proposee pourrait etre de diviser les donnees de E en
  # plusieurs sous intervalles ici en l'occurrence on pourrait le diviser
  # en 2 sous intervalles egaux qui donnerait des donnees lineaires et 
  # monotones pour E et Y
}

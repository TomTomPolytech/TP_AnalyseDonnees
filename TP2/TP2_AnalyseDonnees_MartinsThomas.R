{
  # AUTEUR : THOMAS MARTINS INFO4A - InSI
  library(rgl)
  Database <- read.delim("https://raw.githubusercontent.com/agusbudi/DataAnalysis/master/data1TP2.txt", header=TRUE, sep="\t")
  A=Database
   
  # 1) On trace en 3 dimensions le nuage de points
  x = A$Stature 
  y = A$Taille
  z = A$Poids
  plot3d(x,y,z,type="s",radius=0.8,col="red", xlab="X",ylab="Y",zlab="Z", add = FALSE) 
  
  # 2) On centre les valeurs de la matrice A dans B
  B = matrix(nrow = 10, ncol = 3)
  B[,1] = A[,1] - mean( A[,1] )
  B[,2] = A[,2] - mean( A[,2] )
  B[,3] = A[,3] - mean( A[,3] )
  
  # Ainsi que la matrice de covariance V
  V = cov(A)
  
  # 3) On determine la representation spectrale
  x = eigen(V)
  
  # 4) la representation spectrale x contient les valeurs propres de la matrice de covariance
  # ainsi que les vecteurs propres correspondant a ses valeurs propres.
  # Les vecteurs propres correspondent aux directions des axes principaux
  # Les valeurs propres correspondent a la quantite de variance des donnees associee aux vecteurs
  # propres ( ou composantes principales ou axes principales)
  # Donc plus une valeur propre est elevee, plus la composante pricipale associee (i.e vecteur
  # propre) donnera une quantité maximale de variation du jeu de données.
  # On evite alors au mieux la redondance des donnees si les valeurs propres sont elevees
  # 
  # On a donc :
  # un axe principal VP (valeur propre) = 97.08, vecteur propre=(-0.70,-0.69,-0.11)
  # un axe secondaire VP = 22.18, vecteur propre=(-0.43,0.30,0.84)
  # un axe secondaire VP = 6.34, vecteur propre=(0.56,-0.65,0.52)

  # 5) On genere le tableau C  
  C = B %*% x$vectors
  # On constate bien que princomp(A)$scores nous donne la meme matrice que C
  
  # 6) On visualise le nuage de points avec l'axe principale
  # Pour afficher l'axe pricipale on a besoin de 2 points en X, Y et Z
  # Le premier point est l'origine ( 0, 0, 0 )
  # Le second est la direction de l'axe principale * 20 pour qu'il soit bien
  # visible soit (-0.70,-0.69,-0.11) * 20
  X = c(0,20*(x$vectors[1,1]) )
  Y = c(0,20*(x$vectors[2,1]) )
  Z = c(0,20*(x$vectors[3,1]) )
  
  plot3d(B[,1],B[,2],B[,3],type="s",radius=0.8,col="red", xlab="Stature",ylab="Taille",zlab="Poids", add = FALSE)
  plot3d(X,Y,Z,type="l",radius=0.8,col="green", add=TRUE)

  # 7) On représente en 2D les donnees selon les 2 premiers axes 
  plot(C[,1],C[,2],xlab="Axe Principal (PC1)",ylab="Axe Secondaire (PC2)")

  # 8) Le resultat obtenu est en fait la meilleure façon de visualiser les donnees
  # Comme dit precedemment, les 2 premiers axes permettent de visualiser les donnees
  # avec une variance des donnees maximales, elles sont donc "dispatchees" au mieux
  # et contiennent un minimum de redondance
  # On a donc reduit d'une dimension les donnees en supprimant le moins d'information
  # possible
}
      

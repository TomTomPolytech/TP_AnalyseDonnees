#Author Martins Thomas - INFO 4A INsI
{
  par(mfrow = c(1,2))
  Nvalue=100
  genere_valeurs()
  #On commence ici par générer 3 classes de valeurs
  
  #On reparti les valeurs en 3 classes
  classe1 = cbind(val1_x,val1_y)
  classe2 = cbind(val2_x,val2_y)
  classe3 = cbind(val3_x,val3_y)
  
  #On met toutes les 300 valeurs dans tableau tab
  tab = rbind( classe1, classe2, classe3 )

  #On lance la fonction de classification par hiérarchie ascendante
  #avec comme critère d'arrêt 3 classes a atteindre
  res=hrch_ascd(tab,3)
  
  #On separe dans 3 tableaux les ensembles de points des 3 classes calculés
  {
    indice_classe1=(res[,1]==1)
    indice_classe2=(res[,2]==1)
    indice_classe3=(res[,3]==1)
    
    calcul_class1=separe_class(tab,indice_classe1)
    calcul_class2=separe_class(tab,indice_classe2)
    calcul_class3=separe_class(tab,indice_classe3)
  }
  
  #On affiche les points correspondants avec un couleur différente pour chaque classe
  plot(min(tab):max(tab), min(tab):max(tab), type = "n", xlab = "Mon calcul",ylab="")
  points(calcul_class1, type="p", col="red")
  points(calcul_class2, type="p", col="blue")
  points(calcul_class3, type="p", col="green")

  #On affiche également le centre de chaque classe
  points(centre_des_classes[1,1],centre_des_classes[1,2], pch=11)
  points(centre_des_classes[2,1],centre_des_classes[2,2], pch=11)
  points(centre_des_classes[3,1],centre_des_classes[3,2], pch=11)
  
  
  #Ici on refait la meme chose mais on utilise les fonction hclust et cutree
  #pour réaliser la classification par hierarchie ascendante
  D<-dist(tab,method = "euclidean")
  AscHierarchique <- hclust(D,method="complete")
  cluster=cutree(AscHierarchique,3)

  
  {
    indice_classe1=(cluster==1)
    indice_classe2=(cluster==2)
    indice_classe3=(cluster==3)
    
    calcul_class1_v=separe_class(tab,indice_classe1)
    calcul_class2_v=separe_class(tab,indice_classe2)
    calcul_class3_v=separe_class(tab,indice_classe3)
  }
  
  # On affiche les points de chaque classe comme précédemment
  plot(min(tab):max(tab), min(tab):max(tab), type = "n", xlab = "Fonction", ylab="")
  points(calcul_class1_v, type="p", col="red")
  points(calcul_class2_v, type="p", col="blue")
  points(calcul_class3_v, type="p", col="green")
  
  
  #On va maintenant calculer l'inertie relative intra classe
  #On commence par récupérer la distance entre tout les points de chaque classe
  inertie_intra_class1=dist(calcul_class1, method = "euclidean", p = 2)
  inertie_intra_class2=dist(calcul_class2, method = "euclidean", p = 2)
  inertie_intra_class3=dist(calcul_class3, method = "euclidean", p = 2)
  #On eleve ces distances au carré
  inertie_intra_class1=inertie_intra_class1^2
  inertie_intra_class2=inertie_intra_class2^2
  inertie_intra_class3=inertie_intra_class3^2
  # Il ne reste plus qu'à faire la somme de ces distances au carré
  inertie_intra_class=sum( inertie_intra_class1 )+sum(inertie_intra_class2)+sum(inertie_intra_class3)
  print(inertie_intra_class)
  
  #On constate que lorsque les points sont bien classé on a une intertie intra classe  
  #qui tourne autour de 56 000
}

#Ceci est la fonction hierarchique ascendante 
hrch_ascd<-function(tab,K){
  #On commence par initialiser une matrice identité carré de dimension n x n
  mat_id=diag(nrow(tab))
  
  #Tant qu'il plus de K classes (ici on choisi K=3 classes) 
  while(nrow(mat_id) > K){
    #On calcul la distance de tout les points
    m_dist=dist(tab, method = "euclidean", p = 2)
    test=as.matrix(m_dist)
    #On cherche les 2 points ayant la distance la plus petite qui les séparent
    lemin=which(test==min(m_dist), arr.ind = T)
    min1=lemin[2,1]
    min2=lemin[2,2]
    #On remplace dans le tableau des valeurs les valeurs en x et y du premier 
    #des 2 points les plus proches par la moyenne des valeurs en x et y des 2
    #points les plus proches
    tab[min1,1]=mean(cbind( tab[min1,1],tab[min2,1] ) )
    tab[min1,2]=mean(cbind( tab[min1,2],tab[min2,2] ) )
    #On supprime du tableau des valeurs le 2eme point des 2 points les plus proches 
    tab=tab[-min2,]
    #Dans la matrice binaire on fait la somme des lignes correspondant aux 2 points
    #les plus proches et on stock cette somme dans la ligne du premier point et on supprime
    #la ligne du 2eme point
    mat_id[min1,]=mat_id[min1,]+mat_id[min2,]
    mat_id=mat_id[-min2,]
  }
  #On sauvegarde le centre des classes dans la variable globale centre_des_classes
  centre_des_classes<<-tab
  #Puis on renvoi la matrice binaire renseignant la classe de chaque point du tableau initial
  #tab
  t(mat_id)
  
  #On observe que le calcul de ma fonction classe les 300 points générés de manière 
  #assez similaire à la fonction hclust. 
  #Cependant les 2 fonctions classes parfois assez mal les 300 points. Pour palier a ce 
  #probleme on pourrait par exemple explorer d'autres maniere de fusionner 2 classes
  #au lieu de fusionner les 2 classes ayant leurs centre d'inertie le plus proche on
  #pourrait fusionner 2 classes qui contiennent ayant chacun un point proche de l'autre
} 

#Cette fonction sert simplement a separer les points de tab qui se situe à l'index
# indice de tab
separe_class <- function(tab, indice){
  temp1=tab[,1]
  temp1=temp1[indice==1]
  temp2=tab[,2]
  temp2=temp2[indice==1]
  class=cbind(temp1,temp2)
  class
}

#Cette fonction génère simplement les valeurs demandé au 1) dans des variables
#globales
genere_valeurs <- function(){
  val1_x<<-runif(Nvalue,0,1)
  val1_y<<-runif(Nvalue,0,1)
  
  val2_x<<-rnorm(Nvalue, 4, 1)
  val2_y<<-rnorm(Nvalue, 0, 1)
  
  val3_x<<-rnorm(Nvalue, 0.5, sqrt(2))
  val3_y<<-rnorm(Nvalue, 6, sqrt(2))
  
  min_x<<-min(min(val1_x),min(val2_x),min(val3_x))
  max_x<<-max(max(val1_x),max(val2_x),max(val3_x))
  
  min_y<<-min(min(val1_y),min(val2_y),min(val3_y))
  max_y<<-max(max(val1_y),max(val2_y),max(val3_y))
}

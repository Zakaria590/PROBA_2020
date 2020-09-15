# TP 3

# Partie 
#1 Simuler une somme de variables aléatoires indépendantes

#Q1
Unif <-function(N,a,b){
  runif(N,a,b)
}

#Q2
Norm<-function(N,mu,sd){
  rnorm(N,mu,sd)
}
Norm(10,0,1)#pour la loi normale centre a 0 et reduite a 1

# Question 3
unif = function(n,a,b,c,d) {
  x=runif(n,a,b) # n abscisses prises aléatoirement entre a et b
  y=runif(n,c,d) # n ordonnées prises aléatoirement entre c et d
  coord=matrix(c(x,y),n,2)
  colnames(coord) = c("Abscisse", "Ordonnée")
  print(coord)
}

#le 27/04/2020

# Test
points=unif(100,0,1,3,4)
plot(points[,1],points[,2]) # représentation des points suivant leur abscisse et leur ordonnée 

SommeUnifNorm <- function(N, a, b, µ, s) {
  S = matrix(c(runif(N, a, b), rnorm(N, µ, s)), nrow = N, ncol = 2) # X est la première colonne de S et Y la seconde
  
  Z = rowSums(S)
  return(Z)
}



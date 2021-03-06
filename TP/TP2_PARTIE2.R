#TP2_Partie2
#Loi forte des grands nombres
M=2000
X = runif(M,0,1) #cree M variable aleatoire uniforme
S = cumsum(X) #cree vecteur des sommes succesif 
N = seq(1,M, by=1) #des valeur entre 1 et M le pas c'est un 
plot(N, S/N) #dessin de Sn/N en fonction de N 
abline(h=0.5, col="red") #ligne horizontale d'ordre E[X 1].
#on peut dire que quand N est grand Sn/N tende vers meu et cest LDGN 
#le graphe a chaque fois qu'on compile le code par ce que on produit des nouvelle variable aleatoirre defirantes des pricidente 

#Illustrations pour d’autres lois
funif <- function(a,b,N){
  X = runif(N,a,b)
  S = cumsum(X)
  n = seq(1,N, by=1)
  plot(n, S/n)
  abline(h=(a+b)/2, col="red")
}
funif(2,5,2000)

fexp <- function(a,N){
  X = rexp(N,a)#rexp genere N variable aléatoire de loi exponentielle de paramètre alpha = a dans notre cas
  S = cumsum(X)
  n = seq(1,N, by=1)
  plot(n, S/n)
  abline(h=1/a, col="red")
}
fexp(10,10000)
#le graphe a chaque fois qu'on compile le code par ce que on produit des nouvelle variable aleatoirre defirantes des pricidente 
#donc on peut dire que pour la somme des variable aleatoire de meme lois et de meme parametre quand N est grand est grand la lois tend vers mue

fcauchy <- function(N){
  X = rcauchy(N)
  S = cumsum(X)
  n = seq(1,N, by=1)
  plot(n, S/n)
}
fcauchy(100)
# a chque fois qu'on rexecute le code on a des points differents 
#quand on augemente le N on as une forme qui resemble a la lois expo





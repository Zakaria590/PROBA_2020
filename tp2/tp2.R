#rendu 15/03/2020
#Salah Zakaria OUAICHOUCHE
#Groupe : 3
#TP2
#les reponses en commentaires 

#Partie 1 : Répartitions observée et répartition théorique
#Binomial pour l les  lois descrete on optien la probabilite pour chaue points (pas de densite de proba)
dbinom(5, 10,0.5) #->0.2460938
# 5 : vecteur de quantiles
# 10 : nombre d'essais 
# 0.5 :logique; si VRAI, les probabilités p sont données sous forme de log (p)
#probabilite c'est toujour entre 0 et 1

##va nous donne une probabilite 
dbinom(5, 4,0.8) #->0 pas possible (proba nul)
# probabilite c'est 0
dbinom(5, 5,1) #->1
# probabilite c'est 1
rbinom(10, 5, 0.8) #->5 3 4 3 4 5 3 4 5 5 binomial(n=5,p=0.8) qui teste la 
# r essaie alleatoire (random)
# 10 c'est la taille 
# 5 c'est le nombre d'essaie 
# 0.8 probabilité de succès à chaque essai
# donc la on fait 5 essai aleatoire de probabilite 0.8 de succes a  chaque essqie et notre taille q'on affiche c'est 10

#2 tirage aleatoire
c("pile", "face") -> piece
sample(piece, 1)
sample(piece, 10, replace = TRUE)

#3 lois uniforme pour les lois continue on optien la densite de probabiliter 
#help(dunif)
#help(runif)
dunif(5, 0,10)  #->0.1
#densite uniforme toujour entre 0 et 1
dunif(5, 0,4) #->0
#
dunif(5, 0,20) #->0.05
#
runif(10, -2,3)
#on tire aleatoirement 10 valeur entre -2 et 3

#4 simulation de la realisation de loi uniforme sur [-2,3]
x=runif(100000,-2,3) 
hist(x,freq=FALSE)
y=runif(100,-2,3)

#pour les reprisenter on fait hist(x)
#on peut remarque que quent on augemente la taille de la donnee l'histogramme deuniforme

#5 simulation N=10 jets de de
x=runif(10,1,6)
hist(x,freq=FALSE)
#on augmente N 
x=runif(1000000,1,6)
hist(x,freq=FALSE)
# qund on augmente N on remarque que notre histogramme devien uniforme on peut le remarque avec la barre verte que je viens d'integrer 
x=runif(1000000,1,6)
hist(x,freq=FALSE)
abline(h=0.2,col="green")

#6 tracer sur le graphe 
densite <- density(lancerUnif) # estimer la densit? que represente ces diff?rentes valeurs
curve(dunif(x, 1,6),
      from = 1,to = 6,
      ylab="densit?",
      add=T,
      col="red")

#7 Reprendre les questions qui precedent
y=rnorm(10000,-1,2)
hist(y,freq=FALSE)
#curve(exp((-(x+1)^2)/8)(2*sqrt(2*pi)),-10,8,add=TRUE,col="red")

# Loi binomiale
xbin = rbinom(100,100,0.3)
gbinom(100, 0.3)
hist(xbin,
     probability=T,  #permet de mettre l'histogramme sous forme de proba
     add=T)


# loi exponentielle
#
expo = rexp(100,50) 
hist(expo,
     probability=T,
     breaks = 10)
curve(dexp(x,50),
      ylab="densit?",
      add=T,
      col="red")

# loi Normale
gnorm(-1, 2)
norm = rnorm(1000,mean = -1,sd = 2) # simulation d'un ?chantillon d'une loi normale d'esp?rance 1 et de variance 4
hist(norm,
     probability=T,
     add = T)    #ajouter u dessus du pr?c?dent graph

#curve(dnorm(x,mean = -1,sd = 2),
#      from = -6,to = 5,
#      ylab="densit?",
#      add=T,
#      col="red")

#on peut remarquer de cette partie qu'on a pratiquement la meme alure de courbe et ca independamenet de la loi utiliser


# TP 2 - Partie 1
# Répartition observée et répartition théorique


# Question 1 : commandes dbinom et rbinom
  # http://pages.stat.wisc.edu/~larget/R/prob-R.pdf

help(dbinom) 
  # dbinom(x, size, prob) donne la proba d'obtenir x succès sur size épreuves de Bernouilli de proba prob de succès ie P(X=x)
dbinom(3, 5, 0.3)
help(pbinom)
  # pbinom(q, size, prob) donne la valeur de la fonction de répartition d'une Bin(size, prob) au point q
help(qbinom)
  # qbinom(p, size, prob) donne la plus petite valeur pour laquelle la fonction de répartition d'une Bin(size, prob) vaut p 
help(rbinom) 
  # rbinom(n, size, prob) génére une suite de n nombres représentant le nombre de succès d'une binomiale de paramètres (size, prob)
  # simule n lois binomiales de paramètres (size, prob)

dbinom(5, 10, 0.5) # proba d'obtenir 5 succès suivant une Bin(10, 0.5)
dbinom(5, 4, 0.8) # 5 succès sur 4 tirages => proba nulle
dbinom(5, 5, 1) # 5 succès sur 5 tirages, tous de proba 1 de succès => proba 1
rbinom(10, 5, 0) 


# Question 2

N=100
  # N lancers d'une pièce équilibrée, pile vaut succès (1) avec proba 1/2
  # N représentations d'une Bernoulli de paramètre 1/2, i.e. binomiale de paramètres (1, 0.5)
x = rbinom(N, 1, 0.5)
sum(x==1)

# Question 3

help(dunif)
  # dunif(x, min=0, max=1) donne la valeur de la fonction de densité au point x 
help(punif)
  # punif(q, min=0, max=1) donne la valeur de la foncton de répartition au point p
help(qunif)
  # qunif(p, min=0, max=1) donne la plus petite valeur pour laquelle la fonction de répartition vaut p
help(runif)
  # runif(n, min = 0, max = 1), génère n observations d'une loi uniforme sur (0;1)
  # ne génère pas les bornes sauf si max==min ou max-min est petit par rapport à min

dunif(5, 0, 10) # valeur prise par la fonction de densité d'une uniforme sur (0;10) au point 5, soit 0.1 (on divise par 10, longueur de l'intervalle)
dunif(5, 0, 4) # valeur prise par la fonction de densité d'une uniforme sur (0;4) au point 5, soit 0 (5 n'appartient pas à l'intervalle) 
dunif(5, 0, 20) # valeur prise par la fonction de densité d'une uniforme sur (0;10) au point 5, soit 0.05

runif(10, -2, 3) # génère 10 nombres aléatoires (uniformément) sur (-2;3)


# Question 4

N=100
  # N réalisations d'une loi uniforme sur (2;3)
r=runif(N, -2, 3)

hist(r, main="Histogramme de 100 réalisations d'une loi uniforme sur [-2,3]", ylab="Fréquence, nombre d'observations")

# Idem pour N plus grand
N=10000
r=runif(N, -2, 3)
hist(r, main="Histogramme de N réalisations d'une loi uniforme sur [-2,3]", ylab="Fréquence, nombre d'observations")


# Question 5

N=10
X=runif(N, 0, 6)
#On crée un vecteur de 6 entrées qui va donner le nombre d'apparitions de chaque face du dé en divisant l'intervalle de départ en 6 sous-intervalles
d=rep(0,6)
for (x in X) {
  for (i in 1:6){
    if (x <= i && x > i-1) {d[i] <- d[i]+1}
  }  
}
print(d)  
# Histogramme de N jets de dé
hist(X, main="Histogramme du nombre d'apparition de chaque face après 10 lancers de dé", xlab="Num de la face", ylab = "Nombre d'apparitions",  breaks=6)
# Pour N plus grand
N=1000
X=runif(N, 0, 6)
hist(X, main="Histogramme du nombre d'
     apparition de chaque face après N lancers de dé", xlab="Num de la face", ylab = "Nombre d'apparitions",  breaks=6)


# Question 6

xmin=-1
xmax=7
f = function(x, a, b) {
  if (x>=a && x<=b) {1/(b-a)} else {0}
}
h = Vectorize(f)
hist(runif(10, 0, 6), freq=FALSE, main="Histogramme des fréquences d'apparition de chaque face après 10 lancers de dé", xlab="Num de la face", ylab = "Fréquence relative",  breaks=6)
curve(h(x, 0, 6), from=xmin, to=xmax, col="blue", add=TRUE)
legend("topright", legend=c("fonction de densité"), col=c("blue"), lwd=2)

# Autre méthode
x = seq(-1, 7, length.out = 1000)
plot(x, dunif(x, 0, 6), type="l", col="red", asp=10.0) # asp est le ratio y/x
hist(runif(10, 0, 6), freq=FALSE, add=TRUE, main="Histogramme des fréquences d'apparition de chaque face après 10 lancers de dé", xlab="Num de la face", ylab = "Fréquence relative",  breaks=6)
legend("topright", legend=c("fonction de densité"), col=c("red"), lwd=2)

# Avec un nombre de lancers plus grand...
xmin=-1
xmax=7
f = function(x, a, b) {
  if (x>=a && x<=b) {1/(b-a)} else {0}
}
h = Vectorize(f)
hist(runif(100, 0, 6), freq=FALSE, main="Histogramme des fréquences d'apparition de chaque face après 100 lancers de dé", xlab="Num de la face", ylab = "Fréquence relative",  breaks=6)
curve(h(x, 0, 6), from=xmin, to=xmax, col="blue", add=TRUE)
legend("topright", legend=c("fonction de densité"), col=c("blue"), lwd=2)

# Encore plus grand
xmin=-1
xmax=7
f = function(x, a, b) {
  if (x>=a && x<=b) {1/(b-a)} else {0}
}
h = Vectorize(f)
hist(runif(1000, 0, 6), freq=FALSE, main="Histogramme des fréquences d'apparition de chaque face après 1000 lancers de dé", xlab="Num de la face", ylab = "Fréquence relative",  breaks=6)
curve(h(x, 0, 6), from=xmin, to=xmax, col="blue", add=TRUE)
legend("topright", legend=c("fonction de densité"), col=c("blue"), lwd=2)


# Question 7

N=10000

### Loi binomiale ###
n=10
p=0.4

binom= function(x) {
  dbinom(as.integer(x), n, p)*N
}
x=rbinom(N, n, p)
hist(x, right=FALSE, main="Histogramme des fréquences de succès de 10000 simulations de loi B(10, 0.4)", breaks=seq(-1,n+1))
# right=FALSE, inclure les valeurs inf dans la classe de l'histogramme [a,b) au lieu de (a,b]
curve(binom, col='blue', add = TRUE)
legend("topright", legend=c("fonction de densité"), col=c("blue"), lwd=2)

### Loi exponentielle ###

x=rexp(N, 1)
hist(x, freq=FALSE, main="Histogramme des densités de 10000 simulations de loi exponentielle de paramètre 1")
curve(dexp(x,1), col='blue', add = TRUE)
legend("topright", legend=c("fonction de densité"), col=c("blue"), lwd=2)

### Loi normale ###

µ=-1
sigma=2
f = function(x) {
  exp(-((x-µ)^2)/(2*sigma^2))/(sigma*sqrt(2*pi))
}

x=rnorm(N, µ, sigma)
hist(x, freq=FALSE, main="Histogramme des densités de 10000 simulations de loi normale N(-1,2)")
curve(f, col='blue', add = TRUE)
legend("topright", legend=c("fonction de densité"), col=c("blue"), lwd=2)


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

fcauchy <- function(){
  X = runif(N,a,b)
  S = cumsum(X)
  n = seq(1,N, by=1)
  plot(n, S/n)
  abline(h=(a+b)/2, col="red")
}
funif(2,5,2000)






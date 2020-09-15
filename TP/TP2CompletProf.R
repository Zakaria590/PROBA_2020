## TP2 with R
## C. Calgaro
## L2 info 2020
##==============

##=========
## PARTIE 1
##=========
##===
## 1)
##===
## The binomial distribution with size = n and prob = p has density
## p(X=x) = choose(n, x) p^x (1-p)^(n-x) for x = 0, …, n. 
## The binomial coefficients is computed by choose in R.
## If an element of x is not integer, the result of dbinom is zero.

help("dbinom")
## dbinom(x, size, prob, log = FALSE)
## x = vector of quantiles
## size=number of trials (zero or more).
## prob=probability of success on each trial.
## log, log.p=logical; if TRUE, probabilities p are given as log(p).

help("rbinom")
## rbinom(n, size, prob)
## n=	number of observations
## size=number of trials (zero or more).
## prob=probability of success on each trial.

## dbinom gives the density
## rbinom generates random deviates.

dbinom(5,10,0.5) ## densité de proba d'avoir exactement 5 succes pour size=10 tirages, avec p=0.5
## c'est P(X=5) avec X qui suit la loi binomiale B(n,p)=B(10,0.5)
## [1] 0.2460938
dbinom(5,4,0.8) ## densité de proba d'avoir exactement 5 succes pour size=4 tirages, avec p=0.8
## c'est P(X=5) avec X qui suit la loi binomiale B(n,p)=B(4,0.8)
## [1] 0
dbinom(5,5,1) ## densité de proba d'avoir exactement 5 succes pour size=5 tirages, avec p=1
## c'est P(X=5) avec X qui suit la loi binomiale B(n,p)=B(5,1)
## [1] 1
rbinom(10,5,0.8) # c'est un vecteur de 10 entiers random pour 5 tirages avec proba de succès=0.8
## [1] 4 4 4 5 4 4 4 4 5 4
rbinom(10,5,0.2) # c'est un vecteur de 10 entiers random pour 5 tirages avec proba de succès=0.2
## [1] 1 0 2 0 0 1 2 1 1 2

##===
## 2)
##===
rbinom(100,1,0.5) # c'est un vecteur de 100 entiers random entre 0 et 1 avec proba de succès=0.5 (voir pile ou face)
## [1] 0 0 0 1 0 0 1 0 1 0 0 0 0 0 1 1 1 1 0 0 0 0 0 1 1 0 0 0 0 0 1 0 0 1 0 1 1 1 1 1 0 1 1 0 1 1 1 0 0 0 1 0 0 1 1
##[56] 1 0 1 1 1 1 1 0 1 1 0 1 0 1 1 1 1 0 0 1 0 0 0 0 1 0 1 1 0 1 1 1 0 0 1 0 0 0 1 1 1 0 1 0 0

##===
## 3)
##===
## uniform distribution on the interval from min to max
## The uniform distribution has density
## f(x) = 1/(max-min) for min ≤ x ≤ max, 0 sinon.

help("dunif") ## dunif gives the density
help("runif") ## runif generates random deviates

## dunif(x, min = 0, max = 1, log = FALSE) ## gives the density
## punif(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE) ## gives the distribution function 
## qunif(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE) ## gives the quantile function
## runif(n, min = 0, max = 1) ## generates random deviates

dunif(5,0,10) ## c'est la densité f(x=5)=1/(10-0)
## [1] 0.1
dunif(5,0,4) ## c'est la densité f(x=5)=1/(4-0)
## [1] 0
dunif(5,0,20) ## c'est la densité f(x=5)=1/(20-0)
## [1] 0.05
runif(10,-2,3) ## génère 10 valeurs random entre xmin=-2 et xmax=3
##[1]  2.1758752 -1.7649208  1.6940093  1.9735791  1.9187619  2.7472691  1.0974820 -1.7182855 -0.3854851  1.2863574

##===
## 4)
##===
## plus N augmente, plus la valeur tend vers 1/5
v_100<-runif(100,-2,3)
hist(v_100,freq=FALSE,xlim=c(-2,3),breaks = 10)
v_500<-runif(500,-2,3)
hist(v_500,freq=FALSE,xlim=c(-2,3),breaks = 10)
v_5000<-runif(5000,-2,3)
hist(v_5000,freq=FALSE,xlim=c(-2,3),breaks = 10)

##===
## 5)
##===
## plus N augmente, plus la valeur tend vers 1/5
u_10<-runif(10,1,6)
hist(u_10,freq=FALSE,xlim=c(1,6),breaks = 3)
u_100<-runif(100,1,6)
hist(u_100,freq=FALSE,xlim=c(1,6),breaks = 6)
u_1000<-runif(1000,1,6)
hist(u_1000,freq=FALSE,xlim=c(1,6),breaks = 6)
u_10000<-runif(10000,1,6)
hist(u_10000,freq=FALSE,xlim=c(1,6),breaks = 6)

##===
## 6)
##===
## loi uniforme
N=10000
uN<-runif(N,-2,3)# mettre N=10000 realisations pour bien voir
x=seq(-2,3,0.1)
loi_unif<-dunif(x,-2,3)
plot(x,loi_unif) ## ou plot(x,loi_unif),"l") # ici les ordonnées entre 0.1 et 0.3
hist(uN,freq=FALSE,xlim=c(-2,3),breaks = 10, add = TRUE) # ici les ordonnées entre 0 et 0.3 aussi

##===
## 6-bis)
##===
## loi uniforme
N=10000
uN<-runif(N,1,6)# mettre N=10000 realisations pour bien voir
x=seq(1,6,0.1)
loi_unif<-dunif(x,1,6)
plot(x,loi_unif) ## ou plot(x,loi_unif),"l") # ici les ordonnées entre 0.1 et 0.3
hist(uN,freq=FALSE,xlim=c(1,6),breaks = 6, add = TRUE) # ici les ordonnées entre 0 et 0.3 aussi

##===
## 7)
##===
## loi binomiale
N=10000
bN<-rbinom(N,30,0.3)# mettre N=10000 realisations pour bien voir
x<-0:30
loi_binom<-dbinom(x,30,0.3)
plot(x,loi_binom)
hist(bN,freq=FALSE,xlim=c(0,30),breaks = 20, col = "blue", add = TRUE)

## loi exponentielle avec differents alpha
## rexp(n, rate=alpha) # n tirages, rate λ=alpha
#If rate is not specified, it assumes the default value of 1.
#The exponential distribution with rate λ has density
#f(x) = λ {e}^{- λ x} for x ≥ 0.

alpha=0.1;
e_1000 <- rexp(1000, rate=alpha) # 1000 tirages, loi exponentielle
curve(alpha*exp(-alpha*x), 0, 30, col = "red")
hist(e_1000,freq=FALSE,xlim=c(0,30),breaks = 20, add = TRUE)
alpha=0.5;
e_1000 <- rexp(1000, rate=alpha) # 1000 tirages, loi exponentielle
curve(alpha*exp(-alpha*x), 0, 30, col = "red")
hist(e_1000,freq=FALSE,xlim=c(0,30),breaks = 20, add = TRUE)
alpha=1;
e_1000 <- rexp(1000, rate=alpha) # 1000 tirages, loi exponentielle
curve(alpha*exp(-alpha*x), 0, 30, col = "red")
hist(e_1000,freq=FALSE,xlim=c(0,30),breaks = 20, add = TRUE)
alpha=2;
e_1000 <- rexp(1000, rate=alpha) # 1000 tirages, loi exponentielle
curve(alpha*exp(-alpha*x), 0, 30, col = "red")
hist(e_1000,freq=FALSE,xlim=c(0,30),breaks = 20, add = TRUE)


## loi normale
## rnorm(n, mean = 0, sd = 1)
## If mean or sd are not specified they assume the default values of 0 and 1, respectively.
## The normal distribution has density
## f(x) = 1/(√(2 π) σ) e^-((x - μ)^2/(2 σ^2))
## where μ is the mean of the distribution and σ the standard deviation.

mu=-1
s=2
curve(exp(-(x-mu)^2/(2*(s^2)))/(s*sqrt(2*pi)), -10,10, col = "red")
n_1000 <- rnorm(1000,mean=mu,sd=s)   # 100 tirages, loi N(-1,4)
hist(n_1000,freq=FALSE,xlim=c(-10,10),breaks = 20, add = TRUE)
mu=0
s=1
curve(exp(-(x-mu)^2/(2*(s^2)))/(s*sqrt(2*pi)), -10,10, col = "red")
n_10000 <- rnorm(10000,mean=mu,sd=s)   # 100 tirages, loi N(-1,4)
hist(n_10000,freq=FALSE,xlim=c(-10,10),breaks = 20, add = TRUE)
mu=5
s=4
curve(exp(-(x-mu)^2/(2*(s^2)))/(s*sqrt(2*pi)), -10,30, col = "red")
n_10000 <- rnorm(10000,mean=mu,sd=s)   # 100 tirages, loi N(-1,4)
hist(n_10000,freq=FALSE,xlim=c(-10,30),breaks = 20, add = TRUE)


##=========
## PARTIE 2
##=========
##===
## 1)
##===
## Pour un échantillon de la loi uniforme sur [0,1] de taille n=1000, 
## calculons les moyennes empiriques successives et traçons la moyenne empirique 
## en fonction de la taille de l'échantillon. 
## On observe que la moyenne empirique converge bien vers la valeur $\mu=0.5$ 
## représentée par une droite horizontale rouge.

n<-1000
X<-runif(n, min = 0, max = 1) ## n v.a.i. de loi uniforme
Y<-cumsum(X) ## somme cumulative
N<-seq(1,n, by=1)
plot(N, Y/N,"l")
abline(h=mean(X),col="red")

## idem pour la loi exponentielle
n<-10000
alpha=2;
curve(alpha*exp(-alpha*x), 0, 30, col = "red")
X<-rexp(n, rate=alpha) ## n v.a.i. de loi exponentielle
Y<-cumsum(X) ## somme cumulative
N<-seq(1,n, by=1)
plot(N, Y/N,"l")
abline(h=mean(X),col="red")

## idem pour la loi exponentielle
n<-10000
alpha=1;
curve(alpha*exp(-alpha*x), 0, 30, col = "red")
X<-rexp(n, rate=alpha) ## n v.a.i. de loi exponentielle
Y<-cumsum(X) ## somme cumulative
N<-seq(1,n, by=1)
plot(N, Y/N,"l")
abline(h=mean(X),col="red")

##===
## 2)
##===

## The Cauchy distribution with location l and scale s has density
## f(x) = 1 / (π s (1 + ((x-l)/s)^2)) for all x.

## dcauchy(x, location = 0, scale = 1, log = FALSE)
## rcauchy(n, location = 0, scale = 1)

n<-10000
X<-rcauchy(n, location = 0, scale = 1) ## n v.a.i. avec la loi de Cauchy
curve(1/(pi*(1+x^2)), -20, 20, col = "red")
Y<-cumsum(X) ## somme cumulative
N<-seq(1,n, by=1)
plot(N, Y/N,"l")
ab=-100:100
Z<-dcauchy(ab, location = 0, scale = 1, log = FALSE)
mean(Z)

##==========================
## PARTIE 3
## THEOREME LIMITE CENTRALE
## Soyent X1, ... , XN N v.a.i. de même loi, d'espérence mu et variance sigma.
## Alors leur somme SN=X1+...+XN suit une loi normale d'espérence mu*N et variance sigma*sqrt(N)
##==========================

##================================
## TLC avec loi uniforme sur [0,1]
##================================

## FIXER LES PARAMETRES
N=1000  ## Nombre d'observations
M=10000 ## nombre de répétitions de l'expérience (prendre M très grand)
## Q1
matrice = matrix(runif(M*N,min=0,max=1), nrow=N, ncol=M)
## Q2
help(colMeans)
moyennes = colMeans(matrice)  ## Moyenne par colonne
##SN=(1/N)*colSums(matrice);  ## c'est pareil : Moyenne par colonne
## Q3
hist(moyennes, breaks=2*M**(1/3), freq=FALSE)
## Q4
moy=0.5 # moyenne de la loi uniforme
sigma2=1/12 # variance de la loi uniforme
curve(dnorm(x, mean=moy, sd=sqrt(sigma2/N)), col="red", add=TRUE)
## pareil que
## curve((2*pi*(sigma2/N))^(-1/2)*exp(-(x-moy)^2/(2*sigma2/N)), add=T, col="blue")

##================================
## TLC avec loi uniforme sur [a,b]
##================================

##Q5
TCL_uniforme <- function(N=1000, M=10000, a=1, b=6){
  titre = paste("Distribution de la moyenne de", N, " variables \n de loi uniforme sur [", a, ",", b, "]")
  hist(colMeans(matrix(runif(M*N,min=a,max=b), nrow=N, ncol=M)),
       breaks=2*M**(1/3), 
       freq=FALSE,
       xlab = "",
       ylab = "",
       main = titre,
       cex.main = 0.9)
  moy=(a+b)/2 # moyenne de la loi uniforme
  sigma2=(b-a)^2/12 # variance de la loi uniforme
  sigmaN=sqrt(sigma2/N)
  curve(dnorm(x, mean=moy, sd=sigmaN), add=TRUE, from=moy-4*sigmaN, to=moy+4*sigmaN, col="red")
}
TCL_uniforme()

##===========================
## TLC avec loi exponentielle
##===========================

## Q6
TCL_exp = function(N=100, M=10000, alpha=1){
  titre = paste("Distribution de la moyenne de", N, " variables \n de loi exponentielle de paramètre", alpha)
  hist(colMeans(matrix(rexp(M*N,rate=alpha), nrow=N, ncol=M)),
       breaks=2*M**(1/3),
       freq=FALSE,
       xlab = "",
       ylab = "",
       main = titre,
       cex.main = 0.9)
  moy=1/alpha
  sigma2=1/alpha^2
  curve(dnorm(x, mean=moy, sd=sqrt(sigma2/N)), add=TRUE, col="red")
}
TCL_exp()

##=======================
## TLC avec loi de Cauchy
##=======================

## Q7
TCL_cauchy <- function(N=100, M=10000){
  titre = paste("Distribution de la moyenne de", N, "variables \n de loi de Cauchy")
  moyennes = colMeans(matrix(rcauchy(M*N), nrow=N, ncol=M))
  # on supprime les trop grandes valeurs
  # pour avoir un histogramme lisible
  moyennes = moyennes[moyennes < 10]
  moyennes = moyennes[moyennes > -10]
  size = length(moyennes)
  hist(moyennes,
       freq=FALSE,
       breaks = 2 *(size)**(1/3),
       #ylim = c(0,1/pi),
       xlab = "",
       ylab = "",
       main = titre,
       cex.main = 0.9)
  curve(dcauchy(x), add=TRUE, col="red")
  curve(dnorm(x,mean(moyennes),sd(moyennes)),add=TRUE, col="blue")
}

TCL_cauchy(10)
TCL_cauchy(100)
TCL_cauchy(1000)

##===============================================
## TCL avec loi de Bernouilli de paramètres (n,p)
##===============================================

## Q8
TCL_bernouilli = function(N=1000, M=10000, n=50, p=0.5){
  titre = paste("Distribution de la moyenne de", N, "variables \n de loi de Bernouilli B(",n,",",p,")")
  matrice=matrix(rbinom(N*M,size=n,p), nrow=N, ncol=M);
  moyennes = colMeans(matrice)
  ##SN=(1/N)*colSums(matrice);  ## c'est pareil : Moyenne par colonne
  hist(moyennes,
       breaks=2*M**(1/3),
       freq=FALSE,
       xlab = "",
       ylab = "",
       main = titre,
       cex.main = 0.9)
  moy=n*p
  sigma2=n*p*(1-p)
  curve(dnorm(x, mean=moy, sd=sqrt(sigma2/N)), add=TRUE, col="red")
}
TCL_bernouilli()

##=================================
## PARTIE 4
## Etude de la formule de transfert
##=================================

N=1000;
a=5;

## Q1
X<-runif(N, -a,a);
Y<-X^2
hist(Y, freq=FALSE) 
curve((2*a)^(-1)*(sqrt(x))^(-1), add=TRUE)

X2<-runif(N, 0,a);
Y2<-X2^2
hist(Y2, freq=FALSE) 
curve((2*a)^(-1)*(sqrt(x))^(-1), add=TRUE)

## Q2
X3<-runif(N, -a,a);
Y3<-X3^3
hist(Y3, freq=FALSE,ylim=c(0,0.02)) 
x=seq(-a^3,a^3,1)
curve(1/(2*a*abs(x)), from=-a^3, to=a^3, add=TRUE)

##Q3
## Après calcul sur papier, on trouve
## pour Y=X^2
## rho_Y(y)= 1/(2 a sqrt(y)) pour y dans [0,a^2]
## Par tatonemment 
## pour Y=X^3
## rho_Y(y)= 1/(2 a abs(y) pour y dans [-a^3,a^3]
## on verifie sur papier qu'on a bien E(Y)=0

##Q4
N=10000
alpha=1
X<-rexp(N,rate=alpha)
Y<-X^2
hist(Y, breaks=2*N**(1/2),
     xlim = c(0,10),
     freq=FALSE) 
x=seq(0,10,0.1)
curve((alpha/sqrt(x))*exp(-alpha*sqrt(x)), add=TRUE)


##===============================
## PARTIES 5-6
## Un autre exemple de loi limite
##===============================

N = 100
M = 10000

# On adapte les question de la partie TCL
# Construction d'une matrice avec M*N variables uniformes
matrice = matrix(runif(M*N), nrow=N, ncol=M)

help(apply)
# On calcule le maximum de chaque colonne
maxima = apply(matrice, 2, max)
# On calcule le minimum de chaque colonne
minima = apply(matrice, 2, min)

# On fait l'histogramme de maxima
hist(maxima, breaks=2*M**(1/3), freq=FALSE)

# On fait l'histogramme de minima
hist(minima, breaks=2*M**(1/3), freq=FALSE)

# On fait l'histogramme de 1-maxima
hist(1-maxima, breaks=2*M**(1/3), freq=FALSE)

# On compare avec la densité d'une loi exponentielle
curve(dexp(x, rate=N), 
      add=TRUE, 
      col="red")






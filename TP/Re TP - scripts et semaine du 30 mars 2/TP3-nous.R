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

# Q3
SommeUnifNorm <- function(N,a,b,mu,sd) {
  X=Unif(N,a,b) 
  Y=Norm(N,mu,sd) 
  Z=X+Y
  return(Z)
}
N = 100
Z=SommeUnifNorm(N,5,7,8,4)
n = seq(1,N,by=1)
plot(n, Z)##à verifie
abline(h=1/2*(5+7)+8,col="green")
# Q4
#Esperance
#E(Z)=E(X+Y)=E(X)+E(Y)
#1/2(b+a)+mu =14
# comment calculer la variance de V(X+Y)
 #Variance 

#Autres cas
#1 lois exponentielles
Expo <-function(N,a){
  rexp(N,a)
}

SommeExpo <- function(N,mu,alpha) {
  X=Expo(N,mu) 
  Y=Expo(N,alpha) 
  Z=X+Y
  return(Z)
}
N = 100
Z=SommeExpo(N,5,7)
n = seq(1,N,by=1)
plot(n, Z)##à verifie
abline(h=1/5+1/7,col="red")
#Esperance
#E(Z)=E(X+Y)=E(X)+E(Y)
#1/mu+1/alpha =0.3
# comment calculer la variance de V(X+Y)
#Variance 

#2 loi uniforme sur [a, b]
Unif <-function(N,a,b){
  runif(N,a,b)
}

SommeUnif <- function(N,a,b) {
  X=Unif(N,a,b) 
  Y=Unif(N,a,b) 
  Z=X+Y
  return(Z)
}
N = 100
Z=SommeUnif(N,0,1)
n = seq(1,N,by=1)
plot(n, Z)##à verifie
abline(h=1/2*(a+b)+1/2*(a+b),col="green")
#Esperance
#E(Z)=E(X+Y)=E(X)+E(Y)
#1/mu+1/alpha =1
# comment calculer la variance de V(X+Y)
#Variance 

#3fonction SommeLUnif(N, L, a, b)
SommeLUnif <- function(N, L, a, b){
  Z=0
  for(i in 1:L){
    Z=Z+Unif(N,a,b)
  }
  return(Z)
}

N = 100
L= 50
Z=SommeLUnif(N,L,0,1)
n = seq(1,N,by=1)
plot(n, Z)
abline(h=1/2*(a+b)*(L),col="green")

#2 Simuler une loi uniforme sur un rectangle et sur un disque


#calculer la variance de deux variable aleatoire et comment l'ecrire en R 
#TP2 Questions 5 a verifier  et 6 
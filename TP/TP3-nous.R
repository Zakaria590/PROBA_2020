# TP 3
#Salah Zakaria OUAICHOUCHE
#Groupe3
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
plot(n, Z)
abline(h=1/2*(5+7)+8,col="green")
# Q4
#Esperance
#E(Z)=E(X+Y)=E(X)+E(Y)
#1/2(b+a)+mu =14
# comment calculer la variance de V(X+Y)
#Variance 
mean(Z)#Esperance 
sd(Z)#Variance


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
mean(Z)#Esperance 
sd(Z)#Variance

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
mean(Z)#Esperance 
sd(Z)#Variance

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
abline(h=1/2*(0+1)*(L),col="green")
mean(Z)#Esperance 
sd(Z)#Variance

#!!! comment écrire le main / voir dico
hist(Z, freq = FALSE,main="Somme de L variable aléatoires simulés N fois", xlab="Valeurs prises par les simulations", ylab="Fréquence absolue")
#!!! dessiner la moyenne
#2 Simuler une loi uniforme sur un rectangle et sur un disque
#Dans un rectangle
unif <- function(a,b,c,d){
  return(matrix(c(runif(1,a,b),runif(1,c,d)),1,2))
}
x=unif(1,2,3,4)
x
plot(x[,1],x[,2])
Nunif <- function(N,a,b,c,d){
  return(matrix(c(runif(N,a,b),runif(N,c,d)),N,2))
}
y=Nunif(100,1,2,3,4)
y
plot(y[,1],y[,2])



#Dans un disque
distance<-function(x,y){
  return(sqrt(x**2+y**2))
}
Ndisque<-function(N,r){
  cpt = 0
  matrice = matrix(0,N,2)
  while(cpt<N){
    point = unif(-r,r,-r,r)
    x = point[1,1]
    y= point[1,2]
    if(distance(x,y)<=r){
      matrice[cpt,1] = x
      matrice[cpt,2] = y
      cpt= cpt+1
    }
    
    
  }
  final = matrix(matrice,2,N,TRUE)
  return(matrice)
}
N = 10000
m = Ndisque(N,1)
plot(m[,1],m[,2],main="tirer N points aléatoirement dans un disque centré en (0, 0) de rayon 1", xlab="X Abscisse", ylab="Y ordonnée")
#comment faire un "append" sur une matrice???? pas possible
#2
d = distance(m[,1],m[,2])  
moy = mean(d)
hist(d, freq = FALSE)
curve(2*x, add= TRUE,col="green")
# la loi de  la variable d n'est pas umiforme 
# la loi optenue nous donnela fonction de densite est  2*x (la loi admet pour fonction de densité de proba la fonction defini par f(x)=2x, si x appartient à [0,r] 0 sinon
# la loi de la distance au centre n'est pas une loi usuelle
# la fonction de repartition P(x<=X)
# l'histogramme nous donne l'allure de la fonction de densité
# ici on une fonction linéaine entre [0,1] de la forme y = ax avec a = 2

#3 Méthode de Monte-Carlo
#Exercice1
distance2points = function(x1,x2,y1,y2){
  return(sqrt((x2-x1)^2+(y2-y1)^2))
}
N = 4
dis = c()
points = Nunif(N,0,1,0,1)
cpt = 0
for(i in 1:(N-1)){
  for(j in (i+1):N){
    cpt = cpt+1
    x1 = points[i,1]
    y1 = points[i,2]
    x2 = points[j,1]
    y2 = points[j,2]
    distance = sqrt((x2-x1)^2+(y2-y1)^2)
    dis = append(dis, distance)
  }
}
moy = sum(dis)/cpt 
#moy1 = mean(dis)


#Exercice2
Cnorm<-function(N){
  #N=10
  Nnorm=rnorm(N,0,1)
  f = exp(Nnorm)-1
  for (i in 1:N){
    f[i]=max(f[i],0)
  }
  C=mean(f)
  return(C)
}
Cnorm(10000)

#4 Des serpents et des échelles
#question1
destination=c(1:100)
destination[4]=14
destination[8]=31
destination[20]=38
destination[28]=84
destination[40]=59
destination[51]=67
destination[63]=81
destination[71]=91
destination[17]=7
destination[54]=34
destination[62]=19
destination[64]=60
destination[93]=73
destination[95]=75
destination[99]=78

#question2
#parce qu'on doit pas dépasser le 100 pour cloire la partie

#question3
jeu <- function (depart){
  cpt=0
  if (1<=depart && depart<=100){
    while(depart<100){
      de = sample(1:6)[1]
      #print(de)
      depart = depart + de
      depart = min(depart,100)
      depart = destination[depart]
      cpt = cpt +1
    }
    return(cpt)
  }
}
jeu(1)

#question4
TempsMoyen <- function(N,start){
  tm = c()
  for (i in 1:N) {
    tm = append(tm, jeu(start))
  }
  return(sum(tm)/N)
}
#question5
TempsMoyen(10000,1)

#question6
v = c()
for (i in 1:6){
  v = append(v,TempsMoyen(10000,1+i))
}
v
min(v)
match(min(v),v)

#exemple d'execution 
#[1] 31.3014 31.2653 31.4109 31.4419 31.2766 30.7124
#> min(v)
#[1] 30.7124
#> match(min(v),v)
#[1] 6
#donc on peut dire que  si Le joueur commence à la case 1,
#le meilleur coup de dé qu’il puisse faire c'est le 6.
#Oui c'etait previsible par ce que c'etait la valeur max qu'on peut avoir

#question7

v = c()
for (i in 1:6){
  depart = 18+i
  print(depart)
  v = append(v,TempsMoyen(10000,depart))
}
v
match(min(v),v)
#exemple d'execution
#[1] 19
#[1] 20
#[1] 21
#[1] 22
#[1] 23
#[1] 24
#> v
#[1] 27.2539 27.1220 26.9337 25.2721 25.9722 26.0319
#> match(min(v),v)
#[1] 4
#donc on peut dire que  si Le joueur est à la case 18,
#le meilleur coup de dé qu’il puisse faire c'est le 4.

#question8
#à revoir avec la prof

#question 9
start=1
tm = c()
for (i in 1:N) {
  tm = append(tm, jeu(start))
}
#
min(tm)
x = sum(tm==7)/10000
x
v=c(1,1,1,2,3,3,2)
y = sum(v==1)
y

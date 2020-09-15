#GROUPE : 6
#TP4

      
#Exercice 1 Le paradoxe des anniversaires:

#1)Simulation

proba_anniv=function(n,ite){
m=ite
x=0
for(i in 1:m){
e=sample(c(1:365),n,replace=T)
x=x+(length(unique(e))==n)
}
x=x/m
x=1-x
x
}

proba_anniv(10,1000)


simule_proba=function(n,ite){
m=ite
y=0
for (k in 2:n) {
y=union(y,proba_anniv(k,m))
}
y
}

y=simule_proba(100,1000);
y

#2)Calcul explicite

explicite_proba=function(n){
x=choose(365,n)*factorial(n)/(365^n)
x=1-x
x
}

z=explicite_proba(c(1:100));
z
plot(z,type="l",col="blue",main="Paradoxe des anniversaires",xlab="Nombre de personnes", ylab! ="Probabilites")
points(y,type="l",col="red")


#Exercice 2 : Biais de l'estimateur de la variance

mvariance_poisson=function(n,L){
x=rpois(n,L)
m=mean(x)
v=(x-m)^2
v=mean(v)
mv=c(moy=m,var=v)
mv
}

biais_poisson=function(ite,taille,L){
print(noquote(c("Loi de Poisson: moyenne = variance=",L)))
I=ite
n=taille
mv=mvariance_poisson(100,100)
m=mv[1]
v=mv[2]
for (k in 2:I){
mv=mvariance_poisson(100,100)
mv[1]=(k-1)*m[k-1]+mv[1]
mv[2]=(k-1)*v[k-1]+mv[2]
m=c(m,mv[1]/k)
v=c(v,mv[2]/k)
}
vc=v*n/(n-1)
m=m-L
v=v-L
vc=vc-L
biais=cbind(moy=m,var=v,vcor=vc)
return(biais)
}

graphes_biaispoisson=function(moyenne,variance,varcorrigee){
plot(variance,type="l",col="blue",main="Moyenne et Variance empiriques", xlab="taille echa! ntillon", ylab="biais")
points(varcorrigee,type="l",col="green")
points(moyenne,type="l",col="red")
}

b=biais_poisson(10000,100,100)
graphes_biaispoisson(b[,1],b[,2],b[,3])


#Exercice 3 :

somme=function(n,m1,s1,m2,s2){
x=rnorm(n,mean=m1,sd=s1)
y=rnorm(n,mean=m2,sd=s2)
z=x+y
v=s1^2+s2^2
hist(z,prob=T)
curve(exp(-(x-m1-m2)^2/(2*v))/sqrt(2*pi*v),col="blue",add=T)
}

somme(10000,2,1,5,3)


#Exercice 4 : Nombre poissonnien d'evenements

renouvelle_expo=function(ite,param,temps){
m=ite
L=param
T=temps
x=0
for (k in 1:m){
n=0
y=0
while (y<=T){
y=y+rexp(1,L)
n=n+1
}
x=c(x,n-1)
}
x=x[-1]
x
}

renouvelle_expo(1000,0.2,50)



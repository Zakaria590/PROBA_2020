#Groupe : 6
#TP2

#Partie 1 : Loi forte des grands nombres

#question1
x=runif(10000)
sum(x)/10000
y=rexp(10000,a)
sum(y)/10000
S=c(1:10000)
for (i in 1 :10000){S[i]=sum(x[1:i])/i}
plot(S)

#Question2
x=rcauchy(10000)
S=c(1:10000)
for (i in 1 :10000){S[i]=sum(x[1:i])/i}
plot(S)


#Partie 2 : Répartitions observée et répartition théorique

#question1
x=runif(10000,-2,3)

#question2
x=runif(10000,-2,3)
hist(x,freq=FALSE)
abline(h=0.2,col="green")

#question3
y=rnorm(10000,-1,2)
hist(y,freq=FALSE)
curve(exp((-(x+1)^2)/8)(2*sqrt(2*pi)),-10,8,add=TRUE,col="red")

#Partie 3 : Théorème de la limite centrale

#question1
t=rbinom(500000,10,0.8)
x=matrix(t,nrow=500, ncol=1000,byrow=TRUE)

#question2
for (i in 1:500){
  y[i]=sqrt(1000)*(sum(x[i, 1:10000])/1000-8)
}

#question3
hist(y,freq=FALSE)
curve(exp(-(x^2)/3.2)/(sqrt(3.2*pi)),-8,8,add= TRUE,col = "red")

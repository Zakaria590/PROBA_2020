#Salah Zakaria OUAICHOUCHE
#Groupe : 3
#TP2 Derniere partie 

#Partie 2 : Loi forte des grands nombres
#Illustration pour la loi uniforme sur [0,1].
M=3 
x = c()
for (i in 1:M){
  x = append(x, runif(4,0,1))
}

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

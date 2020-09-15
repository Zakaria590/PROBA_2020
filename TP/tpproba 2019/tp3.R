#GROUPE : 33
#TP3
#Exercice 1:

unif<-function(n,a,b,c,d){
x = runif(n,a,b)
y = runif(n,c,d)
z = cbind(x,y)
z
}

#Exemple de resultat 
#> unif(5,1,2,3,4)
#            x        y
#[1,] 1.086975 3.751149
#[2,] 1.086895 3.830767
#[3,] 1.114301 3.553464
#[4,] 1.611570 3.376443
#[5,] 1.979803 3.035645



#Exercice 2:

plus<-function(x){
for (i in 1:length(x)){
  if (x[i]<0){
   x[i]=0}
 }
return (x)
}

g = rnorm(100000)
f = exp(g)-1
res = mean(plus(f))
res

#Exemple de resultat 
#[1] 0.8801616
#[1] 0.8891379


#Exercice 3:

distance_moyenne_carre<-function(n){
x=0
for(i in 1:n){
x=x+norm(unif(1,0,1,0,1)-unif(1,0,1,0,1))
}
return(x/n)
}

#Exemple de resultat 
#[1] 0.4905592
#[1] 0.6369238

#Exercice 4:

unifdis=function(x){
	v=unif(x,-1,1,-1,1)
	for(i in 1:x){
		r = sqrt(sum(v[i,]^2))
		while(r>1){
			v[i,]=unif(1,-1,1,-1,1)
			r=sqrt(sum(v[i,]^2))
		}
	}
	v
}

n = 1000
v = unifdis(n)
plot(v[,1],v[,2],xlim=c(-1,1),ylim=c(-1,1))
symbols(0,0,circles=1,inches=F,add=T)

#Exercice 5:

simu = function(n){
	v = unif(n,0,1,0,1.5)
	for (i in 1:n){
		f = 6*v[i,1]*(1-v[i,1])
		while(setequal((v[i,2]>f),T)){
			v[i,]=unif(1,0,1,0,1.5)
			f = 6*v[i,]*(1-v[i,])
		}
	}
	v
}

plot(simu(n),col='blue')
curve(6*x*(1-x),-1,1,col='red',add=T)




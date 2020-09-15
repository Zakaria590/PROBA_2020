#Groupe : 6
#TP1
#partie 1
ls(pat="^m")
m=9

#partie 2
#q1
data(AirPassengers)

#q2
taille = c(168,175,172,183) #on stock les taille a l'aide de la fonction c()
poid = c(67,75,69,81)

taille = scan (text = "168 175 172 183") #on stock les taille a l'aide de la fonction scan ()
poid = scan (text = "67 75 69 81")

mydata= data.frame(taille,poid)

tp = matrix(c(168, 175, 172, 183, 67, 75, 69, 81), nrow = 2, ncol = 4, byrow = TRUE, dimnames = list(c("Taille","Poids"),c("C.1","C.2","C.3","C.4")))

#q3
x = c(1:20)

y = seq(from=1,to=10,by=0.5)

z = array(2,12)

t = gl(3,20,labels=c("A","B","C"))

#q4
nbr = sample(c(0,1),1)

bernoulli <- function(n){ sample(c(0,1),n,replace = TRUE)}
X = bernoulli(10)

#q5
M = matrix(c(1:30),10,3)
M[8:10,1:3]

#partie 3
#q1
X1 = rbinom(100,1,0.5)
X2 = c(X1[2:100])
X2[100]=X1[1]

#pour P00: U=(X1==0 & X2==0)
#          sum(u==TRUE)/100

#pour P01: U=(X1==0 & X2==1)
#          sum(u==TRUE)/100

#pour P10: U=(X1==1 & X2==0)
#          sum(u==TRUE)/100

#pour P11: U=(X1==1 & X2==1)
#          sum(u==TRUE)/100


#q2
X3 =c(X2[2:100])
C=(X1==0 a X2==0 & X3==0)
# sum(c==TRUE)/10


#q3
#pour n= 100


#pour n=1000
W1 = bernoulli(100)
W2 = c(W1[2:100],W1[1])
W2 = c(W1[2:100],W1[1])

#partie 4

#q1
s = sample(c("pile","face"),1000,replace = TRUE,prob=c(0.25,0.75))

#q2 : 1000*0.25 = 250

#q3
S=sample(c(0,1),1000,replace = TRUE,prob=c(0.25,0.75))
hist(S)

#q4
curve(exp(-x^2/2)/(sqrt(2*pi)),-10,10)

#q5 (representation de 4 courbe differente
par(mfrow =c(2,2))
curve(exp(-x^2/2)/(sqrt(2*pi)),-10,10)
curve(exp(-(x-4)^2/2)/(sqrt(2*pi)),-10,10)
curve(exp(-(x+4)^2/2)/(sqrt(2*pi)),-10,10)
curve(exp(-x^2/10)/(sqrt(2*pi)),-10,10)

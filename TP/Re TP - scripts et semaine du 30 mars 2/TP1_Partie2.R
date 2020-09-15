# TP 1 - Partie 2

# Données : les charger, les créer, les manipuler

# Question 1 

# Suite d'entiers de 1 à 20 dans x, un vecteur 
x = c(1:20)

# Suite de 1 à 10 par pas de 0.5
y = seq(from=1, to=10, by=0.5) # c'est un vecteur
y1 = seq(1, 10, 0.5) # pas besoin du nom des arguments 
y2 = seq(1,10,length.out=19) # plutôt que de spécifier le pas, on peut préciser la taille du vecteur de sortie

# Suite de 2 répété 12 fois
z = rep(2, times = 12) 
z1 = rep(2, 12)  # pas besoin de marquer le nom des arguments

# 2 autres manières de faire avec les méthodes simplifiées
z2 = rep.int(2, times=12)
z3 = rep_len(2, length.out=12)

# Suite de charactère répétée 
a = rep("A", 20)
b = rep("B", 20)
c = rep("C", 20)
w = c(a,b,c)

w1 = gl(3, 20, labels = c("A", "B", "C"))

w2 = gl(3, 20, 120, labels = c("A", "B", "C")) # 20 répétitions de chacune des lettres à la suite à deux reprises


# Question 2

data(morley)
morley # 5 séries de 20 observations de la mesure de la vitesse de la lumière en 1879 auxquelles ont été retirées 299000km/h
    # sous la forme d'une liste

help(morley)


# Question 3

# Ranger les données de AirPassengers dans une matrice 12*12

data(AirPassengers)
    # nb de passagers aériens par mois entre 1949 et 1960, en milliers
    # typeof(AirPassengers)=double, càd réels

P = matrix(AirPassengers, 12, 12) #données data, nombre de lignes nrows, nombre de colonnes ncol
    # les lignes représentent alors les mêmes mois de différentes années => problème, pas de classement par année

P2 = matrix(AirPassengers, 12, 12, TRUE) #byrow = TRUE, pour avoir la transposée et forcer le classement par ligne
print(P2) # chaque ligne représente une année et les mois sont rangès par colonne

    #Si on souhaite conserver les noms des lignes et colonnes dans la matrice
rownames(P2) = c("1949", "1950", "1951", "1952", "1953", "1954", "1955", "1956", "1957", "1958", "1959", "1960")
colnames(P2) = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
print(P2)

# Extraction des données des mois de Février à Juin, des années 58 et 59

    # Il s'agit des élèments situés sur les lignes 10 et 11 et les colonnes 2 à 6

extrait = P2[10:11,2:6] # les ":" sont parfaits pour extraire une suite d'élèments consécutifs

extrait2 = P2[c(10,11),c(2,6)] # Pour ne prendre que les mois de Février et Juin 58 et 59

# Les deux manières de faire font des extraction sous forme de matrices... on veut un tableau

t1=data.frame(extrait, row.names=c("1958", "1959")) # pour obtenir un tableau et plus une matrice, on peut nommer les lignes
names(t1)=c("Février", "Mars", "Avril", "Mai", "Juin") # pour nommer les lignes
t1

t2=data.frame(extrait2, row.names =c("1958", "1959"))
names(t2)=c("Février", "Juin")
t2


# Question 4

# Données à stocker :
  # Taille : 168 ; 175 ; 172 ; 183
  # Poids : 67 ; 75 ; 69 ; 81
  # IMC : masse/taille^2 (taille en m)

t = c(168,175,172,183)
p = c(67,75,69,81)
i = p/((t/100)^2)
m = matrix(c(t,p,i), 3, 4, TRUE)

# sous la forme d'un data frame
t=data.frame(m, row.names = c("taille", "poids", "IMC"))
names(t)=c("Jean", "Fatima", "Kevin", "Eduardo")
print(t)

# sous la forme d'une matrice
rownames(m) = c("taille", "poids", "IMC")
colnames(m) = c("Jean", "Fatima", "Kevin", "Eduardo")
print(m)
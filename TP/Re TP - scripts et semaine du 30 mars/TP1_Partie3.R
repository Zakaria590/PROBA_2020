# TP 1 - Partie 3

# Echantillon et histogramme

# Question 1

morley

# Préparer des vecteurs pour stocker les résultats
X = c() 
M = c()
S = c()
for (i in 1:5) {
  x_i = mean(morley[morley$Expt == i, "Speed"]) # moyenne de chaque série
  X = append(X, x_i) # accole au vecteur X des moyennes la moyenne suivante
  
  m_i = median(morley[morley$Expt == i, "Speed"]) # médiane de chaque série
  M = append(M, m_i)
  
  s_i = sd(morley[morley$Expt == i, "Speed"]) # écart-type de chaque série
  S = append(S, s_i)
}

# Afficher les résultats séparemment
print(X) # vecteur des moyennes de chaque série
print(M) # vecteur des médianes de chaque série
print(S) # vecteur des écarts-types de chaque série

# Afficher les résultats dans une matrice / un tableau par série
v = matrix(c(X,M,S), 5, 3) # matrice composée des 3 données pour chaque série

rownames(v) = c("Série 1", "Série 2", "Série 3", "Série 4", "Série 5")
colnames(v) = c("Moyenne", "Médiane", "Ecart-type")
print(v) # dans une matrice 

V = data.frame(v, row.names = c("Série 1", "Série 2", "Série 3", "Série 4", "Série 5"))
names(V)=c("Moyenne", "Médiane", "Ecart-type")
print(V) # dans un tableau

# Calculer les stats globales
moyG = mean(morley$Speed)
medG = median(morley$Speed)
sG = sd(morley$Speed)
# Les afficher
  # paste permet de mélanger les chaînes de caractères sachant qu'il a transformé les chiffres en caractères
print(paste("Moyenne globale : ", moyG, "\\ Mediane globale : ", medG, "\\ Ecart type : ", sG))
# Autrement
Globale=c(moyG,medG,sG)
G = data.frame(Globale, row.names = c("Moyenne", "Médiane", "Ecart-type"))
print(G)

# Permet d'obtenir toutes les stats basiques 
summary(morley$Speed)

#Histogramme global 
hist(morley$Speed, main="Histogramme global", xlab="Vitesse de la lumière mesurée", ylab="Nombre d'observations")
abline(v = c(moyG, medG), col=c("blue", "red"), lwd=5) # lwd=line width, la largeur de la ligne
legend("topright", legend=c("Moyenne", "Mediane"), col=c("blue", "red"), lty=1, lwd=5) # lty=line style, le type de ligne

# Faire les 5 histogrammes
for (i in 1:5) {
  data = morley[morley$Expt==i,"Speed"]
  title = paste("Histogramme de l'expérience n°",i)
  hist(data, main=title, xlab="Vitesse de la lumière mesurée", ylab="Nombre d'observations")
  abline(v = c(X[i], M[i]), col=c("blue", "red"), lwd=5)
  legend("topright", legend=c("Moyenne", "Mediane"), col=c("blue", "red"), lty=1, lwd=5)
}


# Question 2

AirPassengers

P = matrix(AirPassengers, 12, 12, TRUE) # byrow = TRUE, pour avoir la transposée et forcer le classement par ligne
  P # chaque ligne représente une année et les mois sont rangès par colonne

data = P[8:12,] # extraction des 60 données de 1956 à 1960
  rownames(data) = c("1956", "1957", "1958", "1959", "1960")
  colnames(data) = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
  print(data)

  #Pour le mode, il n'existe pas d'opérateur déjà créé, on peut donc créer la fonction nommée "mode"
  
mode <- function(vecteur) {
  uniqv <- unique(vecteur) # on extrait les valeurs prises dans le vecteur
  position = match(vecteur, uniqv) # on associe à chaque élément de vecteur sa position dans uniqv
  compte = tabulate(position) # on compte les fréquences de chaque position
  plusfrequent = which.max(compte) # on extrait la position qui apparait le plus : la position dans compte est la même que dans unique
  uniqv[plusfrequent] # on récupère le nombre qui est à cette position dans uniqv
}
  
print(paste("Moyenne de l'échantillon : ", mean(data)))
print(paste("Médiane de l'échantillon : ", median(data)))
print(paste("Mode de l'échantillon : ", mode(data)))
print(paste("Ecart-type de l'échantillon : ", sd(data)))

# Afficher plusieurs versions de l'histogramme
hist(data, main="Histogramme du nombre de passagers aériens entre 1956 et 1960 (en milliers)", xlab="Nb de passagers", ylab="Fréquence (nb de mois)")
  # K=1+ln(60)/ln(2)=6,9 -> donne 8 classes
hist(data, main="Histogramme du nombre de passagers aériens entre 1956 et 1960 (en milliers)", xlab="Nb de passagers (5 classes)", ylab="Fréquence (nb de mois)", breaks=5) # 5 classes
hist(data, main="Histogramme du nombre de passagers aériens entre 1956 et 1960 (en milliers)", xlab="Nb de passagers (19 classes)", ylab="Fréquence (nb de mois)", breaks=20) # 19 classes


# Question 3
N = 100
# On génère une série de N nombres régulièrement espacés entre 0 et 2*pi, 0 et 2*pi inclus, dans un vecteur
t = seq(0, 2*pi, length.out=N)
# On prend le sinus de chacun de ces nombres
x = sin(t) # c'est un vecteur
# On fait le graphique
plot(t,x, main="Fonction sinus sur l'intervalle [0;2pi] pour N points équirépartis", xlab="t", ylab="sin(t)")
# On fait l'histogramme
hist(x, breaks=20, main="Histogramme de la valeur du sinus sur l'intervalle [0;2pi]", xlab="Valeur du sinus", ylab="Fréquence, nombre d'observations")



# Des données aux graphiques

# Bornes du graphique 
xmin = -5
xmax = 5

  # Question 1

# Première fonction normale centrée réduite
f = function(x) {
  exp(-(x**2)/2)/(sqrt(2*pi))
}
# Tracer la première fonction
curve(f, xmin, xmax, main="Fonction de densité de la loi normale centrée réduite")

  # Question 2

# Deuxième fonction avec la moyenne et l'écart type
f2 = function(x, mu, sigma) {
  exp(-((x-mu)**2)/(2*sigma**2))/(sigma*sqrt(2*pi))
}
# Quelques essais : 
curve(f2(x, 2, 1), xmin, xmax, main="Fonction de densité de la loi normale de moyenne mu=2 et d'écart-type sigma=1")
curve(f2(x, 0, 3), xmin, xmax, main="Fonction de densité de la loi normale de moyenne mu=0 et d'écart-type sigma=3")

# Graphe global
curve(f2(x, 2, 0.8), xmin, xmax, main="Représentations de la fonction de densité de différentes lois normales (mu, sigma)", ylab = "f2(x, mu, sigma)", col="green", lwd=2)
curve(f2(x, 0, 1), xmin, xmax, col="red", lwd=2,  add=TRUE) 
curve(f2(x, 0, 2), xmin, xmax, col="blue", lwd=2, add=TRUE)
curve(f2(x, -3, 1), xmin, xmax, col="purple", lwd=2, add=TRUE)
legend("topright", legend=c("mu=2, sigma=0.8", "mu=0, sigma=1", "mu=0, sigma=2", "mu=-3, sigma=1"), col=c("green", "red", "blue", "purple"), lwd=2)


  # Question 3

# Bornes du graphique 
xmin = 0
xmax = 15

# La loi eponentielle, avec alpha>0
e = function(alpha, x) {
  alpha*exp(-alpha*x)
}
# Quelques essais
curve(e(1, x), xmin, xmax, main="Représentation de la fonction de densité de la loi exponentielle de paramètre alpha=1", ylab = "exp(-x)")
curve(add=TRUE,e(3, x), xmin, xmax, main="Représentation de la fonction de densité de la loi exponentielle de paramètre alpha=3", ylab = "3exp(-3x)")

# Graphe global
curve(e(1, x), xmin, xmax, main="Différentes représentations de la fonction de densité de lois exponentielles de paramètre alpha", ylab = "alpha.exp(-alpha*x)", col="green", lwd=2)
curve(e(2, x), xmin, xmax, col="red", lwd=2,  add=TRUE) 
curve(e(1/2, x), xmin, xmax, col="blue", lwd=2, add=TRUE)
curve(e(5, x), xmin, xmax, col="purple", lwd=2, add=TRUE)
legend("topright", legend=c("alpha=1", "alpha=2", "alpha=1/2", "alpha=5"), col=c("green", "red", "blue", "purple"), lwd=2)


# 2.3.1 Simulation de la loi de fi
rintensite <- function(n, params) {
  m <- params[1]
  x0 <- params[2]
  nu <- params[3]
  
  # Valeur initiale pour a (paramètre loi cauchy)
  a <- 1
  
  fi <- function(x) {
    return(1 / (m + abs(x - x0)^nu))
  }
  
  cauchy <- function(x, a) {
    return(10 / (9 * m * (1 + ((x - x0) / a)^2)))
  }
  
  # Détermination de N : borne supérieure (99.99 % des valeurs)
  N <- 1
  Ci <- integrate(fi, lower = 0, upper = Inf)
  
  while (abs(integrate(fi, lower = 0, upper = N)$value - Ci$value) > 0.001) {
    N <- N + 1
  }
  
  val <- N * runif(100)
  
  # Ajustement de a : on veut un a qui soit assez grand pour majorer fi à 99.999%
  for (i in 1:100) {
    while (fi(val[i]) > cauchy(val[i], a)) {
      a <- a + 0.5
    }
  }
  
  # Méthode rejet, majoration par loi Cauchy
  vecteur <- numeric(n)  # Création d'un vecteur numérique pour stocker les résultats
  I <- 0
  
  while (I < n) {
    # Générer un échantillon de la distribution cauchy
    abscisse <- rcauchy(1, location = x0, scale = a)
    
    # Répéter le processus jusqu'à obtenir un résultat positif
    while (abscisse <= 0) {
      abscisse <- rcauchy(1, location = x0, scale = a)
    }
    
    ordonnee <- runif(1, min = 0, max = cauchy(abscisse, a))
    
    if (ordonnee < fi(abscisse)) {
      I <- I + 1
      vecteur[I] <- abscisse  # Ajoutez la valeur à vecteur[I]
    }
  }
  
  return(vecteur)  # Renvoyez le vecteur de résultats
}
# 2.3.1 Simulation de la loi de fi
rintensite <- function(n, params) {
  m <- params[1]
  x0 <- params[2]
  nu <- params[3]
  
  # Valeur initiale pour sigma
  sigma <- 0.1
  
  fi <- function(x, sigma) {
    return((m / (sigma * sqrt(2 * pi))) / (m + abs(x - x0) ^ nu))
  }
  
  norm <- function(x, sigma) {
    return(((10 / (9 * sigma * sqrt(2 * pi))) * exp(-0.5 * ((x - x0) / sigma)^2)))
  }
  
  fi_ <- function(x) {
    return(1 / (m + abs(x - x0) ** nu))
  }
  
  Ci <- integrate(fi_, lower = 0, upper = Inf)
  
  # Détermination de N : borne supérieure  (99.99 % des valeurs)
  N <- 1
  while (abs(integrate(fi_, lower = 0, upper = N)$value - Ci$value) > 0.01) {
    N <- N + 1
  }
  
  val <- N * runif(100)
  
  # ajustement de sigma : on veut un sigma qui soit assez grand pour majorer fi à 99.9%
  for (i in 1:100) {
    while (fi(val[i], sigma) > norm(val[i], sigma)) {
      sigma <- sigma + 1
      Ci_fi <- integrate(fi, sigma = sigma, lower = 0, upper = Inf)$value
      Ci_norm <- integrate(norm, sigma = sigma, lower = 0, upper = Inf)$value
      
      if (Ci_norm / Ci_fi > 5) {
        break # On majore sigma pour éviter que le rejet dépasse 1/5 (complexité trop grande, trop de rejet)
      }
    }
  }
  
  # Méthode rejet, majoration par loi Normale
  vecteur <- numeric(n)  # Création d'un vecteur numérique pour stocker les résultats
  I <- 0
  
  while (I < n) {
    # Générer un échantillon de la distribution normale
    abscisse <- qnorm(runif(1), mean = x0, sd = sigma)
    
    # Répéter le processus jusqu'à obtenir un résultat positif
    while (abscisse <= 0) {
      abscisse <- x0 + sigma * rnorm(1)
    }
    
    ordonnee <- runif(1, min = 0, max = norm(abscisse, sigma))
    
    if (ordonnee < fi(abscisse, sigma)) {
      I <- I + 1
      vecteur[I] <- abscisse  # Ajoutez la valeur à vecteur[I]
    }
  }
  
  return(vecteur)  # Renvoyez le vecteur de résultats
}
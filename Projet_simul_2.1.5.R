petoile.petite <- function(n.simul, params) {
  m <- params[1]
  x0 <- params[2]
  nu <- params[3]
  l0 <- params[4]
  a <- params[5]
  v <- params[6]
  h0 <- params[7]
  
  # Intensité du processus de Poisson : on choisit une intensité 1,5 fois plus grande que l(t)
  # Le but de considérer une intensité plus grande est de réduire la variance par échantillonnage préférentiel
  # Regarder théorie de la ruine 27 minutes 30 pour comprendre la méthode
  # Intensité choisi pour appliquer Monté Carlo 
  
  # Intensité du processus de Poisson
  l <- function(t) { 
    return (l0 * (1 + a * sin(4 * pi * t)))
  }
  
  l_max <- function(t) {
    l <- l(t)
    return(1.5 * l)
  }
  
  # Définition de la fonction r
  r <- function(t) {
    ifelse(t < 0, 0, exp(-v * t))
  }
  
  # Définition de la fonction H
  H <- function(t, T, I) {
    S <- 0
    for (k in 1:length(T)) {
      S <- S + I[k] * r(t - T[k])
    }
    return(S)
  }
  
  # Application de la méthode Monte Carlo
  n_inondation <- 0
  for (i in 1:n.simul) {
    # Création d'un vecteur T, grosse simplification : on considère pas la variation de l entre T[k] et T[k+1]
    t <- 0
    T <- c()
    while (t < 1) {
      rate <- l_max(t)
      if (is.na(rate) || !is.finite(rate) || rate <= 0) {
        # cas où le taux (rate) est non défini, négatif où infini
        break
      }
      t <- t + rexp(1, rate)
      if (t < 1) {
        T <- c(T, t)
      }
    }
    
    # Création d'un vecteur I correspondant
    params2 <- c(m, x0, nu)
    I <- rintensite(length(T), params2)
    
    # Calcul du niveau le plus haut au cours de l'année (recherche de Hmax)
    h_plot <- function(t) {
      return(H(t, T, I))
    }
    # Gestion des cas lorsque T est vide, optimize ne prend pas les fonctions nulles (pas dans le code en tout cas)
    if (length(T) == 0) {
      Hmax <- 0
    } else {
      Hmax <- optimize(h_plot, interval = c(0, 1), maximum = TRUE)
      Hmax <- Hmax$objective
    }
    
    if (Hmax > h0) {
      n_inondation <- n_inondation + 1 * (1 / 1.5) ^ length(T)
    }
  }
  
  # Section de l'intervalle de confiance
  intervalle_confiance <- 0.95
  # Calcul de "u" pour un niveau de confiance de 95%, valeur = 1,96 d'après les tables
  u <- qnorm((intervalle_confiance + 1) / 2)
  
  # Calcul de la proportion d'inondations (espérance)
  proportion <- n_inondation / n.simul
  
  # Calcul de la proportion d'inondations selon l(t), sachant que l(T)=l0 et l_max(T)=10xl0
  proportion <- proportion * exp(1.5 * l0 - l0)
  
  # Calcul dispersion
  variance <- proportion * (1 - proportion) / (n.simul)
  
  # Calcul de la demi-largeur de l'intervalle de confiance
  demi_largeur <- u * sqrt(variance)
  
  return(list(p = proportion, demi.largeur = demi_largeur))
}
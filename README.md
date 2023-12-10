# Projet Simulation

## 1 Modèle

### 1.1 Contexte

On considère le fleuve Coulantine traversant la ville d’Aussec. On souhaite savoir quelle est la probabilité que ce fleuve déborde et inonde la ville au cours de l’année 2024, selon qu’on construise ou non des murets autour de son lit. On modélise les pluies par un ensemble de dates où elles tombent (on considère ces pluies instantanées), et des intensités pour chacune de ces dates. À partir de ces données, aléatoires, un modèle nous permet de calculer la hauteur d’eau dans le fleuve à tout moment de l’année, et donc tester si la hauteur maximale dépasse un certain seuil correspondant au début d’une inondation.

### 1.2 Dates des pluies

On modélise les dates des pluies par un processus de Poisson inhomogène, de fonction intensité λ définie par

$\[ \lambda(t) = \lambda_0(1 + \alpha \sin(4\pi t)) \]$

où λ₀ et α sont des constantes du modèle, vérifiant λ₀ > 0 et 0 ≤ α < 1. Dans cette expression, t représente le temps en années, avec t = 0 au début de l’année 2024 et t = 1 à la fin de l’année 2024. On note T₁, T₂, ... les variables aléatoires correspondant aux dates des pluies.

### 1.3 Intensité des pluies

On modélise les intensités de chacune des pluies par des variables aléatoires indépendantes, et indépendantes des dates des pluies, toutes de loi Pi de densité fᵢ définie par :

$\[ f_i(x) = \frac{c_i}{(1 + (x - x₀)^2)^{\frac{η}{2}}} \]$

où cᵢ est une constante de normalisation, et m > 0, x₀ > 0 et η > 2 sont des paramètres du modèle. On note I₁, I₂, ... les variables aléatoires correspondant aux intensités des pluies.

### 1.4 Hauteur d’eau

La hauteur d’eau H(t) dans le fleuve Coulantine au temps t est définie comme suit :

$\[ H(t) = \int_0^t f(t - s)vH(s)ds \]$

où r est la fonction de résorption de l’eau de ce fleuve, en notant v la vitesse de résorption. On notera Hmax la hauteur maximale de l’eau dans le fleuve Coulantine au cours de l’année 2024.

## 2 Résultat attendu

On cherche à :

- Approximer, par une méthode de Monte Carlo, la probabilité que Hmax dépasse le seuil h₀ (donné) correspondant à l’inondation de la ville d’Aussec : $\[ \text{Prob}(H_{\text{max}} > h₀) \]$
  
- Étudier l’influence des paramètres λ₀, α, x₀, v et h₀ sur la probabilité p∗(h₀). Le sens de cette influence était-il attendu?

## 3 Explication des fonctions 

- Projet_simul_2.3.R : Simule des intensité de pluies aléatoires, application de méthode de rejet et de majoration par une loi normale
- Projet_simul_2.4.R : Application de la méthode de Monté carlo pour estimer une probabilité d'inondation
- Projet_simul_2.5.R : Amélioration de la méthode de monté Carlo, optimisation de la convergence vers proba grâce à une méthode d'échantillonage préférentiel (pour les probas proches de 0)
- ShinyDashboard_WeatherFloodSimulation2.R : Création d'une application RShiny Dashboard pour la présentation des résultats et l'étude de l'influence des paramètres.
  

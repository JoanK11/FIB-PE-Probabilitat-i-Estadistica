# Modelos estadísticos
# Tenemos las variables T, S, R y Q que siguen la distribución t-Student con los siguientes grados de libertad:
  T = 24; S = 23; R = 13; Q = 56
# Además, las variables X, Y y U siguen la distribución Chi-Cuadrado (χ2) con los siguientes grados de libertad:
  X = 23; Y = 8; U = 53

# 1. Prob(T > 1.711)
pregunta1 <- 1 - pt(1.711, T)
pregunta1 # Coger solo 3 decimales

# 2. El argumento marcado con '?' tal que Prob(S < ?) = 0.75
pregunta2 <- qt(0.75, S)
pregunta2 # Coger solo 3 decimales

# 3. P(|R| < 3.852) = ?
pregunta3 <- pt(3.852, R) - pt(-3.852, R)
pregunta3 # Coger solo 3 decimales

# 4. P(|Q| > 2.05495) = ?
pregunta4 <- 1 - pt(2.05495, Q) + pt(-2.05495, Q)
pregunta4

# 5. Prob(X < 35.172)
pregunta5 <- pchisq(35.172, X)
pregunta5 # Coger solo 3 decimales

# 6. El argumento marcado con '?' tal que Prob(Y > ?) = 0.995
pregunta6 <- qchisq(0.005, Y)
pregunta6 # Coger solo 3 decimales

# 7. ¿Cuál es la probabilidad de que la variable U presente un valor entre 62.48 y 64.27?
pregunta7 <- pchisq(64.27, U) - pchisq(62.48, U)
pregunta7
# El exprimidor
n = nrow(data);

Z1 <- c(); Z2 <- c();
Y1 <- c(); Y2 <- c();

for (i in 1:n) {
  if (data$G[i] == 1) {
    Z1 <- c(Z1, data$Z[i]);
    Y1 <- c(Y1, data$Y[i]);
  } else {
    Z2 <- c(Z2, data$Z[i]);
    Y2 <- c(Y2, data$Y[i]);
  }
}
nZ1 = length(Z1); nZ2 = length(Z2);
nY1 = length(Y1); nY2 = length(Y2);

# Pregunta 1: Consideremos los datos basados en muestras independientes. Admitiendo que los dos sistemas comparten una variabilidad común en la producción de zumo, dé una estimación de la variancia común.
pregunta1 <- ((nZ1 - 1)*sd(Z1)^2 + (nZ2 - 1)*sd(Z2)^2)/(n - 2); pregunta1


# Pregunta 2: Resuelva la prueba de hipótesis para comparar los promedios del peso obtenido de zumo, y diga cuál es el resultado obtenido para el valor P/el estadístico de la prueba.
S = sqrt(pregunta1)
T = (mean(Z1) - mean(Z2))/(S*sqrt(1/nZ1 + 1/nZ2))

# Si piden para el estadístico de la prueba
pregunta2 = T; pregunta2

# Si piden para el valor P
pregunta2 <- 2*(1 - pt(abs(T), n - 2)); pregunta2


# Pregunta 3: Planteamos una prueba de variancias:
  # H0: σ12 = σ22
  # H1: σ12 ≠ σ22
# Calcule el valor del estadístico de esta prueba (procurando que caiga en la zona de la derecha), y el límite establecido con un riesgo {alpha = 5%} que nos permitiría rechazar la hipótesis de partida.
alpha = 0.05
varZ1 = var(Z1); varZ2 = var(Z2);
SM = max(varZ1, varZ2); sm = min(varZ1, varZ2)
F = SM/sm;
pc = qf(1 - alpha/2, nZ1 - 1, nZ2 - 1);
pregunta3 <- print(paste(F, pc))


# Pregunta 4: Consideramos ahora la segunda parte. En este caso, tendremos en cuenta que los valores esperados que se comparan corresponden al zumo de media naranja. Calcule cuánto vale ahora el valor P/el estadístico de la prueba.
D = Y1 - Y2; nD = length(D)
T = mean(D)/(sd(D)/sqrt(nD))

# Si piden para el estadístico de la prueba
pregunta4 = T; pregunta4

# Si piden para el valor P
pregunta4 <- 2*(1 - pt(abs(T), nD - 1)); pregunta4


# Pregunta 5: Estime por intervalo de confianza del {prob = 95%} la diferencia de zumo promedio que nos puede dar una naranja exprimida con uno u otro sistema, de acuerdo con el procedimiento basado en muestras independientes/apareadas.
# El signo de los valores que introduzca se deriva de la diferencia sistema 1 - sistema 2.
prob = 0.95; alpha = 1 - prob;

# Para muestras independientes
IC1 <- (mean(Z1) - mean(Z2)) - qt(1 - alpha/2, n - 2)*S*sqrt(1/nZ1 + 1/nZ2);
IC2 <- (mean(Z1) - mean(Z2)) + qt(1 - alpha/2, n - 2)*S*sqrt(1/nZ1 + 1/nZ2);
pregunta5 <- print(paste(IC1, IC2))

# Para muestras apareadas
IC1 <- mean(D) - qt(1 - alpha/2, nD - 1)*sd(D)*sqrt(1/nD);
IC2 <- mean(D) + qt(1 - alpha/2, nD - 1)*sd(D)*sqrt(1/nD);
pregunta5 <- print(paste(IC1*2, IC2*2))


# Pregunta 6: A partir del análisis según muestras independientes, elige la respuesta más acertada:
  # 1. El exprimidor que gira solo en un sentido es mejor
  # 2. Creemos que los dos tipos de exprimidor son equivalentes
  # 3. No lo sabemos, no hemos hallado nada concluyente
  # 4. El exprimidor que gira en ambos sentidos es mejor

if (IC1 > 0 && IC2 > 0) {
  pregunta6 <- print("El exprimidor que gira solo en un sentido es mejor");
} else if (IC1 < 0 && IC2 < 0) {
  pregunta6 <- print("El exprimidor que gira en ambos sentidos es mejor");
} else {
  pregunta6 <- print("No lo sabemos, no hemos hallado nada concluyente");
}
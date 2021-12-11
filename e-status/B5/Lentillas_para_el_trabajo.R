# Lentillas para el trabajo
n = nrow(data)
data <- data[order(data$Nvo, data$Id),] # Molt important executar aquesta instrucció, per tenir les taules ordenades

# Pregunta 1: Calcula la media de la humedad medida en las lentillas de tipo Ref/New.
HumedadRef <- c()
HumedadNew <- c()

for (i in 1:n) {
  if (data$Nvo[i] == 0) {
    HumedadRef <- c(HumedadRef, data$Hume[i])
  }
  else {
    HumedadNew <- c(HumedadNew, data$Hume[i])
  }
}
nN = length(HumedadNew); nR = length(HumedadRef)
# Media Ref
pregunta1 <- mean(HumedadRef); pregunta1
# Media New
pregunta1 <- mean(HumedadNew); pregunta1

# Pregunta 2: Halla las diferencias (New - Ref) en la humedad medida entre los dos tipos de lente, y calcula el promedio de estas diferencias.
D <- HumedadNew - HumedadRef; nD = length(D)
pregunta2 <- mean(D); pregunta2

# Pregunta 3: Calcula la desviación típica de las diferencias
pregunta3 <- sd(D); pregunta3

# Pregunta 4: Calcula la desviación típica de la media de las diferencias (el error típico)
sD = pregunta3
pregunta4 <- sD/sqrt(nD); pregunta4

# Pregunta 5: Calcula el valor del estadístico para contrastar la hipotesis nula:
# "no hay diferencia en humedad entre lentillas de tipo New y de tipo Ref".
pregunta5 <- pregunta2/pregunta4; pregunta5

# Pregunta 6: ¿Cuántos grados de libertad hay que utilizar para la distribución de referencia del estadístico calculado?
pregunta6 = nD - 1; pregunta6

# Pregunta 7: Calcula el p-valor de la prueba de hipótesis anterior.
T = pregunta5; grados = pregunta6
if (T >= 0) {
  pregunta7 <- 2*(1 - pt(T, grados))
} else {
  pregunta7 <- 2*pt(T, grados)
}
pregunta7

# Pregunta 8: Con un riesgo del 5%, ¿aceptarías o rechazarías la hipótesis nula? ¿Y con un riesgo del 1%?
  # 1. aceptar con 1 y 5;
  # 2. aceptar con 5 y rechazar con 1;
  # 3. rechazar con 5 y aceptar con 1;
  # 4. rechazar con 1 y 5.
pvalor = pregunta7
if (pvalor >= 0.05) {
  a5 = "Aceptar con 5"
} else a5 = "Rechazar con 5"

if (pvalor >= 0.01) {
  a1 = "Aceptar con 1"
} else a1 = "Rechazar con 1"

pregunta8 <- print(paste(a5, a1))

# Pregunta 9: Calcula el intervalo de confianza del 98 por ciento para el efecto que consigue el nuevo tipo de lentilla (incremento de humedad)
prob = 0.98; alpha = 1 - prob
IC1 = mean(D) - qt(1 - alpha/2, grados)*sD/sqrt(nD);
IC2 = mean(D) + qt(1 - alpha/2, grados)*sD/sqrt(nD);
pregunta9 <- print(paste(IC1, IC2))

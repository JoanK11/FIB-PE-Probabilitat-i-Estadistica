# Competencia gasolinera
G <- c(1.465, 1.472, 1.496, 1.452, 1.488, 1.48, 1.508)
H <- c(1.453, 1.472, 1.496, 1.518, 1.534)

# Pregunta 1: ¿Cuál es la desviación típica para la marca G?
pregunta1 <- sd(G)
pregunta1

# Pregunta 2: ¿Cuál es la desviación típica para la marca H?
pregunta2 <- sd(H)
pregunta2

# Pregunta 3: Introduzca el valor del estadístico de referencia en comparación de variancias (el cociente entre la variancia mayor y la variancia menor).
SA = max(sd(G), sd(H))
SB = min(sd(G), sd(H))
pregunta3 <- SA^2/SB^2
pregunta3

# Pregunta 4: Bajo la hipótesis nula de que ambas variancias poblacionales son iguales, el estadístico anterior sigue una ley F de Fisher. Introduzca los parámetros de la ley que corresponde a este caso (primero, grados de libertad del numerador; despues, grados de libertad del denominador)
if (SA == sd(H)) {
  nA = length(H); nB = length(G);
} else {
  nA = length(G); nB = length(H);
}
pregunta4 <- print(paste(nA - 1, nB - 1));

# Pregunta 5: ¿Cuál es el valor que utilizará, con un riesgo α=5% bilateral, para decidir si el estadístico anterior permite rechazar la hipótesis nula?
alpha = 0.05
pregunta5 <- qf(1 - alpha/2, nA - 1, nB - 1)
pregunta5

# Pregunta 6: Aunque no lo necesita, diga cuál sería el valor crítico que limita el estadístico por la parte inferior.
pregunta6 <- qf(alpha/2, nA - 1, nB - 1)
pregunta6

# Pregunta 7: Para hallar el p-valor de la prueba necesitará el ordenador. Tenga en cuenta que debe calcular la probabilidad de superar el valor del estadístico que ha encontrado, y multiplicar por 2, ya que se trata de una prueba bilateral. Obtenga tres decimales correctos, al menos.
# A pesar de que su estadístico es mayor que 1, puede pasar que la cola de la derecha posea una probabilidad superior a 0.5. En ese caso, duplique la probabilidad de la cola por la izquierda.
F = pregunta3
pregunta7 <- 2*(1 - pf(F, nA - 1, nB - 1))
if (pregunta7 >= 1) {
  pregunta7 <- 2*pf(F, nA - 1, nB - 1)
}
pregunta7

# Pregunta 8: Una de estas tres frases es la conclusión correcta. ¿Cuál es?
  # 1. tenemos pruebas de que las dos variancias pueden ser idénticas.
  # 2. hemos conseguido demostrar que el coste de G y H no es el mismo.
  # 3. no hemos hallado evidencias de que las dos marcas difieran en dispersión del precio.

# Si pregunta7 < alpha <--> F > pregunta5 --> Rechazamos --> 2
# Si pregunta7 > alpha <--> F < pregunta5 --> No podemos rechazar --> 3
if (F > pregunta5) {
  print(2)
} else {
  print(3)
}

# Pregunta 9: Vamos a ver si tenemos claro lo de la distribución F de Fisher-Snedecor. Pruebe a calcular aquel valor x tal que una variable que sigue la distribución F con grados de libertad {11, 17} tenga probabilidad 0.828 de ser menor que x.
gA = 11; gB = 17; p = 0.828
pregunta9 <- qf(p, gA, gB)
pregunta9
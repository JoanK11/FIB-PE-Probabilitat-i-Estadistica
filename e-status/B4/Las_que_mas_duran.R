# Las que más duran
# Un fabricante nos ofrece un nuevo tipo de baterías para el aparato de medición que utilizamos habitualmente, el cual usa otro modelo que dura en media 80 horas. En principio aquellas tienen una duración superior,  y queremos comprobar esta afirmación. La prueba de hipótesis que se plantea es:
  # H: μ = 80

# Con esta formulación, suponemos que la media μ de la población de las nuevas baterías es igual a la de las anteriores, es decir, adoptamos una postura conservadora (honesta) que sólo nos permitirá concluir que las nuevas baterias son "mejores" cuando los datos de la muestra nos haga la hipótesis H poco creíble. Es decir, cuando la evidencia (información) empírica, contenida en las observaciones de la muestra, nos indique que una población con una duración media superior a la de las baterías que se fabricaban antes es un origen más probable. 
# La muestra recogida es la duración en horas (con precisión de media hora) de 20 dispositivos que han funcionado hasta dar señales de que la batería se ha descargado.
esperanca = 80;
nterm <- c(75, 93.5, 93.5, 89.5, 83, 78, 73.5, 87, 65.5, 90, 104.5, 84.5, 89.5, 95, 92, 97.5, 73.5, 71, 85.5, 100.5)
n = length(nterm)

# 1. Supongamos conocida la varianza de la población, igual a {variancia= 199}. Calcular el valor P para la prueba de hipótesis del enunciado.
variancia = 199;
Z = (mean(nterm)-esperanca)/sqrt(variancia/n)
pregunta1 = 1 - pnorm(Z) # P(Z > z) = 1 - P(Z < z)
pregunta1

# 2. Según el resultado obtenido, elige la respuesta apropiada:
  # 1. sería interesante utilizar las nuevas baterías
  # 2. sería sensato no cambiar
pc = qnorm(0.95)
if (Z > pc) {
  print(1)      # Rebutjar si Z > pc
} else print(2) # No rebutjar si Z < pc

# 3. Supongamos que no tenemos datos sobre la desviación estándar de la población. Calcule la desviación estándar de la muestra.
pregunta3 <- sd(nterm)
pregunta3

# 4. Calcule el estadístico t según la desviación estándar de la muestra.
pregunta4 <- (mean(nterm)-esperanca)*sqrt(n)/sd(nterm)
pregunta4

# 5. Con riesgo α=0.02, ¿dónde se ubica el punto crítico para rechazar la hipótesis H?
alpha = 0.02
pregunta5 <- qt(1-alpha, n-1)
pregunta5

# 6. Halle el P valor asociado a la prueba, con cuatro decimales de precisión.
Z = pregunta4
pregunta6 <- 1 - pt(Z, n-1) # P = P(T > t)
pregunta6

# 7. ¿Qué interpretación piensa que es adecuada?
  # 1. con riesgo 5%, podemos afirmar que las nuevas baterías duran más en media
  # 2. no es correcto hacer el IC sin conocer sigma
  # 3. la muestra es escasa, o los nuevos modelos se comportan como los anteriores
# Si pregunta6 está más cerca de 0 que de 1 -> El rendimiento ha mejorado -> Respuesta 1
# Si pregunta6 está más cerca de 1 que de 0 -> La muestra es escasa -> Respuesta 3

# 8. Si se quiere estimar la duración media con un intervalo de confianza al {IC = 99.9%}, ¿qué obtenemos con esta muestra?
IC = 0.999; alpha = 1 - IC
x1 = mean(nterm) - qt(1-alpha/2, n-1)*sd(nterm)/sqrt(n);
x2 = mean(nterm) + qt(1-alpha/2, n-1)*sd(nterm)/sqrt(n);
print(paste(x1, x2))

# 9. Suponiendo que se pretende obtener un IC al {IC = 95%} de confianza para la verdadera media de la duración, con una amplitud del IC no mayor que {amplitud = 282} minutos,
# ¿cuántas observaciones se necesitan, asumiendo una desviación tipo poblacional igual a {sd = 1411} minutos?
IC = 0.95; alpha = 1 - IC; amplitud = 282/60; sd = 1411/60
pregunta9 <- ceiling((2*qnorm(1-alpha/2)*sd/amplitud)^2)
pregunta9

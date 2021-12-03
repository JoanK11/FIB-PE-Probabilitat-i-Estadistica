# Papilla de zombi
# El mundo ha caído bajo el contagio de un virus que ha transformado a toda la población en zombis. Tu último refugio es nada más que tu coche, con el que deberás atropellar a cada uno de los muertos vivientes.
# Tú (digamos A) y tu amigo (B) os habéis pasado la noche pisando zombis, y ha llegado la hora de determinar quién ha jugado mejor, basándose en el número promedio de pisoteos por partida. El registro del juego para cada partida (cada vez juega solo uno) nos informa sobre el número de zombis aplastados hasta que la acumulación de carne pútrida detiene el coche y es reglamentariamente devorado el cerebro del conductor (game over).

A <- c(82, 75, 77, 60, 103, 92, 45, 97, 83, 72, 73, 63, 76, 95, 70, 82, 91, 108)
B <- c(74, 106, 88, 83, 65, 60, 62, 93, 95, 83, 86, 81, 54, 97, 80)
na = length(A)
nb = length(B)

# Pregunta 1: Vamos a comparar los resultados. Primero, di cuál es la diferencia promedio entre los zombies que has atropellado tú y los que ha atropellado B por partida.
pregunta1 <- mean(A) - mean(B)
pregunta1

# Pregunta 2: Calcula qué desviación estándar tiene los resultados del jugador B.
pregunta2 <- sd(B)
pregunta2

# Pregunta 3: Considerando que las características de dispersión (a lo largo de infinitas partidas) fueran iguales en los dos jugadores, calcula una estimación de la variancia que tenéis en común.
pregunta3 <- ((na-1)*sd(A)^2 + (nb-1)*sd(B)^2)/(na + nb - 2)
pregunta3

# Pregunta 4: Sabemos que, de noche a noche, la diferencia de promedios de A y B fluctúa, con una desviación tipo que llamamos error tipo o standard error. ¿Puedes estimar este valor con los datos disponibles?
s_pooled = sqrt(pregunta3)
pregunta4 <- s_pooled*sqrt(1/n1 + 1/n2) 
pregunta4

# Pregunta 5: La información que conocemos de la muestra de esta noche da lugar a un estadístico, utilizado para poner a prueba la hipótesis H: μA = μB, que tomaremos bilateral porque a priori ninguno de los dos tiene ventaja. ¿Cuánto vale este estadístico?
se = pregunta4
pregunta5 <- (mean(A) - mean(B))/se
pregunta5

# Pregunta 6: Halla el valor P asociado a la prueba de hipótesis que has realizado.
Z = pregunta5
pregunta6 <- 2*(1 - pt(-Z, n1 + n2 - 2))
pregunta6

# Pregunta 7: La pregunta definitiva es qué diferencia hay entre los promedios de los dos jugadores. Estima dicha diferencia con un intervalo de confianza 99%.
alpha = 1 - 0.99
IC1 <- (mean(A) - mean(B)) - qt(1-alpha/2, n1+n2-2)*sqrt(s_pooled^2/na + s_pooled^2/nb)
IC2 <- (mean(A) - mean(B)) + qt(1-alpha/2, n1+n2-2)*sqrt(s_pooled^2/na + s_pooled^2/nb)
pregunta7 <- print(paste(IC1, IC2))

# Pregunta 8: ¿Cuál sería la conclusión de la prueba?
  # 1. La evidencia es que A lo hace mejor
  # 2. La evidencia es que B lo hace mejor
  # 3. No se puede descartar que A y B tengan el mismo nivel
  # 4. Parece que los dos sois igual de buenos

# pregunta5 < qt(0.975, n1+n2-2) <--> pregunta6 > 0.05 --> Respuesta 3

# El reparador
# El tiempo estipulado para reparar una avería de un tipo determinado en el taller de reparación de ordenadores es de {esperanca = 1.5} horas, aunque en realidad este tiempo es variable, siguiendo una distribución Normal, con esta media, pero con desviación tipo igual a {desviacion = 28} minutos.

#En las siguientes preguntas vamos a explorar temas relacionados con la distribución del tiempo para una reparación al azar, para varias reparaciones independientes, y para el tiempo promedio de un conjunto de reparaciones.
esperanca = 1.5*60;
desviacion = 28;

# Pregunta 1: Halla la probabilidad de que una avería necesite más de {t = 89} minutos de tiempo de reparación.
t = 89;
x = (t - esperanca)/desviacion;
pregunta1 <- 1 - pnorm(x);
# Si nos piden menos de 89 minutos no hay que hacer la resta: pregunta1 <- pnorm(x)
pregunta1;

# Pregunta 2: ¿Cuántos minutos podemos asegurar, con un {p = 99.5%} de probabilidad, que una avería no necesitará más de ese tiempo para su reparación? Responde con dos decimales correctos.
p = 0.995;
pregunta2 <- qnorm(p, esperanca, desviacion);
pregunta2;

# Pregunta 3: ¿Qué parámetros tiene la distribución de probabilidad del tiempo total empleado en reparar {n = 38} averías? Utilizar horas como unidades.
n = 38;
esperanca2 = esperanca*n/60;
desviacion2 = desviacion*sqrt(n)/60;
pregunta3 <- print(paste(esperanca2, desviacion2));

# Pregunta 4: Consideremos un grupo de {n = 13} averías sin relación entre sí. Calcular la probabilidad de que el tiempo promedio de reparación para un grupo de este tamaño sea mayor que {t = 103} minutos.
n = 13; t = 103;
desviacion3 = desviacion/sqrt(n);
pregunta4 <- 1 - pnorm(t, esperanca, desviacion3);
pregunta4;
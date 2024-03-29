# El reparador
# El tiempo estipulado para reparar una aver�a de un tipo determinado en el taller de reparaci�n de ordenadores es de {esperanca = 1.5} horas, aunque en realidad este tiempo es variable, siguiendo una distribuci�n Normal, con esta media, pero con desviaci�n tipo igual a {desviacion = 28} minutos.

#En las siguientes preguntas vamos a explorar temas relacionados con la distribuci�n del tiempo para una reparaci�n al azar, para varias reparaciones independientes, y para el tiempo promedio de un conjunto de reparaciones.
esperanca = 1.5*60;
desviacion = 28;

# Pregunta 1: Halla la probabilidad de que una aver�a necesite m�s de {t = 89} minutos de tiempo de reparaci�n.
t = 89;
x = (t - esperanca)/desviacion;
pregunta1 <- 1 - pnorm(x);
# Si nos piden menos de 89 minutos no hay que hacer la resta: pregunta1 <- pnorm(x)
pregunta1;

# Pregunta 2: �Cu�ntos minutos podemos asegurar, con un {p = 99.5%} de probabilidad, que una aver�a no necesitar� m�s de ese tiempo para su reparaci�n? Responde con dos decimales correctos.
p = 0.995;
pregunta2 <- qnorm(p, esperanca, desviacion);
pregunta2;

# Pregunta 3: �Qu� par�metros tiene la distribuci�n de probabilidad del tiempo total empleado en reparar {n = 38} aver�as? Utilizar horas como unidades.
n = 38;
esperanca2 = esperanca*n/60;
desviacion2 = desviacion*sqrt(n)/60;
pregunta3 <- print(paste(esperanca2, desviacion2));

# Pregunta 4: Consideremos un grupo de {n = 13} aver�as sin relaci�n entre s�. Calcular la probabilidad de que el tiempo promedio de reparaci�n para un grupo de este tama�o sea mayor que {t = 103} minutos.
n = 13; t = 103;
desviacion3 = desviacion/sqrt(n);
pregunta4 <- 1 - pnorm(t, esperanca, desviacion3);
pregunta4;
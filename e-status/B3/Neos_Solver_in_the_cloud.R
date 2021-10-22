# Neos Solver in the cloud

# Pregunta 1
# Se admite en nuestro caso que la variable de estudio se distribuye según una ley Poisson con promedio = 4.
# Calcule la probabilidad de que en cierta hora el número de problemas recibidos sea menor que 5.
ppois(q  = 4, lambda = 4);

# Pregunta 2
# Calcule la probabilidad de observar exactamente 3 llegadas en una hora.
dpois(x = 3, lambda = 4);

# Pregunta 3
# Averigüe ahora cuál es la probabilidad de que en un intervalo de 2 horas lleguen 6 problemas a Neos Solver.
dpois(x = 6, lambda = 8);

# Pregunta 4
# Para este sistema con un solo servidor, ¿cuál es el valor esperado de la variable tiempo (en minutos) entre dos llegadas sucesivas? Introducir al menos dos decimales correctos.
# Respuesta: Si el promedio/esperanza de problemas recibidos por hora es 4, eso significa que recibiremos 1 problema cada 60/4 = 15 minutos.
60/4

# Pregunta 5
# ¿Cuál es la probabilidad de estar menos de 16 minutos sin recibir un problema?
1 - dpois(q = 0, lambda = 4/60*16);

# Pregunta 6
# A continuación suponga que existen 10 servidores preparados para resolver los problemas que llegan al site. Todos ellos se caracterizan por una probabilidad 0.07 de recibir 1 problema en una hora.
# Preguntamos por la probabilidad de tener en cierta hora más de 3 servidores recibiendo exactamente 1 problemas.
# Atención, ciertas probabilidades pueden estar muy cerca de 0 o de 1. Introduzca al menos 4 decimales correctos para obtener una mínima precisión.
1 - pbinom(q = 3, size = 10, prob = 0.07);

# Aeropuerto
# En un aeropuerto, la llegada de pasajeros se produce de acuerdo a un proceso aleatorio, ya que no se puede saber cuándo se va a producir la próxima llegada.

# Los pasajeros obtienen su tarjeta de embarque en los puntos de facturación. La probabilidad de que un pasajero encuentre cola en facturación es {x = 0.384}. Por otro lado, la probabilidad de que un pasajero no tenga que facturar (porque sólo lleva equipaje de mano) es {y = 0.504}; además, se sabe que el {z = 11.2%} de los pasajeros que no llevan equipaje para facturar encuentran cola cuando van a recoger su tarjeta de embarque.

# Definiremos las variables discretas:
  # X = "0 si no encuentra cola en facturación; 1 si encuentra",
  # Y = "0 si sólo lleva equipaje de mano; 1 si lleva equipaje para facturar".
# Responda a las siguientes preguntas. Los valores han de ser correctos hasta el decimal que se indique (redondee hacia arriba o hacia abajo si es preciso).
x = 0.384;
y = 0.504;
z = 0.112;

# Pregunta 1: ¿Cuál es la función de probabilidad conjunta de X e Y?
#       Y=0     Y=1
# X=0    a       b
# X=1    c       d
# Los valores a b c d, han de ser escritos en este orden y separados por espacios (3 decimales correctos).
c = y*z;
d = x - c;
a = y - c;
b = 1 - a - c - d;
a; b; c; d;

# Pregunta 2: ¿Cuánto vale la covariancia entre ambas variables?
esperancaX = c + d;
esperancaY = b + d;
cov = (0-esperancaX)*(0-esperancaY)*a + (0-esperancaX)*(1-esperancaY)*b + (1-esperancaX)*(0-esperancaY)*c + (1-esperancaX)*(1-esperancaY)*d;
cov;

# Pregunta 3: Cierto punto de facturación se caracteriza porque el número de viajeros que llegan por minuto se distribuye según una ley Poisson con una tasa de llegadas de {promedio = 10.5}. Indique la esperanza y la desviación estándar de esta variable.
promedio = 10.5;
desviacion = sqrt(promedio);
promedio; desviacion;

# Pregunta 4: Considerando los mostradores de facturación del 1 al 6 {n = 6} caracterizados por una probabilidad {p = 0.52} de observar exactamente 0 llegadas en un minuto, indique la esperanza y la varianza de la variable número de puntos de facturación con 0 llegadas en un minuto dado.
n = 6;
p = 0.52;
esperanca = n*p;
desviacion = n*p*(1-p);
esperanca; desviacion;

# Pregunta 5: Si consideramos los {n = 215} mostradores de facturación de una terminal caracterizados por una probabilidad {p = 0.047} de observar más de 3 llegadas en un minuto, halle la esperanza y la desviación estándar de la variable número de puntos de facturación con más de 3 llegadas en un minuto, usando el modelo Binomial.
n = 215;
p = 0.047;
esperanca = n*p;
desviacion = sqrt(n*p*(1-p));
esperanca; desviacion;

# Pregunta 6: Repita la pregunta para un supuesto en el que empleamos un modelo de Poisson.
desviacion = sqrt(esperanca);
esperanca; desviacion;

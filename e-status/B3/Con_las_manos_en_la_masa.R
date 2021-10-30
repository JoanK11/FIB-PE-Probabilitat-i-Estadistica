# Con las manos en la masa
# Cecilia es una aficionada a la pintura, a la fotografía y al cine en general, pero lo que más le gusta es guardarse los vídeos de cocina de Karlos Arguiñano para inspirarse en sus obras, esperando que un día sean conocidas por el mundo entero.
# Ya se ha dado cuenta de que Arguiñano pierde el sentido del tiempo mientras explica sus recetas, y que la duración de estos vídeos es considerablemente variable.
# Lo que es cierto es que un vídeo, en promedio, dura {esperanca = 7.3} minutos, y que la duración de uno es independiente de la de los demás. Cecilia archiva sus recetas en colecciones de {n = 65} vídeos.
esperanca = 7.3;
n = 65;

# Pregunta 1: Suponga que la distribución de la duración de un vídeo es {X}, con extremo inferior = {a = 2.5} minutos. ¿Cuánto vale su variancia?

# Si X = uniforme asigna 0 a la variable X. Si X = exponencial asigna 1 a la variable X.
# En este ejemplo haremos que X sea uniforme.
X = ;

if (X == 0) {
  a = 2.5
  b = esperanca*2 - a
  pregunta1 <- ((b - a)^2)/12
} else { 
  lambda = 1/esperanca
  pregunta1 <- 1/lambda^2
}

pregunta1;

# Pregunta 2: ¿Probabilidad de que un vídeo dure por lo menos {x = 8.58} minutos?
x = 8.58;

if (X == 0) {
  pregunta2 <- 1 - punif(x, a, b);
} else {
  pregunta2 <- 1 - pexp(x, lambda);
}

pregunta2;

# Pregunta 3: ¿Probabilidad de que Arguiñano emplee en una receta un tiempo superior a {x = 11.95} minutos?
x = 11.95;

if (X == 0) {
  pregunta3 <- 1 - punif(x, a, b);
} else {
  pregunta3 <- exp(-lambda*x);
}

pregunta3;

# Pregunta 4: Cada colección de 65 vídeos es grabada en un DVD con capacidad de {c = 4.3} GB (1GB = 1024MB).
# Teniendo en cuenta que aproximadamente un minuto de grabación se corresponde con {e = 8.8} MB de espacio en disco, calcule la probabilidad de que Cecilia no pueda grabar todos esos vídeos en un DVD porque excede su capacidad.
# Para la distribución del tamaño de la colección, considere la aplicación del Teorema Central del Límite.
capacidad = 4.3*1024;
espacio = 8.8;

esperanca2 = esperanca*n*espacio;
variancia = pregunta1;
desviacion2 = sqrt(variancia*n)*espacio;
pregunta4 <- 1 - pnorm(capacidad, esperanca2, desviacion2);

pregunta4;

# Pregunta 5:  ¿Cuál es el tamaño máximo en MB (con probabilidad {p = 99%}) que puede alcanzar una colección? Redondee al entero más próximo.
p = 0.99;
pregunta5 <- round(qnorm(p, esperanca2, desviacion2));
pregunta5;

# Pregunta 6: ¿Cuántos vídeos debería incluir una colección para tener una seguridad del {p = 95%} de que cabrá en un DVD? Redondee el resultado a un entero por defecto.
p = 0.95;

for (i in n:1) {
  if (qnorm(p, i*esperanca, sqrt(i*variancia))*espacio < capacidad) break;
}
pregunta6 <- i;
pregunta6;
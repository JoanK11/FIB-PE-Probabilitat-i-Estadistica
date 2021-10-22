# Distribución Binomial
# Sean las siguientes variables aleatorias discretas:
    # X1 ~ B(14, 0.35)
    n1 = 14; p1 = 0.35; 
    # X2 ~ B(21, 0.838)
    n2 = 21; p2 = 0.838;
# Responde a las siguientes preguntas. Ten en cuenta que se espera que las tres primeras se respondan con tablas; para las tres siguientes es mejor utilizar un programa que facilite los resultados exactos, de lo contrario será muy difícil conseguir la precisión requerida (cuatro decimales).

# FORMULARIO COMPARACIÓN ÚNICA
    # P(X >= k)  -->  1 - pbinom(k-1, n, p)
    # P(X >  k)  -->  1 - pbinom(k, n, p)
    # P(X <= k)  -->  pbinom(k, n, p)
    # P(X <  k)  -->  pbinom(k-1, n, p)

# FORMULARIO COMPARACIÓN DOBLE
    # P(k1 <= X <= k2) -->  pbinom(k2, n, p) - pbinom(k1-1, n, p)
    # P(k1 <= X <  k2) -->  pbinom(k2-1, n, p) - pbinom(k1-1, n, p)
    # P(k1 <  X <= k2) -->  pbinom(k2, n, p) - pbinom(k1, n, p)
    # P(k1 <  X <  k2) -->  pbinom(k2-1, n, p) - pbinom(k1, n, p)

# Pregunta 1: P(X1 <= 9)
k = 9;
pregunta1 <- pbinom(k, n1, p1);
pregunta1;

# Pregunta 2: P(4 < X1 <= 9)
k1 = 4; k2 = 9;
pregunta2 <- pbinom(k2, n1, p1) - pbinom(k1, n1, p1);
pregunta2;

# Pregunta 3:  ¿Cuál es el menor valor entero k tal que P(X1 <= k) es superior a 0.8905?
k = 0.8905;
pregunta3 <- qbinom(k, n1, p1);
pregunta3;

# Pregunta 4: P(X2 <= 12)
k = 12;
pregunta4 <- pbinom(k, n2, p2);
pregunta4;

# Pregunta 5: P(X2 < 11)
k = 11;
pregunta5 <- pbinom(k-1, n2, p2);
pregunta5;

# Pregunta 6: P(11 < X2 <= 12)
k1 = 11; k2 = 12;
pregunta6 <- pbinom(k2, n2, p2) - pbinom(k1, n2, p2);
pregunta6;

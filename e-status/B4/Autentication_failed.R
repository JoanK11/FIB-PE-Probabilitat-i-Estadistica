# Autentication failed
# El equipo de IT ha añadido una funcionalidad en el sistema de login, con el objetivo de dotarlo de más seguridad para los usuarios. Simplemente, se registra la velocidad con la que el usuario teclea la contraseña, midiendo los tiempos que median entre dos caracteres consecutivos, tal como se ve en la figura, representando la acción que acaba de intentar acceder al sistema (tiempos en milisegundos).
nterm <- c(94, 70, 74, 48, 51, 130, 130, 109, 68, 60, 64, 43, 125)
n = length(nterm)

# 1. La línea gris indica la estimación puntual observada del tiempo promedio entre dos pulsaciones para este usuario.
# Estime con dos decimales el error típico de la media muestral.
se = sd(nterm)/sqrt(n)
se

# 2. Seguidamente, con la muestra observada, estime la variancia del tiempo entre pulsaciones.
variancia = sd(nterm)^2
variancia

# 3. Hemos conocido una estimación por intervalo de confianza para el parámetro “tiempo esperado entre dos caracteres”, correspondiente a nuestro usuario: (64.8, 99.2). Sin asumir que conoce el valor de la desviación típo poblacional, ¿puede decir el grado de confianza utilizado?
q1 = 64.8; q2 = 99.2
Z = (q2-mean(nterm))*sqrt(n)/sd(nterm)
p = pt(Z, n-1)
pregunta3 <- 2*p-1
pregunta3*100
100*(2*pt(-(q1-mean(nterm))*sqrt(n)/sd(nterm), n-1)-1)

# 4. El sistema supone, a partir de la información registrada previamente, que el tiempo medio del supuesto usuario es {medio = 89}. Si pusiera a prueba que la entrada que se acaba de hacer proviene efectivamente de tal usuario, diga cuál es el valor P que hallaría.
medio = 89
t = (mean(nterm)-medio)*sqrt(n)/sd(nterm)
pregunta4 = 2*(1- pt(t, n-1))
if (pregunta4 > 1) pregunta4 = 2*pt(t, n-1)
pregunta4

# 5. Obtenga un intervalo de confianza de {IC = 75%} para la variancia del tiempo, según la muestra disponible.
IC = 0.75; alpha = 1 - IC
x1 = sd(nterm)^2*(n-1)/qchisq(1-alpha/2, n-1)
x2 = sd(nterm)^2*(n-1)/qchisq(alpha/2, n-1)
pregunta5 <- print(paste(x1, x2))

# 6. Uno de estos tres intervalos no es un intervalo de confianza para la variancia. ¿Puede decir cuál?
  # (621.4 2093.7)
  # (696.7 1761)
  # (579.3 2330.6)
# Comprobamos que el intervalo de confianza es válido si tanto el mínimo como el máximo de un intervalo usan el mismo IC
# El intervalo que tenga mayor diferencia entre el IC del mínimo y de su máximo será el que no sea intervalo de confianza
min1 = 621.4; max1 = 2093.7;
Zmin1 = sd(nterm)^2*(n-1)/min1; Zmax1 = sd(nterm)^2*(n-1)/max1
Pmin1 = 2*pchisq(Zmin1, n-1) - 1; Pmax1 = 1 - 2*pchisq(Zmax1, n-1)
diff1 = abs(Pmax1 - Pmin1)

min2 = 696.7; max2 = 1761;
Zmin2 = sd(nterm)^2*(n-1)/min2; Zmax2 = sd(nterm)^2*(n-1)/max2
Pmin2 = 2*pchisq(Zmin2, n-1) - 1; Pmax2 = 1 - 2*pchisq(Zmax2, n-1)
diff2 = abs(Pmax2 - Pmin2)

min3 = 579.3; max3 = 2330.6;
Zmin3 = sd(nterm)^2*(n-1)/min3; Zmax3 = sd(nterm)^2*(n-1)/max3
Pmin3 = 2*pchisq(Zmin3, n-1) - 1; Pmax3 = 1 - 2*pchisq(Zmax3, n-1)
diff3 = abs(Pmax3 - Pmin3)

if (max(diff1, diff2, diff3) == diff1) print(1);
if (max(diff1, diff2, diff3) == diff2) print(2);
if (max(diff1, diff2, diff3) == diff3) print(3);
# Rectas para todo
n = 61
X = 3.95828
Y = 20.25477
sX = 19.56843
sY = 0.55217
sXY = -2.75517

b1 = sXY/sX^2
b0 = Y - b1*X
s2 =(((n - 1)*sY^2) - (b1*(n-1)*sXY))/(n - 2)
sb0 = sqrt(s2*(1/n + X^2/((n - 1)*sX^2)))
sb1 = sqrt(s2/((n - 1)*sX^2))
R2 = (sXY/(sX*sY))^2;

# ¿Cuánto vale la estimación del término independiente?
b0

# ¿Cuánto vale la pendiente estimada?
b1

# La estimación del coeficiente lineal vale...
b1

# ¿De cuántos grados de libertad dispone este modelo para estimar los parámetros?
n - 2

# Halle el coeficiente de determinación asociado al modelo.
R2

# De acuerdo con la hipótesis: β0 = {B0 = 20.2}, diga qué valor toma el estadístico de la prueba.
B0 = 20.2
t = (b0 - B0)/sb0; t

# De acuerdo con la hipótesis: β1 = {B1 = -0.009}, diga qué valor toma el estadístico de la prueba.
B1 = -0.009
t = (b1 - B1)/sb1; t

# ¿Se podría rechazar la hipótesis: β0 = {B0 = 20.2}? (use riesgo α = {alpha = 5%}; 0: no, 1: sí).
B0 = 20.2
alpha = 0.05
t = qt(1 - alpha/2, n - 2)
ifelse((b1 - B1)/sb1 > t, 1, 0)

# ¿Se podría rechazar la hipótesis: β1 = {B1 = -0.009}? (use riesgo α = {alpha = 5%}; 0: no, 1: sí).
B1 = -0.009
alpha = 0.05
t = qt(1 - alpha/2, n - 2)
ifelse((b1 - B1)/sb1 > t, 1, 0)

# ¿Qué parte (en %) de la variabilidad total explica la variable X?
R2*100

# ¿Qué parte (en %) de la variabilidad total se atribuye a factores diferentes de X?
100 - R2*100

# Obtenga una estimación de la variancia residual.
((n - 1)*sY^2 - b1*(n - 1)*sXY)/(n - 2)

# Diga el valor que se ha estimado para la desviación típica del término aleatorio del modelo.
sqrt(s2)

# Contrastando el término constante hemos obtenido un estadístico igual a {T = -0.69033}. ¿Qué valor se estaba contrastando?
T = -0.69033
B0 = b0 - T*sb0; B0

# Contrastando el parámetro lineal hemos obtenido un estadístico igual a {T = 1.35267}. ¿Qué valor se estaba contrastando?
T = 1.35267
B1 = b1 - T*sb1; B1

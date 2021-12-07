# Técnicas de optimización
A <- c(); B <- c();
n = nrow(data);

for (i in 1:n) {
  if (data$met[i] == 'A') {
    A <- c(A, data$stat[i]);
  } else {
    B <- c(B, data$stat[i]);
  }
}
nA = length(A); nB = length(B);

# Pregunta 1: Complete la tabla de frecuencias. Introduzca los valores por columnas, separados por un blanco.
nA0 = 0; nA1 = 0; nA2 = 0; nB0 = 0; nB1 = 0; nB2 = 0;
for (i in 1:nA) {
  if (A[i] == 0) { nA0 = nA0 + 1;
  } else if (A[i] == 1) { nA1 = nA1 + 1;
  } else nA2 = nA2 + 1;
}
for (i in 1:nB) {
  if (B[i] == 0) { nB0 = nB0 + 1;
  } else if (B[i] == 1) { nB1 = nB1 + 1;
  } else nB2 = nB2 + 1;
}
pregunta1 <- print(paste(nA0, nB0, nA1, nB1, nA2, nB2));

# Pregunta 2: ¿Cuál sería el número esperado de observaciones, si en cuanto al estado final los dos procedimientos respondieran sin diferencias en su distribución?
# Responda para el caso concreto de acabar en estado imposible usando la formulación Punto Interior.
n0 = nA0 + nB0; n1 = nA1 + nB1; n2 = nA2 + nB2;
eA0 = nA*n0/n; eA1 = nA*n1/n; eA2 = nA*n2/n;
eB0 = nB*n0/n; eB1 = nB*n1/n; eB2 = nB*n2/n;

cat("Simplex y óptima:\t", eA0, "\nSimplex y subóptima:\t", eA1, "\nSimplex e imposible:\t", eA2,
    "\nPunto interior y óptima:\t", eB0, "\nPunto interior y subóptima:\t", eB1, "\nPunto interior e imposible:\t", eB2);

# Pregunta 3: La prueba de Pearson consiste en observar si los valores esperados en caso de independencia están muy alejados de los reales. Si fueran parecidos, querría decir que no hay motivos para sospechar que un método funcione globalmente de forma distinta al otro. Pero solo que haya una celda que demuestre ser muy diferente, eso hace que el estadístico de Pearson (X2) aumente significativamente.
# En este caso, ¿cuánto vale el estadístico de Pearson?
xA0 = (nA0-eA0)^2/eA0; xA1 = (nA1-eA1)^2/eA1; xA2 = (nA2-eA2)^2/eA2;
xB0 = (nB0-eB0)^2/eB0; xB1 = (nB1-eB1)^2/eB1; xB2 = (nB2-eB2)^2/eB2;
pregunta3 <- xA0 + xA1 + xA2 + xB0 + xB1 + xB2;
pregunta3;

# Pregunta 4: Escoja la opción más adecuada:
  # La significación estadística de la prueba de Pearson es:
  # muy escasa (1 de cada 5, o más). No hay evidencias sobre una relación
  # posible (entre 1 de cada 5, y 1 de cada 20). Hay una evidencia muy débil
  # moderada (entre 1 de cada 20, y 1 de cada 200). La evidencia sobre la relación es sustancial
  # fuerte (1 de cada 200, o menos). Es muy raro que el resultado observado se pueda deber únicamente al azar.
X = pregunta3; grados = (2 - 1)*(3 - 1);
P = 1 - pchisq(X, grados);
if (P > 1/5) {
  pregunta4 <- print(1) # Muy escasa
} else if (P > 1/20 && P < 1/5) {
  pregunta4 <- print(2) # Posible
}	else if (P > 1/200 && P < 1/20) {
  pregunta4 <- print(3) # Moderada
} else if (P < 1/200) {
  pregunta4 <- print(4) # "Fuerte"
}

# Pregunta 5: Mirando las proporciones logradas en los estados finales, ¿son similares Simplex y Punto Interior? ¿Hay más proporción de instancias que acaban en estado imposible con Simplex que con el otro método?
pA0 = nA0/nA; pA1 = nA1/nA; pA2 = nA2/nA; pA2
pB0 = nB0/nB; pB1 = nB1/nB; pB2 = nB2/nB; pB2

cat("Simplex y óptima:\t", pA0, "\nSimplex y subóptima:\t", pA1, "\nSimplex e imposible:\t", pA2,
    "\nPunto interior y óptima:\t", pB0, "\nPunto interior y subóptima:\t", pB1, "\nPunto interior e imposible:\t", pB2);

# Pregunta 6: Suponiendo la hipótesis H
  # H: la proporción de resultados de tipo imposible es la misma para ambos métodos
# ¿cuál es la estimación de la probabilidad común que presenta este tipo de resultado?
eA0/(eA0 + eA1 + eA2); eB0/(eB0 + eB1 + eB2) # Óptimo
eA1/(eA0 + eA1 + eA2); eB1/(eB0 + eB1 + eB2) # Subóptimo
eA2/(eA0 + eA1 + eA2); eB2/(eB0 + eB1 + eB2) # Imposible

# Pregunta 7: Calcule el error tipo correspondiente a la diferencia de dos proporciones, bajo el supuesto de la hipótesis H.
P0 = (nA*pA0+nB*pB0)/(nA+nB) # Óptimo
P1 = (nA*pA1+nB*pB1)/(nA+nB) # Subóptimo
P2 = (nA*pA2+nB*pB2)/(nA+nB) # Imposible

se0 = sqrt(P0*(1 - P0)/nA + P0*(1 - P0)/nB); # Óptimo
se1 = sqrt(P1*(1 - P1)/nA + P1*(1 - P1)/nB); # Subóptimo
se2 = sqrt(P2*(1 - P2)/nA + P2*(1 - P2)/nB); # Imposible

# Pregunta 8: Para responder a la prueba que compara las proporciones de ambos métodos en el estado imposible, diga cuánto vale el estadístico asociado a la prueba (puede obviar el signo, ya que se tratará como bilateral), y el valor de P de dicha prueba.
Z0 = (nA0/nA - nB0/nB)/se0; Z1 = (nA1/nA - nB1/nB)/se1; Z2 = (nA2/nA - nB2/nB)/se2;

# Z = Z0 # Óptimo
# Z = Z1 # Subóptimo
Z = Z2; # Imposible

if (Z >= 0) {
  P = 2*(1 - pnorm(Z));
  } else {
  P = 2*pnorm(Z)
}

pregunta8 <- print(paste(abs(Z), P));

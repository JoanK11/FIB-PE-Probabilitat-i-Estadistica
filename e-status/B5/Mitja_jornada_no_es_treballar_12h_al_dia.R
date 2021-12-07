# Mitja jornada no és treballar 12h al dia
nB = 10; meanB = 54; sdB = 8.5505;
nW = 11; meanW = 46.09091; sdW = 13.11072;

# Pregunta 1: Troba una estimació conjunta per a la variància del temps dedicat a una tasca, tenint en compte que es suposa que aquest paràmetre val el mateix per als dos equips.
pregunta1 <- ((nB - 1)*sdB^2 + (nW - 1)*sdW^2)/(nB + nW - 2)
pregunta1

# Pregunta 2: Com pots comprovar a les dades i gràficamente, hi ha una diferència entre les mitjanes, però no sabem si aquesta diferència és purament deguda a l'atzar.
# Pots estimar quant val l'error tipus de la diferència de mitjanes en aquest cas? Assumeix la premissa de l'apartat anterior.
S = sqrt(pregunta1)
pregunta2 <- S*sqrt(1/nB + 1/nW) 
pregunta2

# Pregunta 3: Considerant una hipòtesi H: μB = μW, contesta amb el valor de l'estadístic t de la prova corresponent.
se = pregunta2
pregunta3 <- (meanB - meanW)/se
pregunta3

# Pregunta 4: Quina és correcta?
  # 1. hem demostrat que l'esforç és similar als dos equips.
  # 2. B empra més temps per resoldre tasques
  # 3. W empra més temps per resoldre tasques
  # 4. no es pot assegurar res

if (abs(pregunta3) < qt(0.975, nB + nW - 2)) {
  print(4); # No podem rebutjar
}

# Pregunta 5: Admitim provisionalment que la desviació tipus del temps val 13. En aquest cas, què val l'estadístic de la prova pertinent?
sd = 13
pregunta5 <- (meanB - meanW)/(sd*sqrt(1/nB + 1/nW))
pregunta5

# Pregunta 6: Suposant que la prova d'hipòtesis contempla una alternativa bilateral, calcula el valor P del resultat de la pregunta anterior.
T = pregunta5
if (T >= 0) {
  pregunta6 <- 2*(1 - pnorm(T))
} else pregunta6 <- 2*pnorm(T)
pregunta6

# Pregunta 7: Anem a estudiar un escenari diferent. El responsable de l'estudi pot estar dubtant sobre la homogeneïtat dels dos equips. Potser un d'ells és més expert, i la seva variabilitat és menor que la de l'altre.
# Si fixem un nivell d'error α del 10% per fer una trova de comparació de les variàncies, troba el punt que separa la regió d'acceptació de la de rebuig per a aquest cas.
alpha = 0.1
pregunta7 <- qf(1-alpha/2, nW - 1, nB - 1)
pregunta7

# Pregunta 8: D'acord amb els resultats trobats:
  # 1. l'evidència ens diu que tenim dues variàncies diferents.
  # 2. hem trobat proves de que la variància a B és igual a la de W.
  # 3. és versemblant que els dos equips presentin homogeneïtat quant a la dispersió dels seus temps.

# Si F > pregunta7 --> Rebutgem --> 1
# Si F < pregunta7 --> No podem rebutjar --> 3
SM = max(sdB, sdW); Sm = min(sdB, sdW)
F = SM^2/Sm^2
if (F > pregunta7) {
  print(1)
} else {
  print(3)
}

# Pregunta 9: Quant més triga l'equip B respecte W a resoldre una tasca, en mitjana?
# Feu una estimació per interval de confiança al 98% de la diferència de mitjanes del temps que B i W (assumiu variància comuna i desconeguda)
prob = 0.98; alpha = 1 - prob
IC1 = (meanB - meanW) - qt(1-alpha/2, nB + nW - 2)*se
IC2 = (meanB - meanW) + qt(1-alpha/2, nB + nW - 2)*se
pregunta9 <- print(paste(IC1, IC2))

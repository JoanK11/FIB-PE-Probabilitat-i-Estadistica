# ECTS
# La Facultat ha pres una mostra de les respostes dels alumnes a la enquesta ECTS, amb la qual s'estudia el temps que es dedica a cada assignatura (resultats en hores). A la mostra que es visualitza a la figura han participat {n = 12} estudiants que han valorat una assignatura determinada.
nterm <- c(117, 94, 107, 101, 115, 108, 101, 105, 87, 91, 117, 144)
n <- length(nterm)

# 1. Quina és l'estimació puntual del temps mitjà que els estudiants dediquen a l'assignatura en qüestió?
pregunta1 <- sum(nterm)/n
pregunta1

# 2. Amb la mostra disponible, doneu una estimació de l'error de la mitjana (error tipus, o standard error, la magnitud de la variació d'origen aleatori que és propi de la mitjana mostral).
pregunta2 <- sd(nterm)/sqrt(n)
pregunta2

# 3. Es demana que trobeu una estimació per interval de confiança al {p = 90%} de la mitjana poblacional de la variable "Temps dedicat a l'assignatura". Assumiu que la desviació poblacional val {sd = 14}.
p = 0.9; sd = 14
p1 = (1-p)/2; p2 = 1- p1
z1 <- qnorm(p1, pregunta1, sd/sqrt(n))
z2 <- qnorm(p2, pregunta1, sd/sqrt(n))
pregunta3 <- print(paste(z1, z2))

# 4. Calculeu una altra estimació per al mateix paràmetre, però sense assumir coneguda la desviació poblacional. Feu l'interval amb confiança {p = 95%}.
p = 0.95;
p1 = (1-p)/2; p2 = 1- p1
z1 <- pregunta1 + qt(p1, n-1)*sd(nterm)/sqrt(n)
z2 <- pregunta1 - qt(p1, n-1)*sd(nterm)/sqrt(n)
pregunta4 <- print(paste(z1, z2))

# 5. Volem trobar un interval de confiança al {p = 98%} per a la mitjana, Suposem també que l'autèntica desviació és σ=14, i es desitja que l'amplada de l'interval sigui de {amplada = 5} hores, com a molt. Quantes observacions necessitem recollir?
# amplada = límit superior - límit inferior
p = 0.98; sd = 14; amplada = 5
IC2 = pregunta1 + amplada/2
z = qnorm(p+(1-p)/2);
pregunta5 <- ceiling((z*sd/(IC2-pregunta1))^2)
pregunta5

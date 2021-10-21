# Made in Taiwan
# La figura representa: a l'esquerra, temps de vida de certs components electrònics, que segueixen una distribució exponencial amb mitjana 12 mesos; a la dreta, freqüència del rellotge d'un determinat tipus de processador, d'acord a una distribucio Normal amb esperança 2.2 GHz i desviacio tipus 0.054 GHz.
mitjana = 12;
esperanca = 2.2;
desviacio = 0.054;

# Pregunta 1: Quina és la probabilitat que un component que ja ha funcionat 1 mes en duri més de 4?
mes1 = 1; mes2 = 4;
diff = mes2 - mes1;
lambda = 1/mitjana;
pregunta1 <- 1 - pexp(diff, lambda);
pregunta1

# Pregunta 2: Quina es la probabilitat que un component duri entre 2 i 21 mesos?
mes_i = 2; mes_f = 21;
pregunta2 <- pexp(mes_f, lambda) - pexp(mes_i, lambda);
pregunta2;

# Pregunta 3: Quin es el percentil del 97% per a un component de la primera població especificada?
prob = 0.97;
pregunta3 <- qexp(prob, lambda);
pregunta3;

# Pregunta 4: Els 39% dels components de la competència arriben a duracions superiors a 14 mesos. Quina és la duració mitjana d'aquests components?
prob = 0.39;
durada = 14;
lambda = -log(prob)/durada;
pregunta4 <- 1/lambda;
pregunta4;

# Pregunta 5: Trobeu la probabilitat que la freqüència mesurada d'un processador sigui mes gran que 2.253.
freq = 2.253;
pregunta5 <- 1 - pnorm(freq, esperanca, desviacio);
# En cas que demanin que freq sigui menor que 2.253 no s'ha de restar 1 a pnorm.
pregunta5;

# Pregunta 6: Trobeu la probabilitat que la freqüència mesurada sigui menor que 2.19.
freq = 2.19;
pregunta6 <- pnorm(freq, esperanca, desviacio);
pregunta6;

# Pregunta 7: Digueu quina és la freqüència que correspon com a percentil de la probabilitat 78.7%.
prob = 0.787;
pregunta7 <- qnorm(prob, esperanca, desviacio);
pregunta7;
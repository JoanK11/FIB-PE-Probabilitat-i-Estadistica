# Un de cada milió de sectors
# El proveidor de discs durs de l'any passat comercialitza ara un model de {tamany = 325} GB, i estem analitzant si ens convé. Aquest fabricant ha fet proves per quantificar l'aparició de sectors defectuosos, que no sol ser un problema mentre sigui un nombre petit, i ha determinat que la probabilitat que no s'observi cap defecte en un disc tipus de 100 GB durant un any de funcionament és {prob = 0.1653}.
tamany = 325;
prob = 0.1653;

# Pregunta 1: Una d'aquestes distribucions correspon al nombre de sectors defectuosos d'un disc de 325 GB en un any (es mostren els valors corresponents al 99% central). Selecciona la distribució correcta (1, 2 o 3).
lambda = -log(prob)*tamany/100;
x1 = floor(lambda-1); x2 = floor(lambda);
x3 = floor(lambda+1); x4 = floor(lambda+2);
q1 = dpois(x1, lambda); q2 = dpois(x2, lambda);
q3 = dpois(x3, lambda); q4 = dpois(x4, lambda);
cat("En x =", x1, ": p =", q1, "\nEn x =", x2, ": p =", q2, "\nEn x =", x3, ": p =", q3, "\nEn x =", x4, ": p =", q4);
# El resultat serà la gràfica que tingui en els valors x1, x2, x3 i x4 les seves probabilitats corresponents.

# Pregunta 2: Quin seria el nombre esperat de defectes en el disc que es comercialitza, per a un hipotètic periode de {anys = 4} anys i {mesos = 6} mesos d'activitat?
anys = 4; mesos = 6;
temps = anys + mesos/12;
pregunta2 <-  lambda*temps;
pregunta2;

# Pregunta 3: Segurament és preferible tenir una mesura de quin és el risc en una situació més extrema. Per exemple, saber quin és el màxim nombre previst de sectors defectuosos, amb un error del {error = 5}%, per discos amb {anys = 4} anys i {mesos = 6} mesos d'antiguitat. Respon amb un nombre enter, arrodonit per excés.
error = 5;
temps = anys + mesos/12;
pregunta3 <- ceiling(qnorm(1-error/100, lambda*temps, sqrt(lambda*temps)));
pregunta3;

# Pregunta 4: La garantia dels discs estableix que es reemplaçaran de franc, i per un únic cop, els discos avariats dins del període de garantia, entenent-se per avariat un disc amb més de 'n' sectors defectuosos. Calculeu quin ha de ser el valor de 'n' per tal que, en promig, només calgui substituir, com a màxim, un 1% dels discos al llarg d'un període de garantia de {anys = 3} anys.
anys = 3;
pregunta4 <- ceiling(qnorm(0.99, lambda*anys, sqrt(lambda*anys)));
pregunta4;

# Pregunta 5: En lloc d'una garantia de {anys1 = 3} anys, s'ofereix una alternativa: {anys2 = 2.5} anys, tenint en compte que s'enten com a disc avariat si presenta {n = 21} sectors defectuosos com a mínim. Si un client agafa aquest garantia,
    # quina és la probabilitat que li hagin de reemplaçar el disc?
    # És preferible aquesta garantia, des del punt de vista del client?
anys1 = 3; anys2 = 2.5;
n = 21;
p1 <- pnorm(n, lambda*anys1, sqrt(lambda*anys1));
p2 <- pnorm(n, lambda*anys2, sqrt(lambda*anys2));
millor = "S";
if (p1 >= p2) millor = "N";
print(paste(1 - p2, millor));

# Pregunta 6: Calculeu el valor esperat del cost per substitució en garantia per disc venut, si el període de garantia és de {anys = 3} anys, 'n' = {n = 24} i el cost d'un disc és de {preu = 1542} euros.
anys = 3;
n = 24;
preu = 1542;
pregunta6 <- preu * (1 - pnorm(n, lambda*anys, sqrt(lambda*anys)));
pregunta6;
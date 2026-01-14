
# Lectura de datos
library(foreign)
datos_obesidad <- read.spss("C:/Users/anton/Desktop/datos_obesidad_pescado_carne.sav", to.data.frame = T)

#### Latent class analisis correcto ####
library(poLCA)

levels(datos_obesidad$Ejercicio_est_obesidad)
levels(datos_obesidad$Fruta_est_obesidad)
levels(datos_obesidad$verduras_est_obesidad)
levels(datos_obesidad$Dulces_est_obesidad)
levels(datos_obesidad$Refrescos_est_obesidad)
levels(datos_obesidad$Pescado_est_obesidad)
levels(datos_obesidad$Cane_est_obesidad)


datos_obesidad$Ejercicio_est_obesidad_dic[datos_obesidad$Ejercicio_est_obesidad == "Inactivo"] <- 1
datos_obesidad$Ejercicio_est_obesidad_dic[datos_obesidad$Ejercicio_est_obesidad == "Activo"] <- 2

datos_obesidad$Fruta_est_obesidad_dic[datos_obesidad$Fruta_est_obesidad == "No a diario"] <- 1
datos_obesidad$Fruta_est_obesidad_dic[datos_obesidad$Fruta_est_obesidad == "A diario"] <- 2

datos_obesidad$verduras_est_obesidad_dic[datos_obesidad$verduras_est_obesidad == "No a diario"] <- 1
datos_obesidad$verduras_est_obesidad_dic[datos_obesidad$verduras_est_obesidad == "A diario"] <- 2

datos_obesidad$Dulces_est_obesidad_dic[datos_obesidad$Dulces_est_obesidad == "A diario"] <- 1
datos_obesidad$Dulces_est_obesidad_dic[datos_obesidad$Dulces_est_obesidad == "No a diario"] <- 2

datos_obesidad$Refrescos_est_obesidad_dic[datos_obesidad$Refrescos_est_obesidad == "A diario"] <- 1
datos_obesidad$Refrescos_est_obesidad_dic[datos_obesidad$Refrescos_est_obesidad == "No a diario"] <- 2

datos_obesidad$Pescado_est_obesidad_dic[datos_obesidad$Pescado_est_obesidad == "lo contrario=malo"] <- 1
datos_obesidad$Pescado_est_obesidad_dic[datos_obesidad$Pescado_est_obesidad == "3 o más veces por semana, pero no a diario"] <- 2

datos_obesidad$Cane_est_obesidad_dic[datos_obesidad$Cane_est_obesidad == "Más de 3 veces a la semana"] <- 1
datos_obesidad$Cane_est_obesidad_dic[datos_obesidad$Cane_est_obesidad == "Menos de 3 veces a la semana"] <- 2


summary(datos_obesidad$Ejercicio_est_obesidad_dic)
summary(datos_obesidad$Fruta_est_obesidad_dic)
summary(datos_obesidad$verduras_est_obesidad_dic)
summary(datos_obesidad$Dulces_est_obesidad_dic)
summary(datos_obesidad$Refrescos_est_obesidad_dic)
summary(datos_obesidad$Pescado_est_obesidad_dic)
summary(datos_obesidad$Cane_est_obesidad_dic)


f1 = cbind(Ejercicio_est_obesidad_dic, Fruta_est_obesidad_dic, verduras_est_obesidad_dic, Dulces_est_obesidad_dic, 
           Refrescos_est_obesidad_dic, Pescado_est_obesidad_dic, Cane_est_obesidad_dic) ~ 1


#### Analisis en la poblacion >=18 y segmentando por sexo ####
datos_obesidad <- datos_obesidad[which(datos_obesidad$EDADa >= 18),]
datos_obesidad_men <- datos_obesidad[which(datos_obesidad$SEXOa == "Varón"), ]
datos_obesidad_women <- datos_obesidad[which(datos_obesidad$SEXOa == "Mujer"), ]

# LCA Mujeres
M2w <- poLCA(f1, datos_obesidad_women, nclass = 2, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M2w
poLCA.entropy(M2w)

##RELATIVE ENTROPY
##Numerator:
nume.E2w <- -sum(M2w$posterior * log(M2w$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E2w <- 52480*log(2)
##Relative Entropy
Entro2w <- 1-(nume.E2w/deno.E2w)
Entro2w



M3w <- poLCA(f1, datos_obesidad_women, nclass = 3, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M3w
poLCA.entropy(M3w)

##RELATIVE ENTROPY
##Numerator:
nume.E3w <- -sum(M3w$posterior * log(M3w$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E3w <- 52480*log(3)
##Relative Entropy
Entro3w <- 1-(nume.E3w/deno.E3w)
Entro3w



M4w <- poLCA(f1, datos_obesidad_women, nclass = 4, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M4w
poLCA.entropy(M4w)


##RELATIVE ENTROPY
##Numerator:
nume.E4w <- -sum(M4w$posterior * log(M4w$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E4w <- 52480*log(4)
##Relative Entropy
Entro4w <- 1-(nume.E4w/deno.E4w)
Entro4w




M5w <- poLCA(f1, datos_obesidad_women, nclass = 5, verbose = F, graphs = T, na.rm = T, maxiter = 30000, nrep = 2)
M5w
poLCA.entropy(M5w)


##RELATIVE ENTROPY
##Numerator:
nume.E5w <- -sum(M5w$posterior * log(M5w$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E5w <- 52480*log(5)
##Relative Entropy
Entro5w <- 1-(nume.E5w/deno.E5w)
Entro5w



M6w <- poLCA(f1, datos_obesidad_women, nclass = 6, verbose = F, graphs = T, na.rm = T, maxiter = 30000, nrep = 2)
M6w
poLCA.entropy(M6w)


##RELATIVE ENTROPY
##Numerator:
nume.E6w <- -sum(M6w$posterior * log(M6w$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E6w <- 52480*log(6)
##Relative Entropy
Entro6w <- 1-(nume.E6w/deno.E6w)
Entro6w




M7w <- poLCA(f1, datos_obesidad_women, nclass = 7, verbose = F, graphs = T, na.rm = T, maxiter = 40000, nrep = 2)
M7w
poLCA.entropy(M7w)


##RELATIVE ENTROPY
##Numerator:
nume.E7w <- -sum(M7w$posterior * log(M7w$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E7w <- 52480*log(7)
##Relative Entropy
Entro7w <- 1-(nume.E7w/deno.E7w)
Entro7w





# LCA Hombres
M2m <- poLCA(f1, datos_obesidad_men, nclass = 2, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M2m
poLCA.entropy(M2m)

##RELATIVE ENTROPY
##Numerator:
nume.E2m <- -sum(M2m$posterior * log(M2m$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E2m <- 41015*log(2)
##Relative Entropy
Entro2m <- 1-(nume.E2m/deno.E2m)
Entro2m




M3m <- poLCA(f1, datos_obesidad_men, nclass = 3, verbose = F, graphs = T, na.rm = T, maxiter = 10000, nrep = 2)
M3m
poLCA.entropy(M3m)

##RELATIVE ENTROPY
##Numerator:
nume.E3m <- -sum(M3m$posterior * log(M3m$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E3m <- 41015*log(3)
##Relative Entropy
Entro3m <- 1-(nume.E3m/deno.E3m)
Entro3m





M4m <- poLCA(f1, datos_obesidad_men, nclass = 4, verbose = F, graphs = T, na.rm = T, maxiter = 20000, nrep = 3)
M4m
poLCA.entropy(M4m)

##RELATIVE ENTROPY
##Numerator:
nume.E4m <- -sum(M4m$posterior * log(M4m$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E4m <- 41015*log(4)
##Relative Entropy
Entro4m <- 1-(nume.E4m/deno.E4m)
Entro4m




M5m <- poLCA(f1, datos_obesidad_men, nclass = 5, verbose = F, graphs = T, na.rm = T, maxiter = 20000, nrep = 3)
M5m
poLCA.entropy(M5m)

##RELATIVE ENTROPY
##Numerator:
nume.E5m <- -sum(M5m$posterior * log(M5m$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E5m <- 41015*log(5)
##Relative Entropy
Entro5m <- 1-(nume.E5m/deno.E5m)
Entro5m




M6m <- poLCA(f1, datos_obesidad_men, nclass = 6, verbose = F, graphs = T, na.rm = T, maxiter = 20000, nrep = 3)
M6m
poLCA.entropy(M6m)

##RELATIVE ENTROPY
##Numerator:
nume.E6m <- -sum(M6m$posterior * log(M6m$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E6m <- 41015*log(6)
##Relative Entropy
Entro6m <- 1-(nume.E6m/deno.E6m)
Entro6m



M7m <- poLCA(f1, datos_obesidad_men, nclass = 7, verbose = F, graphs = T, na.rm = T, maxiter = 40000, nrep = 2)
M7m
poLCA.entropy(M7m)

##RELATIVE ENTROPY
##Numerator:
nume.E7m <- -sum(M7m$posterior * log(M7m$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E7m <- 41015*log(7)
##Relative Entropy
Entro7m <- 1-(nume.E7m/deno.E7m)
Entro7m




#### Adjudicando a cada sujeto su clase del latent class analysis ####
library(writexl)
# Mujeres
datos_obesidad_women <- datos_obesidad_women[complete.cases(datos_obesidad_women$Ejercicio_est_obesidad_dic),]
datos_obesidad_women <- datos_obesidad_women[complete.cases(datos_obesidad_women$Fruta_est_obesidad_dic),]
datos_obesidad_women <- datos_obesidad_women[complete.cases(datos_obesidad_women$verduras_est_obesidad_dic),]
datos_obesidad_women <- datos_obesidad_women[complete.cases(datos_obesidad_women$Dulces_est_obesidad_dic),]
datos_obesidad_women <- datos_obesidad_women[complete.cases(datos_obesidad_women$Refrescos_est_obesidad_dic),]
datos_obesidad_women <- datos_obesidad_women[complete.cases(datos_obesidad_women$Pescado_est_obesidad_dic),]
datos_obesidad_women <- datos_obesidad_women[complete.cases(datos_obesidad_women$Cane_est_obesidad_dic),]

datos_obesidad_women$predclass <- M6w$predclass
datos_obesidad_women$predclass  <- as.factor(datos_obesidad_women$predclass)
table(datos_obesidad_women$predclass);prop.table(table(datos_obesidad_women$predclass))

write_xlsx(datos_obesidad_women, 'mujeres_obesidad_lca.xlsx')


# Hombres
datos_obesidad_men <- datos_obesidad_men[complete.cases(datos_obesidad_men$Ejercicio_est_obesidad_dic),]
datos_obesidad_men <- datos_obesidad_men[complete.cases(datos_obesidad_men$Fruta_est_obesidad_dic),]
datos_obesidad_men <- datos_obesidad_men[complete.cases(datos_obesidad_men$verduras_est_obesidad_dic),]
datos_obesidad_men <- datos_obesidad_men[complete.cases(datos_obesidad_men$Dulces_est_obesidad_dic),]
datos_obesidad_men <- datos_obesidad_men[complete.cases(datos_obesidad_men$Refrescos_est_obesidad_dic),]
datos_obesidad_men <- datos_obesidad_men[complete.cases(datos_obesidad_men$Pescado_est_obesidad_dic),]
datos_obesidad_men <- datos_obesidad_men[complete.cases(datos_obesidad_men$Cane_est_obesidad_dic),]

datos_obesidad_men$predclass <- M6m$predclass
datos_obesidad_men$predclass  <- as.factor(datos_obesidad_men$predclass)
table(datos_obesidad_men$predclass);prop.table(table(datos_obesidad_men$predclass))

write_xlsx(datos_obesidad_men, 'hombres_obesidad_lca.xlsx')


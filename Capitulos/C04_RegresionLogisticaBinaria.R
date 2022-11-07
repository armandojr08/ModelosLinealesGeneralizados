
# REGRESION LOGISTICA BINARIA ---------------------------------------------


# Aplicacion 1 ------------------------------------------------------------


# Datos
Datos <- data.frame(cirro=c(1,1,0,0),hepa=c(1,0,1,0),n=c(51,149,9,191))
Datos


# Estimacion de los coeficientes de regresion
mod_cirrosis <- glm(cirro ~ hepa, weights = n, family = binomial(), data = Datos)
summary(mod_cirrosis)
coef(mod_cirrosis)

"Modelo logistico
logit(pi) = -0.2483 + 1.9829*X

- La probabilidad que un paciente padezca cirrosis disminuye cuando
  no ha tenido hepatitis (X=0)
- La probabilidad que el paciente padezca cirrosis aumenta cuando
  el paciente presento cirrosis que cuando no la presento (X=0)"


# Prueba de bondad de ajuste - prueba de devianza

"H0: El modelo logistico se ajusta a los datos"
"H1: El modelo logistico no se ajusta a los datos"

summary(mod_cirrosis)

" alfa = 0.05 "
alfa <- 0.05
chi_tab <- qchisq(1 - alfa, mod_cirrosis$df.residual)
p_valor <- 1 - pchisq(mod_cirrosis$deviance, mod_cirrosis$df.residual)
p_valor
"Se rechaza H0"

# Coeficiente de determinacion  de Mc Fadden

R2 <- (1 - mod_cirrosis$deviance/mod_cirrosis$null.deviance)*100
R2

"El modelo logistico binario explica en un 6.8 % la prediccion de un
paciente con cirrosis en funcion de kla hepatitis."

# Prueba de significacion de los coeficientes de regresion
summary(mod_cirrosis)

"A un nivel de significancia de 5% se puede concluir que la
presencia de hepatitis permite explicar la existencia de
cirrosis. Por lo tanto, la variable es signifactiva."


# Odds ratio

exp(coef(mod_cirrosis))
OR(Hepatitis) = exp(1.983) = 7.2639821 

"El riesgo de padecer cirrosis es 7.27 veces mas probable
para pacientes que tuvieron hepatitis que para aquellos 
que no la tuvieron"


# IC para coeficientes y OR

"IC para Beta 0"
summary(mod_cirrosis)
LI_bo <- -0.2483  - qnorm(0.975)*0.1093
LS_bo <- -0.2483  + qnorm(0.975)*0.1093
c(LI_bo, LS_bo)

"IC para Beta 1"
LI_b1 <- 1.9829  - qnorm(0.975)*0.3777
LS_b1 <- 1.9829  + qnorm(0.975)*0.3777
c(LI_b1, LS_b1)

"IC para OR de beta 1"
exp(c(LI_b1, LS_b1))


# Estimacion de valores predecidos

"La probabilidad de que un paciente padezca cirrosis si
se sabe que ha presentado hepatitis es: P(Y=1/X=1)"

Xo <- c(1,1)
P_Xo <- sum(Xo * coef(mod_cirrosis) )
P_Xo
P1 <- exp(P_Xo) / (1+exp(P_Xo))
P1


# Aplicacion 2 ------------------------------------------------------------

# Datos

Edad <- c(25,32,37,42,47,52,57,65) 
CI <- c(1,2,3,5,6,5,13,8) 
n <- c(10,15,12,15,13,8,17,10)
Datos2 <- data.frame(CI=CI,Edad=Edad,n=n) 
Datos2

# Estimacion de los coeficientes de regresion

mod_cardio <- glm(cbind(CI, n - CI) ~ Edad, family = binomial)
summary(mod_cardio)

"Modelo logistico
logit(pi) = -5.029 + 0.10471*X

La probabilidad de fallecer por cardiopatia disminiuye cuando
la edad es 0.
La probabilidad de que un paciente diabetico fallezca por CI 
aumenta cuando aumenta la Edad
"


# Aplicacion 3 ------------------------------------------------------------

# Datos
agro <- read.csv("./Datos/DatosRegresionLogistica02.csv", sep = ";")
# Convirtiendo a factor las variables categoricas
agro$X2_Fertiliza <- as.factor(agro$X2_Fertiliza)
agro$X3_Tecnologia <- as.factor(agro$X3_Tecnologia)
agro$Y_Produccion <- as.factor(agro$Y_Produccion)
attach(agro)
str(agro)

# Estimacion de los coeficientes del Modelo
mod_agro <- glm(Y_Produccion ~ ., family = binomial, data = agro)
summary(mod_agro)

# prueba de significacion del modelo
alfa <- 0.05
chi_tab <- qchisq(1-alfa, mod_agro$df.residual); chi_tab
p_valor <- 1-pchisq(mod_agro$deviance, mod_agro$df.residual); p_valor


# Coeficiente de determinacion
R2 <- (1-mod_agro$deviance/mod_agro$null.deviance)*100
R2



# INFERENCIA EN LOS MLG ---------------------------------------------------


# Ejercicio 1 -------------------------------------------------------------

# Entrada de datos
y<-c(1,3,7,15,9,17,5,13)
x<-c(2,4,6,9,7,12,2,10)

# Estimación de los coeficientes y la ecuación estimada
Modelo_31<-glm(y~x,family=poisson(log))
summary(Modelo_31)

# Cálculo de la función de VM del modelo saturado: l(bmax)
l_bmax=sum(y*log(y)-y-log(factorial(y))); l_bmax 

# Cálculo de la función de VM del modelo de interés: l(b) 
yest<-exp(coef(Modelo_31)[1]+coef(Modelo_31)[2]*x); yest
l_b=sum(y*log(yest)-yest-log(factorial(y))); l_b 

# Cálculo de la Deviance
D=2*(l_bmax-l_b); D 
D=2*sum(y*log(y/yest)); D # (Usando la fórmula) 

# Cálculo de la Desviance Null o Modelo mínimo
D_Null=2*sum(y*log(y/mean(y))); D_Null 
glm(y~1,family=poisson(log)) 

# Cálculo de AIC
AIC=-2*sum(y*log(yest)-yest-log(factorial(y)))+2*2; AIC

# Cálculo de los residuals de Pearson
Ri=(y-yest)/sqrt(yest); Ri 
Res_Per<-residuals(Modelo_31,type = "pearson"); Res_Per 

# Cálculo de los residuals de Desviance 
Di=y*log(y/yest); Di 
Res_Des<-residuals(Modelo_31,type="deviance") ;Res_Des 

# Cálculo del Coeficiente de determinación
Desv_Residual=2*sum(y*log(y/yest)); Desv_Residual 
Desv_Nulo=2*sum(y*log(y/mean(y))); Desv_Nulo 
R2=100*(1-Desv_Residual/Desv_Nulo); R2 
R2= (1-Modelo_31$deviance/Modelo_31$null.deviance)*100; R2 



# Ejercicio 2 -------------------------------------------------------------


# Entrada de datos
y<-c(55,52,57,55,50,50) 
n<-c(102,99,108,76,81,90) 
yy<-cbind(y,n-y) 
fuerza<-c(40,150,350,40,150,350) 
factor<-c(0,0,0,1,1,1) 
lfuerza<-log(fuerza);  fxf<-lfuerza*factor 

# Modelo 1. Covariable: X2=log(Fuerza Centrífuga)
Modelo1<-glm(yy~lfuerza,family=binomial(link=logit))
summary(Modelo1) 

# Prueba estadistica de la covariable
# Cálculo de la desviance 
Pi=fitted(Modelo1); Yest=n*Pi 
D = 2*sum(y*log(y/Yest)+(n-y)*log((n-y)/(n-Yest))); D
# Ho: El Modelo1 de regresión logístico se ajusta a los datos 
# H1: El Modelo1 de regresión logístico No se ajusta a los datos  
Alfa=0.05
Chi_Tab=qchisq(1-Alfa,Modelo1$df.residual); Chi_Tab 
p_valor=1-pchisq(Modelo1$deviance,Modelo1$df.residual); p_valor 

# Cálculo del Coeficiente de determinación  (Pseudo R2)
Desv_Residual=2*sum(y*log(y/Yest)+(n-y)*log((n-y)/(n-Yest))); Desv_Residual 
R2= (1-Modelo1$deviance/Modelo1$null.deviance)*100; R2 

# Prueba de significacion del modelo 2
# Modelo 2. Facto: X1=Almacenamiento y Covariable: X2= log(Fuerza Cent)
Modelo2<- glm(yy~lfuerza+factor,family=binomial(link=logit))
summary(Modelo2) 
Alfa=0.05 
Chi_Tab=qchisq(1-Alfa,Modelo2$df.residual); Chi_Tab
p_valor=1-pchisq(Modelo2$deviance,Modelo2$df.residual); p_valor  

# Prueba de significacion del modelo 3
Modelo3<-glm(yy~lfuerza+factor+fxf,family=binomial(link=logit)) 
summary(Modelo3) 
Alfa=0.05 
Chi_Tab=qchisq(1-Alfa,Modelo3$df.residual); Chi_Tab 
p_valor=1-pchisq(Modelo3$deviance,Modelo3$df.residual); p_valor 

# Prueba estadistica de la interaccion
anova(Modelo2, Modelo3, test= "Chisq") 
# Modelo2 se ajusta mejor a los datos que Modelo3

# Intervalos de confianza del 95% para el Modelo 2
NC=0.95 
confint.default(Modelo2, level=NC) 

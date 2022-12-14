
# ESTIMACION EN LOS MLG ---------------------------------------------------


# Ejercicio 1--------------------------------------------------------------

# Entrada de datos
y = c(1,3,7,15,9,17,5,13)
X = cbind(X0=c(1,1,1,1,1,1,1,1), X1=c(2,4,6,9,7,12,2,10)) 
n = dim(X)[1]; p = dim(X)[2] 
W = matrix(0, nrow=n, ncol=n) 

#  Valores iniciales para b
b=c(0.8, 0.1)

# Inicio de las iteraciones
m=5
for(i in 0:m){
  cat("Iteracion = ",i," Coeficientes estimados:   ", b,"\n")
  #  Estimacion MV con el algoritmo Scoring 
  Xb=b[1]+b[2]*X[,2]
  wii=exp(Xb); diag(W)=wii 
  z=Xb+y/exp(Xb)-1
  b=solve(t(X)%*%W%*%X)%*%(t(X)%*%W%*%z)
}

#  Vector de coeficientes estimados 
b

#  Ecuaciones normales 
XWX = t(X)%*%W%*%X; XWX # matriz de informacion
XWz = t(X)%*%W%*%z; XWz

# Intervalos de confianza de los coeficientes
Cov_B = solve(t(X)%*%W%*%X) # inversa de ka nmatriz de informacion
Cov_B
NC=0.95 # nivel de confianza
for(i in 1:p) {
  LI=b[i] - qnorm(1-((1-NC)/2))*sqrt(Cov_B[i,i])
  LS=b[i] + qnorm(1-((1-NC)/2))*sqrt(Cov_B[i,i])
  cat("IC Coeficiente :  b",i-1," LI = ",LI, "   LS = ",LS,"\n") 
} 



# Ejercicio 2 -------------------------------------------------------------


# Entrada de datos
y=c(1,2,3,5,6,5,13,8)
n=c(10,15,12,15,13,8,17,10) 
yy=cbind(y,n-y)
X=cbind(X0=c(1,1,1,1,1,1,1,1), X1=c(25,32,37,42,47,52,57,65))
nn=dim(X)[1]; p=dim(X)[2]
W=matrix(0,nrow=nn,ncol=nn)

#  Valores iniciales 
b=c(-3.0, 0.1) 

# Inicio de las iteraciones
m=5
for(i in 0:m) { 
  cat("Iteracion = ",i," Coeficientes estimados:   ",b,"\n") 
    # Estimacion MV con el algoritmo Scoring
    Xb=b[1]+b[2]*X[,2]
    Pi=exp(Xb)/(1+exp(Xb))
    wii=n*Pi*(1-Pi); diag(W)=wii 
    z=Xb+(y-n*Pi)/(n*Pi*(1-Pi))
    b=solve(t(X)%*%W%*%X)%*%(t(X)%*%W%*%z)
}

# Vector de coeficientes estimados
b 

# Ecuaciones normales
XWX=t(X)%*%W%*%X; XWX # matriz de informacion
XWz=t(X)%*%W%*%z; XWz 


# Intervalos de confianza de los coeficientes
Cov_B=solve(t(X)%*%W%*%X); Cov_B 
NC=0.95
for(i in 1:p) {
  LI=b[i] - qnorm(1-((1-NC)/2))*sqrt(Cov_B[i,i])
  LS=b[i] + qnorm(1-((1-NC)/2))*sqrt(Cov_B[i,i]) 
  cat("IC Coeficiente :  b",i-1," LI = ",LI, "   LS = ",LS,"\n")
} 



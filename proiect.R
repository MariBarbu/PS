#EX1
#a)
#citim n si m
n <- readline(prompt="Introduceti n: ")
n <- as.integer(n)
m <- readline(prompt="Introduceti m: ")
m <- as.integer(m)
#citim variabila aleatorie X
vals <- c()
probabs <- c()

for(val in c(1:n))
{
  x <- readline(prompt="Introduceti x: ")
  x <- as.integer(x)
  p1 <- readline(prompt="Introduceti numarator p: ")
  p1 <- as.integer(p1)
  p2 <- readline(prompt="Introduceti numitor p: ")
  p2 <- as.integer(p2)
  p <- fractions(p1/p2)
  vals <- append(vals,x)
  probabs <- append(probabs,p)
}

X <- RV(vals,probabs)
X_prob <- fractions(probs(X))
#citim variabila aleatoare Y
vals <- c()
probabs <- c()

for(val in c(1:m))
{
  x <- readline(prompt="Introduceti x: ")
  x <- as.integer(x)
  p1 <- readline(prompt="Introduceti numarator p: ")
  p1 <- as.integer(p1)
  p2 <- readline(prompt="Introduceti numitor p: ")
  p2 <- as.integer(p2)
  p <- fractions(p1/p2)
  vals <- append(vals,x)
  probabs <- append(probabs,p)
}
Y <- RV(vals,probabs)
Y_prob <- fractions(probs(Y))

#a) cream repartitia comuna incompleta

frepcomgen <- function(x,y)
{
  a <- matrix(0,nrow=x,ncol=y)
  for (val in c(1:x-1))
  {
    for(val1 in c(1:y))
    { 
      a[val,val1] <- fractions(X_prob[val]*Y_prob[val1])
    }
  }
  return (fractions(a))
}
rep_com <- frepcomgen(n,m)

#b) completam repartitia comuna
fcomplrepcom <- function(matr1)
{
  matr <- matrix(0,nrow=n,ncol=m)
  matr <- matr1
  for(val in c(1:ncol(matr)))
  {
    sum <- 0
    for(val1 in c(1:(nrow(matr)-1)))
    {
      sum <- sum+matr[val1,val]
    }
    cnt <- (Y_prob[[val]])-sum
    matr[nrow(matr),val] <- cnt
  }
  return(matr)
}
rep_com <- fcomplrepcom(rep_com)

#c)
#1)
#Cov(2X+3,4Y-11)=E((2X+3)*(4Y-11))-E(2X+3)*E(4Y-11)
#Cov(2X+3,4Y-11)=2*4*Cov(X,Y)+Cov(2X,-11)+Cov(3,4Y)+Cov(3,-11)
#Cov(2X+3,4Y-11)=8Cov(X,Y)
#Cov(X,Y)=E(X*Y)-E(X)*E(Y)
#X*Y
inmultire <- function(X,Y)
{
  vals <- c()
  probabs <- c()
  for(val1 in c(1:n))
  {
    for(val2 in c(1:m))
    {
      cnt <- X[val1]*Y[val2]
      vals <- append(vals,cnt)
      buff <- rep_com[val1,val2]
      probabs <- append(probabs,buff)
    }
  }
  XY <- RV(vals,probabs)
  return(XY)
}
XY <- inmultire(X,Y)
XY_prob <- fractions(probs(XY))
#functia pt medie
media <- function(X,X_prob)
{
  sum <- 0
  for(val in c(1:length(X_prob)))
  {
    cnt <- (X_prob[[val]])*(X[val])
    sum <- sum + cnt
  }
  return(fractions(sum))
}
EX <- media(X,X_prob)
EY <- media(Y,Y_prob)
EXY <- media(XY,XY_prob)

#Cov(X,Y)=E(X*Y)-E(X)*E(Y)
Cv <- EXY-(EX*EY)
#Cov(2X+3,4Y-11)=8Cov(X,Y)
Cov <- 8*Cv

#2)P(0<X<8|Y>3)
X1 <- RV(c(4,5),c(1/2,1/2))
X2 <- RV(c(1,2,3,4),c(1/4,1/4,1/4,1/4))
P1 <- P(((X1>2)%AND%(X1<8))%AND%(X2>3))
P2 <- P(X2>3)
P_final=P1/P2

#3) P(X>2,Y<7)
P((X1>2)%AND%(X2<7))

#d)
fverind <- function(X,Y)
{
  if(independent(X,Y)==TRUE)
    print("Variabilele sunt independente!")
  else
    print("Variabilele sunt dependente!")
}
fverind(X,Y)
#coeficientul de corelatie Pearson : RO(X,Y)=COV(X,Y)/SQRT(Var(X)*Var(Y))
fvernecor <- function(X,Y)
{
  numitor <- sqrt(V(X)*V(Y))
  pearson <- Cov/numitor
  
  if(pearson==0)
    print("Variabilele sunt necorelate!")
  if((abs(pearson)>0) %AND% (abs(pearson)<0.25))
    print("Variabilele sunt slab corelate!")
  if((abs(pearson)>=0.25) %AND% (abs(pearson)<=0.75))
    print("Variabilele sunt corelate!")
  if((abs(pearson)>0.75) %AND% (abs(pearson)<=1))
    print("Variabilele sunt puternic corelate!")
  if(abs(pearson)>1)
    print("Nu avem suficiente informatii!")
}
fvernecor(X,Y)

##########################################################

#EX2

print("Introduceti  densitatile de probabilitate a doua variabile aleatoare continue independente si una dintre urmatoarele optiuni!")
x <- readline(prompt="Introduceti valoarea lui x pentru prima densitate de probabilitate : ")
x <- as.numeric(x)

x1 <- readline(prompt="Introduceti valoarea lui x pemntru a doua densitate de probabilitate : ")
x1 <- as.numeric(x1)

x2 <- readline(prompt="Introduceti valoarea lui y pentru formula de convolutie : ")
x2 <- as.numeric(x2)

optiune <- readline(prompt="Optiuni :
1 pentru suma variabilelor aleatoare
2 pentru diferenta variabilelor aleatoare
3 pentru media si disperia celor 2 variabile aleatoare
4 pentru repezentarea grafica, in acelasi reper, a celor 2 densitati, cu culori diferite")
optiune <- as.integer(optiune)

ex2 <- function(x1,x, x2, optiune)
{

  dens11 <- function (x, x2)
  {
    if(0 < x && x < 2) return (3/8*(4*(x-x2) - 2*((x-x2)**2)))
    else return (0)
    
  }
  
  dens1 <- function (x)
  {
    if(0 < x && x < 2) return (3/8*(4*(x) - 2*((x)**2)))
    else return (0)
    
  }
  
  dens2 <- function (x)
  {
    if(0 <= x && x <= 1) return ((exp(1)/(exp(2)-1))*(exp(-x) + exp(x)))
    else return (0)
    
  }
  
  suma <- function(a)
  {
    
    rez <- integrate (dens11, 0, 2, x2 = a)$value * integrate(dens2, 0, 1)$value
    return (rez)
    
  }
  
  reprezentare <- function()
  {
    valori1 <- runif(10, 0, 2)
    valori2 <- runif(10, 0, 1)
    
    for(val in c(1:10))
    {
      x <- dens1(valori1[val])
      valori1[val] <- x
      
      y <- dens2(valori2[val])
      valori2[val] <- y
      
    }
    
    
    
    plot(1:10, valori1, col="magenta",ylim=c(0,2), type = "o")
    lines(1:10, valori2, col="green", ylim=c(0,2), type = "o")
    
  }
  
  
  if(optiune==1)
  {
    
   print(suma(0.1))
    
  }
  else if(optiune==2)
  {
    
  }
  else if(optiune==3)
  {
   print(mean(c(dens1(x), dens2(x1))))
  print(var(c(dens1(x), dens2(x1))))

  }
  else if(optiune==4)
  {
    reprezentare()
  }
  else
  {
    print("Introduceti o optiune valida!")
  }
  
}


ex2(1.5, 0.5, 0.1 , 4)

##########################################################

#EX3
time <- format(Sys.time(), "%H:%M:%S")
#1) 
genx1 <- function()
{
  nr <- 3
  x1 <- NULL
  while(nr>0)
  {
    #calculam t1
    ore <- format(Sys.time(), "%H")
    ore <- as.integer(ore)
    minute <- format(Sys.time(), "%M")
    minute <- as.integer(minute)
    secunde <- format(Sys.time(), "%S")
    secunde <- as.integer(secunde)
    t1 <- minute*100+secunde
    t <- t1%%12
    if(t==0)
    {
      x1 <- rnorm(1,minute,secunde)
      nr <- 0
    }
    else if(t==1)
    {
      y1 <- runif(1,0,1)
      x1 <- rpois(1,minute+y1)
      nr <- 0
    }
    else if(t==5)
    {
      x1 <- rexp(1,ore)
      nr <- 0
    }
    else if(t==7)
    {
      y1 <- runif(1,0,5)
      x1 <- rbinom(1,ore,1/minute+y1)
      nr <- 0
    }
    else if(t==8)
    {
      x1 <- runif(1,5,12)
      nr <- 0
    }
    else if(t==9)
    {
      #nn=nr observatii, n=nr bile negre, m=nr bile albe,k=nr bile extrase
      y1 <- rhyper(1,21,15,10)
      #n=nr observatii,shape>=0,rate,scale>0(1/rate)
      x1 <- rgamma(1,2,1/5)
      x1 <- x1-y1
      nr <- 0
    }
    #daca nu a intrat pe niciunul, scadem nr
    else
    {
      nr <- nr-1
    }
  }
  #daca s-a executat de 3 ori
  if(is.null(x1)) 
  {
    x1 <- rnorm(1,0,1)
  }
  return(x1)
}

#2)
#xn=a*xn-1+b 
#generam sirul
gensir <- function(n)
{
  x1 <- genx1()
  sir <- c()
  a <- rexp(1,2)
  b <- rnorm(1,5,1)
  sir <- append(sir,x1)
  n <- n-1
  while(n)
  {
    x <- a*x1+b
    sir <- append(sir,x)
    x1 <- x
    n <- n-1
  }
  return(sir)
}

generator <- function(n)
{
  sir <- gensir(n)
  hist(sir,main="Histograma numerelor aleatoare generate",col="magenta",border="green",xlab="Valori",breaks=3)
  print(sir)
}
generator(10)


##########################################################

#EX4
#OrchardSprays

set <- OrchardSprays

#media

mean(set$decrease)

#varianta

var(set$decrease)

#quartile

q1 <- quantile(set$decrease,1/4)
q2 <- quantile(set$decrease,2/4)
q3 <- quantile(set$decrease,3/4)

#boxplot

boxplot(c(set[1]),col="darkgreen")

boxplot(set$decrease~set$treatment ,col="darkgreen")

#interpretari

hist(set$decrease)

#Pentru tratamentul A, fiind foarte putine experimente realizate, 
#valorile obtinute in decrease sunt insuficiente pentru a obtine un rezultat conclusiv.
#Asadar, putem neglija aceasta categorie.


#Pe de alta parte, la o prima analizare a graficului, observam, 
#comparand categoriile de tratament E si F, ca media de descrestere are un 
#rezultat mai bun in categora F fata de categoria E, insa limitele inferioare ale acestora 
#contrazic acest fapt.


#Observam prezenta a trei outlieri, ceea ce inseamna ca experimentele au avut o rata de succes ridicata.


#Observam ca mediile corespunzatoare rezultatelor sunt in ordine crescatoare de la A la H.







## Problem 1 on paper

# Neither max nor min: 0, if sign doesnt change around 0, it is not a minimizer / maximizer


fa <- function(x){
  ((x^2)+1)/(2*x)
}
fb <- function(x){
  x*(exp(x^2))
}
fc <- function(x){
  -3*x^4 +4*x^3 +7
}
ranges <- c(0.5,sqrt(5))

?uniroot


fc1 <- function(x){
  x^3+x^2
}

polyroot(c(0,0,1,1))


## Problem 2

cur <- function(f,ranges){
  curve(f,ranges[1],ranges[2])
}

cur(fa,ranges)
cur(fb,ranges)
cur(fc,ranges)


opt <- function(f,ranges){
  tol <- 1e-20
  ret <- c(optimize(f,ranges,maximum=FALSE,tol=tol))
  ret <- c(ret, optimize(f,ranges,maximum=TRUE,tol=tol))
  names(ret) <- c("minimum","minval","maximum","maxval")
  ret
}

opt(fa,ranges)
opt(fb,ranges)
opt(fc,ranges)

## Problem 3 on paper

## TODO: Problem 4 on paper
as <- polyroot(c(1,6,8))
bs <- polyroot(c(1,3,-4))

zaa <- function(a) 8 - (1/(a^2))
zbb <- function(b) - 4 -(1/(b^2))
zaa(as[1])
zaa(as[2])
zbb(bs[1])
zbb(bs[2])

## Problem 5


z <- function(a,b){
  print(paste(a,b))
  (4*(a^2)) -(2*(b^2)) +(6*a) +(3*b) +log(a*b)
}

z2 <- function(x){
  a <- x[1]
  b <- x[2]
  (4*(a^2)) -(2*(b^2)) +(6*a) +(3*b) +log(a*b)
}

aseq <- seq(-0.3,-0.2,0.001)
bseq <- seq(-0.3,-0.2,0.001)

arep <- rep(aseq, each=100)
brep <- rep(bseq, times=100)

?matrix

evalWithMatrix <- function(z,rangeA,rangeB,interval){
  #Extract parameter values for range
  aBegin <- rangeA[1]
  aEnd <- rangeA[2]
  bBegin <- rangeB[1]
  bEnd <- rangeB[2]
  
  #Generate sequences in range with specified interval
  aSeq <- seq(aBegin,aEnd,interval)
  bSeq <- seq(bBegin,bEnd,interval)
  
  #Set up for loop
  ret <- matrix(nrow=length(aSeq),ncol=length(bSeq))
  ai = 1
  bi = 1
  
  #Fill matrix
  for(a in aSeq){
    for(b in bSeq){
      ret[ai,bi] <- z(a,b)
      bi <- bi + 1
    }
    ai <- ai + 1
    bi <- 1
  }
  
  #Draw contour plot
  contour(aSeq,bSeq,ret)
  
  ret
}

eval2 <- function(z,rangeA,rangeB,lout){
  aSeq <- seq(rangeA[1],rangeA[2],length.out = lout)
  bSeq <- seq(rangeB[1],rangeB[2],length.out = lout)
  mesh <- expand.grid(aSeq,bSeq)
  
  z <- apply(mesh,1, z2)
  z <- matrix(z, ncol = lout, byrow = FALSE)
  contour(aSeq, bSeq, z, nlevels = 20)
  z
}

#e1
interval <- 0.001
rangeA <- c(-0.3,-0.2)
rangeB <- c(-0.3,-0.2)
zVal <- evalWithMatrix(z,rangeA,rangeB,0.001)
zVal2 <- eval2(z,rangeA,rangeB,100)


#e2
rangeA <- c(-0.5,-0.4)
rangeB <- c(-0.4,-0.2)
zVal2 <- evalWithMatrix(z,rangeA,rangeB,0.001)
zVal2 <- eval2(z,rangeA,rangeB,100)

#testing
rangeA <- c(-1,1)
rangeB <- c(-1,1)
zVal2 <- evalWithMatrix(z,rangeA,rangeB,0.01)

rangeA <- c(-1,0)
rangeB <- c(-1,0)
zVal2 <- evalWithMatrix(z,rangeA,rangeB,0.01)


?optim

optim(c(-0.3,-0.2), z2, lower = c(-0.3,-0.3), upper = c(-0.2,-0.2))
optim(c(-0.3,-0.2), z2, lower = c(-0.3,-0.3), upper = c(-0.2,-0.2), control = list(fnscale = -1))

## Problem 6 on paper

## Problem 7

fun1 <- function(x) {
  a <- x[1] ; b <- x[2]
  -(a^2+b^2-2)*(a^2+b^2-1)*(a^2+b^2)*(a^2+b^2+1)*(a^2+b^2+2)*(2-sin(a^2-b^2)*cos(b-exp(b)))
}

optim(c(-1.3, -1.4),fun1,control=list(fnscale = -1, factr = 1e10), 
      lower = c(-1.5,-1.5), upper = c(1.5,1.5) ,method= "L-BFGS-B")
optim(c(-1.3, 0),fun1, control=list(fnscale = -1), 
      lower = c(-1.5,-1.5), upper = c(1.5,1.5) ,method= "L-BFGS-B")
optim(c(0,0),fun1, control=list(fnscale = -1), 
      lower = c(-1.5,-1.5), upper = c(1.5,1.5) ,method= "L-BFGS-B")
optim(c(1.2,1.2),fun1, control=list(fnscale = -1), 
      lower = c(-1.5,-1.5), upper = c(1.5,1.5) ,method= "L-BFGS-B")
#b
{
  m <- c()
  for(i in 1:20){
    r<-runif(2, -1.5, 1.5)
    m<-c(m,optim(c(r[1],r[2]), lower = c(-1.5, -1.5), upper = c(1.5,1.5),fun1, control=list(fnscale = -1))$value)
  }
  print(max(m))
}

## Problem 8 on paper
#a
f2<-function(a){x <- a[1]
y <- a[2]
(1-x)^2 + 100*(y-x^2)^2
}

granularity <- 200
xmesh <- seq(-0.5, 0.5, length.out = granularity)
ymesh <- seq(-0.2, 0.4, length.out = granularity)
mesh <- expand.grid(xmesh, ymesh)
z <- apply(mesh, 1, f2)
z <- matrix(z, ncol = granularity, byrow = FALSE)
contour(xmesh, ymesh, z, nlevels = 50, col = 1)

optim(c(-1,2),f2)  

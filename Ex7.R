#1
pHat <- 182/200
p0 <- 0.95

sigmaPHat <- sqrt((p0*(1-p0))/200)

z <- (pHat - p0)/sigmaPHat
z

pnorm(z)

z0.5 <- qnorm(0.95)
z0.1 <- qnorm(0.99)

z < -z0.5
z < -z0.1
# H0: p0 =(>=) 0.95
# Ha: pa < 0.95
# Reject H0 at both, as there is sufficient evidence at 0.05 and 0.01 level of significance to indicate that


#2
m1 <- c(3.4, 4.5, 6.3, 3.5, 6.3, 5.7, 4, 4.9, 7.2, 3.9, 6.2, 7)
m2 <- c(6.1, 5.7, 8.4, 7.4, 8.3, 5.3, 6.6, 7.4, 4.1, 4.9, 4.4, 5.5)

sdM1 <- sd(m1)
sdM2 <- sd(m2)
nM1 <- length(m1)
nM2 <- length(m2)
sdPooled2 <- ((nM1-1)*sdM1^2+(nM2-1)*sdM2^2)/(nM1+nM2-2)

t <- ((mean(m1)-mean(m2))-0)/sqrt(sdPooled2*((1/nM1)+(1/nM2)))
t
tA <- qt(0.95,nM1+nM2-2)
t < -tA
t.test(x=m2,y=m1,alternative="greater")

#we do not reject the null hypothesis


#3

m_1 <- c(5.6, 3.5, 5.1, 4.8, 6.9, 6.8, 4, 4.8, 7.7, 7.9, 6.2, 5.9)
m_2 <- c(6.6, 4.4, 5.9, 5.6, 8.4, 7.6, 4.8, 5.4, 8.7, 8.8, 7.1, 6.8)
m_diff <- m_2 - m_1
m_diff

t_diff <- (mean(m_diff) - 0) /(sd(m_diff)/sqrt(length(m_diff)))
t_diff

t_A <- qt(0.95,length(m_diff - 1))

t_diff > t_A

t.test(x = m_2, y = m_1, paired = T, alternative = "greater")

#we reject the null hypothesis

#5


proportion.test <- function(successes, failures, alt, p0){
  
  pHat <- successes/(successes + failures)
  
  sigmaPHat <- sqrt((p0*(1-p0))/(successes + failures))
  
  z <- (pHat - p0)/sigmaPHat
  z
  if(alt == "greater"){
    return(pnorm(z,lower.tail = F))
  }else if(alt == "less"){
    return(pnorm(z,lower.tail = T))
  }else if(alt == "two.sided"){
    if(z>0){
      return(pnorm(z,lower.tail = F)*2)
    }else{
      return(pnorm(z,lower.tail = T)*2)
    }
  }else{
    stop("invalid alternative!")
  }

}

p.values <- replicate(1000, {
  successes <- 50+sample.int(100, 1)
  failures <- 50+sample.int(100, 1)
  alt <- sample(c("two.sided", "greater", "less"), 1)
  p <- runif(1, 0.3, 0.7)
  c(proportion.test(successes, failures, alt, p),
    prop.test(x = as.table(c(successes, failures)),
              p = p, alternative = alt,
              correct = FALSE)$p.value)
})
isTRUE(all.equal(p.values[1,], p.values[2,]))


#6


mean.test <- function(x,alternative, mu, conf.level, sigma.x){
  #construct hypothesis
  altHypoFunc <- function(alternative, mu) {
    altTxt <- ""
    if(alternative == "two.sided"){
      altTxt <- "not equal to"
    }else if(alternative == "greater" || alternative == "less"){
      altTxt <- paste(alternative,"than")
    }else{
      stop("invalid alternative!")
    }
    altHypo <- paste("alternative hypothesis: true mean is",altTxt,mu)
  }
  altHypo <- altHypoFunc(alternative, mu)
  test <- ""
  statistic <- 0
  pval <- 0
  confintervaltxt <- paste(conf.level*100, "percent confidence interval:")
  conf.int <- c()
  pTxt <- "p-value ="
  
  #get p value from function, val, df and alt
  getVal <- function(alternative, val, func, df = NULL){
    if(!is.null(df)){
      if(alternative == "greater"){
        return(func(val,lower.tail = F,df=df))
      }else if(alternative == "less"){
        return(func(val,lower.tail = T,df=df))
      }else if(alternative == "two.sided"){
        if(val>0){
          return(func(val,lower.tail = F,df=df)*2)
        }else{
          return(func(val,lower.tail = T,df=df)*2)
        }
      }
    }
    if(alternative == "greater"){
      return(func(val,lower.tail = F))
    }else if(alternative == "less"){
      return(func(val,lower.tail = T))
    }else if(alternative == "two.sided"){
      if(val>0){
        return(func(val,lower.tail = F)*2)
      }else{
        return(func(val,lower.tail = T)*2)
      }
    }
  }
  
  #check if sd is known
  if(is.null(sigma.x)||length(x)<=30){
    sigma.x <-sd(x) #estimate standard deviation
  }
  
  #get limit for a conf level
  getLimitConfLvl <- function(alternative, conf.level, mu, sigma.x, df = NULL,n){
    alpha <- 1-conf.level
    if(alternative == "two.sided"){
      if(is.null(df)){
        dist <- abs(getVal("less", 1-alpha/2, qnorm)) * (sigma.x / sqrt(n))
      }else{
        dist <- abs(getVal("less", 1-alpha/2, qt, df)) * (sigma.x / sqrt(n))
      }
      return(c(mu - dist, mu + dist))
    }
    if(is.null(df)){
      dist <- abs(getVal("less", 1-alpha, qnorm)) * (sigma.x / sqrt(n))
    }else{
      dist <- abs(getVal("less", 1-alpha, qt, df)) * (sigma.x / sqrt(n))
    }
    if(alternative == "greater"){
      return(c(mu-dist,Inf))
    }else if(alternative == "less"){
      return(c(-Inf,mu+dist))
    }
  }

  
  n<-length(x) #sample size
  xbar<-mean(x) #sample mean
  
  
  if(length(x)>30){#ztest
    test <- "z"
    statistic<-(xbar-mu)/(sigma.x/(sqrt(n))) #test statistic
    pval <- getVal(alternative, statistic, pnorm)
    conf.int <- getLimitConfLvl(alternative, conf.level,xbar, sigma.x,NULL,n)
  }else{#ttest
    test <- "t"
    statistic <- (xbar-mu)/(sigma.x/sqrt(n))#test statistic
    df <- n-1
    pval <- getVal(alternative, statistic, pt,df)
    conf.int <- getLimitConfLvl(alternative, conf.level,xbar, sigma.x,df,n)
  }
  
  l1 <- paste(test,"test")
  l2 <- paste()
  l3 <- paste(test,"statistic =",statistic,pTxt,pval)
  l4 <- paste (altHypo)
  l5 <- paste(confintervaltxt)
  l6 <- paste(conf.int[1],conf.int[2])
  
  cat(l1,l2,l3,l4,l5,l6,sep = "\n")
  return(pval<1-conf.level)
}

#testing

x <- c(3, 4, 5)
(mean.test(x, "two.sided", 4, 0.94, sd(x))) == F
(mean.test(x, "two.sided", 4, 0.94, NULL)) == F
(mean.test(x, "less", 4.8, 0.5, 2)) == T
(mean.test(x, "greater", 4.8, 0.99, 0.5)) == F
(mean.test(x, "greater", -4.8, 0.99, 2)) == T

set.seed(272727)
heightp <- rnorm(250, 168, 20)
height<-sample(heightp,40)
mean.test(height,"less",175,.95,NULL) == T


set.seed(272727)
heightp <- rnorm(250, 168, 20)
height<-sample(heightp,40)
mean.test(height,"less",175,.95, NULL)
#funcsign  function(x,alternative, mu, conf.level, sigma.x)
dataset <- c(17.26, 13.53, 12.86, 12.82, 26.35, 11.43, 14.64, 10.75, 17.76, 14.4,
             15.05, 14.78, 9.93, 11, 5.37, 5.89, 16.46, 11.62, 20.35, 19.69, 15.74)
length(dataset)

mean.test(dataset,"greater",12,.99, NULL)

t.test(dataset, alternative = "greater", mu = 12, conf.level = 0.99)


#7

getVal <- function(alternative, val, func, df = NULL){
  if(!is.null(df)){
    if(alternative == "greater"){
      return(func(val,lower.tail = F,df=df))
    }else if(alternative == "less"){
      return(func(val,lower.tail = T,df=df))
    }else if(alternative == "two.sided"){
      if(val>0){
        return(func(val,lower.tail = F,df=df)*2)
      }else{
        return(func(val,lower.tail = T,df=df)*2)
      }
    }
  }
  if(alternative == "greater"){
    return(func(val,lower.tail = F))
  }else if(alternative == "less"){
    return(func(val,lower.tail = T))
  }else if(alternative == "two.sided"){
    if(val>0){
      return(func(val,lower.tail = F)*2)
    }else{
      return(func(val,lower.tail = T)*2)
    }
  }
}

getVal("greater",-1.01,pt,18)
getVal("two.sided",0.017,pnorm)
getVal("two.sided",4.98,pt,8)
getVal("two.sided",-2.55,pnorm)
getVal("less",-0.55,pnorm)


#8

z.test <- function(x, y, sigma.x, sigma.y, alternative, mu, conf.level){

  if(is.null(y)){#normal 1 sample z test
    n<-length(x) #sample size
    if(is.null(sigma.x)){
      sigma.x <-sd(x) #estimate standard deviation
    }
    sdm<-sigma.x/(sqrt(n)) #standard deviation of sample mean
    xbar<-mean(x) #sample mean
    z<-(xbar-mu)/sdm #test statistic
    if(alternative == "greater"){
      z.alpha <- qnorm(conf.level)
      if(z>z.alpha){
        return(T)
      }else{
        return(F)
      }
    }else if (alternative == "less"){
      z.alpha <- qnorm(conf.level)
      if(z< -z.alpha){
        return(T)
      }else{
        return(F)
      }
    }else if (alternative == "two.sided"){
      z.half.alpha <- qnorm(1-(1-conf.level)/2)
      if(abs(z)<z.half.alpha){
        return(F)
      }else{
        return(T)
      }
    }else{
      stop("invalid alternative!")
    }
  }else{
    if(is.null(sigma.x) || is.null(sigma.y)){
      sigma.x <- sd(x)
      sigma.y <- sd(y)
    }
    nx <- length(x)
    ny <- length(y)

    z <- ((mean(x)-mean(y))-mu)/sqrt((sigma.x^2)/nx+(sigma.y^2)/ny)

    if(alternative == "greater") {
      z.alpha <- qnorm(conf.level)
      if(z>z.alpha){
        return(T)
      }else{
        return(F)
      }
    } else if (alternative == "less") {
      z.alpha <- qnorm(conf.level)
      if(z< -z.alpha){
        return(T)
      }else{
        return(F)
      }
    } else if (alternative == "two.sided") {
      z.half.alpha <- qnorm(1-(1-conf.level)/2)
      if(abs(z)<z.half.alpha){
        return(F)
      }else{
        return(T)
      }
    } else {
      stop("invalid alternative!")
    }
  }

}


m1 <- c(3.4, 4.5, 6.3, 3.5, 6.3, 5.7, 4, 4.9, 7.2, 3.9, 6.2, 7)
m2 <- c(6.1, 5.7, 8.4, 7.4, 8.3, 5.3, 6.6, 7.4, 4.1, 4.9, 4.4, 5.5)

sdM1 <- sd(m1)
sdM2 <- sd(m2)
nM1 <- length(m1)
nM2 <- length(m2)
sdPooled2 <- ((nM1-1)*sdM1^2+(nM2-1)*sdM2^2)/(nM1+nM2-2)

t <- ((mean(m1)-mean(m2))-0)/sqrt(sdPooled2*((1/nM1)+(1/nM2)))
t
tA <- qt(0.95,nM1+nM2-2)
t < -tA

z.test(m1,m2,NULL,NULL,"greater",0,0.95)
#signature function(x, y, sigma.x, sigma.y, alternative, mu, conf.level)

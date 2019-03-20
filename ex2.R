#1 - c
f1 <- function(x){
  ((x[1]-1)^2)+((x[2]-2)^2)
}

fineness <- 200
xmesh <- seq(-20, 20, length.out = fineness)
ymesh <- seq(-20, 20, length.out = fineness)
mesh <- expand.grid(xmesh, ymesh)

z <- apply(mesh, 1, f1)
z <- matrix(z, ncol = fineness, byrow = FALSE)
contour(xmesh, ymesh, z, nlevels = 20, col = 1,asp=1)
abline(a = 7, b = -2, col = "red")
points(2.2, 2.6, col = 2, pch = 3)


#6
library(lpSolve)
f2 <- function(x){
  2000 * x[1] + 9000 * x[2]
}
fineness <- 1000
xmesh <- seq(0, 6000, length.out = fineness)
ymesh <- seq(0, 12000, length.out = fineness)
mesh <- expand.grid(xmesh, ymesh)

z <- apply(mesh, 1, f2)
z <- matrix(z, ncol = fineness, byrow = FALSE)
contour(xmesh, ymesh, z, nlevels = 20, col = 1)
abline(a = 10000, b = -5/2, col = "red")
abline(a = 3000, b = -3/5, col = "red")

#7
#constroptim
constM <- matrix(c(1, 0,
                   0, 1,
                   5, 2,
                   3, 5), nrow = 4, ncol = 2, byrow = TRUE)
constV<- c(0, 0,20000-1e-05, 15000-1e-05)
solution=constrOptim(theta=c(10000,10000),f2,ui=constM,ci = constV, grad = NULL)
#diff: 5.281439e-05
print('O and S')
solution$par
print('with a minimum value')
solution$value
solution

#lp
first_method <- lp(direction="min",
                   objective.in = c(2000, 9000),# 2000 * x[1] + 9000 * x[2]
                   const.mat = constM,
                   const.dir = c(">=", ">=",">=", ">="),
                   const.rhs = constV)

first_method$solution


#5000|0
#b


contour(xmesh, ymesh, z, nlevels = 20, col = 1, xlab = "O", ylab = "S")
extreme<-rbind(c(5000,1300),c(3800,2100),c(250,10600))  # legend coordinates

y <- c(10000, 790, 0, 0, 100000)  # The y-coordinate of the vertices
x <- c(0, 3685, 5000, 100000, 0)  # The x-coordinate of the vertices
polygon(x, y, density = 5)  # feasible solutions

text(extreme, c("(5000,0)","(3685,790)","(0,10000)"), col = 2, cex = 1.5) # legend text

abline(a = 10000, b = -5/2, col = "red")
abline(a = 3000, b = -3/5, col = "blue")

points(5000,0, pch = 19, cex = 1, col = 'green')  # solution
#x axis -> O
#y axis -> S

#8
f.obj <- c(1, 3, 4, 1)  # 1. function
f.obj2 <- c(1, -3, 4, 1)  # 2. function - becomes unbounded (error status 3) 
f.con <- matrix(c(1,0,0,0,
                  0,1,0,0,
                  0,0,1,0,
                  0,0,0,1,
                  1, -2, 0, 0,
                  0, 3, 1, 0,
                  0, 1, 0, 1), ncol=4, byrow=TRUE)
f.dir <- c(">=",">=",">=",">=",">=", ">=", ">=")  
f.rhs <- c(0,0,0,0,9, 9, 10)  # right side values of constraints

lp1 <- lp(direction= "min", f.obj, f.con, f.dir, f.rhs)
print(c("x1", "x2", "x3", "x4"))
lp1$solution

lp2 <- lp(direction= "min", f.obj, f.con, f.dir, f.rhs, all.int = T)
print(c("x1", "x2", "x3", "x4"))
lp2$solution
print(c("x1", "x2", "x3", "x4"))
lp2$solution-lp1$solution

lp3 <- lp(direction= "min", f.obj2, f.con, f.dir, f.rhs)
#status	Numeric indicator: 0 = success, 2 = no feasible solution
lp3
print(c("x1", "x2", "x3", "x4"))
lp3$solution


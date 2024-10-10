# Assaig Bernoulli
x <- c(0,1)
ptele <- c(0.68,0.32)
plot(x,ptele, type="h", ylim=x,col="red")
points(x,ptele,pch=16,col="red")
n <- 43
mostra <- sample(x,n,ptele,replace=TRUE)
pie <- pie(table(mostra))
bar <- barplot(table(mostra)/n)
points(bar, ptele, pch=16,col="red")
lines(bar,ptele,type="h")
# a) numericament, amb enquestes tendeix a inf
X <- function(i){(sum(sample(x,n,ptele,replace=TRUE)))}
set.seed(123)
m <- 400000 # fem 40 enquestes de 43 persones dient si tenen 2 teles
enquestes <- sapply(1:m,X)
# barplot(table(sapply(1:m,X)))
fr <- table(enquestes)/m
fr
fr[13]
fr[17]
# a) amb R
dbinom(13,43,0.32) # probabilitat que 13 persones tinguin 2 teles P(X=13)
pbinom(16,44,0.32) # probabilitat de 0 a 16 persones tinguin 2 teles P(X<17)
# b) 24 llars i Y = màxim 1 tele
x2 <- c(0,1)
ptele2 <- c(0.32,0.68)
n2 <- 24
Y <- function(i){(sum(sample(x2,n2,ptele2,replace=TRUE)))}
enq <- sapply(1:m,Y)
enq
mean(enq)
var(enq)
quantile(enq)
qbinom(enq)
qbinom(0.25,24,0.68)
# c) 
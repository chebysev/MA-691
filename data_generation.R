ls(all=TRUE)
rm(list=ls(all=TRUE))

# Generalized exponential distribution

gexp<-function(a,l){
u = runif(1)
x = -log(1-u^(1/a))/l
return(x)
}

# Bivariate generalized exponential

bgexp<- function(a1,a2,a3,l){
u1 = gexp(a1,l)
u2 = gexp(a2,l)
u3 = gexp(a3,l)
x1 = max(u1,u3)
x2 = max(u2,u3)
x = c(x1,x2)
return(x)
}


b = c()
for(i in 1:100){
x = bgexp(2,3,4,0.1)
b = rbind(b,x)
}
print(b)

ls(all=TRUE)
rm(list=ls(all=TRUE))

# Bivariate weibull


bwb <- function(a,l1,l2,l3){
u1 = rweibull(1,a,l1)
u2 = rweibull(1,a,l2)
u3 = rweibull(1,a,l3)
x1 = min(u1,u2)
x2 = min(u1,u3)
x = c(x1,x2)
return(x)
}

b = c()
for(i in 1:100){
x = bwb(1,5,3,2)
b = rbind(b,x)
}
print(b)

ls(all=TRUE)
rm(list=ls(all=TRUE))



# Generalized exponential distribution

gexp<-function(a,l){
u = runif(1)
x = -log(1-u^(1/a))/l
return(x)
}

# Bivariate generalized exponential

bgexp<- function(a1,a2,a3,l){
u1 = gexp(a1,l)
u2 = gexp(a2,l)
u3 = gexp(a3,l)
x1 = max(u1,u3)
x2 = max(u2,u3)
x = c(x1,x2)
return(x)
}

# Absolutely Continuous Bivariate Generalized Exponential

c = c()
while(length(c)<200){
x = bgexp(2,6,3,4)
if(x[1] == x[2]){
next
}
c= rbind(c,x)
}
print(c)

ls(all=TRUE)
rm(list=ls(all=TRUE))

# Absolutely Continuous Bivariate Weibull distribution

bwb <- function(a,l1,l2,l3){
u1 = rweibull(1,a,l1)
u2 = rweibull(1,a,l2)
u3 = rweibull(1,a,l3)
x1 = min(u1,u2)
x2 = min(u1,u3)
x = c(x1,x2)
return(x)
}


b = c()
while(length(b)<200){
x = bwb(6,2,3,4)
if(x[1] == x[2]){
next
}
b= rbind(b,x)
}
print(b)





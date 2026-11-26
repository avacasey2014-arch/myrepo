
#3.3 Exercise 3
#1
df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))

head(df)
#2
nll_lm <- function(data, par) {
  
  b0<- par[1]
  b1<- par[2]
  b2<- par[3]
  b3<- par[4]
  sigma <- par[5]
  mu <- b0 + b1*data$x1 + b2*data$x2 + b3*data$x3
  e <- data$y - mu
  llik <- dnorm(e, mean=0, sd=sigma, log=TRUE)
  return(-sum(llik))
}



#3

inits <- c(mean(df$y), 0, 0, 0, 3)

fit <- optim(inits, nll_lm, data=df,
             method="L-BFGS-B",
             lower=c(-Inf, -Inf, -Inf, -Inf, 0.01),
             upper=c(Inf, Inf, Inf, Inf, Inf))

fit$par
fit$convergence



#5

X <- cbind(1, df$x1, df$x2, df$x3)
y <- df$y

beta<- solve(t(X) %*% X) %*% t(X) %*% y

fit$par[1:4]


#6
e <- y - X %*% beta
n <- length(y)
p <- ncol(X)

sigma <- sqrt(sum(e^2)/(n-p))

sigma
fit$par[5]




#8

fit <- optim(inits, nll_lm, data=df,
             method="L-BFGS-B",
             lower=c(-Inf, -Inf, -Inf, -Inf, 0.01),
             upper=c(Inf, Inf, Inf, Inf, Inf),
             hessian=TRUE)
sqrt(diag(solve(fit$hessian)))[1:4]

#4
mod <- lm(y ~ x1 + x2 + x3, data=df)

coef(mod)

summary(mod)$sigma


## Week 4 - Homework

# ----> Question 1 and 2 

# As we have seen in class, the problem with our regression model is that
# it corresponds to the following model : Y = ßX where ß does not include 
# the intercept. Our line does not fin R line, because R tales into accont
# the intercept. 
# Thus we now want to take into account the intercept. The extended model is : 
# Y=Xaß  where Xa is the 10x2 matrix with only one in the first column 
# and whose second column equals X and ß = [ß_0 ß_1]^-(1)
# (Xa stands for X adjusted)

# We thus need to go from the object X we used in class to the objet Xa

require(MASS)

# First, we set the correlation parameter and the mean
beta = 0.5
SIGMA = matrix(c(1,beta,beta,1), ncol=2)
MU = c(2.0, 1.0)

# Second, we set the sample size
N = 50

# Third, we draw your sample
data <- mvrnorm(N, mu = MU, Sigma = SIGMA)
dim(data)
data[1:10,]
plot(data)
abline(lm(data[,2]~data[,1]), col="green")

# Now we build our regression model.
# First, we define Y and X
y <- data[, 2]
X <- data [, 1]

# Now we want to go to Xa
# To do this we will create a vector of dim 10 with only ones which we are 
# going to merge with the X vector to obtain the 2x10 matrix Xa

one <- c(rep(1, 50))
Xa <- cbind(one, X)

# Now we can use the formula and do the computations. Instead of giving a 
# single value for beta, it is going to be a vector with 
# 2 components : ß_0 and ß_1 :

XaT = t(Xa)
XaTXa = XaT%*%Xa
invXaTXa = solve(XaTXa)
XaTy = XaT%*%y
Beta = invXaTXa %*% XaTy
Beta
Beta[2]

# Now add this line to the plot
plot(data)
abline(lm(data[,2]~data[,1]), col="green") # regression line (y~x) 
abline(a=Beta[1], b=Beta[2], col="blue")

# Now our line fits to the R line ! 

# ----> Question 3

# Now we want to draw a sample from a tri-dimensional randow variable. 
# Now we are thus defining SIGMA2 as a 3x3 variance-covariance matrix,
# and MU as a vector with 3 elements to build our data. 

beta1 = 0.5
SIGMA2 = matrix(c(1,beta,beta,beta,1,beta,beta,beta,1), ncol=3)
MU2 = c(3.0, 2.0, 1.0)
N = 50
data2 <- mvrnorm(N, mu = MU2, Sigma = SIGMA2)

# Now our model looks like : 
# y(i) = ß0 + ß1x1(i) + ß2x2(i) (where i is in [1;50])
# Thus we will draw two things from our sample: Y first, and then X which 
# is a 50x2 matrix which first columns represent the vector of x1(i) and 
# second column represent the vectors of x2(i). (To avoid repeating code 
# i use a instead of y and B instead of Xa)

a <- data2[, 1]
X1 <- data2[, 2]
X2 <- data2[, 3]
B <- cbind(one,X1, X2)

BT = t(B)
BTB = BT%*%B
invBTB = solve(BTB)
BTa = BT%*%a
BETA = invBTB %*% BTa
BETA

# We can check the fit of the model by plotting predicted a (Yhat)
# and empirical a (a) on the same plot

Yhat <- B %*% BETA 

plot(a, Yhat, type="p", col="orange")
abline(a=0, b=1, col="purple")

# It is not super fitted but this was random draws so we should not
# be expected a great fitting ! 


# ----> Question 4

# We want to write a function that works like this : 
  # INPUT : MU, SIGMA, N 
  # PROCESS : using them to a) build a sample b) from which we draw 
  # one vector (Y) and a matrix X c) make the computations 
  # OUTPUT : beta 
# So, first, we can see that Beta is always defined as 
# beta <- solve(t(X) %*% X) %*% t(X) %*% Y
# We are going to use this in the output.

# We define first our variables : 

I <- 5 
beta <- runif(1)
Mu <- rep(0, i)
Sig <- matrix(0, ncol=5, nrow=5, byrow=T)
n <- sample(100:200, 1)

for(j in (1:I)) {
  Mu[j] <- sample(1:6, 1)
}
Mu

for(k in 1:I){
for(s in 1:I){
    if(k == s){
      Sig[k, s] <- 1
    } else{
      Sig[k, s] <- beta
    }
  }
}
Sig

# Now we have Mu, Sigma and n and we will be able to plug them into 
# a function. 

reg <- function(n, Mu, Sig){
  data <- mvrnorm(n, mu = Mu, Sigma = Sig)
  Y <- data[,2]
  X <- data[,1]
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  return(beta)
}

reg(50, c(2.0, 1.0), SIGMA)

# Question : I do not get why when I define the function differently by
# setting : Y <- data[,1] and X <- data[,2] I get results that are larger 
# than 1 … Do you have a clue ?

# Now I think I can be better in two ways : 
# --> 1) create a function of beta that output the variance-covariance matrix

varmat <- function(beta){
  varmat <- matrix(0, ncol=5, nrow=5, byrow=T)
  for(k in 1:I){
    for(s in 1:I){
      if(k == s){
        varmat[k, s] <- 1
      } else{
        varmat[k, s] <- beta
      }
    }
  }
  return(varmat)
}

varmat(0.5)
s <- varmat(0.5)
s

# Finally, I might get along with functions ! 
# Now I will try to write again the function reg using the function : 

######### comment
# you need to define x! or beta as above
# Also, your varmat funciton returns a covariance matrix
# of dimension 5, which doesn't match your mu
#reg1 <- function(n, Mu, varmat=function(x) x){
reg1 <- function(n, Mu, beta=0.1, varmat=function(x) x){
  #data <- mvrnorm(n, mu = Mu, Sigma = varmat(x))
  varmat(beta)
  data <- mvrnorm(n, mu = Mu, Sigma = varmat(beta))
  Y <- data[,2]
  X <- data[,1]
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  return(beta)
}

reg1(50, c(2.0,1.0), 0.5)

# It does not work :( I tried to change several things in the syntax but I did 
# not success to make the reg1 function work ! I tried : 

reg11 <- function(n, Mu, varmat=function(x) x){
  S <- varmat(x)
  data <- mvrnorm(n, mu = Mu, Sigma = S)
  Y <- data[,2]
  X <- data[,1]
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  return(beta)
}

# --> 2) with the function reg I have created, I am again not taking 
# into account the intercept. I am going to take the first reg function
# and just add a line : 

one <- c(rep(1, n))

reg2 <- function(n, Mu, Sig){
  data <- mvrnorm(n, mu = Mu, Sigma = Sig)
  Y <- data[,2]
  x <- data[,1]
  X <- cbind(one,x)
  beta <- solve(t(X) %*% X) %*% t(X) %*% Y
  return(beta)
}

reg2(50, c(2.0,1.0), SIGMA)

# Again it works since we obtain a vector for ß !

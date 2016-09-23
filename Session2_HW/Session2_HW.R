#Week 2 - First Assignment

# ----> Question 1

#We assign an initial wealth
w0<-100
#We define a number of period over which the gambler plays
T<-1000
#We define the vectors of wealth and
w <- rep(0, T+1)
x <- rep(0, T)
w[1]=w0


for(t in 1:T) {
  u <- runif(1)
  p <- runif(1)
  if(u <= 1 - p){
    print("You lose!")
    x[t] <- 0
    w[t+1] <- w[t] - 1
  } else {
    print("You win!")
    x[t] <- 1
    w[t+1] <- w[t] + 1
  }

print(sprintf("Now you've got $%s.", w[t+1]))
}

# The first time I ran this code I got p = 0.6321 and u = 0.7867
# The second time I ran it I got p = 0.5576 and u = 0.8698

# ----> Question 2 

# We can see from the above code that p took different values, but u
# (u corresponds to the random shock each period, i.e. the fact that 
# the gamble works or not). 
# As we want the shock to be fixed, we are going to define it outside 
# the loop : 
u = runif(1)

#Defining the vector for initial wealth
v0 <- 100
v <- rep(0, T+1)
v[1]=v0

T<-1000
x <- rep(0, T)

for(t in 1:T) {
  p <- runif(1)
  if(u <= 1 - p){
    print("You lose!")
    x[t] <- 0
    v[t+1] <- v[t] - 1
  } else {
    print("You win!")
    x[t] <- 1
    v[t+1] <- v[t] + 1
  }
  print(sprintf("Now you've got $%s.", v[t+1]))
}

v

# ----> Question 3 

# a) For question 1

time<-T+1

plot(1:time, w, type='l')

# a) For question 2

plot(1:time, v, type='l')


# ----> Question 4

# Here we want to define two loops : one is going to account for 
# the different trials, the other is going to account for the 
# path of wealth in each trial j that leads to the final wealth of
# trial j, in which we are interested.

#We now fix p : 
p <- 0.5

#We need to define a loop for m simultions of all the periods
J <- 100

#We again define wealth : 
y0 <- 100
y <- rep(0, T+1)
y[1]=v0

# We now need a vector for terminal Wealth
Wt <- rep(0, J+1)
Wt0 <- y0

T<-10
x <- rep(0, T)


for(j in 1:J) {
for(t in 1:T) {
  u <- runif(1)
  if(u <= 1 - p){
    print("You lose!")
    x[t] <- 0
    y[t+1] <- y[t] - 1
  } else {
    print("You win!")
    x[t] <- 1
    y[t+1] <- y[t] + 1
  }
  print(sprintf("Now you've got $%s.", y[t+1]))
}
Wt[j] <-y[T+1]
}

# Now we are going to create a variable averageFW that 
# gives the average Final Wealth of our experiments : 

averageFW <- mean(Wt)
averageFW

# ----> Question 5 

# Now we are interested in two vectors : the one containing the values of p
# and the one containing the values of final wealth. We are thus going to
# consider the same two loops, but we are building a vector P : 
P <- rep(0, J)


for(j in 1:J) {
  for(t in 1:T) {
    p <- runif(1)
    P[j] <- p
    u <- runif(1)
    if(u <= 1 - p){
      print("You lose!")
      x[t] <- 0
      y[t+1] <- y[t] - 1
    } else {
      print("You win!")
      x[t] <- 1
      y[t+1] <- y[t] + 1
    }
    print(sprintf("Now you've got $%s.", y[t+1]))
  }
  Wt[j] <-y[T+1]
}

# I wanted to see if I could plot P and Wt, and as both vectors did not
# have the same number of elements, I just created a new vector which
# would not take into account initial wealth

Wt2 <- rep(0,J)
z<-1
while (z <= J) {
  Wt2[z]=Wt[z]
  z = z+1
}
Wt2

plot(P, Wt2)

# ---> Question 6 

# I don't really understand how to do this. I can think of creating
# vectors that are defined to take the values we want them to take but I
# do not really see how to do it without loops. 



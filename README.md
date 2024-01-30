#question 1
m <- 2^31 -1 
a <- 7^5
x <- numeric(length=1e5)
x[1] <-3
c <- 2^10 -1 
for(i in 2:1e5){
  x[i] <-  (a*x[i-1])%%m + c 
}
par(mfrow = c(1,2))
plot(x/m)
plot.ts(x/m)


#qustrion 2

mybern <- function(p){
  accept <- 0
  count <- 0
  u <- runif(1)
  q <- 1-p
  if(u<q){
    return(0)
    accept =1
  }else{
    return(1)
    accept =1
  }
}

replicate(1e3,mybern(0.25))
r

#
#question 3
sample(100,size = 100)

####?DOUDT
mysample <- function(n,size,replace){
  u <- numeric(length = size)
  for(i in 1:size){
    u <- as.integer(runif(1,min =0,max=n))
  }
  if(replace== "TRUE"){
    for(i in 1:size){
      for( j in 1:size){
        if(u[i]==u[j]){
          u[i] =as.integer(runif(1,min =0,max=n))
        }else{
          
        }
      }
    }
  }
  u
} 


################################
#worksheet 2
###########################################
## Accept Reject algorithm to draw from
## Binomial(n,p)
###########################################
# setting the seed makes it so that the same sets of
# random variables are realized.
set.seed(1)  

# Function draws one value from Binom(n,p)
# n = number of trials
# p = probability of success
draw_binom <- function(n, p)
{
  accept <- 0 # Will track the acceptance
  try <- 0 # Will track the number of proposals
  
  # upper bound calculated in the notes
  x <- 0:n
  all_c <- choose(n,x) * (1-p)^(n - 2*x) * p^(x-1)  # from notes
  c <- max(all_c) + 0.01  # what is the value of c ?
    
    while(accept == 0)
    {
      try <- try + 1
      
      U <- runif(1)
      prop <- rgeom(1, prob = p) #draw proposal
      
      ratio <- (choose(n,prop) * (1-p)^(n - 2*prop) * p^(prop-1))/c  # calculate the ratio
        if(U < ratio)
        {
          accept <- 1
          rtn <- prop
        }
    }
  return(c(rtn, try))
}
draw_binom(n = 10, p = .25)


###
# If we want X1, ..., Xn ~ Binom(n.p)
# we need to call the function multiple times

# sample size
N <- 1e3
samp <- numeric(N)
n.try <- numeric(N)
for(t in 1:N)
{
  # I use as a dummy variable often
  foo <- draw_binom(n = 10, p = .25)
  samp[t] <- foo[1]
  n.try[t] <- foo[2]
}
mean(samp) #should be n*p = 2.5
mean(n.try)

###########################################
## A closer look at Binomial and Geometric
###########################################
# Turns out, this choice of Binomial and Geometric
# can work, but not always. In the code below, 
# increase n to see what happens

p <- .25
n <- 10
x <- 0:(n)
mass.geom <- dgeom(x, p)
mass.bin <- dbinom(x, size = n, prob = p)

all_c <- choose(n,x) * (1-p)^(n - 2*x) * p^(x-1)
(c <- max(all_c))


plot(x, mass.geom, pch = 16, col = "red", type= "n")
points(mass.bin, pch = 16, col = "red", type= "h")
points(mass.geom, pch = 16, col = "blue", type = "h", lty = 2)


# Matching the means:
# choosing p* for rgeom so that np = (1-p*)/p*
p.star <- 1/(n*p + 1)
mass.geom <- dgeom(x, p.star)
mass.bin <- dbinom(x, size = n, prob = p)

all_c <- choose(n,x) * (1-p.star)^(n - 2*x) * p.star^(x-1)
(c <- max(all_c))


plot(mass.geom, pch = 16, col = "red", type= "n")
points(mass.bin, pch = 16, col = "red", type= "h")
points(mass.geom, pch = 16, col = "blue", type = "h", lty = 2)

###################
#question 3
c <- 1/ppois(20,20)
c <- 1/sum((exp(-lmda)*lmda^(0:m))/factorial(0:m)) 
c

trunc_pois <- function(m=20,lmda=20){
  accept <- 0
  try <- 0
  while(accept ==0){
    try <- try +1
    prop <- rpois(1,lmda)
    
    if(prop<=m){1
      accept <- 1 
    }
    
    
  }
  return(c(prop,try))
}

n <- 1e3
result <- replicate(n,trunc_pois())
mean(result[1,])
mean(result[2,])
hist(result[1,])

#roeng solution 
trunc_poisson <- function(n,m,lmda){
  accept <- 0 
  try <- 0
  m<- 20
  lmda <- 20
  all_c <- numeric(length=m)
  all_c[1] <- 0
  for(i in 2:m+2){
  all_c[i] <- sum(exp(lmda+log(factorial(0:m)) - (0:m)*log(lmda)))
     all_c[i] <- all_c[i] + all_c[i-1]
  }
  all_c <- all_c[-(1:3)]
  c <- max(all_c)
  while(accept ==0){
    try <- try +1
    u <- runif(1)
    prop <- rpois(1,lmda)
     }
  }

################################
#question 4

x <- 0:500
p <- 0.25
all_c <- dgeom(x,p)/dpois(x,10)
c <- max(all_c)
c
# for the large value of the x the c goes to infinite  so this is not a good proposal 

############################\
#worksheet 3


#########################
## Accept-reject for
## Beta(4,3) distribution
## Using U(0,1) proposal
#########################
set.seed(1)
beta_ar <- function() 
{
  c <- 60 *(3/5)^3 * (2/5)^2 
  accept <- 0
  counter <- 0   # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1
    
    # fill in the lines of code here to implement
    U <- runif(1)
    prop <- runif(1)
    ratio <- dbeta(prop,4,3)/c
    
    if(U < ratio)
    {
      accept <- 1
      return(c(prop, counter))
    }
  }
}


### Obtaining 10^4 samples from Beta() distribution
N <- 1e4
samp <- numeric(length = N)
counts <- numeric(length = N)
for(i in 1:N)
{
  rep <- beta_ar()  ## fill in
    samp[i] <- rep[1] ## fill in
    counts[i] <-rep[2] ## fill in
}
# Make a plot of the estimated density from the samples
# versus the true density
x <- seq(0, 1, length = 500)
plot(density(samp), main = "Estimated density from 1e4 samples")
lines( x,dbeta(x,4,3), col = "red", lty = 2) ## Complete thi2
legend("topleft", lty = 1:2, col = c("black", "red"), legend = c("AR", "truth"))

# This is c
(c <- 60 *(3/5)^3 * (2/5)^2)

# This is the mean number of loops required
mean(counts)

#They should be almost the same!

############################
#question 2
#by using inversee transform method 

trunc_exp <- function(){
  accept <- 0
  try <- 0
  while(accept==0){
    try <- try +1
    u <- runif(1)
    expo <- -log(u)
    if(expo <= 0.05){
      accept <- 1
      return(c(expo,))
    }
  }
  
}

trunc_exp()


##############################
#by accept reject 
exp_dis <- function(){
  accept <- 0
  try <- 0
  while(accept == 0){
    try <- try + 1
    x <- runif(1, 0, 0.05)
    c <- exp(-x - log(1 - exp(-0.05)))
    prop <- runif(1)
    ratio <- exp(-prop - log(1 - exp(-0.05))) / c
    u <- runif(1)
    if(u <= ratio){
      accept <- 1
    }
  }
  return(c(x, try))
}

#question 3

##########################
## Accept-reject for obtaining
## sample uniformlyfrom a standard circle
## using a box as a proposal
##############################
set.seed(1)
circle_ar <- function() 
{
  accept <- 0
  counter <- 0   # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1
    # Fill code here for accept-reject
    x <- runif(1,-1,1)
    y <- runif(1,-1,1)
    c <- 4/pi
    
    if( x^2 +y^2 <1 ) # fill condition
    {
      accept <- 1
      prop  <- c(x,y)
      return(c(prop, counter))
    }
  }
}

# Simulation 10^4 samples from circle
N <- 1e4
samp <- matrix(0, ncol = 2, nrow = N)
counts <- numeric(length = N)
for(i in 1:N)
{
  foo <- circle_ar()  # I use foo as a dummy name
  samp[i,] <- foo[1:2]
  counts[i] <- foo[3]
}


4/pi
# [1] 1.27324
mean(counts)  # should be very close

# Plotting the obtained samples
# no paritcular part of the circle is favored more
# than any other part.
plot(samp[,1], samp[,2], xlab = "x", ylab = "y", 
     main = "Uniform samples from a circle", asp = 1)



####
##################################
## Accept-reject for obtaining
## sample uniformlyfrom a standard circle
## using a box as a proposal
##############################
set.seed(1)
circle_ar <- function()
{
  accept <- 0
  counter <- 0 # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1
    prop.temp <- runif(2) # from U(0,1)
    prop <- -1 + 2*prop.temp # from U(-1,1)
    if(prop[1]^2 + prop[2]^2 <= 1) # fill condition
    {
      accept <- 1
      return(c(prop, counter))
    }

  }
}
#######################
#question 4


set.seed(1)
circle_ar <- function()
{
  accept <- 0
  counter <- 0 # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1
    prop.temp <- runif(2) # from U(0,1)
    prop <- -1 + 2*prop.temp # from U(-1,1)
    if(prop[1]^2 + prop[2]^2 <= 1) # fill condition
    {
      accept <- 1
      return(c(prop, counter))
    }
    
  }
}

N <- 1e4
samp1 <- numeric(length = 1e4)
samp2 <- numeric(length = 1e4)
try <- numeric(length = 1e4)
for (i in 1: N){
  p <- circle_ar()
  samp1[i] <- p[1]
  samp2[i] <- p[2]
  try[i] <- p[3]
}
acceptance_probability <- sum(try <= 2) / N

#### b part 

c_sphere <- function(p){
  rtn <- (gamma(p/2+1)*2^p)/pi^(p/2)
  return(rtn)
}
c_sphere(c(2:6,10,30))

p_dim <- function(p){
  accept <- 0
  try <- 0
  while(accept==0){
    try <- try + 1
    u <- runif(p)
    prop <- -1+2*u
    if(sum(prop^2)<=1){
      accept <-1
      
    }
  }
  return(c(prop,try))
}


n <- 1e3
p <- 4
samp <- matrix(0,ncol=p,nrow = n)
count <- numeric(length = n)
for(i in 1:n){
  foo <- p_dim(p=p)
  samp[i,1:p] <- samp[1:p]
  count[i] <- foo[p+1]
}
mean(count)

#question 5



set.seed(1)
beta_ar <- function() 
{
  c <- (gamma(2.1)*(3/5)*(2/5)^-0.9)/(gamma(2)*gamma(0.1))
  accept <- 0
  counter <- 0   # count the number of loop
  while(accept == 0)
  {
    counter <- counter + 1
    
    # fill in the lines of code here to implement
    U <- runif(1)
    prop <- runif(1)
    ratio <- dbeta(prop,2,0.1)/c
    
    if(U < ratio)
    {
      accept <- 1
      return(c(prop, counter))
    }
  }
}


### Obtaining 10^4 samples from Beta() distribution
N <- 1e4
samp <- numeric(length = N)
counts <- numeric(length = N)
for(i in 1:N)
{
  rep <- beta_ar()  ## fill in
  samp[i] <- rep[1] ## fill in
  counts[i] <-rep[2] ## fill in
}

mean(counts)


############question 6
####################
# Sample from dist of Y
####################
distY <- function(alpha, lambda)
{
  l <- length(alpha)
  Wi <- numeric(length = l)
  for(i in 1:l)
  {
    U <- runif(1)
    Wi[i] <- lambda*(-log(1-U))^(1/alpha[i])
  }
  return(sum(Wi))
}

### Estimate expectation with average
samples <- replicate(1e3, distY(alpha = 1:5, lambda = 5))
## Final answer
mean(samples^2)
hist(samples)

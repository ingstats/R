#11-04 시뮬
#8.6

mu=-0.1; sigma=0.3; A=B=10
n.sim=10000

#Straight Approach
I.straight=numeric(n.sim)
for (i in 1:n.sim){
  S=0
  repeat{
    x=rnorm(1, mu, sigma)
    S=S+x
    if (S > B | S< -A) break
  }
  I.straight[i]=as.numeric(S>B)
}
c(mean(I), sd(I)/n.sim)


#Importance Sampling
theta.hat=I=numeric(n.sim)
for (i in 1:n.sim){
  S=0
  repeat{
    x=rnorm(1, -mu, sigma)
    S=S+x
    if (S > B | S< -A) break
  }
  I[i]=as.numeric(S > B)
  theta.hat[i] = I*exp(2*mu*S/sigma^2)
}
c(mean(theta.hat), sd(theta.hat)/n.sim)


#실습2

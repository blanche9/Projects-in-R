optimal.production= function(d,r,k,a,K,s){
  n=length(d)
  
  #Cost vector
  x=0
  y=0
  for (i in 1:n){
    x[i]= k+((n-i)*s)
    y[i]= K+((n-i)*s)
  }
  cost= c(x,y)
  
  #3rd part of the constraing with the upper triangular matrixes
  const3.1= matrix(1, ncol = length(d), nrow = length(d))
  const3.2 = const3.1
  const3.2[upper.tri(const3.2)]<- 0
  
  const3= cbind(const3.2, const3.2)
  
  #MATRIX OF CONSTRAINS
  A= rbind(diag(n*2),diag(n*2), const3)
  
  #DIRECTION VECTOR
  dir= c ( rep(">=", (2*n)), rep("<=", (2*n)), rep(">=", (n-1)), "=")
  
  #VECTOR OF ADDED DEMAND
  dem=c()
  count= 0
  for (i in 1:n){
    dem[i]= count+d[i]
    count= dem[i]
  }
  #RHS VECTOR
  
  RHS= c (rep(0,(2*n)), rep(r,(n)), rep(a, (n)), dem)
  
  
  
  lp("min", cost, A, dir, RHS)$solution
  
  
}

d=c(400,200, 500, 300,400, 100)
r=300
k=35
a=200
K=60
s=15
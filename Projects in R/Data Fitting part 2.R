#PART 2

line.minmax=function (x,y){ #line of best fit
  n = length(x)
  g = c(0,0,max(rep(1,n))) #this is going to be out theta
  A.upper= cbind (x, 1, max(-diag(n))) #first matrix 
  A.lower= cbind( x, 1, max(diag(n))) #the second matrix
  A= rbind(A.upper, A.lower)
  dir= c(rep("<=",n ), rep(">=",n)) #inequalities 
  RHS= c(y, y)
  lp("min", g, A, dir, RHS)$solution[1:2] # will show you the first two numbers  without the errors 
}

x= runif(100, min=20, max=80)
y= 6*x+20+ runif(100, min = -3, max=3) #runif is the noise 

plot(x,y)
curve( 5.9969*x+ 17.3148, add = TRUE, col="red")

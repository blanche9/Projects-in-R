#PART 3 

women

x= women$height
y= women$weight

plot(x,y)
best.line(x,y) 
curve(2.0769*x-0.000,add=TRUE, col="red")
line.minmax(x,y)
curve(2.1450*x+0.000,add=TRUE, col="blue")
curve(3.375*x-82.875, add= TRUE, col= "purple") #correct line by changing class code ax+b to ax-b


A= cbind(x,1)
qr.solve(A,y) #gives a almost the same answer
curve(3.45*x -87.51, add= TRUE, col="green")



line.minmax=function (x,y){ #line of best fit
  n = length(x)
  g = c(0,0,max(rep(1,n))) #this is going to be theta
  A.upper= cbind (x, 1, -max(diag(n))) #first matrix 
  A.lower= cbind( x, 1, max(diag(n))) #the second matrix
  A= rbind(A.upper, A.lower)
  dir= c(rep("<=",n ), rep(">=",n)) #inequalities 
  RHS= c(y, y)
  lp("min", g, A, dir, RHS)$solution[1:2] # will show you the first two numbers  without the errors 
}
best.line=function (x,y){ #line of best fit
  n = length(x)
  g = c(0,0,rep(1,n)) #our g function. g(a,b,e1,e2...)
  A.upper= cbind (x, -1, -diag(n)) #first matrix   (changed to -1 for ax - b )
  A.lower= cbind( x, -1, diag(n)) #the second matrix
  A= rbind(A.upper, A.lower)
  ineq= c(rep("<=",n ), rep(">=",n)) #inequalities 
  rhs= c(y, y)
  lp("min", g, A, ineq, rhs)$solution[1:2] # will show you the first two numbers  without the errors 
}

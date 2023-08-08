#PART1 

line= function(a,b,c){
  
  g= c(0, 0, 0, 1,1,1)
  
  mat= rbind(c(2*a,b,-c), c(a,-2*b,c), c(3*a,b,0*c))
  
  A.upper= cbind(mat, -diag(1,3))
  A.lower= cbind (mat, diag(1,3))
  A= rbind(A.upper, A.lower)
  
  dir= c( rep("<=", 3), rep(">=", 3))
  RHS= c(c(-1,2,-1),c(-1,2,-1))
  
  lp("min", g, A, dir, RHS)$solution[1:2]
  
}

l
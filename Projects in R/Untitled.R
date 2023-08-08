#Linear Optimization
#Suppose we have an ordered list of n items of food. Let a ∈ Rn 
#denote the price vector of all these food items, let p ∈ Rn denote the protein vector, 
#c ∈ Rn denote the carbohy- drate vector, f ∈ Rn denote the fats vector.
#Denote by xi to be amount of food item i, and x = (x1, x2, ..., xn)
#to be the vector all these food combinations.
#Find the vector x which will minimize the total cost, i.e. atx, 
#subject to the constraints that the total protein of the food combination needs to be at least p,
# total carbohydrate needs to be at least c, 
#and the total fats need to be at least f (here p,c,f are given real numbers). 
#Let us further require that the total calories of the food combination needs to be in the interval
# [callow,calhigh] i.e. at least callow calories but at most calhigh calories,
# again the numbers callow and calhigh must be specified.

#Create a function in R called,
#optimize.food(a,p.vec,c.vec,f.vec,p,c,f,cal.low,cal.high)
#Which will return the vector x of food combinations that need to be selected that will minimize the
# total cost of production.



a= c()#price vector of n 
p=c()#protein vector of n 
c= c()#carb vector
f= c() #fat vector
x=(1:n) #vector of all food combinations 

#constrains:  total protein >= p 
#             total carbs   >= c
#             total fats.   >= f
#             total cal     cal.low<= total cal<= cal.high


optimize.food= function (a, p.vec, c.vec , f.vec , p , c, f, cal.low, cal.high){
  cal.vec=c(590, 540, 400, 400, 450, 390)
  n= length(a)
  A=matrix(a,ncol=1)
  B= rbind (p.vec, c.vec, f.vec, cal.vec,cal.vec, diag(n))
  dir=c(rep(">=",4), "<=", rep(">=",n))
  RHS= c(p,c,f,cal.low,cal.high,rep(0,n))
  lp("min",A, B, dir, RHS)$solution
}


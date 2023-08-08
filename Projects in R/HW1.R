
n = 1e3
f = rep(NA,n) 
f[1:2] = 1 
for(i in 3:n){
  if(f[i-1] >= 1e100) { break
  }
  f[i] = f[i-1] + f[i-2]
}
i-1
#while loop


f = c(1,1)
i=2
while( f[i] < 1e100 ){
  f[i+1] = f[i] + f[i-1]
  i=i+1
}
i
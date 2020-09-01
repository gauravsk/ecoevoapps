library(diagram); library(ggplot2); library(ggplotify); library(reshape2)
source("struct_pop_function.R")

# Test strpop()
init = c(100,100,100)
leslie = matrix(c(0,8,1,.12,0,0,0,.5,0), ncol=3, byrow=T); leslie
tt = 100
strpop(leslie=leslie,init=init,tt=tt)





### INPUT
# Initial sizes
N1 = 71
N2 = 20
N3 = 9

# Leslie matrix
L = matrix(
  c( 0, 6, 1,
    .5, 0, 0,
     0,.8, 0),
  ncol = 3, 
  byrow = T, 
  dimnames = list(c("Age 1","Age 2", "Age 3"), c("Age 1","Age 2", "Age 3"))
  )

# Diagram
name = c(expression(Age[1]), expression(Age[2]), expression(Age[3]) )
plotmat(A = L, pos = 3, curve = 0.5, name = name, lwd = 1.5, my = -0.1,
        arr.len = 0.2, arr.width = 0.25, arr.lwd = 2, arr.type = "simple",
        self.lwd = 2, self.shiftx = 0.115, self.shifty = 0.1, self.cex = .5,
        box.size = 0.1, dtext = 0.2, box.lwd = 3.5 
  )


# Projection: number of time steps
t = 100

# Population growth simulation
pop = matrix(c(N1,N2,N3,sum(c(N1,N2,N3))),nrow=4,ncol=1) # start the matrix with initial population sizes
lambda = numeric() # discrete population growth
for(i in 1:t){
  pop = cbind(pop, rbind(round(L %*% pop[1:3,i,drop=F]),NA)) # run one time-step of the model
  pop[4,i+1] = sum(pop[1:3,i+1])
  lambda[i] = pop[4,i+1]/pop[4,i]
  if(pop[4,i+1]==0){break}
  if(i>10){if(round(lambda[i],3)==round(lambda[i-3],3)){break}}
  }
pop
round(lambda,3)

# Stable age structure
round(pop[1:3,ncol(pop)]*100/sum(pop[1:3,ncol(pop)]),2)
eigen(L)$vectors[,1]*100/sum(eigen(L)$vectors[,1])

## Plots
# Growth trajectories of each age class
plot(x = 1, y = 1, type = "n", log = "y",
     xlab = "time steps", ylab = "number of individuals",
     xlim = c(0,i), ylim = c(1,max(pop)),
     main = "Simulation", yaxt="n")
abline(h=1,lty=3,col="grey")
points(x = 0:i, pop[1,]+1 ,type = "l", lty = 1, pch = 1 , cex = .7, col=1) 
points(x = 0:i, pop[2,]+1 ,type = "l", lty = 1, pch = 15, cex = .7, col="grey") 
points(x = 0:i, pop[3,]+1 ,type = "l", lty = 3, pch =  6, cex = .7, col=1) 
tmp = ceiling(log10(max(pop[1:3,])+1))
axis(side=2,at=c(1,10^seq(1,tmp)+1),c(0,10^seq(1,tmp)))
if(eigen(L)$values[1]<1){tmp = "topright"}else{tmp = "bottomright"}
legend(tmp, lty=c(1,1,3), bty = "n",c("1","2","3"), col=c(1,"grey",1))

# Proportions of each age class through time
tmp = cbind(pop[1,]/pop[4,],pop[2,]/pop[4,],pop[3,]/pop[4,])
plot(tmp,type="n",xlim=c(0,nrow(tmp)),ylim=c(0,1),ylab="Proportion",
     xlab="time",main="Stable age distribution")
points(x = 0:(nrow(tmp)-1), tmp[,1] ,type = "l", lty = 1, pch = 1 , cex = .7, lwd=2, col=1) 
points(x = 0:(nrow(tmp)-1), tmp[,2] ,type = "l", lty = 1, pch = 15, cex = .7, lwd=2, col="grey") 
points(x = 0:(nrow(tmp)-1), tmp[,3] ,type = "l", lty = 3, pch =  6, cex = .7, lwd=2, col=1) 
abline(h=eigen(L)$vec[,1]/sum(eigen(L)$vec[,1]),lty=3,col=2)
legend("topright",bty="n",col=2,lty=3,"eigenvector")

# Total population growth
plot(x=1:(ncol(pop)-1),y=lambda,type="l",ylab=expression(lambda),
     xlab="time",ylim=c(0,2),lwd=2,main="Total population growth")
abline(h=c(1,eigen(L)$val[1]),col=c("grey",2),lty=c(3,3),lwd=c(1,2))
if(eigen(L)$val[1]>1.75){tmp="bottomright"}else{tmp="topright"}
legend(tmp,bty="n",col=2,lty=3,lwd=2,"eigenvalue")





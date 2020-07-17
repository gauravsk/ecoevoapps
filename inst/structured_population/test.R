library(diagram)

### INPUT
# Initial sizes
N1 = 100
N2 = 100
N3 = 100

# Leslie matrix
L = matrix(
  c( 1, 4, 8,
    .5, 0, 0,
     0,.8, 0),
  ncol = 3, 
  byrow = T, 
  dimnames = list(c("Age 1","Age 2", "Age 3"), c("Age 1","Age 2", "Age 3"))
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
  if(i>10){if(round(lambda[i],3)==round(lambda[i-1],3)){break}} 
  }
pop
round(lambda,3)

# Stable age structure
round(pop[1:3,ncol(pop)]*100/sum(pop[1:3,ncol(pop)]),2)
eigen(L)$vectors[,1]*100/sum(eigen(L)$vectors[,1])

# Plot
plot(x = 1, y = 1, type = "n", log = "y",
     xlab = "time steps", ylab = "number of individuals + 1",
     xlim = c(0,i), ylim = c(1,max(pop)),
     main = "Simulation")
#abline(v = 0:i, col = "grey", lty = 3)
points(x = 0:i, pop[1,]+1 ,type = "b", lty = 3, pch = 1 , cex = .7) 
points(x = 0:i, pop[2,]+1 ,type = "b", lty = 4, pch = 15, cex = .7) 
points(x = 0:i, pop[3,]+1 ,type = "b", lty = 2, pch =  6, cex = .7) 
points(x = 0:i, pop[4,]+1 ,type = "l", lty = 1, pch = 14, cex = .7, lwd=2)
legend("topleft", lty=c(3,4,2,1), horiz = T,
       pch=c(1,15,6,NA), lwd = c(1,1,1,2), bty = "n",
       c("1","2","3","All"))

# Diagram
name = c(expression(Age[1]), expression(Age[2]), expression(Age[3]) )
plotmat(A = L, pos = 3, curve = 0.5, name = name, lwd = 1.5, my = -0.1,
        arr.len = 0.2, arr.width = 0.25, arr.lwd = 2, arr.type = "simple",
        self.lwd = 2, self.shiftx = 0.115, self.shifty = 0.1, self.cex = .5,
        box.size = 0.1, dtext = 0.2, box.lwd = 3.5, 
        )




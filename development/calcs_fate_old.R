#removal/disp prob, distance, disp eaten prob, cache prob, undisp eaten prob

#All years, all treatments, black oak
raw.data <- read.csv('data/hee_dispersal.csv',header=TRUE)
p.surface <- 0.75
p.buried <- 0.4

d2Dt = function(              # Clark's 2Dt distribution for distance x
  x,
  shape,
  scale,
  log = TRUE
){
  a = shape
  b = scale
  L = 2*pi*x*(a/(pi*b))*(1 + (x^2)/b)^(-a-1)
  if(log) log(L) else(L)
}

abfun = function(ab, r, pdf){
  ablim = c(0, 0.75, 25000, 5)
  if(any(exp(ab) <= ablim[1:2], exp(ab) >= ablim[3:4])) return(1e24)
  ab = exp(ab)
  -sum(d2Dt(r, scale = ab[1], shape = ab[2], log = TRUE))
}

abfun2 = function(ab, r, pdf){
  ablim = c(0, 0.75, 25000, 5)
  if(any(exp(ab) <= ablim[1:2], exp(ab) >= ablim[3:4])) return(1e24)
  ab = exp(ab)
  -sum(dweibull(r, scale = ab[1], shape = ab[2], log = TRUE))
}

r2Dt = function(n, a, b, tol = 1e-5, dtol = -25){
  
  # random generation from the 2Dt distribution
  
  maxx = optimize(
    function(x) (dtol - d2Dt(x, scale = a, shape = b))^2, 
    c(a,5000000)
  )$minimum
  dx = maxx * tol
  x = seq(dx/2, maxx + dx/2, dx)
  pdf = d2Dt(x, scale = a, shape = b, log = F)
  cdf = cumsum(c(0, pdf))*dx
  z = x[findInterval(runif(n), cdf)]
  z[is.na(z)] = mean(z, na.rm = TRUE)
  z = runif(n, z - dx/2, z + dx/2)
  z
}


param.set <- function(x){
  
  out <- vector(length=6)
  
  x <- x[x$squirrel==0,]
  
  add.surface.no <- round((1-p.surface)*length(x$removed[x$removed==1&x$buried==0]))
  select.surface <- sample(c(1:length(x$removed[x$removed==1&x$buried==0])),add.surface.no)
  add.surface <- x[x$removed==1&x$buried==0,][select.surface,]
  
  add.buried.no <- round((1-p.buried)*length(x$removed[x$removed==1&x$buried==1]))
  select.buried <- sample(c(1:length(x$removed[x$removed==1&x$buried==1])),add.buried.no)
  add.buried <- x[x$removed==1&x$buried==1,][select.buried,]
  
  x <- rbind(x,add.surface,add.buried)
  
  ab <- optim(log(c(a = 20, b = 1.01)), abfun, r = x$dist[x$removed==1], pdf = pdf, hessian = TRUE)
  
  ab2 <- optim(log(c(a = 20, b = 1.01)), abfun2, r = x$dist[x$removed==1], pdf = pdf, hessian = TRUE)
  
  out[1] <- mean(x$removed,na.rm=T)
  out[2] <- mean(x$dist[x$removed==1])
  out[3] <- exp(ab2$par[1])
  out[4] <- exp(ab2$par[2])
  out[5] <- exp(ab$par[1])
  out[6] <- exp(ab$par[2])
  out[7] <- mean(x$eaten[x$removed==1],na.rm=T)
  out[8] <- mean(x$buried[x$removed==1&x$eaten==0],na.rm=T)
  out[9] <- mean(x$eaten[x$removed==0],na.rm=T)

  return(round(out,3))
}

#  
disp.params <- data.frame(matrix(nrow=31,ncol=9))
names(disp.params) <- c('dispProb','lam','weibSc','weibSh','2dtA','2dtB','dispEatenProb','cacheProb','undispEatenprob')

disp.params[1,] <- param.set(raw.data[raw.data$wo==0,])
disp.params[2,] <- param.set(raw.data[raw.data$wo==1,])
disp.params[3,] <- param.set(raw.data[raw.data$wo==0&raw.data$shelter==0,])
disp.params[4,] <- param.set(raw.data[raw.data$wo==1&raw.data$shelter==0,])
disp.params[5,] <- param.set(raw.data[raw.data$wo==0&raw.data$shelter==1,])
disp.params[6,] <- param.set(raw.data[raw.data$wo==1&raw.data$shelter==1,])
disp.params[7,] <- param.set(raw.data[raw.data$wo==0&raw.data$year12==0&raw.data$year13==0&raw.data$year14==0,])
disp.params[8,] <- param.set(raw.data[raw.data$wo==1&raw.data$year12==0&raw.data$year13==0&raw.data$year14==0,])
disp.params[9,] <- param.set(raw.data[raw.data$wo==0&raw.data$year12==1,])
disp.params[10,] <- param.set(raw.data[raw.data$wo==1&raw.data$year12==1,])
disp.params[11,] <- param.set(raw.data[raw.data$wo==0&raw.data$year13==1,])
disp.params[12,] <- param.set(raw.data[raw.data$wo==1&raw.data$year13==1,])
disp.params[13,] <- param.set(raw.data[raw.data$wo==0&raw.data$year14==1,])
disp.params[14,] <- param.set(raw.data[raw.data$wo==1&raw.data$year14==1,]) #no buried WO
disp.params[15,] <- param.set(raw.data[raw.data$wo==0&raw.data$year12==0&raw.data$year13==0&raw.data$year14==0&raw.data$shelter==0,])
disp.params[16,] <- param.set(raw.data[raw.data$wo==0&raw.data$year12==0&raw.data$year13==0&raw.data$year14==0&raw.data$shelter==1,])
disp.params[17,] <- param.set(raw.data[raw.data$wo==1&raw.data$year12==0&raw.data$year13==0&raw.data$year14==0&raw.data$shelter==0,])
disp.params[18,] <- param.set(raw.data[raw.data$wo==1&raw.data$year12==0&raw.data$year13==0&raw.data$year14==0&raw.data$shelter==1,])
disp.params[19,] <- param.set(raw.data[raw.data$wo==0&raw.data$year12==1&raw.data$shelter==0,])
disp.params[20,] <- param.set(raw.data[raw.data$wo==0&raw.data$year12==1&raw.data$shelter==1,])
disp.params[21,] <- param.set(raw.data[raw.data$wo==1&raw.data$year12==1&raw.data$shelter==0,])
disp.params[22,] <- param.set(raw.data[raw.data$wo==1&raw.data$year12==1&raw.data$shelter==1,])
disp.params[23,] <- param.set(raw.data[raw.data$wo==0&raw.data$year13==1&raw.data$shelter==0,])
disp.params[24,] <- param.set(raw.data[raw.data$wo==0&raw.data$year13==1&raw.data$shelter==1,])
disp.params[25,] <- param.set(raw.data[raw.data$wo==1&raw.data$year13==1&raw.data$shelter==0,])
disp.params[26,] <- param.set(raw.data[raw.data$wo==1&raw.data$year13==1&raw.data$shelter==1,])
disp.params[27,] <- param.set(raw.data[raw.data$wo==0&raw.data$year14==1&raw.data$shelter==0,])
disp.params[28,] <- param.set(raw.data[raw.data$wo==0&raw.data$year14==1&raw.data$shelter==1,])
disp.params[29,] <- param.set(raw.data[raw.data$wo==1&raw.data$year14==1&raw.data$shelter==0,])
disp.params[30,] <- param.set(raw.data[raw.data$wo==1&raw.data$year14==1&raw.data$shelter==1,])
disp.params[31,] <- param.set(raw.data)

disp.params[8,] <- disp.params[7,]
disp.params[17,] <- disp.params[15,]
disp.params[18,] <- disp.params[16,]
#For deer exclosure
disp.params[c(14,24,28,29,30),8] <- 0
disp.params[20,9] <- 0

row.names(disp.params) <- c('AllYrAllTrtBO','AllYrAllTrtWO','AllYrContBO','AllYrContWO',
                            'AllYrSheltBO','AllYrSheltWO','y11BO','y11WO','y12BO','y12WO','y13BO','y13WO','y14BO','y14WO',
                            'y11ContBO','y11SheltBO','y11ContWO','y11SheltWO','y12ContBO','y12SheltBO','y12ContWO','y12SheltWO',
                            'y13ContBO','y13SheltBO','y13ContWO','y13SheltWO','y14ContBO','y14SheltBO','y14ContWO','y14SheltWO',
                            'AllyrAllTrt')

hist(raw.data$dist[raw.data$removed==1&raw.data$wo==0],freq=F)
x = seq(0,30,0.01)
lines(x,dweibull(x,disp.params[1,4],disp.params[1,3]))

#Species combined

disp.params <- data.frame(matrix(nrow=4,ncol=9))
names(disp.params) <- c('dispProb','lam','weibSc','weibSh','2dtA','2dtB','dispEatenProb','cacheProb','undispEatenprob')

disp.params[1,] <- param.set(raw.data[raw.data$year12==0&raw.data$year13==0&raw.data$year14==0,])
disp.params[2,] <- param.set(raw.data[raw.data$year12==1,])
disp.params[3,] <- param.set(raw.data[raw.data$year13==1,])
disp.params[4,] <- param.set(raw.data[raw.data$year14==1,])


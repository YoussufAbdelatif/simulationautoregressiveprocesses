set.seed(5000)
ar_process_1=c()
ar_process_2=c()
ar_process_3=c()

  n=10000
  new_process_1 <- arima.sim(list(order = c(1, 0, 0), ar = 0.9), n = n, sd = 1)
  ar_process_1=c(ar_process_1,new_process_1)
  
  
  new_process_2 <- arima.sim(list(order = c(1, 0, 0), ar = 0.1), n = n, sd = 1)
  ar_process_2=c(ar_process_2,new_process_2)
  
  
  x=numeric(n)
  for(i in 2:length(x)){
    x[i] <- 1 * x[i - 1] + rnorm(1)
  }
  new_process_3=x
  ar_process_3=c(ar_process_3,new_process_3)

par(mfrow=c(3,1))
  
  for (i in seq(1000,10000,by=500)) {

  Sys.sleep(1)
  print(plot(ar_process_1[1:i],type="l",main = "ALPHA = 0.9; STATIONARY"))
  print(abline(h=0,col="red"))
  print(plot(ar_process_2[1:i],type="l",main = "ALPHA = 0.1; STATIONARY"))
  print(abline(h=0,col="red"))
  print(plot(ar_process_3[1:i],type="l",main = "ALPHA = 1; NON-STATIONARY"))
  print(abline(h=0,col="red"))
  Sys.sleep(3)
  }

  
  


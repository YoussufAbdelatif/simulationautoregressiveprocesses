
ar_process_1=c()
ar_process_2=c()
ar_process_3=c()

par(mfrow=c(3,1))
for (i in seq(100,5000,by=100)) {
  n=i
  new_process_1 <- arima.sim(list(order = c(1, 0, 0), ar = 0.9), n = n, sd = 1)
  ar_process_1=c(ar_process_1,new_process_1)

  
  n=i
  new_process_2 <- arima.sim(list(order = c(1, 0, 0), ar = 0.1), n = n, sd = 1)
  ar_process_2=c(ar_process_2,new_process_2)

  
  n=i
  x=numeric(n)
  for(i in 2:length(x)){
    x[i] <- 1 * x[i - 1] + rnorm(1)
  }
  new_process_3=x
  ar_process_3=c(ar_process_3,new_process_3)
  
  Sys.sleep(1)
  print(plot(ar_process_1,type="l",main = "ALPHA = 0.9; STATIONARY"))
  print(abline(h=0,col="red"))
  print(plot(ar_process_2,type="l",main = "ALPHA = 0.1; STATIONARY"))
  print(abline(h=0,col="red"))
  print(plot(ar_process_3,type="l",main = "ALPHA = 1; NON-STATIONARY"))
  print(abline(h=0,col="red"))
  Sys.sleep(3)
  dev.off()
  par(mfrow=c(3,1))
  
}

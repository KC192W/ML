n <- 25
x <- c(3.28,2.42,3.19 ,4.13 ,5.51 ,4.37 ,5.51 ,6.22 ,6.15 ,2.80 ,3.39 ,3.13 ,3.65 ,3.88 ,5.50 ,4.01 ,4.84, 5.43 ,7.86 ,5.61 ,3.31 ,4.14 ,4.50 ,4.16 ,4.64)
m_x<- mean(x)
m_x
s_x<-var(x)
s_x
ASL1 <- numeric();
CI1 <- numeric()
for(i in 1:10000){
  B <- 100;Boot <- numeric();Boot_s <- numeric();numb <- 0
  for(b in 1:B){
    ind <- sample(1:n,size=n,replace = T)
    boot<- mean(x[ind])
    boot_s<- var(x[ind])
    Boot[b]<- boot   # boot sample mean
    Boot_s[b]<- boot_s # boot sample variance
    if (boot<m_x){
      numb=numb+1
    }else{
      numb=numb+0
    }
  }
  ASL1[i]<- numb/B
  
  Boot<- sort(Boot)
  L <- Boot[5]
  H <- Boot[95]
  CI1[i] <- H-L
}
mean(ASL1)
mean(CI1)
Boot
Boot_s

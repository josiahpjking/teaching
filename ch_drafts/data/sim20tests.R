
simtest <- function(){
  df = as.data.frame(MASS::mvrnorm(n=20,mu=rep(0,20),Sigma=diag(20)))
  df$catdog = rep(c("cat","dog"), e=10)
  ps = list(1:20)
  for(i in 1:20){
    ps[i]<-t.test(df[,i]~df[,21])$p.value
  }
  sum(unlist(ps)<.05)
}

res = replicate(1e3, simtest())
barplot(table(res))

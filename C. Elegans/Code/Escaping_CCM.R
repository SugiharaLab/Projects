library(rEDM)

do_ccm_escaping <- function(tp,m, E = 10){
  out <- ccm(m, target_column = 1, lib_column = 2, lib_sizes = 100, random_libs = T, E = E,
             num_samples = 50, tp = tp, silent = T, exclusion_radius = 10)
  return(mean(out$rho))
}

worms = list.files(paste0(getwd(),"/For Github/Data/Escape Response Worms"))
totals_Escaping = array(NA, dim = c(41,4,4,length(worms)))
count = 1
for(w in worms){
  print(w)
  eigens = read.csv(paste0(getwd(),"/For Github/Data/Escape Response Worms/",w), header = T)
  eigens = eigens[200:600,] #the first 200 timepoints are pre-stimulus and are thus excluded.
  for(i in c(1:4)){
    for(j in c(1:4)){
      if(i != j){
        m = cbind(eigens[,i], eigens[,j])
        tps = mclapply(c(-40:0), do_ccm_escaping, m=m, mc.cores = 7)
        tps = t(as.data.frame(tps))
        totals_Escaping[,i,j,count] <- tps
      }
    }
  }
  count = count +1
}


optimal_tps = array(NA, dim = c(4,4))
par(mfrow = c(4,4), mai = c(.3,.3,.3,.3))
for(i in c(1:4)){
  for(j in c(1:4)){
    if(i != j){
      plot(0,0, xlim =c(-2,0), ylim = c(0,1), col = "white")
      t1 = {}
      for(p in c(1:ncol(totals_Escaping[,i,j,]))){
        lne = normalize(totals_Escaping[,i,j,p])
        t1 = rbind(t1, lne)
        lines(c(-40:0)/20, lne, col = rgb(.2,.2,.2,.15))
      }
      avg1 = normalize(colMeans(t1, na.rm = T))
      
      optimal_tps[i,j] <-c(-40:0)[which.max(avg1)]
      
      lines(c(-40:0)/20,avg1, lwd = 3, col = 'blue')
    }else{
      plot.new()
    }
  }
}


########### Optimal E #########


do_ccm_escaping <- function(E,m,tp){
  out <- ccm(m, target_column = 1, lib_column = 2, lib_sizes = 50, random_libs = T, E = E,
             num_samples = 50, tp = tp, silent = T, exclusion_radius = 10)
  return(mean(out$rho))
}

Es = c(2:40)
worms = list.files(paste0(getwd(),"/For Github/Data/Escape Response Worms"))
totals_E_escaping = array(NA, dim = c(4,4,length(worms)))
count = 1
for(w in worms){
  print(w)
  eigens = read.csv(paste0(getwd(),"/For Github/Data/Escape Response Worms/",w), header = T)
  eigens = eigens[200:600,] #the first 200 timepoints are pre-stimulus and are thus excluded.
  for(i in c(1:4)){
    for(j in c(1:4)){
      if(i != j){
        
        
        tp = optimal_tps[i,j]
        m = cbind(eigens[,i], eigens[,j])
        out <- mclapply(Es, do_ccm_escaping, m=m, tp=tp, mc.cores = 7)
        out = t(as.data.frame(out))
        
        totals_E_escaping[i,j,count] <- Es[which.max(out)]
      }
    }
  }
  count = count +1
}
par(mfrow = c(4,1), mai = c(.3,.3,.3,.3))
for(i in c(1:4)){
  hist(totals_E_escaping[i,,], breaks = 10, col = rgb(.1,.1,.9,.5), main = i)
}


#We store all output in the "totals_foraging" array. This contains all 33 CCM values (tp = -32:0) for each pair of eigenworms for all 12 individuals. 

#This will loop through and read the .txt file for each of the timeseries of the 12 foraging worms
totals_foraging = array(NA, dim = c(33,4,4,12))
tps = c(-32:0)
for(n in c(1:12)){
  print(n)
  eigens=  read.csv(paste("/Data/Foraging Worms/w",n,"/tracking.txt",sep = ""), header = F, sep = "")
  out = mclapply(as.data.frame(eigens), do_worm_ccm_TP, mc.cores = detectCores()-1)
  out = as.data.frame(out)
  loop = 1
  for(i in c(1:4)){
    for(j in c(1:4)){
      totals_foraging[,i,j,n] <- out[,loop]
      loop = loop+1
    }
  }
}

optimal_tps = array(NA, dim = c(4,4))
par(mfrow = c(4,4), mai = c(.3,.3,.3,.3))
for(i in c(1:4)){
  for(j in c(1:4)){
    if(i != j){
      plot(0,0, xlim =c(-2,0), ylim = c(0,1), col = "white")
      t1 = {}
      for(p in c(1:ncol(totals_foraging[,i,j,]))){
        lne = normalize(totals_foraging[,i,j,p])
        t1 = rbind(t1, lne)
        lines(tps/16, lne, col = rgb(.2,.2,.2,.15))
      }
      avg1 = normalize(colMeans(t1, na.rm = T))
      
      optimal_tps[i,j] <-tps[which.max(avg1)]
      
      lines(tps/16,avg1, lwd = 3, col = 'red')
    }else{
      plot.new()
    }
  }
}



################## TESTING OPTIMAL E ##################

#We store all output in the "totals_foraging" array. This contains all 33 CCM values (tp = -32:0) for each pair of eigenworms for all 12 individuals. 

#This will loop through and read the .txt file for each of the timeseries of the 12 foraging worms
totals_E = array(NA, dim = c(4,4,12))
Es = c(2:40)
for(n in c(1:12)){
  print(n)
  eigens=  read.csv(paste("/Data/Foraging Worms/w",n,"/tracking.txt",sep = ""), header = F, sep = "")
  
  for(i in c(1:4)){
    print(i)
    for(j in c(1:4)){
     tp = optimal_tps[i,j]
      
      if(i != j){
        out <- mclapply(Es,do_worm_ccm_E,  target =eigens[,i],
                        lib = eigens[,j],mc.cores = detectCores()-1, trials = 10, samples = 100)
        out = t(as.data.frame(out))
        totals_E[i,j,n] <- Es[which.max(out)]
      }
    }
  }
  
  
}
par(mfrow = c(4,1), mai = c(.3,.3,.3,.3))
for(i in c(1:4)){
  hist(totals_E[i,,], breaks = 10, col = rgb(.9,.1,.1,.5), main = i)
}

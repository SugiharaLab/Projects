load("Code Output/Escaping_CCM_Outputs.RData")
load("Code Output/Foraging_CCM_Output.RData")
load("helper_functions.RData")
#### Differences Between Foraging Worms #######
totals = totals_foraging
ds_f = {}
for(i in c(1:12)){
  for(j in c(1:12)){
    if(i!= j){
      ds = {}
      for(x in c(1:4)){
        for(y in c(1:4)){
          if(x!= y){
            a=totals[,x,y,i]
            b=totals[,x,y,j]
            
            a2 = normalize(a)
            b2 = normalize(b)
            
            a2 = a2[which(a>0 & b>0)]
            b2 = b2[which(a>0 & b>0)]
            
            d = mean(abs(a2 - b2), na.rm = T)
            ds = c(ds,d)
            
          }
        }
      }
      ds_f = c(ds_f, mean(ds,na.rm=T))
    }
  }
}
boxplot(ds_f)


####### Differences Between Escaping Worms

totals = totals_Escaping
ds_e = {}
for(i in c(1:91)){
  for(j in c(1:91)){
    if(i!= j){
      ds = {}
      for(x in c(1:4)){
        for(y in c(1:4)){
          if(x!= y){
            a=totals[,x,y,i]
            b=totals[,x,y,j]
            
            a2 = normalize(a)
            b2 = normalize(b)
            
            a2 = a2[which(a>0 & b>0)]
            b2 = b2[which(a>0 & b>0)]
            
            d = mean(abs(a2 - b2), na.rm = T)
            ds = c(ds,d)
            
          }
        }
      }
      ds_e = c(ds_e, mean(ds,na.rm=T))
    }
  }
}
boxplot(ds_f, ds_e)


###### Differences between Foraging and Escaping

# Because foraging and escaping are not sampled at the same frequency,
# This function interpolates data to make them appear at the same frequency.
interpolate <- function(vals, times, out_f = .1){
  fit = {}
  desired = seq(-2, 0, out_f)
  for(d in desired){
    df = times-d
    df[is.na(df)] = -100 
    found = F 
    i = 2
    while(found == F){
      if(df[(i-1)] <= 0 & df[i] >= 0){
        found = T
      }
      
      i = i+1
      if(i>500){print(i)}
    }
    i = i-1
    v1 = vals[i]
    v2 = vals[i-1]
    
    slope = (v2-v1)/(times[i] - times[i-1])
    
    v = vals[i-1] + df[i-1]*slope
    fit = c(fit, v)
  }
  return(fit)
}

times_f = seq(-2,0,1/16)
times_e = seq(-2,0, 1/20)
d_between = {}
for(i in c(1:12)){
  for(j in c(1:91)){
    ds = {}
    for(x in c(1:4)){
      for(y in c(1:4)){
        if(x!= y){
          a = totals_foraging[,x,y,i]
          b = totals_Escaping[,x,y,j]
          
          a = interpolate(a, times_f, .1)
          b = interpolate(b, times_e, .1)
          a = normalize(a)
          b = normalize(b)
          
          a2 = a[which(a>0 & b>0)]
          b2 = b[which(a>0 & b>0)]
          
          d = mean(abs(a2-b2))
          ds = c(ds,d)
          
        }
        
      }
      
    }
    d_between = c(d_between, mean(ds))
  }
}

boxplot( ds_f,ds_e, d_between )

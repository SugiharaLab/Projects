normalize <- function(data, method = 1){
  ts = data[!is.na(data)]
  
  if(method == 1){
    data = (data - min(ts))/(max(ts)-min(ts))
  }
  if(method == 2){
    data = (data - mean(ts))/(sd(ts))
  }
  return(data)
}

perform_ccm <- function(tp, library_index, target_index, indexes, E = 10){
  
  library_ts = eigens[(abs(tp)+1):nrow(eigens),library_index]
  target_ts = eigens[1:(nrow(eigens)-abs(tp)),target_index]
  
  block = cbind(target_ts[E:length(target_ts)], embed(library_ts, E))
  block = block[indexes,]
  out <- block_lnlp(block, target_column = 1, columns = c(2:ncol(block)),
                    method = 'simplex', tp = 0, silent = T)
  return(out$rho)
}
calculate_interaction_profile <- function(eigens, tps, num_eigens = 4, E = 10, random_indexes = F,
                                          random_window = F, num_samples = 100,
                                          normalize = T, negative_zero = T){
  
  if(random_window){
    index = sample( (num_samples+1):(nrow(eigens) + min(tps) - E), 1 )
    indexes = c((index-num_samples):index )
  }
  if(random_indexes){
    indexes = sample( 1: (nrow(eigens) - E + min(tps)), num_samples )
  }
  profile = array(NA, dim = c(num_eigens,num_eigens, length(tps) ) )
  for(i in c(1:num_eigens)){
    for(j in c(1:num_eigens)){
      if( i != j ){
  
        output = unlist(mclapply(c(-32:0), perform_ccm, target_index = i, library_index = j,indexes = indexes, mc.cores = 8))
        if(negative_zero){
          output[output < 0] = 0
        }
        if(normalize){
          profile[i,j,] <- normalize(output)
        }else{
          profile[i,j,] <- output
        }
      }
    }
  }
  return(profile)
}

tps = c(-32:0)
s = 10
for(n in c(1:12)){
  print(n)
  eigens=  read.csv(paste("./Data/Foraging Worms/w",n,"/tracking.txt",sep = ""), header = F, sep = "")
  all_profiles = array(NA, dim = c(4,4,33,s))
  for(i in c(1:s)){
    profile = calculate_interaction_profile(eigens, random_window = T,
                                            random_indexes = F, normalize = T,
                                            tps = c(-32:0), num_samples = 100)
    all_profiles[,,,i] <- profile
  }
  diffs = {}
  for(i in c(1:s)){
    for(j in c(1:s)){
      if(i != j){
        profile_a = all_profiles[,,,i]
        profile_b = all_profiles[,,,j]
        diff = as.vector(abs(profile_a - profile_b))
        diff = mean(diff[!is.na(diff)])
        diffs = c(diffs, diff)
      }
      
    }
  }
  boxplot(diffs, d2)
  
  test = apply(all_profiles, c(1,2,3), mean)
}




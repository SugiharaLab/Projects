load("/Users/eriksaberski/Desktop/C. Elegans Analysis/For Github/helper_functions.RData")
removes = {}
genes_with_data={}
genes = list.files("/Users/eriksaberski/Desktop/C. Elegans Analysis/For Github/Data/Strains CCM Profiles")
count = 1
all_interaction_profiles = array(NA, dim = c(length(genes),4,4,11))
for(g in genes){
  load(paste0("/Users/eriksaberski/Desktop/C. Elegans Analysis/For Github/Data/Strains CCM Profiles/",g))
  if(length(dim(interaction_profiles)) == 4){
    char = ""
    index = 1
    while(char != "."){
      char = substr(g, start = index, stop = index)
      index = index +1
    }
    n = substr(g, start = 1, stop = index-2)
    genes_with_data = c(genes_with_data,n)
    
    for(i in c(1:4)){
      for(j in c(1:4)){
        if(i != j){
          t = {}
          for(p in c(1:ncol(interaction_profiles[,i,j,]))){
            curr = interaction_profiles[,i,j,p]
            curr[curr<0] = 0
            if(sum(is.na(curr)) ==0){
              t = rbind(t, normalize(curr))
            }
           
            
          }
          avg = colMeans(t, na.rm = T)
          if(length(avg) == 11 & sum(is.na(avg)) == 0){
            all_interaction_profiles[count,i,j,] <- normalize(avg) 
          }
        }
      }
    }
  }else{removes = c(removes, count)}
  count = count + 1
  
}
all_interaction_profiles = all_interaction_profiles[-removes,,,]

distances = array(NA, dim =c(length(genes_with_data),length(genes_with_data)) )
for(i in c(1:length(genes_with_data))){
  for(j in c(1:length(genes_with_data))){
    d = {}
    for(l1 in c(1:4)){
      for(l2 in c(1:4)){
        if(l1 != l2){
          a = all_interaction_profiles[i,l1,l2,]
          b = all_interaction_profiles[j,l1,l2,]
          # a2 = a[which(a>0 & b>0)]
          # b2 = b[which(a>0 & b>0)]
          a2 = a
          b2 = b
          d = c(d, median(abs(a2-b2)))
        }
      }
    }
    distances[i,j] <- mean(d, na.rm = T)
    
  }
}
colnames(distances) = genes_with_data
rownames(distances) = genes_with_data
median(distances, na.rm = T)



phenos = read.csv("/Users/eriksaberski/Desktop/C. Elegans Analysis/For Github/Data/Classifications_from_Brown2103.csv")
classifications = unique(as.character((phenos$Phenotypic.or.Functional.Class)))
classifiers = {}
removes = {} #Here we remove based on what is in "phenos" from brown et al.
count = 1
for(n in genes_with_data){
  
  
  if(n %in% phenos$Strain){
    index = which(phenos$Strain == n)
    class = as.character(phenos$Phenotypic.or.Functional.Class[index])
    classifiers = c(classifiers,class)
  }else{
    removes = c(removes, count)
  }
  count = count + 1
}
distances = distances[-removes,-removes]
genes_with_data=genes_with_data[-removes]

sub_distances = {}
for(c in classifications){
  indexes = which(classifiers == c)
  distances_sub_matrix = as.vector(distances[indexes,indexes])
  distances_sub_matrix = distances_sub_matrix[distances_sub_matrix>0]
  plot = boxplot(distances_sub_matrix, plot = F)
  sub_distances = cbind(sub_distances, plot$stats)
  
  pval = t.test(distances_sub_matrix,as.vector(distances[distances>0]),alternative = "less" )
  
}


names_for_boxplot = classifications[order(sub_distances[3,], decreasing = T)]
sub_distances=sub_distances[,order(sub_distances[3,], decreasing = T)]

dev.off()
par(mai = c(2,3,.2,0.2))
boxplot(sub_distances, horizontal = T, col = 'grey', names = names_for_boxplot, las = 1)
abline(v = mean(distances,na.rm = T), col = 'red', lwd = 2)



########## HSN Analysis ##########
hsn_info = read.csv("/Users/eriksaberski/Desktop/C. Elegans Analysis/For Github/Data/HSN_categories.csv", header = F)
g1 = as.character(hsn_info[,1])
g2 = as.character(hsn_info[,2])

g1s = {}
for(n in g1){
  index = which(genes_with_data == n)
  
  g1s = c(g1s, index)
}
g2s = {}
for(n in g2){
  index = which(genes_with_data == n)
  g2s = c(g2s, index)
}
g1_distances = as.vector(distances[g1s,g1s])
g1_distances= unique(g1_distances[g1_distances>0 & !is.na(g1_distances)])

g2_distances = as.vector(distances[g2s,g2s])
g2_distances= unique(g2_distances[g2_distances>0 & !is.na(g2_distances)])

boxplot(g2_distances,g1_distances, col = 'chocolate',
        horizontal = T,
        names = c("HSN Not Affected", "HSN Affected"),
        las =2)
t.test(g1_distances,g2_distances)
t.test(g1_distances, as.vector(distances))



############## CLustering Analysis (Figure 4b)


library(corrplot)
output <- array(NA, c(9,9))
pval_matrix = array(NA, c(9,9))
count1 = 1
for(c1 in names_for_boxplot){
  print(c1)
  count2 = 1
  indexes1 = which(classifiers == c1)
  
  
  for(c2 in names_for_boxplot){
    
    indexes2 = which(classifiers == c2)
    all_indexes = c(indexes1, indexes2)
    distances_sub_matrix = distances[all_indexes,all_indexes]
    
    correct = 0
    for(i in c(1:length(indexes1))){
      curr = distances_sub_matrix[,i]
      curr = curr[curr>0]
      closest = which.min(curr)
      
      if(closest %in% c(1:length(indexes1))){
        correct = correct + 1
      }
    }
    p_correct = correct/length(indexes1)
    
    corrects = {}
    for(trial in c(1:10000)){
      correct = 0
      r = sample(1:length(all_indexes), length(indexes1))
      indexes1_r = all_indexes[r]
      indexes2_r = all_indexes[c(1:length(all_indexes))[-r]]
      
      for(i in c(1:length(indexes1))){
        curr = distances_sub_matrix[,i]
        curr = curr[curr>0]
        closest = which.min(curr)
        
        if(closest %in% r){
          correct = correct + 1
        }
      }
      corrects = c(corrects, correct/length(indexes1))
    }
    
    p_val = sum(corrects>p_correct)/10000
    pval_matrix[count1,count2] = p_val
    output[count1,count2] <- p_correct
    count2 = count2+1
  }
  count1 = count1+1
}
colnames(output)<-names_for_boxplot
rownames(output)<-names_for_boxplot
output = output*2-1
diag(output) <- 0
# corrplot(output, tl.col = 'black', method = "color",
#   outline = T, cl.pos="n")
colnames(pval_matrix)<-names_for_boxplot
rownames(pval_matrix)<-names_for_boxplot
pval_plot  = pval_matrix
pval_plot[pval_matrix>0.11] = 1
pval_plot[pval_matrix<0.11 & pval_matrix > 0.06] = 0
pval_plot[pval_matrix<0.06] = -1
diag(pval_plot) <- 1



colfunc <- colorRampPalette(c("white", "forestgreen"))
cols = colfunc(100)

pval_plot  = pval_matrix
pval_plot = 1-pval_plot
pval_plot[pval_plot<.66] = .66
pval_plot = ((pval_plot-0.66) * 2.8)*2 - 1
diag(pval_plot) <- -1

corrplot(pval_plot, tl.col = 'black', method = "color",
         outline = T, cl.pos="n", col =cols)










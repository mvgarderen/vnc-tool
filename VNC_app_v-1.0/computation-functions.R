
extractNB <- function(x,y,v_total,nb_mask,dims){
  # move to lower left corner of the neighbourhood
  x <- x-dims$rx
  y <- y-dims$ry 
  # create matrix
  nb_matrix <- matrix(nrow=dims$my,ncol=dims$mx)
  # for all elements in the neighbourhood
  for(i in 0:(dims$mx-1)){
    for(j in 0:(dims$my-1)){
      # if this mask cell is within the range of v_total
      if((x+i>0) & (x+i<=dims$vx) & (y+j>0) & (y+j<=dims$vy)){ 
        # if this cell is in the neighbourhood 
        if(nb_mask[j+1,i+1] >= 0){
          # copy the value
          nb_matrix[j+1,i+1] <- v_total[y+j,x+i]
        }
      }
    }
  }
  return(nb_matrix)
}

extractNB2 <- function(x,y,v_total1,v_total2,nb_mask,vs_matrix,dims){
  # move to lower left corner of the neighbourhood
  x <- x-dims$rx
  y <- y-dims$ry 
  # create matrix
  nb_matrix <- matrix(nrow=dims$my,ncol=dims$mx)
  # for all elements in the neighbourhood
  for(i in 0:(dims$mx-1)){
    for(j in 0:(dims$my-1)){
      # if this mask cell is within the range of v_total
      if((x+i>0) & (x+i<=dims$vx) & (y+j>0) & (y+j<=dims$vy)){ 
        # if this cell is in the neighbourhood 
        if(nb_mask[j+1,i+1] >= 0){ # copy the appropriate value
          if(vs_matrix[j+1,i+1]==1) nb_matrix[j+1,i+1] <- v_total1[y+j,x+i]
          if(vs_matrix[j+1,i+1]==2) nb_matrix[j+1,i+1] <- v_total2[y+j,x+i]
        }
      }
    }
  }
  return(nb_matrix)
}

globalAvg <- function(v_total,nb_mask,dims,vsmode){# computes v_avg for all cells
  # create matrix v_avg
  v_avg <- array(dim = c(dims$vy,dims$vx))
  # loop to compute AVG for all cells, store result in v_avg
  for(i in 1:dims$vx){
    for(j in 1:dims$vy){
      if(is.na(v_total[j,i])){
        v_avg[j,i] <- NA
      }
      else{
        nb_values <- as.numeric(extractNB(i,j,v_total,nb_mask,dims))
        v_avg[j,i] <- mean(nb_values,na.rm = TRUE)
      }
    }
    incProgress(1/(vsmode*dims$vx), detail = paste(i,"/",dims$vx))
  }
  # return v_avg
  return(v_avg)
}

globalProm <- function(v_avg,v_total,dims,vsmode){# computes v_prom for all cells
  # create matrix v_prom
  v_prom <- array(dim = c(dims$vy,dims$vx))
  # loop to compute PROM for all cells, store result in v_prom
  for(i in 1:dims$vx){
    for(j in 1:dims$vy){
      if(is.na(v_total[j,i])){
        v_prom[j,i] <- NA
      }
      else{
        v_prom[j,i] <- v_total[j,i]-v_avg[j,i]
      }
    }
    incProgress(1/(vsmode*dims$vx), detail = paste(i,"/",dims$vx))
  }
  # return v_prom
  return(v_prom)
}

globalExtremes <- function(v_total,nb_mask,dims,vsmode){# computes extreme values for all cells
  # create matrices
  v_min <- array(dim = c(dims$vy,dims$vx))
  v_max <- array(dim = c(dims$vy,dims$vx))
  v_range <- array(dim = c(dims$vy,dims$vx))
  
  # loop to compute values for all cells, store result in matrices
  for(i in 1:dims$vx){
    for(j in 1:dims$vy){
      if(is.na(v_total[j,i])){# if not in study area, don't compute a value
        v_min[j,i] <- NA
        v_max[j,i] <- NA
        v_range[j,i] <- NA
      }
      else{# otherwise, compute values
        nb_values <- as.numeric(extractNB(i,j,v_total,nb_mask,dims))
        v_min[j,i] <- min(nb_values, na.rm = TRUE)
        v_max[j,i] <- max(nb_values, na.rm = TRUE)
        v_range[j,i] <- v_max[j,i]-v_min[j,i]
      }
    }
    incProgress(1/(vsmode*dims$vx), detail = paste(i,"/",dims$vx))
  }
  
  # return results
  return(list(v_min,v_max,v_range))
}

groupAnalysis <- function(v_total,nb_mask,dims,vsmode){# computes the groups with extreme values
  # create matrices
  v_minavg <- array(dim = c(dims$vy,dims$vx))
  v_maxavg <- array(dim = c(dims$vy,dims$vx))
  v_minval <- array(dim = c(dims$vy,dims$vx))
  v_maxval <- array(dim = c(dims$vy,dims$vx))
  v_minrange <- array(dim = c(dims$vy,dims$vx))
  v_maxrange <- array(dim = c(dims$vy,dims$vx))
  # loop to compute values for all cells, store result in matrices
  for(i in 1:dims$vx){
    for(j in 1:dims$vy){
      if(is.na(v_total[j,i])){# if not in study area, don't compute a value
        v_minavg[j,i] <- NA
        v_maxavg[j,i] <- NA
        v_minval[j,i] <- NA
        v_maxval[j,i] <- NA
        v_minrange[j,i] <- NA
        v_maxrange[j,i] <- NA
      }
      else{# otherwise, compute values
        nb_matrix <- extractNB(i,j,v_total,nb_mask,dims)
        extremes <- localGroupAnalysis(nb_matrix,nb_mask)
        v_minavg[j,i] <- extremes[1]
        v_maxavg[j,i] <- extremes[2]
        v_minval[j,i] <- extremes[3]
        v_maxval[j,i] <- extremes[4]
        v_minrange[j,i] <- extremes[5]
        v_maxrange[j,i] <- extremes[6]
      }
    }
    incProgress(1/(vsmode*dims$vx), detail = paste(i,"/",dims$vx))
  }
  # return results
  return(list(v_minavg,v_maxavg,v_minval,v_maxval,v_minrange,v_maxrange))
}


localGroupAnalysis <- function(nb_matrix,nb_mask){
  # create group table
  groups <- getGroups()
  restab <- data.frame(group=groups,avg=NA,min=NA,max=NA,range=NA)
  # compute min/max/range for each group
  for(i in 1:nrow(restab)){
    nb_group <- nb_matrix[which(nb_mask==groups[i])]
    restab[i,"avg"] <- mean(nb_group,na.rm = TRUE)
    restab[i,"min"] <- min(nb_group,na.rm = TRUE)
    restab[i,"max"] <- max(nb_group,na.rm = TRUE)
    restab[i,"range"] <- restab[i,"max"]-restab[i,"min"]
  }
  # filter out rows corresponding to empty groups
  restab <- restab[!is.nan(restab$avg),]
  # find the minimum/maximum avg/value/range values
  minavgval <- min(restab$avg,na.rm = TRUE)
  maxavgval <- max(restab$avg,na.rm = TRUE)
  minval <- min(restab$min,na.rm = TRUE)
  maxval <- max(restab$max,na.rm = TRUE)
  minrangeval <- min(restab$range,na.rm = TRUE)
  maxrangeval <- max(restab$range,na.rm = TRUE)
  # find the group(s) containing this value
  minavggroup <- restab[restab$avg==minavgval,"group"]
  maxavggroup <- restab[restab$avg==maxavgval,"group"]
  mingroup <- restab[restab$min==minval,"group"]
  maxgroup <- restab[restab$max==maxval,"group"]
  minrangegroup <- restab[restab$range==minrangeval,"group"]
  maxrangegroup <- restab[restab$range==maxrangeval,"group"]
  # convert to string
  minavggroup <- paste(as.character(minavggroup),collapse = ", ")
  maxavggroup <- paste(as.character(maxavggroup),collapse = ", ")
  mingroup <- paste(as.character(mingroup),collapse = ", ")
  maxgroup <- paste(as.character(maxgroup),collapse = ", ")
  minrangegroup <- paste(as.character(minrangegroup),collapse = ", ")
  maxrangegroup <- paste(as.character(maxrangegroup),collapse = ", ")
  # return results
  return(c(minavggroup,maxavggroup,mingroup,maxgroup,minrangegroup,maxrangegroup))
}


computeRMSE1 <- function(v_total,nb_mask,exp_matrix,dims,global,grouped){# computes RMSE for all cells
  # create matrix
  if(global) v_rmse <- array(dim = c(dims$vy,dims$vx))
  if(grouped) v_grmse <- array(dim = c(dims$vy,dims$vx))
  # loop to compute RMSE for all cells, store result in matrix
  for(i in 1:dims$vx){
    for(j in 1:dims$vy){
      if(is.na(v_total[j,i])){# if not in study area, don't compute a value
        if(global) v_rmse[j,i] <- NA
        if(grouped) v_grmse[j,i] <- NA
      }
      else{# otherwise, compute RMSE
        nb_matrix <- extractNB(i,j,v_total,nb_mask,dims)
        if(global) v_rmse[j,i] <- localRMSE(nb_matrix,exp_matrix,dims)
        if(grouped) v_grmse[j,i] <- groupRMSE(nb_matrix,exp_matrix,nb_mask,dims)
      }
    }
    incProgress(1/dims$vx, detail = paste(i,"/",dims$vx))
  }
  # return results
  if(global & grouped)  return(list(v_rmse,v_grmse))
  else if(global) return(v_rmse)
  else return(v_grmse)
}

computeRMSE2 <- function(v_total1,v_total2,nb_mask,exp_matrix,vs_matrix,dims,global,grouped){# computes RMSE for all cells
  # create matrix
  if(global) v_rmse <- array(dim = c(dims$vy,dims$vx))
  if(grouped) v_grmse <- array(dim = c(dims$vy,dims$vx))
  # loop to compute RMSE for all cells, store result in matrix
  for(i in 1:dims$vx){
    for(j in 1:dims$vy){
      if(is.na(v_total1[j,i]) | is.na(v_total2[j,i])){# if not in study area, don't compute a value
        if(global) v_rmse[j,i] <- NA
        if(grouped) v_grmse[j,i] <- NA
      }
      else{# otherwise, compute RMSE
        nb_matrix <- extractNB2(i,j,v_total1,v_total2,nb_mask,vs_matrix,dims)
        if(global) v_rmse[j,i] <- localRMSE(nb_matrix,exp_matrix,dims)
        if(grouped) v_grmse[j,i] <- groupRMSE(nb_matrix,exp_matrix,nb_mask,dims)
      }
    }
    incProgress(1/dims$vx, detail = paste(i,"/",dims$vx))
  }
  # return results
  if(global & grouped)  return(list(v_rmse,v_grmse))
  else if(global) return(v_rmse)
  else return(v_grmse)
}

# compute root-mean-square error between two matrices 
localRMSE = function(nb_matrix,exp_matrix,dims){
  sqerror <- 0
  count <- 0
  for (i in 1:dims$mx) {
    for (j in 1:dims$my) {
      if(exp_matrix[j,i]!=-1 & !is.na(nb_matrix[j,i])){# if cell is in the neighbourhood and not NA
        sqerror <- sqerror + (exp_matrix[j,i] - nb_matrix[j,i])^2
        count <- count + 1
      }
    }
  }
  sqerror <- sqerror / count
  rmse <- sqrt(sqerror)
  return(rmse)
}

# compute grouped root-mean-square error 
groupRMSE = function(nb_matrix, exp_matrix, nb_mask, dims){
  groups <- getGroups()
  restab <- data.frame(group=groups,sqerror=0,count=0)
  for (i in 1:dims$mx) {
    for (j in 1:dims$my) {
      group <- nb_mask[j,i]
      if(group!=-1 & !is.na(nb_matrix[j,i])){# if cell is in the neighbourhood and not NA
        r <- which(restab$group==group)
        restab[r,"sqerror"] <- restab[r,"sqerror"] + (exp_matrix[j,i] - nb_matrix[j,i])^2
        restab[r,"count"] <- restab[r,"count"] + 1
      }
    }
  }
  restab <- restab[restab$count!=0,] #ignore empty groups
  restab$sqerror <- restab$sqerror/restab$count #compute average error per group
  rmse <- sqrt(mean(restab$sqerror)) #compute rmse weighting each group equally
  return(rmse)
}


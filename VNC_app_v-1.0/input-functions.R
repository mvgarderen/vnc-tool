
getSep <- function(val){# returns the appropriate separator based on the user's choice
  if(val==1) return("")
  if(val==2) return("\t")
  if(val==3) return(";")
  if(val==4) return(",")
}

readVS <- function(file,head,sep,remneg,nonorm){# reads and preprocesses a total viewshed
  # Read input file
  v_total <- as.matrix(read.table(file$datapath,header=head,sep=sep))
  # If selected, remove negative values
  if(input$remneg) v_total[v_total<0] <- NA
  # Unless turned off, normalize values
  if(!nonorm){
    minval <- min(v_total,na.rm=TRUE)
    maxval <- max(v_total,na.rm=TRUE)
    r <- maxval-minval
    for(i in 1:nrow(v_total)){
      for(j in 1:ncol(v_total)){
        if(!is.na(v_total[i,j])){v_total[i,j] <- (v_total[i,j]-minval)/r}
      }
    }
  }
  return(v_total)
}

getNB <- function(){
  validate(need(
    input$nbfile != "" || input$nbmode != 1,
    "Please upload a neighbourhood file or generate a neighbourhood mask"
  ))
  if(input$nbmode==1){# Read neighbourhood mask from file
    file <- input$nbfile
    nb_mask <- as.matrix(read.table(file$datapath,input$nbhead,getSep(input$nbsep)))
  }
  if(input$nbmode==2 & input$equidist==1){# Compute neighbourhood with equal distance bands
    nb_mask <- computeNBequi(input$nbradius,input$nbbands,input$sepfocus)
  }
  if(input$nbmode==2 & input$equidist==2){# Compute neighbourhood with varying distance bands
    vals <- NULL
    for(i in 1:input$nbbands){
      vals <- c(vals, eval(parse(text=paste0('input$bandwidth',i))))
    }
    nb_mask <- computeNBvar(vals,input$sepfocus)
  }
  if(input$nbmode==3){# Create neigbourhood with maximum number of bands
    nb_mask <- computeNBequi(input$gradradius,input$gradradius,input$sepfocus)
  }
  if(input$nbmode==4){# Compute neighbourhood with wedges
    nb_mask <- computeNBwedge(input$wedgenr,input$wedgeradius)
  }
  return(nb_mask)
}

computeNBequi <- function(r,n,sepfocus){# computes a neighbourhood mask with equal bands based on given parameters
  nb_mask <- matrix(data=-1,nrow=2*r+1,ncol=2*r+1)
  for(i in 1:nrow(nb_mask)){
    for(j in 1:ncol(nb_mask)){
      d <- sqrt((r+1-i)^2+(r+1-j)^2)
      if(d<1.01*r){nb_mask[i,j] <- ceiling(d*n/(1.05*r))}
    }
  }
  if(!sepfocus){nb_mask[r+1,r+1] <- 1}
  return(nb_mask)
}

computeNBvar <- function(vals,sepfocus){
  r <- sum(vals)
  n <- length(vals)
  nb_mask <- matrix(data=-1,nrow=2*r+1,ncol=2*r+1)
  for(i in 1:nrow(nb_mask)){
    for(j in 1:ncol(nb_mask)){
      for(b in n:1){
        cr <- sum(vals[1:b])
        d <- sqrt((r+1-i)^2+(r+1-j)^2)
        if(d<1.01*cr){nb_mask[i,j] <- b}
      }
    }
  }
  if(sepfocus){nb_mask[r+1,r+1] <- 0}
  return(nb_mask)
}

computeNBwedge <- function(wedgenr,r){# computes a neighbourhood mask with wedges based on given parameters
  nb_mask <- matrix(data=-1,nrow=2*r+1,ncol=2*r+1)
  for(i in 1:nrow(nb_mask)){
    for(j in 1:ncol(nb_mask)){
      d <- sqrt((r+1-i)^2+(r+1-j)^2)
      if(d<1.01*r){
        if(i>=r+1 & j>r+1){
          if(i>=j) nb_mask[i,j] <- 1
          else nb_mask[i,j] <- 2
        }
        if(i>r+1 & j<=r+1){
          if((i-r-1)<=-(j-r-1)) nb_mask[i,j] <- 7
          else nb_mask[i,j] <- 8
        }
        if(i<r+1 & j>=r+1){
          if(-(i-r-1)<=(j-r-1)) nb_mask[i,j] <- 3
          else nb_mask[i,j] <- 4
        }
        if(i<=r+1 & j<r+1){
          if(i<=j) nb_mask[i,j] <- 5
          else nb_mask[i,j] <- 6
        }
      }
    }
  }
  if(wedgenr==1){
    nb_mask[which(nb_mask==1|nb_mask==2,arr.ind = TRUE)] <- 1
    nb_mask[which(nb_mask==3|nb_mask==4,arr.ind = TRUE)] <- 2
    nb_mask[which(nb_mask==5|nb_mask==6,arr.ind = TRUE)] <- 3
    nb_mask[which(nb_mask==7|nb_mask==8,arr.ind = TRUE)] <- 4
  }
  if(wedgenr==2){
    nb_mask[which(nb_mask==1|nb_mask==8,arr.ind = TRUE)] <- 1
    nb_mask[which(nb_mask==3|nb_mask==2,arr.ind = TRUE)] <- 2
    nb_mask[which(nb_mask==5|nb_mask==4,arr.ind = TRUE)] <- 3
    nb_mask[which(nb_mask==7|nb_mask==6,arr.ind = TRUE)] <- 4
  }
  return(nb_mask)
}

getGroups <- function(){
  groups <- sort(unique(as.numeric(getNB())))
  groups <- groups[groups!=-1]
  return(groups)
}

getExp <- function(){
  if(input$nbmode==3){
    exp_tab <- gradualExp(input$exprange)
  }
  else{
    validate(need(
      input$expfile != "" || input$expmode != 2,
      "Please upload an expectation file or enter the values manually"
    ))
    if(input$expmode==2){
      file <- input$expfile
      exp_tab <- read.table(file$datapath,input$exphead,getSep(input$expsep))
      if(ncol(exp_tab)==2) colnames(exp_tab) <- c("group","value")
      if(ncol(exp_tab)==3) colnames(exp_tab) <- c("group","value","viewshed")
    }
    if(input$expmode==3){
      exp_tab <- generateExp()
    }
  }
  return(exp_tab)
}

generateExp <- function(){
  groups <- getGroups() 
  vals <- NULL
  vss <- NULL
  for(i in 1:length(groups)){
    vals <- c(vals, eval(parse(text=paste0('input$valband',i))))
    if(input$vsmode==2){
      vss <- c(vss, eval(parse(text=paste0('input$vsband',i))))
    }
  }
  if(input$vsmode==1){
    exp_tab <- data.frame(group=groups,value=vals)
  }
  else{
    exp_tab <- data.frame(group=groups,value=vals,viewshed=vss)
  }
  return(exp_tab)
}

gradualExp <- function(exprange){
  groups <- getGroups()
  minexp <- exprange[1]
  maxexp <- exprange[2]
  inc <- (maxexp-minexp)/(length(groups)-1)
  exp_tab <- data.frame(group=groups,value=NA)
  if(input$incdec==1){
    for(i in 0:(nrow(exp_tab)-1)){
      exp_tab[i+1,"value"] <- minexp + i*inc
    }
  }
  else{
    for(i in 0:(nrow(exp_tab)-1)){
      exp_tab[i+1,"value"] <- maxexp - i*inc
    }
  }
  if(input$vsmode==2){
    exp_tab$viewshed <- input$expvs
  }
  return(exp_tab)
}

getExpMatrix <- function(exp_tab,nb_mask){
  exp_matrix <- matrix(data=-1,nrow = nrow(nb_mask),ncol=ncol(nb_mask))
  for(i in 1:nrow(exp_matrix)){
    for(j in 1:ncol(exp_matrix)){
      group <- nb_mask[i,j]
      if(group!=-1){
        exp_matrix[i,j] <- exp_tab[exp_tab$group==group,"value"]
      }
    }
  }
  return(exp_matrix)
}


getVsMatrix <- function(exp_tab,nb_mask){
  vs_matrix <- matrix(data=-1,nrow = nrow(nb_mask),ncol=ncol(nb_mask))
  for(i in 1:nrow(vs_matrix)){
    for(j in 1:ncol(vs_matrix)){
      group <- nb_mask[i,j]
      if(group!=-1){
        vs_matrix[i,j] <- exp_tab[exp_tab$group==group,"viewshed"]
      }
    }
  }
  return(vs_matrix)
}
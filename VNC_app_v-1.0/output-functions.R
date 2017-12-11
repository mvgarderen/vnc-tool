plotNB <- function(nb_mask,labeltext,gradual){# plots a neighbourhood mask
  nb <- melt(nb_mask)
  nb <- nb[nb$value!=-1,] 
  if(gradual){
    p <- ggplot(nb, aes(x = Var2, y = -Var1, fill = value)) +  geom_raster()
    p <- p + theme_bw() + coord_equal() + scale_fill_distiller(palette = "Spectral",direction=-1,name = labeltext)
    p <- p + theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
    p <- p + theme(panel.border = element_blank(),panel.grid = element_blank())
    p <- p + theme(legend.position = "none")
  }
  else{
    p <- ggplot(nb, aes(x = Var2, y = Var1, fill = factor(value))) +  geom_raster()
    p <- p + theme_bw() + coord_equal() + scale_fill_brewer(palette = "Spectral",direction=-1,name = labeltext)
    p <- p + theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
    p <- p + theme(panel.border = element_blank(),panel.grid = element_blank())
    p <- p + theme(legend.text = element_text(size=15),legend.title = element_text(size=20))
  }
  return(p)
}


plotExp <- function(exp_matrix,labeltext,gradual){# plots a neighbourhood mask
  exp_tab <- melt(exp_matrix)
  exp_tab <- exp_tab[exp_tab$value!=-1,]
  if(gradual){
    p <- ggplot(exp_tab, aes(x = Var2, y = -Var1, fill = value)) +  geom_raster()
    p <- p + theme_bw() + coord_equal() + scale_fill_distiller(palette = "YlOrRd",direction=1,name = labeltext)
    p <- p + theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
    p <- p + theme(panel.border = element_blank(),panel.grid = element_blank())
    p <- p + theme(legend.text = element_text(size=15),legend.title = element_text(size=20))
  }
  else{
    p <- ggplot(exp_tab, aes(x = Var2, y = Var1, fill = factor(value))) +  geom_raster()
    p <- p + theme_bw() + coord_equal() + scale_fill_brewer(palette = "YlOrRd",direction=1,name = labeltext)
    p <- p + theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
    p <- p + theme(panel.border = element_blank(),panel.grid = element_blank())
    p <- p + theme(legend.text = element_text(size=15),legend.title = element_text(size=20))
  }
  return(p)
}

writeOut <- function(vs,filename,plotlabel,continuous,outdir,makeplot,nbx,nby){# writes a table and, if selected, a plot
  if(input$edgefilter & nbx>0 & nby>0){
    vs[,1:nbx] <- NA
    vs[,(ncol(vs)-nbx):ncol(vs)] <- NA
    vs[1:nby,] <- NA
    vs[(nrow(vs)-nby):nrow(vs),] <- NA
  }
  if(makeplot){
    if(continuous){p <- plotScapeContinuous(vs,plotlabel)}
    else{p <- plotScapeDiscrete(vs,plotlabel)}
    ggsave(paste0(outdir,filename,"_plot.pdf"),p,width = 10,height = 10,units="in")
  }
  vs[is.na(vs)] <- -9999
  write.table(vs,file = paste0(outdir,filename,".asc"),row.names = FALSE,col.names = FALSE)
}

plotScapeContinuous <- function(vs,labeltext){# plots a visualscape
  if(labeltext=="Prominence\n"){
    min <- -1
    br <- c(-1,-0.5,0,0.5,1)}
  else{
    min <- 0
    br <- c(0,0.2,0.4,0.6,0.8,1)}
  vs <- melt(vs)
  p <- ggplot(vs, aes(x = Var2, y = -Var1, fill = value)) +  geom_raster()
  p <- p + theme_bw() + coord_equal() 
  p <- p + scale_fill_distiller(palette = "Spectral",direction=-1,name = labeltext,limits=c(min,1),breaks=br)
  p <- p + theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
  p <- p + theme(panel.border = element_blank(),panel.grid = element_blank())
  return(p)
}

plotScapeDiscrete <- function(vs,labeltext){# plots a visualscape
  vs <- melt(vs)
  p <- ggplot(vs, aes(x = Var2, y = -Var1, fill = value)) +  geom_raster()
  p <- p + theme_bw() + coord_equal() + scale_fill_brewer(palette = "Set3",direction=-1,name = labeltext,na.value="grey50")
  p <- p + theme(axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
  p <- p + theme(panel.border = element_blank(),panel.grid = element_blank())
  return(p)
}

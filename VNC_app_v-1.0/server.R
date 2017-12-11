library(shiny)
library(reshape2)
library(ggplot2)

options(shiny.maxRequestSize=100*1024^2)

shinyServer(function(input, output, session){
  source("navigation.R",local = TRUE)
  source("input-functions.R",local = TRUE)
  source("output-functions.R",local = TRUE)
  source("computation-functions.R",local = TRUE)
  
  ##### MAIN SCRIPT #####
  finaloutput <- eventReactive(input$run,{
    logtext <- paste("Analyis started",Sys.time(),"\n")
    logtaxt <- c(logtext,paste("Output directory:",input$outdir,"\n"))
    
    withProgress(message = "VNC Analysis in progress...",value=0,{
      n <- 5
      incProgress(1/n,detail="Processing input")
      vsmode <- as.numeric(input$vsmode)
      logtext <- c(logtext,paste("Number of viewsheds:",vsmode))
      
      # Read input files
      # ------------------------------
      validate(need(input$vsfile1 != "",
                    "Error: Please upload a viewshed file."))
      v_total1 <- readVS(input$vsfile1,input$vshead,getSep(input$vssep),input$remneg,input$nonorm)
      logtext <- c(logtext,paste("Viewshed file 1:",input$vsfile1[1]))
      if(vsmode==2){
        validate(need(input$vsfile2 != "",
                      "Error: Please upload two viewshed files or switch to single-viewshed-mode."))
        v_total2 <- readVS(input$vsfile2,input$vshead,getSep(input$vssep),input$remneg,input$nonorm)
        logtext <- c(logtext,paste("Viewshed file 2:",input$vsfile2[1]))
      }
      
      # write output v_total 
      writeOut(v_total1,"input1","Normalized value\n",TRUE,input$outdir,input$plotoutput,0,0)
      if(vsmode==2){
        writeOut(v_total2,"input2","Normalized value\n",TRUE,input$outdir,input$plotoutput,0,0)
      }
      
      # Obtain neighbourhood mask
      nb_mask <- getNB()
      
      # determine dimensions
      vx <- ncol(v_total1)
      vy <- nrow(v_total1)
      mx <- ncol(nb_mask)
      my <- nrow(nb_mask)
      rx <- (mx-1)/2
      ry <- (my-1)/2
      dims <- data.frame(vx=vx,vy=vy,mx=mx,my=my,rx=rx,ry=ry)
      
      logtext <- c(logtext,paste("Viewshed dimensions",vx,"x",vy,"cells\n"))
      logtext <- c(logtext,"NEIGHBOURHOOD MASK")
      for(i in 1:my){
        logtext <- c(logtext,paste(nb_mask[i,],collapse = " "))
      }
      logtext <- c(logtext,"\nSELECTED ANALYSES")
      incProgress(1/n,detail="Performing basic analysis")
      
      # Basic analysis : average and prominence
      # ---------------------------------------
      if (input$avg | input$prom) {# compute average
        withProgress(message = "Average", value = 0, {
          v_avg1 <- globalAvg(v_total1, nb_mask, dims, vsmode)
          if(vsmode==2){
            v_avg2 <- globalAvg(v_total2, nb_mask, dims, vsmode)
          }
          if (input$avg) {# output average
            logtext <- c(logtext,"Basic analysis - Average (results stored in v_avg)")
            writeOut(v_avg1,"v_avg1","Average",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            if(vsmode==2){
              writeOut(v_avg2,"v_avg2","Average",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            }
          }
        })
        if (input$prom ) {# compute and write prominence
          withProgress(message = "Prominence", value = 0, {
            logtext <- c(logtext,"Basic analysis - Prominence (results stored in v_prom)")
            v_prom1 <- globalProm(v_avg1, v_total1, dims, vsmode)
            writeOut(v_prom1,"v_prom1","Prominence\n",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            if(vsmode==2){
              v_prom2 <- globalProm(v_avg2, v_total2, dims, vsmode)
              writeOut(v_prom2,"v_prom2","Prominence\n",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            }
          })
        }
      }
      
      # Basic analysis : min, max, range
      #---------------------------------
      if (input$min | input$max | input$range){ # compute extremes
        withProgress(message = "Extreme values", value = 0, {
          extremes1 <- globalExtremes(v_total1, nb_mask, dims, vsmode)
          if(vsmode==2){
            extremes2 <- globalExtremes(v_total2, nb_mask, dims, vsmode)
          }
          if (input$min) {# output minimum
            logtext <- c(logtext,"Basic analysis - Minimum value (results stored in v_min)")
            writeOut(extremes1[[1]],"v_min1","Minimum\n",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            if(vsmode==2){
              writeOut(extremes2[[1]],"v_min2","Minimum\n",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            }
          }
          if (input$max) {# output maximum
            logtext <- c(logtext,"Basic analysis - Maximum value (results stored in v_max)")
            writeOut(extremes1[[2]],"v_max1","Maximum\n",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            if(vsmode==2){
              writeOut(extremes2[[2]],"v_max2","Maximum\n",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            }
          }
          if (input$range) {# output range
            logtext <- c(logtext,"Basic analysis - Range (results stored in v_range)")
            writeOut(extremes1[[3]],"v_range1","Range\n",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            if(vsmode==2){
              writeOut(extremes2[[3]],"v_range2","Range\n",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            }
          }
        })
      }
      
      incProgress(1/n,detail="Performing group-based analysis")
      

      # Group-based analysis : average, min, max, range
      #----------------------------------------------
      if (input$minavg | input$maxavg | input$minval | input$maxval | input$minrange | input$maxrange ){
        withProgress(message = "Group-based analysis", value = 0, {
          groupresults1 <- groupAnalysis(v_total1,nb_mask,dims,vsmode)
          if(vsmode==2){
            groupresults2 <- groupAnalysis(v_total2,nb_mask,dims,vsmode)
          }
          
          if (input$minavg) {# output minimum average group
            logtext <- c(logtext,"Group-based analysis - Minimum average (results stored in v_minavg)")
            writeOut(groupresults1[[1]],"g_minavg1","Minimum\naverage\ngroup\n",FALSE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            if(vsmode==2){
              writeOut(groupresults2[[1]],"g_minavg2","Minimum\naverage\ngroup\n",FALSE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            }
          }
          if (input$maxavg) {# output maximum average group
            logtext <- c(logtext,"Group-based analysis - Maximum average (results stored in v_maxavg)")
            writeOut(groupresults1[[2]],"g_maxavg1","Maximum\naverage\ngroup\n",FALSE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            if(vsmode==2){
              writeOut(groupresults2[[2]],"g_maxavg2","Maximum\average\ngroup\n",FALSE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            }
          }
          if (input$minval) {# output minimum value group
            logtext <- c(logtext,"Group-based analysis - Minimum value (results stored in v_minval)")
            writeOut(groupresults1[[3]],"g_minval1","Minimum\nvalue\ngroup\n",FALSE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            if(vsmode==2){
              writeOut(groupresults2[[3]],"g_minval2","Minimum\nvalue\ngroup\n",FALSE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            }
          }
          if (input$maxval) {# output maximum value group
            logtext <- c(logtext,"Group-based analysis - Maximum value (results stored in v_maxval)")
            writeOut(groupresults1[[4]],"g_maxval1","Maximum\nvalue\ngroup\n",FALSE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            if(vsmode==2){
              writeOut(groupresults2[[4]],"g_maxval2","Maximum\nvalue\ngroup\n",FALSE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            }
          }
          if (input$minrange) {# output minimum range group
            logtext <- c(logtext,"Group-based analysis - Minimum range (results stored in v_minrange)")
            writeOut(groupresults1[[5]],"g_minrange1","Minimum\nrange\ngroup\n",FALSE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            if(vsmode==2){
              writeOut(groupresults2[[5]],"g_minrange2","Minimum\nrange\ngroup\n",FALSE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            }
          }
          if (input$maxrange) {# output maximum range group
            logtext <- c(logtext,"Group-based analysis - Maximum range (results stored in v_maxrange)")
            writeOut(groupresults1[[6]],"g_maxrange1","Maximum\nrange\ngroup\n",FALSE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            if(vsmode==2){
              writeOut(groupresults2[[6]],"g_maxrange2","Maximum\nrange\ngroup\n",FALSE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            }
          }
        })
      }
      
      
      incProgress(1/n,detail="Performing expectation-based analysis")
     
      
      # Expectation-based analysis : RMSE 
      #----------------------------------------------
      
      if(input$genrmse | input$grouprmse){
        withProgress(message = "RMSE analysis", value = 0, {
          exp_tab <- getExp()
          exp_matrix <- getExpMatrix(exp_tab,nb_mask)
          if(vsmode==1){
            rmse <- computeRMSE1(v_total1,nb_mask,exp_matrix,dims,input$genrmse,input$grouprmse)
          }
          if(vsmode==2){
            vs_matrix <- getVsMatrix(exp_tab,nb_mask)
            rmse <- computeRMSE2(v_total1,v_total2,nb_mask,exp_matrix,vs_matrix,dims,input$genrmse,input$grouprmse)
          }
          if(input$genrmse & input$grouprmse){
            logtext <- c(logtext,"Expectation-based analysis - Global RMSE (results stored in rmse_global)")
            logtext <- c(logtext,"Expectation-based analysis - Grouped RMSE (results stored in rmse_grouped)")
            writeOut(rmse[[1]],"rmse_global","Root-mean-square\nerror\n",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
            writeOut(rmse[[2]],"rmse_grouped","Root-mean-square\nerror\n",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
          }else if(input$genrmse){
            writeOut(rmse,"rmse_global","Root-mean-square\nerror\n",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
          }else{
            writeOut(rmse,"rmse_grouped","Root-mean-square\nerror\n",TRUE,input$outdir,input$plotoutput,dims$rx,dims$ry)
          }
          logtext <- c(logtext,"\nEXPECTATION TABLE")
          for(i in 1:nrow(exp_tab)){
            logtext <- c(logtext,paste0(exp_tab[i,],collapse = " "))
          }
        })
      }
      
      incProgress(1/n,detail="Wrapping up...")
      
      logtext <- c(logtext,paste("\nAnalyis finished",Sys.time(),"\n"))
      write(logtext,file = paste0(input$outdir,"logfile.txt"))
    
      paste("Analysis completed with ",vx,"x",vy,
            " study area and ",mx,"x",my," neighbourhood mask")
      
    })
    
    
  })
  #-----------------------
  output$t <- renderText({
    finaloutput()
  })
  
  #--- NEIGHBOURHOOD PLOT ---#
  output$nbplot <- renderPlot({
    validate(need(
      !is.null(input$nbradius),
      "Please wait a second"
    ))
    nb_mask <- getNB()
    plotNB(nb_mask, "Group\n", input$nbmode==3)
  })

  #--- EXPECTATION VALUE CONTROLS ---#
  output$expcontrols <- renderUI({
    groups <- getGroups()
    lapply(1:length(groups), function(i) {
      conditionalPanel(
        "input.expmode == 3",
        sliderInput(
          paste0('valband', i), h5(paste0('Group ', groups[i])),
          value = i/10,min = 0,max = 1,step = 0.05
        ),
        conditionalPanel(
          "input.vsmode == 2",
          radioButtons(
            paste0('vsband', i),h6(""),
            choices = list(
              "Viewshed 1" = 1,
              "Viewshed 2" = 2
            ),
            inline = TRUE
          )
        ),
        br()
      )
    })
  })
  
  
  #--- DISTANCE BAND CONTROLS ---#
  output$nbcontrols <- renderUI({
    n <- input$nbbands
    lapply(1:n, function(i) {
      conditionalPanel(
        "input.equidist == 2",
        sliderInput(
          paste0('bandwidth', i), h5(paste0('Width of band ', i)),
          value = 3,min = 1,max = 25,step = 1
        ),
        br()
      )
    })
  })
  
  
  #--- RADIUS CONTROL ---#
  output$radcontrol <- renderUI({
    if(!is.null(input$nbradius)){
      prevrad <- input$nbradius 
      if(prevrad<input$nbbands) prevrad <- input$nbbands
      sliderInput("nbradius",label = h5("Radius (number of cells)"), value = prevrad, min = input$nbbands, max = 100, step = 1)
    }
    else{
      sliderInput("nbradius",label = h5("Radius (number of cells)"), value = input$nbbands, min = input$nbbands, max = 100, step = 1)
    }
  })
  
  #--- EXPECTATION PLOT ---#
  output$expmatrix <- renderPlot({
    validate(need(
      !is.null(input$valband1)|input$nbmode==3,
      "Please wait a second"
    ))
    exp_matrix <- getExpMatrix(getExp(),getNB())
    plotExp(exp_matrix,"Expectation value\n",input$nbmode==3)
  })
  
  #--- OUTPUT DIRECTORY ---#
  output$dircheck <- renderText({
    validate(need(
      dir.exists(input$outdir),
      "This directory does not exists. Please specify an existing directory."
    ))
    outdir <- input$outdir
    "Note that existing output files in this directory will be overwritten."
  })

 
  
  

  
})


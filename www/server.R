options(rgl.useNULL=TRUE)

library(shiny)
library(shinyRGL)
library(rgl)
library(devtools)
library(compiler)
library(plyr)
library(doParallel)
library(ggplot2)

source("shinyfunctions.R")

cl <- makeCluster(2)
#cl <- makeCluster(6)
registerDoParallel(cl)
#####Check for rgl package installed on machine running program
# required.packages <- c("<rgl>","<shinyRGL>","<shiny>")#,"devtools","compiler","plyr","doParallel","ggplot2")
# new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

#####Clear console screen
cat("\014") 


shinyServer(function(input, output, session) {
  
  observe({
  
    
    p.height.cust <- as.numeric(input$p.height.cust)  
    
  
    ###################################################################################################################################################################
    output$sctPlot.cust <- renderWebGL({
      
    inFile <- input$file1
      
    if(is.null(inFile))
        return(NULL)
      
      
      rawdata.cust <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                               quote=input$quote)
      rawdata.mask.check <- rawdata.cust
      updateCheckboxGroupInput(session, "plant.dates.cust",choices = names(rawdata.cust), 
                               selected = input$plant.dates.cust)
      updateSelectizeInput(session, "e2", choices = names(rawdata.cust), 
                           selected = input$e2)
      isolate(updateSelectizeInput(session, "select", choices = names(rawdata.cust), 
                                   selected = input$select))
   
    #####################################################################################   
      raw.yr.choices <- sort(as.numeric(1))#input$raw.choices))
      num.raw.choices <- length(raw.yr.choices)
      
      
      temp <- names(rawdata.cust)
      date.temp <- as.character(input$plant.dates.cust)

      date.temp.match <- match(temp,date.temp)
      date.choices <- which(date.temp.match != is.na(date.temp.match))#c(which(temp[i] == date.temp[i]))#input$plant.dates.cust)#sort(as.numeric(input$plant.dates.cust))
      num.p.dates <- length(date.choices)#as.numeric(input$plant.dates.cust))
      
      num.raw.col <- num.p.dates#(num.t.choices * num.p.dates * num.raw.choices)# / num.raw.choices
    #####################################################################################
    midnight <- grep("00:00:00",rawdata.cust[["Day.of.Year"]])
    doy.range <- as.numeric(gsub(":00:00:00","",rawdata.cust[midnight,"Day.of.Year"]))
    #####################################################################################  
    tod.range.input <- input$tod.range.slider
    
    alist <- seq(0,45,by=15)
    blist <- alist + 100
    clist <- c(alist,blist)
    inc <- 100
    
    for(i in 1:22)
    {
      inc <- inc + 100
      blist <- alist + inc
      clist <- c(clist,blist)
    }
    
    #show(clist)
    
    tod.range.start <- which(clist == tod.range.input[1])
    tod.range.end <- which(clist == tod.range.input[2])
    #show(tod.range.input)
    #tod.range.input <- tod.range.input / 100
    #show(tod.range.input)
    
#     tod.range.start <- ((round(tod.range.input[1],0) * 4) + 
#       ((tod.range.input[1] - round(tod.range.input[1],0)) / .15)) + 1
#     tod.range.end <- ((round(tod.range.input[2],0) * 4) + 
#       ((tod.range.input[2] - round(tod.range.input[2],0)) / .15)) - 1

    
    show(tod.range.start)
    show('****************')
    show(tod.range.end)
    show('****************')
    #####################################################################################   
    ext.x <- as.numeric(input$ext.x.cust)
    ext.y <- as.numeric(input$ext.y.cust)
    ext.z <- as.numeric(input$ext.z.cust)
    #####################################################################################      
    n.col.plots <- as.numeric(input$n.plots)#number of columns
    m.row.plots <- n.col.plots#as.numeric(input$m.plots)#number of rows
    if(m.row.plots > num.raw.col){
      depth.plots <- round(num.raw.col/m.row.plots)
    }else depth.plots <- m.row.plots
    
#####################################################################################
      #zlim <- (c(0,60))*ext.z
      #zlen <- zlim[2]
      
      color.start <- input$color.range.start.cust
      color.end <- input$color.range.end.cust
      #colorlut <- rev(rainbow(zlen, start=color.start, end=color.end, alpha=1)) # .755height color lookup table

col.t <- read.csv(file = "TempSymbology2.csv", header = TRUE)
zlim <- (c(0,nrow(col.t))) * ext.z
colorlut2 <- seq(1,length(col.t[,1]))

for (i in 1:length(colorlut2)){
  colorlut2[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
}
colorlut <- colorlut2 
#       if(input$mask.b.num.cust > as.numeric(0))
#       {
#         mask.b.temp <- (as.numeric(input$mask.b.num.cust) + 1) * ext.z  
#         colorlut[1:mask.b.temp]="#FFFFFFFF"#"grey" #Mask all temperatures under 30 Celsius
#       }
#       
#       if(input$mask.a.num.cust > as.numeric(0))
#       {
#         mask.a.temp <- (as.numeric(input$mask.a.num.cust) + 1) * ext.z  
#         colorlut[mask.a.temp:max(zlim)]="grey" #"FFFFFFFF" #Mask all temperatures under 30 Celsius
#       }
#####################################################################################
show("Building.....")
##############################################################
treat.choices.offset <- date.choices
##############################################################
mask.range.cust <- input$mask.range
      if(as.numeric(input$mask.b.num.cust) > as.numeric(0))
      {
        #mask.b.temp <- as.numeric(mask.range.cust[1])#*ext.z
        if(input$mask.color.cust == 1) colorlut[1]="grey"##A9A9A9#FFFFFFFF"#"grey"
        if(input$mask.color.cust == 2) colorlut[1]="white"##FFFFFFFF"#"white"
        if(input$mask.color.cust == 3) colorlut[1]="black"##000000#FFFFFFFF"#"black"
        mask.col <- input$select
        show("mask.column")
        show(mask.col)
        if (is.na(rawdata.mask.check[,mask.col]) == TRUE) rawdata.cust[,c(1,date.choices)] <- as.numeric(0)
       
        mask.row <- which(rawdata.mask.check[,mask.col] < mask.b.temp)
        #show(mask.row)
        temp.rawdata <- rawdata.cust[,c(1,date.choices)]
        temp.rawdata[mask.row,] <- as.numeric(0)#temp.rawdata[-mask.row,c(1,date.choices)]
        rawdata <- temp.rawdata
      } else rawdata <- rawdata.cust[,c(1,date.choices)]
##############################################################
      titles <- c('P1','P2','P3','P4','P5','P6','P7')#c(4.03,4.15,4.29,5.13,5.13,6.03,6.16)
      col.titles <- titles[date.choices]
      plant.titles <- rep(col.titles,(num.raw.choices * num.p.dates))
      #############################################################################    
      num.treatments <- as.numeric(ncol(rawdata))
      treatment.days <- round((length(rawdata[,2]) - 1) / 96)

      doy.dap.view <- input$n.start.end.cust
      treatment.col1 <- as.numeric(doy.dap.view[1])#input$n.start.cust)#1)#readline("Enter the start of DAP -----> "))
      treatment.col2 <- as.numeric(doy.dap.view[2])#input$n.end.cust)#as.numeric(200)#readline("Enter the END of DAP ------>  "))
      treat.range <- treatment.col2 - treatment.col1
      #####################################################################################
      
temp.start <- tod.range.start#midnight[which(doy.range == treatment.col1)] + tod.range.start
temp.end <- tod.range.end - 1#temp.start + 95
show("temp start")
show(temp.start)
show(temp.end)
      #####Loop for Sensor dataset that will be graphed####################################
      if(input$y.view.radio.cust == 1)temp.marker <- as.numeric(1)#DOY View
      if(input$y.view.radio.cust == 2)temp.marker <- as.numeric(2)#DAP View

      treatment.array <- create.treatment.array.cust(doy.range,treatment.col1,num.raw.col,treat.range,
                                                     num.treatments,rawdata,temp.marker,2,midnight)
      #####Check for canopy temperature outliners > 150####################################
      treatment.array[treatment.array > 100] <- c(0)
      treatment.array[treatment.array < 0] <- c(0)
      treatment.array[is.na(treatment.array)] <- c(0)
      #show("Changed all temperature values less than 0 & greater than 100 to-----> 0") 
      ##############################################################################
      open3d()
      #bg3d("grey")
      #axes3d()
      phi.angle <- as.numeric(input$pov.angle.cust) 
      ##############################################################################
      if(input$back.color.cust == 1)bg3d("grey")
      if(input$back.color.cust == 2)bg3d("white")
      if(input$back.color.cust == 3)bg3d("black")
      clear3d(type=("lights"))
      view3d( theta=-90, phi=phi.angle)
      ##############################################################################
      #Dap and Dop x axis view options
      x.start <- treatment.col1
      x.end <- treatment.col2 
      ##############################################################################
      #####Assign variables from treatment matrix to variables used in rgl.surface
      x <- ext.x * (1:96) #TOD
      y <- ext.y *(1:treat.range)
      z.vector <- array(dim = c(96, treat.range, num.treatments))
      x.vector <- z.vector
      offset.stack <- as.numeric(input$stack.offset.cust)
      width.offset <- as.numeric(input$side.width.offset.cust)
      side.heigth.offset <- as.numeric(input$side.heigth.offset.cust)
      #####################################################################################
      progress <- shiny::Progress$new(session,"working")
      on.exit(progress$close())
      progress$set(message = 'Working...')
      if(as.numeric(input$view.radio.cust) == 1){
        l_ply(1,transform,
              stack.rgl.3d.plot
              (num.raw.col,x,y,z,ext.x,ext.y,ext.z,depth.plots,0,z.vector,treatment.array,
               colorlut,zlim,treat.range,0,offset.stack,session,width.offset),
              #.progress="working",
              .parallel=TRUE)
        marker <- as.numeric(1)
      }else if(as.numeric(input$view.radio.cust) == 2){
        l_ply(1,transform,
              side.rgl.3d.plot
              (num.raw.col,x,y,z,ext.x,ext.y,ext.z,0,depth.plots,0,x.vector,treatment.array,
               colorlut,zlim,treat.range,width.offset,session,side.heigth.offset),
              #.progress="working",
              .parallel=TRUE)
        marker <- as.numeric(0)
        
      }
#       if(as.numeric(input$labels.cust) == 1)plot.axis.labels(x.start, x.end,ext.x,ext.y,ext.z,treat.range,
#                                                              num.t.choices,0,x,x.vector,
#                                                              num.t.choices,0,treat.choices,num.raw.col,
#                                                              plant.titles,yr.titles,offset.stack,marker,side.heigth.offset,width.offset)
      
      ####################################################################################
   # },height = p.height.cust)


################################################################################################################################################################
output$ex_out <- renderPrint({
  str(sapply(sprintf('e%d', 2), function(id) {
    input[[id]]
  }, simplify = FALSE))
})
################################################################################################################################################################
output$mytable.cust <- renderDataTable({
      inFile <- input$file1
      
      if(is.null(inFile))
        return(NULL)
      
      
      rawdata.cust <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                               quote=input$quote)
      updateCheckboxGroupInput(session, "show_vars.cust",choices = names(rawdata.cust), selected =input$show_vars.cust)
      rawdata.cust[, input$show_vars.cust, drop = FALSE]}, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
      
    )
    },height = p.height.cust)
############################################################################################################################################################
 
################################################################################################################################################################

})#End of Observe
})

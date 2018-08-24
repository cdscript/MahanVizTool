options(rgl.useNULL=TRUE)
options(shiny.maxRequestSize=30*1024^2)

#setwd("C:/Users/user1/Dropbox/Code/R Code/shinyEnviro")

library(shiny)
library(shinyRGL)
library(rgl)
library(devtools)
library(compiler)
library(plyr)
library(doParallel)
library(pryr)

source("shinyfunctions.R")

#cl <- makeCluster(2)
cl <- makeCluster(4)
registerDoParallel(cl)
#####Check for rgl package installed on machine running program
# required.packages <- c("<rgl>","<shinyRGL>","<shiny>")#,"devtools","compiler","plyr","doParallel","ggplot2")
# new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)

show(getwd())

show("Current Working Directory")
show(getwd())
work.direct <- as.character(getwd())  


#####Clear console screen
cat("\014") 


shinyServer(function(input, output, session) {
  
observe({  
    
p.height.cust <- as.numeric(input$p.height)    


     
###################################################################################################################################################################
output$sctPlot.cust <- renderWebGL({

#p.height.cust <- as.numeric(input$p.height)    
#height = p.height.cust
l.choices=as.numeric(input$location.view.radio2)#,csv.dir=c("/Lubbock/","/Florida/","/CSIRO/"))
csv.dir <- seq(1,length(l.choices))

if(length(l.choices) >= 1){ 
if(l.choices[1] == 1)csv.dir[1] <- "/Lubbock/"
if(l.choices[1] == 2)csv.dir[1] <- "/CSIRO/"
if(l.choices[1] == 3)csv.dir[1] <- "/Florida/"
}
if(length(l.choices) >= 2){

  if(l.choices[2] == 2)csv.dir[2] <- "/CSIRO/"
  if(l.choices[2] == 3)csv.dir[2] <- "/Florida/"
}
if(length(l.choices) >= 3){
  csv.dir[3] <- "/Florida/"
}
###################################################################################################################################################################
row.sq <- c(rep(c(8760,8784,8760,8760),2),8760,8784,8760,8760)
#row.sq <- c(rep(c(8784,8760,8760,8760),2),8784,8760,8760,8760)
raw.array <- array(dim = c(8784,11,12))# * length(csv.dir)))
raw.array.1 <- array(dim = c(8784,11,12))# * length(csv.dir)))
raw.array.2 <- array(dim = c(8784,11,12))# * length(csv.dir)))
##########################################################################
new.direct <- paste(work.direct, csv.dir[1], "yearlyHourCSVS", sep="")
setwd(new.direct)
#show("NEW Working Directory number 1")
#show(getwd())
raw.csv.names <- list.files(pattern = '.csv')
for (i in 1:length(raw.csv.names)) {
  #show(i)
  raw.csv <- as.matrix(read.csv(file = raw.csv.names[i], header = TRUE))
  #raw.array[1:row.sq[i],,i] <- raw.csv
  raw.array[1:8784,,i] <- raw.csv
  #raw.array[1:8784,,i] <- raw.csv
}

colnames(raw.array) <- c('AT','Dew_Point','RH_Percent','VPD',  'ppt_mm', "ppt_mm_sum",
                         'Soil4in_C', 'Soil8in_C', 'Solar_WM2', 'ws2m_ms', 'ws10m_ms')
#temp <- names(raw.csv)
###########################################################################
if(length(l.choices) > 1){
#for(j in 2:length(l.choices))
#{
new.direct.1 <- paste(work.direct, csv.dir[2], "yearlyHourCSVS", sep="")
setwd(new.direct.1)
#show("NEW Working Directory for 2nd location choice")
#show(getwd())
raw.csv.names.1 <- list.files(pattern = '.csv')
for (i in 1:length(raw.csv.names.1)) {
  #show(i)
    raw.csv.1 <- as.matrix(read.csv(file = raw.csv.names.1[i], header = TRUE))
    #raw.array.1[1:row.sq[i],,i] <- raw.csv.1
    raw.array.1[1:8784,,i] <- raw.csv.1
}
}
colnames(raw.array.1) <- c('AT','Dew_Point','RH_Percent','VPD',  'ppt_mm', "ppt_mm_sum",
                         'Soil4in_C', 'Soil8in_C', 'Solar_WM2', 'ws2m_ms', 'ws10m_ms')
############################################################################
if(length(l.choices) > 2){

    new.direct.2 <- paste(work.direct, csv.dir[3], "yearlyHourCSVS", sep="")
    setwd(new.direct.2)
    #show("NEW Working Directory for 3rd location choice")
    #show(getwd())
    raw.csv.names.2 <- list.files(pattern = '.csv')
    for (i in 1:length(raw.csv.names.2)) {
      #show(i)
      raw.csv.2 <- as.matrix(read.csv(file = raw.csv.names.2[i], header = TRUE))
      #raw.array.2[1:row.sq[i],,i] <- raw.csv.2
      raw.array.2[1:8784,,i] <- raw.csv.2
    }
}
colnames(raw.array.2) <- c('AT','Dew_Point','RH_Percent','VPD',  'ppt_mm', "ppt_mm_sum",
                         'Soil4in_C', 'Soil8in_C', 'Solar_WM2', 'ws2m_ms', 'ws10m_ms')
############################################################################
raw.yr.choices <- sort(as.numeric(input$yr.dates.cust))
num.raw.choices <- length(raw.yr.choices)
#yr.choices = c('2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014')
##########################################################################################################################
if(input$mask.reset == TRUE){
    updateSelectizeInput(session, "select", choices = names(rawdata.cust), selected = 0)#input$select)
    updateCheckboxInput(session,"mask.reset", "Reset Column Mask Choice", FALSE)
    updateNumericInput(session,"mask.b.num.cust", label = h5("Below:"), value = 0)
  }
##########################################################################################################################          
temp <- c('AT','Dew_Point','RH_Percent','VPD', 'ppt_mm', "ppt_mm_sum",
          'Soil4in_C', 'Soil8in_C', 'Solar_WM2', 'ws2m_ms', 'ws10m_ms')
date.temp <- as.character(input$plant.dates.cust)
date.temp.match <- match(temp,date.temp)
date.temp.match <- which(is.na(date.temp.match)==FALSE)
date.choices <- date.temp#which(date.temp.match != is.na(date.temp.match))#c(which(temp[i] == date.temp[i]))#input$plant.dates.cust)#sort(as.numeric(input$plant.dates.cust))
num.p.dates <- length(date.temp.match)#date.choices)#as.numeric(input$plant.dates.cust))
num.raw.col <- length(date.temp.match) * num.raw.choices * length(l.choices)#num.p.dates#(num.t.choices * num.p.dates * num.raw.choices)# / num.raw.choices
##########################################################################################################################
rawdata.cust <- as.matrix(raw.array[,c(date.temp.match),c(raw.yr.choices[1])])#raw.array[,,raw.yr.choices[1]]
rawdata.cust.1 <- as.matrix(raw.array.1[,c(date.temp.match),c(raw.yr.choices[1])])#raw.array[,,raw.yr.choices[1]]
rawdata.cust.2 <- as.matrix(raw.array.2[,c(date.temp.match),c(raw.yr.choices[1])])#raw.array[,,raw.yr.choices[1]]

if(num.raw.choices > 1){
  for(i in 2:num.raw.choices){
    rawdata.cust <- cbind(rawdata.cust,raw.array[,c(date.temp.match),raw.yr.choices[i]])
    rawdata.cust.1 <- cbind(rawdata.cust.1,raw.array.1[,c(date.temp.match),raw.yr.choices[i]])
    rawdata.cust.2 <- cbind(rawdata.cust.2,raw.array.2[,c(date.temp.match),raw.yr.choices[i]])
  }
}
##########################################################################################################################
setwd(work.direct)
raw.time <- read.csv(file = "time.csv", header = TRUE)    
midnight <- grep("2400",raw.time[["TOD2"]])
midnight <- c(0,midnight)
midnight <- midnight + 1
doy.range <- as.numeric(raw.time[["Day2"]])#DOYoff"]])
doy.range <- doy.range[midnight]
#############################################################
o <- rep(seq(0,45,by=15),24)
t <- rep(seq(0,2345,by=100),each=4)
v <- t+o
tod.range <- c(0,2345)#input$tod.start.cust,input$tod.end.cust)
tod.start <- tod.range[1]
#tod.start <- which(tod.range[1] == v)
tod.end <- tod.range[2]
##############################################################      
ext.x <- as.numeric(input$ext.x.cust)
ext.y <- as.numeric(input$ext.y.cust)
ext.z <- as.numeric(input$ext.z.cust)
##############################################################       
#n.col.plots <- length(date.temp.match)#as.numeric(10)
#m.row.plots <- n.col.plots#
m.row.plots <-as.numeric(input$m.plots)#number of rows
# if(m.row.plots > num.raw.col){
#     depth.plots <- round(num.raw.col/m.row.plots)
# }else depth.plots <- m.row.plots
depth.plots <- m.row.plots
##############################################################  
show("Building TempSymbology.....")
#####################################################################################
direct.tempsynbol <- "/TempSymbology"
new.work.direct <- paste(work.direct, direct.tempsynbol, sep="")
setwd(new.work.direct)
col.t <- read.csv(file = "TempSymbologyFull.csv", header = TRUE)
zlim <- (c(0,67)) * ext.z
colorlut2 <- seq(1,length(col.t[,1]))
for (i in 1:length(colorlut2)){
  colorlut2[i] <- rgb(col.t[i,2],col.t[i,3],col.t[i,4],maxColorValue=255)#max)
}
colorlut <- colorlut2
#####################################################################################
col.solar <- read.csv(file = "solar.csv", header = TRUE)
zlim.s <- (c(0,14)) * ext.z
colorlut.solar <- seq(1,length(col.solar[,1]))
for (i in 1:length(colorlut.solar)){
  colorlut.solar[i] <- rgb(col.solar[i,2],col.solar[i,3],col.solar[i,4],maxColorValue=255)#max)
}
colorlut.s <- colorlut.solar
#####################################################################################
col.dpt <- read.csv(file = "dewpt2.csv", header = TRUE)
zlim.dpt <- (c(0,26)) * ext.z
colorlut.dpt <- seq(1,length(col.dpt[,1]))
for (i in 1:length(colorlut.dpt)){
  colorlut.dpt[i] <- rgb(col.dpt[i,2],col.dpt[i,3],col.dpt[i,4],maxColorValue=255)#max)
}
colorlut.dp <- colorlut.dpt
#####################################################################################
col.rh <- read.csv(file = "rh.csv", header = TRUE)
zlim.rh <- (c(0,1)) * ext.z
colorlut.rh <- seq(1,length(col.rh[,1]))
for (i in 1:length(colorlut.rh)){
  colorlut.rh[i] <- rgb(col.rh[i,2],col.rh[i,3],col.rh[i,4],maxColorValue=255)#max)
}
colorlut.rh <- colorlut.rh
#####################################################################################
col.ppt <- read.csv(file = "ppt4.csv", header = TRUE)
zlim.ppt <- (c(0,40)) * ext.z
colorlut.ppt <- seq(1,length(col.ppt[,1]))
for (i in 1:length(colorlut.ppt)){
  colorlut.ppt[i] <- rgb(col.ppt[i,2],col.ppt[i,3],col.ppt[i,4],maxColorValue=255)#max)
}
colorlut.pt <- colorlut.ppt
#####################################################################################
col.ppt.sum <- read.csv(file = "ppt_sum_color.csv", header = TRUE)
zlim.ppt.sum <- (c(0,100)) * ext.z
colorlut.ppt.sum <- seq(1,length(col.ppt.sum[,1]))
for (i in 1:length(colorlut.ppt.sum)){
  colorlut.ppt.sum[i] <- rgb(col.ppt.sum[i,2],col.ppt.sum[i,3],col.ppt.sum[i,4],maxColorValue=255)#max)
}
colorlut.pt.s <- colorlut.ppt.sum
#####################################################################################
col.ws <- read.csv(file = "ws.csv", header = TRUE)
zlim.ws <- (c(0,33)) * ext.z
colorlut.ws <- seq(1,length(col.ws[,1]))
for (i in 1:length(colorlut.ws)){
  colorlut.ws[i] <- rgb(col.ws[i,2],col.ws[i,3],col.ws[i,4],maxColorValue=255)#max)
}
colorlut.ws <- colorlut.ws
#####################################################################################
col.vpd <- read.csv(file = "vpd.csv", header = TRUE)
#zlim.vpd <- range(sort(rawdata.cust[["Solar_WM2"]]))
zlim.vpd <- (c(0,10)) * ext.z
#show(zlim.vpd)
colorlut.vpd <- seq(1,length(col.vpd[,1]))
for (i in 1:length(colorlut.vpd)){
  colorlut.vpd[i] <- rgb(col.vpd[i,2],col.vpd[i,3],col.vpd[i,4],maxColorValue=255)#max)
}
colorlut.vp <- colorlut.vpd
#####################################################################################
rawdata <- as.data.frame(rawdata.cust)
rm(rawdata.cust)
show(summary(rawdata))
rawdata.1 <- as.data.frame(rawdata.cust.1)
rm(rawdata.cust.1)
show(summary(rawdata.1))
rawdata.2 <- as.data.frame(rawdata.cust.2)
rm(rawdata.cust.2)
show(summary(rawdata.2))
#############################################################################    
num.treatments <- length(date.temp.match) * num.raw.choices*length(l.choices)#num.raw.col#as.numeric(ncol(rawdata))
doy.dap.view <- input$n.start.end.cust
treatment.col1 <- as.numeric(input$n.start.cust)#1)#readline("Enter the start of DAP -----> "))
treatment.col2 <- as.numeric(input$n.end.cust)#as.numeric(200)#readline("Enter the END of DAP ------>  "))
treat.range <- (treatment.col2 - treatment.col1) + 1
#####################################################################################
dap.seq <- doy.range
#############################################################################
if(input$mask.all.cols == FALSE){
if(input$mask.b.num.cust > as.numeric(0)){# & input$mask.all.cols == FALSE)
  mask.b.temp <- as.numeric(input$mask.b.num.cust)
  #mask.b.temp <- as.numeric(mask.range.cust[1])#*ext.z
  if(input$mask.color.cust == 1) colorlut[1]="grey"##A9A9A9#FFFFFFFF"#"grey"
  if(input$mask.color.cust == 2) colorlut[1]="white"##FFFFFFFF"#"white"
  if(input$mask.color.cust == 3) colorlut[1]="black"##000000#FFFFFFFF"#"black"
  mask.col <- input$select
  show("mask.column")
  show(mask.col)
  #if (is.na(rawdata.mask.check[,mask.col]) == TRUE) rawdata.cust[,c(1,date.choices)] <- as.numeric(0)
  show("names")
  show(names(rawdata.cust[,c(date.choices)]))
  mask.row <- which(rawdata.mask.check[,mask.col] < mask.b.temp)
  show(mask.row[1:10])
  temp.rawdata <- rawdata#t[,c(1,date.choices)]#matrix(nrow=nrow(rawdata.cust),ncol=length(date.choices))
if(is.na(temp.rawdata[mask.row,]) == FALSE)temp.rawdata[mask.row,] <- as.numeric(0)#temp.rawdata[-mask.row,c(1,date.choices)] 
  rawdata <- temp.rawdata
  show(length(mask.row))
}else {
  mask.row <- 0
}
}
#############################################################################
yr.choices = c('2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014')
raw.yr.titles <- c(yr.choices[raw.yr.choices])
raw.titles <- c(date.temp)
if(num.raw.choices > 1)raw.titles <- rep(raw.titles,num.raw.choices)
####################################################################################
if(input$mask.all.cols == TRUE)
{
if(input$mask.b.num.cust > as.numeric(0))
{
  mask.b.temp <- (as.numeric(input$mask.b.num.cust) ) * ext.z  
  colorlut[1:mask.b.temp]="#FFFFFFFF"#"grey" #Mask all temperatures under 30 Celsius
}  
if(input$mask.a.num.cust > as.numeric(0))
{
  mask.a.temp <- (as.numeric(input$mask.a.num.cust) ) * ext.z  
  colorlut[mask.a.temp:max(zlim)]="grey" #"FFFFFFFF" #Mask all temperatures under 30 Celsius
}
}
##############################################################################    
num.meas.days <- as.numeric(24)#input$n.meas.days)
#####Loop for Sensor dataset that will be graphed####################################
if(input$y.view.radio.cust == 1)temp.marker <- as.numeric(1)#DOY View
if(input$y.view.radio.cust == 2)temp.marker <- as.numeric(2)#DAP View
##############################################################
treatment.array <- create.treatment.array.cust(doy.range,dap.seq,treatment.col1,num.raw.col,treat.range,
                                              num.treatments,rawdata,temp.marker,1,midnight,mask.row,
                                              tod.start,tod.end,num.meas.days,raw.titles)
#####Check for canopy temperature outliners > 150####################################
treatment.array[treatment.array < 0] <- c(0)
treatment.array[is.na(treatment.array)] <- c(0)
##############################################################################
if(length(l.choices) > 1){
treatment.array.1 <- create.treatment.array.cust(doy.range,dap.seq,treatment.col1,num.raw.col,treat.range,
                                               num.treatments,rawdata.1,temp.marker,1,midnight,mask.row,
                                               tod.start,tod.end,num.meas.days,raw.titles)
#####Check for canopy temperature outliners > 150####################################
treatment.array.1[treatment.array.1 < 0] <- c(0)
treatment.array.1[is.na(treatment.array.1)] <- c(0)
}else (treatment.array.1 <- treatment.array)
#write.table(treatment.array.1, file = "test1.csv", sep = ",", row.names = FALSE, col.names = TRUE)
##############################################################################
if(length(l.choices) > 2){
treatment.array.2 <- create.treatment.array.cust(doy.range,dap.seq,treatment.col1,num.raw.col,treat.range,
                                                 num.treatments,rawdata.2,temp.marker,1,midnight,mask.row,
                                                 tod.start,tod.end,num.meas.days,raw.titles)
#####Check for canopy temperature outliners > 150####################################
treatment.array.2[treatment.array.2 < 0] <- c(0)
treatment.array.2[is.na(treatment.array.2)] <- c(0)
}else (treatment.array.2 <- treatment.array)
#write.table(treatment.array.2, file = "test2.csv", sep = ",", row.names = FALSE, col.names = TRUE)
##############################################################################
if(as.numeric(input$labels.cust) == 1)label.marker <- as.numeric(1)
if(as.numeric(input$labels.cust) != 1)label.marker <- as.numeric(0)
open3d()
#bg3d("grey")
if(label.marker == 1)#axes3d()
axes3d()
phi.angle.f <- as.numeric(input$pov.angle.cust.f) 
phi.angle.s <- as.numeric(input$pov.angle.cust.s) 
##############################################################################
if(input$back.color.cust == 1)bg3d("grey")
if(input$back.color.cust == 2)bg3d("white")
if(input$back.color.cust == 3)bg3d("black")
clear3d(type=("lights"))
zoom.f <- as.numeric(input$zoom)
if(as.numeric(input$view.radio.cust) == 1)view3d( theta=phi.angle.s, phi=phi.angle.f, zoom = zoom.f)
if(as.numeric(input$view.radio.cust) == 2)view3d( theta=phi.angle.s, phi=phi.angle.f, zoom = zoom.f)
##############################################################################
#Dap and Dop x axis view options
x.start <- treatment.col1
x.end <- treatment.col2
#############################################################################
#####Assign variables from treatment matrix to variables used in rgl.surface
x <- ext.x * (1:num.meas.days) #287)#96) #TOD
y <- ext.y *(1:treat.range)
z.vector <- array(dim = c(num.meas.days, treat.range,num.treatments))
x.vector <- z.vector
offset.stack <- as.numeric(input$stack.offset.cust)
width.offset <- as.numeric(input$side.width.offset.cust)
side.heigth.offset <- as.numeric(input$side.heigth.offset.cust)
#####################################################################################
progress <- shiny::Progress$new(session,"working")
on.exit(progress$close())
progress$set(message = 'Working...')
if(as.numeric(input$view.radio.cust) == 1){

stack.rgl.3d(num.raw.col,x,y,z,ext.x,ext.y,ext.z,depth.plots,0,z.vector,treatment.array,
   colorlut,zlim,treat.range,0,offset.stack,session,width.offset,
   raw.titles,label.marker,colorlut.s,zlim.s,colorlut.dp,zlim.dpt,colorlut.rh,zlim.rh,
   colorlut.pt,zlim.ppt,colorlut.ws,zlim.ws,colorlut.vpd,zlim.vpd,raw.yr.titles)#,

marker <- as.numeric(1)

}else if(as.numeric(input$view.radio.cust) == 2){
#plot.depth.offset <- length(l.choices) * length(raw.titles)
side.rgl.3d(num.raw.col,(x*-1),y,z,ext.x,ext.y,ext.z,0,depth.plots,0,x.vector,treatment.array, 
  treatment.array.1, treatment.array.2,
  colorlut,zlim,treat.range,width.offset,session,side.heigth.offset,
  raw.titles,label.marker,colorlut.s,zlim.s,colorlut.dp,zlim.dpt,colorlut.rh,zlim.rh,
  colorlut.pt,zlim.ppt,colorlut.ws,zlim.ws,colorlut.vpd,zlim.vpd,raw.yr.titles,l.choices,
  zlim.ppt.sum,colorlut.pt.s)#,
  marker <- as.numeric(0)
}  
show(mem_used())      
####################################################################################
},height = p.height.cust)
############################################################################################################################################################
})#End of Observe
})

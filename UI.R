# Must be executed BEFORE rgl is loaded on headless devices.
options(rgl.useNULL=TRUE)
library(shiny)
library(shinyRGL)
#library(devtools)

ui.raw03 <- read.csv(file='Lubbock/2003hrnames.csv', header = TRUE)
colnames(ui.raw03) <- names(ui.raw03)

shinyUI(
  
  fluidPage(
      column(2,
       hr(),
       #wellPanel(
       img(src ="chdlogo2.png",inline=TRUE),
       #),
       hr(),
       wellPanel(
         checkboxGroupInput("location.view.radio2",label = h5("Location"),
                            choices = list("Lubbock, Texas" = 1,  "CSIRO-Australia" = 2, "Citra, Florida" = 3), 
                            selected = seq(3))#2)#seq(1))
       ),
       
       wellPanel(
       h5("View"),
       radioButtons("y.view.radio.cust",label = NULL,choices = list("Day of Year (DOY)" = 1),#,
                                                                    #" DEMO" = 2, "MAHAN" = 3
                                                                    #),#"Days after Planting (DAP)" = 2), 
          selected = 1),
       radioButtons("view.radio.cust",label=NULL,choices = list("Stacked" = 1, "Side by Side" = 2), 
          selected = 2),
       h5("Axis Labels"),  
       radioButtons("labels.cust", label=NULL, 
          choices = list('ON' = 1, 'OFF' = 2),
          selected = 2),
       numericInput("m.plots", label = h5("Set Number of Plots per Row"), value = 3),
       submitButton(h5("APPLY"))
       ),
       
       wellPanel(#h5("Build Plot Matrix by:"),
         checkboxGroupInput("yr.dates.cust", label = h5("Years"), 
                            choices = list('2003' = 1,'2004'= 2, '2005' = 3,'2006'= 4, '2007' = 5,'2008'= 6,
                                           '2009' = 7,'2010'= 8, '2011' = 9,'2012'= 10, '2013'=11,'2014'=12),
                            selected = 12),#seq(1)),
         checkboxGroupInput("plant.dates.cust", label = h5("Enviroment"),
                            choices = names(ui.raw03),
#                            choices = list('AT' = 1,'VPD'= 2, 'RH' = 3, 'Dew_Point' = 4, 'ppt_mm' = 5,
#                             'Soil4in_C' = 6, 'Soil8in_C' = 7, 'Solar_WM2' = 8, 'ws10m_ms' = 9, 'ws2m_ms' = 10),
                            selected = c('ppt_mm'))#NULL)#names(ui.raw03))#'AT')
           
       )
       
        ),
      column(2,
            wellPanel(h5("Mask All Columns of Data by: "),
              checkboxInput("mask.all.cols", "Temperature(C)", FALSE),
             selectInput("select", label = h5("Column Choice"), 
                         choices = NULL,#"Temperature (C)" = 1, "Solar Radiation" = 2, 
                                       # "Time (24hr;1300=1pm)" = 3, "RH" = 4, "Air Temperature" = 5), 
                         selected = NULL),
              checkboxInput("mask.reset", "Reset Column Mask Choice", FALSE),
              #sliderInput("mask.range", label = h5("Mask Everything Outside of this Range"), min = 0, max = 100, value = c(0,68)),
              numericInput("mask.b.num.cust", label = h5("Below:"), value = 0),      
              numericInput("mask.a.num.cust", label = h5("Above:"), value = 0)
            )
                         
      ),
        
      column(2,
             wellPanel(h5("Extrapolate Axis"),
             #numericInput("ext.x.cust", label = h5("TOD(x)"),value = 80),
             numericInput("ext.x.cust", label = h5("TOD(x)"),value = 1),
             #numericInput("ext.y.cust", label = h5("DOY(y)"),value = 10),
             numericInput("ext.y.cust", label = h5("DOY(y)"),value = 1),
             numericInput("ext.z.cust", label = h5("Surface(z)"),value = 1),
             numericInput("zoom", label = h5("Zoom Factor"),value = .75)
             )
      ),
      column(2,
        wellPanel(      
        #sliderInput("n.start.end.cust", label = h5("DOY/DAP RANGE"), min = 1, max = 366, value = c(1,365)),
        #sliderInput("ui.tod.range", label = h5("TOD RANGE"), min = 0, max = 2355, value = c(0,2345))
        numericInput("n.start.cust",label = h5("DOY/DAP Start"), value = 1),
        numericInput("n.end.cust",label = h5("DOY/DAP End"), value = 365),
        selectInput("mask.color.cust", label = h5("Set Masking Below Color"), 
                    choices = list("GREY" = 1, "WHITE" = 2, "BLACK" = 3), 
                    selected = 2),
        selectInput("back.color.cust", label = h5("Background Color"), 
                    choices = list("GREY" = 1, "WHITE" = 2, "BLACK" = 3), 
                    selected = 1)        
#         numericInput("tod.start.cust",label = h5("TOD Start"), value = 0),
#         numericInput("tod.end.cust",label = h5("TOD End"), value = 2345)
        )
      ),
            
      column(2,
        wellPanel(
        sliderInput("pov.angle.cust.f", label = h5("Front Point of View Angle"),min = -90,max = 90,
                    value = 30, step = 15),
        sliderInput("pov.angle.cust.s", label = h5("Side Point of View Angle"),min = -360,max = 360,
                    value = 180, step = 15),
        sliderInput("p.height", label = h5("Plot Height"),min = 100,max = 4000,
                  value = 1300, step = 100)
        )
              ),
      column(2,
        wellPanel(
        h5("Plot Arrangement"),         
          numericInput("stack.offset.cust", label = h5("Stacked View Plot Offset"),value = 200),#,min = 0,max = 1000,
          numericInput("side.width.offset.cust", label = h5("Side by Side Height Offset"),value = 100),#,min = 0,max = 10000,
          numericInput("side.heigth.offset.cust", label = h5("Side by Side Width Offset"),value = 50)#,min = 0,max = 500,
        
        )
  
      ),
      
    column(10,
      webGLOutput("sctPlot.cust", height = "100%"))                     
  ))
                              
                            


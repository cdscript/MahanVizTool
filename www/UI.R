# Must be executed BEFORE rgl is loaded on headless devices.
options(rgl.useNULL=TRUE)
library(shiny)
library(shinyRGL)
library(ggplot2)
library(devtools)

rawdata14 <- read.csv(file = "MahanCotton2014mod.csv", header = TRUE)
rawdata12 <- read.csv(file = "MahanCotton2012mod.csv", header = TRUE)
rawdata11 <- read.csv(file = "MahanCotton2011mod.csv", header = TRUE)
rawdata.cust <- rawdata14#eead.csv(file = "MC2.csv", header = TRUE)


shinyUI(
  
  fluidPage(
      column(2,
       hr(),
       img(src ="chdlogo2.png"),
       hr(),
       wellPanel(
       h5("View"),
       radioButtons("y.view.radio.cust",label = NULL,choices = list("Day of Year (DOY)" = 1, "Days after Planting (DAP)" = 2), 
          selected = 1),
       radioButtons("view.radio.cust",label=NULL,choices = list("Stacked" = 1, "Side by Side" = 2), 
          selected = 2),
       h5("Axis Labels"),  
       radioButtons("labels.cust", label=NULL, 
          choices = list('ON' = 1, 'OFF' = 2),
          selected = 2),
       submitButton(h5("APPLY"))
       ),
       
       wellPanel(h5("Build Plot Matrix by:"),
         radioButtons("row.col", label=NULL, 
                      choices = list('ROW' = 1, 'COLUMN' = 2),
                      selected = 2),       
         numericInput("n.plots",label = h5("# of plots per Row/Column"), value = 4),
        # selectizeInput('e2', 'Select Multi-plot order in ROW/COLUMN', choices = list('TEMPORARY COLUMN 1' = 1,'TEMPORARY COLUMN 2'= 2), multiple = TRUE),
         #submitButton(h5("SUBMIT COLUMN/ROW ASSIGNMENT")),
       h5("Automatic Matrix"),
         checkboxGroupInput("plant.dates.cust", label = h5("Columns"), 
                            choices = list('TEMPORARY COLUMN 1' = 1,'TEMPORARY COLUMN 2'= 2),
                            selected = 1)
           
       ),
       
       wellPanel(
         
         fileInput('file1', 'Choose CSV File',accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
         tags$hr(),
         checkboxInput('header', 'Header', TRUE),
         radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),',')               
         
       )
        ),
      column(2,
            wellPanel(h5="Mask All Columns of Data by: ",
            checkboxInput("mask.all.cols", "Select to Mask all Columns by # Range on Slider Below", FALSE), 
             
             selectInput("select", label = h5("Mask Data by Single Column Range"), 
                         choices = list("Temperature (C)" = 1, "Solar Radiation" = 2, 
                                        "Time (24hr;1300=1pm)" = 3, "RH" = 4, "Air Temperature" = 5), 
                         selected = 1)
                         ),
            #sliderInput("mask.range", label = h5("Mask Everything Outside of this Range"), min = 0, max = 100, value = c(0,68)),
              numericInput("mask.b.num.cust", label = h5("Below:"), value = 0),      
              numericInput("mask.a.num.cust", label = h5("Above:"), value = 0)
#              selectInput("mask.color.cust", label = h5("Set Masking Below Color"), 
#                          choices = list("GREY" = 1, "WHITE" = 2, "BLACK" = 3), 
#                          selected = 1)
      
            
             
      ),
        
      column(2,
             #wellPanel(
               h5("Extrapolate axises"),
             numericInput("ext.x.cust", label = h5("TOD(x)"),value = 20),#,min = 1,max = 100,
             #value = 20, step = 1),
             numericInput("ext.y.cust", label = h5("DOY/DAP(y)"),value = 20),#,min = 1,max = 100,
             #value = 20, step = 1),
             numericInput("ext.z.cust", label = h5("Surface(z)"),value = 1),#,min = 1,max = 100,
             #value = 15, step = 1)
             
             selectInput("back.color.cust", label = h5("Background Color"), 
                         choices = list("GREY" = 1, "WHITE" = 2, "BLACK" = 3), 
                         selected = 1)

             #)
      ),
      column(2,
        #wellPanel(
        sliderInput("pov.angle.cust", label = h5("Point of View Angle"),min = 0,max = 360,
                    value = 90, step = 15),
        
        sliderInput("n.start.end.cust", label = h5("DOY/DAP RANGE"), min = 1, max = 366, value = c(93,365)),
        sliderInput("tod.range.slider", label = h5("TOD RANGE"), min = 0, max = 2345, value = c(0,2345),step=15)
        #  numericInput("n.start.cust",label = h5("DOY/DAP Start"), value = 93),

          #   numericInput("n.end.cust",label = h5("DOY/DAP End"), value = 365)
      ),
            
      column(2,
        #wellPanel(
               sliderInput("p.height.cust", label = h5("Plot Height"),min = 100,max = 4000,
                  value = 1000, step = 100),
      sliderInput("color.range.start.cust", label = h5("Canopy Color Start"),min = 0,max = 1,
                  value = .85, step = .01),
      sliderInput("color.range.end.cust", label = h5("Canopy Color End"),min = 0,max = 1,
                  value = .75, step = .01)
        #)
              ),
      column(2,
        #wellPanel(
        h5("Plot Arrangement"),         
          numericInput("stack.offset.cust", label = h5("Stacked View Plot Offset"),value = 100),#,min = 0,max = 1000,
           #value = 100, step = 25),
          numericInput("side.width.offset.cust", label = h5("Side by Side Width Offset"),value = 100),#,min = 0,max = 10000,
           #value = 1000, step = 1000),
          numericInput("side.heigth.offset.cust", label = h5("Side by Side Height Offset"),value = 100)#,min = 0,max = 500,
          #value = 100, step = 50)
        #)
  
      ),
#       column(10,
#              #dataTableOutput('mytable.cust')
#              verbatimTextOutput('ex_out')
#              ),
      
    column(10,
      webGLOutput("sctPlot.cust", height = "100%"))    
#ataTableOutput('mytable.cust'))
                 
  ))
                              
                            


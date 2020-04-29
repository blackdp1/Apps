# Author: David Black
# Date: March '20
# GUI that reads Zephyr data: Heart Rate and Core Body Temp csv data

# Input Fields:
# Browse button to upload a CSV file
# Age: age of participant, which will affect HR threshold
# Color: Change color of graph
# Download: User can choose directory and it will save the graphs and summary
# Note: It will append an existing summary table or create a new file

# Data:
# File requires first row metadata file
# Ex of metadata: 24-001 Subject log / [20 Jun 2018 / 17:16:09] / [05:03:45] ( GPS ) Heart Rate (BPM) 
# Second row are column names "X" "Y"
# Third row and beyond is the data
# First column is the Time for HR
# Second column is HR
# Third column is Time for Core Temp
# Fourth column is Core Temp
# If datasets are not equal, program adds 0's to the end

# Output:
# Two tabs that display HR and Core Temp
# Each tab is composed of a plot of the time series
# If the data reaches a critical limit (above 101.3 or 180-age for 3 min or beyond)
# the data is highlighted
# A smaller graph that is used to define the beginning and end of the signal
# Click and drag the highlighted box over the area
# Below this, there is a histogram/density plot 
# Finally, a summary table of descriptives of the data for HR and 1 min after recovery

list.of.packages <- c("shiny",
                      "shinythemes",
                      "dplyr",
                      "ggplot2",
                      "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)

library(shiny)
library (shinythemes)
library(ggplot2)
library(dplyr)
library(lubridate)

peaks <- function(mydata){
  
  #peaks <-data.frame(Index=numeric(),Color=character(),Time=character(),HR=numeric())
  peaks <- data.frame(matrix(ncol = 4, nrow = 3))
  colnames(peaks) <- c("Index","Color","Time","HR")
  n <- round((nrow(mydata)/3),0)
  peaks$Index[1] <- which.max(mydata$HR[1:n-1])
  peaks$Index[2] <- (which.max(mydata$HR[n:(n*2-1)])+n-1)
  peaks$Index[3] <- (which.max(mydata$HR[(n*2):(n*3)])+n*2-1)
  
  if (mydata$HR[peaks$Index[1]+60]<120){
    peaks$Color[1]<-"green"}
  else{peaks$Color[1] <- "red"}
  
  if (mydata$HR[peaks$Index[2]+60]<120){
    peaks$Color[2]<-"green"}
  else{peaks$Color[2] <- "red"}
  
  if (peaks$Index[3]+60<nrow(mydata)){
    if (mydata$HR[peaks$Index[3]+60]<120){
      peaks$Color[3]<-"green"}
    else{peaks$Color[3] <- "red"}
  }
  else{
    if (mydata$HR[nrow(mydata)]<120){
      peaks$Color[3]<-"green"}
    else{peaks$Color[3] <- "red"}
  }
  peaks$Time[1] <- (as.character(mydata$Time[peaks$Index[1]]))
  peaks$Time[2] <- (as.character(mydata$Time[peaks$Index[2]]))
  peaks$Time[3] <- (as.character(mydata$Time[peaks$Index[3]]))
  peaks$Time <- as.POSIXct(peaks$Time,tz="GMT")
  peaks$HR[1] <- mydata$HR[peaks$Index[1]]
  peaks$HR[2] <- mydata$HR[peaks$Index[2]]
  peaks$HR[3] <- mydata$HR[peaks$Index[3]]
  
  return(peaks)
}


HRData <- function(mydata,agethreshold){
  
  hit <- which(mydata$HR > agethreshold)
  if (length(hit)>0){
    n <- length(hit)
    ind <- which(hit[-1] - hit[-n] > 1)
    starts <- c(hit[1], hit[ ind+1 ])
    ends <- c(hit[ ind ], hit[n])
    y=data.frame(cbind(starts,ends))
    y$diff = y$ends-y$starts
    y = filter(y,diff > 180)
    if (nrow(y)>0){
      for (i in 1: nrow(y)){
        mydata$HRColor[y$starts[i]:y$ends[i]] = TRUE}
      
    }
  }
  else {y <- NULL}
  datalist <- list(y,mydata)
  return(datalist)
}

TempData <- function(mydata,tempthreshold){
  
  hit <- which(mydata$Temp > tempthreshold)
  if (length(hit)>0){
    n <- length(hit)
    
    ind <- which(hit[-1] - hit[-n] > 1)
    starts <- c(hit[1], hit[ ind+1 ])
    ends <- c(hit[ ind ], hit[n])
    y <- data.frame(cbind(starts,ends))
    y$diff = y$ends-y$starts
    #y = filter(y,diff > 180)
    if (nrow(y)>0){
      for (i in 1: nrow(y)){
        mydata$TempColor[y$starts[i]:y$ends[i]] = TRUE}
      
    }
  }
  else{y <- NULL}
  
  datalist <- list(y,mydata)
  return(datalist)
}


ui = fluidPage(
  #theme = shinytheme("darkly"),
  #@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
  #font-family: 'Lobster', cursive;
  
  tags$head(
    tags$style(HTML("
      h1 {
        font-weight: 300;
        line-height: 1.1;
        color: #808080;
        font-size: 25px
      }
    "))
  ),
  
  headerPanel(("Zephyr HR and Core Temp App")),
  tags$head(tags$style(
    HTML('#title {
           color: blue;
           font-size: 40px;
           font-style: bold;
          }'))),
  
  sidebarPanel(
    fileInput(
      "file1",
      "Choose Zephyr File",
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    numericInput("Age", "Age:", min = 18, max = 100, value="25",width="100px"),
    selectInput("Color","Time Series Color:",
                choices = c("Black"="black","Blue"="blue",
                            "Orange"="orange3","Purple"="purple",
                            "Green"="green3","Gray"="gray35",
                            "Cyan"="cyan","Maroon"="maroon3"),
                selected ="black",width="125px"),
    actionButton("download1","Download",class="btn-primary"), width = 3
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Heart Rate",uiOutput("HR1"),tableOutput("HRSummary"),tableOutput("HRRecoverySummary")),
      tabPanel("Core Body Temp",uiOutput("Temp1"),tableOutput("TempSummary"))
    )
  )
)
#############################################################################

server <- function(session,input, output) {
  
  df <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    #if ((input$Age > 75)||(input$Age < 18)){
    #  updateNumericInput(session,"Age",value=25)
    #  }
    
    metadata <- readLines(inFile$datapath,n=1)
    indextimedata <- which(strsplit(metadata, "")[[1]]=="/")
    indexdatedata <- which(strsplit(metadata, "")[[1]]=="[")
    Participant <- unlist(strsplit(metadata," log"))[1]
    datedata <- substr(metadata,indexdatedata[1]+1,indexdatedata[1]+11)
    datedata <- as.Date(datedata, "%d %b %Y")
    timedata <- (substr(metadata,indextimedata[2]+2,indextimedata[2]+9))
    mydata <- read.csv(inFile$datapath, skip = 1,stringsAsFactors = FALSE)
    colnames(mydata)<-c("Time","HR","TempTime","Temp")
    mydata$Time <- seq(as.POSIXct(paste(datedata,timedata), tz="GMT"),length.out=nrow(mydata), by='1 sec')    
    #browser()
    temptime <- hms(paste("0:",mydata$TempTime[1]))
    
    mydata$TempTime <- seq((temptime+mydata$Time[1]),length.out=nrow(mydata), by='1 sec')    
    mydata$Temp[is.na(mydata$Temp)] <- 0
    mydata$HR[is.na(mydata$HR)] <- 0
    mydata$Participant <- Participant
    mydata$HRColor <- FALSE
    mydata$TempColor <- FALSE
    mydata
    
  })
  ##############################################################################
  ### Render HR UI plots
  
  output$HR1 <- renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    #if ((input$Age > 75)||(input$Age < 18)){
    #  updateNumericInput(session,"Age",value=25)
    #}
    
    tagList(
      plotOutput("HRGraph"),
      plotOutput("HRThreshold",brush = brushOpts(id = "plot_brush"),height=150),
      plotOutput("HRHist")
      
    )
  })
  ##############################################################################
  ### Render Temp UI plots
  
  output$Temp1 <- renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    #if ((input$Age > 75)||(input$Age < 18)){
    #  updateNumericInput(session,"Age",value=25)
    #}
    
    tagList(
      plotOutput("TempGraph"),
      plotOutput("TempThreshold",brush = brushOpts(id = "temp_brush"),height=150),
      plotOutput("TempHist")
    )
  })
  
  observeEvent(input$Age,{
    if ((input$Age > 75)||(input$Age < 18))
    {
      showModal(modalDialog("Age must be between 18 - 75"))
      updateNumericInput(session,"Age",value=25)
    }
  })
  
  ##############################################################################
  ### Call reactive HR Plots
  
  output$HRGraph <- renderPlot({
    HRInput()
  })
  ##############################################################################
  ### Generate HR plot
  
  HRInput <- reactive({
    agethreshold <- 180 - input$Age
    mydata <- trimdata()
    colorgraph <- input$Color
    mydata$HRColor <- FALSE
    datalist <- HRData(mydata,agethreshold)
    mydata <- datalist[[2]]
    mydata <- mydata %>% mutate(HRColor = replace(HRColor, HRColor == FALSE, input$Color))
    mydata <- mydata %>% mutate(HRColor = replace(HRColor, HRColor == TRUE, "red"))
    mydata$HRColor <- as.factor(mydata$HRColor)
    
    peakdata <- peaks(mydata)
    rects <- data.frame(xmin = -Inf, 
                        xmax = Inf,
                        ymin = c(0,60,80),  
                        ymax = c(60,80,Inf),
                        fill = c("green", "yellow", "red"))
    
    
    x1 <- mydata$Time[1]
    x2 <- mydata$Time[nrow(mydata)]
    zone1b <- (220-input$Age)*.6
    zone2b <- (220-input$Age)*.7
    zone3b <- (220-input$Age)*.8
    zone4b <- (220-input$Age)*.9
    zone5b <- (220-input$Age)
    
    ggplot(mydata,aes(x=Time,y=HR,color=HRColor))+geom_path(aes(group=1))+
      scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "15 min")+
      scale_color_manual(values = c(colorgraph,"red"))+
      geom_hline(yintercept=agethreshold,color = "red", size=1)+
      ylab("Heart Rate (BPM)")+
      xlab("TIME")+
      coord_cartesian(xlim = c(x1,x2), ylim = c(0,200),expand=FALSE)+
      ggtitle(paste(mydata$Participant[1], ": Heart Rate Across Time \n Age",input$Age))+
      theme_classic()+
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank())+
      annotate("point", x = peakdata$Time[1], y = peakdata$HR[1], colour = peakdata$Color[1],size=4)+
      annotate("point", x = peakdata$Time[2], y = peakdata$HR[2], colour = peakdata$Color[2],size=4)+
      annotate("point", x = peakdata$Time[3], y = peakdata$HR[3], colour = peakdata$Color[3],size=4)+
      annotate("rect",
               xmin = x1, xmax = x2, 
               ymin = 0, ymax = zone1b,  fill = "dodgerblue", alpha=.2)+
      annotate("rect",
               xmin = x1, xmax = x2, 
               ymin = zone1b, ymax = zone2b,  fill = "green", alpha=.2)+
      annotate("rect",
               xmin = x1, xmax = x2, 
               ymin = zone2b, ymax = zone3b,  fill = "yellow", alpha=.2)+
      annotate("rect",
               xmin = x1, xmax = x2, 
               ymin = zone3b, ymax = zone4b,  fill = "orange", alpha=.2)+
      annotate("rect",
               xmin = x1, xmax = x2, 
               ymin = zone4b, ymax = zone5b,  fill = "red", alpha=.2)
    

  }) 
  ##############################################################################
  ### Calls reactive temp plot
  
  output$TempGraph <- renderPlot({
    TempInput()
  })
  ##############################################################################
  ### Generates temp plot
  
  TempInput <- reactive({
    tempthreshold <- 101.3
    mydata <- trimtempdata()
    colorgraph <- input$Color
    mydata$TempColor <- FALSE
    datalist <- TempData(mydata,tempthreshold)
    mydata <- datalist[[2]]
    mydata <- mydata %>% mutate(TempColor = replace(TempColor, TempColor == FALSE, input$Color))
    mydata <- mydata %>% mutate(TempColor = replace(TempColor, TempColor == TRUE, "red"))
    mydata$TempColor <- as.factor(mydata$TempColor)
    
    ggplot(mydata,aes(x=TempTime,y=Temp,color=TempColor))+geom_path(aes(group=1))+
      scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "15 min")+
      scale_color_manual(values = c(colorgraph,"red"))+
      geom_hline(yintercept=tempthreshold,color = "red", size=1)+
      labs(x="TIME",y = expression("Estimated Core Body Temp ("*~degree*F*")"))+
      scale_y_continuous(limits=c(96,103),breaks=seq(96, 103, 1))+
      ggtitle(paste(mydata$Participant[1], ": Estimated Core Temp Across Time \n Age",input$Age))+
      coord_cartesian(expand=FALSE)+
      theme_classic()+
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
    
    
  }) 
  ##############################################################################
  ### Generates HR plot that define signal
  
  output$HRThreshold <- renderPlot({
    ggplot(df(),aes(x=Time,y=HR))+geom_line(color=input$Color)+
      scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "15 min")+
      ylab("Heart Rate (BPM)")+
      xlab("TIME")+
      ggtitle("Define Start and End Signal")+
      coord_cartesian(expand=FALSE)+
      theme_classic()+
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))
  })
  ##############################################################################
  ### Generates Temp plot that define signal
  
  output$TempThreshold <- renderPlot({
    ggplot(df(),aes(x=TempTime,y=Temp))+geom_line(color=input$Color)+
      scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "15 min")+
      labs(x="TIME",y = expression("Estimated Core Body Temp ("*~degree*F*")"))+
      ggtitle("Define Start and End Signal")+
      scale_y_continuous(limits=c(96,103),breaks=seq(96, 103, 1))+
      coord_cartesian(expand=FALSE)+
      theme_classic()+
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))
  })
  ##############################################################################
  ### Generates reactive HR density plot
  
  HRHistInput <- reactive({
    ggplot(trimdata(), aes(x=HR)) + 
      geom_histogram(aes(y=..density..), colour="black", fill="white",bins=20)+
      geom_density(alpha=.2, fill="#FF6666")+
      xlab("Heart Rate (BPM)")+
      ggtitle(paste(trimdata()$Participant[1],": Heart Rate Distribution \n","Age",input$Age))+
      coord_cartesian(expand=FALSE)+
      theme_classic()+
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
  })
  ##############################################################################
  ### Calls reactive HR density plot
  
  output$HRHist <- renderPlot({
    HRHistInput()
  })
  ##############################################################################
  ### Generates reactive Temp density plot
  
  TempHistInput <- reactive({
    ggplot(trimtempdata(), aes(x=Temp)) + 
      geom_histogram(aes(y=..density..), colour="black", fill="white", bins=20)+
      geom_density(alpha=.2, fill="#FF6666")+
      labs(x = expression("Estimated Core Body Temp ("*~degree*F*")"))+
      ggtitle(paste(trimdata()$Participant[1],": Estimated Core Body Temp Distribution \n","Age",input$Age))+
      coord_cartesian(expand=FALSE)+
      theme_classic()+
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))
  })
  ##############################################################################
  ### Calls reactive Temp density plot
  
  output$TempHist <- renderPlot({
    TempHistInput()
  })
  
  ##############################################################################
  ### Generates +120 after 1 min Summary Table
  SummaryHRRec <- reactive({
    mydata <- trimdata()
    peakdata <- peaks(mydata)
    Summary <- data.frame(matrix(ncol = 5, nrow = 0))
    colnames(Summary) <- c("Participant","Age","Date_Time","Peak HR","Recovery HR (1 min)")
    if ((peakdata$Color[1]=="green")&&(peakdata$Color[2]=="green")&&(peakdata$Color[3]=="green")){
      return(Summary)  
    }  
    counter=1
    for (i in 1:3){
      if (peakdata$Color[i]=="red"){
        Summary[counter,1] <- mydata$Participant[1]
        Summary[counter,2] <- input$Age
        Summary[counter,3] <- as.character(peakdata$Time[i])
        Summary[counter,4] <- peakdata$HR[i]
        if (peakdata$Index[i]+60< nrow(mydata)){
          Summary[counter,5] <- mydata$HR[peakdata$Index[i]+60]
        }
        else{
          Summary[counter,5] <- NA
        }
        counter=counter+1
      }
    }
    
    return(Summary)
  })
  ##############################################################################
  ### Calls reactive HR summary table
  
  output$HRRecoverySummary <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)   
    SummaryHRRec()
    
  })    
  
  
  ##############################################################################
  ### Generates HR Summary Table
  
  SummaryHRTable <- reactive({
    
    agethreshold <- 180 - input$Age
    mydata <- trimdata()
    datalist <- HRData(mydata,agethreshold)
    thresholddata <- datalist[[1]]
    mydata <- datalist[[2]]
    mydata$Time <- as.character(mydata$Time)
    Summary <- data.frame(matrix(ncol = 8, nrow = 0))
    colnames(Summary) <- c("Participant","Age","ACGIH_Guideline","Start_Date_Time","End_Date_Time","HR_Duration","Avg_HR","MaxHR")
    if (!is.null(thresholddata))
    {
      if(nrow(thresholddata)>0){
        for (i in 1:nrow(thresholddata))
        {
          Summary[i,1] <- mydata$Participant[1]
          Summary[i,2] <- input$Age
          Summary[i,3] <- agethreshold
          Summary[i,4] <- mydata$Time[thresholddata$starts[i]]
          Summary[i,5] <- mydata$Time[thresholddata$ends[i]]
          Summary[i,6] <- thresholddata$ends[i]-thresholddata$starts[i]
          Summary[i,7] <- round(mean(mydata$HR[thresholddata$starts[i]:thresholddata$ends[i]], na.rm = TRUE),2)
          Summary[i,8] <- round(max(mydata$HR),2)
        }
      }
      else{
        return(NULL)
      }
    }
    else{
      return(NULL)
    }
    Summary
    
  })
  ##############################################################################
  ### Calls reactive HR summary table
  
  output$HRSummary <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)   
    SummaryHRTable()
  })  
  ##############################################################################
  ### Generates Temp summary table
  
  SummaryTempTable <- reactive({
    tempthreshold <- 101.3
    mydata <- trimtempdata()
    datalist <- TempData(mydata,tempthreshold)
    thresholddata <- datalist[[1]]
    mydata <- datalist[[2]]
    mydata$TempTime <- as.character(mydata$TempTime)
    Summary <- data.frame(matrix(ncol = 8, nrow = 0))
    colnames(Summary) <- c("Participant","Age","ACGIH_Guideline","Start_Date_Time","End_Date_Time","HR_Duration","Avg_HR","MaxHR")
    
    if (!is.null(thresholddata)){
      if(nrow(thresholddata)>0){
        
        for (i in 1:nrow(thresholddata))
        {
          Summary[i,1] <- mydata$Participant[1]
          Summary[i,2] <- input$Age
          Summary[i,3] <- tempthreshold
          Summary[i,4] <- mydata$TempTime[thresholddata$starts[i]]
          Summary[i,5] <- mydata$TempTime[thresholddata$ends[i]]
          Summary[i,6] <- thresholddata$ends[i]-thresholddata$starts[i]
          Summary[i,7] <- round(mean(mydata$Temp[thresholddata$starts[i]:thresholddata$ends[i]]),2)
          Summary[i,8] <- round(max(mydata$Temp),2)
        }
      }
      else{
        return(NULL)
      }
      
    }
    else{
      return(NULL)
    }
    
    Summary
  })
  ##############################################################################
  ### Calls reactive Temp summary table
  
  output$TempSummary <- renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)   
    SummaryTempTable()
  })  
  ##############################################################################
  ### Trims HR data based on user selection
  
  trimdata <- reactive({  
    res <- brushedPoints(df(), input$plot_brush, "Time", "HR")
    if (nrow(res) == 0)
      return(df())
    res
  })
  ##############################################################################
  ### Trims Temp data based on user selection
  
  trimtempdata <- reactive({  
    res <- brushedPoints(df(), input$temp_brush, "TempTime", "Temp")
    if (nrow(res) == 0)
      return(df())
    res
  })
  ##############################################################################
  ### Exports summary tables and plots
  
  observeEvent(input$download1, {
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    workingdir = choose.dir(default = paste0(getwd(), "/*.*"), caption =
                              "Select Folder")
    setwd(workingdir)
    datafilename <- substr(inFile$name, 1, nchar(inFile$name) - 4)
    
    HRRaw <- trimdata()[,c(1,2,5)]
    TempRaw <- trimtempdata()[,c(3,4,5)]
    write.table(
      SummaryHRRec(),
      paste("SummaryHRRec.csv", sep=""),
      row.names = FALSE,
      col.names = TRUE,
      sep = ",",
      append = TRUE
    )
    
    write.table(
      SummaryHRTable(),
      paste("SummaryHRData.csv", sep=""),
      row.names = FALSE,
      col.names = TRUE,
      sep = ",",
      append = TRUE
    )
    
    write.table(
      SummaryTempTable(),
      paste("SummaryTempData.csv", sep=""),
      row.names = FALSE,
      col.names = TRUE,
      sep = ",",
      append = TRUE
    )
    
    write.table(
      HRRaw,
      paste("RawHRData",Sys.Date(),".csv", sep=""),
      row.names = FALSE,
      col.names = TRUE,
      sep = ",",
      append = TRUE
    )    
    write.table(
      TempRaw,
      paste("RawHRData",Sys.Date(),".csv", sep=""),
      row.names = FALSE,
      col.names = TRUE,
      sep = ",",
      append = TRUE
    )    
    
    
    ggsave(paste("HRPlot_",df()$Participant[1],"_",Sys.Date(),".png"),plot=HRInput())
    ggsave(paste("HRDensity_",df()$Participant[1],"_",Sys.Date(),".png"),plot=HRHistInput())
    ggsave(paste("TempPlot_",df()$Participant[1],"_",Sys.Date(),".png"),plot=TempInput())
    ggsave(paste("TempDensity_",df()$Participant[1],"_",Sys.Date(),".png"),plot=TempHistInput())
    
  })
  
  #session$onSessionEnded(function() { 
  #  stopApp()
  #  q("no") 
  #})
  
  
}

shinyApp(ui, server)
#runApp(list(ui=ui, server=server),launch.browser = TRUE)



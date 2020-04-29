# Author: David Black
# Date: Jan '20
# GUI that reads and cleans APS, Nano scan, and ELPI data

# Produces the raw (APS and Nano scan) or normalized (ELPI) data

# Generates total concentration, median size, particle size distribution, and
# relative frequency plots

# Animates the particle size distribution over time

# Produces a summary table of the mean, median, mode, Geo SD particle size
# averaged over the peak and end and immediate peak and end values

# User defines: the start and end of signal, dilution correction factor 
# (multiplies data by value), whether to bin relative frequency and 
# particle size distribution plots, time interval for summary table and 
# particle size distribution, and number of frames to skip to shorten/speed up
# animation time series

list.of.packages <- c("shiny",
                      "plotly",
                      "gridExtra",
                      "dplyr",
                      "ggplot2",
                      "shinydashboard",
                      "DT",
                      "readxl")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
library(shiny)
library(plotly)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(shinyjs)
library(DT)
library(readxl)

#Increase size Shiny can handle
options(shiny.maxRequestSize = 100 * 1024 ^ 2)
fileuploadPeak <<- FALSE
fileuploadEnd <<- FALSE
fileuploadInterval <<- FALSE
inFile <<- NULL
instrument <<- NULL
DataDate <<- NULL
#############################################################################
###         UI Section

ui <- dashboardPage(
  dashboardHeader(title = "Composite Data"),
  dashboardSidebar(
    width = 250,
    div(style = "display:inline-block;vertical-align:top;"),
    
    fileInput(
      "file1",
      "Choose APS/Nano/ELPI File",
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    submitButton("Submit"),
    fluidRow(
    column(4, checkboxInput("bin", "Bin Graphs", FALSE)
    ),
    column(4,
           checkboxInput("background", "Filter Noise", FALSE))),
    
    fluidRow(column(6,
      numericInput(
      width = 110,
      "peak",
      "Start Signal:",
      NULL,
      min = 1
    )),
    column(6,
      numericInput(
      width = 110,
      "end",
      "End Signal:",
      NULL,
      min = 1
    ))),

    fluidRow(column(6,
      numericInput(
        width=110,
      "SBackground",
      "Start Background:",
      NULL,
      min = 1
  )),
  column(6, numericInput(
    width=110,
    "EBackground",
    "End Background:",
    NULL,
    min = 1
  ))),
  
  fluidRow(column(6,numericInput(
                    width = 110,
                    "interval",
                    "Time Interval:",
                    NULL,
                    min = 1
                  )),
                  column(6,
    numericInput(
    width = 110, 
    "dilution", 
    "Dilution Factor:", 
    1))),
  
  actionButton("action", "Download"),
  textInput("ConTitle","Title of Concentration Plot:"),
  textInput("DistTitle","Title of Dist. Plot:"),
  textInput("RFTitle","Title of Rel. Freq. Plot:")
  ),
  dashboardBody(
    tabsetPanel(
      type = "tabs",
      tabPanel("Raw Data", DT::dataTableOutput("contents"),
               style = "height:500px;overflow-y: scroll; overflow-x: scroll;"),
      tabPanel("Total Concentration", plotlyOutput("ConGraph")),
      tabPanel("Particle Size Median Graphs",
               plotlyOutput("MedianPlots")),
      tabPanel("Particle Size Graphs", uiOutput("PSGraph")),
      tabPanel("Relative Freq Graphs", plotlyOutput("RelativeSize")),
      tabPanel("Summary Table", dataTableOutput("Summary"),
               style = "height:500px;overflow-y: scroll; overflow-x: scroll;"),
      tabPanel("Help", htmlOutput("help"))
      
    )
  )
)

##############################################################################
##############################################################################
##############################################################################
###         Server Section

server <- function(input, output, session) {

#############################################################################
###         Read and clean APS, Nano scan, and ELPI data
 
  df<- reactiveValues(data=NULL)
  
observeEvent(input$file1,{
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    showNotification("Calculating...", duration = 10)
    updateNumericInput(session, inputId = "peak", value = NULL)
    #Determine File Type
    filetype <-
      substr(inFile$name, nchar(inFile$name) - 2, nchar(inFile$name))
    #Determine Instrument based on file type
    if (filetype == "txt") {
      instrument <<- "APS"
    }
    else if (filetype == "csv") {
      instrument <<- "Nano"
    }
    else {
      instrument <<- "ELPI"
    }
    #Clean Data
    
    if (instrument == "APS") {
      skipline <- 6
      removeSI <- 57
      removeEI <- 76
      renameSI <- 57
      renameEI <- 62
      rawdataSI <- 5
      rawdataEI <- 56
      #Read and transpose data
      mydata <- read.delim(
        inFile$datapath,
        skip = skipline,
        stringsAsFactors = FALSE,
        header = TRUE,
        check.names = FALSE,
        na.strings = "-1.#IND"
      )
      
      col1 <- colnames(mydata)
      mydata <- rbind(col1, mydata)
      tnames <- mydata[, 1]
      mydata <- as.data.frame(t(mydata[, -1]))
      colnames(mydata) <- tnames
      #Remove "<" from column name
      colnames(mydata) <- gsub("<", "", colnames(mydata))
      
      #Rename columns and convert to proper format
      colnames(mydata) <- gsub("Start Time", "Time", colnames(mydata))
      colnames(mydata) <- gsub("Sample #", "Index", colnames(mydata))
      mydata$Index <- as.numeric(as.character(mydata$Index))
      mydata$Time <- as.character(mydata$Time)
      mydata <- mydata[, -c(removeSI:removeEI)]
      colnames(mydata)[renameSI:renameEI] <-
        c("Median", "Mean", "GMean",
          "Mode", "GSD", "Concentration")
      
      #Change Particle Count Data to Numeric Format
      mydata[, 5:62] <- sapply(mydata[, 5:62], as.character)
      mydata[, 5:62] <- sapply(mydata[, 5:62], as.numeric)
      #Dilution Correction
      mydata[, c(rawdataSI:rawdataEI, renameEI)] <-
        mydata[, c(rawdataSI:rawdataEI, renameEI)] * input$dilution
      #Get Date
      DataDate <<- as.character(mydata$Date[1])
      
    }
    else if (instrument == "Nano") {
      skipline = 8
      removeSI <- 24
      removeEI <- 25
      renameSI <- 17
      renameEI <- 23
      rawdataSI <- 4
      rawdataEI <- 16
      #Read and clean data
      mydata <-
        read.csv(
          inFile$datapath,
          skip = skipline,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          header = TRUE
        )
      colnames(mydata) <- gsub("Sample #", "Index", colnames(mydata))
      mydata$Index <- as.numeric(mydata$Index)
      mydata <- mydata[, -c(removeSI:removeEI)]
      #Rename Columns
      colnames(mydata)[renameSI:renameEI] <-
        c("Concentration",
          "Median",
          "Mean",
          "GMean",
          "Mode",
          "GSD",
          "Particle Density")
      
      #Dilution Correction
      mydata[, c(rawdataSI:rawdataEI + 1)] <-
        mydata[, c(rawdataSI:rawdataEI + 1)] * input$dilution
      #Change format of time column
      DataDate <<-
        as.character(as.Date(mydata$Date[1], format = "%m/%d/%Y"))
    }
    
    else{
      skipline <- 6
      removeSI <- 24
      removeEI <- 25
      renameSI <- 17
      renameEI <- 23
      rawdataSI <- 4
      rawdataEI <- 16
      #Read and clean data
      mydata <-
        as.data.frame(read_xlsx(
          inFile$datapath,
          "calculated.moment",
          skip = 6,
          col_names = TRUE
        ))
      DataDate <<-
        as.character(as.Date(mydata$Time[1], format = "%m/%d/%Y"))
      DataDate <<- as.character(as.Date(mydata$Time[1]))
      colnames(mydata)[17] <- "Concentration"
      mydata$Index <- rep(1:nrow(mydata))
      mydata <- mydata[, c(ncol(mydata), 1:(ncol(mydata) - 1))]
      mydata <- mydata[, -3]
      #rename columns to shorten decimal
      colnames(mydata)[3:16] <-
        as.character(round(as.numeric(colnames(mydata[, 3:16])), 3))
      
      #Calculate Median, Mean, Geometric SD
      size <- as.numeric(colnames(mydata[, 3:16]))
      sizeln <- log(size)
      for (i in 1:nrow(mydata)) {
        temp <- mydata[i, 3:16]
        mydata$Median[i] <- exp(sum(sizeln * temp) / sum(temp))
        mydata$Mean[i] <- sum(temp * size) / (sum(temp))
        mydata$GSD[i] <-
          exp(sqrt(sum(((sizeln - log(mydata$Median[i])) ^ 2
          ) * temp) / (sum(temp) - 1)))
        mydata$MassMedian[i] <- mydata$Median[i]*exp((log(mydata$GSD[i])^2)*3)
        
      }
      
    }
    #Calculate max peak
    #mydata <- mydata %>% filter_all(all_vars(GSD < 2.5))
    #mydata <- mydata %>% filter_all(all_vars(!is.infinite(.)))
    MaxIndex <- which.max(mydata$Concentration)
    #Change upload booleans
    fileuploadPeak <<- TRUE
    fileuploadEnd <<- TRUE
    fileuploadInterval <<- TRUE
    df$data <- mydata
  })
#     #############################################################################
#     ###             Background
#     # Filters out background data 
    # observeEvent(input$background,{
    #   inFile <- input$file1
    #   if (is.null(inFile))
    #     return(NULL)
    #   if(input$background){
    #   if (!is.na(input$SBackground)&&(!is.na(input$EBackground))){
    # 
    #     if (instrument == "APS") {
    #       rawdataSI <- 5
    #       rawdataEI <- 56
    #     }
    #     else if (instrument == "Nano") {
    #       rawdataSI <- 4
    #       rawdataEI <- 16
    #     }
    #     else{
    #       rawdataSI <- 3
    #       rawdataEI <- 16
    #     }
    #     backgrounddata <- colMeans(df$data[input$SBackground:input$EBackground,
    #                                     rawdataSI:rawdataEI],na.rm=T)
    #     df$data[,rawdataSI:rawdataEI]=df$data[,rawdataSI:rawdataEI]-backgrounddata
    #     browser()
    #     }
    #     }
    #    })
    # 
#############################################################################
###             filter ELPI Raw Data

filteredELPIdata <- reactive({
  temp <- df$data
  dftemp <- df$data
  s <- input$SBackground
  e <- input$EBackground
  backgrounddata <- colMeans(temp[s:e,3:16],na.rm=T)
  temp=sweep(temp[,3:16],2,backgrounddata)
  #temp=temp[,3:16]-backgrounddata
  temp[temp<0] <- 0
  dftemp[,3:16] <- temp
  dftemp$Concentration <- rowSums(temp)

  size <- as.numeric(colnames(dftemp[, 3:16]))
  sizeln <- log(size)
  for (i in 1:nrow(dftemp)) {
    temp <- dftemp[i, 3:16]
    dftemp$Median[i] <- exp(sum(sizeln * temp) / sum(temp))
    dftemp$Mean[i] <- sum(temp * size) / (sum(temp))
    dftemp$GSD[i] <-
      exp(sqrt(sum(((sizeln - log(dftemp$Median[i])) ^ 2
      ) * temp) / (sum(temp) - 1)))
    dftemp$MassMedian[i] <- dftemp$Median[i]*exp((log(dftemp$GSD[i])^2)*3)
  }
  return(dftemp)
})
  #############################################################################
  ###             Subset Data
  # Subsets data 
  
  tempdata <- reactive({
    td <- df$data
    
    if (instrument == "APS") {
      rawdataSI <- 5
      rawdataEI <- 56
    }
    else if (instrument == "Nano") {
      rawdataSI <- 4
      rawdataEI <- 16
    }
    else{
      rawdataSI <- 3
      rawdataEI <- 16
    }
    
    return(td[, rawdataSI:rawdataEI])
  })
  ##############################################################################
  ###             NORMALIZE ELPI DATA FOR GRAPHS
  # If ELPI, normalizes data: 
  # Particle Count / (log(end of size bin)+ log(start of size bin))
  
  NormData <- reactive({
    tempnorm <- tempdata()
    # if (instrument == "ELPI") {
    #   tempnorm[, 1] <- tempnorm[, 1] / (log10(.014) - log10(.006))
    #   tempnorm[, 2] <- tempnorm[, 2] / (log10(.0191) - log10(.014))
    #   tempnorm[, 3] <- tempnorm[, 3] / (log10(.0336) - log10(.0191))
    #   tempnorm[, 4] <- tempnorm[, 4] / (log10(.0508) - log10(.0336))
    #   tempnorm[, 5] <- tempnorm[, 5] / (log10(.098) - log10(.0508))
    #   tempnorm[, 6] <- tempnorm[, 6] / (log10(.17) - log10(.098))
    #   tempnorm[, 7] <- tempnorm[, 7] / (log10(.315) - log10(.17))
    #   tempnorm[, 8] <- tempnorm[, 8] / (log10(.59) - log10(.315))
    #   tempnorm[, 9] <- tempnorm[, 9] / (log10(.909) - log10(.59))
    #   tempnorm[, 10] <- tempnorm[, 10] / (log10(1.63) - log10(.909))
    #   tempnorm[, 11] <- tempnorm[, 11] / (log10(2.47) - log10(1.63))
    #   tempnorm[, 12] <- tempnorm[, 12] / (log10(3.65) - log10(2.47))
    #   tempnorm[, 13] <- tempnorm[, 13] / (log10(5.36) - log10(3.65))
    #   tempnorm[, 14] <- tempnorm[, 14] / (log10(9.87) - log10(5.36))
    #   
    # }

    if(input$background){
      if (!is.na(input$SBackground)&&(!is.na(input$EBackground))){
    #browser()    
    backgrounddata <- colMeans(tempnorm[input$SBackground:input$EBackground,],na.rm=T)
    #tempnorm=tempnorm-backgrounddata
    tempnorm=sweep(tempnorm,2,backgrounddata)
    tempnorm[tempnorm<0] <- 0
      }}
      return(tempnorm)
  })
  
  ##############################################################################
  ###             RAW DATA TABLE
  # Outputs raw data
  # If ELPI it outputs normalized data
  
  output$contents <- renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    temprawdata = df$data
    if (instrument == "ELPI") {
      if(input$background){
        if (!is.na(input$SBackground)&&(!is.na(input$EBackground))){
          temprawdata <- filteredELPIdata()#filteredELPI(df$data,input$SBackground,input$EBackground)
        }
      }

      temprawdata[, 3:16] <- NormData()
      }
    else if (instrument == "APS") {
      temprawdata[, 5:56] <- NormData()
    }
    else {
      temprawdata[, 4:16] <- NormData()
    }
    
    #return(
    #  datatable(temprawdata) %>% formatRound(c(3:17), 3) %>%
    #    formatStyle(columns = c(3:17), 'text-align' = 'center')
        
    #)
    temprawdata
  },options = list(lengthMenu = c(100, 500, 1000), pageLength = 100))

  
  ##############################################################################
  ###             CREATE PLOT AREAS
  # Creates plot areas for particle size distribution plots 
  # based on time interval
  
  output$PSGraph <- renderUI({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    plot_output_list <- lapply(1:numplots(), function(i) {
      plotname <- paste("plot", i, sep = "")
      plotlyOutput(plotname, height = 600, width = 800)
    })
    
    do.call(tagList, plot_output_list)

  })
  
  ##############################################################################
  ###             CALCULATE NUMBER OF PLOTS
  
  numplots <- reactive({
    if (ends() != peaks()) {
      if (intervals() <= (ends() - peaks())) {
        numplots <- floor((ends() - peaks()) / intervals())
      }
    }
    else{
      numplots <- 1
    }
  })
  ##############################################################################
  ###             PEAK VALUE CHECK
  
  peaks <- reactive({
    if ((input$peak > nrow(df$data)) ||
        (is.na(input$peak)) || (input$peak < 1) ||
        (fileuploadPeak == TRUE)) {
      temppeaks <- which.max(df$data$Concentration)
      fileuploadPeak <<- FALSE
    }
    else {
      temppeaks <- input$peak
    }
    updateNumericInput(session, inputId = "peak", value = temppeaks)
    return(temppeaks)
  })
  ##############################################################################
  ###             END VALUE CHECK
  
  ends <- reactive({
    if ((input$end > nrow(df$data)) ||
        (is.na(input$end)) || (input$end < 1) || (fileuploadEnd == TRUE)) {
      tempends <- nrow(df$data)
      fileuploadEnd <<- FALSE
    }
    else {
      tempends <- input$end
    }
    updateNumericInput(session, inputId = "end", value = tempends)
    return(tempends)
  })
  ##############################################################################
  ###             INTERVAL VALUE CHECK
  
  intervals <- reactive({
    tend <- ends()
    tpeak <- peaks()
    
    if ((input$interval > (tend - tpeak)) ||
        (is.na(input$interval)) ||
        (input$interval < 1) || (fileuploadInterval == TRUE)) {
      tempintervals <- (tend - tpeak)
      fileuploadInterval <<- FALSE
    }
    else if (tend == tpeak) {
      tempinterval == 1
    }
    else {
      tempintervals <- input$interval
    }
    updateNumericInput(session, inputId = "interval", value = tempintervals)
    return(tempintervals)
  })
  

  ##############################################################################
  ###             DISTRIBUTIONS OF PARTICLE SIZE IN TIME INTERVALS
  # Plots the avg distribution of particle sizes for each time interval
  # If bins option is checked, it takes the sum across the size interval
  
  observe({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    else {
      #showNotification("Calculation in progress...",duration=10)
      if (input$bin == FALSE) {
        tempdata1 <- NormData()
        Size <-
          factor(colnames(tempdata1),
                 ordered = TRUE,
                 levels = colnames(tempdata1))
        
      }
      else{
        if (instrument == "APS") {
          tempdata1 <-
            data.frame(
              "Bin1" = rowSums(NormData()[, 1:10], na.rm = TRUE),
              "Bin2" = rowSums(NormData()[, 11:23], na.rm =
                                 TRUE),
              "Bin3" = rowSums(NormData()[, 24:29], na.rm =
                                 TRUE),
              "Bin4" = rowSums(NormData()[, 30:52], na.rm =
                                 TRUE)
            )
          Size <- c("< 1", "1.00-2.49", "2.50-3.99", "4+")
        }
        else if (instrument == "ELPI") {
          tempdata1 <- data.frame(
            "Bin1" = rowSums(NormData()[, 1:5], na.rm = TRUE),
            "Bin2" = rowSums(NormData()[, 6:9], na.rm =
                               TRUE),
            "Bin3" = rowSums(NormData()[, 10:12], na.rm =
                               TRUE),
            "Bin4" = rowSums(NormData()[, 13:14], na.rm =
                               TRUE)
          )
          Size <- c("0-0.099", "0.100-1.00", "1.00-3.99", "4+")
        }
        else{
          tempdata1 <- data.frame(
            "Bin1" = rowSums(NormData()[, 1:8], na.rm = TRUE),
            "Bin2" = rowSums(NormData()[, 9:13], na.rm =
                               TRUE)
          )
          Size <- c("0-0.99", "1.00+")
        }
      }
      
      mplot <- max(df$data$Concentration)#peakY()
      for (i in 1:numplots()) {
        local({
          my_i <- i
          s <- (intervals() * (my_i - 1)) + peaks()
          e <- (intervals() * my_i) + peaks()
          plotname <- paste("plot", my_i, sep = "")
          Concentration <-
            as.numeric(colMeans(tempdata1[s:e, ], na.rm = TRUE))
          if (max(Concentration) > mplot) {
            mplot <- max(Concentration)
          }
          smin <- round((s / 60), digits = 2)
          emin <- round((e / 60), digits = 2)
          output[[plotname]] <- renderPlotly({
            p <- ggplot() + geom_bar(
              aes(x = Size, y = Concentration),
              fill = "blue",
              stat = "summary",
              fun.y = "mean"
            )+

            ylab("Avg Concentration")+   
            theme(text = element_text(size = 18)) +
            #   #ylim(0,mplot)+
               theme(axis.text.x = element_text(angle = 45)) +
              theme(axis.text.x = element_text(size=8)) +
              ggtitle(paste(input$DistTitle)) +
              #ggtitle(paste(smin, "to", emin, "min")) +
               theme(plot.title = element_text(hjust = .5))#+
            
            p <- ggplotly(p)
            dev.off()
            return(p)
            
          })
          
        })
      }
    }
  })
  ##############################################################################
  ###             MEDIAN PLOTS
  #Plots the median particle diameter over time
  
  MP <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    plotfilename <- substr(inFile$name, 1, nchar(inFile$name) - 4)
    temprawdata <-df$data
    if (instrument=="ELPI"){
    if(input$background){
      if (!is.na(input$SBackground)&&(!is.na(input$EBackground))){
        temprawdata <- filteredELPIdata()
      }
    }
    }
    p <-ggplot(temprawdata[peaks():ends(), ], aes(Index, Median)) + 
      geom_line(color ="blue") +
      xlab("Time (sec)") + ylab("Median Diameter (\u03bc)") +
      ggtitle(paste("Median Diameter ", plotfilename)) +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      )
    p <- ggplotly(p)
    
  })
  
  
  ##############################################################################
  ###             MEDIAN PLOTS
  
  output$MedianPlots <- renderPlotly({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    MP()
  })
  
  ##################################################################################
  ###             REACTIVE FREQUENCY PLOT
  
  output$RelativeSize <- renderPlotly({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    RF()
  })
  
  ##################################################################################
  ###            REACTIVE RELATIVE FREQUENCY
  #Calculates the contribution of each particle size (0 - 1)
  #Has the option to bin the data
  
  RF <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    p <- peaks()
    e <- ends()

    tempSum <- rowSums(NormData()[p:e, ])
    tempRF <- NormData()[p:e, ] / tempSum
    if (input$bin == TRUE) {
      if (instrument=="APS"){
        tempdata1 <- data.frame(
          "Bin 1" = rowSums(tempRF[, 1:10], na.rm = TRUE),
          "Bin 2" = rowSums(tempRF[, 11:23], na.rm =
                              TRUE),
          "Bin 3" = rowSums(tempRF[, 24:29], na.rm =
                              TRUE),
          "Bin 4" = rowSums(tempRF[, 30:52], na.rm =
                              TRUE)
        )
        names(tempdata1)[1] <- "<1"
        names(tempdata1)[2] <- "1-2.49"
        names(tempdata1)[3] <- "2.5-3.99"
        names(tempdata1)[4] <- "4+"
        RFData <- stack(tempdata1)
        RFData$Time <- rep(1:(e - p + 1), times = 4)
        names(RFData)[2] = "Size"
      }
      else if (instrument == "ELPI") {
        tempdata1 <- data.frame(
          "Bin1" = rowSums(tempRF[, 1:5], na.rm = TRUE),
          "Bin2" = rowSums(tempRF[, 6:9], na.rm =
                             TRUE),
          "Bin3" = rowSums(tempRF[, 10:12], na.rm =
                             TRUE),
          "Bin4" = rowSums(tempRF[, 13:14], na.rm =
                             TRUE)
        )
        names(tempdata1)[1] <- "0-0.099"
        names(tempdata1)[2] <- "0.100-0.99 "
        names(tempdata1)[3] <- "1.00-3.99"
        names(tempdata1)[4] <- "4+"
        RFData <- stack(tempdata1)
        names(RFData)[2] = "Size"
        RFData$Time <- rep(1:(e - p + 1), times = 4)      
        
      }
      else{
        tempdata1 <- data.frame(
          "Bin1" = rowSums(tempRF[, 1:8], na.rm = TRUE),
          "Bin2" = rowSums(tempRF[, 9:13], na.rm = TRUE)
        )
        names(tempdata1)[1] <- "0-0.99"
        names(tempdata1)[2] <- "1+ "
        RFData <- stack(tempdata1)
        RFData$Time <- rep(1:(e - p + 1), times = 2)      
        names(RFData)[2] = "Size"
        
      }
    }
    else {
      tempdata1 <- tempRF
      numcol <- ncol(tempdata1)
      RFData <- stack(tempdata1)
      RFData$Time <- rep(1:(e - p + 1), times = numcol)
      names(RFData)[2] = "Size"
      
    }
    plotfilename <- substr(inFile$name, 1, nchar(inFile$name) - 4)
    p <-ggplot(RFData) + geom_line(aes(Time, values, group = Size, 
                                       color = Size)) +
      xlab("Time (sec)") + ylab("Relative Frequency (%)") +
      #ggtitle(paste("Relative Frequency ", plotfilename)) +
      ggtitle(paste(input$RFTitle)) +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      )
    p <- ggplotly(p)
    p
    
  })
  #############################################################################
  ###            REACTIVE SUMMARY TABLE
  #Define data frame for summary data for each instrument
  #Data include statistics at peak, end, and average
  
  SummaryTable <- reactive({
    
    if (instrument == "ELPI") {

      Summary <- data.frame(matrix(ncol = 18, nrow = 0))
      colnames(Summary) <-
        c(
          "Instrument",
          "File Name",
          "Date",
          "Start_Min",
          "End_Min",
          "Avg_Median",
          "Avg_Mean",
          "Avg_Geo_SD",
          "Avg_Total_Concentration",
          "Start_Median",
          "End_Median",
          "Start_Mean",
          "End_Mean",
          "Start_GeoSD",
          "End_GeoSD",
          "Start_Concentration",
          "End_Concentration",
          "Avg_Mass_Median"
        )
        
        if(input$background){
          if (!is.na(input$SBackground)&&(!is.na(input$EBackground))){
            tempsummary <- filteredELPIdata()
          }
        
        }
      else{
        tempsummary <- df$data
      }
      
      for (i in 1:numplots()) {
        s <- (intervals() * (i - 1)) + peaks()
        e <- ((intervals() * i) + peaks())
        # if (e == nrow(tempsummary)){
        #   e <- e - 2
        # }
        # if (s==1){
        #   s <- 3
        # }
        tempsummary1 <- tempsummary %>% filter_all(all_vars(GSD < 2.5))

        smin <- round((s / 60), digits = 2)
        emin <- round((e / 60), digits = 2)
        Summary[i, 1] <- instrument
        Summary[i, 2] <- input$file1$name
        Summary[i, 3] <- DataDate
        Summary[i, 4] <- smin
        Summary[i, 5] <- emin
        Summary[i, 6] <- mean(tempsummary$Median[s:e], na.rm = T)
        Summary[i, 7] <- mean(tempsummary$Mean[s:e], na.rm = T)
        Summary[i, 8] <- mean(tempsummary$GSD[s:e], na.rm = T)
        Summary[i, 9] <- mean(tempsummary$Concentration[s:e], na.rm = T)
        Summary[i, 10] <- (tempsummary$Median[s])
        Summary[i, 11] <- (tempsummary$Median[e])
        Summary[i, 12] <- (tempsummary$Mean[s])
        Summary[i, 13] <- (tempsummary$Mean[e])
        Summary[i, 14] <- (tempsummary$GSD[s])
        Summary[i, 15] <- (tempsummary$GSD[e])
        Summary[i, 16] <- (tempsummary$Concentration[s])
        Summary[i, 17] <- (tempsummary$Concentration[e])
        Summary[i, 18] <- mean(tempsummary1$MassMedian,na.rm=T)


        }
    }
    else{
      Summary <- data.frame(matrix(ncol = 24, nrow = 0))
      colnames(Summary) <-
        c(
          "Instrument",
          "File Name",
          "Date",
          "Start_Min",
          "End_Min",
          "Avg_Median",
          "Avg_Mean",
          "Avg_GMean",
          "Avg_Mode",
          "Avg_Geo_SD",
          "Avg_Total_Concentration",
          "Rate of Change",
          "Start_Median",
          "End_Median",
          "Start_Mean",
          "End_Mean",
          "Start_GMean",
          "End_GMean",
          "Start_Mode",
          "End_Mode",
          "Start_GeoSD",
          "End_GeoSD",
          "Start_Concentration",
          "End_Concentration"
        )
      
      for (i in 1:numplots()) {
        s <- (intervals() * (i - 1)) + peaks()
        e <- (intervals() * i) + peaks()
        rateofchange <-
          (df$data$Concentration[e] - df$data$Concentration[s]) / (e - s)
        if (e == nrow(df$data)){
          e <- e - 2
        }
        if (s==1){
          s <- 3
        }        
        smin <- round((s / 60), digits = 2)
        emin <- round((e / 60), digits = 2)
        Summary[i, 1] <- instrument
        Summary[i, 2] <- input$file1$name
        Summary[i, 3] <- DataDate
        Summary[i, 4] <- smin
        Summary[i, 5] <- emin
        Summary[i, 6] <- mean(df$data$Median[s:e], na.rm = T)
        Summary[i, 7] <- mean(df$data$Mean[s:e], na.rm = T)
        Summary[i, 8] <- mean(df$data$GMean[s:e], na.rm = T)
        Summary[i, 9] <- mean(df$data$Mode[s:e], na.rm = T)
        Summary[i, 10] <- mean(df$data$GSD[s:e], na.rm = T)
        Summary[i, 11] <- mean(df$data$Concentration[s:e], na.rm = T)
        Summary[i, 12] <- rateofchange
        Summary[i, 13] <- mean(df$data$Median[(s-2):(s+2)], na.rm=T)
        Summary[i, 14] <- mean(df$data$Median[(e-2):(e+2)], na.rm=T)
        Summary[i, 15] <- mean(df$data$Mean[(s-2):(s+2)], na.rm=T)
        Summary[i, 16] <- mean(df$data$Mean[(e-2):(e+2)], na.rm=T)
        Summary[i, 17] <- mean(df$data$GMean[(s-2):(s+2)], na.rm=T)
        Summary[i, 18] <- mean(df$data$GMean[(e-2):(e+2)], na.rm=T)
        Summary[i, 19] <- mean(df$data$Mode[(s-2):(s+2)], na.rm=T)
        Summary[i, 20] <- mean(df$data$Mode[(e-2):(e+2)], na.rm=T)
        Summary[i, 21] <- mean(df$data$GSD[(s-2):(s+2)], na.rm=T)
        Summary[i, 22] <- mean(df$data$GSD[(e-2):(e+2)], na.rm=T)
        Summary[i, 23] <- mean(df$data$Concentration[(s-2):(s+2)], na.rm=T)
        Summary[i, 24] <- mean(df$data$Concentration[(e-2):(e+2)], na.rm=T)
      }
    }
    Summary
  })
  
  
  #################################################################################
  ###             CALL SUMMARY TABLE
  output$Summary <- renderDataTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    datatable(SummaryTable())
  })
  #################################################################################
  ###             REACTIVE CONCENTRATION PLOT
  # Plot the total concentration across time
  
  concentrationPlot <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    #ttime = strptime(df$data$Time, "%Y-%m-%d %H:%M:%S")
    #timexaxis <- as.POSIXct(format(ttime, "%H:%M:%S"), format = "%H:%M:%S")
    tempCon <- df$data
    if (instrument=="ELPI"){
      if(input$background){
        if (!is.na(input$SBackground)&&(!is.na(input$EBackground))){
          tempCon <- filteredELPIdata()#filteredELPI(df$data,input$SBackground,input$EBackground)
        }
      }
    }
    
    plotfilename <- substr(inFile$name, 1, nchar(inFile$name) - 4)
    p <-
      ggplot(tempCon) + geom_line(aes(x = Index, y = Concentration), 
                               color = "blue") +
      geom_vline(xintercept = peaks(),
                 color = 'green',
                 linetype = "dashed") +
      geom_vline(xintercept = ends(),
                 color = 'red',
                 linetype = "dashed") +
      xlab("Time (sec)") +
      labs(y = "Concentration(Num per cm<sup>3</sup>)") +
      ggtitle(paste(input$ConTitle))+
      #ggtitle(paste("Total Concentration ", plotfilename)) +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)
      )
    
    
    p <- ggplotly(p)
  })
  
  ################################################################################
  ###             CALL CONCENTRATION PLOT
  
  output$ConGraph <- renderPlotly({
    inFile <- input$file1
    if (is.null(inFile)) {
      return(NULL)
    }
    else if (is.na(input$peak))
    {
      showModal(modalDialog("Click on submit..."))
      return(NULL)
    }
    
    concentrationPlot()
    
  })
  ##############################################################################
  ###             SAVE DATA
  # Asks user to select directory and saves: Summary of data, raw data,
  # concentration plot, relative frequency plot, and median plot
  
  observeEvent(input$action, {
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    workingdir = choose.dir(default = paste0(getwd(), "/*.*"), caption =
                              "Select Folder")
    setwd(workingdir)
    datafilename <- substr(inFile$name, 1, nchar(inFile$name) - 4)
    write.table(
      SummaryTable(),
      paste("Summary.csv"),
      row.names = FALSE,
      col.names = TRUE,
      sep = ",",
      append = TRUE
    )
    write.table(
      df$data,
      paste("RawData", datafilename, Sys.Date(), ".csv"),
      row.names = FALSE,
      col.names = TRUE,
      sep = ","
    )
    htmlwidgets::saveWidget(concentrationPlot(),
                            file = paste("Concentration", 
                                         datafilename, Sys.Date(), ".html"))
    htmlwidgets::saveWidget(RF(),
                            file = paste("Relative Frequency", 
                                         datafilename, Sys.Date(), ".html"))
    htmlwidgets::saveWidget(MP(),
                            file = paste("Median Plot", 
                                         datafilename, Sys.Date(), ".html"))
  })
  

  ##############################################################################
  ###             Help Section
  
  output$help <- renderUI({
    str0 <-
      paste(
        "This program is designed to read APS, NanoScan, and ELPI files.
  The file format is critical for this application to work properly. See
  XXX for example file formats.",
        '<br/>' ,
        '<br/>'
      )
    
    str1 <-
      paste(
        "Browse: Searches for the data file.",
        '<br/>',
        "Download: Saves
                total concentration, median, and relative frequency graphs in
                htlm. If you need to save it as an image file, open the graph
                in the browser and click on the camera option.",
        '<br/>',
        "Dilution Factor: Allows the user to multiply the data by a
                constant.",
        '<br/>',
        "Peak Index: Assign the start
                of the data set.",
        '<br/>',
        "End Signal Index: Assign the end of
                the data set.",
        '<br/>',
        "Time Interval: Interval used for
                particle size graphs and summary.",
        '<br/>',
        "Max Y Axis: Changes
                maximum value of the y axis for particle size graphs.",
        '<br/>',
        "Num sec per frame: Skips frames to speed up the animation
                plot",
        '<br/>',
        '<br/>'
      )
    
    str2 <-
      paste(
        "The program detects what kind of instrument based on the file type. APS must be a text file.
  The program skips the first 6 lines of code and starts reading the data. The
  columns start with Sample #, Date, Start Time, Aerodynamic Diameter, and the
  data columns that run through column 56. The program skips columns 57 through
  76. The data picks back up from 77 to 82. Under Total Concentration, the
  units must be present in data",
        '<br/>' ,
        '<br/>'
      )
    
    str3 <-
      paste(
        "NanoScan files must be CSV. The program skips the first 8
                lines. The columns are File Index, Sample #, Start Time,
                data from columns 4 - 16. Next 7 columns are data and it
                skips the rest.",
        '<br/>' ,
        '<br/>'
      )
    str4 <-
      paste(
        "ELPI files must an Excel Macro file (.xlsm). The program
                reads the 4th tab, Calculated Moments. The application reads
                columns A through Q starting on row 7. It calculates the
                median and Geometric SD."
      )
    HTML(paste(str0, str1, str2, str3, str4))
  })
  
}

##############################################################################
##############################################################################

shinyApp(ui, server)
#runApp(list(ui=ui, server=server),launch.browser = TRUE)


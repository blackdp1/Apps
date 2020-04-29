# Author: David Black
# Date: Feb '20
# GUI that reads and cleans APS, Nano scan, and ELPI data
# Dynamically populates 2 dropdowns to select columns to plot
# Plots the scatter plot of the 2 variables with regression line and output
# Overlays 2 time series plots 
# Also separates the two time series plots
# Requires .csv file with column names and time series data in the rows

list.of.packages <- c("shinydashboard", "plotly", "ggplot2", "dplyr")
new.packages <- list.of.packages[!(list.of.packages %in%
                                     installed.packages()[, "Package"])]
if (length(new.packages))
  install.packages(new.packages)
library(plotly)
library(dplyr)
library(ggplot2)
library(shinydashboard)


#Increase size Shiny can handle
options(shiny.maxRequestSize = 100 * 1024 ^ 2)
globaldf <<- data.frame()

#############################################################################
#             Regression Equation
# Creates regression equation with r^2
lm_eqn <- function(df1) {
  g <- as.character("y = a + b x, r2 = R2 ")
  
  m <- lm(df1[, 2] ~ df1[, 1])
  
  eq <- g %<>%
    gsub("a", format(coef(m)[1], digits = 2), .) %>%
    gsub("b", format(coef(m)[2], digits = 2), .) %>%
    gsub("R2", format(summary(m)$r.squared, digits = 3), .)
}
#############################################################################
#             UI

ui <- dashboardPage(
  dashboardHeader(title = "Plot 2 Variables"),
  dashboardSidebar(
    width = 250,
    fileInput(
      "file1",
      "Choose .CSV File",
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    #selectInput("PlotType","Choose Type of Plot:",choices=c("Scatter Plot"= "scp","Time Series"="ts")),
    selectInput("x_axis", "Choose Axis 1:", choices = NULL),
    selectInput("y_axis", "Choose Axis 2:", choices = NULL),
    actionButton("Submit", "Submit")
    
    
  ),
  dashboardBody(
    verbatimTextOutput("SummaryModel"),
    plotlyOutput("DataPlot"),
    plotOutput("TSDataPlot"),
    plotlyOutput("TSDataPlot2", height = 1000)
    
  )
)


##############################################################################
##############################################################################
##############################################################################
#             Server

server <- function(input, output, session) {
  df <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    globaldf <<- read.csv(inFile$datapath)
    
  })
  #############################################################################
  #             Populates dropdown with colnames
  
  observe({
    updateSelectInput(session, "x_axis", choices = colnames(df()))
    updateSelectInput(session, "y_axis", choices = colnames(df()))
  })
  ##########################################################################
  #             Scatter plot
  # Plots both variables as a scatter plot with regression line and equation
  
    dp <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    plotdata <- globaldf[c(input$x_axis, input$y_axis)]
    
    mod <- fitted(lm(plotdata[, 2] ~ plotdata[, 1]))
    plot_ly(x =  ~ plotdata[, 1], y =  ~ plotdata[, 2]) %>%
      layout(
        xaxis = list(title = colnames(plotdata[1])),
        yaxis = list(title = colnames(plotdata[2])),
        showlegend = FALSE,
        title = lm_eqn(plotdata)
      ) %>%
      add_markers(y = ~ plotdata[, 2]) %>%
      add_lines(y = ~ mod)
  })
  
  ##########################################################################
#             Overlay time series plot
# Plots both time series plots with regression equation on same plot
    
  ts_Plot <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    plotdata <- globaldf[c(input$x_axis, input$y_axis)]
    plotdata$Time <- 1:nrow(plotdata)
    Var1Title <- colnames(plotdata[1])
    Var2Title <- colnames(plotdata[2])
    PlotTemp = rbind(
      data.frame(
        Time = plotdata$Time,
        TVar = plotdata[, 1],
        DataSet = Var1Title
      ),
      data.frame(
        Time = plotdata$Time,
        TVar = plotdata[, 2],
        DataSet = Var2Title
      )
    )
    ratio <- max(plotdata[, 1]) / max(plotdata[, 2])
    ggplot(PlotTemp, aes(x = Time)) +
      geom_line(aes(y = TVar, color = DataSet), data = PlotTemp[PlotTemp$DataSet ==
                                                                  Var1Title, ]) +
      geom_line(aes(y = TVar * ratio, color = DataSet), data = PlotTemp[PlotTemp$DataSet ==
                                                                          Var2Title, ]) +
      scale_y_continuous(name = Var1Title,
                         sec.axis = sec_axis( ~ . / ratio, name = Var2Title)) +
      theme(text = element_text(size = 20))
    
  })
  ##########################################################################
  #           Two Time Series Plots
  # Separates both time series plots 
  
  twoTSPlots <- reactive({
    plotdata <- globaldf[c(input$x_axis, input$y_axis)]
    plotdata$Time <- 1:nrow(plotdata)
    Var1Title <- colnames(plotdata[1])
    Var2Title <- colnames(plotdata[2])
    PlotTemp = rbind(
      data.frame(
        Time = plotdata$Time,
        TVar = plotdata[, 1],
        DataSet = Var1Title
      ),
      data.frame(
        Time = plotdata$Time,
        TVar = plotdata[, 2],
        DataSet = Var2Title
      )
    )
    
    ggplotly(
      ggplot(PlotTemp) + geom_line(aes(
        x = Time, y = TVar, color = DataSet
      )) +
        facet_grid(rows = vars(DataSet), scales = "free_y") +
        theme(axis.title.y = element_blank(), text = element_text(size = 16))
    )
    
  })
  ##########################################################################
  #       `   Main Body
  # Calls all the reactive statements
  
  observeEvent(input$Submit, {
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    output$TSDataPlot <- renderPlot({
      ts_Plot()
    })
    output$SummaryModel <- renderPrint({
      plotdata <- globaldf[c(input$x_axis, input$y_axis)]
      mod <- lm(plotdata[, 2] ~ plotdata[, 1])
      summary(mod)
      
    })
    output$DataPlot <- renderPlotly({
      dp()
    })
    output$TSDataPlot2 <- renderPlotly({
      twoTSPlots()
    })
  })
}
shinyApp(ui, server)


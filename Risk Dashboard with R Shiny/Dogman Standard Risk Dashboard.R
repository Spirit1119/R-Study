library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(party)
library(caret)
library(plotly)
library(curl)
library(RCurl)
library(e1071)
library(rpart)
library(ranger)
library(HARModel)
library(highfrequency)
library(QuantTools)
library(pdfetch)
library(lubridate)
library(tidyverse)
library(PerformanceAnalytics)
library(quantmod)
library(neuralnet)
library(caret)

# Download data
price_10 <- as.data.frame(pdfetch_YAHOO(c("SPY", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU"), fields = c("adjclose"), from = ymd(Sys.Date()) - years(10), to = Sys.Date(), interval = "1d"))
price_10$date <- as.Date(rownames(price_10), "%Y-%m-%d")
price_10[is.na(price_10)] <- 0

price_5 <- as.data.frame(pdfetch_YAHOO(c("SPY", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU"), fields = c("adjclose"), from = ymd(Sys.Date()) - years(5), to = Sys.Date(), interval = "1d"))
price_5$date <- as.Date(rownames(price_5), "%Y-%m-%d")
price_5[is.na(price_5)] <- 0

price_3 <- as.data.frame(pdfetch_YAHOO(c("SPY", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU"), fields = c("adjclose"), from = ymd(Sys.Date()) - years(3), to = Sys.Date(), interval = "1d"))
price_3$date <- as.Date(rownames(price_3), "%Y-%m-%d")
price_3[is.na(price_3)] <- 0

price_1 <- as.data.frame(pdfetch_YAHOO(c("SPY", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU"), fields = c("adjclose"), from = ymd(Sys.Date()) - years(1), to = Sys.Date(), interval = "1d"))
price_1$date <- as.Date(rownames(price_1), "%Y-%m-%d")
price_1[is.na(price_1)] <- 0


# Caluculate daily,weekly and monthly return
Log_Ret_10 <- as.data.frame(CalculateReturns(price_10[, -11], method = "log"))
Log_Ret_10[is.na(Log_Ret_10)] <- 0
Weekly_Ret_10 <- as.data.frame(rollsumr(Log_Ret_10[, -11], k = 5, fill = 0))
Monthly_Ret_10 <- as.data.frame(rollsumr(Log_Ret_10[, -11], k = 21, fill = 0))
Log_Ret_10$date <- as.Date(rownames(price_10), "%Y-%m-%d")
Weekly_Ret_10$date <- as.Date(rownames(price_10), "%Y-%m-%d")
Monthly_Ret_10$date <- as.Date(rownames(price_10), "%Y-%m-%d")

Log_Ret_5 <- as.data.frame(CalculateReturns(price_5[, -11], method = "log"))
Log_Ret_5[is.na(Log_Ret_5)] <- 0
Weekly_Ret_5 <- as.data.frame(rollsumr(Log_Ret_5[, -11], k = 5, fill = 0))
Monthly_Ret_5 <- as.data.frame(rollsumr(Log_Ret_5[, -11], k = 21, fill = 0))
Log_Ret_5$date <- as.Date(rownames(price_5), "%Y-%m-%d")
Weekly_Ret_5$date <- as.Date(rownames(price_5), "%Y-%m-%d")
Monthly_Ret_5$date <- as.Date(rownames(price_5), "%Y-%m-%d")

Log_Ret_3 <- as.data.frame(CalculateReturns(price_3[, -11], method = "log"))
Log_Ret_3[is.na(Log_Ret_3)] <- 0
Weekly_Ret_3 <- as.data.frame(rollsumr(Log_Ret_3[, -11], k = 5, fill=0))
Monthly_Ret_3 <- as.data.frame(rollsumr(Log_Ret_3[, -11], k = 21, fill=0))
Log_Ret_3$date <- as.Date(rownames(price_3), "%Y-%m-%d")
Weekly_Ret_3$date <- as.Date(rownames(price_3), "%Y-%m-%d")
Monthly_Ret_3$date <- as.Date(rownames(price_3), "%Y-%m-%d")

Log_Ret_1 <- as.data.frame(CalculateReturns(price_1[, -11], method = "log"))
Log_Ret_1[is.na(Log_Ret_1)] <- 0
Weekly_Ret_1 <- as.data.frame(rollsumr(Log_Ret_1[, -11], k = 5, fill = 0))
Monthly_Ret_1 <- as.data.frame(rollsumr(Log_Ret_1[, -11], k = 21, fill = 0))
Log_Ret_1$date <- as.Date(rownames(price_1), "%Y-%m-%d")
Weekly_Ret_1$date <- as.Date(rownames(price_1), "%Y-%m-%d")
Monthly_Ret_1$date <- as.Date(rownames(price_1), "%Y-%m-%d")

# read highfrequency data
hq_10yr <- read.csv("spy_data_10yr.csv")
hq_5yr <- read.csv("spy_data_5yr.csv")
hq_3yr <- read.csv("spy_data_3yr.csv")
hq_1yr <- read.csv("spy_data_1yr.csv")

# Caluculate model by neutral network
nn_10 <- neuralnet(rv5 ~ bv + rsv_ss + rk_parzen + rk_th2 + medrv + rsv + rv10_ss + rv5_ss + bv_ss + rv10, 
                   data = hq_10yr, hidden = 3)
pred_nn_10 <- compute(nn_10,hq_10yr[,c("bv","rv10_ss", "rk_parzen", "rk_th2","medrv", "rsv", "rv10_ss", "rv5_ss", "bv_ss", "rv10")])
pred_nn_10 <- as.data.frame(abs(pred_nn_10$net.result))
HAR_10 <- cbind(hq_10yr[1],hq_10yr[5],pred_nn_10)
HAR_10$Date <- as.Date(HAR_10$Date)
HAR_10$outlier = apply(HAR_10[1:2516,2:3], 1, max)

nn_5 <- neuralnet(rv5 ~ bv + rsv_ss + rk_parzen + rk_th2 + medrv + rsv + rv10_ss + rv5_ss + bv_ss + rv10, 
                   data = hq_5yr, hidden = 3)
pred_nn_5 <- compute(nn_5,hq_5yr[,c("bv","rv10_ss", "rk_parzen", "rk_th2","medrv", "rsv", "rv10_ss", "rv5_ss", "bv_ss", "rv10")])
pred_nn_5 <- as.data.frame(abs(pred_nn_5$net.result))
HAR_5 <- cbind(hq_5yr[1],hq_5yr[5],pred_nn_5)
HAR_5$Date <- as.Date(HAR_5$Date)
HAR_5$outlier = apply(HAR_5[1:1258,2:3], 1, max)

nn_3 <- neuralnet(rv5 ~ bv + rsv_ss + rk_parzen + rk_th2 + medrv + rsv + rv10_ss + rv5_ss + bv_ss + rv10, 
                  data = hq_3yr, hidden = 3)
pred_nn_3 <- compute(nn_3,hq_3yr[,c("bv","rv10_ss", "rk_parzen", "rk_th2","medrv", "rsv", "rv10_ss", "rv5_ss", "bv_ss", "rv10")])
pred_nn_3 <- as.data.frame(abs(pred_nn_3$net.result))
HAR_3 <- cbind(hq_3yr[1],hq_3yr[5],pred_nn_3)
HAR_3$Date <- as.Date(HAR_3$Date)
HAR_3$outlier = apply(HAR_3[1:754,2:3], 1, max)

nn_1 <- neuralnet(rv5 ~ bv + rsv_ss + rk_parzen + rk_th2 + medrv + rsv + rv10_ss + rv5_ss + bv_ss + rv10, 
                  data = hq_1yr, hidden = 3)
pred_nn_1 <- compute(nn_1,hq_1yr[,c("bv","rv10_ss", "rk_parzen", "rk_th2","medrv", "rsv", "rv10_ss", "rv5_ss", "bv_ss", "rv10")])
pred_nn_1 <- as.data.frame(abs(pred_nn_1$net.result))
HAR_1 <- cbind(hq_1yr[1],hq_1yr[5],pred_nn_1)
HAR_1$Date <- as.Date(HAR_1$Date)
HAR_1$outlier = apply(HAR_5[1:251,2:3], 1, max)


##
ui <- dashboardPage(skin = "purple", 
  dashboardHeader(title = "Dogmand Standard Risk Dashboard", 
                  dropdownMenu(type = "messages", messageItem(from = "Dogman Standard",message = "Welcome!",time = Sys.time()),
                                                  messageItem(from = "New User", message = "Please update your user file.",icon = icon("cog", lib = "glyphicon"),time = Sys.time())),
                  dropdownMenu(type = "notifications",notificationItem(text = "Welcome!",icon("users")),
                                                      notificationItem(text = "Try our new functions now!",icon = icon("exclamation-triangle"),status = "warning")),
                  dropdownMenu(type = "tasks", badgeStatus = "success",taskItem(value = 30, color = "green","User file"))),
  
  dashboardSidebar(
    sidebarMenu(
    menuItem("Parameters", tabName = "Parameters", icon = icon("cog", lib = "glyphicon")),
    menuItem("Historical Summary", tabName = "Summary", icon = icon("dashboard")),
    menuItem("Visualizations", icon = icon("bar-chart-o"),
      menuSubItem("Daily Return", tabName = "DailyReturn"),
      menuSubItem("Weekly Return", tabName = "WeeklyReturn"),
      menuSubItem("Monthly Return", tabName = "MonthlyReturn")),
    menuItem("Models", tabName = "Models", icon = icon("refresh")),
    menuItem("Results", tabName = "Results", icon = icon("list-alt")))),
                    
  dashboardBody(
    tabItems(                                       
      tabItem(tabName = "Parameters", h2("Input Details"),
              fluidRow(
                box(title = "Please select index:", height = 150, width =4,solidHeader = TRUE, status = "primary",
                    selectInput(inputId = "index",label = h6(""),
                    choices = list("SPY" = 1, "XLY" = 2, "XLP" = 3, "XLE" = 4,"XLF" = 5, "XLV" = 6,
                                   "XLI" = 7, "XLB" = 8, "XLK" = 9, "XLU" = 10, "XLC" = 11), selected = 1)),
                box(title = "Please select time horizon:", height = 150, width =4,solidHeader = TRUE, status = "warning",
                    selectInput(inputId = "date",label = h6(""),
                    choices = list("Past 10 years" = 1, "Past 5 years" = 2, "Past 3 years" = 3, "Past 1 year" = 4), selected = 1)),
                box(title = "Please select expected return in %:", height = 150, width =4, solidHeader = TRUE, status = "info",
                    numericInput("return_threshold", label = h6(""), min = -10, max = 10, value = 0, step = 1)),
        
              fluidRow(
                column(width = 8,
                box(align = "center", title = "Submission", height = 110, width = 3, background = "red", actionButton("submit","Submit")))))),
     
      tabItem(tabName = "Summary",h2("Summary"),
              fluidRow(
                box(title = "Dataset Summary", background = "purple", width = 10, status = "primary", 
                    solidHeader = TRUE, collapsible = TRUE,verbatimTextOutput("dailysummary"),verbatimTextOutput("weeklysummary"),verbatimTextOutput("monthlysummary")),
                box(title = "Model Summary", background = "purple",width = 7, status = "primary",
                    solidHeader = TRUE, collapsible = TRUE,verbatimTextOutput("summary2")))),
      
      tabItem(tabName = "DailyReturn",h2("Daily Return"),
              fluidRow( 
                box(title = "Daily Return",  width = 12,  background = "red",status = "primary", solidHeader = TRUE, 
                    collapsible = TRUE, textOutput("DAY"),plotlyOutput("viewDay"),plotlyOutput("viewday")))),
      tabItem(tabName = "WeeklyReturn", h2("Weekly Return"),
              fluidRow(
                box(title = "Weekly Return",  width = 12,  background = "red", status = "primary", solidHeader = TRUE, 
                    collapsible = TRUE, textOutput("WEEK"), plotlyOutput("viewWeek"), plotlyOutput("viewweek")))),
      tabItem(tabName = "MonthlyReturn",h2("Monthly Return"),
              fluidRow(
                box(title = "Monthly Return",  width = 12,  background = "red",status = "primary", solidHeader = TRUE,
                    collapsible = TRUE,textOutput("MONTH"), plotlyOutput("viewMonth"), plotlyOutput("viewmonth")))),
      
      tabItem(tabName = "Models", h2("Model"),
              fluidRow(
                box(title = "HAR Model with Neutral Network",  width = 12,  height = 500, background = "aqua",status = "primary", 
                    solidHeader = TRUE, collapsible = TRUE,plotlyOutput("HAR")))),
      
      tabItem(tabName = "Results",h2("Prediction"),
              fluidRow(
                box(title = "Entered Observation", background = "orange",width = 8, status = "primary", 
                    solidHeader = TRUE, collapsible = TRUE,tableOutput("table")),
                box(title = "Prediction", background = "red", status = "primary",  
                    solidHeader = TRUE, collapsible = TRUE,verbatimTextOutput("pred")))))))

server <- function(input, output) {
  daily <- reactive({
    x = as.numeric(input$date)
    if (x == 1){
      Log_Ret <- Log_Ret_10
    } else if (x == 2){
      Log_Ret <- Log_Ret_5
    } else if (x == 3){
      Log_Ret <- Log_Ret_3
    } else{
      Log_Ret <- Log_Ret_1
    }
    y = as.numeric(input$index)
    dailyret = as.data.frame(Log_Ret[, y])
    dailyret$date = as.Date(Log_Ret$date, "%Y-%m-%d")
    dailyret
  })
  
  weekly <- reactive({
    x = as.numeric(input$date)
    if (x == 1){
      Weekly_Ret <- Weekly_Ret_10
    } else if (x == 2){
      Weekly_Ret <- Weekly_Ret_5
    } else if (x == 3){
      Weekly_Ret <- Weekly_Ret_3
    } else{
      Weekly_Ret <- Weekly_Ret_1
    }
    y = as.numeric(input$index)
    weeklyret = as.data.frame(Weekly_Ret[,y])
    weeklyret$date = as.Date(Weekly_Ret$date, "%Y-%m-%d")
    weeklyret
  })
  
  monthly <- reactive({
    x = as.numeric(input$date)
    if (x == 1){
      Monthly_Ret <- Monthly_Ret_10
    } else if (x == 2){
      Monthly_Ret <- Monthly_Ret_5
    } else if (x == 3){
      Monthly_Ret <- Monthly_Ret_3
    } else{
      Monthly_Ret <- Monthly_Ret_1
    }
    y = as.numeric(input$index)
    monthlyret = as.data.frame(Monthly_Ret[,y])
    monthlyret$date = as.Date(Monthly_Ret$date, "%Y-%m-%d")
    monthlyret
  })
  
  alpha <- reactive({
    as.numeric(input$return_threshold)
  })
  
  output$dailysummary <- renderText({
    paste("Daily Return Summary:",summary(daily()[1]))
  })
  
  output$weeklysummary <- renderText({
    paste("Weekly Return Summary:", summary(weekly()[1]))
  })
  
  output$monthlysummary <- renderText({
    paste("Monthly Return Summary:", summary(monthly()[1]))
  })
  
  output$DAY <- renderText({
    a = alpha()/100
    probability = round(100 * length(which(daily()[1] >= a))/nrow(daily()), 2)
    paste("Probability of daily return larger than", alpha(), "% is ", probability, "%")
  })
  output$viewDay = renderPlotly({
    a = alpha()/100
    p <- ggplot(daily(), aes(x = daily()[,1], fill = daily()[1] > a)) +
      geom_histogram(binwidth = 0.001) +
      labs(x = "Daily Return", y = "Frequency") +
      ggtitle("The Histogram of Daily Return")
    ggplotly(p) %>%
      layout(showlegend = FALSE)
  })
  output$viewday = renderPlotly({
    a = alpha()/100
    p <- ggplot(daily(), aes(x = daily()[,2], y = daily()[,1], group = 1)) +
      geom_line(col = "blue") +
      geom_hline(yintercept = a, linetype = "dashed") +
      labs(x = "Date", y = "Return") +
      ggtitle("The Line Chart of Daily Return")
    ggplotly(p)
  })
  
  
  output$WEEK <- renderText({
    a = alpha()/100
    probability = round(100 * length(which(weekly()[1] >= a))/nrow(weekly()), 2)
    paste("Probability of weekly return larger than", alpha(), "% is ", probability, "%")
  })
  output$viewWeek = renderPlotly({
    a = alpha()/100
    p <- ggplot(weekly(), aes(x = weekly()[,1], fill = weekly()[1] > a)) +
      geom_histogram(binwidth = 0.001) +
      labs(x = "Weekly Return", y = "Frequency") +
      ggtitle("The Histogram of Weekly Return")
    ggplotly(p) %>%
      layout(showlegend = FALSE)
  })
  output$viewweek = renderPlotly({
    a = alpha()/100
    p <- ggplot(weekly(), aes(x = weekly()[,2], y = weekly()[,1], group = 1)) +
      geom_line(col = "blue") +
      geom_hline(yintercept = a, linetype = "dashed") +
      labs(x = "Date", y = "Return") +
      ggtitle("The Line Chart of Weekly Return")
    ggplotly(p) 
  })
  
  output$MONTH <- renderText({
    a = alpha()/100
    probability = round(100 * length(which(monthly()[1] >= a))/nrow(monthly()), 2)
    paste("Probability of monthly return larger than", alpha(), "% is ", probability, "%")
  })
  output$viewMonth = renderPlotly({
    a = alpha()/100
    p <- ggplot(monthly(), aes(x = monthly()[,1], fill = monthly()[1] > a)) +
      geom_histogram(binwidth = 0.001) +
      labs(x = "Monthly Return", y = "Frequency") +
      ggtitle("The Histogram of Monthly Return")
    ggplotly(p) %>%
      layout(showlegend = FALSE)
  })
  output$viewmonth = renderPlotly({
    a = alpha()/100
    p <- ggplot(monthly(), aes(x = monthly()[,2], y = monthly()[,1], group = 1)) +
      geom_line(col = "blue") +
      geom_hline(yintercept = a, linetype = "dashed") +
      labs(x = "Date", y = "Return") +
      ggtitle("The Line Chart of Monthly Return")
    ggplotly(p)
  })
  
  HAR <- reactive({
    x = as.numeric(input$date)
    if (x == 1){
      HAR <- HAR_10
    } else if (x == 2){
      HAR <- HAR_5
    } else if (x == 3){
      HAR <- HAR_3
    } else{
      HAR <- HAR_1
    }
    return(HAR)
  })
    
  output$HAR = renderPlotly({
    p <- ggplot(HAR(), aes(x = HAR()$Date)) +
      geom_line(aes(y=HAR()$V1,col = "blue")) +
      geom_point(aes(y=HAR()$outlier,col = "red"))+
      labs(x = "Date", y = "Realized Variance") +
      ggtitle("The HAR Model Prediction and Outlier")
    ggplotly(p) %>%
      layout(showlegend=FALSE)
    # plot(HAR()$outlier,ylim=c(0,0.004), xlab="Date", ylab="Realized Meatures",type="p",pch = ".", cex = 3,col = "red")
    # lines(HAR()$V1, col = "blue", cex = 10)
    # legend("topright", legend=c("Outlier", "Predicted"),col=c("red", "blue"), lty=2:1, cex=2)
  })
      
        

}  
shinyApp(ui = ui, server = server)
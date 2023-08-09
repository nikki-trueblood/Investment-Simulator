# ===============================================
# Fill in the following fields
# ===============================================
# Title: Homework 5 Shiny App
# Description: An app that simulates various investment strategies.
# Author: Nikki Trueblood
# Date: 11/7/22


# ===============================================
# Required packages (you can use other packages if you want)
# ===============================================
library(shiny)
library(tidyverse)
library(reshape2)
library(ggthemes)
library(rsconnect)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Investment Simulator"),
  h3("See how changing these sliders changes the graph!"),
  fluidRow(
    # Inputs for initial amount, and periodic contributions 
    column(width = 3,
           h4(" "),
           sliderInput(inputId = "initial", 
                       label = "Initial Amount ($):", 
                       min = 1, 
                       max = 5000, 
                       value = 1000, 
                       step = 10),
           sliderInput(inputId = "periodic_contributions", 
                       label = "Periodic Contributions ($):", 
                       min = 1, 
                       max = 1000, 
                       value = 360, 
                       step = 10)
    ),
    
    # Inputs for target amount, and number of years 
    column(width = 3,
           h4(" "),
           sliderInput(inputId = "target_amount", 
                       label = "Target Amount ($):", 
                       min = 1, 
                       max = 10000, 
                       value = 5000, 
                       step = 50),
           sliderInput(inputId = "years", 
                       label = "Number of Years:", 
                       min = 1, 
                       max = 100, 
                       value = 10, 
                       step = 1)
    ),
    
    # Inputs for mean and standard deviation of annual inflation rates
    column(width = 3,
           h4(" "),
           sliderInput(inputId = "stock", 
                       label = "Stock % (Opposite of Bond %):", 
                       min = 0, 
                       max = 100, 
                       value = 60, 
                       step = 5),
    ),
    
    # Inputs for number of simulations, and random seed
    column(width = 3,
           h4(" "),
           sliderInput(inputId = "sims", 
                       label = "Number of Simulations:", 
                       min = 1, 
                       max = 500, 
                       value = 50, 
                       step = 1),
           sliderInput(inputId = "seed", 
                       label = "Random Seed", 
                       min = 1, 
                       max = 200, 
                       value = 1, 
                       step = 1)
    )
  ),
  
  hr(),
  h4('Each line represents a single simulation!'),  # customize text
  plotOutput('plot1'),

  hr(),
  h4('Each bar represents a year!'),  # customize text
  plotOutput('plot2'),
  
  hr(),
  h4('More info!'),
  tableOutput('table1')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # code for plot1
  output$plot1 <- renderPlot({
    #creates stock-bond table to calculate mean/standard deviation
    stocks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
    stock_avg = c(0.0509,0.054,0.057,0.06,0.0629,0.0657,0.0684,0.0711,0.0737,0.0762,0.0787,0.0811,0.0834,0.0856,0.0878,0.0899,0.0919,0.0937,0.0957,0.0974,0.0991)
    stock_sd = c(0.0403,0.0399,0.0411,0.0437,0.0474,0.0521,0.0574,0.0632,0.0694,0.0759,0.0825,0.0894,0.0964,0.1035,0.1107,0.1180,0.1254,0.1328,0.1403,0.1479,0.1555)
    stock_bond = data.frame(stocks,stock_avg, stock_sd)  
    stock_bond
    avg = filter(stock_bond,stocks==input$stock)$stock_avg
    avg
    
    sd = filter(stock_bond,stocks==input$stock)$stock_sd
    sd
    
    
    #building the main dataframe that includes years, simulations, and the cumulative amount of money
    set.seed(input$seed)
    amounts = matrix(0, nrow = input$years, ncol = input$sims)
    for (sim in 1:input$sims) {
      prev = input$initial
      for (year in 1:input$years) {
        x = prev*(1 + rnorm(1, avg, sd)) + (input$periodic_contributions * year)
        amounts[year,sim] = x
        prev = x
      }
    }
    d = t(amounts)
    rownames(d) = paste("sim", seq(input$sims), sep="")
    colnames(d) = paste("years", seq(input$years), sep="")
    df = as.data.frame(d)
    df$sim = rownames(df)
    dfplot1 = melt(df, id.vars="sim")
    dfplot1$years = as.numeric(gsub("years", "", dfplot1$variable))
    
    #finds the average number of years it takes to reach target
    indices = c()
    for (rr in 1:input$sims) {
      for (cc in 1:input$years) {
        if (d[rr,cc] >= input$target_amount) {
          indices = append(indices, cc)
          break
        } 
      }
    }
    average = mean(indices)
  
    #plots the years vs the accumulated amount of money, with each line representing a single simulation
    #includes a horizontal line representing the target amount and a vertical line representing the average years it takes to reach the target amount
    ggplot(dfplot1, aes(x=years, y=value, group=sim)) + 
      geom_line(size=0.4, alpha=0.4) + 
      geom_hline(aes(yintercept=input$target_amount), color="forest green", size=1.4) + 
      geom_vline(aes(xintercept=average), color="brown", size=1.4) + 
      theme(panel.grid=element_blank()) + 
      theme_economist() + scale_color_economist() + 
      ggtitle("Years vs. Cumulative Amount of Money") + 
      xlab("Time (Years)") + ylab("Cumulative Amount of Money ($)") + 
      annotate("text", min(dfplot1$years), input$target_amount, vjust = -1, label = "Target Amount", size=4, color = "forest green") + 
      annotate("text", x = average, average, label = round(average,2), hjust = 1.3, size=5, color = "brown")
    
  })
  
  
  # code for plot2
  output$plot2 <- renderPlot({
    #same as plot1
    stocks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
    stock_avg = c(0.0509,0.054,0.057,0.06,0.0629,0.0657,0.0684,0.0711,0.0737,0.0762,0.0787,0.0811,0.0834,0.0856,0.0878,0.0899,0.0919,0.0937,0.0957,0.0974,0.0991)
    stock_sd = c(0.0403,0.0399,0.0411,0.0437,0.0474,0.0521,0.0574,0.0632,0.0694,0.0759,0.0825,0.0894,0.0964,0.1035,0.1107,0.1180,0.1254,0.1328,0.1403,0.1479,0.1555)
    stock_bond = data.frame(stocks,stock_avg, stock_sd)  
    stock_bond
    avg = filter(stock_bond,stocks==input$stock)$stock_avg
    avg
    
    sd = filter(stock_bond,stocks==input$stock)$stock_sd
    sd
    
    
    #building the main dataframe that includes years, simulations, and the cumulative amount of money
    set.seed(input$seed)
    amounts = matrix(0, nrow = input$years, ncol = input$sims)
    for (sim in 1:input$sims) {
      prev = input$initial
      for (year in 1:input$years) {
        x = prev*(1 + rnorm(1, avg, sd)) + (input$periodic_contributions * year)
        amounts[year,sim] = x
        prev = x
      }
    }
    d = t(amounts)
    rownames(d) = paste("sim", seq(input$sims), sep="")
    colnames(d) = paste("years", seq(input$years), sep="")
    df = as.data.frame(d)
    df$sim = rownames(df)
    dfplot1 = melt(df, id.vars="sim")
    dfplot1$years = as.numeric(gsub("years", "", dfplot1$variable))
    
    #creates dataframe of proportion of simulations that reached the target amount for each year
    props = matrix(0, nrow = input$years, ncol = 2)
    for (y in 1:input$years) {
      total = 0
      for (x in filter(dfplot1, years == y)$value) {
        if (x >= input$target_amount) {
          total = total + 1
        }
      }
      props[y,1] = y
      props[y, 2] = total / input$sims
    }
    props = as.data.frame(props)
    
    #plots a bar graph with the proportion of siulations that reached the target amount vs the year
    ggplot(data = props, aes(x=V1,y=V2)) +
      geom_col(color="black", fill="white") + 
      theme(panel.grid=element_blank()) + 
      theme_economist() + scale_color_economist() + 
      ggtitle("Years vs. Probability of Reaching Target Amount") + 
      xlab("Time (Years)") + ylab("Probability of Reaching Target Amount")
  })
  
    
  # code for additional information
  output$table1 <- renderTable({
    #same as plot1
    stocks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100)
    stock_avg = c(0.0509,0.054,0.057,0.06,0.0629,0.0657,0.0684,0.0711,0.0737,0.0762,0.0787,0.0811,0.0834,0.0856,0.0878,0.0899,0.0919,0.0937,0.0957,0.0974,0.0991)
    stock_sd = c(0.0403,0.0399,0.0411,0.0437,0.0474,0.0521,0.0574,0.0632,0.0694,0.0759,0.0825,0.0894,0.0964,0.1035,0.1107,0.1180,0.1254,0.1328,0.1403,0.1479,0.1555)
    stock_bond = data.frame(stocks,stock_avg, stock_sd)  
    stock_bond
    avg = filter(stock_bond,stocks==input$stock)$stock_avg
    avg
    
    sd = filter(stock_bond,stocks==input$stock)$stock_sd
    sd
    
    
    #building the main dataframe that includes years, simulations, and the cumulative amount of money
    set.seed(input$seed)
    amounts = matrix(0, nrow = input$years, ncol = input$sims)
    for (sim in 1:input$sims) {
      prev = input$initial
      for (year in 1:input$years) {
        x = prev*(1 + rnorm(1, avg, sd)) + (input$periodic_contributions * year)
        amounts[year,sim] = x
        prev = x
      }
    }
    d = t(amounts)
    rownames(d) = paste("sim", seq(input$sims), sep="")
    colnames(d) = paste("years", seq(input$years), sep="")
    df = as.data.frame(d)
    df$sim = rownames(df)
    dfplot1 = melt(df, id.vars="sim")
    dfplot1$years = as.numeric(gsub("years", "", dfplot1$variable))
    
    #same as plot2
    props = matrix(0, nrow = input$years, ncol = 2)
    for (y in 1:input$years) {
      total = 0
      for (x in filter(dfplot1, years == y)$value) {
        if (x >= input$target_amount) {
          total = total + 1
        }
      }
      props[y,1] = y
      props[y, 2] = total / input$sims
    }
    props = as.data.frame(props)
    
    #creates vectors for years, average amount of money accumulated, and probability of reaching target amount
    Probability_of_Reaching_Target = props[,2]
    Years = c(1:input$years)
    Average_Amount_Accumulated = c()
    for (year in 1:input$years) {
      Average_Amount_Accumulated = append(Average_Amount_Accumulated, mean(df[,year]))
    }
    
    #displays these stats in a table
    output_table = data.frame(Years, Average_Amount_Accumulated, Probability_of_Reaching_Target)
    output_table
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

# Investment-Simulator
Investment Simulator App for Statistics Class Project (Fall 2022)

See the project in action here: https://nikkitrueblood.shinyapps.io/investment-app-nikki-trueblood/?_ga=2.114022599.1577198851.1668127128-461480328.1668127128

## Description

This project creates an app using R Shiny that allows users to simulate and visualize various investment options by toggling metrics such as initial amount, periodic contributions, stock percent, and number of years. I used tidyverse and ggplot to manipulate user input and create simple but eye-catching visuals.

The code consists of two parts: the user interface and the server logic.

## User Interface

Creates the visual appearance of the app through the following steps:
- Build the title panel and subheader to instruct users to move the sliders.
- Distribute seven slider inputs across four columns to allow users to view all of them at the same time and toggle initial amount, periodic contributions, target amount, number of years, stock %, number of simulations, and seed. Set slider minimums, maximums, steps, and default values to reasonable numbers based on each metric.
- Add two visualizations and one table to the interface along with headers describing what they are to give users various ways to examine the data.

## Server Logic

Receives real-time user input from the sliders and generates two visualizations and one table through the following steps:

- Creates vectors of historical data related to stock %'s and their corresponding averages and standard deviations  (pulled from internet source) and adds them to a dataframe.
- Sets stock average to the historical stock average corresponding with the slider input stock percentage.
- Does the same for stock standard deviation.
- Sets the seed to the slider input seed.
- Conducts the slider input number of simulations by inputting the slider input intial amount, stock average, stock standard deviation, slider input periodic contributions, and slider input number of years into an equation.
- Plot 1:
  - Based on the above simulations, finds the average number of years it takes to reach the slider input target amount.
  - Plots the years vs the accumulated amount of money on a line graph with each line representing a single simulation. Includes a horizontal line representing the slider input target amount and a vertical line representing the average number of years it takes to reach that target amount.
- Plot 2:
  - Based on the above simulations, creates a dataframe with the proportion of simulations that reach or exceed the target amount for each year.
  - Plots the years vs the proportion of simulations that reach/exceed the target amount on a bar graph.
- Table:
  - Based on the above simulations, creates vectors of data for years, average amount of money accumulated, and probability of reaching the target amount and consolidates them into a table.


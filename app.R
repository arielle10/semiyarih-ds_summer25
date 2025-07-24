
#loads required libraries
library(shiny)
library(dplyr)
library(ggplot2)

#defines the UI (user interface)

ui <- fluidPage(
  
#this section will load the title panel and will add input controls for the user
titlePanel("MTCARS"),
  sidebarLayout(sidebarPanel
      
#checkboxGroupInput will allow the user to choose which variables to display in the table

(checkboxGroupInput(inputId = "vars", label = "Select variables to display in table:",
    choices = names(mtcars), selected = c("mpg", "cyl", "hp")),
      
#the selectInput sections allow the user to choose the y-axis and x-axis of the boxplot,
#x-axis of the bar plot, and x-axis of the histogram
      
hr(),
  selectInput(inputId = "boxplot_y", label = "Boxplot: Choose a continuous variable (Y-axis)",
  choices = c("mpg", "hp", "wt", "qsec"), selected = "mpg"),
      
    selectInput( inputId = "boxplot_x", label = "Boxplot: Choose a discrete grouping variable (X-axis)",
    choices = c("cyl", "gear", "carb", "am", "vs"), selected = "cyl"),
      
hr(),
  selectInput( inputId = "bar_var", label = "Bar Chart: Choose a discrete variable",
   choices = c("cyl", "gear", "carb", "am", "vs"), selected = "gear"),
      
      
hr(),
  selectInput( inputId = "hist_var", label = "Histogram: Choose a continuous variable", 
   choices = c("mpg", "hp", "wt", "qsec", "disp", "drat"), selected = "hp")),

#this section will create a tabbed layout where each tab (tabPanel) will display the different 
#outputs like the data table, summary table, boxplot etc. 

mainPanel(
    tabsetPanel(
      tabPanel("Data", tableOutput("data_table")),
        
      tabPanel("Summary", h4("Summary of Discrete Variables"), 
                verbatimTextOutput("summary_discrete"),
                h4("Summary of Continuous Variables"),
                verbatimTextOutput("summary_continuous")),
        
      tabPanel("BoxPlot", plotOutput("boxplot")),
        
      tabPanel("Bar", plotOutput("barplot")),
        
      tabPanel("Histogram", plotOutput("histogram"))
      )
     )
    )
  )

#this defines how the app will respond to the user's input. It will process selected variables,
#generate plots, calculate summaries etc. 

server <- function(input, output) {

#this will define the variables in mtcars that are discrete vs continuous
  
discrete_vars <- c("cyl", "gear", "carb", "vs", "am")
continuous_vars <- setdiff(names(mtcars), discrete_vars)
  
#this will generate the data table for the data tab showing the variables the user selected
#req(input$vars requires that at least one variable is selected)

output$data_table <- renderTable({req(input$vars)
  mtcars[, input$vars, drop = FALSE]})
  
#the output$summary_discrete and output$summary_continuous will display the stats for discrete
#variables and continuous variables

output$summary_discrete <- renderPrint({
  summary(select(mtcars, all_of(discrete_vars)))})
  
output$summary_continuous <- renderPrint({
  summary(select(mtcars, all_of(continuous_vars)))})
  
#this will create the boxplot for the selected continuous variable (y-axis) and grouped by the 
#selected discrete variable (x-axis)

output$boxplot <- renderPlot({
  ggplot(mtcars, aes_string(x = input$boxplot_x, y = input$boxplot_y)) +
    geom_boxplot(fill = "plum") +
    labs( title = paste("Boxplot of", input$boxplot_y, "by", input$boxplot_x),
      x = input$boxplot_x,
      y = input$boxplot_y) +
    theme_minimal()})
  
#this will create the bar plot of the selected discrete variables

output$barplot <- renderPlot({
  ggplot(mtcars, aes_string(x = input$bar_var)) +
    geom_bar(fill = "cyan3") +
    labs(title = paste("Bar Chart of", input$bar_var),
      x = input$bar_var,
      y = "Count") +
    theme_minimal()})
  
#this will create the histogram of the selected continuous variables

output$histogram <- renderPlot({
  ggplot(mtcars, aes_string(x = input$hist_var)) +
    geom_histogram(fill = "orange", color = "black", bins = 10) +
    labs(title = paste("Histogram of", input$hist_var),
      x = input$hist_var,
      y = "Frequency") +
    theme_minimal()})
}

#this final line runs the Shiny app, using the ui and server previously defined
shinyApp(ui = ui, server = server)





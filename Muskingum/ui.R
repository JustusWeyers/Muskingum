#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  
  ### Title "Muskingum Application"
  
  shiny::fluidRow(
    shiny::titlePanel("Muskingum Application")
  ),
  
  
  ### Data input
  
  h2("Data input"),
  
  shiny::fluidRow(
    col_6(
      shinydashboard::box(
        title = "Q", width = 12,
        rhandsontable::rHandsontableOutput("ui_Q")
      )
    ),
    col_6(
      shinydashboard::box(
        title = "Zw", width = 12,
        rhandsontable::rHandsontableOutput("ui_Zw")
      )
    ),
    
    col_6(
      shinydashboard::box(
        title = "Select parameter X", width = 12,
        shiny::sliderInput(
          "x", 
          label = NULL, 
          value = 0.30,
          min = 0.05, 
          max = 0.95, 
          step = 0.05
        )
      )
    ),
      
    col_6(
      shinydashboard::box(
        title = "Actions", width = 12,
        shiny::actionButton("clear_input", "Clear input"),
        shiny::actionButton("demo_data", "Use demo data"),
        shiny::actionButton("run", "Run", class = "btn-warning")
      )
    )
      
  ),
  
  ### Calculations
  
  shiny::uiOutput("ui_calculations"),
  
  ### Plots
  
  shiny::uiOutput("ui_plots")
  
)

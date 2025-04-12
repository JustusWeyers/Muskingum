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
        title = "Calibration in- and outflow event", width = 12,
        p("Table 1: Fill-in table for timestamp t [h], in- and outflow Q_in and Q_out [both in m^3/s]."),
        rhandsontable::rHandsontableOutput("ui_Q")
      )
    ),
    col_6(
      shinydashboard::box(
        title = "Flood event", width = 12,
        p("Table 2: Flood event at inflow measuring point."),
        rhandsontable::rHandsontableOutput("ui_flood")
      )
    ),
  ),
  shiny::fluidRow(
    col_6(
      shinydashboard::box(
        title = "Select additional X", width = 12,
        shiny::sliderInput(
          "X", 
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
  
  ### Hysteresis plots
  
  shiny::uiOutput("ui_plots"),
  
  ### Floodrouting
  
  shiny::uiOutput("ui_floodrouting"),
  
  
)

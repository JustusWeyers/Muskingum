#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinydashboard)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  ### Values
  
  input_vals = shiny::reactiveValues(
    Zw = demo_Zw, # empty_df(r = 12, c = 2, cn = c("t", "Q_Zw")),
    Q = demo_Q # empty_df(r = 12, c = 3, cn = c("t", "Q_Z", "Q_A"))
  )
  
  vals = shiny::reactiveValues()
  
  ### Serverlogic
  
  shiny::observe({
    if (!is.null(input$ui_Q)) {
      input_vals[["Q"]] = rhandsontable::hot_to_r(input$ui_Q)
    }
  })
  
  shiny::observe({
    if (!is.null(input$ui_Zw)) {
      input_vals[["Zw"]] = rhandsontable::hot_to_r(input$ui_Zw)
    }
  })
  
  shiny::observeEvent(input$demo_data, {
    input_vals$Zw = demo_Zw
    input_vals$Q = demo_Q
  })

  shiny::observeEvent(input$clear_input, {
    input_vals$Zw = empty_df(r = 12, c = 2, cn = c("t", "Q_Zw"))
    input_vals$Q = empty_df(r = 12, c = 3, cn = c("t", "Q_Z", "Q_A"))
    vals$Q = NULL
    vals$Zw = NULL
  })
  
  shiny::observeEvent(input$run, {
    if (table_OK(input_vals$Zw) & table_OK(input_vals$Q)) {
      input_vals$Zw = table_prep(input_vals$Zw)
      input_vals$Q = table_prep(input_vals$Q)
      vals$Zw = input_vals$Zw
      vals$Q = input_vals$Q
    }
  })
  
  baseflow = shiny::reactive(
    min(c(vals$Q$Q_Z, vals$Q$Q_A))
  )
  
  dt = shiny::reactive(
    unique(diff(vals$Q$t))
  )
  
  # Numerators 
  numerators = shiny::reactive({
    if (!is.null(vals$Q)) {
      print("# Numerators")
      n = 0
      for (i in 1:(nrow(vals$Q))-1) {
        n = c(n, numerator(t = i, dt = dt(), Q_Z = vals$Q$Q_Z, Q_A = vals$Q$Q_A))
      }
      return(n)
    }
  })
  
  numdf = shiny::reactive({
    if (!is.null(vals$Q)) {
      data.frame(
        t = vals$Q$t,
        Q_Z = vals$Q$Q_Z,
        Q_A = vals$Q$Q_A,
        num = numerators(),
        csum_num = cumsum(numerators())
      )
    }
  })
  
  X = shiny::reactive(
    c(0.15, 0.2, 0.25, input$x)
  )
  
  # Denominators 
  denominators = shiny::reactive({
    if (!is.null(vals$Q)) {
      
      print("# Denominators")
      
      dns = function(x, Q) {
        
        d = 0
        
        for (i in 1:(nrow(Q)-1)) {
          d = c(d, denominator(t = i, x = x, Q_Z = Q$Q_Z, Q_A = Q$Q_A))
        }
        
        df = data.frame(
          dnom = d,
          csum_dnom = cumsum(d)
        )

      }

      return(lapply(X = X(), FUN = dns, vals$Q))
    }
  })
  
  ### Output
  
  output$ui_Q <- rhandsontable::renderRHandsontable({
    if (!is.null(input_vals[["Q"]]))
      rhandsontable::rhandsontable(input_vals[["Q"]], useTypes = TRUE, stretchH = "all")
  })
  
  output$ui_Zw <- rhandsontable::renderRHandsontable({
    if (!is.null(input_vals[["Q"]]))
      rhandsontable::rhandsontable(input_vals[["Zw"]], useTypes = TRUE, stretchH = "all")
  })
  
  output$ui_numdf = shiny::renderTable(
    if (!is.null(numdf())) {
      numdf()
    }
  )
  
  output$ui_calculations = shiny::renderUI(
    if (!is.null(vals$Q)) {
      shiny::tagList(
        h2("Calculations"),
        
        shiny::fluidRow(
          col_12(
            shinydashboard::box(
              solidHeader = TRUE, width = 12,
              
              shiny::fluidRow(
                col_4(
                  shinydashboard::box(
                    title = "Numertors / Denominators", width = "100%",
                    shiny::tableOutput("ui_numdf"),
                  )
                ),
                col_8(
                  shiny::uiOutput("ui_dnom_array")
                )
              )
            )
          )
        )
      )
    }
  )
  
  output$ui_dnom = shiny::renderTable(
    if (!is.null(denominators())) {
      denominators()[[1]]
    }
  )
  
  output$ui_dnom_array = shiny::renderUI(
    if (!is.null(vals$Q)) {
      dnom_box = function(i) {
        col_3(
          shinydashboard::box(
            title = paste("X = ", X()[i]), width = "100%",
            shiny::renderTable(denominators()[i])
          )
        )
      }
      return(lapply(1:length(X()), FUN = dnom_box))
    }
  )
  
  output$ui_plots = shiny::renderUI(
    if (!is.null(vals$Q)) {
      shiny::tagList(
        h2("Plots"),
        lapply(1:length(X()), FUN = plot_array, numdf(), denominators(), X())
      )
    }
  )
  
}

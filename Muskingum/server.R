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
    flood = demo_flood, # empty_df(r = 12, c = 2, cn = c("t", "Q_flood")),
    Q = demo_Q # empty_df(r = 12, c = 3, cn = c("t", "Q_in", "Q_out"))
  )
  
  vals = shiny::reactiveValues()
  
  ### Serverlogic
  
  shiny::observe({
    if (!is.null(input$ui_Q)) {
      input_vals[["Q"]] = rhandsontable::hot_to_r(input$ui_Q)
    }
  })
  
  shiny::observe({
    if (!is.null(input$ui_flood)) {
      input_vals[["flood"]] = rhandsontable::hot_to_r(input$ui_flood)
    }
  })
  
  shiny::observeEvent(input$demo_data, {
    input_vals$flood = demo_flood
    input_vals$Q = demo_Q
  })

  shiny::observeEvent(input$clear_input, {
    input_vals$flood = empty_df(r = 12, c = 2, cn = c("t", "Q_flood"))
    input_vals$Q = empty_df(r = 12, c = 3, cn = c("t", "Q_in", "Q_out"))
    vals$Q = NULL
    vals$flood = NULL
  })
  
  shiny::observeEvent(input$run, {
    if (table_OK(input_vals$flood) & table_OK(input_vals$Q)) {
      vals$flood = input_vals$flood
      vals$Q = input_vals$Q
    }
  })
  
  baseflow = shiny::reactive(
    min(c(vals$Q$Q_in, vals$Q$Q_out))
  )
  
  baseflow_flood = shiny::reactive({
    min(vals$flood$Q_flood)
  })
  
  dt = shiny::reactive(
    # Delta t in seconds
    unique(diff(vals$Q$t)) * 3600
  )
  
  # Numerators 
  numerators = shiny::reactive({
    if (!is.null(vals$Q)) {
      n = c()
      for (i in 1:(nrow(vals$Q))-1) {
        n = c(n, numerator(t = i, dt = dt(), Q_in = vals$Q$Q_in, Q_out = vals$Q$Q_out))
      }
      return(n)
    }
  })
  
  numdf = shiny::reactive({
    if (!is.null(vals$Q)) {
      data.frame(
        num = numerators(),
        csum_num = cumsum(numerators())
      )
    }
  })
  
  X = shiny::reactive(
    c(0.15, 0.2, 0.25, input$X)
  )
  
  # Denominators 
  denominators = shiny::reactive({
    if (!is.null(vals$Q)) {
      dns = function(X, Q) {
        d = c()
        for (i in 1:(nrow(Q)-1)) {
          d = c(d, denominator(t = i, X = X, Q_in = Q$Q_in, Q_out = Q$Q_out))
        }
        df = data.frame(
          dnom = d,
          csum_dnom = cumsum(d)
        )
      }
      return(lapply(X = X(), FUN = dns, vals$Q))
    }
  })
  
  regression = shiny::reactive({
    lapply(denominators(), FUN = linReg, num = numdf())
  })
  
  K = shiny::reactive({
    sapply(regression(), FUN = function(v) getElement(v, "K"))
  })
  
  r_squared = shiny::reactive(
    sapply(regression(), FUN = function(v) getElement(v, "r_squared"))
  )
  
  b = shiny::reactive(
    sapply(regression(), FUN = function(v) getElement(v, "b"))
  )
  
  check = shiny::reactive({
    sapply(1:length(X()), FUN = function(i) {
      XK_OK(X = X()[i], K = K()[i], dt = dt())
    })
  })
  
  colors = shiny::reactive({
    lapply(check(), FUN = function(c) {
      if (c) {
        return(c("#000000", "#0000FF"))
      } else {
        return(c("#B0B0B0", "#6495ED"))
      }
    })
  })
  
  chooX = shiny::reactive(
    as.numeric(input$chooX)
  )
  
  c0 = shiny::reactive({
    c_0(X = X()[chooX()], K = K()[chooX()], dt = dt())
  })
  
  c1 = shiny::reactive(
    c_1(X = X()[chooX()], K = K()[chooX()], dt = dt())
  )
  
  c2 = shiny::reactive(
    c_2(X = X()[chooX()], K = K()[chooX()], dt = dt())
  )
  
  sum_c = shiny::reactive(
    c0() + c1() + c2()
  )
  
  flood_table = shiny::reactive({
    df = data.frame(
      t = vals$flood$t,
      Q_in = vals$flood$Q_flood
    )
    df$Q_out = routing(df, c0(), c1(), c2(), baseflow = baseflow_flood())
    return(df)
  })
  
  ### Output
  
  output$ui_Q <- rhandsontable::renderRHandsontable({
    if (!is.null(input_vals[["Q"]]))
      rhandsontable::rhandsontable(input_vals[["Q"]], useTypes = TRUE, stretchH = "all")
  })
  
  output$ui_flood <- rhandsontable::renderRHandsontable({
    if (!is.null(input_vals[["Q"]]))
      rhandsontable::rhandsontable(input_vals[["flood"]], useTypes = TRUE, stretchH = "all")
  })
  
  output$ui_numdf = shiny::renderTable(
    if (!is.null(numdf())) {
      data.frame(
        t = vals$Q$t,
        Q_in = vals$Q$Q_in,
        Q_out = vals$Q$Q_out,
        num = c(0, numdf()$num),
        csum_num = c(0, numdf()$csum_num)
      )
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
        h2("Hysteresis plots"),
        lapply(
          X = 1:length(X()), 
          FUN = function(i) {
            plot_array(
              plt = hyst_plot(
                numdf(), 
                denominators()[[i]], 
                X()[i], 
                K = K()[i],
                b = b()[i],
                colors()[[i]]
              ), 
              X = X()[i], 
              K = K()[i], 
              b = b()[i], 
              dt = dt(), 
              r_squared = r_squared()[i],
              colors = colors()[[i]]
            )
          }
        )
      )
    }
  )
  
  output$ui_c_parameters = shiny::renderUI(
    HTML(
      paste(
        paste("c_0 =", round(c0(), 2)),
        paste("c_1 =", round(c1(), 2)),
        paste("c_2 =", round(c2(), 2)),
        "",
        paste("c_1 + c_2 + c_3 =", round(sum_c(), 2)),
        sep = "<br/>"
      )
    )
  )

  output$ui_flood_plot = shiny::renderPlot({
    p = flood_plot(flood_table())
    return(p)
  })
  
  output$ui_flood_table = shiny::renderTable({
    flood_table()
  })
  
  output$ui_floodrouting = shiny::renderUI(
    if (!is.null(vals$Q)) {
      shiny::tagList(
        h2("Floodrouting"),
        col_3(
          shinydashboard::box(
            title = "", width = 12,
            radioButtons(
              "chooX",
              label = "Choose X:",
              choiceNames = paste("X =", X()),
              choiceValues = 1:length(X())
            ),
            strong("c parameters:"),
            shiny::uiOutput(
              "ui_c_parameters"
            )
          )
        ),
        col_3(
          shiny::tableOutput("ui_flood_table")
        ),
        col_6(
          shiny::plotOutput("ui_flood_plot")
        )
      )
    }
  )
    
  
}

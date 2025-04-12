
S = function(K, X, Q_out, Q_in) {
  K * Q_out + K * X * (Q_in - Q_out)
}

numerator = function(t, dt, Q_in, Q_out) {
  dt * 0.5 * ((Q_in[t+1] + Q_in[t]) - (Q_out[t+1] + Q_out[t]))
}

denominator = function(t, X, Q_in, Q_out) {
  X * (Q_in[t+1] - Q_in[t]) + (1-X) * (Q_out[t+1] - Q_out[t])
}

XK_OK = function(X, K, dt) {
  K >= dt & X < dt/(2*K) & 0 < X & X < 0.5
}

c_0 = function(X, K, dt) {
  n = -K * X + 0.5 * dt
  d = K * (1 - X) + 0.5 * dt
  return(n/d)
}

c_1 = function(X, K, dt) {
  n = K * X + 0.5 * dt
  d = K * (1 - X) + 0.5 * dt
  return(n/d)
}

c_2 = function(X, K, dt) {
  n = K - K * X - 0.5 * dt
  d = K * (1 - X) + 0.5 * dt
  return(n/d)
}

routing = function(df, c0, c1, c2, baseflow) {
  print("# Routing")
  print(baseflow)
  Q_out = c(baseflow)
  for (t in df$t[1:length(df$t)] ) {
    q = c0 * df$Q_in[t+1] + c1 *df$Q_in[t] + c2 * Q_out[t]
    Q_out = append(Q_out, q)
  }
  print(Q_out)
  
  return(Q_out)
}

XK_diag = function(X, K, dt) {
  t = ""
  if (K >= dt & X < dt/(2*K) & 0 < X & X < 0.5) {
    return(t)
  }
  if (!(K >= dt)) {
    t = paste(t, "K is less than dt")
  }
  if (!(X < dt/(2*K))) {
    t = paste0(t, "X not less than dt/(2*K);")
  }
  if (!(0 < X)) {
    t = paste(t, "X is not greater than 0;")
  }
  if (!(X < 0.5)) {
    t = paste(t, "X is not less than 0.5;")
  }
  return(t)
}

empty_df = function(r, c, cn) {
  df = data.frame(matrix(NA_real_, nrow = r, ncol = c))
  colnames(df) = cn
  return(df)
}

table_OK = function(df) {
  tryCatch({
    # Checks
    num = all(sapply(X = df, FUN = is.numeric))
    dup = !(any(duplicated(df$t)))
    dt = var(diff(df$t[order(df$t)]))
    
    if (is.na(dt)) {
      break
    } else {
      dt = dt == 0
    }
    
    # Messages
    if (!num) {
      shiny::showNotification("Not numeric", type = "error")
    }
    if (!dup) {
      shiny::showNotification("Duplicates in column t", type = "error")
    }
    if (!dt) {
      shiny::showNotification("Non-equidistant t", type = "error")
    }
    # All right?
    return(num & dup & dt)
  }, error = function(e) {
    shiny::showNotification("Invalid input", type = "error")
    return(FALSE)
  })
}

linReg = function(dnom, num) {
  c(
    b = unname(coef(lm(num$csum_num ~ dnom$csum_dnom))[1]),
    K = unname(coef(lm(num$csum_num ~ dnom$csum_dnom))[2]),
    r_squared = round(cor(num$csum_num, dnom$csum_dnom)^2, 4)
  )
}

style = function(color) {
  paste0("color:", color, ";")
}

hyst_plot = function(num, dnom, X, K, b, colors) {
  p = {
    base::plot(
      x = dnom$csum_dnom,
      y = num$csum_num, 
      xlab = "Sum Denominators",
      ylab = "Sum Numerators",
      main = paste("X =", X),
      type = "l",
      col = colors[1],
      col.main = colors[1],
      col.lab = colors[1],
      axes = FALSE
    )
    axis(1, col.ticks = colors[1], col.axis = colors[1])
    axis(2, col.ticks = colors[1], col.axis = colors[1])
    graphics::abline(a = b, b = K, col = colors[2])
    graphics::box(col = colors[1])
  }
  return(p)
}

plot_array = function(plt, X, K, b, dt, r_squared, colors) {
  col_3(
    shiny::renderPlot(plt),
    shinydashboard::box(
      solidHeader = TRUE, width = 12,
      strong("Regression results", style = style(colors[1])),
      p(paste("X =", X), style = style(colors[1])),
      p(paste("K =", round(K, 2)), style = style(colors[1])),
      p(paste("b =", round(b, 2)), style = style(colors[1])),
      p(paste("dt =", dt), style = style(colors[1])),
      p(paste("r^2 =", round(r_squared, 4)), style = style(colors[1])),
      p(paste0("dt/(2*K) = ", round(dt/(2*K), 4), ";"), style = style(colors[1])),
      p(XK_diag(X = X, K = K, dt = dt), style = "color:red;")
    )
  )
}

flood_plot = function(df) {
  ylim = c(0, max(c(df$Q_in, df$Q_out)))
  p = {
    plot(
      x = df$t, 
      y = df$Q_in, 
      type = "l", 
      ylim = ylim,
      xlab = "time [h]",
      ylab = "Q [m^3/s]",
      main = "Hydrograph",
      col = "blue"
    )
    lines(df$t, df$Q_out, type = "l", col = "darkred")
    abline(0, 0)
  }
  return(p)
}

col_2 = function(...) {
  shiny::column(2, ...)
}

col_3 = function(...) {
  shiny::column(3, ...)
}

col_4 = function(...) {
  shiny::column(4, ...)
}

col_6 = function(...) {
  shiny::column(6, ...)
}

col_8 = function(...) {
  shiny::column(8, ...)
}

col_10 = function(...) {
  shiny::column(10, ...)
}

col_12 = function(...) {
  shiny::column(12, ...)
}
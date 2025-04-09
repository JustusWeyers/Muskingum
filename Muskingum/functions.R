
S = function(K, x, Q_A, Q_Z) {
  K * Q_A + K * x * (Q_Z - Q_A)
}

numerator = function(t, dt, Q_Z, Q_A) {
  dt * 0.5 * ((Q_Z[t+1] + Q_Z[t]) - (Q_A[t+1] + Q_A[t]))
}

denominator = function(t, x, Q_Z, Q_A) {
  x * (Q_Z[t+1] - Q_Z[t]) + (1-x) * (Q_A[t+1] - Q_A[t])
}

xK_OK = function(x, K, dt) {
  K >= dt & x < dt/(2*K) & 0 < x & x < 0.5
}

xK_diag = function(x, K, dt) {
  if (K >= dt & x < dt/(2*K) & 0 < x & x < 0.5) {
    return("Alright")
  } else if (!(K >= dt)) {
    return("K is less than dt")
  } else if (!(x < dt/(2*K))) {
    return("x not less than dt/(2*K)")
  } else if (!(0 < x)) {
    return("x is not greater than 0")
  } else if (!(x < 0.5)) {
    return("x is not less than 0.5")
  }
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
    
    print(var(diff(df$t[order(df$t)])))
    
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

hyst_plot = function(num, dnom, x) {
  linReg = coef(lm(num$csum_num ~ dnom$csum_dnom))
  r_squared = round(cor(num$csum_num, dnom$csum_dnom)^2, 4)
  
  p = {
    plot(x = dnom$csum_dnom,
       y = num$csum_num, 
       xlab = "Sum Denominators",
       ylab = "Sum Numerators",
       main = paste("X =", x, "\n R^2 =", r_squared),
       type = "l"
    )
    abline(
      linReg[1], linReg[2], col = "blue"
    )
  }
  
  return(p)
  
}

plot_array = function(i, num, dnom, x) {
  col_3(
    shiny::renderPlot(hyst_plot(num, dnom[[i]], x = x[i])) 
  )
}

table_prep = function(df) {
  df = df[order(df$t),]
  rownames(df) <- NULL
  return(df)
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
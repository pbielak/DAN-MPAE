# Util functions for estimation
library(plotrix)

run_point_estimation <- function(data, attribute_name) {
  probe_x <- data[, attribute_name]
  n <- length(probe_x)
  
  mean_probe <- mean(probe_x)
  var_probe <- var(probe_x)
  sd_probe <- sqrt(var_probe)
  
  title <- paste('Rozkład wartości atrybutu:', attribute_name, '\n',
                 'Średnia próbkowa:', round(mean_probe, 2), '\n',
                 'Wariancja próbkowa:', round(var_probe, 2))
  
  x11()
  hist(probe_x, 
       xlab = attribute_name, ylab = NULL,
       main = title,
       col = c('lightgrey'),
       probability = TRUE,
       breaks = 'Freedman-Diaconis')
  
  curve(dnorm(x, mean = mean_probe, sd = sqrt(var_probe)), 
        add = TRUE, col = 'red', lwd = 2)
  
  return(c(mean_probe, var_probe))
}

run_interval_estimation <- function(data, attribute_name, alpha) {
  x = data[, attribute_name]
  n = length(x)
  
  if (n < 30) {
    mean_int = t_Student_interval_estimation(x, alpha)
  }
  else {
   mean_int = normal_interval_estimation(x, alpha) 
  }
  
  var_int = chisq_interval_estimation(x, alpha)
  
  return(c(mean_int, var_int))
}

t_Student_interval_estimation <- function(x, alpha) {
  n = length(x)
  mean_x = mean(x)
  var_x = var(x)
  
  common_part = qt(1 - alpha / 2, df = n - 1) * var_x / sqrt(n)
  L = mean_x - common_part
  R = mean_x + common_part
  
  return(c(L, R))
}

normal_interval_estimation <- function(x, alpha) {
  n = length(x)
  mean_x = mean(x)
  var_x = var(x)
  
  common_part = qnorm(1 - alpha / 2) * var_x / sqrt(n)
  L = mean_x - common_part
  R = mean_x + common_part
  
  return(c(L, R))
}

chisq_interval_estimation <- function(x, alpha) {
  n = length(x)
  mean_x = mean(x)
  var_x = var(x) * n / (n - 1)
  
  common_part = (n - 1) * var_x
  
  L = common_part / qchisq(1 - alpha / 2, df = n - 1)
  R = common_part / qchisq(alpha / 2, df = n - 1)
    
  return(c(L, R))
}

make_interval_plot <- function(alphas, data, attribute_name) {
  x <- data[, attribute_name]
  mean_x <- mean(x)
  var_x <- var(x)
  
  mean_lls <- c()
  mean_uls <- c()
  mean_y <- c()
  
  var_lls <- c()
  var_uls <- c()
  var_y <- c()

  for(alpha in alphas) {
    ie_res <- run_interval_estimation(data, attribute_name, alpha)
    mean_lls <- append(mean_lls, ie_res[1])
    mean_uls <- append(mean_uls, ie_res[2])
    mean_y <- append(mean_y, mean_x)
    
    var_lls <- append(var_lls, ie_res[3])
    var_uls <- append(var_uls, ie_res[4])
    var_y <- append(var_y, var_x)
  }
  
  conf_levels <- 1 - alphas
  
  x11()
  ylim <- c(min(mean_lls, var_lls), max(mean_uls, var_uls))
  title <- paste('Przedziały ufności dla średniej / wariancji\n',
                 'Poziomy ufności = [', paste(rev(1 - alphas), collapse = ', '), ']')
  
  plot(x = conf_levels, y = mean_y, 
       type = 'l', lwd = 2, col = 'red',
       ylim = ylim,
       main = title,
       xlab='Poziom ufności', ylab='Średnia')
  plotCI(x = conf_levels, y = mean_y, 
         li = mean_lls, ui = mean_uls, 
         add = TRUE, col = 'red')
  
  lines(x = conf_levels, y = var_y, 
        lwd = 2, col = 'blue',
        ylim = ylim)
  plotCI(x = conf_levels, y = var_y, 
         li = var_lls, ui = var_uls, 
         add = TRUE, col = 'blue')
  
  legend('top', legend = c('Średnia', 'Wariancja'), 
         col = c('red', 'blue'), lty=1:2, horiz = TRUE)
}

run_hypothesis_verification <- function(data, attribute_name, groups, type, alpha) {
  x_m = data[data$Sex == groups[1], attribute_name]
  x_f = data[data$Sex == groups[2], attribute_name]
  
  test_result <- t.test(x = x_m, y = x_f, alternative = type, conf.level = 1 - alpha)
  p_value <- round(test_result$p.value, 6)
  
  if (p_value < alpha) {
    return(c(p_value, "H0 rejected"))
  }
  
  return(c(p_value, "H0 accepted"))
  
}
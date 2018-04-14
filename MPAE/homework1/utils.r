# Util functions for graphs

make_barplot_with_labels <- function(table_data, title_str, ylab, xlab, x_names) {
  x11()
  bp <- barplot(table_data,
                col = c("purple", "grey", "blue"),
                ylab = ylab,
                xlab = xlab,
                ylim = c(0, 1.1 * max(table_data)),
                names.arg = x_names)
  text(x = bp,
       y = table_data,
       labels = table_data,
       pos = 3, cex = 0.8)
  title(title_str)
}


make_histograms <- function(data) {
  x11()

  layout(mat = matrix(c(1,2,3,4, 5,6,7,8, 9,9,9,9), nrow = 3, ncol = 4, byrow = TRUE),
         heights = c(0.45, 0.45, 0.1))

  for(attr in names(data)) {

    if(attr == "Sex") {
      next
    }

    par(mar = c(2, 2, 1, 1))
    curr_x <- data[, attr]

    hist(curr_x,
         xlab = attr, ylab = NULL,
         main = paste("Rozkład atrybutu:", attr),
         col = c("lightgrey"),
         probability = TRUE,
         breaks = "Freedman-Diaconis")

    curve(dnorm(x, mean=mean(curr_x), sd=sd(curr_x)),
          col = "red", lwd = 2, add = TRUE)

    lines(density(curr_x), col = "green", lwd = 2)
    abline(v = mean(curr_x), col = "blue", lwd = 2)
    abline(v = median(curr_x), col = "pink", lwd = 2)
  }

  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  legend(x = "top",
         inset = 0,
         legend = c("Rozkład normalny", "Estymator jądrowy", "Średnia", "Mediana"),
         col = c("red", "green", "blue", "pink"),
         lwd = 2,
         cex = 1,
         horiz = TRUE)
}

make_boxplots <- function(data) {
  x11()
  colors <- rainbow(8)
  i <- 1

  par(mfrow = c(2, 4))
  for(attr in names(data)) {

    if(attr == "Sex") {
      next
    }

    curr_x <- data[, attr]

    boxplot(curr_x,
         xlab = NULL, ylab = NULL,
         main = paste("Zróżnicowanie atrybutu:", attr),
         col = colors[i])
    i <- i + 1
  }
}

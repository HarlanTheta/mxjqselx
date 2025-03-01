#' @title 正态分布检验
#' @param data data.frame
#' @param xname feature
#' @param yname label
mxjqshapirop <- function(data, xname, yname){
  datax <- data[[xname]]
  glevel <- unique(data[[yname]])
  shapiro <- c()
  for (i in seq_along(glevel)) {
    g <- glevel[i]
    grow <- data[[yname]] == g
    dataxg <- datax[grow]
    shapiroi <- shapiro.test(dataxg)
    shapiro <- c(shapiro, as.numeric(shapiroi$p.value))
  }
  return(shapiro)
}

#' @title 连续-连续
#' @param data data.frame
#' @param xname feature
#' @param yname label
#' @param type test type
#' @importFrom fastmit mi.test

mxjqcon2con <- function(data, xname, yname, type = "cor.test"){
  x <- data[[xname]]
  y <- data[[yname]]
  note <- ""
  if (type == "cor.test") {
    cctest <- cor.test(x, y)
  } else if (type == "mi.test") {
    cctest <- fastmit::mi.test(x, y)
  }
  ccpvalue1 <- as.numeric(cctest$p.value)
  ccpvalue2 <- as.character(
    ifelse(ccpvalue1 < 0.001, "p<0.001", round(ccpvalue1, 4))
  )
  return(data.frame(
    x = xname,
    y = yname,
    type = type,
    pvalue = ccpvalue2,
    actualp = ccpvalue1,
    note = note
  ))
}

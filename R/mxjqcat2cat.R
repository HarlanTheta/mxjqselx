#' @title 分类-分类
#' @param data data.frame
#' @param xname feature
#' @param yname label
#' @param type test type
mxjqcat2cat <- function(data, xname, yname, type = "chisq.test"){
  x <- data[[xname]]
  y <- data[[yname]]
  note <- ""
  if (type == "chisq.test") {
    minexp <-
      length(x) * min(table(x)/length(x)) * min(table(y)/length(y))
    if (minexp > 5) {
      cctest <- chisq.test(table(x, y))
    } else {
      cctest <- chisq.test(table(x, y), simulate.p.value = T)
      note <- "simulate.p.value"
    }
  } else if (type == "fisher.test") {
    cctest <- fisher.test(table(x, y))
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

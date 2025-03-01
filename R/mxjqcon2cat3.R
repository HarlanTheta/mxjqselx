#' @title 连续-多分类
#' @param data data.frame
#' @param xname feature
#' @param yname label
mxjqcon2cat3 <- function(data, xname, yname){
  x <- data[[xname]]
  y <- data[[yname]]
  note <- ""

  if(is.factor(x) & is.numeric(y)) {
    numericname <- yname
    factorname <- xname
  } else {
    numericname <- xname
    factorname <- yname
  }

  normtest <- mxjqshapirop(data, numericname, factorname)
  vstest <-
    bartlett.test(as.formula(paste0(numericname, "~", factorname)), data)

  if (all(normtest > 0.05) & vstest$p.value > 0.05) {
    cctest <-
      summary(aov(as.formula(paste0(numericname, "~", factorname)), data))
    ccpvalue1 <- cctest[[1]][1,5]
    type <- "aov"
  } else {
    cctest <-
      kruskal.test(as.formula(paste0(numericname, "~", factorname)), data)
    ccpvalue1 <- as.numeric(cctest$p.value)
    type <- "kruskal.test"
    note <- "break_aov_assumption"
  }

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

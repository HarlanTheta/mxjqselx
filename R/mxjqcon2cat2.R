#' @title 连续-二分类
#' @param data data.frame
#' @param xname feature
#' @param yname label
#' @param type test type
#' @importFrom pROC roc.test
mxjqcon2cat2 <- function(data, xname, yname, type = "roc.test"){
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

  if (type == "roc.test") {
    cctest <- pROC::roc.test(
      response = data[[factorname]],
      predictor1 = data[[numericname]],
      predictor2 = rep(0.5, length(x))
    )
    note <- paste0(
      paste0("AUC=", round(as.numeric(cctest$roc1$auc), 4)),
      ",",
      paste(cctest$roc1$levels, collapse = cctest$roc1$direction)
    )
  } else {
    normtest <- mxjqshapirop(data, numericname, factorname)
    if (all(normtest > 0.05)) {
      cctest <-
        t.test(as.formula(paste0(numericname, "~", factorname)), data)
      type <- "t.test"
    } else {
      cctest <-
        wilcox.test(as.formula(paste0(numericname, "~", factorname)), data)
      type <- "wilcox.test"
      note <- "break_ttest_assumption"
    }
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

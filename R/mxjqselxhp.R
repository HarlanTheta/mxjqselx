#' @title 汇总函数
#' @param data data.frame
#' @param xat feature at
#' @param yat label at
#' @param hp4cat2cat cat and cat test type
#' @param hp4con2con con and con test type
#' @param hp4con2cat2 con and cat2 test type
#' @export

# 综合
mxjqselxhp <- function(
    data, xat, yat,
    hp4cat2cat = "chisq.test",
    hp4con2con = "cor.test",
    hp4con2cat2 = "t.test"
) {
  testresult <- list()
  for (i in seq_along(xat)) {
    iidx <- xat[i]
    datai <- data[, c(iidx, yat)]
    xi <- datai[[1]]
    yi <- datai[[2]]
    xnamei <- colnames(data)[iidx]
    ynamei <- colnames(data)[yat]
    if(is.factor(xi) & is.factor(yi)) {
      testi <- mxjqcat2cat(datai, xnamei, ynamei, type = hp4cat2cat)
    } else if(is.numeric(xi) & is.factor(yi)) {
      if(length(levels(yi)) == 2) {
        testi <- mxjqcon2cat2(datai, xnamei, ynamei, type = hp4con2cat2)
      } else {
        testi <- mxjqcon2cat3(datai, xnamei, ynamei)
      }
    } else if(is.factor(xi) & is.numeric(yi)) {
      if(length(levels(xi)) == 2) {
        testi <- mxjqcon2cat2(datai, xnamei, ynamei, type = hp4con2cat2)
      } else {
        testi <- mxjqcon2cat3(datai, xnamei, ynamei)
      }
    } else if(is.numeric(xi) & is.numeric(yi)) {
      testi <- mxjqcon2con(datai, xnamei, ynamei, type = hp4con2con)
    }
    testresult[[i]] <- testi
  }

  testresult2 <- bind_rows(testresult)
  return(testresult2)
}

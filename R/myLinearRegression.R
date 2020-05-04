#' Linear Regression Analysis
#'
#' Creates  a scatterplot  of  each pair  of covariates. Also creates a linear regression of Y on X. Written by Sara Khorramshahgol
#'
#' @param X: a matrix of  covariates
#' @param Y: a vector  of outcomes
#' @param sub: a list of  subjects (i.e.  a set of integers corresponding to rows in X)
#' @export
#' @return coef: a set  of coefficients  from  the linear  regression of  Y on X
#' @return pvals: the set of corrresponding p-values
myLinearRegression <- function(Y, X, sub) {
  df <- data.frame(one = double())
  #use only rows in sub
  for (i in 1:length(sub)) {
    df[i, ] <- X[sub[i], ] #populate df only with the rows we want
  }

  #use ggpairs to create a scatterplot of each pair of covariates
  if (ncol(df) < 5) {
    ggpairs(df, columns = 1:4)
  }
  else {
    print("Too many variables to plot")
  }

  #run a linear regression of Y on X
  df[Y] <- Y #add Y column
  lr <- lm(Y ~ ., df)

  return_list <- list("coef" = lr$coefficients, "pvals" = summary(lr)$coefficients[,4])
  return(return_list)
}

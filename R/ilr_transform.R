# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Isometric Log Ratio Transformation
#' @description This function does an isometric log ratio transformation.
#' @param X Matrix or data frame that includes only variables in the composition.
#' @return A matrix with (n-1) columns containing the isometric log ratio 
#'     transformed values.
#' @details This function does an isometric log ratio transformation as
#'     described by \href{https://pubmed.ncbi.nlm.nih.gov/26461112/}{Chastin et al., 2005}.
#' @rdname ilr_transform
#' @export 

ilr_transform <- function(X){
  
  if(is.data.frame(X)) X <- as.matrix(X)
  if(any(X < 0)) stop("Negative values are not allowed.")
  
  first_var <- colnames(X)[1]
  c <- ncol(X)
  j <- as.integer(c - 1)
  r <- nrow(X)
  
  X.ilr <- matrix(NA, r, j)
  rX.ilr <- X[, c:1, drop = FALSE]
  
  for (i in 1:r) {
    for (k in j:1) {
      X.ilr[i, k] <- sqrt(k/(k+1)) * (log(rX.ilr[i, k+1] / (prod(rX.ilr[i, k:1])^(1/k))))
    }
  }
  
  X.ilr <- X.ilr[, j:1, drop = FALSE]
  colnames(X.ilr) <- vapply(1:j, function(x) paste0("z", x), character(1))
  
  message(paste("Relative effect of", first_var, "on other variables"))
  
  return(X.ilr)
}

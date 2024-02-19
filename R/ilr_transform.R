# Copyright © 2023 University of Kansas. All rights reserved.
#
# Creative Commons Attribution NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)

#' @title Pivot coordinates for Isometric Log Ratio Transformation
#' @description This function does an isometric log ratio transformation using
#'    methods similar to the `robCompositions` R package.
#' @param X Matrix or data frame that includes only variables in the composition.
#' @return A data frame with (n-1) columns containing the isometric log ratio
#'     transformed values.
#' @details This function does an isometric log ratio transformation as
#'     described by \href{https://pubmed.ncbi.nlm.nih.gov/26461112/}{Chastin et al., 2005}
#'     and comparable to the `pivotCoord` function from the `robCompositions` package.
#' @rdname ilr_pivotCoord
#' @export


ilr_pivotCoord <- function(X){
  
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
  
  X.ilr <- data.frame(X.ilr[, j:1])
  
  if (all(nchar(colnames(X)) > 1)) {
    for (i in 1:(ncol(X) - 1)) {
      colnames(X.ilr)[i] <- paste(colnames(X)[i], "_", paste(substr(colnames(X)[(i + 1):ncol(X)], 1, 2), collapse = "-"), collapse = "", sep = "")
    }
  }
  
  return(X.ilr)
}


#' @noRd
#' @keywords internal

get_psi <- function(sbp){
  isPos = (sbp > 0)
  isNeg = (sbp < 0)
  m <- matrix(1, nrow(sbp), nrow(sbp))
  nPos = m %*% isPos # comparable to column sums
  nNeg = m %*% isNeg
  sbp_t = (isPos * nNeg - isNeg * nPos)
  nn = sapply(1:ncol(sbp_t), function(i) {
    1/sqrt(sbp_t[, i] %*% sbp_t[, i])
  })
  nn = matrix(nn, ncol = ncol(sbp_t), nrow = nrow(sbp_t), byrow = TRUE)
  psi = sbp_t * nn
  return(psi)
}


#' @title Isometric Log Ratio Transformation with a Sequential Binary Partition
#' @description This function does a isometric log ratio transformation using
#'     methods similar to the `compositions` R package.
#' @param X Matrix of data frame that includes only variables in the composition
#' @param sbp A matrix containing the sequential binary partition
#' @return A data frame with (n-1) columns containing the isometric log ratio
#'     transformed values.
#' @details This function does an isometric log ratio transformation as described
#'     by \href{https://www.sciencedirect.com/science/article/pii/S009830040700101X}{van den Boogaart et al., 2008}
#'     and comparable to the `ilr` function from the `compositions` package.
#' @rdname get_ilr
#' @export

get_ilr <- function(X, sbp){
  psi <- get_psi(sbp)
  # centered log ratio transformation
  LOG <- unclass(log(ifelse(X > 0 , X, 1)))
  clr <- ifelse(X > 0, LOG - rowSums(LOG)/rowSums(X > 0), 0)
  # isometric log ratio transform
  ilr <- clr %*% psi
  return(data.frame(ilr))
}
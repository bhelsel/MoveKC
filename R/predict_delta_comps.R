


predict_delta_comps_copy <- function(
    dataf, # data.frame of data
    y, # character name of outcome in dataf
    comps, # character vector of names of compositions in dataf
    covars = NULL, # character vector of names of covariates (non-comp variables) in dataf
    deltas = c(0, 10, 20) / (24 * 60), # changes in compositions to be computed pairwise
    comparisons = c("prop-realloc", "one-v-one")[1],
    alpha = 0.05,
    model = c("lm", "tobit")[1],
    limits = c(0, Inf)
){
  
  dataf <- codaredistlm:::rm_na_data_rows(data, c(y, comps, covars))
  mean_comps <- compositions::mean.acomp(compositions::acomp(dataf[, comps]), robust = FALSE)
  
  # Covariates
  find_ref <- function(var) {
    rf <- rownames(stats::contrasts(var))[which.max(stats::contrasts(var) == 0)]
    rf[!is.na(rf)]
  }
  
  m_cov <- data.frame(matrix(ncol = length(cov)))
  for(i in 1:length(cov)){
    var <- dataf[, cov[i], drop = TRUE]
    if(is.numeric(var)){
      m_cov[, i] <- mean(var)
    } 
    if(is.factor(var)){
      m_cov[, i] <- find_ref(var)
    } 
  }
  colnames(m_cov) <- cov
  
  # Compositional Means
  mean_X <- 
    mean_comps %>%
    t() %>%
    as.data.frame() %>%
    robCompositions::pivotCoord() %>%
    `colnames<-`(paste0("ilr", 1:ncol(.))) %>%
    cbind(., m_cov)
  
  X <- 
    dataf %>%
    dplyr::select(dplyr::all_of(comps)) %>%
    as.matrix() %>%
    robCompositions::pivotCoord() %>%
    `colnames<-`(paste0("ilr", 1:ncol(.))) %>%
    cbind(., dataf) %>%
    dplyr::select(rlang::sym(y), dplyr::contains("ilr"), dplyr::all_of(cov))
  
  # fit model
  if(model == "lm"){
    lm_X <- stats::lm(stats::as.formula(paste(y, "~ .")), data = X)
    m_pred <- cbind(mean_X, stats::predict(lm_X, newdata = mean_X, interval = "confidence", level = 1 - alpha))
  } else if(model == "tobit"){
    lm_X <- AER::tobit(stats::as.formula(paste(y, "~ .")), left = limits[1], right = limits[2], data = X)
    m_pred <- cbind(mean_X, fit = stats::predict(lm_X, newdata = mean_X))
  }
  
  lm_quants <- codaredistlm:::extract_lm_quantities(lm_X, alpha = alpha)
  delta_mat <- codaredistlm:::get_delta_mat(deltas, comparisons, comps, mean_comps)
  poss_comps <- codaredistlm:::get_all_comparison_mat(deltas, comparisons, comps, mean_comps)
  m_comps <- matrix(rep(mean_comps, nrow(delta_mat)), nrow = nrow(delta_mat), byrow = TRUE)
  m_delta <- m_comps + delta_mat
  m_delta_less_0 <- Matrix::rowSums(m_delta < 0)
  ilr_means <- as.matrix(robCompositions::pivotCoord(m_comps[m_delta_less_0 != 1, ]))
  ilr_delta <- as.matrix(robCompositions::pivotCoord(m_delta[m_delta_less_0 != 1, ]))
  x0_star <- codaredistlm:::get_x0_star(lm_quants$dmX, nrow(ilr_delta), paste0("ilr", 1:sum(grepl("ilr", colnames(X)))), ilr_delta, ilr_means)
  y0_star <- x0_star %*% lm_quants$beta_hat
  se_y0_star <- codaredistlm:::get_se_y0_star(x0_star, lm_quants$s_e, lm_quants$XtX_inv)
  
  
  
  # get labels and deltas for reallocations
  realloc_nms <- codaredistlm:::get_realloc_nms(comps, comparisons, poss_comps)[m_delta_less_0 != 1, ]
  delta_list <- codaredistlm:::get_pred_deltas(delta_mat[m_delta_less_0 != 1, ], realloc_nms)
  
  
  preds <-
    cbind(
      as.data.frame(realloc_nms, stringsAsFactors = FALSE),
      delta_list,
      alpha,
      codaredistlm:::get_pred_bounds(y0_star, lm_quants$crit_val, se_y0_star, bound =  0),
      codaredistlm:::get_pred_bounds(y0_star, lm_quants$crit_val, se_y0_star, bound = -1),
      codaredistlm:::get_pred_bounds(y0_star, lm_quants$crit_val, se_y0_star, bound =  1)
    )
  
  colnames(preds) <- c("comp+", "comp-", "delta", "alpha", "delta_pred", "ci_lo", "ci_up")
  
  preds$sig <- ifelse(preds$ci_lo <= 0 & preds$ci_up >= 0, "", "*")
  
  # data.frame to return
  ret_obj <- preds
  class(ret_obj) <- c(class(preds), "deltacomp_obj")
  # add info from function call
  attr(ret_obj, "dataf") <- dataf
  attr(ret_obj, "y") <- y
  attr(ret_obj, "comps") <- comps
  attr(ret_obj, "covars") <- covars
  attr(ret_obj, "lm") <- lm_X
  attr(ret_obj, "deltas") <- deltas
  attr(ret_obj, "comparisons") <- comparisons
  attr(ret_obj, "alpha") <- alpha
  attr(ret_obj, "ilr_basis") <- "Used robCompositions::pivotCoord"
  attr(ret_obj, "mean_pred") <- m_pred
  
  return(ret_obj)
}




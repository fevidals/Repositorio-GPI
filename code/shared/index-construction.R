create_matrix <- function(...) {
  dots <- rlang::list2(...)
  df <- suppressMessages(as_tibble(matrix(unlist(dots, use.names = FALSE), ncol = length(dots), byrow = FALSE), .name_repair = "unique"))
  colnames(df) <- paste0("X", seq_len(ncol(df)))
  df
}

impute_interacted <- function(col, predictors) {
  
  predictors <- predictors %>% 
    mutate_at(vars(starts_with("X")), list(is_na = is.na)) %>%   # add variable called var_is_na
    unite(col = "missing_fct", ends_with("is_na")) %>%           # create a factor var for every combination of missingness (i.e., X1 missing and X2 nonmissing)
    # mutate(missing_fct_fit = as.factor(replace(missing_fct, is.na(col), NA_character_))) %>% 
    # mutate(missing_fct_pred = factor(missing_fct, levels = levels(missing_fct_fit))) %>% 
    mutate_at(vars(starts_with("X")), ~if_else(is.na(.) & str_detect(missing_fct, "FALSE"), 0, .)) %>%        # replace NAs in X with zeros except when there is no nonmissing data for that unit
    fastDummies::dummy_cols("missing_fct", remove_first_dummy = TRUE) 
  
  x_varnames <- colnames(predictors)[!str_starts(colnames(predictors), "fe") & colnames(predictors) != "missing_fct"]
  # (1) scoping out irrelevant special case when there are some units with all missing (no prediction is used) and others with all nonmissing
  # creates two factor levels that break lm; here we just drop the missingness indicator
  # (2) if there is only one case (all nonmissing) then don't include indicator for same reasons
  # missing_levels <- sort(levels(predictors$missing_fct))
  # if(length(missing_levels) == 1 | (length(missing_levels) == 2 & !str_detect(missing_levels[1], "TRUE") & !str_detect(missing_levels[2], "FALSE"))){
  #   x_varnames <- x_varnames[!str_starts(x_varnames, "missing_fct")]
  # }
  
  if(!"fe" %in% colnames(predictors)){
    fit <- lm(as.formula(paste0("col ~ ", paste0(x_varnames, collapse = "*"))), data = predictors)
  } else {
    predictors <- predictors %>% fastDummies::dummy_cols("fe", remove_first_dummy = TRUE) %>% select(-fe)
    fe_varnames <- colnames(predictors)[str_starts(colnames(predictors), "fe")]
    fit <- lm(as.formula(paste0("col ~ ", paste0(x_varnames, collapse = "*"), "+", paste0(fe_varnames, collapse = "+"))), data = predictors)
  }
  
  pred <- rep(NA, nrow(predictors))
  # note this means a prediction is made as long as there is at least one nonmissing item
  pred[predictors$missing_fct != paste(rep("TRUE", sum(str_starts(colnames(predictors), "X"))), collapse = "_")] <- 
    predict(fit, newdata = predictors[predictors$missing_fct != paste(rep("TRUE", sum(str_starts(colnames(predictors), "X"))), collapse = "_"), , drop = FALSE])
  
  if_else(is.na(col), pred, col)
}

#' Mean index with imputation
#'
#' @param ... Unquoted variable names of items to be included in the index
#' @param Z Unquoted name of treatment condition indicator. Imputation only occurs within treatment condition.
#' @param fe (Optional) Unquoted name of fixed effects indicator, used in the imputation regression if provided.
#'
#' @return A mean index, with imputed values
idx_mean <- function(
  ..., # items as unquoted variable names
  tx = NULL, # treatment variable to make predictions only within treatment group
  fe = NULL # fixed effects variable, i.e. blocks as unquoted variable name
) {

  items <- create_matrix(...) # converts dots to matrix and adds fe if used
  n_items <- ncol(items)
  
  items_with_no_nonmissing <- apply(items, 2, function(x) all(is.na(x)))
  if(any(items_with_no_nonmissing)){
    stop("Some of the items you are trying to index have no nonmissing data. Please manually drop them from the index.")
  }
  
  if(is.null(tx)){
    stop("Please provide the treatment variable, within which the function imputes missing data.")
  }

  if(!is.null(fe)){
    items$fe <- factor(fe, levels = na.omit(unique(fe)), labels = seq_along(na.omit(unique(fe))))
  }

  items_imputed <- matrix(NA, nrow = nrow(items), ncol = n_items)
  for(i in seq_len(n_items)) {
    # run the imputation function for each item
    for(val in unique(tx)){
      # make predictions only within treatment group
      items_imputed[tx == val, i] <- 
        impute_interacted(
          col = items %>% filter(tx == val) %>% pull(i),
          predictors = items %>% select(-i) %>% filter(tx == val)
        )
    }
  }
  
  # calculate simple mean across items for each observation
  # NB: this will leave as NA the rows for which all items are NA 
  apply(items_imputed, 1, mean)
}

#' Mean index with imputation (listwise deletion, if any item is missing index is missing)
#'
#' @param ... Unquoted variable names of items to be included in the index
#'
#' @return A mean index, with listwise deletion in items
idx_mean_listwise <- function(
  ... # items as unquoted variable names
) {
  
  items <- create_matrix(...) # converts dots to matrix and adds fe if used
  apply(items, 1, mean)
}


#' Standardize Variable
#'
#' @param variable Unquoted name of variable to standardize
#' @param compare Variable to use to calculate mean and standard deviation from
#' @param condition Logical vector (TRUE or FALSE) used to subset \code{compare} variable before calculating mean and standard deviation.
#' @param na.rm Option to remove NA's when calculating mean and standard deviation. Set to \code{TRUE} by default. Note that all values will be NA in the standardized variable if set to \code{FALSE}.
#'
#' @return A vector with the standardized variable
#'
#' var <- rnorm(100, mean = 0.5, sd = 1)
#'
#' var_std <- stdize(var)
#'
#' summary(var_std)
#'
stdize <- function(variable, to, condition, na.rm = TRUE) {
  
  # checks
  if(missing(variable)) stop("Please provide variable you wish to standardize.")
  if(!is.numeric(variable)) stop("variable must be numeric.")
  if(missing(to)) {
    to <- variable
  } else {
    if(!is.numeric(to)) stop("to must be a numeric variable.")
  }
  if(missing(condition)) {
    condition <- TRUE
  } else {
    if(!is.logical(condition)) condition <- as.logical(condition)
  }
  
  # standardize
  (variable - mean(to[condition == TRUE], na.rm = na.rm)) / sd(to[condition == TRUE], na.rm = na.rm)
}

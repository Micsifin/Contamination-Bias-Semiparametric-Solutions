###############################################################################
# This code provides the functions to:
# 1 - Clean the datasets
# 2 - Create splines from the datasets
# 3 - Identify splines from specific columns
# 4 - Run regressions with MulTE
# 5 - Make the tables with all coefficients
###############################################################################

###############################################################################
# 1 - Cleaning Datasets -----------
###############################################################################

clean_art3 <- function(fp){
  # Load the dataset (all dataset are .dta files)
  data <- haven::read_dta(fp)

  # Create a copy of the data to avoid modifying the original
  result_data <- data.frame(data)

  # Creating additional variables
  result_data <- result_data %>%
    group_by(id) %>%
    mutate(
        # Create time-specific output variables
        preoutput = ifelse(time <= 8, output, NA),
        output14 = ifelse(time <= 4, output, NA),
        output58 = ifelse(time > 4 & time <= 8, output, NA),
        geoutput = ifelse(time > 8, output, NA),
        
        # Calculate means by id
        preoutputmean = mean(preoutput, na.rm = TRUE),
        output14mean = mean(output14, na.rm = TRUE),
        output58mean = mean(output58, na.rm = TRUE),
        geoutputmean = mean(geoutput, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Create log variables and Treatment Variables
    mutate(
        treat = case_when(
            d_pos_gift == 1 ~ 1,
            d_neg_gift == 1 ~ 2,
            d_knd_gift == 1 ~ 3,
            TRUE ~ 0
        ),
        log_output = as.numeric(log(output)),
        log_preoutputmean =  as.numeric(log(preoutputmean)),
        log_output14mean =  as.numeric(log(output14mean)),
        log_output58mean =  as.numeric(log(output58mean)),
        log_geoutputmean =  as.numeric(log(geoutputmean)),
        treat = factor(treat, levels=0:3,
                        labels=c("Control", "Positive Gift",
                                 "Negative Gift", "Positive In-Kind Gift"))
        
    ) %>%
    select(
        id, time, treat, output, log_output,
        preoutputmean, output58mean,
        log_preoutputmean, log_output58mean, group
    )
  
  return(result_data)
}

clean_art8 <- function(fp){
  # Load the dataset (all dataset are .dta files)
  data <- haven::read_dta(fp)

  # Create a copy of the data to avoid modifying the original
  result_data <- data.frame(data)

  # Remove original dataset from environment
  rm(data)
  gc()

  # Creating additional variables
  result_data <- result_data %>%
    # Create isrcash and Treatment Variables
    mutate(
        isrcash = ifelse(isrstock == 1 | cash == 1, 1, 0),
        treat = case_when(
            palstock == 1 ~ 1, # palstock
            isrcash == 1 ~ 2,  # isrcash
            TRUE ~ 0           # cc
        ),
        treat = factor(treat, levels=0:2,
                        labels=c("Control", "Palestinian Assets",
                                 "Non-Palestinian Assets")),
        pricechange_m16 = as.numeric(pricechange_m16) / 100
    ) %>%
    select(
      coarseorder_s3, p_index_s3, treat,
      age, age2, newses, willingrisk1to10,
      plitscore, pricechange_m16,
      right_2013, left_2013, male, postsecondary,
      BA_student, college_grad, married,
      tradestock6all, r_trad, r_relig,
      r_ultra, g_jerusalem, g_north,
      g_haifa, g_telaviv, g_south,
      g_wb, patient, block13
    )
  
  return(result_data)
}

clean_art10 <- function(fp){
  # Load the dataset (all dataset are .dta files)
  data <- haven::read_dta(fp)

  # Creating additional variables
  data <- data %>%
    # Create Treatment Variable
    mutate(
        treat = case_when(
            moral_rel == 1 ~ 1,
            cash_rebate == 1 ~ 2,
            credit_rep == 1 ~ 3,
            simple_rem == 1 ~ 4,
            religious_plac == 1 ~ 5,
            moral_imp == 1 ~ 6,
            moral_non == 1 ~ 7,
            TRUE ~ 0
        ),
        treat = factor(treat, levels=0:7,
                        labels=c("Control", "Moral Incentive",
                                 "Cash Rebate", "Credit Reputation",
                                 "Simple Reminder", "Religious Placebo",
                                 "Implicit Moral Incentive", "Nonreligious Moral Incentive"))
    ) %>%
    subset(main_exp == 1) %>%
    select(
      delinquent, treat, age, income, creditlimit,
      female, muslim, province,
      sample_before, poor_credit_history, wave
    )
  
  return(data)
}

clean_art13 <- function(fp, t){
  # Load the dataset (all dataset are .dta files)
  data <- haven::read_dta(fp)

  if (t == 0){
  # Creating additional variables
  data <- data %>%
    mutate(
      TreatmentGroup = factor(TreatmentGroup, levels=0:3,
                        labels=c("Control", "Instructor Incentives",
                                 "Combined Incentives", "Student Incentives"))
    ) %>%
    select(
      TeacherID, TreatmentGroup, ZPercentage,
      FullTime, CoReq, Fall, White, Male, LowDiscount,
      LowCommit, Age, BaselineCredits, DepartmentCode,
      Valid, Demographics
    )
  } else if (t == 1) {
  # Creating additional variables
  data <- data %>%
    mutate(
      TreatmentGroup = factor(TreatmentGroup, levels=0:3,
                        labels=c("Control", "Instructor Incentives",
                                 "Combined Incentives", "Student Incentives"))
    ) %>%
    select(
      TeacherID, TreatmentGroup, ZPercentage, ZGradePoint, ZNonExamPct,
      FullTime, CoReq, Fall, White, Male, LowDiscount,
      LowCommit, Age, BaselineCredits, DepartmentCode,
      Valid, Demographics, Completion, Percentage
    )
  } else {print("Error: t must be 0 or 1")}
  
  return(data)
}

clean_art14 <- function(fp){
    # Load the dataset (all dataset are .dta files)
    data <- haven::read_dta(fp)
    
    # Creating additional variables
    data <- data %>%
        # Create Treatment Variable
        mutate(
                treat = case_when(
                        tx1 == 1 ~ 1,
                        tx2 == 1 ~ 2,
                        tx3 == 1 ~ 3,
                        TRUE ~ 0
                ),
                treat = factor(treat, levels=0:3,
                        labels=c("Control", "Low Subsidy", "Medium Subsidy", "High Subsidy")),
                # Additional variables → already created
                #takeup = takeup * 100,               # Multiply takeup by 100 
                #prop200m = ifelse(is.na(prop200m), 0, prop200m),  # Replace missing values with 0
                #prop200m2 = prop200m * 100           # Create prop200m2 as prop200m * 100
        )

    data <- data %>%
        # Select relevant columns
        select(
            siteno, connected, treat, takeup,
            wall, senior, notfarmer,
            bank, employed, busia, funded,
            market, black, someschool,
            hhsize, age, chickens, electrification,
            population, earn, prop200m2,
            t1_wall, t2_wall, t3_wall,
            t1_someschool, t2_someschool, t3_someschool,
            t1_black, t2_black, t3_black,
            t1_electrification, t2_electrification, t3_electrification,
            t1_prop200m2, t2_prop200m2, t3_prop200m2,
            t1_earn, t2_earn, t3_earn
        )
    
    return(data)
}


###############################################################################
# 2 - Create Spline from Dataset -----------
###############################################################################

#' @param data A dataframe containing the covariates
#' @param covariates A character vector of column names for continuous covariates
#' @param r Integer, degree of polynomial (default = 3)
#' @param J Integer, number of knots/quantiles (default = 2)
#' @return A dataframe with additional columns for spline variables
create_splines <- function(data, covariates, r = 3, J = 2) {
  # Create a copy of the data to avoid modifying the original
  result_data <- data.frame(data)

  # Remove original dataset from environment
  rm(data)
  gc()
  
  # Loop through each covariate
  for (w_i in covariates) {
    # Check if the covariate is continuous (not a factor or character)
    if (!is.numeric(result_data[[w_i]])) {
        warning(paste("Skipping", w_i, "because it is not numeric. Splines require continuous variables."))
        next  # Skip to the next covariate
    }

    # 1. Create r new columns with powers of W_i
    for (k in 1:r) {
      new_col_name <- paste0(w_i, "_pow", k)
      result_data[[new_col_name]] <- result_data[[w_i]]^k
    }
    
    # 2. Define quantile points to use as knots
    knots <- seq(0, 1, length.out = J + 2)[2:(J+1)]
    quantile_probs <- seq(0, 1, length.out = J + 2)[2:(J+1)]
    
    for (j in 1:J) {
      # Use the quantile value directly as the knot
      knots[j] <- quantile(result_data[[w_i]], quantile_probs[j], na.rm = TRUE)
    }
    
    # 3. Create J new columns with truncated power terms
    for (j in 1:J) {
      new_col_name <- paste0(w_i, "_knot", j)
      result_data[[new_col_name]] <- (pmax(0, result_data[[w_i]] - knots[j]))^(r-1)
    }
  }
  
  return(result_data)
}


###############################################################################
# 3 - Identify Splines from Specific Columns -----------
###############################################################################

#' Identify spline columns for a specific variable
#'
#' @param data A dataframe containing spline columns
#' @param variable The name of the original variable for which splines were created
#' @return A character vector containing names of spline columns for the specified variable
get_variable_splines <- function(data, variables) {
  # Get all column names
  all_cols <- colnames(data)

  # Do the process for each variable in variables
  spline_cols <- c()
  for (var in variables){
    # Create patterns to match power and knot columns for this variable
    pow_pattern <- paste0("^", var, "_pow\\d+$")
    knot_pattern <- paste0("^", var, "_knot\\d+$")
  
    # Find columns matching the patterns
    pow_cols <- all_cols[grepl(pow_pattern, all_cols)]
    knot_cols <- all_cols[grepl(knot_pattern, all_cols)]
    
    # Combine all spline columns
    spline_cols <- c(spline_cols, pow_cols, knot_cols)
  }
  
  return(spline_cols)
}

###############################################################################
# 4 - Run regression with MulTE -----------
###############################################################################
#' Here, we have 2 functions:

#' Run regressions and their MulTE analysis without splines
#'
#' @param data (subsetted) dataframe containing the data for regression
#' @param depvar Dependent variable name (string)
#' @param treat_categ Treatment and categorical variables as a string: "treat + categ1 + categ2 + ..."
#' @param treat_name Name of the treatment variable (string): "treat"
#' @param cont_vars A character vector of continuous control variable names: c(cont1, cont2, ...)
#' @param cluster Logical; whether to cluster standard errors
#' @param cluster_var the definitive variable to cluster on: subset(df, condition)$var_to_cluster
#' @return Returns the MulTE regression coefficients

multe_simple_regression <- function(df, depvar, treat_categ, treat_name,
                                    cont_vars, cluster = FALSE, cluster_var = NULL, na_action = FALSE) {
    # Creating depvar ~ treat + categ1 + categ2 + ...
    dtc <- paste(depvar, treat_categ, sep = " ~ ")

    # Creating the regression formula with continuous variables
    formula <- paste(dtc, paste(cont_vars, collapse = " + "))

    # Run simple regression
    reg <- lm(formula, data = df)

    # Run MulTE analysis clustering if specified
    if (cluster == TRUE){
        if (na_action == TRUE){
          reg$clust <- factor(cluster_var)[-na.action(reg)]
        }
        else {
            reg$clust <- factor(cluster_var)
        }
        reg_m <- multe::multe(reg, treatment = treat_name, cluster = reg$clust)
    } else {
        reg_m <- multe::multe(reg, treatment = treat_name)
    }

    reg_m
}

#' Run regression MulTE analysis with splines
#'
#' @param data (subsetted) dataframe containing the data for regression
#' @param depvar Dependent variable name (string)
#' @param treat_categ Treatment and categorical variables as a string: "treat + categ1 + categ2 + ..."
#' @param treat_name Name of the treatment variable (string): "treat"
#' @param cont_vars A character vector of continuous control variable names: c(cont1, cont2, ...)
#' @param cluster Logical; whether to cluster standard errors
#' @param cluster_var the definitive variable to cluster on: subset(df, condition)$var_to_cluster
#' @return Returns the MulTE regression coefficients

multe_spline_regression <- function(df, depvar, treat_categ, treat_name, cont_vars,
                                    cluster = FALSE, cluster_var = NULL, na_action = FALSE) {
    # Creating depvar ~ treat + categ1 + categ2 + ...
    dtc <- paste(depvar, treat_categ, sep = " ~ ")

    # Get the corresponding spline columns for the continuous variables
    spls_vars <- paste(get_variable_splines(data = df, variable = cont_vars), collapse = " + ")

    # Run regression with splines
    reg_spl <- lm( paste(dtc, spls_vars), data = df )

    # Run MulTE analysis clustering if specified
    if (cluster == TRUE) {
      if (na_action == TRUE) {
        reg_spl$clust <- factor(cluster_var)[-na.action(reg_spl)]
      } else {
        reg_spl$clust <- factor(cluster_var)
      }
        reg_spl_m <- multe::multe(reg_spl, treatment = treat_name, cluster = reg_spl$clust)
    } else {
        reg_spl_m <- multe::multe(reg_spl, treatment = treat_name)
    }
    reg_spl_m
}

###############################################################################
# 5 - Make the tables -----------
###############################################################################

#' This function creates the LaTeX tables with the regressions results
#' for a single panel, and only 1 set of standard errors (if specified)
coeffs_table_spl <- function(a, a_spl, file, digits=3, two_SE=FALSE) {  
  # Reorganize columns: PL | ATE_beta, ATE_spline | EW_beta, EW_spline | CW_beta, CW_spline
  T1 <- cbind(
    a$est_f[, "PL"],              # Column 1: PL
    a$est_f[, "ATE"],             # Column 2: ATE beta
    a_spl$est_f[, "ATE"],         # Column 3: ATE spline
    a$est_f[, "EW"],              # Column 4: EW beta
    a_spl$est_f[, "EW"],          # Column 5: EW spline
    a$est_f[, "CW"],              # Column 6: CW beta
    a_spl$est_f[, "CW"]           # Column 7: CW spline
  )
  
  # Sample sizes and control counts
  ns <- c(NA, a$n_f, a_spl$n_f, NA, NA, NA, NA)      # Sample sizes
  ks <- c(NA, a$k_f, a_spl$k_f, NA, NA, NA, NA)      # Number of controls

  # If overlap fails, append their results as well
  if (!is.null(a$est_o)) {
    # Reorganize overlap columns the same way
    T1_o <- cbind(
      a$est_o[, "PL"],
      a$est_o[, "ATE"],
      a_spl$est_o[, "ATE"],
      a$est_o[, "EW"],
      a_spl$est_o[, "EW"],
      a$est_o[, "CW"],
      a_spl$est_o[, "CW"]
    )

    T1 <- cbind(T1, T1_o)        # Append overlap estimates
    
    # Append additional control counts and sample sizes
    ks <- c(ks, c(NA, a$k_o, a_spl$k_o, NA, NA, NA, NA))
    ns <- c(ns, c(NA, a$n_o, a_spl$n_o, NA, NA, NA, NA))
  }
  
  # Calculate indices for point estimates and standard errors
  pop <- (seq.int(nrow(T1)/3)-1)*3+1  # Every third row starting at 1 (point estimates)

  # Define formatting functions
  format_int <- function(x) format(x, nsmall=0, big.mark=",")  # Format integers with commas
  format_re <- function(x) formatC(x, format="f", digits=digits)  # Format real numbers
  
  # Format main estimates table
  t1 <- ifelse(is.na(T1), "", format_re(T1))
  t1[pop+1, ] <- paste0("(", paste(t1[pop+1, ], sep=","), ")")  # Add parentheses to std errors
  t1[pop+2, ] <- paste0("[", paste(t1[pop+2, ], sep=","), "]")  # Add brackets to conf intervals
  
  # Clean up empty parentheses/brackets
  t1 <- ifelse(t1=="()", "", t1)
  t1 <- ifelse(t1=="[]", "", t1)
  
  # Add control count and sample size rows
  t1 <- rbind(t1,
        "",         # this row will be the one with \midrule
        ifelse(is.na(ks), "", paste0("\\multicolumn{1}{r}{",
                       format_int(ks), "}")),
        ifelse(is.na(ns), "", paste0("\\multicolumn{1}{r}{",
                       format_int(ns), "}")))

  # Clean up row names
  rownames(t1)[c(pop+1, pop+2)] <- ""

  # Add descriptive row names for sample info
  rownames(t1)[NROW(t1)-2] <- ""
  rownames(t1)[NROW(t1)-1] <- "Number of controls"
  rownames(t1)[NROW(t1)] <- "Sample size"

  # Add row names as first column
  t1 <- cbind(rownames(t1), t1)

  # Convert to LaTeX table format with & separators and \\ line endings
  t1 <- tidyr::unite(data=as.data.frame(t1), col="z", sep = " & ")
  t1 <- unlist(tidyr::unite(data=cbind(t1, c(rep("\\\\", nrow(t1)-1), "")),
                col="z", sep=""))
  if(is.null(a$est_o)){
    # caso em que não falhou o overlap
    t1[NROW(t1)-2] <- "\\cmidrule(lr){2-8}"     # Adds column with a midrule between estimates and sample info
  } else{
    # caso em que falhou o overlap
    t1[NROW(t1)-2] <- "\\cmidrule(lr){2-8} \\cmidrule(lr){9-15}"     # Adds column with a midrule between estimates and sample info
  }


  if(!two_SE){
    # Removes the second set of standard errors, the ones in brackets
    t1_new <- ""
    for (row in seq(1, NROW(t1))){
        # every third row is a standard error in bracket,
        # except the last 3.
        if(row %% 3 != 0 || row == NROW(t1)-2 || row == NROW(t1)-1 || row == NROW(t1)){
            t1_new <- c(t1_new, t1[row])
        }
    }
    t1 <- t1_new
  }
                
  # Write output to LaTeX files
  write(unname(t1), file=paste0(file, ".tex"))  # Main estimates table
}

###############################################################################
# 5 - Export estimates for coefficient and SE analyses ------------------------
###############################################################################

# This function exports the estimates.
# It is based on the one in Goldsmith Pinkham, Hull and Kolesár (2024)'s
# replication package.
# In this case, it does not export own_est, own_se, bias_est, or bias_se.
# But it does export ate_se, ew_se, cw_se, t_estat_reg
export_est_se <- function(a) {
    est <- if(is.null(a$est_o)) a$est_f else a$est_o
    #cb <- if(is.null(a$cb_o)) a$cb_f else a$cb_o
    row <- (seq.int(nrow(est)/3)-1)*3+1
    #row_cb <- (seq.int(nrow(est)/3)-1)*2+1
    d <- data.frame(treatment=row.names(est[row, ]),
                    reg_est=est[row, 1],
                    ate_est=est[row, 3],
                    ew_est=est[row, 4],
                    cw_est=est[row, 5],
                    reg_se=est[row+1, 1],
                    ate_se=est[row+1, 3],
                    ew_se=est[row+1, 4],
                    cw_se=est[row+1, 5])
    rownames(d) <- NULL
    d
}

###############################################################################
# A.1 - Make the tables with all coefficients ---------------------------------
###############################################################################

#' This is a LEGACY function, used in previous versions of the code.
#' It is being kept here for curiosity's sake.
#' 
#' This function creates the LaTeX tables with the regressions results
#' in two panels, with both sets of standard errors.
coeffs_table_spl_og <- function(a, a_spl, file, digits=3) {  
  # Reorganize columns: PL | ATE_beta, ATE_spline | EW_beta, EW_spline | CW_beta, CW_spline
  T1 <- cbind(
    a$est_f[, "PL"],              # Column 1: PL
    a$est_f[, "ATE"],             # Column 2: ATE beta
    a_spl$est_f[, "ATE"],      # Column 3: ATE spline
    a$est_f[, "EW"],              # Column 4: EW beta
    a_spl$est_f[, "EW"],       # Column 5: EW spline
    a$est_f[, "CW"],              # Column 6: CW beta
    a_spl$est_f[, "CW"]        # Column 7: CW spline
  )
  
  # Same reorganization for confidence bounds
  T2 <- cbind(
    a$cb_f[, "PL"],               # Column 1: PL
    a$cb_f[, "ATE"],              # Column 2: ATE beta
    a_spl$cb_f[, "ATE"],       # Column 3: ATE spline
    a$cb_f[, "EW"],               # Column 4: EW beta
    a_spl$cb_f[, "EW"],        # Column 5: EW spline
    a$cb_f[, "CW"],               # Column 6: CW beta
    a_spl$cb_f[, "CW"]         # Column 7: CW spline
  )
  
  # Sample sizes and control counts
  ns <- c(NA, a$n_f, a_spl$n_f, NA, NA, NA, NA)      # Sample sizes
  ks <- c(NA, a$k_f, a_spl$k_f, NA, NA, NA, NA)      # Number of controls

  # If overlap fails, append their results as well
    if (!is.null(a$est_o)) {
    # Reorganize overlap columns the same way
    T1_o <- cbind(
      a$est_o[, "PL"],
      a$est_o[, "ATE"],
      a_spl$est_o[, "ATE"],
      a$est_o[, "EW"],
      a_spl$est_o[, "EW"],
      a$est_o[, "CW"],
      a_spl$est_o[, "CW"]
    )
    
    T2_o <- cbind(
      a$cb_o[, "PL"],
      a$cb_o[, "ATE"],
      a_spl$cb_o[, "ATE"],
      a$cb_o[, "EW"],
      a_spl$cb_o[, "EW"],
      a$cb_o[, "CW"],
      a_spl$cb_o[, "CW"]
    )

    T1 <- cbind(T1, T1_o)        # Append overlap estimates
    T2 <- cbind(T2, T2_o)         # Append overlap confidence bounds
    
    # Append additional control counts and sample sizes
    ks <- c(ks, c(NA, a$k_o, a_spl$k_o, NA, NA, NA, NA))
    ns <- c(ns, c(NA, a$n_o, a_spl$n_o, NA, NA, NA, NA))
  }
  
  # Calculate indices for point estimates and standard errors
  pop <- (seq.int(nrow(T1)/3)-1)*3+1  # Every third row starting at 1 (point estimates)
  odd <- (seq.int(nrow(T2)/2)-1)*2+1  # Every other row starting at 1 (for confidence bounds)

  # Define formatting functions
  format_int <- function(x) format(x, nsmall=0, big.mark=",")  # Format integers with commas
  format_re <- function(x) formatC(x, format="f", digits=digits)  # Format real numbers
  
  # Format main estimates table
  t1 <- ifelse(is.na(T1), "", format_re(T1))
  t1[pop+1, ] <- paste0("(", paste(t1[pop+1, ], sep=","), ")")  # Add parentheses to std errors
  t1[pop+2, ] <- paste0("[", paste(t1[pop+2, ], sep=","), "]")  # Add brackets to conf intervals
  
  # Clean up empty parentheses/brackets
  t1 <- ifelse(t1=="()", "", t1)
  t1 <- ifelse(t1=="[]", "", t1)
  
  # Add control count and sample size rows
  t1 <- rbind(t1,
        ifelse(is.na(ks), "", paste0("\\multicolumn{1}{r}{",
                       format_int(ks), "}")),
        ifelse(is.na(ns), "", paste0("\\multicolumn{1}{r}{",
                       format_int(ns), "}")))

  # Format confidence bounds table
  t2 <- ifelse(is.na(T2), "", format_re(T2))
  t2[odd+1, ] <- paste0("(", paste(t2[odd+1, ], sep=","), ")")  # Add parentheses to std errors
  t2 <- ifelse(t2=="()", "", t2)  # Clean up empty parentheses

  # Clean up row names
  rownames(t2)[odd+1] <- ""
  rownames(t1)[c(pop+1, pop+2)] <- ""

  # Add descriptive row names for sample info
  rownames(t1)[NROW(t1)-1] <- "Number of controls"
  rownames(t1)[NROW(t1)] <- "Sample size"

  # Add row names as first column
  t1 <- cbind(rownames(t1), t1)
  t2 <- cbind(rownames(t2), t2)

  # Convert to LaTeX table format with & separators and \\ line endings
  t1 <- tidyr::unite(data=as.data.frame(t1), col="z", sep = " & ")
  t1 <- unlist(tidyr::unite(data=cbind(t1, c(rep("\\\\", nrow(t1)-1), "")),
                col="z", sep=""))
  t2 <- tidyr::unite(data=as.data.frame(t2), col="z", sep = " & ")
  t2 <- unlist(tidyr::unite(data=cbind(t2, c(rep("\\\\", nrow(t2)-1), "")),
                col="z", sep=""))
                
  # Write output to LaTeX files
  write(unname(t1), file=paste0(file, "A.tex"))  # Main estimates table
  write(unname(t2), file=paste0(file, "B.tex"))  # Confidence bounds table
}
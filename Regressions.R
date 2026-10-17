###############################################################################
# Topic: Scientific Iniciation in Econometrics
# Advisor: Vítor Augusto Possebom
# Autor: Michel Csillag Finger
# Goal: Run all regressions and save respective MulTE objects
###############################################################################
# Organize the working environment
###############################################################################

# Clean the working environment
rm(list = ls())
load.lib <- c("haven", "dplyr", "ggplot2", "stargazer", "multe", "tidyr", "stringr", "testthat")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)
remove(install.lib, lib, load.lib)

setwd("/home/micsifin/transfer/Estudos/FGV-EESP/IC/Codigo")

# Functions to load
source("Functions.R")

###############################################################################
# Regression Analysis Organization -----------
###############################################################################

#' this script contains the following organization for each article:

#' 1 - Setup: creates all the variables needed for this article.
#' Variables include: file path, continuous covariates, treatment, etc.

#' 2 - Creating Splines: creates the splines for the continuous covariates

#' 3 - Regressions: for each selected column in the article, we run the
#' analysis to obtain the respective MulTE object. The analysis is done
#' for the regression with and without splines.

#' 4- Saving Output and Cleaning memory:
#' exports MulTE objects to .RData files, and removes objects from memory.



###############################################################################
# Article 2 - When Doctors Fall From the Sky -----------
###############################################################################
#' No continuous covariates in tables 2 and 3, only categorical, and strata.
#' Result: splines do not apply. Skipping this analysis

###############################################################################
# Article 3 - Social Preferences and Gift Exchange at Work - Table 1 ----------
###############################################################################

#' 1 - Setup
art_3_fp <- "Data/3_T1_CleanedDataLong_260107.dta"
art_3_covs <- c("preoutputmean", "output58mean",
                "log_preoutputmean", "log_output58mean")

#' 2 - Creating Splines
art_3_df <- clean_art3(art_3_fp)
art_3_spline_df <- create_splines(data = art_3_df, covariates = art_3_covs)

# subsetting to keep only relevant time periods
art_3_df_9_10 <- subset(art_3_spline_df, time %in% c(9, 10))


#' 3 - Regressions

# Column 2 - abs
a3_c2_abs_m <- multe_simple_regression(
    df = art_3_df_9_10,
    depvar = "output",
    treat_categ = "treat + ",
    treat_name = "treat",
    cont_vars = c("preoutputmean"),
    cluster = TRUE,
    cluster_var = art_3_df_9_10$group
)
a3_c2_abs_spls_m <- multe_spline_regression(
    df = art_3_df_9_10,
    depvar = "output",
    treat_categ = "treat + ",
    treat_name = "treat",
    cont_vars = c("preoutputmean"),
    cluster = TRUE,
    cluster_var = art_3_df_9_10$group
)

# Column 2 - log (Column 5)
a3_c2_log_m <- multe_simple_regression(
    df = art_3_df_9_10,
    depvar = "log_output",
    treat_categ = "treat + ",
    treat_name = "treat",
    cont_vars = c("log_preoutputmean"),
    cluster = TRUE,
    cluster_var = art_3_df_9_10$group
)
a3_c2_log_spls_m <- multe_spline_regression(
    df = art_3_df_9_10,
    depvar = "log_output",
    treat_categ = "treat + ",
    treat_name = "treat",
    cont_vars = c("log_preoutputmean"),
    cluster = TRUE,
    cluster_var = art_3_df_9_10$group
)

# Column 3 - abs
a3_c3_abs_m <- multe_simple_regression(
    df = art_3_df_9_10,
    depvar = "output",
    treat_categ = "treat + ",
    treat_name = "treat",
    cont_vars = c("output58mean"),
    cluster = TRUE,
    cluster_var = art_3_df_9_10$group
)
a3_c3_abs_spls_m <- multe_spline_regression(
    df = art_3_df_9_10,
    depvar = "output",
    treat_categ = "treat + ",
    treat_name = "treat",
    cont_vars = c("output58mean"),
    cluster = TRUE,
    cluster_var = art_3_df_9_10$group
)

# Column 3 - log (Column 6)
a3_c3_log_m <- multe_simple_regression(
    df = art_3_df_9_10,
    depvar = "log_output",
    treat_categ = "treat + ",
    treat_name = "treat",
    cont_vars = c("log_output58mean"),
    cluster = TRUE,
    cluster_var = art_3_df_9_10$group
)
a3_c3_log_spls_m <- multe_spline_regression(
    df = art_3_df_9_10,
    depvar = "log_output",
    treat_categ = "treat + ",
    treat_name = "treat",
    cont_vars = c("log_output58mean"),
    cluster = TRUE,
    cluster_var = art_3_df_9_10$group
)



#' 4- Saving Output and Cleaning memory

save(
    a3_c2_abs_m, a3_c2_abs_spls_m,
    a3_c2_log_m, a3_c2_log_spls_m,
    a3_c3_abs_m, a3_c3_abs_spls_m,
    a3_c3_log_m, a3_c3_log_spls_m,
    file = "MulTE/Art_3.RData"
)

rm(a3_c2_abs_m, a3_c2_abs_spls_m, a3_c2_log_m, a3_c2_log_spls_m,
    a3_c3_abs_m, a3_c3_abs_spls_m, a3_c3_log_m, a3_c3_log_spls_m,
    art_3_fp, art_3_covs, art_3_df, art_3_spline_df, art_3_df_9_10)


###############################################################################
# Article 8 - Valuing Peace - Table 9 -----------------------------------------
###############################################################################

#' 1 - Setup
art_8_fp <- "Data/8_T9_replicationdata.dta"

# Dependent variables
a8_pA_depvar <- "coarseorder_s3"
a8_pB_depvar <- "p_index_s3"

# Treatment and covariates
art_8_cov_cont <- c("age", "age2", "willingrisk1to10", "plitscore", "pricechange_m16")

a8_tc <- "treat + right_2013 + left_2013 + as.factor(block13) + male + postsecondary + BA_student + college_grad + married + tradestock6all + r_trad + r_relig + r_ultra + g_jerusalem + g_north + g_haifa + g_telaviv + g_south + g_wb + as.factor(newses) + patient + "

a8_c1_cont <- c("age", "age2", "willingrisk1to10", "plitscore")
a8_c5_cont <- c("age", "age2", "willingrisk1to10", "plitscore", "pricechange_m16")


#' 2 - Creating Splines
art_8_df <- clean_art8(art_8_fp)
art_8_spline_df <- create_splines(data = art_8_df, covariates = art_8_cov_cont)

#' 3 - Regressions
# Panel A - Column 1
a8_pAc1_m <- multe_simple_regression(
    df = art_8_spline_df,
    depvar = a8_pA_depvar,
    treat_categ = a8_tc,
    treat_name = "treat",
    cont_vars = a8_c1_cont
) 
#' as.factor(block13)104 failed overlap - dropped;
#' The following variables have no within-treatment variation and are dropped: left_2013

a8_pAc1_spls_m <- multe_spline_regression(
    df = art_8_spline_df,
    depvar = a8_pA_depvar,
    treat_categ = a8_tc,
    treat_name = "treat",
    cont_vars = a8_c1_cont
)
#' Dropped due to collinearity: age2_pow1
#' Failed overlap: as.factor(block13)104
#' The following variables have no within-treatment variation and are dropped: left_2013


# Panel A - Column 5
a8_pAc5_m <- multe_simple_regression(
    df = art_8_spline_df,
    depvar = a8_pA_depvar,
    treat_categ = a8_tc,
    treat_name = "treat",
    cont_vars = a8_c5_cont
)
# Warning messages:
# 1: In x * ws^2 * cw * ro$residuals :
#  longer object length is not a multiple of shorter object length
# e mais 7 dessas
#' => não tem problema!
#' houve os mesmos erros que a8pAc1, porém a diferença é aqui
#' dropparam pricechange_m16, que é a única variável que difere entre as regressões

a8_pAc5_spls_m <- multe_spline_regression(
    df = art_8_spline_df,
    depvar = a8_pA_depvar,
    treat_categ = a8_tc,
    treat_name = "treat",
    cont_vars = a8_c5_cont
)
# Error in solve.default(He[idx1, idx1, drop = FALSE], He[idx1, -idx1, drop = FALSE]) :
#  Lapack routine dgesv: system is exactly singular: U[1,1] = 0

# Panel B - Column 1
a8_pBc1_m <- multe_simple_regression(
    df = art_8_spline_df,
    depvar = a8_pB_depvar,
    treat_categ = a8_tc,
    treat_name = "treat",
    cont_vars = a8_c1_cont
)
#' as.factor(block13)104 failed overlap - dropped;
#' The following variables have no within-treatment variation and are dropped: left_2013

a8_pBc1_spls_m <- multe_spline_regression(
    df = art_8_spline_df,
    depvar = a8_pB_depvar,
    treat_categ = a8_tc,
    treat_name = "treat",
    cont_vars = a8_c1_cont
)
#' as.factor(block13)104 failed overlap - dropped;
#' The following variables have no within-treatment variation and are dropped: left_2013

# Panel B - Column 5
a8_pBc5_m <- multe_simple_regression(
    df = art_8_spline_df,
    depvar = a8_pB_depvar,
    treat_categ = a8_tc,
    treat_name = "treat",
    cont_vars = a8_c5_cont
)
# Warning messages:
# 1: In x * ws^2 * cw * ro$residuals :
#  longer object length is not a multiple of shorter object length
# e mais 7 dessas

a8_pBc5_spls_m <- multe_spline_regression(
    df = art_8_spline_df,
    depvar = a8_pB_depvar,
    treat_categ = a8_tc,
    treat_name = "treat",
    cont_vars = a8_c5_cont
)
# Error in solve.default(He[idx1, idx1, drop = FALSE], He[idx1, -idx1, drop = FALSE]) : 
#  Lapack routine dgesv: system is exactly singular: U[1,1] = 0

#' 4- Saving Output and Cleaning memory
save(
    a8_pAc1_m, a8_pAc1_spls_m,
    a8_pBc1_m, a8_pBc1_spls_m,
    #a8_pAc5_m, a8_pAc5_spls_m,
    #a8_pBc5_m, a8_pBc5_spls_m,
    # Took c5 out as it drops the only additional variable
    file = "MulTE/Art_8.RData"
)

rm(
    a8_pAc1_m, a8_pAc1_spls_m,
    a8_pAc5_m, a8_pAc5_spls_m,
    a8_pBc1_m, a8_pBc1_spls_m,
    a8_pBc5_m, a8_pBc5_spls_m,
    art_8_df, art_8_spline_df,
    art_8_fp, art_8_cov_cont,
    a8_pA_depvar, a8_pB_depvar,
    a8_tc, a8_c1_cont, a8_c5_cont
)


###############################################################################
# Article 10 - Moral Incentives in Credit Card Debt Repayment -----------------
###############################################################################
#' Setup
art_10_fp <- "Data/10_T2_3_4_data_replication.dta"

#' Dependent variable, Treatment and Covariates
art_10_cov_cont <- c("age", "income", "creditlimit")
a10_dtc <- "delinquent ~ treat + female + muslim + as.factor(province) + sample_before + poor_credit_history + as.factor(wave) + " # treatment, categorical covs (tc)

#' Creating Splines
art_10_df <- clean_art10(art_10_fp)
art_10_spline_df <- create_splines(data = art_10_df, covariates = art_10_cov_cont)

#' Regressions
# Tables 2-4, column 3
# Simple regression
a10 <- lm(paste(a10_dtc, paste(art_10_cov_cont, collapse = " + "), collapse = " + "), data = art_10_spline_df)
a10_m <- multe::multe(a10, treatment = "treat")
#' Sample for efficient common weights is empty.
#' For variable as.factor(province) the following levels fail overlap: 3, 15, 17, 20, 31, 32 → dropped
#' The following variables have no within-treatment variation and are dropped:
#' as.factor(wave)2, as.factor(wave)3, as.factor(wave)4, as.factor(wave)5, as.factor(wave)6

# Regression with splines
a10_spls <- paste(get_variable_splines(data = art_10_spline_df, variable = art_10_cov_cont), collapse = " + ")
a10_spl <- lm(paste(a10_dtc, a10_spls, collapse = ""), data = art_10_spline_df)
a10_spl_m <- multe::multe(a10_spl, treatment = "treat")
# These columns are dropped due to collinearity: creditlimit_knot2
# Sample for efficient common weights is empty.
# For variable as.factor(province) the following levels fail overlap: 3, 15, 17, 20, 31, 32 → dropped
# The following variables have no within-treatment variation and are dropped:
# as.factor(wave)2, as.factor(wave)3, as.factor(wave)4, as.factor(wave)5, as.factor(wave)6

#' 4- Saving Output and Cleaning memory
save(a10_m, a10_spl_m, file = "MulTE/Art_10.RData")

# Remove used regressions to free up memory and moving on to next article
rm(a10, a10_m, a10_spls, a10_spl, a10_spl_m, art_10_df,
   art_10_spline_df, art_10_fp, art_10_cov_cont, a10_dtc)


###############################################################################
# Article 13 - Improving College Instruction through Incentives ---------------
###############################################################################

#' 1 - Setup
art_13_t2_fp <- "Data/13_T2_Anon_Data_clean.dta"
art_13_t3_4_fp <- "Data/13_T3_4_Anon_Data_clean_panel.dta"

art_13_cov_cont <- c("Age", "BaselineCredits")

#' 2 - Creating Splines
# Table 2 data
art_13_t2_df <- clean_art13(art_13_t2_fp, 0)
art_13_t2_spline_df <- create_splines(data = art_13_t2_df, covariates = art_13_cov_cont)
a13_t2_spl_df_c1234 <- subset(art_13_t2_spline_df, Valid == 1 & Demographics == 1)
a13_t2_spl_df_c6 <- subset(a13_t2_spl_df_c1234, Fall == 0)

# Tables 3 and 4 data
art_13_t3_4_df <- clean_art13(art_13_t3_4_fp, 1)
art_13_t3_4_df <- art_13_t3_4_df %>%
    mutate(TakeTest = ifelse(!is.na(Percentage) & Percentage > 0, 1, 0))
art_13_t3_4_spline_df <- create_splines(data = art_13_t3_4_df, covariates = art_13_cov_cont)

# Subsetting the dataframe for analyses in Tables 3 and 4
a13_t3_spl_df <- subset(art_13_t3_4_spline_df, Valid == 1 & Demographics == 1 & TakeTest == 1)
a13_t4_df_c2 <- subset(art_13_t3_4_spline_df, Demographics == 1)
a13_t4_df_c3 <- subset(art_13_t3_4_spline_df, Demographics == 1 & Valid == 1)
a13_t4_df_c5 <- subset(art_13_t3_4_spline_df, Demographics == 1 & Completion == 1)

#' 3 - Regressions

# Table 2 - Column 2
a13_t2_c2_tc <- "TreatmentGroup + as.factor(FullTime) + as.factor(CoReq) + as.factor(Fall) + as.factor(White) + as.factor(Male) + as.factor(LowDiscount) + as.factor(LowCommit) + as.factor(DepartmentCode) + "
a13_t2_c2_cont <- c("Age", "BaselineCredits")

a13_t2_c2_m <- multe_simple_regression(
    df = a13_t2_spl_df_c1234,
    depvar = "ZPercentage",
    treat_categ = a13_t2_c2_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t2_c2_cont,
    cluster = TRUE,
    cluster_var = a13_t2_spl_df_c1234$TeacherID, na_action = TRUE
) # Fail overlap, drop as.factor(CoReq)1, as.factor(Fall)1 as no variation
a13_t2_c2_spls_m <- multe_spline_regression(
    df = a13_t2_spl_df_c1234,
    depvar = "ZPercentage",
    treat_categ = a13_t2_c2_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t2_c2_cont,
    cluster = TRUE,
    cluster_var = a13_t2_spl_df_c1234$TeacherID, na_action = TRUE
) # Fail overlap, drop as.factor(CoReq)1, as.factor(Fall)1 as no within-treat variation

save(a13_t2_c2_m, a13_t2_c2_spls_m, file = "MulTE/Art_13_v2.RData")


# Table 2 - Column 4
a13_t2_c4_tc <- a13_t2_c2_tc
a13_t2_c4_cont <- a13_t2_c2_cont

a13_t2_c4_m <- multe_simple_regression(
    df = a13_t2_spl_df_c1234,
    depvar = "ZPercentage",
    treat_categ = a13_t2_c4_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t2_c4_cont,
    cluster = TRUE,
    cluster_var = a13_t2_spl_df_c1234$TeacherID, na_action = TRUE
)# Fail overlap, drop as.factor(CoReq)1, as.factor(Fall)1 as no variation
a13_t2_c4_spls_m <- multe_spline_regression(
    df = a13_t2_spl_df_c1234,
    depvar = "ZPercentage",
    treat_categ = a13_t2_c4_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t2_c4_cont,
    cluster = TRUE,
    cluster_var = a13_t2_spl_df_c1234$TeacherID, na_action = TRUE
) # Fail overlap, drop as.factor(CoReq)1, as.factor(Fall)1 as no within-treat variation


# Table 2 - Column 6
a13_t2_c6_tc <- "TreatmentGroup + as.factor(FullTime) + as.factor(CoReq) + as.factor(White) + as.factor(Male) + as.factor(LowDiscount) + as.factor(LowCommit) + as.factor(DepartmentCode) + "
a13_t2_c6_cont <- c("Age", "BaselineCredits")

a13_t2_c6_m <- multe_simple_regression(
    df = a13_t2_spl_df_c6,
    depvar = "ZPercentage",
    treat_categ = a13_t2_c6_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t2_c6_cont,
    cluster = TRUE,
    cluster_var = a13_t2_spl_df_c6$TeacherID, na_action = TRUE
) # Fail overlap, drop as.factor(CoReq)1, as.factor(LowCommit)1 as no variation
a13_t2_c6_spls_m <- multe_spline_regression(
    df = a13_t2_spl_df_c6,
    depvar = "ZPercentage",
    treat_categ = a13_t2_c6_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t2_c6_cont,
    cluster = TRUE,
    cluster_var = a13_t2_spl_df_c6$TeacherID, na_action = TRUE
) # Fail overlap, drop as.factor(CoReq)1, as.factor(LowCommit)1 as no within-treat variation


# Table 3 - Column 6
a13_t3_c6_tc <- "TreatmentGroup + as.factor(FullTime) + as.factor(CoReq) + as.factor(Fall) + as.factor(White) + as.factor(Male) + as.factor(LowDiscount) + as.factor(LowCommit) + as.factor(DepartmentCode) + "
a13_t3_c6_cont <- c("Age", "BaselineCredits")

a13_t3_c6_m <- multe_simple_regression(
    df = a13_t3_spl_df,
    depvar = "ZPercentage",
    treat_categ = a13_t3_c6_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t3_c6_cont,
    cluster = TRUE,
    cluster_var = a13_t3_spl_df$TeacherID
) # Fail overlap, drop as.factor(CoReq)1, as.factor(Fall)1
a13_t3_c6_spls_m <- multe_spline_regression(
    df = a13_t3_spl_df,
    depvar = "ZPercentage",
    treat_categ = a13_t3_c6_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t3_c6_cont,
    cluster = TRUE,
    cluster_var = a13_t3_spl_df$TeacherID
) #Fail overlap, drop as.factor(CoReq)1, as.factor(Fall)1


# Table 4 - Column 2
a13_t4_c2_tc <- "TreatmentGroup + as.factor(FullTime) + as.factor(CoReq) + as.factor(Fall) + as.factor(White) + as.factor(Male) + as.factor(LowDiscount) + as.factor(LowCommit) + as.factor(DepartmentCode) + "
a13_t4_c2_cont <- c("Age", "BaselineCredits")

a13_t4_c2_m <- multe_simple_regression(
    df = art_13_t4_df_c2,
    depvar = "ZGradePoint",
    treat_categ = a13_t4_c2_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t4_c2_cont,
    cluster = TRUE,
    cluster_var = art_13_t4_df_c2$TeacherID
) # Fail overlap, drop as.factor(CoReq)1, as.factor(Fall)1
a13_t4_c2_spls_m <- multe_spline_regression(
    df = art_13_t4_df_c2,
    depvar = "ZGradePoint",
    treat_categ = a13_t4_c2_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t4_c2_cont,
    cluster = TRUE,
    cluster_var = art_13_t4_df_c2$TeacherID
) # Fail overlap, drop as.factor(CoReq)1, as.factor(Fall)1


# Table 4 - Column 3
a13_t4_c3_tc <- a13_t4_c2_tc
a13_t4_c3_cont <- a13_t4_c2_cont

a13_t4_c3_m <- multe_simple_regression(
    df = art_13_t4_df_c3,
    depvar = "ZNonExamPct",
    treat_categ = a13_t4_c3_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t4_c3_cont,
    cluster = TRUE,
    cluster_var = art_13_t4_df_c3$TeacherID, na_action = TRUE
) # Fail overlap, drop as.factor(CoReq)1, as.factor(Fall)1
a13_t4_c3_spls_m <- multe_spline_regression(
    df = art_13_t4_df_c3,
    depvar = "ZNonExamPct",
    treat_categ = a13_t4_c3_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t4_c3_cont,
    cluster = TRUE,
    cluster_var = art_13_t4_df_c3$TeacherID, na_action = TRUE
) # Fail overlap, drop as.factor(CoReq)1, as.factor(Fall)1


# Table 4 - Column 5
a13_t4_c5_tc <- a13_t4_c2_tc
a13_t4_c5_cont <- a13_t4_c2_cont

a13_t4_c5_m <- multe_simple_regression(
    df = art_13_t4_df_c5,
    depvar = "ZGradePoint",
    treat_categ = a13_t4_c5_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t4_c5_cont,
    cluster = TRUE,
    cluster_var = art_13_t4_df_c5$TeacherID
) # Fail overlap, drop as.factor(CoReq)1, as.factor(Fall)1
a13_t4_c5_spls_m <- multe_spline_regression(
    df = art_13_t4_df_c5,
    depvar = "ZGradePoint",
    treat_categ = a13_t4_c5_tc,
    treat_name = "TreatmentGroup",
    cont_vars = a13_t4_c5_cont,
    cluster = TRUE,
    cluster_var = art_13_t4_df_c5$TeacherID
) # Fail overlap, drop as.factor(CoReq)1, as.factor(Fall)1


#' 4 - Saving Output and Cleaning memory
#dir.create("MulTE", showWarnings = FALSE, recursive = TRUE)
art_13_outfile <- "MulTE/Art_13.RData"

save(
    a13_t2_c2_m,
    a13_t2_c2_spls_m,
    a13_t2_c4_m,
    a13_t2_c4_spls_m,
    a13_t2_c6_m,
    a13_t2_c6_spls_m,
    a13_t3_c6_m,
    a13_t3_c6_spls_m,
    a13_t4_c2_m,
    a13_t4_c2_spls_m,
    a13_t4_c3_m,
    a13_t4_c3_spls_m,
    a13_t4_c5_m,
    a13_t4_c5_spls_m,
    file = art_13_outfile
)

rm(
    art_13_t2_fp,
    art_13_t3_4_fp,
    art_13_cov_cont,
    art_13_t2_df,
    art_13_t2_spline_df,
    art_13_t3_4_df,
    art_13_t3_4_spline_df,
    a13_t2_spl_df_c1234,
    a13_t2_spl_df_c6,
    a13_t3_spl_df,
    art_13_t4_df_c2,
    art_13_t4_df_c3,
    art_13_t4_df_c5,
    a13_t2_c2_tc,
    a13_t2_c2_cont,
    a13_t2_c4_tc,
    a13_t2_c4_cont,
    a13_t2_c6_tc,
    a13_t2_c6_cont,
    a13_t3_c6_tc,
    a13_t3_c6_cont,
    a13_t4_c2_tc,
    a13_t4_c2_cont,
    a13_t4_c3_tc,
    a13_t4_c3_cont,
    a13_t4_c5_tc,
    a13_t4_c5_cont,
    a13_t2_c1_m,
    a13_t2_c1_spls_m,
    a13_t2_c2_m,
    a13_t2_c2_spls_m,
    a13_t2_c3_m,
    a13_t2_c3_spls_m,
    a13_t2_c4_m,
    a13_t2_c4_spls_m,
    a13_t2_c6_m,
    a13_t2_c6_spls_m,
    a13_t3_c5_m,
    a13_t3_c5_spls_m,
    a13_t3_c6_m,
    a13_t3_c6_spls_m,
    a13_t4_c2_m,
    a13_t4_c2_spls_m,
    a13_t4_c3_m,
    a13_t4_c3_spls_m,
    a13_t4_c5_m,
    a13_t4_c5_spls_m,
    art_13_outfile
)


###############################################################################
# Article 14 - Experimental Evidence on the Economics of Rural Electrification
###############################################################################

#' 1 - Setup
art_14_fp <- "Data/14_T2_dados_tab_2.dta"

art_14_cov_cont <- c(
    "hhsize", "age", "chickens", "electrification",
    "population", "earn", "prop200m2")

#' 2 - Creating Splines
art_14_df <- clean_art14(art_14_fp)
art_14_spline_df <- create_splines(data = art_14_df, covariates = art_14_cov_cont)
a14_spl_df <- subset(art_14_spline_df, connected == 0)
a14_cont <- c("hhsize", "age", "chickens", "electrification", "population")
a14_cat <- paste("senior", "notfarmer", "bank", "employed", "busia", "funded", "market + ", sep = " + ")

#' 3 - Regressions
# Table 2 - Column 2
a14_tc <- paste("treat", "wall", "someschool", a14_cat, sep = " + ")

a14_m <- multe_simple_regression(
    df = a14_spl_df,
    depvar = "takeup",
    treat_categ = a14_tc,
    treat_name = "treat",
    cont_vars = a14_cont,
    cluster = TRUE,
    cluster_var = a14_spl_df$siteno, na_action = TRUE
)

a14_spls_m <- multe_spline_regression(
    df = a14_spl_df,
    depvar = "takeup",
    treat_categ = a14_tc,
    treat_name = "treat",
    cont_vars = a14_cont,
    cluster = TRUE,
    cluster_var = a14_spl_df$siteno, na_action = TRUE
)


#' Not doing columns 3-8 because the only difference in the regression is
#' the inclusion of an interaction term on the original regression, while
#' all solutions already do that to calculate the respective estimands.
#' Therefore, they are repeated results.
#' Only the first one is reported.

#' Covariates on (2)
#' categ: wall, someschool, senior, notfarmer, bank, employed, busia, funded, market
#' cont: hhsize, age, chickens, electrification, population
#' Interacted variables on other columns:
#' (3) wall, (4) someschool, (5) electrification, (6) prop200m2, (7) black, (8) earn
#' not in (2): prop200m2, black, earn → should I make these regressions as well?

#' 4 - Saving Output and Cleaning memory
save(a14_m, a14_spls_m, file = "MulTE/Art_14_Final.RData")

rm(a14_tc, a14_m, a14_spls_m, art_14_df, art_14_spline_df,
   a14_spl_df, art_14_fp, art_14_cov_cont, a14_cat, a14_cont)


###############################################################################
# Article 16 - Are Referees and Editors in Economics Gender Neutral? ----------
###############################################################################

#' 1 - Setup
a16_fp <- "Data/16_T6_data_clean.dta"
a16_covs_cont <- c("umodel", "uempirical", "upolicy",
                   "Field_frGender", "Field_frF_5yr",
                   "asum1", "asum2", "asum3", "asum4")

#' 2 - Creating Splines
a16_df <- clean_art16(a16_fp)
a16_spl_df <- create_splines(data = a16_df, covariates = a16_covs_cont)

field_fr_vars <- names(a16_spl_df)[str_starts(names(a16_spl_df), "fr_")]
two_digit_JEL_vars <- names(a16_spl_df)[str_starts(names(a16_spl_df), "JEL_fr")]


#' 3 - Regressions

# Column 2 → foco nas especificações com fields
a16_c2_treat_categ <- paste("Gender_Auth_2", "as.factor(Pubs35_l5yr)",
                            "as.factor(auth_count)",
                            paste(field_fr_vars, collapse = " + "),
                            "as.factor(journal_year_key) + ", sep = " + ")

a16_c2_m <- multe_simple_regression(
    df = a16_spl_df,
    depvar = "asinhGS_2019",
    treat_categ = a16_c2_treat_categ,
    treat_name = "Gender_Auth_2",
    cont_vars = c("Field_frGender", "Field_frF_5yr")
    # REMEMBER: heteroskedasticity-robust SEs
)
# Drop due to collinearity: fr_other;
# Fail overlap
# The following variables have no within-treatment variation and are dropped:
# as.factor(Pubs35_l5yr)2, as.factor(Pubs35_l5yr)3, as.factor(Pubs35_l5yr)4,
# as.factor(Pubs35_l5yr)6, as.factor(auth_count)2, as.factor(auth_count)3,
#  as.factor(auth_count)4, fr_micro, fr_theory, fr_metrics, fr_macro,
# fr_internat,fr_fin, fr_pub, fr_labor, fr_healthurblaw, fr_hist, fr_io,
# fr_dev, fr_lab, Field_frGender, Field_frF_5yr

a16_c2_spls_m <- multe_spline_regression(
    df = a16_spl_df,
    depvar = "asinhGS_2019",
    treat_categ = a16_c2_treat_categ,
    treat_name = "Gender_Auth_2",
    cont_vars = c("Field_frGender", "Field_frF_5yr")
    # REMEMBER: heteroskedasticity-robust SEs
)
# Drop due to collinearity: fr_other, Field_frGender_knot1, Field_frGender_knot2
# Fail overlap
# The following variables have no within-treatment variation and are dropped:
# as.factor(Pubs35_l5yr)2, as.factor(Pubs35_l5yr)3, as.factor(Pubs35_l5yr)4,
# as.factor(Pubs35_l5yr)6, as.factor(auth_count)2, as.factor(auth_count)3,
# as.factor(auth_count)4, fr_micro, fr_theory, fr_metrics, fr_macro,
# fr_internat, fr_fin, fr_pub, fr_labor, fr_healthurblaw, fr_hist, fr_io,
# fr_dev, fr_lab, Field_frGender_pow1, Field_frGender_pow2, Field_frGender_pow3,
# Field_frF_5yr_pow1, Field_frF_5yr_pow2, Field_frF_5yr_pow3, Field_frF_5yr_knot1, Field_frF_5yr_knot2

# Ou seja, saíram todas as potências das splines, e praticamente todos os knots


# a partir daqui, chance de ter mais colunas que linhas
# Column 3
a16_c3_treat_categ <- paste("Gender_Auth_2", "as.factor(Pubs35_l5yr)",
                            "as.factor(auth_count)",
                            paste(two_digit_JEL_vars, collapse = " + "),
                            "as.factor(journal_year_key) + ", sep = " + ")

a16_c3_m <- multe_simple_regression(
    df = a16_spl_df,
    depvar = "asinhGS_2019",
    treat_categ = a16_c3_treat_categ,
    treat_name = "Gender_Auth_2",
    cont_vars = c("Field_frGender", "Field_frF_5yr")
    # REMEMBER: heteroskedasticity-robust SEs
)
# Error in solve.default(He[idx1, idx1, drop = FALSE], He[idx1, -idx1, drop = FALSE]) : 
#  Lapack routine dgesv: system is exactly singular: U[2,2] = 0

a16_c3_spls_m <- multe_spline_regression(
    df = a16_spl_df,
    depvar = "asinhGS_2019",
    treat_categ = a16_c3_treat_categ,
    treat_name = "Gender_Auth_2",
    cont_vars = c("Field_frGender", "Field_frF_5yr")
    # REMEMBER: heteroskedasticity-robust SEs
)
# Error in solve.default(He[idx1, idx1, drop = FALSE], He[idx1, -idx1, drop = FALSE]) : 
#  Lapack routine dgesv: system is exactly singular: U[4,4] = 0

# Column 4
a16_c4_treat_categ <- paste("Gender_Auth_2", "as.factor(Pubs35_l5yr)",
                            "as.factor(auth_count)",
                            paste(two_digit_JEL_vars, collapse = " + "),
                            "as.factor(journal_year_key) + ", sep = " + ")

a16_c4_m <- multe_simple_regression(
    df = a16_spl_df,
    depvar = "asinhGS_2019",
    treat_categ = a16_c4_treat_categ,
    treat_name = "Gender_Auth_2",
    cont_vars = c("Field_frGender", "Field_frF_5yr", "asum1", "asum2", "asum3", "asum4")
    # REMEMBER: heteroskedasticity-robust SEs
)
# Error in solve.default(He[idx1, idx1, drop = FALSE], He[idx1, -idx1, drop = FALSE]) : 
#  Lapack routine dgesv: system is exactly singular: U[1,1] = 0

a16_c4_spls_m <- multe_spline_regression(
    df = a16_spl_df,
    depvar = "asinhGS_2019",
    treat_categ = a16_c4_treat_categ,
    treat_name = "Gender_Auth_2",
    cont_vars = c("Field_frGender", "Field_frF_5yr", "asum1", "asum2", "asum3", "asum4")
    # REMEMBER: heteroskedasticity-robust SEs
)
# Error in solve.default(He[idx1, idx1, drop = FALSE], He[idx1, -idx1, drop = FALSE]) : 
#  Lapack routine dgesv: system is exactly singular: U[1,1] = 0


# Column 5
a16_c5_treat_categ <- paste("Gender_Auth_2", "as.factor(Pubs35_l5yr)",
                            "as.factor(auth_count)", "utheorem2", "utheorem3",
                            "umath2", "umath3", "uesteq2", "uesteq3",
                            paste(two_digit_JEL_vars, collapse = " + "),
                            "as.factor(journal_year_key) + ", sep = " + ")

a16_c5_m <- multe_simple_regression(
    df = a16_spl_df,
    depvar = "asinhGS_2019",
    treat_categ = a16_c5_treat_categ,
    treat_name = "Gender_Auth_2",
    cont_vars = c("Field_frGender", "Field_frF_5yr", "umodel", "uempirical", "upolicy")
    # REMEMBER: heteroskedasticity-robust SEs
)
# Error in solve.default(He[idx1, idx1, drop = FALSE], He[idx1, -idx1, drop = FALSE]) : 
#  Lapack routine dgesv: system is exactly singular: U[1,1] = 0

a16_c5_spls_m <- multe_spline_regression(
    df = a16_spl_df,
    depvar = "asinhGS_2019",
    treat_categ = a16_c5_treat_categ,
    treat_name = "Gender_Auth_2",
    cont_vars = c("Field_frGender", "Field_frF_5yr", "umodel", "uempirical", "upolicy")
    # REMEMBER: heteroskedasticity-robust SEs
)
# Multiple columns dropped due to collinearity
# Sample for efficient common weights is empty.
# Failed overlap
# Multiple variables have no within-treatment variation, and were dropped.


# Column 6
a16_c6_treat_categ <- paste("Gender_Auth_2", "as.factor(Pubs35_l5yr)",
                            "as.factor(auth_count)", "utheorem2", "utheorem3",
                            "umath2", "umath3", "uesteq2", "uesteq3",
                            paste(two_digit_JEL_vars, collapse = " + "),
                            "as.factor(journal_year_key) + ", sep = " + ")

a16_c6_m <- multe_simple_regression(
    df = a16_spl_df,
    depvar = "asinhGS_2019",
    treat_categ = a16_c6_treat_categ,
    treat_name = "Gender_Auth_2",
    cont_vars = c("Field_frGender", "Field_frF_5yr", "umodel", 
                  "uempirical", "upolicy","asum1", "asum2", "asum3", "asum4")
    # REMEMBER: heteroskedasticity-robust SEs
)
# Error in solve.default(He[idx1, idx1, drop = FALSE], He[idx1, -idx1, drop = FALSE]) : 
#  Lapack routine dgesv: system is exactly singular: U[4,4] = 0

a16_c6_spls_m <- multe_spline_regression(
    df = a16_spl_df,
    depvar = "asinhGS_2019",
    treat_categ = a16_c6_treat_categ,
    treat_name = "Gender_Auth_2",
    cont_vars = c("Field_frGender", "Field_frF_5yr", "umodel",
                  "uempirical", "upolicy","asum1", "asum2", "asum3", "asum4")
    # REMEMBER: heteroskedasticity-robust SEs
)
# Error in solve.default(He[idx1, idx1, drop = FALSE], He[idx1, -idx1, drop = FALSE]) : 
#  Lapack routine dgesv: system is exactly singular: U[4,4] = 0


#' 4- Saving Output and Cleaning memory

#dir.create("MulTE", showWarnings = FALSE, recursive = TRUE)
art_16_outfile <- "MulTE/Art_16.RData"

save(
    a16_c2_m,
    a16_c2_spls_m,
    a16_c5_spls_m,
    file = art_16_outfile
)

rm(
    a16_df,
    a16_spl_df,
    a16_covs_cont,
    field_fr_vars,
    two_digit_JEL_vars,
    a16_c2_treat_categ,
    a16_c2_m,
    a16_c2_spls_m,
    a16_c3_treat_categ,
    a16_c3_m,
    a16_c3_spls_m,
    a16_c4_treat_categ,
    a16_c4_m,
    a16_c4_spls_m,
    a16_c5_treat_categ,
    a16_c5_m,
    a16_c5_spls_m,
    a16_c6_treat_categ,
    a16_c6_m,
    a16_c6_spls_m,
    a16_fp,
    art_16_outfile
)

###############################################################################
# Paul 1 - STAR ---------------------------------------------------------
###############################################################################

#' Not included in the analysis due to the regression
#' specification not having continuous covariates: y ~ treatment + FE_school


###############################################################################
# Paul 2 - DUFLO: Table 2A(1) --------------------------------------
###############################################################################
dt2 <- read_dta("Data/duflo.dta")
dt2$treatment <- (dt2$Uonly == 1) + 2 * (dt2$Honly == 1) +
    3 * (dt2$UH == 1)
dt2$treatment <- factor(dt2$treatment, levels=0:3,
                        labels=c("Control", "Educ.~subsity",
                                 "HIV education", "Both"))
dt2 <- dt2 %>% filter(sex == 2) #dt2 <- dt2[dt2$sex == "2 Female", ]
dt2$stratum <- factor(dt2$stratum)
r2 <- lm(dropout05v3 ~ treatment +  date05v3 + date07v2 + yrbirth_all +
             yrbirth_missing + schsize + stratum, data=dt2)
r2$clust <- factor(dt2$sch03v1)[-na.action(r2)]
a2 <- multe::multe(r2, treatment="treatment", cluster = r2$clust)
#write_rd_table(a2, col=2, file="output/duflo", digits=3)
# failed overlap; 
# The following variables have no within-treatment variation and are dropped:
# date05v3, date07v2, schsize

# Creating Splines
dt2_cont <- c("yrbirth_all", "schsize")
dt2_spls_df <- create_splines(data = dt2, covariates = dt2_cont)
#cont: yrbirth_all, schize

dt2_spls <- paste(get_variable_splines(data = dt2_spls_df, variable = dt2_cont), collapse = " + ")
dt2_formula <- paste("dropout05v3 ~ treatment +  date05v3 + date07v2 + yrbirth_missing + ",
                  dt2_spls, " + stratum", collapse = "")
r2_m <- lm(as.formula(dt2_formula), data=dt2_spls_df)
r2_m$clust <- factor(dt2_spls_df$sch03v1)[-na.action(r2_m)]
a2_spl <- multe::multe(r2_m, treatment="treatment", cluster = r2_m$clust)
# Drop due to collinearity: yrbirth_all_pow3
# Fail overlap
# The following variables have no within-treatment variation and are dropped:
# date05v3, date07v2, schsize_pow1, schsize_pow2, schsize_pow3, schsize_knot1, schsize_knot2
# => droppou toda schsize...., tal qual na regressão anterior!

save(a2, a2_spl, file = "MulTE/Duflo.RData")

rm(dt2, dt2_cont, dt2_spls_df, dt2_spls, dt2_formula,r2, r2_m,
    a2, a2_spl, dt2_outfile)

###############################################################################
# Paul 3 - FRYER-LEVITT --------------------------------------------
###############################################################################
fl <- multe::fl
r3 <- lm(std_iq_24~race+factor(age_24)+female+SES_quintile+
             factor(siblings)+family_structure+
             region + I(poly(fl$mom_age, 5))+mom_age_NA+
             I(poly(parent_score, 5))+parent_score_NA+
             birthweight+ days_premature + multiple_birth +
             factor(interviewer_ID_24), weight=W2C0, data=fl)
## Expect warning that LM test is sensitive to numerical tolerance
testthat::expect_warning(a3 <- multe::multe(r3, "race"))
#write_rd_table(a3, col=2, file="output/fryer_levitt", digits=3)
# These columns are dropped due to collinearity: multiple_birthNot ascertained
# overlap failed
# The following variables have no within-treatment variation and are dropped:
# factor(siblings)6, mom_age_NATRUE, days_premature56, days_premature70, multiple_birthHigher order

# factor: SES_quintile, family_structure, region, birthweight, multiple_birth

# Creating Splines

#fl_cont <- c("SES_quintile", paste0("mom_age_5_", 1:5), paste0("parent_score_5_", 1:5),
#             "birthweight", "days_premature")
fl_cont <- c("mom_age", "parent_score")
fl_spls_df <- create_splines(data = fl, covariates = fl_cont)
# cont: SES_quintile, mom_age_5, parent_score_5, birthweight, days_premature
#' eu acho que deveria incluir days_premature como uma variável contínua e não como as.factor
#' pois o factor tem 13 níveis; o problema é que a regressão original tem 12 dummies,
#' e colocar como variável contínua não leva ao mesmo resultado...
#' não vou colocar!


fl_spls <- paste(get_variable_splines(data = fl_spls_df, variable = fl_cont), collapse = " + ")
fl_formula <- paste("std_iq_24~race+factor(age_24)+female+SES_quintile+
            factor(siblings)+family_structure +
            region + mom_age_NA + parent_score_NA +
            birthweight+ days_premature + multiple_birth +
            factor(interviewer_ID_24) +", fl_spls, collapse = "")
r3_m <- lm(as.formula(fl_formula), weight=W2C0, data=fl_spls_df)
a3_spl <- multe::multe(r3_m, treatment="race")
# dropped due to collinearity: multiple_birthNot ascertained
# Fail overlap
# The following variables have no within-treatment variation and are dropped:
# factor(siblings)6, mom_age_NATRUE, days_premature56, days_premature70, multiple_birthHigher order

# Both methods (with and without splines) yield very similar estimates
# and both have the exact same number of variables and sample size

#' => controls: factor(age_24), female, SES_quintile, factor(siblings),
#' family_structure, region, mom_age_NA, parent_score_NA, birthweight,
#' days_premature, multiple_birth, factor(interviewer_ID_24);
#' continuous: 5x mom_age, 5x parent_score
#' spline: substitutes those 5 for 3 power e 2 knots => same number of variables!

save(a3, a3_spl, file = "MulTE/FL2.RData")

rm(fl, fl_cont, fl_spls_df, fl_spls, fl_formula, r3, a3_spl,
    r3_m, a3)


###############################################################################
# Paul 4 - RIM --------------------------------------------
###############################################################################
dt4 <- read_dta("Data/rim.dta")
## Group is cohort. Only 20 native americans, drop them
dt4 <- dt4 %>% filter(race != 4) #dt4 <- dt4[dt4$race != "Native American", ] => 4 == Native American
dt4$race <- droplevels(haven::as_factor(dt4$race))
r4 <- lm(awd_perf ~ race + female + factor(birth_year) + I(poly(tenure, 3)) +
             I(poly(L_cmpl_civ, 3)) + I(poly(L_arrest_total, 3)) +
             I(poly(L_trr_new, 3)) + factor(group),
         data=dt4[!is.na(dt4$L_cmpl_civ) & !is.na(dt4$tenure), ])
a4 <- multe::multe(r4, "race", cluster=r4$model$"factor(group)")
#write_rd_table(a4, col=2, file="output/rim", digits=3)

# Creating Splines
dt4 <- dt4[!is.na(dt4$L_cmpl_civ) & !is.na(dt4$tenure), ]

dt4_cont <- c("tenure", "L_cmpl_civ", "L_arrest_total", "L_trr_new")

dt4_spls_df <- create_splines(data = dt4, covariates = dt4_cont)

dt4_spls <- paste(get_variable_splines(data = dt4_spls_df, variable = dt4_cont), collapse = " + ")
dt4_formula <- paste("awd_perf ~ race + female + factor(birth_year) +
                     factor(group) + ", dt4_spls, collapse = "")
r4_m <- lm(as.formula(dt4_formula), data=dt4_spls_df)
a4_spl <- multe::multe(r4_m, treatment="race", cluster=r4_m$model$"factor(group)")

# dropped due to collinearity: factor(group)134, L_cmpl_civ_knot1, L_trr_new_knot1
# Fail overlap
# The following variables have no within-treatment variation and are dropped:
# factor(birth_year)1967, factor(birth_year)1969, factor(birth_year)1970, factor(birth_year)1972, factor(birth_year)1973, factor(birth_year)1976, factor(birth_year)1977, factor(birth_year)1978, factor(birth_year)1979, factor(birth_year)1982, factor(birth_year)1984, factor(birth_year)1985, factor(birth_year)1986, factor(birth_year)1987, factor(birth_year)1988, factor(birth_year)1989, tenure_knot1, L_cmpl_civ_pow3, L_cmpl_civ_knot2, L_trr_new_knot2

dt4_outfile <- "MulTE/Rim.RData"

save(a4, a4_spl, file = dt4_outfile)

rm(dt4, dt4_cont, dt4_spls_df, dt4_spls, dt4_formula, r4, a4_spl,
    dt4_outfile, r4_m, a4)

###############################################################################
# Paul 5 - BENHASSINE --------------------------------------------
###############################################################################

dt5 <- read_dta("Data/benhassine.dta")
dt5$schoolid <- factor(dt5$schoolid)
dt5$stratum <- factor(dt5$stratum)
dt5$treatment <- factor(dt5$treatment, levels=0:4, 
                        labels = c("Control", "LCT to fathers", "LCT to mothers", "CCTs to fathers", "CCTs to mothers"))
controls <- paste(c("bs_pchildren_enrolled", "bs_pchildren_enrolled_miss",
                    "age_baseline", "girl", "bs_inschool08",
                    "bs_neverenrolled08", "bs_inschool08_miss",
                    "bs_neverenrolled08_miss", "prel_elec", "prel_elec_miss",
                    "prel_inacc_winter", "prel_inacc_winter_miss",
                    "sampling_frame_problem"), collapse="+")
r5 <- lm(paste0("enroll_attend_May2010 ~ treatment +", controls, "+ stratum"),
         weights=weight_hh, data=dt5)

## bs_neverenrolled08_miss is NA because same as bs_inschool08_miss
r5$cluster <- dt5$schoolid[-attr(r5$model, "na.action")]
a5 <- multe::multe(r5, treatment="treatment", cluster = r5$cluster)
# Dropped due to collinearity: bs_neverenrolled08_miss
# Fail overlap
#write_rd_table(a5, col=2, file="output/benhassine", digits=3)


# Creating Splines
dt5_cont <- c("bs_pchildren_enrolled", "age_baseline", "bs_inschool08", "bs_neverenrolled08", "prel_elec", "prel_inacc_winter")
dt5_spls_df <- create_splines(data = dt5, covariates = dt5_cont)
dt5_spls <- paste(get_variable_splines(data = dt5_spls_df, variable = dt5_cont), collapse = " + ")

categs <- paste(c("bs_pchildren_enrolled_miss", "girl", "bs_inschool08_miss",
                    "bs_neverenrolled08_miss", "prel_elec_miss", 
                    "prel_inacc_winter_miss", "sampling_frame_problem"), collapse="+")

r5_formula <- paste0("enroll_attend_May2010 ~ treatment +", categs, " + ", dt5_spls, "+ stratum")

r5_m <- lm(as.formula(r5_formula), weights=weight_hh, data=dt5_spls_df)
r5_m$cluster <- dt5_spls_df$schoolid[-attr(r5_m$model, "na.action")]
a5_spl <- multe::multe(r5_m, treatment="treatment", cluster = r5_m$cluster)
# columns dropped due to collinearity:
# bs_neverenrolled08_miss, bs_pchildren_enrolled_knot2, bs_inschool08_pow2, bs_inschool08_pow3, bs_inschool08_knot1, bs_inschool08_knot2, bs_neverenrolled08_pow2, bs_neverenrolled08_pow3, bs_neverenrolled08_knot1, bs_neverenrolled08_knot2, prel_elec_pow2, prel_elec_pow3, prel_elec_knot1, prel_elec_knot2, prel_inacc_winter_pow2, prel_inacc_winter_pow3, prel_inacc_winter_knot1, prel_inacc_winter_knot2
# Fail overlap

save(a5, a5_spl, file = "MulTE/Benhassine.RData")

rm(dt5, dt5_cont, dt5_spls_df, dt5_spls, dt5_formula, r5, a5_spl,
    dt5_outfile, r5_m, a5, controls, categs, r5_formula)


###############################################################################
# Paul 6 - WEISBURST --------------------------------------------
###############################################################################

dt6 <- read_dta("Data/weisburst.dta")
dt6 <- dt6[dt6$prace==0, ]#"White", ]
dt6$beat <- factor(dt6$beat)
dt6$race <- factor(dt6$race, levels = 0:3, 
                   labels = c("White", "Black", "Hispanic", "Other"))
r6 <- lm(force_rate_x ~ race + pblack_x + phispanic_x + pother_x + aage_x +
             afemale_x + page_x + pfemale_x + ptrainee_x + pexperience_x +
             pexperience2_x + violent_x + property_x + drug_x + disorderly_x +
             prostitution_x + traffic_x + factor(halfyear) + beat,
         data=dt6)
a6 <- multe::multe(r6, treatment="race", cluster = r6$model$beat)
#write_rd_table(a6, col=1, file="output/weisburst", digits=3)

# Creating Splines
dt6_cont <- c("pblack_x", "phispanic_x", "pother_x", "aage_x",
             "afemale_x", "page_x", "pfemale_x", "ptrainee_x", "pexperience_x",
             "pexperience2_x", "violent_x", "property_x", "drug_x", "disorderly_x",
             "prostitution_x", "traffic_x")
dt6_spls_df <- create_splines(data = dt6, covariates = dt6_cont)
dt6_spls <- paste(get_variable_splines(data = dt6_spls_df, variable = dt6_cont), collapse = " + ")

dt6_categ <- paste(c("factor(halfyear)", "beat"), collapse = " + ")
r6_formula <- paste0("force_rate_x ~ race +", dt6_spls, " + ", dt6_categ)

r6_m <- lm(as.formula(r6_formula), data=dt6_spls_df)
a6_spl <- multe::multe(r6_m, treatment="race", cluster = r6_m$model$beat)
# columns dropped due to collinearity:
# pother_x_knot1, ptrainee_x_knot1, ptrainee_x_knot2, violent_x_knot1, property_x_knot1, drug_x_knot1, disorderly_x_knot1, prostitution_x_knot1, prostitution_x_knot2, traffic_x_knot1

save(a6, a6_spl, file = "MulTE/Weisburst.RData")

rm(dt6, dt6_cont, dt6_spls_df, dt6_spls, r6_formula, r6, a6_spl,
    r6_m, a6, dt6_categ)

###############################################################################
# Paul 7 - COLE --------------------------------------------
###############################################################################

#' Not included in the analysis due to the absence of continuous
#' covariates in the regression specification:
#' Y ~ treatment + Wave_Dummy + FE_Village

###############################################################################
# Paul 8 - DREXLER --------------------------------------------
###############################################################################

## Warning that factor levels are duplicated
#testthat::expect_warning(dt8 <- read_dta("Data/drexler.dta"))
dt8 <- read_dta("Data/drexler.dta")
dt8$treatment <- factor(dt8$treatment, levels = 0:2,
                        labels = c("Control", "Standard Accounting", "Rule-of-Thumb"))
r8 <- lm(e_keepAcct ~ treatment +i_bus1 + i_bus2 + i_bus3 + i_bus4 +
             monto_dese + savings + b_keepAcct, data=dt8)
r8$cluster <- as.factor(dt8$barrio)[-attr(r8$model, "na.action")]
a8 <- multe::multe(r8, treatment="treatment", cluster = r8$cluster)
#write_rd_table(a8, col=1, file="output/drexler", digits=3)

# checking for continuous covariates:
for(var in c("i_bus1","i_bus2","i_bus3","i_bus4",
             "monto_dese", "savings", "b_keepAcct")){
    print(var)
    print(unique(dt8[[var]]))
}
# cont: monto_dese

# Creating Splines
dt8_cont <- c("monto_dese")
dt8_spls_df <- create_splines(data = dt8, covariates = dt8_cont)
dt8_spls <- paste(get_variable_splines(data = dt8_spls_df, variable = dt8_cont), collapse = " + ")

dt8_categ <- paste(c("i_bus1","i_bus2","i_bus3","i_bus4", "savings", "b_keepAcct"), collapse = " + ")
r8_formula <- paste0("e_keepAcct ~ treatment +", dt8_categ, " + ", dt8_spls)

r8_m <- lm(as.formula(r8_formula), data=dt8_spls_df)
r8_m$cluster <- as.factor(dt8$barrio)[-attr(r8_m$model, "na.action")]
a8_spl <- multe::multe(r8_m, treatment="treatment", cluster = r8_m$cluster)

save(a8, a8_spl, file = "MulTE/Drexler.RData")

rm(dt8, dt8_cont, dt8_spls_df, dt8_spls, r8_formula, r8, a8_spl,
    dt8_outfile, r8_m, a8, dt8_categ, var)


###############################################################################
# Paul 9 - DE MEL --------------------------------------------
###############################################################################

#' Not included in the analysis due to the absence of continuous
#' covariates in the regression specification:
#' Y ~ treatment + Strata

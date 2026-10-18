###############################################################################
# This code makes the tables and the figures for the article.
# Naming and numbering of variables in the code reflects previous iterations
# with more articles, hence the numbering is not sequential.
# Part of the analysis was adapted from the replicagion package of
# Goldsmith Pinkham, Hull and Kolesár (2024).
# This code was made with the help of GitHub Copilot.
###############################################################################
# Organize the working environment
###############################################################################

# Clean the working environment
rm(list = ls())
load.lib <- c("haven", "dplyr", "ggplot2", "stargazer", "multe", "tidyr", "stringr", "kableExtra")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)
remove(install.lib, lib, load.lib)

setwd("") # Insert you wd here

# Functions to load
source("Functions.R")
rm(clean_art3, clean_art8, clean_art10, clean_art13, clean_art14,
    create_splines, get_variable_splines, multe_simple_regression,
    multe_spline_regression, coeffs_table_spl_og)

# -----------------------------
# Load MulTE results
# -----------------------------

load("MulTE/Art_3.RData")
load("MulTE/Art_8.RData")
load("MulTE/Art_10.RData")
load("MulTE/Art_13_v2.RData")
load("MulTE/Art_14_Final.RData")
load("MulTE/Duflo.RData")
load("MulTE/FL2.RData")
load("MulTE/Rim.RData")
load("MulTE/Benhassine.RData")
load("MulTE/Weisburst.RData")
load("MulTE/Drexler.RData")


# -----------------------------
# Make Appendix Tables
# -----------------------------

# Tables A.2 - A.5; Dellavigna et al. (2022)
coeffs_table_spl(a3_c2_abs_m, a3_c2_abs_spls_m, file = "Output Tables/a3c2_abs")
coeffs_table_spl(a3_c2_log_m, a3_c2_log_spls_m, file = "Output Tables/a3c2_log")
coeffs_table_spl(a3_c3_abs_m, a3_c3_abs_spls_m, file = "Output Tables/a3c3_abs")
coeffs_table_spl(a3_c3_log_m, a3_c3_log_spls_m, file = "Output Tables/a3c3_log")

# Tables A.6, A.7 - Jha and Shayo (2019)
coeffs_table_spl(a8_pAc1_m, a8_pAc1_spls_m, file = "Output Tables/a8pAc1")
coeffs_table_spl(a8_pBc1_m, a8_pBc1_spls_m, file = "Output Tables/a8pBc1")

# Table A.8 - Bursztyn et al. (2019)
coeffs_table_spl(a10_m, a10_spl_m, file = "Output Tables/a10")

# Table A.9 - Brownback and Sadoff (2020)
coeffs_table_spl(a13_t2_c2_m, a13_t2_c2_spls_m, file = "Output Tables/a13t2c2")

# Table A.10 - Lee et al. (2020)
coeffs_table_spl(a14_m, a14_spls_m, file = "Output Tables/a14")

# Table A.11 - Drexler et al. (2014)
coeffs_table_spl(a8, a8_spl, file = "Output Tables/drexler")

# Table A.12 - Benhassine et al. (2015)
coeffs_table_spl(a5, a5_spl, file = "Output Tables/benhassine")

# Table A.13 - Duflo et al. (2015)
coeffs_table_spl(a2, a2_spl, file = "Output Tables/duflo")

# Table A.14 - Fryer and Levitt (2013)
coeffs_table_spl(a3, a3_spl, file = "Output Tables/fl2")

# Table A.15 - Rim et al. (2020)
coeffs_table_spl(a4, a4_spl, file = "Output Tables/rim2")


# Table 1 - Weisburst (2019)
coeffs_table_spl(a6, a6_spl, file = "Output Tables/weisburst")



# -----------------------------
# Estimates Manipulation
# -----------------------------

# Data Processing
# MulTE
df <- data.frame(reg=c("DellaVigna-C2a", "DellaVigna-C2l", "DellaVigna-C3a", "DellaVigna-C3l",
                         "Jha-Shayo-A", "Jha-Shayo-B", "Burzstyn",
                         "Sadoff", "Miguel", "Duflo", "Benhassine", "Drexler",
                         "Rim", "Fryer-Levitt", "Weisburst"),
                 type=c(rep("Experimental", 12), rep("Observational", 3)))

de <- lapply(list(a3_c2_abs_m, a3_c2_log_m, a3_c3_abs_m, a3_c3_log_m,
                  a8_pAc1_m, a8_pBc1_m, a10_m,
                  a13_t2_c2_m, a14_m, a2, a5, a8,
                  a4, a3, a6),
                  export_est_se)
for (j in seq_along(de)){
    # all levels get the de j-th row (paper and type)
    de[[j]][, c("paper", "type")] <- df[j, ]
}
dt_m <- do.call(rbind, de) # joins seperate rows into a single table

# Adjusts the name of the treatment levels
dt_m$x <- paste(dt_m$paper, dt_m$treatment, sep=": ")
dt_m$x <- factor(dt_m$x, levels=dt_m$x)
dt_m$paper <- factor(dt_m$paper, levels=unique(dt_m$paper))

# OLS t-stat
dt_m$t_stat_OLS <- dt_m$reg_est/dt_m$reg_se

# Normalization WITH sign trick
dt_m[, c(14:17)] <- sign(dt_m$reg_est)*dt_m[, c(2:5)] / dt_m$reg_se

# Normalization WITHOUT sign trick
# est / reg_se
dt_m[, c(18:21)] <- dt_m[, c(2:5)] / dt_m$reg_se

# SE Ratio = SE(spline)/SE(reg) + renaming + reorganizing
dt_m <- dt_m %>%
        mutate(reg_se_ratio = reg_se/reg_se, ate_se_ratio = ate_se/reg_se,
                ew_se_ratio = ew_se/reg_se, cw_se_ratio = cw_se/reg_se) %>%
        rename(
            reg_est_old = reg_est, ate_est_old = ate_est, ew_est_old = ew_est, cw_est_old = cw_est,
            reg_est_norm = reg_est.1, ate_est_norm = ate_est.1, ew_est_norm = ew_est.1, cw_est_norm = cw_est.1,
            reg_est_nosign = reg_est.2, ate_est_nosign = ate_est.2, ew_est_nosign = ew_est.2, cw_est_nosign = cw_est.2
        ) %>%
        relocate(treatment, 
                 reg_est_old, ate_est_old, ew_est_old, cw_est_old,
                 reg_est_norm, ate_est_norm, ew_est_norm, cw_est_norm,
                 reg_est_nosign, ate_est_nosign, ew_est_nosign, cw_est_nosign,
                 reg_se, ate_se, ew_se, cw_se,
                 reg_se_ratio, ate_se_ratio, ew_se_ratio, cw_se_ratio,
                 t_stat_OLS, paper, type, x)

# Pivot longer
d_m <- pivot_longer(dt_m, 
                    reg_est_old:cw_se_ratio, 
                    names_to = c("est_type", ".value"),
                    names_pattern = "^(.+?)_(est_old|est_norm|est_nosign|se_ratio|se)$")
# respective estimates in the same column
# est_old, est_norm, est_nosign, se, se_ratio

# MulTE + Spline
de_spl <- lapply(list(a3_c2_abs_spls_m, a3_c2_log_spls_m, a3_c3_abs_spls_m, a3_c3_log_spls_m,
                  a8_pAc1_spls_m, a8_pBc1_spls_m, a10_spl_m,
                  a13_t2_c2_spls_m, a14_spls_m, a2_spl, a5_spl, a8_spl,
                  a4_spl, a3_spl, a6_spl),
                  export_est_se)
for (j in seq_along(de_spl)){
    de_spl[[j]][, c("paper", "type")] <- df[j, ] # all levels get the de j-th row (paper and type)
}
dt_spl <- do.call(rbind, de_spl) # joins seperate rows into a single table

# Adjusts the name of the treatment levels
dt_spl$x <- paste(dt_spl$paper, dt_spl$treatment, sep=": ")
dt_spl$x <- factor(dt_spl$x, levels=dt_spl$x)
dt_spl$paper <- factor(dt_spl$paper, levels=unique(dt_spl$paper))

# Normalization WITH sign trick
dt_spl[, c(13:16)] <- sign(dt_m$reg_est_old)*dt_spl[, c(2:5)] / dt_m$reg_se

# Normalization WITHOUT sign trick
# est / reg_se
dt_spl[, c(17:20)] <- dt_spl[, c(2:5)] / dt_m$reg_se

# SE Ratio = SE(spline)/SE(reg) + renaming + reorganizing
dt_spl <- dt_spl %>%
    mutate(reg_se_ratio = reg_se/reg_se, ate_se_ratio = ate_se/reg_se,
           ew_se_ratio = ew_se/reg_se, cw_se_ratio = cw_se/reg_se) %>%
    rename(
        reg_est_old = reg_est, ate_est_old = ate_est, ew_est_old = ew_est, cw_est_old = cw_est,
        reg_est_norm = reg_est.1, ate_est_norm = ate_est.1, ew_est_norm = ew_est.1, cw_est_norm = cw_est.1,
        reg_est_nosign = reg_est.2, ate_est_nosign = ate_est.2, ew_est_nosign = ew_est.2, cw_est_nosign = cw_est.2
    ) %>%
    relocate(treatment, 
             reg_est_old, ate_est_old, ew_est_old, cw_est_old,
             reg_est_norm, ate_est_norm, ew_est_norm, cw_est_norm,
             reg_est_nosign, ate_est_nosign, ew_est_nosign, cw_est_nosign,
             reg_se, ate_se, ew_se, cw_se,
             reg_se_ratio, ate_se_ratio, ew_se_ratio, cw_se_ratio,
             paper, type, x)

# Pivot longer
d_spl <- pivot_longer(dt_spl, 
                    reg_est_old:cw_se_ratio, 
                    names_to = c("est_type", ".value"),
                    names_pattern = "^(.+?)_(est_old|est_norm|est_nosign|se_ratio|se)$")
# respective estimates in the same column
# est_old, est_norm, est_nosign, se, se_ratio

rm(a10_m, a10_spl_m, a13_t2_c1_m, a13_t2_c1_spls_m, a13_t2_c2_m, a13_t2_c2_spls_m, a13_t2_c3_m, a13_t2_c3_spls_m,
   a13_t2_c4_m, a13_t2_c4_spls_m, a13_t2_c6_m, a13_t2_c6_spls_m, a13_t3_c5_m, a13_t3_c5_spls_m,
   a13_t3_c6_m, a13_t3_c6_spls_m, a13_t4_c2_m, a13_t4_c2_spls_m, a13_t4_c3_m, a13_t4_c3_spls_m,
   a13_t4_c5_m, a13_t4_c5_spls_m, a14_m, a14_spls_m,
   a3_c2_abs_m, a3_c2_abs_spls_m, a3_c2_log_m, a3_c2_log_spls_m, a3_c3_abs_m, a3_c3_abs_spls_m, a3_c2_log_m, a3_c2_log_spls_m,
   a8_pAc1_m, a8_pAc1_spls_m, a8_pBc1_m, a8_pBc1_spls_m, a2, a2_spl, a5, a5_spl, a8, a8_spl, a16_c2_m, a16_c2_spls_m, a4, a4_spl, a3, a3_spl, a6, a6_spl,
   a16_c5_spls_m, a3_c3_log_m, a3_c3_log_spls_m, a8_pAc5_m, a8_pBc5_m,
   j, de, de_spl, df)

save(d_m, d_spl, file = "MulTE/Compiled_Regression_Results_SE_3.RData")

# Removing "reg" estimates as they are not used
d_m <- d_m %>% filter(est_type != "reg")
d_spl <- d_spl %>% filter(est_type != "reg")

# Uniting the dataframes
d2 <- left_join(d_m, d_spl, by = c("treatment", "paper", "type", "x", "est_type"))
d2 <- d2 %>% rename(est_old_without = est_old.x, est_old_spline = est_old.y,
                    est_norm_without = est_norm.x, est_norm_spline = est_norm.y,
                    est_nosign_without = est_nosign.x, est_nosign_spline = est_nosign.y,
                    se_without = se.x, se_spline = se.y,
                    se_ratio_without = se_ratio.x, se_ratio_spline = se_ratio.y)
d2$norm_dif <- d2$est_norm_spline - d2$est_norm_without

d_m$spline <- 0
d_spl$spline <- 1
d_spl$t_stat_OLS <- d_m$t_stat_OLS
d <- rbind(d_m, d_spl)

# dropping extreme values for better visualization
d$idx <- seq_len(nrow(d))
# Outliers: Cash Rebate, ate_est; Religious Placebo, ate_est; Moral Incentive, ate_est;
# Removing:           196; 205; 193;
# and the corresponding spline-less:  52;  61; 49;
d <- d[-c(49, 52, 61, 193, 196, 205), ]
d2 <- d2[-c(49, 52, 61), ]



# -------------------------------------
# Figures and Tables
# -------------------------------------

### Figure 1a
# histogram of TE differences
diff_hist <- ggplot(d2, aes(x = norm_dif)) +
    geom_histogram(bins = 30, color = "black", alpha = 0.7) +
    labs(x = "Difference of Normalized Treatment Effect Estimates",
         y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

ggsave(diff_hist, filename = "Output Images/hist_differences.png", width = 6, height = 4)

### Figure 1b
# Zooming in
d2_zoom <- d2 %>% filter(norm_dif >= -5 & norm_dif <= 5)

# Zoom in no histograma da diferença
diff_hist_zoom <- ggplot(d2_zoom, aes(x = norm_dif)) +
    geom_histogram(bins = 30, color = "black", alpha = 0.7) +
    labs(x = "Difference of Normalized Treatment Effect Estimates",
         y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

ggsave(diff_hist_zoom, filename = "Output Images/hist_differences_zoom.png", width = 6, height = 4)

### Main text difference benchmarks
# Benchmarks for substantial differences between
# linear parametric and semiparatric splines estimates
# Benchmark 1: difference > 0.2 OLS SD
sum(abs(d2$norm_dif) > 0.2) # 42/141

# Benchmark 2: difference > 95th percentile of N(0,1)
sum(abs(d2$norm_dif) > 1.645) # 11/141

### Table 2 - quantile table
# The resulting table was later manually edited.
# For example, 0% and 100% were substituted for "Min" and "Max", respectively.
# Also, "Yes" and "No" were substituted for \checkmark and \xmark, respectively.
probs <- c(0, 0.25, 0.5, 0.75, 0.80, 0.85, 0.9, 0.95, 1)
quantile(d2$se_ratio_without, probs = probs)
quantile(d2$se_ratio_spline, probs = probs)

ate_spl <- quantile(subset(d2, est_type == "ate")$se_ratio_spline, probs = probs)
ate_sem <- quantile(subset(d2, est_type == "ate")$se_ratio_without, probs = probs)
ew_spl <- quantile(subset(d2, est_type == "ew")$se_ratio_spline, probs = probs)
ew_sem <- quantile(subset(d2, est_type == "ew")$se_ratio_without, probs = probs)
cw_spl <- quantile(subset(d2, est_type == "cw")$se_ratio_spline, probs = probs)
cw_sem <- quantile(subset(d2, est_type == "cw")$se_ratio_without, probs = probs)

quantiles_df <- data.frame(
    Estimator = c("ATE", "ATE", "EW", "EW", "CW", "CW"),
    Spline = rep(c("Yes", "No"), 3),
    rbind(ate_spl, ate_sem, ew_spl, ew_sem, cw_spl, cw_sem),
    row.names = NULL
)

# Round for nicer display
quantiles_df[, -c(1, 2)] <- round(quantiles_df[, -c(1, 2)], 3)

# Fix column names: convert X0. to 0%, X25. to 25%, etc.
colnames(quantiles_df)[-(1:2)] <- paste0(probs * 100, "%")

# Create and save the table
latex_output <- kable(quantiles_df, 
                      format = "latex",
                      caption = "SE ratio Quantile by Estimator and Spline Presence",
                      label = "quantiles",
                      booktabs = TRUE,
                      align = c("c", "c", rep("c", length(probs)))) %>%
    collapse_rows(columns = 1, valign = "middle") %>%
    kable_styling(latex_options = "hold_position")

# Post-process to remove \centering\arraybackslash
latex_text <- as.character(latex_output)
latex_text <- gsub("\\\\centering\\\\arraybackslash ", "", latex_text)

writeLines(latex_text, "Output Tables/SE_ratio_quantiles.tex")
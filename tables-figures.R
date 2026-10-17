###############################################################################
# Topic: Scientific Iniciation in Econometrics
# Advisor: Vítor Augusto Possebom
# Autor: Michel Csillag Finger
# Goal: Make tables and figures
###############################################################################
# Organize the working environment
###############################################################################

# Clean the working environment
rm(list = ls())
load.lib <- c("haven", "dplyr", "ggplot2", "stargazer", "multe", "tidyr", "stringr")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
sapply(load.lib, require, character=TRUE)
remove(install.lib, lib, load.lib)

setwd("/home/micsifin/transfer/Estudos/FGV-EESP/IC/Codigo")

# Functions to load
source("Functions.R")

# -----------------------------
# Load MulTE results
# -----------------------------

load("MulTE/Art_3.RData")
load("MulTE/Art_8.RData")
load("MulTE/Art_10.RData")
load("MulTE/Art_13_v2.RData")
#load("MulTE/Art_14.RData")
load("MulTE/Art_14_Final.RData")
#load("MulTE/Art_16.RData") -> removing Card from the analysis due to over-extrapolation concerns
load("MulTE/Duflo.RData")
#load("MulTE/FL.RData")
load("MulTE/FL2.RData")
#load("MulTE/Rim.RData")
load("MulTE/Rim2.RData")
load("MulTE/Benhassine.RData")
load("MulTE/Weisburst.RData")
load("MulTE/Drexler.RData")


# -----------------------------
# Make Tables
# -----------------------------

# Article 3
coeffs_table_spl(a3_c2_abs_m, a3_c2_abs_spls_m, file = "Output Tables/a3c2_abs")
coeffs_table_spl(a3_c2_log_m, a3_c2_log_spls_m, file = "Output Tables/a3c2_log")
coeffs_table_spl(a3_c3_abs_m, a3_c3_abs_spls_m, file = "Output Tables/a3c3_abs")
coeffs_table_spl(a3_c3_log_m, a3_c3_log_spls_m, file = "Output Tables/a3c3_log")

# Article 8
coeffs_table_spl(a8_pAc1_m, a8_pAc1_spls_m, file = "Output Tables/a8pAc1")
coeffs_table_spl(a8_pBc1_m, a8_pBc1_spls_m, file = "Output Tables/a8pBc1")
#coeffs_table(a8_pBc5_m, a8_pBc5_spls_m, file = "Output Tables/Art8_pBc5")
#coeffs_table(a8_pAc5_m, a8_pAc5_spls_m, file = "Output Tables/Art8_pAc5")
# faltou coluna 5

# Article 10
coeffs_table_spl(a10_m, a10_spl_m, file = "Output Tables/a10")

# Article 13
#coeffs_table_spl(a13_t2_c1_m, a13_t2_c1_spls_m, file = "Output Tables/a13t2c1") # _spls é NULL
coeffs_table_spl(a13_t2_c2_m, a13_t2_c2_spls_m, file = "Output Tables/a13t2c2")
#coeffs_table_spl(a13_t2_c3_m, a13_t2_c3_spls_m, file = "Output Tables/a13t2c3") # _spls é NULL
#coeffs_table_spl(a13_t2_c4_m, a13_t2_c4_spls_m, file = "Output Tables/a13t2c4")
#coeffs_table_spl(a13_t2_c6_m, a13_t2_c6_spls_m, file = "Output Tables/a13t2c6")
#coeffs_table_spl(a13_t3_c5_m, a13_t3_c5_spls_m, file = "Output Tables/a13t3c5") # _spls é NULL
#coeffs_table_spl(a13_t3_c6_m, a13_t3_c6_spls_m, file = "Output Tables/a13t3c6")
#coeffs_table_spl(a13_t4_c2_m, a13_t4_c2_spls_m, file = "Output Tables/a13t4c2")
#coeffs_table_spl(a13_t4_c3_m, a13_t4_c3_spls_m, file = "Output Tables/a13t4c3")
#coeffs_table_spl(a13_t4_c5_m, a13_t4_c5_spls_m, file = "Output Tables/a13t4c5")

# Article 14
coeffs_table_spl(a14_m, a14_spls_m, file = "Output Tables/a14")

# Article 16
#coeffs_table_spl(a16_c2_m, a16_c2_spls_m, file = "Output Tables/a16c2")

# Duflo
coeffs_table_spl(a2, a2_spl, file = "Output Tables/duflo")

# FL
#coeffs_table_spl(a3, a3_spl, file = "Output Tables/fl")
coeffs_table_spl(a3, a3_spl, file = "Output Tables/fl2")

# Rim
#coeffs_table_spl(a4, a4_spl, file = "Output Tables/rim")
coeffs_table_spl(a4, a4_spl, file = "Output Tables/rim2")

# Benhassine
coeffs_table_spl(a5, a5_spl, file = "Output Tables/benhassine")

# Weisburst
coeffs_table_spl(a6, a6_spl, file = "Output Tables/weisburst")

# Drexler
coeffs_table_spl(a8, a8_spl, file = "Output Tables/drexler")


# -----------------------------
# Data Manipulation
# -----------------------------

#' Sadoff's com NULL:
#' a13_t2_c1_spls_m
#' a13_t2_c3_spls_m
#' a13_t2_c5_spls_m
#' Após reunião 13/04, tiramos Card et al. da análise

# Data Processing
# MulTE
df <- data.frame(reg=c("DellaVigna-C2a", "DellaVigna-C2l", "DellaVigna-C3a", "DellaVigna-C3l",
                         "Jha-Shayo-A", "Jha-Shayo-B", "Burzstyn",
                         "Sadoff", # "Sadoff-T2C4", "Sadoff-T2C6", "Sadoff-T3C6", "Sadoff-T4C2", "Sadoff-T4C3", "Sadoff-T4C5",
                         "Miguel",
                         "Duflo", "Benhassine", "Drexler",
                         #"Card",
                         "Rim", "Fryer-Levitt", "Weisburst"),
                 type=c(rep("Experimental", 12), rep("Observational", 3)))

# faltou A8-pAc5 e A8-pBc5 e a16-c5

de <- lapply(list(a3_c2_abs_m, a3_c2_log_m, a3_c3_abs_m, a3_c3_log_m,
                  a8_pAc1_m, a8_pBc1_m, a10_m,
                  #a13_t2_c1_m, a13_t2_c2_m, a13_t2_c3_m, a13_t2_c4_m, a13_t2_c6_m, a13_t3_c5_m,
                  a13_t2_c2_m, #a13_t2_c4_m, a13_t2_c6_m, a13_t3_c6_m, a13_t4_c2_m, a13_t4_c3_m, a13_t4_c5_m,
                  #a14_t2_c2_m, a14_t2_c3_m, a14_t2_c4_m, a14_t2_c5_m, a14_t2_c6_m, a14_t2_c7_m, a14_t2_c8_m,
                  a14_m, a2, a5, a8,
                  #a16_c2_m, 
                  a4, a3, a6),
                  export_est_se)
for (j in seq_along(de)){
    de[[j]][, c("paper", "type")] <- df[j, ] # todos níveis ganham j-ésima linha dp (paper e tipo)
}
dt_m <- do.call(rbind, de) # junta as linhas separadas numa única tabela

# ajuste para nome dos níveis de tratamento
dt_m$x <- paste(dt_m$paper, dt_m$treatment, sep=": ")
dt_m$x <- factor(dt_m$x, levels=dt_m$x)
dt_m$paper <- factor(dt_m$paper, levels=unique(dt_m$paper))

# t-estat OLS
dt_m$t_stat_OLS <- dt_m$reg_est/dt_m$reg_se

# Normalização COM truque de sinal
#dt_m[, c(2:5)] <- sign(dt_m$reg_est)*dt_m[, c(2:5)] / dt_m$reg_se # a tal da normalização
# sinal(reg) * est / reg_se
dt_m[, c(14:17)] <- sign(dt_m$reg_est)*dt_m[, c(2:5)] / dt_m$reg_se

# Normalização SEM truque de sinal
# est / reg_se
dt_m[, c(18:21)] <- dt_m[, c(2:5)] / dt_m$reg_se

# pivot longer (antigo)
#d_m <- pivot_longer(dt_m, reg_est:cw_est, names_to = "est_type",
#                  values_to = "y") # todas estimativas numa única coluna → para fazer histograma

# Razão SE = SE(spline)/SE(reg) + renomeando + reorganizando
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
#d_m <- pivot_longer(dt_m, reg_est:cw_se_ratio, names_to = c("est_type", ".value"),
#                     names_pattern = "(.+?)_(est|se_ratio|se)") 
d_m <- pivot_longer(dt_m, 
                    reg_est_old:cw_se_ratio, 
                    names_to = c("est_type", ".value"),
                    names_pattern = "^(.+?)_(est_old|est_norm|est_nosign|se_ratio|se)$")
# respectivas estimativas numa mesma coluna
# est_old, est_norm, est_nosign, se, se_ratio

# MulTE + Spline
de_spl <- lapply(list(a3_c2_abs_spls_m, a3_c2_log_spls_m, a3_c3_abs_spls_m, a3_c3_log_spls_m,
                  a8_pAc1_spls_m, a8_pBc1_spls_m, a10_spl_m,
                  #a13_t2_c1_spls_m, a13_t2_c2_spls_m, a13_t2_c3_spls_m, a13_t2_c4_spls_m, a13_t2_c6_spls_m, a13_t3_c5_spls_m,
                  a13_t2_c2_spls_m, #a13_t2_c4_spls_m, a13_t2_c6_spls_m, a13_t3_c6_spls_m, a13_t4_c2_spls_m, a13_t4_c3_spls_m, a13_t4_c5_spls_m,
                  #a14_t2_c2_spls_m, a14_t2_c3_spls_m, a14_t2_c4_spls_m, a14_t2_c5_spls_m, a14_t2_c6_spls_m, a14_t2_c7_spls_m, a14_t2_c8_spls_m,
                  a14_spls_m,
                  a2_spl, a5_spl, a8_spl,
                  #a16_c2_spls_m,
                  a4_spl, a3_spl, a6_spl),
                  export_est_se)
for (j in seq_along(de_spl)){
    de_spl[[j]][, c("paper", "type")] <- df[j, ] # todos níveis ganham j-ésima linha dp (paper e tipo)
}
dt_spl <- do.call(rbind, de_spl) # junta as linhas separadas numa única tabela

# ajuste para nome dos níveis de tratamento
dt_spl$x <- paste(dt_spl$paper, dt_spl$treatment, sep=": ")
dt_spl$x <- factor(dt_spl$x, levels=dt_spl$x)
dt_spl$paper <- factor(dt_spl$paper, levels=unique(dt_spl$paper))

# t-estat OLS → seria do OLS com spline: não é útil

# Normalização COM truque de sinal
#dt_spl[, c(2:6)] <- sign(dt_m$reg_est)*dt_spl[, c(2:6)] / dt_m$reg_se # a tal da normalização
# sinal(reg) * est / reg_se
dt_spl[, c(13:16)] <- sign(dt_m$reg_est_old)*dt_spl[, c(2:5)] / dt_m$reg_se

# Normalização SEM truque de sinal
# est / reg_se
dt_spl[, c(17:20)] <- dt_spl[, c(2:5)] / dt_m$reg_se

# pivot longer (antigo)
#d_m <- pivot_longer(dt_m, reg_est:cw_est, names_to = "est_type",
#                  values_to = "y") # todas estimativas numa única coluna → para fazer histograma

# Razão SE = SE(spline)/SE(reg) + renomeando + reorganizando
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
#d_m <- pivot_longer(dt_m, reg_est:cw_se_ratio, names_to = c("est_type", ".value"),
#                     names_pattern = "(.+?)_(est|se_ratio|se)") 
d_spl <- pivot_longer(dt_spl, 
                    reg_est_old:cw_se_ratio, 
                    names_to = c("est_type", ".value"),
                    names_pattern = "^(.+?)_(est_old|est_norm|est_nosign|se_ratio|se)$")
# respectivas estimativas numa mesma coluna
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


# -------------------------------------
# Data Analysis 1 - Understanding the estimates
# -------------------------------------
#' That is, the exploratory analysis:
#' comparing with x without spline estimates, boxplots for outliers, histogram of differences, etc.
# loading data
load("MulTE/Compiled_Regression_Results.RData")
load("MulTE/Compiled_Regression_Results_SE_2.RData")
load("MulTE/Compiled_Regression_Results_SE_3.RData")


# Removing "reg" estimates as they are not used
d_m <- d_m %>% filter(est_type != "reg")
d_spl <- d_spl %>% filter(est_type != "reg")

# Processamento para unir as bases
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


# Boxplot
boxplot_outlier <- ggplot(d, aes(x=factor(spline), y=est_norm, color=factor(spline))) +
    geom_boxplot() +
    scale_color_manual(values=c("0"="blue","1"="red"), name="spline") +
    labs(x="Spline", y="Estimativas Normalizadas", title = "Boxplot de estimativas normalizadas com outliers") +
    theme_minimal() # => há outliers

ggsave(boxplot_outlier, filename = "Output Images/boxplot_outlier_v2.png", width = 6, height = 4)

# dropping extreme values for better visualization
d$idx <- seq_len(nrow(d))
# Outliers: Cash Rebate, ate_est; Religious Placebo, ate_est; Moral Incentive, ate_est;
# Removendo os outliers:           196; 205; 193;
# e os correspondentes sem spline:  52;  61; 49;
# o jeito inteligente era fazer generalizado....
d <- d[-c(49, 52, 61, 193, 196, 205), ]
d2 <- d2[-c(49, 52, 61), ]


# Overlapping histograms
hist1 <- ggplot(d, aes(x=est_norm, fill=factor(spline))) +
     geom_histogram(position="identity", alpha=0.5, bins=30) +
     scale_fill_manual(values=c("0"="blue","1"="red"), name="spline") +
     labs(x="Estimativas Normalizadas", y="Contagem", title = "Histogramas de estimativas normalizadas") +
     theme_minimal()
ggsave(hist1, filename = "Output Images/histogram_junto_rev1.png", width = 6, height = 4)

# scatterplot
scatter <- ggplot(d2, aes(x = est_norm_without, y = est_norm_spline)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    #coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
    labs(x = "Sem Spline", y = "Com Spline",
        title = "Scatterplot de estimativas normalizadas") +
    theme_minimal()

ggsave(scatter, filename = "Output Images/scatterplot_unidos_rev2.png",
             width = 6, height = 4)


# gráficos da diferença com - sem spline
diff_plot <- ggplot(d2, aes(x = seq_len(nrow(d2)), y = dif)) +
    geom_point(alpha = 0.6, size = 2, color = "steelblue") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
    labs(x = "ID", y = "Diferença (Com - Sem Spline)",
        title = "Diferença de estimativas: Efeito da Spline") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(diff_plot, filename = "Output Images/scatter_difference_v3.png", width = 6, height = 4)

# histogram of differences
diff_hist <- ggplot(d2, aes(x = norm_dif)) +
    geom_histogram(bins = 30, color = "black", alpha = 0.7) +
    labs(x = "Difference of Normalized Treatment Effect Estimates",
         y = "Frequency",
         #title = "Distribuição do Efeito da Spline nas Estimativas"
         ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

ggsave(diff_hist, filename = "Output Images/hist_differences_rev5_EN.png", width = 6, height = 4)

# Zooming in
d2_zoom <- d2 %>% filter(norm_dif >= -5 & norm_dif <= 5)

# Zoom in no histograma da diferença
diff_hist_zoom <- ggplot(d2_zoom, aes(x = norm_dif)) +
    geom_histogram(bins = 30, color = "black", alpha = 0.7) +
    labs(x = "Difference of Normalized Treatment Effect Estimates",
         y = "Frequency",
         #title = "Zoom na Distribuição do Efeito da Spline nas Estimativas"
         ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

ggsave(diff_hist_zoom, filename = "Output Images/hist_differences_zoom_rev5_EN.png", width = 6, height = 4)

# Quantos valores mais extremos que |1.645|?
sum(abs(d2$norm_dif) > 0.2)
# >0.2: 42; >1: 13; > 1.645: 11; > 5: 7; total obs: 141



# Zoom in no scatterplot com spline x sem spline
d2_zoom2 <- d2 %>% filter(without >= -5 & without <= 5 & spline >= -5 & spline <= 5)
scatter_zoom <- ggplot(d2_zoom2, aes(x = without, y = spline)) +
    geom_point(alpha = 0.6, size = 2, color = "steelblue") +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    labs(
        x = "Sem Spline",
        y = "Com Spline",
        title = "Scatterplot de estimativas normalizadas (Zoom)"
    ) +
    theme_minimal()

ggsave(scatter_zoom, filename = "Output Images/scatterplot_zoom.png",
             width = 6, height = 4)


# -----------------------------
# Data Analysis 2 - Exploratory comparison of estimates (with x without splines)
# -----------------------------

# 2.1 - Comparing point estimates' variation
#' coluna "inclinação" = vetor de distância da origem no scatterplot
d2$slope <- d2$est_spline / d2$est_without
d2$slope <- d2$est_norm_spline / d2$est_norm_without

# inspeção visual da linha -1
scatter_plusminus1 <- ggplot(d2, aes(x = est_without, y = est_spline, color = est_type)) +
    #geom_point() +
    geom_point(aes(shape = est_type, color = est_type)) + #, size = 3)) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    geom_abline(intercept = 0, slope = -1, color = "purple") +
    coord_cartesian(xlim = c(-2, 4), ylim = c(-4, 4)) +
    labs(x = "Sem Spline", y = "Com Spline",
        title = "Scatterplot de estimativas normalizadas") +
    #geom_text(label=d2$x) +
    theme_minimal()
#' => é, parece muito...

ggsave(scatter_plusminus1, filename = "Output Images/scatter_plusminus1_shapes_rev2.png", width = 6, height = 4)


library(plotly)

#' Scatterplot interativo
plot_ly(data = d2, x = ~est_without, y = ~est_spline, text = ~x, type = 'scatter', mode = 'markers') %>%
    layout(title = "Scatterplot Interativo de Estimativas Normalizadas",
           xaxis = list(title = "Sem Spline"),
           yaxis = list(title = "Com Spline"))


library(latex2exp)
# Scatterplot razão SEs por estimativa spline
scatter_SE_spline_zoom <- ggplot(d2, aes(x = se_ratio_spline, y = est_spline, color = est_type)) +
    geom_point() +
    coord_cartesian(xlim = c(0, 10), ylim = c(-10, 10)) +
    labs(x = TeX("Razão SEs ($SE_{spl} / SE_{reg}$) "), 
         y = TeX("Estimativas Spline Normalizada ( $sign(reg)*est/SE_{reg}$ )"),
        title = "Scatterplot de estimativas normalizadas") +
    theme_minimal()

ggsave(scatter_SE_spline_zoom, filename = "Output Images/scatter_SE_spline_zoom_rev1.png", width = 6, height = 4)

cor.test(subset(d2$se_ratio_spline, d2$se_ratio_spline <= 11), subset(d2$est_spline, -10 <= d2$est_spline & d2$est_spline <= 10))
# -0.0361
# p-value: 0.6772

#' Hipótese era de que adicionar as splines aumentaria a incerteza, traduzida em maior SE
#' Portanto, fizemos o gráfico acima.
#' Curioso: não há a associação positiva esperada. Há variação, porém é uma linha vertical.
#' A correlação é de 3.6%, e é estatisticamente zero.
#' Por isso, passamos à análise da variabilidade abaixo.

# 2.2 - Checking if there is any association between the SE ratio and other variables so far
# workshop this title...

# Scatterplot razão SEs por estimativa SEM SPLINES
# Para ver se é comportamento dos estimadores ou das splines
ggplot(d2, aes(x = se_ratio_without, y = est_without, color = est_type)) +
    geom_point() +
    coord_cartesian(xlim = c(0, 20), ylim = c(-10, 20)) +
    labs(x = "Razão SEs", y = "Estimativas SEM Spline",
        title = "Scatterplot de estimativas normalizadas") +
    theme_minimal()
# => dos estimadores mesmo....

# Scatterplot razão SEs (sem) por razão SEs (spline)
scatter_SE_spl_yesno <- ggplot(d2, aes(x = se_ratio_without, y = se_ratio_spline, color = est_type)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color = "gray") +
    coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 5)) +
    labs(x = "Razão SEs sem", y = "Razão SEs Spline",
        title = "Scatterplot de estimativas normalizadas") +
    theme_minimal()

ggsave(scatter_SE_spl_yesno, filename = "Output Images/scatter_SE_spl_yesno_rev1.png", width = 6, height = 4)

# Scatterplot razão SEs por slope
scatter_SE_slope <- ggplot(d2, aes(x = se_ratio_spline, y = slope, color = est_type)) +
    geom_point() +
    coord_cartesian(xlim = c(0, 5), ylim = c(-5, 5)) +
#    coord_cartesian(xlim = c(0, 10), ylim = c(-10, 10)) +
    labs(x = "Razão SEs Spline", y = "slope",
        title = "Scatterplot de estimativas normalizadas") +
    theme_minimal()

ggsave(scatter_SE_slope, filename = "Output Images/scatter_SE_slope_rev1.png", width = 6, height = 4)

# Investigando a razão de SEs -----
# Histograma da razão SEs
ggplot(d, aes(x = se_ratio)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(x = "Razão de SEs (agregado)", y = "Frequência",
         title = "Distribuição da Razão de SEs") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

# Attempting a overlapping histogram (mostrar esse)
hist_se_ratio <- ggplot(d, aes(x=se_ratio, fill=factor(spline))) +
    geom_histogram(position="identity", alpha=0.5, bins=20) +
    scale_fill_manual(values=c("0"="blue","1"="red"), name="spline") +
    labs(x="Razão de SEs", y="Frequẽncia", title = "Histogramas da razão de SEs ( SE(est)/SE(reg) )") +
    theme_minimal()

ggsave(hist_se_ratio, filename = "Output Images/hist_SE_both_rev3.png", width = 6, height = 4)


library(patchwork)

# Histograma da razão SEs - só SPLINE
hist_se_ratio_spl <- ggplot(d2, aes(x = se_ratio_spline)) +
    geom_histogram(bins = 20, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(x = "Razão de SEs (SPLINE)", y = "Frequência",
         title = "Distribuição da Razão de SEs") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

ggsave(hist_se_ratio_spl, filename = "Output Images/hist_SE_spl_rev3.png", width = 6, height = 4)

# Média:
mean(d2$se_ratio_spline) # = 3.00779
# Desvio padrão:
sd(d2$se_ratio_spline) # = 9.112245
# quantis
quantile(d2$se_ratio_spline)
#        0%        25%        50%        75%       100% 
# 0.5948798  0.9630207  1.0004973  1.0550332 60.8521670
#' => metade da distribuição está a 0.05 unidades de SE do 1.
#' há pouca variação!

# Histograma da razão SEs - SEM
hist_se_ratio_sem <- ggplot(d2, aes(x = se_ratio_without)) +
    geom_histogram(bins = 10, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(x = "Razão de SEs (SEM)", y = "Frequência",
         title = "Distribuição da Razão de SEs") +
    theme_minimal() +
#    xlim(0, 61) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

ggsave(hist_se_ratio_sem, filename = "Output Images/hist_SE_sem_rev3.png", width = 6, height = 4)

# Combine with matched scales
hist_se_ratio_spl + hist_se_ratio_sem + plot_layout(ncol = 2)

# Média:
mean(d2$se_ratio_without) # = 1.08843
# Desvio padrão:
sd(d2$se_ratio_without) # = 0.8192165
# quantis
quantile(d2$se_ratio_without)
#        0%        25%        50%        75%       100% 
# 0.5957795  0.9579692  1.0012784  1.0170344 10.3743774
#' => muito próximo ao redor do 1 também

#' Quantile table
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

library(kableExtra)

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

# -----------------------------
# Data Analysis 3 - distance from original estimates
# -----------------------------

#' 3.1 Quanto aumentar SE aumenta prob sinal trocar?
#' Using dataframe d as it compares BOTH estimates (with and without splines)
#' to the original OLS results

#' Creating sign flip dummy
#' If the "normalized" estimate is negative, that means that there was a sign flip
d$sign_flip <- ifelse(d$est < 0, 1, 0)
d$sign_flip <- ifelse(d$est_norm < 0, 1, 0)

#' Regressão
prob_sign_flip <- lm(sign_flip ~ log(se_ratio) , data = d)
summary(prob_sign_flip)
#' Coefficient: se_ratio = 0.0004485; p = 0.914
#' aumento em 1 no se_ratio aumenta a prob de trocar sinal em 0.4%
#' Ou com log:
#' Coefficient: log(se_ratio) = 0.02; p = 0.58
#' aumento em 1% no se_ratio aumenta prob trocar sinal em 0.0002

# Dummy de spline passa a explicar tudo....
prob_sign_flip2 <- lm(sign_flip ~ se_ratio + spline, data = d)
summary(prob_sign_flip2)
#' Intercept  0.068; p = 0.04
#' se_ratio  -0.004; p = 0.2
#' spline     0.448; p < 2e-16


# Scatterplot
ggplot(d, aes(x = se_ratio, y = sign_flip, group = spline)) +
    geom_point(aes(color = as.factor(spline))) +
    geom_abline(intercept = coef(prob_sign_flip)[1], slope = coef(prob_sign_flip)[2], color = "blue") +
    geom_abline(intercept = coef(prob_sign_flip2)[1], slope = coef(prob_sign_flip2)[2], color = "red") +
    labs(x = "Razão SEs", y = "Sinal trocou?",
         title = "Scatterplot") +
    #geom_text(label=d2$x) +
    theme_minimal()


#' coluna sem correção de sinal
#' t-estat OLS original

#' |estimativa normalizada sem truqeu sinal - t-estat OLS| (eixo Y) x Razão SEs; linha de regressão do scatterplot

#' 2 análises:
#' uma vê se as splines deixam [O QUE?] longe das estimativas originais
#' a outra vê se as splines deixam [O QUE ?] longe das estimativas do Paul


# Comparação do porque resultados sumiram ------
d_n <- d
d2_n <- d2
save(d_og, d2_og, file = "MulTE/DEURUIM_og.RData")
save(d_n, d2_n, file = "MulTE/DEURUIM_n.RData")
load("MulTE/DEURUIM_og.RData")
load("MulTE/DEURUIM_n.RData")

aaa <- data.frame(aonde = d_n$x, type = d_n$est_type, spline = d_n$spline,
                  original = d_og$est, novo = d_n$est_norm)
aaa$dif <- ifelse(aaa$original == aaa$novo, 0, 1)
sum(aaa$dif) # 73 divergências
# muitos do Dellavigna, Bursztyn, Sadoff, Dulfo, Rim, Frier-Levitt
aaa$abs_dif <- ifelse(abs(aaa$original) == abs(aaa$novo), 0, 1)
sum(aaa$abs_dif) # 0 => logo, todas as divergências vieram de algum -1


ggplot(aaa, aes(x = original)) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(x = "Estimativas anteriores", y = "Frequência",
         title = "Distribuição do Efeito da Spline nas Estimativas") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))

ggplot(aaa, aes(x = original, y = novo)) +
    geom_point() +
    labs(x = "original", y = "novo",
         title = "Scatterplot") +
    theme_minimal()

s1 <- ifelse(abs(d2_n$est_norm_without) == abs(d2_n$est_nosign_without), 0, 1) # 73
s2 <- ifelse(abs(d2_n$est_norm_spline) == abs(d2_n$est_nosign_spline), 0, 1) # 141

d2_n$s1 <- s1
d2_n$s2 <- s2

dt_m_pre_norm <- dt_m
dt_m_pos_norm <- dt_m
dt_spl_pre_norm <- dt_spl
save(dt_m_pre_norm, dt_m_pos_norm, dt_spl_pre_norm, file = "MulTE/DEURUIM_dt.RData")
load("MulTE/DEURUIM_dt.RData")

bbb <- dt_spl_pre_norm[, 1:5]
bbb$sign <- sign(dt_m_pre_norm$reg_est)
bbb$reg_se <- dt_m_pre_norm$reg_se

bbb[, c(8:11)] <- sign(dt_m_pre_norm$reg_est)*dt_spl_pre_norm[, c(2:5)] / dt_m_pre_norm$reg_se
bbb[, c(2:5)] <- sign(dt_m_pre_norm$reg_est)*dt_spl_pre_norm[, c(2:5)] / dt_m_pre_norm$reg_se
bbb$ch_reg_est <- ifelse(bbb$reg_est == bbb$reg_est.1, 0, 1)
sum(bbb$ch_reg_est) # 0
bbb$ch_ate_est <- ifelse(bbb$ate_est == bbb$ate_est.1, 0, 1)
sum(bbb$ch_ate_est) # 0
bbb$ch_ew_est <- ifelse(bbb$ew_est == bbb$ew_est.1, 0, 1)
sum(bbb$ch_ew_est) # 0
bbb$ch_cw_est <- ifelse(bbb$cw_est == bbb$cw_est.1, 0, 1)
sum(bbb$ch_cw_est) # 0

# Normalização COM truque de sinal
#dt_spl[, c(2:6)] <- sign(dt_m$reg_est)*dt_spl[, c(2:6)] / dt_m$reg_se # a tal da normalização
# sinal(reg) * est / reg_se
dt_spl[, c(13:16)] <- sign(dt_m$reg_est_old)*dt_spl[, c(2:5)] / dt_m$reg_se

# Normalização SEM truque de sinal
# est / reg_se
dt_spl[, c(17:20)] <- dt_spl[, c(2:5)] / dt_spl$reg_se

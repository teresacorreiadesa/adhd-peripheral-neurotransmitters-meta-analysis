# =============================================================================
#  META-ANALYSIS - optimised
# =============================================================================
#
# PURPOSE: synthesise evidence on peripheral levels of catecholamines (DA, AD,
# NA), 5-HT, and their metabolites (DOPAC, HVA, 5-HIAA) in individuals with
# ADHD compared to typically developing controls, and to examine the extent
# and sources of heterogeneity across studies.
#
# RESOURCES:
# - https://www.cochrane.org/authors/handbooks-and-manuals/handbook/current/chapter-10
# - https://doing-meta.guide
# - https://www.r-bloggers.com/2021/08/meta-analysis-in-r/
#
# =============================================================================

# =============================================================================
# LOAD LIBRARIES
# =============================================================================
library(here)
library(tidyverse)
library(readxl)
library(writexl)
library(meta)
library(dmetar)
library(grid)
library(cowplot)
library(pdftools)
library(magick)
library(ggnewscale)

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

# Run metacont with random effects and add prediction intervals in one step
run_metacont <- function(data) {
       res <- metacont(
              n.e.,
              mean.e.,
              sd.e.,
              n.c.,
              mean.c.,
              sd.c.,
              common = FALSE,
              random = TRUE,
              studlab = Study,
              data = data,
              sm = "SMD"
       )
       update(res, prediction = TRUE)
}

# Save a standard forest plot to a PDF file
make_forest <- function(res, file, width = 10) {
       forest(
              res,
              leftcols = c(
                     "studlab",
                     "n.e.",
                     "mean.e.",
                     "sd.e.",
                     "n.c.",
                     "mean.c.",
                     "sd.c."
              ),
              leftlabs = c("Study", "N", "Mean", "SD", "N", "Mean", "SD"),
              label.e.attach = "sd.e.",
              label.c.attach = "sd.c.",
              label.e = "ADHD",
              label.c = "Control",
              label.left = "Increased in controls",
              label.right = "Increased in ADHD",
              just = "right",
              just.addcols = "right",
              just.studlab = "left",
              addrows.below.overall = 1,
              file = file,
              width = width
       )
}

# Run all four influence analysis plots for one analyte and save to PDF
run_influence <- function(res, name, dir) {
       inf <- InfluenceAnalysis(res, random = TRUE)
       specs <- list(
              baujat = c(w = 5, h = 3),
              influence = c(w = 5, h = 5),
              es = c(w = 12, h = 5),
              i2 = c(w = 12, h = 5)
       )
       for (type in names(specs)) {
              pdf(
                     here(dir, paste0(type, "-", name, ".pdf")),
                     width = specs[[type]]["w"],
                     height = specs[[type]]["h"]
              )
              plot(inf, type)
              dev.off()
       }
       invisible(inf)
}

# Run one subgroup analysis and return the meta object plus a tidy summary table.
#
# subgroup_vals: the raw data column vector (e.g. dopamine$Sample)
# tau_common:   logical — pool tau^2 across subgroups?
# model:        "common" or "random" — which within-subgroup estimates to extract.
#               Use "random" when tau.common = FALSE, or when tau.common = TRUE
#               but the random-effects within-subgroup estimates are preferred.
#               p_Q_between is taken from the matching common/random Q-test.
#
# NOTE (per-subgroup p-values): p_value in the output table tests whether each
# subgroup's pooled estimate differs from zero — NOT whether subgroups differ
# from each other. The latter is p_Q_between. Distinguish these clearly when
# reporting.
run_subgroup <- function(
       res,
       subgroup_vals,
       tau_common,
       model = "common",
       name = NULL
) {
       sg <- update(
              res,
              subgroup = subgroup_vals,
              tau.common = tau_common,
              subset = !is.na(subgroup_vals)
       )

       if (model == "random") {
              tbl <- data.frame(
                     Subgroup = sg$subgroup.levels,
                     k = sg$k.w,
                     n_ADHD = sg$n.e.w,
                     n_control = sg$n.c.w,
                     SMD = sg$TE.random.w,
                     lower_95CI = sg$lower.random.w,
                     upper_95CI = sg$upper.random.w,
                     p_value = sg$pval.random.w,
                     I2 = sg$I2.w,
                     lower_I2 = sg$lower.I2.w,
                     upper_I2 = sg$upper.I2.w,
                     p_Q_between = sg$pval.Q.b.random
              )
       } else {
              tbl <- data.frame(
                     Subgroup = sg$subgroup.levels,
                     k = sg$k.w,
                     n_ADHD = sg$n.e.w,
                     n_control = sg$n.c.w,
                     SMD = sg$TE.common.w,
                     lower_95CI = sg$lower.common.w,
                     upper_95CI = sg$upper.common.w,
                     p_value = sg$pval.common.w,
                     I2 = sg$I2.w,
                     lower_I2 = sg$lower.I2.w,
                     upper_I2 = sg$upper.I2.w,
                     p_Q_between = sg$pval.Q.b.common
              )
       }

       if (!is.null(name)) {
              tbl <- cbind(Subgroup_Variable = name, tbl)
       }
       list(obj = sg, tbl = tbl)
}

# Render a list of PDF file paths to a list of rasterGrobs (for cowplot)
render_pdfs <- function(paths, dpi = 600) {
       lapply(paths, function(f) {
              img <- magick::image_read(pdftools::pdf_render_page(
                     f,
                     page = 1,
                     dpi = dpi
              ))
              grid::rasterGrob(as.raster(img))
       })
}

# Build the standard subgroup analysis bar + I^2 line plot.
#
# df must have columns: Subgroup, k, SMD, I2, p_value  (as produced by
# run_subgroup() with a name argument and then rbind'd into a stacked table).
# desired_order controls which subgroup levels are shown and in what order;
# levels not listed are silently dropped (useful for excluding k = 1 levels
# that you never want shown, though the k > 1 filter already handles those).
#
# Significance stars on bars reflect p_value (per-level test vs. zero):
#   * p <= 0.05   ** p <= 0.01   *** p <= 0.001
#
# Returns an empty ggplot (theme_void) with a warning if no levels remain
# after the k > 1 and desired_order filters.
make_sa_plot <- function(df, desired_order) {
       df <- df[df$k > 1, ] # drop single-study levels
       df$I2 <- as.numeric(df$I2)
       df$Subgroup <- factor(df$Subgroup, levels = desired_order)
       df <- df[!is.na(df$Subgroup), ] # keep only levels in desired_order
       if (nrow(df) == 0) {
              warning(
                     "make_sa_plot: no levels remain after filtering — returning empty plot."
              )
              return(ggplot() + theme_void())
       }
       df$stars <- ifelse(
              df$p_value <= 0.001,
              "***",
              ifelse(
                     df$p_value <= 0.01,
                     "**",
                     ifelse(df$p_value <= 0.05, "*", "")
              )
       )
       smd_max <- max(abs(df$SMD))

       ggplot(df, aes(x = Subgroup)) +
              geom_bar(
                     aes(
                            y = abs(SMD),
                            fill = factor(ifelse(
                                   SMD < 0,
                                   "Negative effect size (SMD)",
                                   "Positive effect size (SMD)"
                            ))
                     ),
                     stat = "identity",
                     color = "black"
              ) +
              geom_text(
                     aes(y = abs(SMD) + smd_max * 0.04, label = stars),
                     vjust = 0,
                     size = 4
              ) +
              new_scale_color() +
              geom_line(
                     aes(y = I2 * smd_max, color = "I² (%)"),
                     group = 1,
                     linetype = "dashed",
                     linewidth = 0.5
              ) +
              geom_point(
                     aes(y = I2 * smd_max, color = "I² (%)"),
                     shape = 1,
                     size = 2
              ) +
              scale_y_continuous(
                     name = "Effect Size (SMD)",
                     sec.axis = sec_axis(~ . * 100 / smd_max, name = "I² (%)")
              ) +
              labs(title = " ", x = NULL) +
              scale_fill_manual(
                     values = c(
                            "Negative effect size (SMD)" = "#454545",
                            "Positive effect size (SMD)" = "#C0C0C0"
                     ),
                     name = NULL
              ) +
              scale_color_manual(values = c("I² (%)" = "black"), name = NULL) +
              theme_bw() +
              theme(
                     panel.grid = element_blank(),
                     axis.text.x = element_text(
                            angle = 45,
                            hjust = 1,
                            vjust = 1,
                            size = 10
                     ),
                     legend.position = "top",
                     legend.box = "horizontal",
                     legend.justification = "center",
                     legend.margin = margin(0, 0, 0, 0, unit = "pt"),
                     legend.text = element_text(size = 10),
                     legend.title = element_text(size = 10),
                     plot.title = element_text(hjust = 0.5)
              ) +
              coord_cartesian(ylim = c(0, smd_max * 1.2))
}

# =============================================================================
# LOAD DATA
# =============================================================================
dopamine <- read_excel(here("data/data_meta-analysis.xlsx"), sheet = 1)
adrenaline <- read_excel(here("data/data_meta-analysis.xlsx"), sheet = 2)
noradrenaline <- read_excel(here("data/data_meta-analysis.xlsx"), sheet = 3)
serotonin <- read_excel(here("data/data_meta-analysis.xlsx"), sheet = 4)
dopac <- read_excel(here("data/data_meta-analysis.xlsx"), sheet = 5)
hva <- read_excel(here("data/data_meta-analysis.xlsx"), sheet = 6)
hiaa <- read_excel(here("data/data_meta-analysis.xlsx"), sheet = 7)

# =============================================================================
# META-ANALYSIS
# =============================================================================
res.dopamine <- run_metacont(dopamine)
res.adrenaline <- run_metacont(adrenaline)
res.noradrenaline <- run_metacont(noradrenaline)
res.serotonin <- run_metacont(serotonin)
res.dopac <- run_metacont(dopac)
res.hva <- run_metacont(hva)
res.hiaa <- run_metacont(hiaa)

summary(res.dopamine)
res.dopamine$I2
res.dopamine$lower.I2
res.dopamine$upper.I2
summary(res.adrenaline)
res.adrenaline$I2
res.adrenaline$lower.I2
res.adrenaline$upper.I2
summary(res.noradrenaline)
res.noradrenaline$I2
res.noradrenaline$lower.I2
res.noradrenaline$upper.I2
summary(res.serotonin)
res.serotonin$I2
res.serotonin$lower.I2
res.serotonin$upper.I2
summary(res.dopac)
res.dopac$I2
res.dopac$lower.I2
res.dopac$upper.I2
summary(res.hva)
res.hva$I2
res.hva$lower.I2
res.hva$upper.I2
summary(res.hiaa)
res.hiaa$I2
res.hiaa$lower.I2
res.hiaa$upper.I2

# =============================================================================
# FOREST PLOTS
# =============================================================================
dir.create(here("outputs/meta-analysis/forest-plots"), recursive = TRUE)

forest_specs <- list(
       list(res = res.dopamine, file = "forest-plot-dopamine.pdf", width = 10),
       list(
              res = res.adrenaline,
              file = "forest-plot-adrenaline.pdf",
              width = 10.1
       ),
       list(
              res = res.noradrenaline,
              file = "forest-plot-noradrenaline.pdf",
              width = 10.3
       ),
       list(
              res = res.serotonin,
              file = "forest-plot-serotonin.pdf",
              width = 10
       ),
       list(res = res.dopac, file = "forest-plot-dopac.pdf", width = 9.8),
       list(res = res.hva, file = "forest-plot-hva.pdf", width = 10.1),
       list(res = res.hiaa, file = "forest-plot-5hiaa.pdf", width = 10.2)
)

for (spec in forest_specs) {
       make_forest(
              spec$res,
              here("outputs/meta-analysis/forest-plots", spec$file),
              spec$width
       )
}

forest_grobs <- render_pdfs(
       lapply(forest_specs, function(s) {
              here("outputs/meta-analysis/forest-plots", s$file)
       })
)

combined_plot <- plot_grid(
       plotlist = forest_grobs,
       labels = c(
              "A - Dopamine",
              "B - Adrenaline",
              "C - Noradrenaline",
              "D - Serotonin",
              "E - 3,4-dihydroxyphenylacetic acid",
              "F - Homovanillic acid",
              "G - 5-hydroxy-indoleacetic acid"
       ),
       nrow = 7,
       rel_heights = c(.7, .9, 1, .74, .5, .605, .52),
       label_size = 7,
       hjust = 0
)

save_plot(
       here("outputs/meta-analysis/forest-plots/combined_forest_plots.pdf"),
       combined_plot,
       base_width = 4.5,
       base_height = 13
)

combined_plot

# =============================================================================
# SENSITIVITY ANALYSIS
# =============================================================================
dir.create(here("outputs/sensitivity-analysis"), recursive = TRUE)

# -----------------------------------------------------------------------------
# INFLUENCE ANALYSIS
# -----------------------------------------------------------------------------
dir.create(
       here("outputs/sensitivity-analysis/influence-analysis"),
       recursive = TRUE
)

inf_dir <- "outputs/sensitivity-analysis/influence-analysis"
run_influence(res.dopamine, "dopamine", inf_dir)
run_influence(res.adrenaline, "adrenaline", inf_dir)
run_influence(res.noradrenaline, "noradrenaline", inf_dir)
run_influence(res.serotonin, "serotonin", inf_dir)
run_influence(res.dopac, "dopac", inf_dir)
run_influence(res.hva, "hva", inf_dir)
run_influence(res.hiaa, "hiaa", inf_dir)

# -----------------------------------------------------------------------------
# FINDING OUTLIERS
# -----------------------------------------------------------------------------
# Stored to make the link to sens1 exclusion indices explicit
outliers.dopamine <- find.outliers(res.dopamine) # -> exclude c(1, 3)
outliers.adrenaline <- find.outliers(res.adrenaline) # -> exclude c(8, 13)
outliers.noradrenaline <- find.outliers(res.noradrenaline) # -> exclude c(7, 10)
outliers.serotonin <- find.outliers(res.serotonin) # -> exclude c(6)
outliers.dopac <- find.outliers(res.dopac) # no outliers
outliers.hva <- find.outliers(res.hva) # no outliers
outliers.hiaa <- find.outliers(res.hiaa) # no outliers

# -----------------------------------------------------------------------------
# REMOVING OUTLIERS
# -----------------------------------------------------------------------------
dir.create(
       here("outputs/sensitivity-analysis/removing-outliers"),
       recursive = TRUE
)

res.dopamine.sens1 <- update(res.dopamine, exclude = c(1, 3)) # outliers.dopamine
res.adrenaline.sens1 <- update(res.adrenaline, exclude = c(8, 13)) # outliers.adrenaline
res.noradrenaline.sens1 <- update(res.noradrenaline, exclude = c(7, 10)) # outliers.noradrenaline
res.serotonin.sens1 <- update(res.serotonin, exclude = c(6)) # outliers.serotonin

summary(res.dopamine.sens1)
summary(res.adrenaline.sens1)
summary(res.noradrenaline.sens1)
summary(res.serotonin.sens1)

sens1_specs <- list(
       list(
              res = res.dopamine.sens1,
              file = "forest-plot-dopamine.pdf",
              width = 10
       ),
       list(
              res = res.adrenaline.sens1,
              file = "forest-plot-adrenaline.pdf",
              width = 10.1
       ),
       list(
              res = res.noradrenaline.sens1,
              file = "forest-plot-noradrenaline.pdf",
              width = 10.3
       ),
       list(
              res = res.serotonin.sens1,
              file = "forest-plot-serotonin.pdf",
              width = 10
       )
)

for (spec in sens1_specs) {
       make_forest(
              spec$res,
              here("outputs/sensitivity-analysis/removing-outliers", spec$file),
              spec$width
       )
}

sens1_grobs <- render_pdfs(
       lapply(sens1_specs, function(s) {
              here("outputs/sensitivity-analysis/removing-outliers", s$file)
       })
)

combined_plot_sens1 <- plot_grid(
       plotlist = sens1_grobs,
       labels = c(
              "A - Dopamine",
              "B - Adrenaline",
              "C - Noradrenaline",
              "D - Serotonin"
       ),
       nrow = 4,
       rel_heights = c(.7, .9, 1, .74),
       label_size = 7,
       hjust = 0
)

save_plot(
       here(
              "outputs/sensitivity-analysis/removing-outliers/combined_forest_plots.pdf"
       ),
       combined_plot_sens1,
       base_width = 4,
       base_height = 8
)

combined_plot_sens1

# -----------------------------------------------------------------------------
# REMOVING LOW QUALITY STUDIES
# -----------------------------------------------------------------------------
dir.create(
       here("outputs/sensitivity-analysis/removing-low-quality-studies"),
       recursive = TRUE
)

res.dopamine.sens2 <- update(res.dopamine, exclude = c(1, 4, 8))
res.adrenaline.sens2 <- update(res.adrenaline, exclude = c(1, 2, 5))
res.noradrenaline.sens2 <- update(res.noradrenaline, exclude = c(1, 2, 3, 8))
res.serotonin.sens2 <- update(res.serotonin, exclude = c(1, 6, 8))
res.hva.sens2 <- update(res.hva, exclude = c(2, 3, 4))
res.hiaa.sens2 <- update(res.hiaa, exclude = c(2, 3))
# no low quality studies for dopac

summary(res.dopamine.sens2)
summary(res.adrenaline.sens2)
summary(res.noradrenaline.sens2)
summary(res.serotonin.sens2)
summary(res.hva.sens2)
summary(res.hiaa.sens2)

sens2_specs <- list(
       list(
              res = res.dopamine.sens2,
              file = "forest-plot-dopamine.pdf",
              width = 10
       ),
       list(
              res = res.adrenaline.sens2,
              file = "forest-plot-adrenaline.pdf",
              width = 10.1
       ),
       list(
              res = res.noradrenaline.sens2,
              file = "forest-plot-noradrenaline.pdf",
              width = 10.3
       ),
       list(
              res = res.serotonin.sens2,
              file = "forest-plot-serotonin.pdf",
              width = 10
       ),
       list(res = res.hva.sens2, file = "forest-plot-hva.pdf", width = 10),
       list(res = res.hiaa.sens2, file = "forest-plot-5hiaa.pdf", width = 10)
)

for (spec in sens2_specs) {
       make_forest(
              spec$res,
              here(
                     "outputs/sensitivity-analysis/removing-low-quality-studies",
                     spec$file
              ),
              spec$width
       )
}

sens2_grobs <- render_pdfs(
       lapply(sens2_specs, function(s) {
              here(
                     "outputs/sensitivity-analysis/removing-low-quality-studies",
                     s$file
              )
       })
)

combined_plot_sens2 <- plot_grid(
       plotlist = sens2_grobs,
       labels = c(
              "A - Dopamine",
              "B - Adrenaline",
              "C - Noradrenaline",
              "D - Serotonin",
              "E - Homovanillic acid",
              "F - 5-hydroxy-indoleacetic acid"
       ),
       nrow = 6,
       rel_heights = c(.7, .9, 1, .74, .58, .47),
       label_size = 7,
       hjust = 0
)

save_plot(
       here(
              "outputs/sensitivity-analysis/removing-low-quality-studies/combined_forest_plots.pdf"
       ),
       combined_plot_sens2,
       base_width = 4,
       base_height = 10
)

combined_plot_sens2

# =============================================================================
# PUBLICATION BIAS
# =============================================================================
dir.create(here("outputs/publication-bias"), recursive = TRUE)

# -----------------------------------------------------------------------------
# FUNNEL PLOTS
# -----------------------------------------------------------------------------
# Fewer than 10 studies for dopamine, serotonin, dopac, hva, hiaa — not included
dir.create(here("outputs/publication-bias/funnel-plots"), recursive = TRUE)

pdf(
       here("outputs/publication-bias/funnel-plots/funnel-plot-adrenaline.pdf"),
       width = 7,
       height = 4.5
)
funnel(
       res.adrenaline,
       level = 0.95,
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("gray75", "gray85", "gray95"),
       lwd = 2,
       cex = 1,
       pch = 16,
       studlab = TRUE,
       cex.studlab = 0.7
)
legend(
       0.8,
       0,
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
       fill = c("gray75", "gray85", "gray95")
)
dev.off()

pdf(
       here(
              "outputs/publication-bias/funnel-plots/funnel-plot-noradrenaline.pdf"
       ),
       width = 7,
       height = 4.5
)
funnel(
       res.noradrenaline,
       level = 0.95,
       contour = c(0.9, 0.95, 0.99),
       col.contour = c("gray75", "gray85", "gray95"),
       lwd = 2,
       cex = 1,
       pch = 16,
       studlab = TRUE,
       cex.studlab = 0.7
)
legend(
       0.7,
       0,
       c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
       fill = c("gray75", "gray85", "gray95")
)
dev.off()

funnel_grobs <- render_pdfs(list(
       here("outputs/publication-bias/funnel-plots/funnel-plot-adrenaline.pdf"),
       here(
              "outputs/publication-bias/funnel-plots/funnel-plot-noradrenaline.pdf"
       )
))

combined_funnel_plots <- plot_grid(
       plotlist = funnel_grobs,
       nrow = 1,
       ncol = 2,
       labels = c("A - Adrenaline", "B - Noradrenaline"),
       label_size = 6,
       hjust = 0
)

save_plot(
       here("outputs/publication-bias/funnel-plots/combined_funnel_plots.pdf"),
       combined_funnel_plots,
       base_width = 6.6,
       base_height = 2
)

combined_funnel_plots

# -----------------------------------------------------------------------------
# EGGER'S TEST
# -----------------------------------------------------------------------------
# Note: the custom eggers.test() wrapper used in earlier versions of this
# script has been removed. metabias(method = "linreg") returns the intercept,
# SE, 95% CI, t-statistic, df, and p-value directly. Results are stored in
# egger.ad and egger.na for reporting.
#
# Fewer than 10 studies for dopamine, serotonin, dopac, hva, hiaa — not conducted.
dir.create(here("outputs/publication-bias/eggers-plots"), recursive = TRUE)

pdf(
       here(
              "outputs/publication-bias/eggers-plots/regression-line-egger-adrenaline.pdf"
       ),
       width = 7,
       height = 4.5
)
egger.ad <- metabias(res.adrenaline, method.bias = "linreg", plotit = TRUE)
dev.off()

pdf(
       here(
              "outputs/publication-bias/eggers-plots/regression-line-egger-noradrenaline.pdf"
       ),
       width = 7,
       height = 4.5
)
egger.na <- metabias(res.noradrenaline, method.bias = "linreg", plotit = TRUE)
dev.off()

egger_grobs <- render_pdfs(list(
       here(
              "outputs/publication-bias/eggers-plots/regression-line-egger-adrenaline.pdf"
       ),
       here(
              "outputs/publication-bias/eggers-plots/regression-line-egger-noradrenaline.pdf"
       )
))

combined_regression_lines <- plot_grid(
       plotlist = egger_grobs,
       nrow = 1,
       ncol = 2,
       labels = c("A - Adrenaline", "B - Noradrenaline"),
       label_size = 6,
       hjust = 0
)

save_plot(
       here(
              "outputs/publication-bias/eggers-plots/combined_regression_lines.pdf"
       ),
       combined_regression_lines,
       base_width = 6.6,
       base_height = 2
)

combined_regression_lines

egger.ad
eggers.test(res.adrenaline)
egger.na
eggers.test(res.noradrenaline)

# =============================================================================
# SUBGROUP ANALYSIS
# =============================================================================
dir.create(here("outputs/subgroup-analysis"), recursive = TRUE)

# dopamine..................................................................\
da.s <- run_subgroup(
       res.dopamine,
       dopamine$Sample,
       tau_common = TRUE,
       name = "Sample"
)
da.up <- run_subgroup(
       res.dopamine,
       dopamine$UrineProtocol,
       tau_common = TRUE,
       name = "UrineProtocol"
)
da.bc <- run_subgroup(
       res.dopamine,
       dopamine$BloodComponents,
       tau_common = TRUE,
       name = "BloodComponents"
)
da.c <- run_subgroup(
       res.dopamine,
       dopamine$Country,
       tau_common = TRUE,
       name = "Country"
)
da.sex <- run_subgroup(
       res.dopamine,
       dopamine$Sex,
       tau_common = TRUE,
       name = "Sex"
)
da.med <- run_subgroup(
       res.dopamine,
       dopamine$ADHDtakesMed,
       tau_common = TRUE,
       name = "ADHDtakesMed"
)
da.aa <- run_subgroup(
       res.dopamine,
       dopamine$ADHDassess,
       tau_common = TRUE,
       name = "ADHDassess"
)
da.am <- run_subgroup(
       res.dopamine,
       dopamine$AssayMethod,
       tau_common = TRUE,
       name = "AssayMethod"
)
da.sq <- run_subgroup(
       res.dopamine,
       dopamine$StudyQuality,
       tau_common = TRUE,
       name = "StudyQuality"
)

# keep named objects for interactive inspection if needed
dopamine.Sample <- da.s$obj
dopamine.UrineProtocol <- da.up$obj
dopamine.BloodComponents <- da.bc$obj
dopamine.Country <- da.c$obj
dopamine.Sex <- da.sex$obj
dopamine.Med <- da.med$obj
dopamine.ADHDassess <- da.aa$obj
dopamine.AssayMethod <- da.am$obj
dopamine.StudyQuality <- da.sq$obj

# adrenaline................................................................
ad.s <- run_subgroup(
       res.adrenaline,
       adrenaline$Sample,
       tau_common = TRUE,
       name = "Sample"
)
ad.up <- run_subgroup(
       res.adrenaline,
       adrenaline$UrineProtocol,
       tau_common = TRUE,
       name = "UrineProtocol"
)
ad.bc <- run_subgroup(
       res.adrenaline,
       adrenaline$BloodComponents,
       tau_common = TRUE,
       name = "BloodComponents"
)
ad.sex <- run_subgroup(
       res.adrenaline,
       adrenaline$Sex,
       tau_common = FALSE,
       model = "random",
       name = "Sex"
)
ad.med <- run_subgroup(
       res.adrenaline,
       adrenaline$ADHDtakesMed,
       tau_common = TRUE,
       name = "ADHDtakesMed"
)
ad.aa <- run_subgroup(
       res.adrenaline,
       adrenaline$ADHDassess,
       tau_common = TRUE,
       name = "ADHDassess"
)
ad.am <- run_subgroup(
       res.adrenaline,
       adrenaline$AssayMethod,
       tau_common = TRUE,
       name = "AssayMethod"
)
ad.sq <- run_subgroup(
       res.adrenaline,
       adrenaline$StudyQuality,
       tau_common = TRUE,
       name = "StudyQuality"
)

adrenaline.Sample <- ad.s$obj
adrenaline.UrineProtocol <- ad.up$obj
adrenaline.BloodComponents <- ad.bc$obj
adrenaline.Sex <- ad.sex$obj
adrenaline.Med <- ad.med$obj
adrenaline.ADHDassess <- ad.aa$obj
adrenaline.AssayMethod <- ad.am$obj
adrenaline.StudyQuality <- ad.sq$obj

# noradrenaline.............................................................\
na.s <- run_subgroup(
       res.noradrenaline,
       noradrenaline$Sample,
       tau_common = FALSE,
       model = "random",
       name = "Sample"
)
na.up <- run_subgroup(
       res.noradrenaline,
       noradrenaline$UrineProtocol,
       tau_common = TRUE,
       name = "UrineProtocol"
)
na.bc <- run_subgroup(
       res.noradrenaline,
       noradrenaline$BloodComponents,
       tau_common = TRUE,
       name = "BloodComponents"
)
na.c <- run_subgroup(
       res.noradrenaline,
       noradrenaline$Country,
       tau_common = TRUE,
       name = "Country"
)
na.sex <- run_subgroup(
       res.noradrenaline,
       noradrenaline$Sex,
       tau_common = FALSE,
       model = "random",
       name = "Sex"
)
na.med <- run_subgroup(
       res.noradrenaline,
       noradrenaline$ADHDtakesMed,
       tau_common = TRUE,
       name = "ADHDtakesMed"
)
na.aa <- run_subgroup(
       res.noradrenaline,
       noradrenaline$ADHDassess,
       tau_common = TRUE,
       name = "ADHDassess"
)
na.am <- run_subgroup(
       res.noradrenaline,
       noradrenaline$AssayMethod,
       tau_common = FALSE,
       model = "random",
       name = "AssayMethod"
)
na.sq <- run_subgroup(
       res.noradrenaline,
       noradrenaline$StudyQuality,
       tau_common = TRUE,
       name = "StudyQuality"
)

noradrenaline.Sample <- na.s$obj
noradrenaline.UrineProtocol <- na.up$obj
noradrenaline.BloodComponents <- na.bc$obj
noradrenaline.Country <- na.c$obj
noradrenaline.Sex <- na.sex$obj
noradrenaline.Med <- na.med$obj
noradrenaline.ADHDassess <- na.aa$obj
noradrenaline.AssayMethod <- na.am$obj
noradrenaline.StudyQuality <- na.sq$obj

# serotonin.................................................................\
ht.s <- run_subgroup(
       res.serotonin,
       serotonin$Sample,
       tau_common = TRUE,
       name = "Sample"
)
ht.bc <- run_subgroup(
       res.serotonin,
       serotonin$BloodComponents,
       tau_common = TRUE,
       name = "BloodComponents"
)
ht.c <- run_subgroup(
       res.serotonin,
       serotonin$Country,
       tau_common = TRUE,
       name = "Country"
)
ht.sex <- run_subgroup(
       res.serotonin,
       serotonin$Sex,
       tau_common = TRUE,
       name = "Sex"
)
ht.med <- run_subgroup(
       res.serotonin,
       serotonin$ADHDtakesMed,
       tau_common = TRUE,
       name = "ADHDtakesMed"
)
ht.aa <- run_subgroup(
       res.serotonin,
       serotonin$ADHDassess,
       tau_common = TRUE,
       name = "ADHDassess"
)
ht.am <- run_subgroup(
       res.serotonin,
       serotonin$AssayMethod,
       tau_common = TRUE,
       model = "random",
       name = "AssayMethod"
)
ht.sq <- run_subgroup(
       res.serotonin,
       serotonin$StudyQuality,
       tau_common = TRUE,
       name = "StudyQuality"
)

serotonin.Sample <- ht.s$obj
serotonin.BloodComponents <- ht.bc$obj
serotonin.Country <- ht.c$obj
serotonin.Sex <- ht.sex$obj
serotonin.Med <- ht.med$obj
serotonin.ADHDassess <- ht.aa$obj
serotonin.AssayMethod <- ht.am$obj
serotonin.StudyQuality <- ht.sq$obj

# dopac.....................................................................\
# Only the Med subgroup was analysed; subgroup levels lacked sufficient data
# for display in the plot (see note in SUBGROUP ANALYSIS — PLOTS below).
dopac.med <- run_subgroup(
       res.dopac,
       dopac$ADHDtakesMed,
       tau_common = TRUE,
       name = "ADHDtakesMed"
)
dopac.Med <- dopac.med$obj

# hva.......................................................................\
hva.up <- run_subgroup(
       res.hva,
       hva$UrineProtocol,
       tau_common = TRUE,
       name = "UrineProtocol"
)
hva.c <- run_subgroup(res.hva, hva$Country, tau_common = TRUE, name = "Country")
hva.sex <- run_subgroup(res.hva, hva$Sex, tau_common = TRUE, name = "Sex")
hva.med <- run_subgroup(
       res.hva,
       hva$ADHDtakesMed,
       tau_common = TRUE,
       name = "ADHDtakesMed"
)
hva.aa <- run_subgroup(
       res.hva,
       hva$ADHDassess,
       tau_common = TRUE,
       name = "ADHDassess"
)
hva.am <- run_subgroup(
       res.hva,
       hva$AssayMethod,
       tau_common = TRUE,
       name = "AssayMethod"
)
hva.sq <- run_subgroup(
       res.hva,
       hva$StudyQuality,
       tau_common = TRUE,
       name = "StudyQuality"
)

hva.UrineProtocol <- hva.up$obj
hva.Country <- hva.c$obj
hva.Sex <- hva.sex$obj
hva.Med <- hva.med$obj
hva.ADHDassess <- hva.aa$obj
hva.AssayMethod <- hva.am$obj
hva.StudyQuality <- hva.sq$obj

# 5-hiaa....................................................................\
hiaa.s <- run_subgroup(
       res.hiaa,
       hiaa$Sample,
       tau_common = TRUE,
       name = "Sample"
)
hiaa.up <- run_subgroup(
       res.hiaa,
       hiaa$UrineProtocol,
       tau_common = TRUE,
       name = "UrineProtocol"
)
hiaa.c <- run_subgroup(
       res.hiaa,
       hiaa$Country,
       tau_common = TRUE,
       name = "Country"
)
hiaa.sex <- run_subgroup(res.hiaa, hiaa$Sex, tau_common = TRUE, name = "Sex")
hiaa.med <- run_subgroup(
       res.hiaa,
       hiaa$ADHDtakesMed,
       tau_common = TRUE,
       name = "ADHDtakesMed"
)
hiaa.aa <- run_subgroup(
       res.hiaa,
       hiaa$ADHDassess,
       tau_common = TRUE,
       name = "ADHDassess"
)
hiaa.am <- run_subgroup(
       res.hiaa,
       hiaa$AssayMethod,
       tau_common = TRUE,
       name = "AssayMethod"
)
hiaa.sq <- run_subgroup(
       res.hiaa,
       hiaa$StudyQuality,
       tau_common = TRUE,
       name = "StudyQuality"
)

hiaa.Sample <- hiaa.s$obj
hiaa.UrineProtocol <- hiaa.up$obj
hiaa.Country <- hiaa.c$obj
hiaa.Sex <- hiaa.sex$obj
hiaa.Med <- hiaa.med$obj
hiaa.ADHDassess <- hiaa.aa$obj
hiaa.AssayMethod <- hiaa.am$obj
hiaa.StudyQuality <- hiaa.sq$obj

# Stack all subgroup tables per analyte and export a single workbook.
# Each sheet = one analyte; each row = one subgroup level; Subgroup_Variable
# identifies which subgroup analysis a row came from.
# Levels with k = 1 are retained here (they are filtered in make_sa_plot()) so
# the full data is preserved for inspection.
sa.tbl.da <- rbind(
       da.s$tbl,
       da.up$tbl,
       da.bc$tbl,
       da.c$tbl,
       da.sex$tbl,
       da.med$tbl,
       da.aa$tbl,
       da.am$tbl,
       da.sq$tbl
)
sa.tbl.ad <- rbind(
       ad.s$tbl,
       ad.up$tbl,
       ad.bc$tbl,
       ad.sex$tbl,
       ad.med$tbl,
       ad.aa$tbl,
       ad.am$tbl,
       ad.sq$tbl
)
sa.tbl.na <- rbind(
       na.s$tbl,
       na.up$tbl,
       na.bc$tbl,
       na.c$tbl,
       na.sex$tbl,
       na.med$tbl,
       na.aa$tbl,
       na.am$tbl,
       na.sq$tbl
)
sa.tbl.5ht <- rbind(
       ht.s$tbl,
       ht.bc$tbl,
       ht.c$tbl,
       ht.sex$tbl,
       ht.med$tbl,
       ht.aa$tbl,
       ht.am$tbl,
       ht.sq$tbl
)
sa.tbl.dopac <- dopac.med$tbl
sa.tbl.hva <- rbind(
       hva.up$tbl,
       hva.c$tbl,
       hva.sex$tbl,
       hva.med$tbl,
       hva.aa$tbl,
       hva.am$tbl,
       hva.sq$tbl
)
sa.tbl.5hiaa <- rbind(
       hiaa.s$tbl,
       hiaa.up$tbl,
       hiaa.c$tbl,
       hiaa.sex$tbl,
       hiaa.med$tbl,
       hiaa.aa$tbl,
       hiaa.am$tbl,
       hiaa.sq$tbl
)

write_xlsx(
       list(
              Dopamine = sa.tbl.da,
              Adrenaline = sa.tbl.ad,
              Noradrenaline = sa.tbl.na,
              Serotonin = sa.tbl.5ht,
              DOPAC = sa.tbl.dopac,
              HVA = sa.tbl.hva,
              "5-HIAA" = sa.tbl.5hiaa
       ),
       here("outputs/subgroup-analysis/sa.tbls.xlsx")
)

# -----------------------------------------------------------------------------
# SUBGROUP ANALYSIS — PLOTS
# -----------------------------------------------------------------------------
# Plot data now comes directly from the in-memory stacked tables (sa.tbl.*)
# built above — no need to read a separate Excel.
# Levels with k = 1 are dropped automatically inside make_sa_plot().
# NOTE: "All studies" (the overall pooled estimate, not a subgroup level) was
# previously added manually to data_subgroup-analysis_plots.xlsx.  It is
# intentionally omitted here; refer to res.* objects for the overall result.

da.sa.plot <- make_sa_plot(
       sa.tbl.da,
       c(
              "Urine",
              "Blood",
              "<24-hr urine collection",
              "Plasma",
              "Serum",
              "Western country",
              "Eastern country",
              "Both sexes",
              "Male",
              "ADHD group takes med",
              "ADHD group doesn't take med",
              "DSM-IV/DSM-IV-TR/DSM-5",
              "Other diagnostic method",
              "HPLC",
              "Other assay method",
              "Fair or Good quality",
              "Very poor or Poor quality"
       )
)

ad.sa.plot <- make_sa_plot(
       sa.tbl.ad,
       c(
              "Urine",
              "Blood",
              "24-hr urine collection",
              "<24-hr urine collection",
              "Plasma",
              "Both sexes",
              "Male",
              "ADHD group takes med",
              "ADHD group doesn't take med",
              "DSM-IV/DSM-IV-TR/DSM-5",
              "Other diagnostic method",
              "HPLC",
              "Other assay method",
              "Fair or Good quality",
              "Very poor or Poor quality"
       )
)

na.sa.plot <- make_sa_plot(
       sa.tbl.na,
       c(
              "Urine",
              "Blood",
              "24-hr urine collection",
              "<24-hr urine collection",
              "Plasma",
              "Serum",
              "Western country",
              "Eastern country",
              "Both sexes",
              "Male",
              "ADHD group takes med",
              "ADHD group doesn't take med",
              "DSM-IV/DSM-IV-TR/DSM-5",
              "Other diagnostic method",
              "HPLC",
              "Other assay method",
              "Fair or Good quality",
              "Very poor or Poor quality"
       )
)

ser.sa.plot <- make_sa_plot(
       sa.tbl.5ht,
       c(
              "Blood",
              "Whole blood",
              "Platelets",
              "Serum",
              "Western country",
              "Eastern country",
              "Both sexes",
              "ADHD group takes med",
              "ADHD group doesn't take med",
              "DSM-IV/DSM-IV-TR/DSM-5",
              "Other diagnostic method",
              "Spectrofluorimetry",
              "LC-MS",
              "RIA/ELISA kit",
              "Fair or Good quality",
              "Very poor or Poor quality"
       )
)

# NOTE: DOPAC only has the Med subgroup analysis. Both Med levels currently
# have k = 1, so make_sa_plot() returns an empty plot; dopac.sa.plot is
# excluded from combined_plot_2 for that reason. If data are added and k > 1
# for either level, add dopac.sa.plot to the combined plot list below.
dopac.sa.plot <- make_sa_plot(
       sa.tbl.dopac,
       c(
              "ADHD group takes med",
              "ADHD group doesn't take med"
       )
)

hva.sa.plot <- make_sa_plot(
       sa.tbl.hva,
       c(
              "24-hr urine collection",
              "Western country",
              "Male",
              "ADHD group takes med",
              "DSM-IV/DSM-IV-TR/DSM-5",
              "Other diagnostic method",
              "Other assay method",
              "Fair or Good quality",
              "Very poor or Poor quality"
       )
)

hiaa.sa.plot <- make_sa_plot(
       sa.tbl.5hiaa,
       c(
              "Urine",
              "Western country",
              "ADHD group takes med",
              "DSM-IV/DSM-IV-TR/DSM-5",
              "Other assay method",
              "Very poor or Poor quality"
       )
)

combined_plot_2 <- plot_grid(
       plotlist = list(
              da.sa.plot,
              ad.sa.plot,
              na.sa.plot,
              ser.sa.plot,
              hva.sa.plot,
              hiaa.sa.plot
       ),
       labels = c(
              "A - Dopamine",
              "B - Adrenaline",
              "C - Noradrenaline",
              "D - Serotonin",
              "E - Homovanillic acid",
              "F - 5-hydroxy-indoleacetic acid"
       ),
       ncol = 2,
       nrow = 3,
       label_size = 11,
       hjust = 0
)

save_plot(
       here("outputs/subgroup-analysis/combined_subgroup_analyses.pdf"),
       combined_plot_2,
       base_width = 13,
       base_height = 13
)

combined_plot_2

# =============================================================================
# SESSION INFO
# =============================================================================
sessionInfo()

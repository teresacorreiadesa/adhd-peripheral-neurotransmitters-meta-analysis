###########################################################################

#' # [Meta-Analysis](https://www.r-bloggers.com/2021/08/meta-analysis-in-r/)

###########################################################################

library(readxl)
dopamine <- read_excel("/Users/teresacorreiadesa/R/Meta-Analysis/data/data_meta-analysis.xlsx", sheet = 1)
adrenaline <- read_excel("/Users/teresacorreiadesa/R/Meta-Analysis/data/data_meta-analysis.xlsx", sheet = 2)
noradrenaline <- read_excel("/Users/teresacorreiadesa/R/Meta-Analysis/data/data_meta-analysis.xlsx", sheet = 3)
serotonin <- read_excel("/Users/teresacorreiadesa/R/Meta-Analysis/data/data_meta-analysis.xlsx", sheet = 4)
dopac <- read_excel("/Users/teresacorreiadesa/R/Meta-Analysis/data/data_meta-analysis.xlsx", sheet = 5)
hva <- read_excel("/Users/teresacorreiadesa/R/Meta-Analysis/data/data_meta-analysis.xlsx", sheet = 6)
hiaa <- read_excel("/Users/teresacorreiadesa/R/Meta-Analysis/data/data_meta-analysis.xlsx", sheet = 7)
# View(data)

## ------------------------------------------------------------------------
#' ## Meta-Analysis
## ------------------------------------------------------------------------
library(meta)
# meta-analysis with continuous outcome
# common/random indicator: whether a common/random effects meta-analysis is to be conducted.
# sm: Three different types of summary measures to chosen,standardized mean difference (SMD), mean difference (MD), ratio of means (ROM)
## ------------------------------------------------------------------------
res.dopamine <- metacont(n.e., mean.e., sd.e., 
                        n.c., mean.c., sd.c.,
                        common = F, random = T, studlab = Study,
                        data = dopamine, sm = "SMD")

res.adrenaline <- metacont(n.e., mean.e., sd.e., 
                        n.c., mean.c., sd.c.,
                        common = F, random = T, studlab = Study,
                        data = adrenaline, sm = "SMD")

res.noradrenaline <- metacont(n.e., mean.e., sd.e., 
                        n.c., mean.c., sd.c.,
                        common = F, random = T, studlab = Study,
                        data = noradrenaline, sm = "SMD")

res.serotonin <- metacont(n.e., mean.e., sd.e., 
                        n.c., mean.c., sd.c.,
                        common = F, random = T, studlab = Study,
                        data = serotonin, sm = "SMD")

res.dopac <-  metacont(n.e., mean.e., sd.e., 
                        n.c., mean.c., sd.c.,
                        common = F, random = T, studlab = Study,
                        data = dopac, sm = "SMD")

res.hva <-  metacont(n.e., mean.e., sd.e., 
                        n.c., mean.c., sd.c.,
                        common = F, random = T, studlab = Study,
                        data = hva, sm = "SMD")

res.hiaa <- metacont(n.e., mean.e., sd.e., 
                        n.c., mean.c., sd.c.,
                        common = F, random = T, studlab = Study,
                        data = hiaa, sm = "SMD")


## ------------------------------------------------------------------------
res.dopamine
## ------------------------------------------------------------------------
res.adrenaline
## ------------------------------------------------------------------------
res.noradrenaline
## ------------------------------------------------------------------------
res.serotonin
## ------------------------------------------------------------------------
res.dopac
## ------------------------------------------------------------------------
res.hva
## ------------------------------------------------------------------------
res.hiaa

## ------------------------------------------------------------------------
#' ## Forest Plot
## ------------------------------------------------------------------------
## hard space: option + space

dir.create("outputs/meta-analysis/forest-plots", recursive = TRUE) # create directory to save forest plots

forest(res.dopamine, 
       leftcols = c('studlab', 'n.e.', 'mean.e.', 'sd.e.', 'n.c.', 'mean.c.', 'sd.c.'), 
       leftlabs = c('Study', 'N', 'Mean', 'SD', 'N', 'Mean', 'SD'), # change labels
       label.e.attach = 'sd.e.', # add group heading to appropriate place
       label.c.attach = 'sd.c.', # same as above but for other group
       label.e = 'ADHD', # name group
       label.c = 'Control', # ...
       label.left = 'Increased in controls', 
       label.right = 'Increased in ADHD',
       just = "right", just.addcols = "right", just.studlab = "left", # justify text
       addrows.below.overall = 1,
       file = 'outputs/meta-analysis/forest-plots/forest-plot-dopamine.pdf', width = 10) 

forest(res.adrenaline,
       leftcols = c('studlab', 'n.e.', 'mean.e.', 'sd.e.', 'n.c.', 'mean.c.', 'sd.c.'), 
       leftlabs = c('Study', 'N', 'Mean', 'SD', 'N', 'Mean', 'SD'), # change labels
       label.e.attach = 'sd.e.', # add group heading to appropriate place
       label.c.attach = 'sd.c.', # same as above but for other group
       label.e = 'ADHD', # name group
       label.c = 'Control', # ...
       label.left = 'Increased in controls',
       label.right = 'Increased in ADHD',
       just = "right", just.addcols = "right", just.studlab = "left", # justify text
       addrows.below.overall = 1,
       file = 'outputs/meta-analysis/forest-plots/forest-plot-adrenaline.pdf', width = 10.1) 

forest(res.noradrenaline,
       leftcols = c('studlab', 'n.e.', 'mean.e.', 'sd.e.', 'n.c.', 'mean.c.', 'sd.c.'), 
       leftlabs = c('Study', 'N', 'Mean', 'SD', 'N', 'Mean', 'SD'), # change labels
       label.e.attach = 'sd.e.', # add group heading to appropriate place
       label.c.attach = 'sd.c.', # same as above but for other group
       label.e = 'ADHD', # name group
       label.c = 'Control', # ...
       label.left = 'Increased in controls',
       label.right = 'Increased in ADHD',
       just = "right", just.addcols = "right", just.studlab = "left", # justify text
       addrows.below.overall = 1,
       file = 'outputs/meta-analysis/forest-plots/forest-plot-noradrenaline.pdf', width = 10.3) 

forest(res.serotonin, 
       leftcols = c('studlab', 'n.e.', 'mean.e.', 'sd.e.', 'n.c.', 'mean.c.', 'sd.c.'), 
       leftlabs = c('Study', 'N', 'Mean', 'SD', 'N', 'Mean', 'SD'), # change labels
       label.e.attach = 'sd.e.', # add group heading to appropriate place
       label.c.attach = 'sd.c.', # same as above but for other group
       label.e = 'ADHD', # name group
       label.c = 'Control', # ...
       label.left = 'Increased in controls',
       label.right = 'Increased in ADHD',
       just = "right", just.addcols = "right", just.studlab = "left", # justify text
       addrows.below.overall = 1,
       file = 'outputs/meta-analysis/forest-plots/forest-plot-serotonin.pdf', width = 10) 

forest(res.dopac, 
       leftcols = c('studlab', 'n.e.', 'mean.e.', 'sd.e.', 'n.c.', 'mean.c.', 'sd.c.'), 
       leftlabs = c('Study', 'N', 'Mean', 'SD', 'N', 'Mean', 'SD'), # change labels
       label.e.attach = 'sd.e.', # add group heading to appropriate place
       label.c.attach = 'sd.c.', # same as above but for other group
       label.e = 'ADHD', # name group
       label.c = 'Control', # ...
       label.left = 'Increased in controls',
       label.right = 'Increased in ADHD',
       just = "right", just.addcols = "right", just.studlab = "left", # justify text
       addrows.below.overall = 1,
       file = 'outputs/meta-analysis/forest-plots/forest-plot-dopac.pdf', width = 9.8) 

forest(res.hva, 
       leftcols = c('studlab', 'n.e.', 'mean.e.', 'sd.e.', 'n.c.', 'mean.c.', 'sd.c.'), 
       leftlabs = c('Study', 'N', 'Mean', 'SD', 'N', 'Mean', 'SD'), # change labels
       label.e.attach = 'sd.e.', # add group heading to appropriate place
       label.c.attach = 'sd.c.', # same as above but for other group
       label.e = 'ADHD', # name group
       label.c = 'Control', # ...
       label.left = 'Increased in controls',
       label.right = 'Increased in ADHD',
       just = "right", just.addcols = "right", just.studlab = "left", # justify text
       addrows.below.overall = 1,
       file = 'outputs/meta-analysis/forest-plots/forest-plot-hva.pdf', width = 10.1) 

forest(res.hiaa, 
       leftcols = c('studlab', 'n.e.', 'mean.e.', 'sd.e.', 'n.c.', 'mean.c.', 'sd.c.'), 
       leftlabs = c('Study', 'N', 'Mean', 'SD', 'N', 'Mean', 'SD'), # change labels
       label.e.attach = 'sd.e.', # add group heading to appropriate place
       label.c.attach = 'sd.c.', # same as above but for other group
       label.e = 'ADHD', # name group
       label.c = 'Control', # ...
       label.left = 'Increased in controls',
       label.right = 'Increased in ADHD',
       just = "right", just.addcols = "right", just.studlab = "left", # justify text
       addrows.below.overall = 1,
       file = 'outputs/meta-analysis/forest-plots/forest-plot-5hiaa.pdf', width = 10.2) 


## ------------------------------------------------------------------------
# convert pdf files to svgs using website converter like https://tools.pdf24.org/en/pdf-to-svg or https://convertio.co/pdf-svg/ 
## ------------------------------------------------------------------------
library(rsvg)
library(grid)
library(cowplot)

# list of SVG file paths
svg_files <- c("outputs/meta-analysis/forest-plots/forest-plot-dopamine.svg", "outputs/meta-analysis/forest-plots/forest-plot-adrenaline.svg",
               "outputs/meta-analysis/forest-plots/forest-plot-noradrenaline.svg", "outputs/meta-analysis/forest-plots/forest-plot-serotonin.svg",
               "outputs/meta-analysis/forest-plots/forest-plot-dopac.svg", "outputs/meta-analysis/forest-plots/forest-plot-hva.svg", 
               "outputs/meta-analysis/forest-plots/forest-plot-5hiaa.svg")

# function to read SVGs and convert to grobs
svg_to_grob <- function(file, dpi = 300) {
  svg_data <- readLines(file, warn = FALSE)
  svg <- paste(svg_data, collapse = "\n")
  
  svg_image <- rsvg::rsvg(charToRaw(svg))
  grob <- grid::rasterGrob(svg_image, width = unit(1,"npc"), height = unit(1,"npc"))
  grob
}

# create a list of grobs
grob_list <- lapply(svg_files, svg_to_grob)

# arrange the plots using plot_grid
combined_plot <- plot_grid(plotlist = grob_list,
                           labels = c("A - Dopamine", "B - Adrenaline", "C - Noradrenaline", "D - Serotonin", 
                           "E - 3,4-dihydroxyphenylacetic acid", "F - Homovanillic acid", "G - 5-hydroxy-indoleacetic acid"),
                           nrow = 7,
                           rel_heights = c(.7, .9, 1, .74, .5, .605, .52),
                           label_size = 7,
                           hjust = 0)

# save the combined plot
save_plot("outputs/meta-analysis/forest-plots/combined_forest_plots.pdf", combined_plot, base_width = 5, base_height = 12)

combined_plot

## ------------------------------------------------------------------------
#' ## Funnel Plot 
## ------------------------------------------------------------------------

dir.create("outputs/meta-analysis/funnel-plots", recursive = TRUE) # create directory to save funnel plots

## ------------------------------------------------------------------------
png("outputs/meta-analysis/funnel-plots/funnel-plot-adrenaline.png", width = 7, height = 4.5, units = "in", res = 300) # open file device to save funnel plot as PNG

funnel(res.adrenaline,
  level = 0.95, contour = c(0.9, 0.95, 0.99),
  col.contour = c("gray75", "gray85", "gray95"),
  lwd = 2, cex = 1, pch = 16, studlab = TRUE, cex.studlab = 0.7)
legend(8.2, 0,
  c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
  fill = c("gray75", "gray85", "gray95"))

dev.off() # close the PNG device
 
## ------------------------------------------------------------------------

png("outputs/meta-analysis/funnel-plots/funnel-plot-noradrenaline.png", width = 7, height = 4.5, units = "in", res = 300) # open file device to save funnel plot as PNG

funnel(res.noradrenaline,
  level = 0.95, contour = c(0.9, 0.95, 0.99),
  col.contour = c("gray75", "gray85", "gray95"),
  lwd = 2, cex = 1, pch = 16, studlab = TRUE, cex.studlab = 0.7)
legend(3.55, 0,
  c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
  fill = c("gray75", "gray85", "gray95"))

dev.off() # close the PNG device

## ------------------------------------------------------------------------

## fewer than 10 studies, so not including funnel plot for dopamine, serotonin, dopac, hva, hiaa (but code is there if needed in the future)

# funnel(res.dopamine,
#   level = 0.95, contour = c(0.9, 0.95, 0.99),
#   col.contour = c("gray75", "gray85", "gray95"),
#   lwd = 2, cex = 1, pch = 16, studlab = TRUE, cex.studlab = 0.7)
# legend(1, 0,
#   c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
#   fill = c("gray75", "gray85", "gray95"))

# funnel(res.serotonin,
#   level = 0.95, contour = c(0.9, 0.95, 0.99),
#   col.contour = c("gray75", "gray85", "gray95"),
#   lwd = 2, cex = 1, pch = 16, studlab = TRUE, cex.studlab = 0.7)
# legend(7.15, 0,
#   c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
#   fill = c("gray75", "gray85", "gray95"))

# funnel(res.dopac,
#   level = 0.95, contour = c(0.9, 0.95, 0.99),
#   col.contour = c("gray75", "gray85", "gray95"),
#   lwd = 2, cex = 1, pch = 16, studlab = TRUE, cex.studlab = 0.7)
# legend(0.58, 0,
#   c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
#   fill = c("gray75", "gray85", "gray95"))

# funnel(res.hva,
#   level = 0.95, contour = c(0.9, 0.95, 0.99),
#   col.contour = c("gray75", "gray85", "gray95"),
#   lwd = 2, cex = 1, pch = 16, studlab = TRUE, cex.studlab = 0.7)
# legend(0.28, 0,
#   c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
#   fill = c("gray75", "gray85", "gray95"))

# funnel(res.hiaa,
#   level = 0.95, contour = c(0.9, 0.95, 0.99),
#   col.contour = c("gray75", "gray85", "gray95"),
#   lwd = 2, cex = 1, pch = 16, studlab = TRUE, cex.studlab = 0.7)
# legend(-0.03, 0,
#   c("0.1 > p > 0.05", "0.05 > p > 0.01", "< 0.01"),
#   fill = c("gray75", "gray85", "gray95"))

## ------------------------------------------------------------------------

library(magick) # for reading PNGs

# list the PNG files
png_files <- list.files("/Users/teresacorreiadesa/R/Meta-Analysis/outputs/meta-analysis/funnel-plots", pattern = "\\.png$", full.names = TRUE)

# check if any PNG files were found
if (length(png_files) == 0) {
  stop("No PNG files found in the specified directory.")
}

# read the PNGs as raster images using magick
png_images <- lapply(png_files, image_read)

# convert image objects to grobs for cowplot
png_grobs <- lapply(png_images, function(img){
  grid::rasterGrob(image = as.raster(img))
})


# arrange the plots using plot_grid
## 2 rows, automatically determining the number of columns
combined_funnel_plots <- plot_grid(plotlist = png_grobs, 
                                   nrow = 1, 
                                   ncol = 2,
                                   labels = c("A - Adrenaline", "B - Noradrenaline"),
                                   label_size = 6,
                                   hjust = 0)



# save the combined plot
save_plot("outputs/meta-analysis/funnel-plots/combined_funnel_plots.pdf", combined_funnel_plots, base_width = 6.6, base_height = 2) # adjust width and height as needed

# display the combined plot
combined_funnel_plots

## ------------------------------------------------------------------------
#' ## Egger’s test 
## ------------------------------------------------------------------------
#' - *metabias*: Test for funnel plot asymmetry, based on rank correlation or linear regression method.
#' - Use Egger’s test to check publication bias, can take string ‘Egger’ or ‘linreg’.
## ------------------------------------------------------------------------
eggers.test = function(x) {

    # Validate
    x = x

    if (x$k < 10) {

        warning(paste("Your meta-analysis contains k =",
                      x$k, "studies. Egger's test may lack the statistical power to detect bias when the number of studies is small (i.e., k<10)."))

    }

    if (class(x)[1] %in% c("meta", "metabin", "metagen", "metacont", "metacor", "metainc", "metaprop")) {

        # Conduct metabias
        eggers = meta::metabias(x, k.min = 3, method = "linreg")

        # Get Intercept
        intercept = as.numeric(eggers$estimate[1])

        # Get SE
        se = as.numeric(eggers$estimate[2])

        # Calculate 95CI
        llci = intercept - qnorm(0.975) * se
        ulci = intercept + qnorm(0.975) * se

        # Get t
        t = as.numeric(eggers$statistic)

        # Get df
        df = as.numeric(eggers$parameters)

        # Get p
        p = as.numeric(eggers$p.value)

        # Make df
        returnlist = list(intercept = intercept,
                          llci = llci,
                          ulci = ulci,
                          t = t,
                          p = p,
                          meta.obj = x)

    } else {

        stop("x must be of type 'metabin', 'metagen', 'metacont', 'metainc' or 'metaprop'")

    }

    class(returnlist) = "eggers.test"

    return(returnlist)

}

dir.create("outputs/meta-analysis/eggers-plots", recursive = TRUE) # create directory to save forest plots

## ------------------------------------------------------------------------
png("outputs/meta-analysis/eggers-plots/regression-line-egger-adrenaline.png", width = 7, height = 4.5, units = "in", res = 300) # open file device to save egger's plot as PNG

metabias(res.adrenaline, method.bias = 'linreg', plotit = T)
eggers.test(res.adrenaline)

dev.off()
## ------------------------------------------------------------------------
png("outputs/meta-analysis/eggers-plots/regression-line-egger-noradrenaline.png", width = 7, height = 4.5, units = "in", res = 300) # open file device to save egger's plot as PNG

metabias(res.noradrenaline, method.bias = 'linreg', plotit = T)
eggers.test(res.noradrenaline)

dev.off()

## ------------------------------------------------------------------------
## fewer than 10 studies, so not including Egger's test for dopamine, serotonin, dopac, hva, hiaa (but code is there if needed in the future)

# metabias(res.dopamine, method.bias = 'linreg', plotit = T) # using a minimum of 5 studies to test for funnel plot asymmetry
# eggers.test(res.dopamine)

# metabias(res.serotonin, method.bias = 'linreg', plotit = T)
# eggers.test(res.serotonin)

# metabias(res.dopac, method.bias = 'linreg', plotit = T)

# metabias(res.hva, method.bias = 'linreg', plotit = T)
# eggers.test(res.hva)

# metabias(res.hiaa, method.bias = 'linreg', plotit = T)

# ## ------------------------------------------------------------------------

# list the PNG files
png_files <- list.files("/Users/teresacorreiadesa/R/Meta-Analysis/outputs/meta-analysis/eggers-plots", pattern = "\\.png$", full.names = TRUE)

# check if any PNG files were found
if (length(png_files) == 0) {
  stop("No PNG files found in the specified directory.")
}

# read the PNGs as raster images using magick
png_images <- lapply(png_files, image_read)

# convert image objects to grobs for cowplot
png_grobs <- lapply(png_images, function(img){
  grid::rasterGrob(image = as.raster(img))
})

# arrange the plots using plot_grid
## 2 rows, automatically determining the number of columns
combined_regression_lines <- plot_grid(plotlist = png_grobs, 
                                   nrow = 1, 
                                   ncol = 2,
                                   labels = c("A - Adrenaline", "B - Noradrenaline"),
                                   label_size = 6,
                                   hjust = 0)

# save the combined plot
save_plot("outputs/meta-analysis/eggers-plots/combined_regression_lines.pdf", combined_regression_lines, base_width = 6.6, base_height = 2) # Adjust width and height as needed

# Display the combined plot
combined_regression_lines
## ------------------------------------------------------------------------
#' ## Subgroup analysis
## ------------------------------------------------------------------------

#' ### dopamine
## ------------------------------------------------------------------------
dopamine.Sample <- update(res.dopamine,
       subgroup = Sample,
       tau.common = TRUE, # as number of studies in subgroups is small (<5)
       subset = !is.na(Sample)) # ignore missing data

dopamine.Sample

sa.tbl.da.s <- data.frame(
           dopamine.Sample$subgroup.levels,
           dopamine.Sample$k.w,
           dopamine.Sample$n.e.w, 
           dopamine.Sample$n.c.w, 
           dopamine.Sample$TE.common.w, 
           dopamine.Sample$lower.common.w, 
           dopamine.Sample$upper.common.w, 
           dopamine.Sample$pval.common.w, 
           dopamine.Sample$I2.w, 
           dopamine.Sample$lower.I2.w, 
           dopamine.Sample$upper.I2.w
           )

dopamine.Sample$pval.Q.b.common

## ------------------------------------------------------------------------
dopamine.UrineProtocol <- update(res.dopamine,
       subgroup = UrineProtocol,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(UrineProtocol)) # ignore missing data


dopamine.UrineProtocol

sa.tbl.da.up <- data.frame(
           dopamine.UrineProtocol$subgroup.levels,
           dopamine.UrineProtocol$k.w,
           dopamine.UrineProtocol$n.e.w, 
           dopamine.UrineProtocol$n.c.w, 
           dopamine.UrineProtocol$TE.common.w, 
           dopamine.UrineProtocol$lower.common.w, 
           dopamine.UrineProtocol$upper.common.w, 
           dopamine.UrineProtocol$pval.common.w, 
           dopamine.UrineProtocol$I2.w, 
           dopamine.UrineProtocol$lower.I2.w, 
           dopamine.UrineProtocol$upper.I2.w
           )

dopamine.UrineProtocol$pval.Q.b.common

## ------------------------------------------------------------------------
dopamine.BloodComponents <- update(res.dopamine,
       subgroup = BloodComponents,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(BloodComponents)) # ignore missing data

dopamine.BloodComponents

sa.tbl.da.bc <- data.frame(
           dopamine.BloodComponents$subgroup.levels,
           dopamine.BloodComponents$k.w,
           dopamine.BloodComponents$n.e.w, 
           dopamine.BloodComponents$n.c.w, 
           dopamine.BloodComponents$TE.common.w, 
           dopamine.BloodComponents$lower.common.w, 
           dopamine.BloodComponents$upper.common.w, 
           dopamine.BloodComponents$pval.common.w, 
           dopamine.BloodComponents$I2.w, 
           dopamine.BloodComponents$lower.I2.w, 
           dopamine.BloodComponents$upper.I2.w
           )

dopamine.BloodComponents$pval.Q.b.common

## ------------------------------------------------------------------------
dopamine.Country <- update(res.dopamine,
       subgroup = Country,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(Country)) # ignore missing data

dopamine.Country

sa.tbl.da.c <- data.frame(
           dopamine.Country$subgroup.levels,
           dopamine.Country$k.w,
           dopamine.Country$n.e.w, 
           dopamine.Country$n.c.w, 
           dopamine.Country$TE.common.w, 
           dopamine.Country$lower.common.w, 
           dopamine.Country$upper.common.w, 
           dopamine.Country$pval.common.w, 
           dopamine.Country$I2.w, 
           dopamine.Country$lower.I2.w, 
           dopamine.Country$upper.I2.w
           )

dopamine.Country$pval.Q.b.common

## ------------------------------------------------------------------------
dopamine.Sex <- update(res.dopamine,
       subgroup = Sex,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(Sex)) # ignore missing data

dopamine.Sex

sa.tbl.da.sex <- data.frame(
           dopamine.Sex$subgroup.levels,
           dopamine.Sex$k.w,
           dopamine.Sex$n.e.w, 
           dopamine.Sex$n.c.w, 
           dopamine.Sex$TE.common.w, 
           dopamine.Sex$lower.common.w, 
           dopamine.Sex$upper.common.w, 
           dopamine.Sex$pval.common.w, 
           dopamine.Sex$I2.w, 
           dopamine.Sex$lower.I2.w, 
           dopamine.Sex$upper.I2.w
           )

dopamine.Sex$pval.Q.b.common

## ------------------------------------------------------------------------
dopamine.Med <- update(res.dopamine,
       subgroup = ADHDtakesMed,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDtakesMed)) # ignore missing data

dopamine.Med

sa.tbl.da.med <- data.frame(
           dopamine.Med$subgroup.levels,
           dopamine.Med$k.w,
           dopamine.Med$n.e.w, 
           dopamine.Med$n.c.w, 
           dopamine.Med$TE.common.w, 
           dopamine.Med$lower.common.w, 
           dopamine.Med$upper.common.w, 
           dopamine.Med$pval.common.w, 
           dopamine.Med$I2.w, 
           dopamine.Med$lower.I2.w, 
           dopamine.Med$upper.I2.w
           )

dopamine.Med$pval.Q.b.common

## ------------------------------------------------------------------------
dopamine.ADHDassess <- update(res.dopamine,
       subgroup = ADHDassess,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDassess)) # ignore missing data

dopamine.ADHDassess

sa.tbl.da.aa <- data.frame(
           dopamine.ADHDassess$subgroup.levels,
           dopamine.ADHDassess$k.w,
           dopamine.ADHDassess$n.e.w, 
           dopamine.ADHDassess$n.c.w, 
           dopamine.ADHDassess$TE.common.w, 
           dopamine.ADHDassess$lower.common.w, 
           dopamine.ADHDassess$upper.common.w, 
           dopamine.ADHDassess$pval.common.w, 
           dopamine.ADHDassess$I2.w, 
           dopamine.ADHDassess$lower.I2.w, 
           dopamine.ADHDassess$upper.I2.w
           )

dopamine.ADHDassess$pval.Q.b.common

## ------------------------------------------------------------------------
dopamine.AssayMethod <- update(res.dopamine,
       subgroup = AssayMethod,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(AssayMethod)) # ignore missing data

dopamine.AssayMethod

sa.tbl.da.am <- data.frame(
           dopamine.AssayMethod$subgroup.levels,
           dopamine.AssayMethod$k.w,
           dopamine.AssayMethod$n.e.w, 
           dopamine.AssayMethod$n.c.w, 
           dopamine.AssayMethod$TE.common.w, 
           dopamine.AssayMethod$lower.common.w, 
           dopamine.AssayMethod$upper.common.w, 
           dopamine.AssayMethod$pval.common.w, 
           dopamine.AssayMethod$I2.w, 
           dopamine.AssayMethod$lower.I2.w, 
           dopamine.AssayMethod$upper.I2.w
           )

dopamine.AssayMethod$pval.Q.b.common

## ------------------------------------------------------------------------
dopamine.StudyQuality <- update(res.dopamine,
       subgroup = StudyQuality,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(StudyQuality)) # ignore missing data

dopamine.StudyQuality

sa.tbl.da.sq <- data.frame(
           dopamine.StudyQuality$subgroup.levels,
           dopamine.StudyQuality$k.w,
           dopamine.StudyQuality$n.e.w, 
           dopamine.StudyQuality$n.c.w, 
           dopamine.StudyQuality$TE.common.w, 
           dopamine.StudyQuality$lower.common.w, 
           dopamine.StudyQuality$upper.common.w, 
           dopamine.StudyQuality$pval.common.w, 
           dopamine.StudyQuality$I2.w, 
           dopamine.StudyQuality$lower.I2.w, 
           dopamine.StudyQuality$upper.I2.w
           )

dopamine.StudyQuality$pval.Q.b.common

## ------------------------------------------------------------------------
sa.tbls.da <- list(sa.tbl.da.s, sa.tbl.da.up, sa.tbl.da.bc, sa.tbl.da.c, sa.tbl.da.sex, sa.tbl.da.med, sa.tbl.da.aa, sa.tbl.da.am, sa.tbl.da.sq)

dir.create("outputs/subgroup-analysis", recursive = TRUE) # create directory to save subgroup analysis tables

library(writexl)
write_xlsx(sa.tbls.da, "/Users/teresacorreiadesa/R/Meta-Analysis/outputs/subgroup-analysis/sa.tbls.da.xlsx")


#' ### adrenaline
## ------------------------------------------------------------------------
adrenaline.Sample <- update(res.adrenaline,
       subgroup = Sample,
       tau.common = TRUE, # as number of studies in subgroups is small (<5)
       subset = !is.na(Sample)) # ignore missing data

adrenaline.Sample

sa.tbl.ad.s <- data.frame(
           adrenaline.Sample$subgroup.levels,
           adrenaline.Sample$k.w,
           adrenaline.Sample$n.e.w, 
           adrenaline.Sample$n.c.w, 
           adrenaline.Sample$TE.common.w, 
           adrenaline.Sample$lower.common.w, 
           adrenaline.Sample$upper.common.w, 
           adrenaline.Sample$pval.common.w, 
           adrenaline.Sample$I2.w, 
           adrenaline.Sample$lower.I2.w, 
           adrenaline.Sample$upper.I2.w
           )

adrenaline.Sample$pval.Q.b.common

## ------------------------------------------------------------------------
adrenaline.UrineProtocol <- update(res.adrenaline,
       subgroup = UrineProtocol,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(UrineProtocol)) # ignore missing data

adrenaline.UrineProtocol

sa.tbl.ad.up <- data.frame(
           adrenaline.UrineProtocol$subgroup.levels,
           adrenaline.UrineProtocol$k.w,
           adrenaline.UrineProtocol$n.e.w, 
           adrenaline.UrineProtocol$n.c.w, 
           adrenaline.UrineProtocol$TE.common.w, 
           adrenaline.UrineProtocol$lower.common.w, 
           adrenaline.UrineProtocol$upper.common.w, 
           adrenaline.UrineProtocol$pval.common.w, 
           adrenaline.UrineProtocol$I2.w, 
           adrenaline.UrineProtocol$lower.I2.w, 
           adrenaline.UrineProtocol$upper.I2.w
           )

adrenaline.UrineProtocol$pval.Q.b.common

## ------------------------------------------------------------------------
adrenaline.BloodComponents <- update(res.adrenaline,
       subgroup = BloodComponents,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(BloodComponents)) # ignore missing data

adrenaline.BloodComponents

sa.tbl.ad.bc <- data.frame(
           adrenaline.BloodComponents$subgroup.levels,
           adrenaline.BloodComponents$k.w,
           adrenaline.BloodComponents$n.e.w, 
           adrenaline.BloodComponents$n.c.w, 
           adrenaline.BloodComponents$TE.common.w, 
           adrenaline.BloodComponents$lower.common.w, 
           adrenaline.BloodComponents$upper.common.w, 
           adrenaline.BloodComponents$pval.common.w, 
           adrenaline.BloodComponents$I2.w, 
           adrenaline.BloodComponents$lower.I2.w, 
           adrenaline.BloodComponents$upper.I2.w
           )

adrenaline.BloodComponents$pval.Q.b.common

## ------------------------------------------------------------------------
adrenaline.Sex <- update(res.adrenaline,
       subgroup = Sex,
       tau.common = FALSE, # as number of studies in subgroups >5
       subset = !is.na(Sex)) # ignore missing data

adrenaline.Sex

sa.tbl.ad.sex <- data.frame(
           adrenaline.Sex$subgroup.levels,
           adrenaline.Sex$k.w,
           adrenaline.Sex$n.e.w, 
           adrenaline.Sex$n.c.w, 
           adrenaline.Sex$TE.random.w, 
           adrenaline.Sex$lower.random.w, 
           adrenaline.Sex$upper.random.w, 
           adrenaline.Sex$pval.random.w, 
           adrenaline.Sex$I2.w, 
           adrenaline.Sex$lower.I2.w, 
           adrenaline.Sex$upper.I2.w
           )

adrenaline.Sex$pval.Q.b.random

## ------------------------------------------------------------------------
adrenaline.Med <- update(res.adrenaline,
       subgroup = ADHDtakesMed,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDtakesMed)) # ignore missing data

adrenaline.Med

sa.tbl.ad.med <- data.frame(
           adrenaline.Med$subgroup.levels,
           adrenaline.Med$k.w,
           adrenaline.Med$n.e.w, 
           adrenaline.Med$n.c.w, 
           adrenaline.Med$TE.common.w, 
           adrenaline.Med$lower.common.w, 
           adrenaline.Med$upper.common.w, 
           adrenaline.Med$pval.common.w, 
           adrenaline.Med$I2.w, 
           adrenaline.Med$lower.I2.w, 
           adrenaline.Med$upper.I2.w
           )

adrenaline.Med$pval.Q.b.common

## ------------------------------------------------------------------------
adrenaline.ADHDassess <- update(res.adrenaline,
       subgroup = ADHDassess,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDassess)) # ignore missing data

adrenaline.ADHDassess

sa.tbl.ad.aa <- data.frame(
           adrenaline.ADHDassess$subgroup.levels,
           adrenaline.ADHDassess$k.w,
           adrenaline.ADHDassess$n.e.w, 
           adrenaline.ADHDassess$n.c.w, 
           adrenaline.ADHDassess$TE.common.w, 
           adrenaline.ADHDassess$lower.common.w, 
           adrenaline.ADHDassess$upper.common.w, 
           adrenaline.ADHDassess$pval.common.w, 
           adrenaline.ADHDassess$I2.w, 
           adrenaline.ADHDassess$lower.I2.w, 
           adrenaline.ADHDassess$upper.I2.w
           )

adrenaline.ADHDassess$pval.Q.b.common

## ------------------------------------------------------------------------
adrenaline.AssayMethod <- update(res.adrenaline,
       subgroup = AssayMethod,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(AssayMethod)) # ignore missing data

adrenaline.AssayMethod

sa.tbl.ad.am <- data.frame(
           adrenaline.AssayMethod$subgroup.levels,
           adrenaline.AssayMethod$k.w,
           adrenaline.AssayMethod$n.e.w, 
           adrenaline.AssayMethod$n.c.w, 
           adrenaline.AssayMethod$TE.common.w, 
           adrenaline.AssayMethod$lower.common.w, 
           adrenaline.AssayMethod$upper.common.w, 
           adrenaline.AssayMethod$pval.common.w, 
           adrenaline.AssayMethod$I2.w, 
           adrenaline.AssayMethod$lower.I2.w, 
           adrenaline.AssayMethod$upper.I2.w
           )

adrenaline.AssayMethod$pval.Q.b.common

## ------------------------------------------------------------------------
adrenaline.StudyQuality <- update(res.adrenaline,
       subgroup = StudyQuality,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(StudyQuality)) # ignore missing data

adrenaline.StudyQuality

sa.tbl.ad.sq <- data.frame(
           adrenaline.StudyQuality$subgroup.levels,
           adrenaline.StudyQuality$k.w,
           adrenaline.StudyQuality$n.e.w, 
           adrenaline.StudyQuality$n.c.w, 
           adrenaline.StudyQuality$TE.common.w, 
           adrenaline.StudyQuality$lower.common.w, 
           adrenaline.StudyQuality$upper.common.w, 
           adrenaline.StudyQuality$pval.common.w, 
           adrenaline.StudyQuality$I2.w, 
           adrenaline.StudyQuality$lower.I2.w, 
           adrenaline.StudyQuality$upper.I2.w
           )

adrenaline.StudyQuality$pval.Q.b.common

## ------------------------------------------------------------------------
sa.tbls.ad <- list(sa.tbl.ad.s, sa.tbl.ad.up, sa.tbl.ad.bc, sa.tbl.ad.sex, sa.tbl.ad.med, sa.tbl.ad.aa, sa.tbl.ad.am, sa.tbl.ad.sq)

library(writexl)
write_xlsx(sa.tbls.ad, '/Users/teresacorreiadesa/R/Meta-Analysis/outputs/subgroup-analysis/sa.tbls.ad.xlsx')


#' ### noradrenaline
## ------------------------------------------------------------------------
noradrenaline.Sample <- update(res.noradrenaline,
       subgroup = Sample,
       tau.common = FALSE, # as number of studies in subgroups is >5
       subset = !is.na(Sample)) # ignore missing data

noradrenaline.Sample

sa.tbl.na.s <- data.frame(
           noradrenaline.Sample$subgroup.levels,
           noradrenaline.Sample$k.w,
           noradrenaline.Sample$n.e.w, 
           noradrenaline.Sample$n.c.w, 
           noradrenaline.Sample$TE.random.w, 
           noradrenaline.Sample$lower.random.w, 
           noradrenaline.Sample$upper.random.w, 
           noradrenaline.Sample$pval.random.w, 
           noradrenaline.Sample$I2.w, 
           noradrenaline.Sample$lower.I2.w, 
           noradrenaline.Sample$upper.I2.w
           )

noradrenaline.Sample$pval.Q.b.random

## ------------------------------------------------------------------------
noradrenaline.UrineProtocol <- update(res.noradrenaline,
       subgroup = UrineProtocol,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(UrineProtocol)) # ignore missing data

noradrenaline.UrineProtocol

sa.tbl.na.up <- data.frame(
           noradrenaline.UrineProtocol$subgroup.levels,
           noradrenaline.UrineProtocol$k.w,
           noradrenaline.UrineProtocol$n.e.w, 
           noradrenaline.UrineProtocol$n.c.w, 
           noradrenaline.UrineProtocol$TE.common.w, 
           noradrenaline.UrineProtocol$lower.common.w, 
           noradrenaline.UrineProtocol$upper.common.w, 
           noradrenaline.UrineProtocol$pval.common.w, 
           noradrenaline.UrineProtocol$I2.w, 
           noradrenaline.UrineProtocol$lower.I2.w, 
           noradrenaline.UrineProtocol$upper.I2.w
           )

noradrenaline.UrineProtocol$pval.Q.b.common

## ------------------------------------------------------------------------
noradrenaline.BloodComponents <- update(res.noradrenaline,
       subgroup = BloodComponents,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(BloodComponents)) # ignore missing data

noradrenaline.BloodComponents

sa.tbl.na.bc <- data.frame(
           noradrenaline.BloodComponents$subgroup.levels,
           noradrenaline.BloodComponents$k.w,
           noradrenaline.BloodComponents$n.e.w, 
           noradrenaline.BloodComponents$n.c.w, 
           noradrenaline.BloodComponents$TE.common.w, 
           noradrenaline.BloodComponents$lower.common.w, 
           noradrenaline.BloodComponents$upper.common.w, 
           noradrenaline.BloodComponents$pval.common.w, 
           noradrenaline.BloodComponents$I2.w, 
           noradrenaline.BloodComponents$lower.I2.w, 
           noradrenaline.BloodComponents$upper.I2.w
           )

noradrenaline.BloodComponents$pval.Q.b.common

## ------------------------------------------------------------------------
noradrenaline.Country <- update(res.noradrenaline,
       subgroup = Country,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(Country)) # ignore missing data

noradrenaline.Country

sa.tbl.na.c <- data.frame(
           noradrenaline.Country$subgroup.levels,
           noradrenaline.Country$k.w,
           noradrenaline.Country$n.e.w, 
           noradrenaline.Country$n.c.w, 
           noradrenaline.Country$TE.common.w, 
           noradrenaline.Country$lower.common.w, 
           noradrenaline.Country$upper.common.w, 
           noradrenaline.Country$pval.common.w, 
           noradrenaline.Country$I2.w, 
           noradrenaline.Country$lower.I2.w, 
           noradrenaline.Country$upper.I2.w
           )

noradrenaline.Country$pval.Q.b.common

## ------------------------------------------------------------------------
noradrenaline.Sex <- update(res.noradrenaline,
       subgroup = Sex,
       tau.common = FALSE, # as number of studies in subgroups is >5
       subset = !is.na(Sex)) # ignore missing data

noradrenaline.Sex

sa.tbl.na.sex <- data.frame(
           noradrenaline.Sex$subgroup.levels,
           noradrenaline.Sex$k.w,
           noradrenaline.Sex$n.e.w, 
           noradrenaline.Sex$n.c.w, 
           noradrenaline.Sex$TE.random.w, 
           noradrenaline.Sex$lower.random.w, 
           noradrenaline.Sex$upper.random.w, 
           noradrenaline.Sex$pval.random.w, 
           noradrenaline.Sex$I2.w, 
           noradrenaline.Sex$lower.I2.w, 
           noradrenaline.Sex$upper.I2.w
           )

noradrenaline.Sex$pval.Q.b.random

## ------------------------------------------------------------------------
noradrenaline.Med <- update(res.noradrenaline,
       subgroup = ADHDtakesMed,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDtakesMed)) # ignore missing data

noradrenaline.Med

sa.tbl.na.med <- data.frame(
           noradrenaline.Med$subgroup.levels,
           noradrenaline.Med$k.w,
           noradrenaline.Med$n.e.w, 
           noradrenaline.Med$n.c.w, 
           noradrenaline.Med$TE.common.w, 
           noradrenaline.Med$lower.common.w, 
           noradrenaline.Med$upper.common.w, 
           noradrenaline.Med$pval.common.w, 
           noradrenaline.Med$I2.w, 
           noradrenaline.Med$lower.I2.w, 
           noradrenaline.Med$upper.I2.w
           )

noradrenaline.Med$pval.Q.b.common

## ------------------------------------------------------------------------
noradrenaline.ADHDassess <- update(res.noradrenaline,
       subgroup = ADHDassess,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDassess)) # ignore missing data

noradrenaline.ADHDassess

sa.tbl.na.aa <- data.frame(
           noradrenaline.ADHDassess$subgroup.levels,
           noradrenaline.ADHDassess$k.w,
           noradrenaline.ADHDassess$n.e.w, 
           noradrenaline.ADHDassess$n.c.w, 
           noradrenaline.ADHDassess$TE.common.w, 
           noradrenaline.ADHDassess$lower.common.w, 
           noradrenaline.ADHDassess$upper.common.w, 
           noradrenaline.ADHDassess$pval.common.w, 
           noradrenaline.ADHDassess$I2.w, 
           noradrenaline.ADHDassess$lower.I2.w, 
           noradrenaline.ADHDassess$upper.I2.w
           )

noradrenaline.ADHDassess$pval.Q.b.common

## ------------------------------------------------------------------------
noradrenaline.AssayMethod <- update(res.noradrenaline,
       subgroup = AssayMethod,
       tau.common = FALSE, # as number of studies in subgroups is >5
       subset = !is.na(AssayMethod)) # ignore missing data

noradrenaline.AssayMethod

sa.tbl.na.am <- data.frame(
           noradrenaline.AssayMethod$subgroup.levels,
           noradrenaline.AssayMethod$k.w,
           noradrenaline.AssayMethod$n.e.w, 
           noradrenaline.AssayMethod$n.c.w, 
           noradrenaline.AssayMethod$TE.random.w, 
           noradrenaline.AssayMethod$lower.random.w, 
           noradrenaline.AssayMethod$upper.random.w, 
           noradrenaline.AssayMethod$pval.random.w, 
           noradrenaline.AssayMethod$I2.w, 
           noradrenaline.AssayMethod$lower.I2.w, 
           noradrenaline.AssayMethod$upper.I2.w
           )

noradrenaline.AssayMethod$pval.Q.b.random

## ------------------------------------------------------------------------
noradrenaline.StudyQuality <- update(res.noradrenaline,
       subgroup = StudyQuality,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(StudyQuality)) # ignore missing data

noradrenaline.StudyQuality

sa.tbl.na.sq <- data.frame(
           noradrenaline.StudyQuality$subgroup.levels,
           noradrenaline.StudyQuality$k.w,
           noradrenaline.StudyQuality$n.e.w, 
           noradrenaline.StudyQuality$n.c.w, 
           noradrenaline.StudyQuality$TE.common.w, 
           noradrenaline.StudyQuality$lower.common.w, 
           noradrenaline.StudyQuality$upper.common.w, 
           noradrenaline.StudyQuality$pval.common.w, 
           noradrenaline.StudyQuality$I2.w, 
           noradrenaline.StudyQuality$lower.I2.w, 
           noradrenaline.StudyQuality$upper.I2.w
           )

noradrenaline.StudyQuality$pval.Q.b.common

## ------------------------------------------------------------------------
sa.tbls.na <- list(sa.tbl.na.s, sa.tbl.na.up, sa.tbl.na.bc, sa.tbl.na.c, sa.tbl.na.sex, sa.tbl.na.med, sa.tbl.na.aa, sa.tbl.na.am, sa.tbl.na.sq)

library(writexl)
write_xlsx(sa.tbls.na, '/Users/teresacorreiadesa/R/Meta-Analysis/outputs/subgroup-analysis/sa.tbls.na.xlsx')


#' ### serotonin
## ------------------------------------------------------------------------
serotonin.Sample <- update(res.serotonin,
       subgroup = Sample,
       tau.common = TRUE, # as number of studies in subgroups is small (<5)
       subset = !is.na(Sample)) # ignore missing data

serotonin.Sample

sa.tbl.5ht.s <- data.frame(
           serotonin.Sample$subgroup.levels,
           serotonin.Sample$k.w,
           serotonin.Sample$n.e.w, 
           serotonin.Sample$n.c.w, 
           serotonin.Sample$TE.common.w, 
           serotonin.Sample$lower.common.w, 
           serotonin.Sample$upper.common.w, 
           serotonin.Sample$pval.common.w, 
           serotonin.Sample$I2.w, 
           serotonin.Sample$lower.I2.w, 
           serotonin.Sample$upper.I2.w
           )

serotonin.Sample$pval.Q.b.common

## ------------------------------------------------------------------------
serotonin.BloodComponents <- update(res.serotonin,
       subgroup = BloodComponents,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(BloodComponents)) # ignore missing data

serotonin.BloodComponents

sa.tbl.5ht.bc <- data.frame(
           serotonin.BloodComponents$subgroup.levels,
           serotonin.BloodComponents$k.w,
           serotonin.BloodComponents$n.e.w, 
           serotonin.BloodComponents$n.c.w, 
           serotonin.BloodComponents$TE.common.w, 
           serotonin.BloodComponents$lower.common.w, 
           serotonin.BloodComponents$upper.common.w, 
           serotonin.BloodComponents$pval.common.w, 
           serotonin.BloodComponents$I2.w, 
           serotonin.BloodComponents$lower.I2.w, 
           serotonin.BloodComponents$upper.I2.w
           )

serotonin.BloodComponents$pval.Q.b.common

## ------------------------------------------------------------------------
serotonin.Country <- update(res.serotonin,
       subgroup = Country,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(Country)) # ignore missing data

serotonin.Country

sa.tbl.5ht.c <- data.frame(
           serotonin.Country$subgroup.levels,
           serotonin.Country$k.w,
           serotonin.Country$n.e.w, 
           serotonin.Country$n.c.w, 
           serotonin.Country$TE.common.w, 
           serotonin.Country$lower.common.w, 
           serotonin.Country$upper.common.w, 
           serotonin.Country$pval.common.w, 
           serotonin.Country$I2.w, 
           serotonin.Country$lower.I2.w, 
           serotonin.Country$upper.I2.w
           )

serotonin.Country$pval.Q.b.common
 
## ------------------------------------------------------------------------
serotonin.Sex <- update(res.serotonin,
       subgroup = Sex,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(Sex)) # ignore missing data

serotonin.Sex

sa.tbl.5ht.sex <- data.frame(
           serotonin.Sex$subgroup.levels,
           serotonin.Sex$k.w,
           serotonin.Sex$n.e.w, 
           serotonin.Sex$n.c.w, 
           serotonin.Sex$TE.common.w, 
           serotonin.Sex$lower.common.w, 
           serotonin.Sex$upper.common.w, 
           serotonin.Sex$pval.common.w, 
           serotonin.Sex$I2.w, 
           serotonin.Sex$lower.I2.w, 
           serotonin.Sex$upper.I2.w
           )

serotonin.Sex$pval.Q.b.common

## ------------------------------------------------------------------------
serotonin.Med <- update(res.serotonin,
       subgroup = ADHDtakesMed,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDtakesMed)) # ignore missing data

serotonin.Med

sa.tbl.5ht.med <- data.frame(
           serotonin.Med$subgroup.levels,
           serotonin.Med$k.w,
           serotonin.Med$n.e.w, 
           serotonin.Med$n.c.w, 
           serotonin.Med$TE.common.w, 
           serotonin.Med$lower.common.w, 
           serotonin.Med$upper.common.w, 
           serotonin.Med$pval.common.w, 
           serotonin.Med$I2.w, 
           serotonin.Med$lower.I2.w, 
           serotonin.Med$upper.I2.w
           )

serotonin.Med$pval.Q.b.common

## ------------------------------------------------------------------------
serotonin.ADHDassess <- update(res.serotonin,
       subgroup = ADHDassess,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDassess)) # ignore missing data

serotonin.ADHDassess

sa.tbl.5ht.aa <- data.frame(
           serotonin.ADHDassess$subgroup.levels,
           serotonin.ADHDassess$k.w,
           serotonin.ADHDassess$n.e.w, 
           serotonin.ADHDassess$n.c.w, 
           serotonin.ADHDassess$TE.common.w, 
           serotonin.ADHDassess$lower.common.w, 
           serotonin.ADHDassess$upper.common.w, 
           serotonin.ADHDassess$pval.common.w, 
           serotonin.ADHDassess$I2.w, 
           serotonin.ADHDassess$lower.I2.w, 
           serotonin.ADHDassess$upper.I2.w
           )

serotonin.ADHDassess$pval.Q.b.common

## ------------------------------------------------------------------------
serotonin.AssayMethod <- update(res.serotonin,
       subgroup = AssayMethod,
       tau.common = FALSE, # as number of studies in subgroups is >5
       subset = !is.na(AssayMethod)) # ignore missing data

serotonin.AssayMethod

sa.tbl.5ht.am <- data.frame(
           serotonin.AssayMethod$subgroup.levels,
           serotonin.AssayMethod$k.w,
           serotonin.AssayMethod$n.e.w, 
           serotonin.AssayMethod$n.c.w, 
           serotonin.AssayMethod$TE.random.w, 
           serotonin.AssayMethod$lower.random.w, 
           serotonin.AssayMethod$upper.random.w, 
           serotonin.AssayMethod$pval.random.w, 
           serotonin.AssayMethod$I2.w, 
           serotonin.AssayMethod$lower.I2.w, 
           serotonin.AssayMethod$upper.I2.w
           )

serotonin.AssayMethod$pval.Q.b.random

## ------------------------------------------------------------------------
serotonin.StudyQuality <- update(res.serotonin,
       subgroup = StudyQuality,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(StudyQuality)) # ignore missing data

serotonin.StudyQuality

sa.tbl.5ht.sq <- data.frame(
           serotonin.StudyQuality$subgroup.levels,
           serotonin.StudyQuality$k.w,
           serotonin.StudyQuality$n.e.w, 
           serotonin.StudyQuality$n.c.w, 
           serotonin.StudyQuality$TE.common.w, 
           serotonin.StudyQuality$lower.common.w, 
           serotonin.StudyQuality$upper.common.w, 
           serotonin.StudyQuality$pval.common.w, 
           serotonin.StudyQuality$I2.w, 
           serotonin.StudyQuality$lower.I2.w, 
           serotonin.StudyQuality$upper.I2.w
           )

serotonin.StudyQuality$pval.Q.b.common

## ------------------------------------------------------------------------
sa.tbls.5ht <- list(sa.tbl.5ht.s, sa.tbl.5ht.bc, sa.tbl.5ht.c, sa.tbl.5ht.sex, sa.tbl.5ht.med, sa.tbl.5ht.aa, sa.tbl.5ht.am, sa.tbl.5ht.sq)
library(writexl)

write_xlsx(sa.tbls.5ht, '/Users/teresacorreiadesa/R/Meta-Analysis/outputs/subgroup-analysis/sa.tbls.5ht.xlsx')


#' ### dopac
## ------------------------------------------------------------------------
dopac.Med <- update(res.dopac,
       subgroup = ADHDtakesMed,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDtakesMed)) # ignore missing data

dopac.Med

sa.tbl.dopac.med <- data.frame(
           dopac.Med$subgroup.levels,
           dopac.Med$k.w,
           dopac.Med$n.e.w, 
           dopac.Med$n.c.w, 
           dopac.Med$TE.common.w, 
           dopac.Med$lower.common.w, 
           dopac.Med$upper.common.w, 
           dopac.Med$pval.common.w, 
           dopac.Med$I2.w, 
           dopac.Med$lower.I2.w, 
           dopac.Med$upper.I2.w
           )

dopac.Med$pval.Q.b.common

## ------------------------------------------------------------------------
sa.tbls.dopac <- list(sa.tbl.dopac.med)

library(writexl)
write_xlsx(sa.tbls.dopac, '/Users/teresacorreiadesa/R/Meta-Analysis/outputs/subgroup-analysis/sa.tbls.dopac.xlsx')


#' ### hva
## ------------------------------------------------------------------------
hva.UrineProtocol <- update(res.hva,
       subgroup = UrineProtocol,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(UrineProtocol)) # ignore missing data

hva.UrineProtocol

sa.tbl.hva.up <- data.frame(
           hva.UrineProtocol$subgroup.levels,
           hva.UrineProtocol$k.w,
           hva.UrineProtocol$n.e.w, 
           hva.UrineProtocol$n.c.w, 
           hva.UrineProtocol$TE.common.w, 
           hva.UrineProtocol$lower.common.w, 
           hva.UrineProtocol$upper.common.w, 
           hva.UrineProtocol$pval.common.w, 
           hva.UrineProtocol$I2.w, 
           hva.UrineProtocol$lower.I2.w, 
           hva.UrineProtocol$upper.I2.w
           )

hva.UrineProtocol$pval.Q.b.common

## ------------------------------------------------------------------------
hva.Country <- update(res.hva,
       subgroup = Country,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(Country)) # ignore missing data

hva.Country

sa.tbl.hva.c <- data.frame(
           hva.Country$subgroup.levels,
           hva.Country$k.w,
           hva.Country$n.e.w, 
           hva.Country$n.c.w, 
           hva.Country$TE.common.w, 
           hva.Country$lower.common.w, 
           hva.Country$upper.common.w, 
           hva.Country$pval.common.w, 
           hva.Country$I2.w, 
           hva.Country$lower.I2.w, 
           hva.Country$upper.I2.w
           )

hva.Country$pval.Q.b.common

## ------------------------------------------------------------------------
hva.Sex <- update(res.hva,
       subgroup = Sex,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(Sex)) # ignore missing data

hva.Sex

sa.tbl.hva.sex <- data.frame(
           hva.Sex$subgroup.levels,
           hva.Sex$k.w,
           hva.Sex$n.e.w, 
           hva.Sex$n.c.w, 
           hva.Sex$TE.common.w, 
           hva.Sex$lower.common.w, 
           hva.Sex$upper.common.w, 
           hva.Sex$pval.common.w, 
           hva.Sex$I2.w, 
           hva.Sex$lower.I2.w, 
           hva.Sex$upper.I2.w
           )

hva.Sex$pval.Q.b.common

## ------------------------------------------------------------------------
hva.Med <- update(res.hva,
       subgroup = ADHDtakesMed,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDtakesMed)) # ignore missing data

hva.Med

sa.tbl.hva.med <- data.frame(
           hva.Med$subgroup.levels,
           hva.Med$k.w,
           hva.Med$n.e.w, 
           hva.Med$n.c.w, 
           hva.Med$TE.common.w, 
           hva.Med$lower.common.w, 
           hva.Med$upper.common.w, 
           hva.Med$pval.common.w, 
           hva.Med$I2.w, 
           hva.Med$lower.I2.w, 
           hva.Med$upper.I2.w
           )

hva.Med$pval.Q.b.common

## ------------------------------------------------------------------------
hva.ADHDassess <- update(res.hva,
       subgroup = ADHDassess,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDassess)) # ignore missing data

hva.ADHDassess

sa.tbl.hva.aa <- data.frame(
           hva.ADHDassess$subgroup.levels,
           hva.ADHDassess$k.w,
           hva.ADHDassess$n.e.w, 
           hva.ADHDassess$n.c.w, 
           hva.ADHDassess$TE.common.w, 
           hva.ADHDassess$lower.common.w, 
           hva.ADHDassess$upper.common.w, 
           hva.ADHDassess$pval.common.w, 
           hva.ADHDassess$I2.w, 
           hva.ADHDassess$lower.I2.w, 
           hva.ADHDassess$upper.I2.w
           )

hva.ADHDassess$pval.Q.b.common

## ------------------------------------------------------------------------
hva.AssayMethod <- update(res.hva,
       subgroup = AssayMethod,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(AssayMethod)) # ignore missing data

hva.AssayMethod

sa.tbl.hva.am <- data.frame(
           hva.AssayMethod$subgroup.levels,
           hva.AssayMethod$k.w,
           hva.AssayMethod$n.e.w, 
           hva.AssayMethod$n.c.w, 
           hva.AssayMethod$TE.common.w, 
           hva.AssayMethod$lower.common.w, 
           hva.AssayMethod$upper.common.w, 
           hva.AssayMethod$pval.common.w, 
           hva.AssayMethod$I2.w, 
           hva.AssayMethod$lower.I2.w, 
           hva.AssayMethod$upper.I2.w
           )

hva.AssayMethod$pval.Q.b.common

## ------------------------------------------------------------------------
hva.StudyQuality <- update(res.hva,
       subgroup = StudyQuality,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(StudyQuality)) # ignore missing data

hva.StudyQuality

sa.tbl.hva.sq <- data.frame(
           hva.StudyQuality$subgroup.levels,
           hva.StudyQuality$k.w,
           hva.StudyQuality$n.e.w, 
           hva.StudyQuality$n.c.w, 
           hva.StudyQuality$TE.common.w, 
           hva.StudyQuality$lower.common.w, 
           hva.StudyQuality$upper.common.w, 
           hva.StudyQuality$pval.common.w, 
           hva.StudyQuality$I2.w, 
           hva.StudyQuality$lower.I2.w, 
           hva.StudyQuality$upper.I2.w
           )

hva.StudyQuality$pval.Q.b.common

## ------------------------------------------------------------------------
sa.tbls.hva <- list(sa.tbl.hva.up, sa.tbl.hva.c, sa.tbl.hva.sex, sa.tbl.hva.med, sa.tbl.hva.aa, sa.tbl.hva.am, sa.tbl.hva.sq)

library(writexl)
write_xlsx(sa.tbls.hva, '/Users/teresacorreiadesa/R/Meta-Analysis/outputs/subgroup-analysis/sa.tbls.hva.xlsx')


#' ### 5-hiaa
## ------------------------------------------------------------------------
hiaa.Sample <- update(res.hiaa,
       subgroup = Sample,
       tau.common = TRUE, # as number of studies in subgroups is small (<5)
       subset = !is.na(Sample)) # ignore missing data

hiaa.Sample

sa.tbl.5hiaa.s <- data.frame(
           hiaa.Sample$subgroup.levels,
           hiaa.Sample$k.w,
           hiaa.Sample$n.e.w, 
           hiaa.Sample$n.c.w, 
           hiaa.Sample$TE.common.w, 
           hiaa.Sample$lower.common.w, 
           hiaa.Sample$upper.common.w, 
           hiaa.Sample$pval.common.w, 
           hiaa.Sample$I2.w, 
           hiaa.Sample$lower.I2.w, 
           hiaa.Sample$upper.I2.w
           )

hiaa.Sample$pval.Q.b.common

## ------------------------------------------------------------------------
hiaa.UrineProtocol <- update(res.hiaa,
       subgroup = UrineProtocol,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(UrineProtocol)) # ignore missing data

hiaa.UrineProtocol

sa.tbl.5hiaa.up <- data.frame(
           hiaa.UrineProtocol$subgroup.levels,
           hiaa.UrineProtocol$k.w,
           hiaa.UrineProtocol$n.e.w, 
           hiaa.UrineProtocol$n.c.w, 
           hiaa.UrineProtocol$TE.common.w, 
           hiaa.UrineProtocol$lower.common.w, 
           hiaa.UrineProtocol$upper.common.w, 
           hiaa.UrineProtocol$pval.common.w, 
           hiaa.UrineProtocol$I2.w, 
           hiaa.UrineProtocol$lower.I2.w, 
           hiaa.UrineProtocol$upper.I2.w
           )

hiaa.UrineProtocol$pval.Q.b.common

## ------------------------------------------------------------------------
hiaa.Country <- update(res.hiaa,
       subgroup = Country,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(Country)) # ignore missing data

hiaa.Country

sa.tbl.5hiaa.c <- data.frame(
           hiaa.Country$subgroup.levels,
           hiaa.Country$k.w,
           hiaa.Country$n.e.w, 
           hiaa.Country$n.c.w, 
           hiaa.Country$TE.common.w, 
           hiaa.Country$lower.common.w, 
           hiaa.Country$upper.common.w, 
           hiaa.Country$pval.common.w, 
           hiaa.Country$I2.w, 
           hiaa.Country$lower.I2.w, 
           hiaa.Country$upper.I2.w
           )

hiaa.Country$pval.Q.b.common

## ------------------------------------------------------------------------
hiaa.Sex <- update(res.hiaa,
       subgroup = Sex,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(Sex)) # ignore missing data

hiaa.Sex

sa.tbl.5hiaa.sex <- data.frame(
           hiaa.Sex$subgroup.levels,
           hiaa.Sex$k.w,
           hiaa.Sex$n.e.w, 
           hiaa.Sex$n.c.w, 
           hiaa.Sex$TE.common.w, 
           hiaa.Sex$lower.common.w, 
           hiaa.Sex$upper.common.w, 
           hiaa.Sex$pval.common.w, 
           hiaa.Sex$I2.w, 
           hiaa.Sex$lower.I2.w, 
           hiaa.Sex$upper.I2.w
           )

hiaa.Sex$pval.Q.b.common

## ------------------------------------------------------------------------
hiaa.Med <- update(res.hiaa,
       subgroup = ADHDtakesMed,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDtakesMed)) # ignore missing data

hiaa.Med

sa.tbl.5hiaa.med <- data.frame(
           hiaa.Med$subgroup.levels,
           hiaa.Med$k.w,
           hiaa.Med$n.e.w, 
           hiaa.Med$n.c.w, 
           hiaa.Med$TE.common.w, 
           hiaa.Med$lower.common.w, 
           hiaa.Med$upper.common.w, 
           hiaa.Med$pval.common.w, 
           hiaa.Med$I2.w, 
           hiaa.Med$lower.I2.w, 
           hiaa.Med$upper.I2.w
           )

hiaa.Med$pval.Q.b.common

## ------------------------------------------------------------------------
hiaa.ADHDassess <- update(res.hiaa,
       subgroup = ADHDassess,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(ADHDassess)) # ignore missing data

hiaa.ADHDassess

sa.tbl.5hiaa.aa <- data.frame(
           hiaa.ADHDassess$subgroup.levels,
           hiaa.ADHDassess$k.w,
           hiaa.ADHDassess$n.e.w, 
           hiaa.ADHDassess$n.c.w, 
           hiaa.ADHDassess$TE.common.w, 
           hiaa.ADHDassess$lower.common.w, 
           hiaa.ADHDassess$upper.common.w, 
           hiaa.ADHDassess$pval.common.w, 
           hiaa.ADHDassess$I2.w, 
           hiaa.ADHDassess$lower.I2.w, 
           hiaa.ADHDassess$upper.I2.w
           )

hiaa.ADHDassess$pval.Q.b.common

## ------------------------------------------------------------------------
hiaa.AssayMethod <- update(res.hiaa,
       subgroup = AssayMethod,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(AssayMethod)) # ignore missing data

hiaa.AssayMethod

sa.tbl.5hiaa.am <- data.frame(
           hiaa.AssayMethod$subgroup.levels,
           hiaa.AssayMethod$k.w,
           hiaa.AssayMethod$n.e.w, 
           hiaa.AssayMethod$n.c.w, 
           hiaa.AssayMethod$TE.common.w, 
           hiaa.AssayMethod$lower.common.w, 
           hiaa.AssayMethod$upper.common.w, 
           hiaa.AssayMethod$pval.common.w, 
           hiaa.AssayMethod$I2.w, 
           hiaa.AssayMethod$lower.I2.w, 
           hiaa.AssayMethod$upper.I2.w
           )

hiaa.AssayMethod$pval.Q.b.common

## ------------------------------------------------------------------------
hiaa.StudyQuality <- update(res.hiaa,
       subgroup = StudyQuality,
       tau.common = TRUE, # as number of studies in subgroups is small
       subset = !is.na(StudyQuality)) # ignore missing data

hiaa.StudyQuality

sa.tbl.5hiaa.sq <- data.frame(
           hiaa.StudyQuality$subgroup.levels,
           hiaa.StudyQuality$k.w,
           hiaa.StudyQuality$n.e.w, 
           hiaa.StudyQuality$n.c.w, 
           hiaa.StudyQuality$TE.common.w, 
           hiaa.StudyQuality$lower.common.w, 
           hiaa.StudyQuality$upper.common.w, 
           hiaa.StudyQuality$pval.common.w, 
           hiaa.StudyQuality$I2.w, 
           hiaa.StudyQuality$lower.I2.w, 
           hiaa.StudyQuality$upper.I2.w
           )

hiaa.StudyQuality$pval.Q.b.common

## ------------------------------------------------------------------------
sa.tbls.5hiaa <- list(sa.tbl.5hiaa.s, sa.tbl.5hiaa.up, sa.tbl.5hiaa.c, sa.tbl.5hiaa.sex, sa.tbl.5hiaa.med, sa.tbl.5hiaa.aa, sa.tbl.5hiaa.am, sa.tbl.5hiaa.sq)

write_xlsx(sa.tbls.5hiaa, '/Users/teresacorreiadesa/R/Meta-Analysis/outputs/subgroup-analysis/sa.tbls.5hiaa.xlsx')


## ------------------------------------------------------------------------
#' ## Subgroup analysis - plots
## ------------------------------------------------------------------------
sa.plt.da <- read_excel('/Users/teresacorreiadesa/R/Meta-Analysis/data/data_subgroup-analysis_plots.xlsx', sheet = 1)
sa.plt.ad <- read_excel('/Users/teresacorreiadesa/R/Meta-Analysis/data/data_subgroup-analysis_plots.xlsx', sheet = 2)
sa.plt.na <- read_excel('/Users/teresacorreiadesa/R/Meta-Analysis/data/data_subgroup-analysis_plots.xlsx', sheet = 3)
sa.plt.5ht <- read_excel('/Users/teresacorreiadesa/R/Meta-Analysis/data/data_subgroup-analysis_plots.xlsx', sheet = 4)
sa.plt.dopac <- read_excel('/Users/teresacorreiadesa/R/Meta-Analysis/data/data_subgroup-analysis_plots.xlsx', sheet = 5)
sa.plt.hva <- read_excel('/Users/teresacorreiadesa/R/Meta-Analysis/data/data_subgroup-analysis_plots.xlsx', sheet = 6)
sa.plt.5hiaa <- read_excel('/Users/teresacorreiadesa/R/Meta-Analysis/data/data_subgroup-analysis_plots.xlsx', sheet = 7)


#' ### dopamine
## ------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(ggnewscale)
library(gridExtra)

# convert I_squared to numeric
sa.plt.da$I_squared <- as.numeric(sa.plt.da$I_squared)

# convert Group to a factor with specified levels
desired_order <- c("All studies", "Urine", "Blood", "24-hr urine collection", "<24-hr urine collection", "Plasma", "Serum", "Western country", "Eastern country", "Both sexes", "Male", "ADHD group takes med", "ADHD group doesn't take med", "DSM-IV/DSM-IV-TR/DSM-5", "Other diagnostic method", "HPLC", "Other assay method", "Fair or Good quality", "Very poor or Poor quality")
sa.plt.da$Group <- factor(sa.plt.da$Group, levels = desired_order)

# create the plot with separate aesthetics
da.sa.plot <- ggplot(sa.plt.da, aes(x = Group)) +
  geom_bar(aes(y = abs(SMD), fill = factor(ifelse(SMD < 0, "Negative effect size (SMD)", "Positive effect size (SMD)"))), stat = "identity", color = "black") +
  new_scale_color() + # use a new colour scale for the points
  geom_line(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), group = 1, linetype = "dashed", size = 0.5) +
  geom_point(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), shape = 1, size = 2) +
  scale_y_continuous(
    name = "Effect Size (SMD)",
    sec.axis = sec_axis(~ . * 100 / max(abs(sa.plt.da$SMD)), name = "I² (%)")
  ) +
  labs(title = " ", x = NULL) +
  scale_fill_manual(values = c("Negative effect size (SMD)" = "#454545", "Positive effect size (SMD)" = "#C0C0C0"), name = NULL) +
  scale_color_manual(values = c("I² (%)" = "black"), name = NULL) + # set colour for line and points
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        legend.position = "top",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.margin = margin(0, 0, 0, 0, unit = "pt"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
           plot.title = element_text(hjust = 0.5)) + # centre the plot title
  coord_cartesian(ylim = c(0, max(abs(sa.plt.da$SMD)) * 1.2))

da.sa.plot

#' ### adrenaline 
## ------------------------------------------------------------------------

# convert I_squared to numeric
sa.plt.ad$I_squared <- as.numeric(sa.plt.ad$I_squared)

# convert Group to a factor with specified levels
desired_order <- c("All studies", "Urine", "Blood", "24-hr urine collection", "<24-hr urine collection", "Both sexes", "Male", "ADHD group takes med", "ADHD group doesn't take med", "DSM-IV/DSM-IV-TR/DSM-5", "Other diagnostic method", "HPLC", "Other assay method", "Fair or Good quality", "Very poor or Poor quality")
sa.plt.ad$Group <- factor(sa.plt.ad$Group, levels = desired_order)

# create the plot with separate aesthetics
ad.sa.plot <- ggplot(sa.plt.ad, aes(x = Group)) +
  geom_bar(aes(y = abs(SMD), fill = factor(ifelse(SMD < 0, "Negative effect size (SMD)", "Positive effect size (SMD)"))), stat = "identity", color = "black") +
  new_scale_color() + # use a new colour scale for the points
  geom_line(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), group = 1, linetype = "dashed", size = 0.5) +
  geom_point(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), shape = 1, size = 2) +
  scale_y_continuous(
    name = "Effect Size (SMD)",
    sec.axis = sec_axis(~ . * 100 / max(abs(sa.plt.ad$SMD)), name = "I² (%)")
  ) +
  labs(title = " ", x = NULL) +
  scale_fill_manual(values = c("Negative effect size (SMD)" = "#454545", "Positive effect size (SMD)" = "#C0C0C0"), name = NULL) +
  scale_color_manual(values = c("I² (%)" = "black"), name = NULL) + # set colour for line and points
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        legend.position = "top",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.margin = margin(0, 0, 0, 0, unit = "pt"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
           plot.title = element_text(hjust = 0.5)) + # centre the plot title
  coord_cartesian(ylim = c(0, max(abs(sa.plt.ad$SMD)) * 1.2))

ad.sa.plot

#' ### noradrenaline
## ------------------------------------------------------------------------

# convert I_squared to numeric
sa.plt.na$I_squared <- as.numeric(sa.plt.na$I_squared)

# convert Group to a factor with specified levels
desired_order <- c("All studies", "Urine", "Blood", "24-hr urine collection", "<24-hr urine collection", "Plasma", "Serum", "Western country", "Eastern country", "Both sexes", "Male", "ADHD group takes med", "ADHD group doesn't take med", "DSM-IV/DSM-IV-TR/DSM-5", "Other diagnostic method", "HPLC", "Other assay method", "Fair or Good quality", "Very poor or Poor quality")
sa.plt.na$Group <- factor(sa.plt.na$Group, levels = desired_order)

# create the plot with separate aesthetics
na.sa.plot <- ggplot(sa.plt.na, aes(x = Group)) +
  geom_bar(aes(y = abs(SMD), fill = factor(ifelse(SMD < 0, "Negative effect size (SMD)", "Positive effect size (SMD)"))), stat = "identity", color = "black") +
  new_scale_color() + # use a new colour scale for the points
  geom_line(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), group = 1, linetype = "dashed", size = 0.5) +
  geom_point(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), shape = 1, size = 2) +
  scale_y_continuous(
    name = "Effect Size (SMD)",
    sec.axis = sec_axis(~ . * 100 / max(abs(sa.plt.na$SMD)), name = "I² (%)")
  ) +
  labs(title = " ", x = NULL) +
  scale_fill_manual(values = c("Negative effect size (SMD)" = "#454545", "Positive effect size (SMD)" = "#C0C0C0"), name = NULL) +
  scale_color_manual(values = c("I² (%)" = "black"), name = NULL) + # set colour for line and points
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        legend.position = "top",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.margin = margin(0, 0, 0, 0, unit = "pt"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
           plot.title = element_text(hjust = 0.5)) + # centre the plot title
  coord_cartesian(ylim = c(0, max(abs(sa.plt.na$SMD)) * 1.2))

na.sa.plot

#' ### serotonin
## ------------------------------------------------------------------------

# convert I_squared to numeric
sa.plt.5ht$I_squared <- as.numeric(sa.plt.5ht$I_squared)

# convert Group to a factor with specified levels
desired_order <- c("All studies", "Urine", "Blood", "Whole blood", "Platelets", "Serum", "Umbilical cord blood", "Western country", "Eastern country", "Both sexes", "Male", "ADHD group takes med", "ADHD group doesn't take med", "DSM-IV/DSM-IV-TR/DSM-5", "Other diagnostic method",  "Spectrofluorimetry", "LC-MS", "RIA/ELISA kit", "Fair or Good quality", "Very poor or Poor quality")
sa.plt.5ht$Group <- factor(sa.plt.5ht$Group, levels = desired_order)

# create the plot with separate aesthetics
ser.sa.plot <- ggplot(sa.plt.5ht, aes(x = Group)) +
  geom_bar(aes(y = abs(SMD), fill = factor(ifelse(SMD < 0, "Negative effect size (SMD)", "Positive effect size (SMD)"))), stat = "identity", color = "black") +
  new_scale_color() + # use a new colour scale for the points
  geom_line(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), group = 1, linetype = "dashed", size = 0.5) +
  geom_point(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), shape = 1, size = 2) +
  scale_y_continuous(
    name = "Effect Size (SMD)",
    sec.axis = sec_axis(~ . * 100 / max(abs(sa.plt.5ht$SMD)), name = "I² (%)")
  ) +
  labs(title = " ", x = NULL) +
  scale_fill_manual(values = c("Negative effect size (SMD)" = "#454545", "Positive effect size (SMD)" = "#C0C0C0"), name = NULL) +
  scale_color_manual(values = c("I² (%)" = "black"), name = NULL) + # set colour for line and points
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        legend.position = "top",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.margin = margin(0, 0, 0, 0, unit = "pt"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
           plot.title = element_text(hjust = 0.5)) + # centre the plot title
  coord_cartesian(ylim = c(0, max(abs(sa.plt.5ht$SMD)) * 1.2))

ser.sa.plot

#' ### dopac
## ------------------------------------------------------------------------

# convert I_squared to numeric
sa.plt.dopac$I_squared <- as.numeric(sa.plt.dopac$I_squared)

# convert Group to a factor with specified levels
desired_order <- c("All studies", "ADHD group takes med", "ADHD group doesn't take med")
sa.plt.dopac$Group <- factor(sa.plt.dopac$Group, levels = desired_order)

# create the plot with separate aesthetics
dopac.sa.plot <- ggplot(sa.plt.dopac, aes(x = Group)) +
  geom_bar(aes(y = abs(SMD), fill = factor(ifelse(SMD < 0, "Negative effect size (SMD)", "Positive effect size (SMD)"))), stat = "identity", color = "black") +
  new_scale_color() + # use a new colour scale for the points
  geom_line(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), group = 1, linetype = "dashed", size = 0.5) +
  geom_point(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), shape = 1, size = 2) +
  scale_y_continuous(
    name = "Effect Size (SMD)",
    sec.axis = sec_axis(~ . * 100 / max(abs(sa.plt.dopac$SMD)), name = "I² (%)")
  ) +
  labs(title = " ", x = NULL) +
  scale_fill_manual(values = c("Negative effect size (SMD)" = "#454545", "Positive effect size (SMD)" = "#C0C0C0"), name = NULL) +
  scale_color_manual(values = c("I² (%)" = "black"), name = NULL) + # set colour for line and points
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        legend.position = "top",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.margin = margin(0, 0, 0, 0, unit = "pt"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
           plot.title = element_text(hjust = 0.5)) + # centre the plot title
  coord_cartesian(ylim = c(0, max(abs(sa.plt.dopac$SMD)) * 1.2))

dopac.sa.plot

#' ### hva
## ------------------------------------------------------------------------

# convert I_squared to numeric
sa.plt.hva$I_squared <- as.numeric(sa.plt.hva$I_squared)

# convert Group to a factor with specified levels
desired_order <- c("All studies", "24-hr urine collection", "<24-hr urine collection", "Western country", "Eastern country", "ADHD group takes med", "ADHD group doesn't take med", "DSM-IV/DSM-IV-TR/DSM-5", "Other diagnostic method", "HPLC", "Other assay method", "Fair or Good quality", "Very poor or Poor quality")
sa.plt.hva$Group <- factor(sa.plt.hva$Group, levels = desired_order)

# create the plot with separate aesthetics
hva.sa.plot <- ggplot(sa.plt.hva, aes(x = Group)) +
  geom_bar(aes(y = abs(SMD), fill = factor(ifelse(SMD < 0, "Negative effect size (SMD)", "Positive effect size (SMD)"))), stat = "identity", color = "black") +
  new_scale_color() + # use a new colour scale for the points
  geom_line(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), group = 1, linetype = "dashed", size = 0.5) +
  geom_point(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), shape = 1, size = 2) +
  scale_y_continuous(
    name = "Effect Size (SMD)",
    sec.axis = sec_axis(~ . * 100 / max(abs(sa.plt.hva$SMD)), name = "I² (%)")
  ) +
  labs(title = " ", x = NULL) +
  scale_fill_manual(values = c("Negative effect size (SMD)" = "#454545", "Positive effect size (SMD)" = "#C0C0C0"), name = NULL) +
  scale_color_manual(values = c("I² (%)" = "black"), name = NULL) + # set colour for line and points
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        legend.position = "top",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.margin = margin(0, 0, 0, 0, unit = "pt"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
           plot.title = element_text(hjust = 0.5)) + # centre the plot title
  coord_cartesian(ylim = c(0, max(abs(sa.plt.hva$SMD)) * 1.2))

hva.sa.plot

#' ### 5-hiaa
## ------------------------------------------------------------------------

# convert I_squared to numeric
sa.plt.5hiaa$I_squared <- as.numeric(sa.plt.5hiaa$I_squared)

# convert Group to a factor with specified levels
desired_order <- c("All studies", "Urine", "Blood", "24-hr urine collection", "<24-hr urine collection", "Western country", "Eastern country", "DSM-IV/DSM-IV-TR/DSM-5", "Other diagnostic method", "HPLC", "Other assay method", "Fair or Good quality", "Very poor or Poor quality")
sa.plt.5hiaa$Group <- factor(sa.plt.5hiaa$Group, levels = desired_order)

# create the plot with separate aesthetics
hiaa.sa.plot <- ggplot(sa.plt.5hiaa, aes(x = Group)) +
  geom_bar(aes(y = abs(SMD), fill = factor(ifelse(SMD < 0, "Negative effect size (SMD)", "Positive effect size (SMD)"))), stat = "identity", color = "black") +
  new_scale_color() + # use a new colour scale for the points
  geom_line(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), group = 1, linetype = "dashed", size = 0.5) +
  geom_point(aes(y = I_squared * max(abs(SMD)), color = "I² (%)"), shape = 1, size = 2) +
  scale_y_continuous(
    name = "Effect Size (SMD)",
    sec.axis = sec_axis(~ . * 100 / max(abs(sa.plt.5hiaa$SMD)), name = "I² (%)")
  ) +
  labs(title = " ", x = NULL) +
  scale_fill_manual(values = c("Negative effect size (SMD)" = "#454545", "Positive effect size (SMD)" = "#C0C0C0"), name = NULL) +
  scale_color_manual(values = c("I² (%)" = "black"), name = NULL) + # set colour for line and points
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        legend.position = "top",
        legend.box = "horizontal",
        legend.justification = "center",
        legend.margin = margin(0, 0, 0, 0, unit = "pt"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
           plot.title = element_text(hjust = 0.5)) + # centre the plot title
  coord_cartesian(ylim = c(0, max(abs(sa.plt.5hiaa$SMD)) * 1.2))

hiaa.sa.plot

## ------------------------------------------------------------------------

# arrange the plots using plot_grid

sa.plts <- list(da.sa.plot, ad.sa.plot, na.sa.plot, ser.sa.plot, hva.sa.plot, hiaa.sa.plot)

combined_plot_2 <- plot_grid(plotlist = sa.plts,
                           labels = c("A - Dopamine", "B - Adrenaline", "C - Noradrenaline", "D - Serotonin", "E - Homovanillic acid", "F - 5-hydroxy-indoleacetic acid"),
                           ncol =2,
                           nrow = 3,
                           label_size = 11,
                           hjust = 0)

# save the combined plot
save_plot("outputs/subgroup-analysis/combined_subgroup_analyses.pdf", combined_plot_2, base_width = 13, base_height = 13)

combined_plot_2

## ------------------------------------------------------------------------
sessionInfo()
## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 8,
  fig.height = 3.5,
  fig.align = "center"
)
has_models <- requireNamespace("glmmTMB", quietly = TRUE) &&
  requireNamespace("MASS", quietly = TRUE)
has_gam <- has_models && requireNamespace("mgcv", quietly = TRUE)
has_compare <- requireNamespace("emmeans", quietly = TRUE) &&
  requireNamespace("lmerTest", quietly = TRUE) &&
  requireNamespace("multcompView", quietly = TRUE)

## -----------------------------------------------------------------------------
library(r4pde)
head(SADRustSoybean)

## -----------------------------------------------------------------------------
x <- sad_data(SADRustSoybean, rater = "rater", image = "leaf", actual = "actual",
              wide = c(UN = "unaided", SAD = "aided"), group = "sad")
x

## ----eval = FALSE-------------------------------------------------------------
# sad_data(d, rater = "rater", image = "image", actual = "actual",
#          estimate = "estimate", method = "method", group = "group")

## -----------------------------------------------------------------------------
x_new <- sad_data(subset(SADRustSoybean, sad == "New"),
                  wide = c(UN = "unaided", SAD = "aided"))
levels(x_new$condition)

## -----------------------------------------------------------------------------
cc <- sad_ccc(x)
head(cc)

## ----eval = has_compare-------------------------------------------------------
cmp <- sad_compare(x, cc)
cmp

## -----------------------------------------------------------------------------
sad_reliability(x)

## ----fig.height = 3-----------------------------------------------------------
p <- sad_plots(x, cc, baseline = "UN")
p$scatter

## ----fig.height = 3.2---------------------------------------------------------
p$box_ccc

## ----fig.width = 6, fig.height = 3.5------------------------------------------
p$gain_vs_baseline

## ----eval = has_models--------------------------------------------------------
fit <- sad_fit(x)
fit

## ----eval = has_models--------------------------------------------------------
summary(fit)

## ----eval = has_models--------------------------------------------------------
pairs <- data.frame(a = c("SAD_Old", "SAD_New", "SAD_New"),
                    b = c("UN_Old",  "UN_New",  "SAD_Old"))
cv <- sad_curve(fit, pairs = pairs)

## ----eval = has_models--------------------------------------------------------
sad_regions(cv)

## ----eval = has_models--------------------------------------------------------
at <- c(10, 20, 50, 75)
tab <- do.call(rbind, lapply(split(cv$curves, cv$curves$condition), function(z)
  data.frame(condition = z$condition[1], severity = at,
             bias = round(approx(z$actual, z$bias, at)$y, 1),
             EAE = round(approx(z$actual, z$expected_abs_error, at)$y, 1))))
tab[!is.na(tab$bias), ]

## ----eval = has_models--------------------------------------------------------
plot(cv, "bias")
plot(cv, "expected_abs_error")
plot(cv, "difference")

## ----eval = has_gam-----------------------------------------------------------
cs <- sad_curve(fit, type = "smooth", pairs = pairs)
sad_regions(cs)
plot(cs, "difference")

## ----eval = has_models, fig.width = 6, fig.height = 4-------------------------
rt <- sad_raters(fit)
plot_sad_raters(rt)

## ----eval = has_models--------------------------------------------------------
head(sad_images(fit))

## ----eval = has_models--------------------------------------------------------
fit_lin <- tryCatch(
  sad_fit(x, structure = fit$structure, dispersion = "lin"),
  error = function(e) NULL
)
if (!is.null(fit_lin)) {
  AIC(fit$model, fit_lin$model)
} else {
  message("The log-linear dispersion model did not converge on this system.")
}

## ----eval = FALSE-------------------------------------------------------------
# fit_ob <- sad_fit(x, family = "ordbeta", dispersion = "lin")
# summary(fit_ob)

## ----eval = FALSE-------------------------------------------------------------
# sad_simulate(n_rater = c(10, 20, 40), n_image = c(20, 40), nsim = 200,
#              shift = -3, cores = 4)
# sad_simulate(n_rater = c(10, 20, 40), n_image = c(20, 40), nsim = 200,
#              shift = 0, slope_change = -0.10, cores = 4)


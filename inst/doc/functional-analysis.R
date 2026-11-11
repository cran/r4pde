## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5,
  fig.align = "center"
)

## ----setup, message = FALSE---------------------------------------------------
library(r4pde)
library(ggplot2)

## ----data---------------------------------------------------------------------
data("BudBlightSoybean", package = "r4pde")
head(BudBlightSoybean)

## ----curves, fig.alt = "Fitted mean disease progress curves over time for soybean bud blight treatments"----
fc <- functional_curves(
  data = BudBlightSoybean,
  time = "time",
  response = "y",
  treatment = "treat",
  block = "block",
  min_points = 3,
  family_try = "quasibinomial",
  show_progress = FALSE
)

# Plot fitted mean curves
plot(fc)

## ----rate, fig.alt = "Instantaneous rate of disease progress and velocity phenotypes across treatments"----
fr <- functional_rate(fc, scale = "response", n_grid = 100)

# Summary of rate phenotypes (r_max, t_r_max, growth duration)
summary(fr)

# Visualize instantaneous progress rates over time
plot(fr)

## ----distances, fig.alt = c("Adjusted functional curves colored by functional cluster", "Hierarchical clustering dendrogram based on L2 functional distances")----
fd <- functional_distances(fc, cluster_k = 2, show_progress = FALSE)

# Plot environment-adjusted curves colored by functional cluster
plot_curves(fd)

# Plot hierarchical clustering dendrogram
plot_dendrogram(fd)

## ----fpca, fig.alt = "Biplot of treatment trajectories in functional principal component score space"----
# Retain 2 components for score biplot
fpca <- functional_pca(fc, n_components = 2)

# Print variance explained by the functional principal components
print(fpca)

# Biplot of treatments in FPCA score space
plot(fpca, type = "scores")

## ----resistance---------------------------------------------------------------
# Using 'PD1' as the reference susceptible treatment
fres <- functional_resistance(
  fc,
  reference = "PD1",
  group_method = "quantile",
  n_groups = 2
)

# View resistance scores and rankings
fres$table


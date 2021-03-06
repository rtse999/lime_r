# ------------------------------------------------------------------------
# Description: "Announcing lime - Explaining the predictions of black-box models"
# Link: https://www.data-imaginist.com/2017/announcing-lime/
#
# Location: /Users/raymondtse/Dropbox/Analysis/R packages/lime/announcinglime.r
# First created: 14:10 - Friday 9 March 2018
# Last modified: 14:10 - Friday 9 March 2018
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time 
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------
library(lime)
library(MASS)

# ------------------------------------------------------------------------
# Session Info
# ------------------------------------------------------------------------
devtools::session_info()

# ------------------------------------------------------------------------
# Modelling
# ------------------------------------------------------------------------
data(biopsy)

# First we'll clean up the data a bit
biopsy$ID <- NULL
biopsy <- na.omit(biopsy)
names(biopsy) <- c('clump thickness', 'uniformity of cell size', 
                   'uniformity of cell shape', 'marginal adhesion',
                   'single epithelial cell size', 'bare nuclei', 
                   'bland chromatin', 'normal nucleoli', 'mitoses',
                   'class')

# Now we'll fit a linear discriminant model on all but 4 cases
set.seed(4)
test_set <- sample(seq_len(nrow(biopsy)), 100)
prediction <- biopsy$class
biopsy$class <- NULL
model <- lda(biopsy[-test_set, ], prediction[-test_set])

sum(predict(model, biopsy[test_set, ])$class == prediction[test_set])/100

# ------------------------------------------------------------------------
# Train explainer using lime 
# ------------------------------------------------------------------------
# Train the explainer
explainer <- lime(biopsy[-test_set,], model, bin_continuous = TRUE, quantile_bins = FALSE)

# Use the explainer on new observations
explanation <- explain(biopsy[test_set[1:4], ], explainer, n_labels = 1, n_features = 4)

tibble::glimpse(explanation)

explanation <- explain(biopsy[test_set[1:4], ], explainer, n_labels = 1, 
                       n_features = 4, kernel_width = 0.5)
explanation[, 2:9]

plot_features(explanation, ncol = 1)

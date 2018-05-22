# ------------------------------------------------------------------------
# Description: Lime Test Using the iris Data Set
# Link: 
#
# Location: /Users/raymondtse/Dropbox/Analysis/R packages/lime/lime_iris.r
# First created: 14:10 - Friday 9 March 2018
# Last modified: 16:52 - Tuesday 22 May 2018
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# System time 
# ------------------------------------------------------------------------
format(Sys.time(), "%a %b %d %H:%M:%S %Y")

# ------------------------------------------------------------------------
# Libraries
# ------------------------------------------------------------------------
library(dplyr)
library(lime)
library(MASS)
library(modelr)

# ------------------------------------------------------------------------
# Session Info
# ------------------------------------------------------------------------
devtools::session_info()

# ------------------------------------------------------------------------
# Modelling
# ------------------------------------------------------------------------
data(iris)
iris_data <- iris

iris %>% 
  group_by(Species) %>% 
  summarise(
    n = n(),
    avg_sepal_length = mean(Sepal.Length),
    avg_sepal_width = mean(Sepal.Width),
    avg_petal_length = mean(Petal.Length),
    avg_petal_width = mean(Petal.Width)
  )

set.seed(123)
n_test_set = 0.5 * nrow(iris_data)
test_set <- sample(seq_len(nrow(iris_data)), n_test_set)
prediction <- iris_data$Species
iris_data$Species <- NULL
model <- lda(iris_data[-test_set, ], prediction[-test_set])

sum(predict(model, iris_data[test_set, ])$class == prediction[test_set])/n_test_set

# ------------------------------------------------------------------------
# Train explainer using lime 
# ------------------------------------------------------------------------
# Train the explainer
explainer <- lime(iris_data[-test_set,], model, bin_continuous = TRUE, quantile_bins = FALSE)

# Use the explainer on new observations
explanation <- explain(iris_data[test_set[1:4], ], explainer, n_labels = 1, n_features = 4)

tibble::glimpse(explanation)

explanation <- explain(iris_data[test_set[1:4], ], explainer, n_labels = 1, 
                       n_features = 4, kernel_width = 0.5)
explanation[, 2:9]

plot_features(explanation, ncol = 1)


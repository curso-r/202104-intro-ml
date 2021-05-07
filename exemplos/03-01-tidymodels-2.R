# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(skimr)
library(tidymodels)

# Dados -------------------------------------------------------------------

data("diamonds")


# base treino e base teste ------------------------------------------------

set.seed(1)
diamonds_initial_split <- diamonds %>% initial_split(3/4)

diamonds_train <- training(diamonds_initial_split)
diamonds_test <- testing(diamonds_initial_split)

# definicao do modelo -----------------------------------------------------

diamonds_model <- decision_tree(
  cost_complexity = tune(),
  min_n = 2, 
  tree_depth = 10
) %>% 
  set_engine("rpart") %>%
  set_mode("regression")


# quebra da base de treino em teste e validacao ---------------------------

diamonds_resamples <- validation_split(diamonds_train, prop = .75)

# tunagem de hiperparametros ----------------------------------------------
diamonds_tune_grid <- tune_grid(
  diamonds_model,
  price ~ x,
  resamples = diamonds_resamples,
  grid = 10,
  metrics = metric_set(rmse, rsq),
  control = control_grid(verbose = TRUE, allow_par = FALSE)
)

autoplot(diamonds_tune_grid)



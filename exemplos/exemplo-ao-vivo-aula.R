
# Pacotes -----------------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(ggplot2)

# Dados -------------------------------------------------------------------

data("diamonds")

set.seed(1)

base_inicial <- diamonds %>% 
  sample_n(10000)

# Separar treino e teste --------------------------------------------------

# aqui eu uso as funcoes initial_split + training e test

quebra_em_treino_e_teste <- initial_split(base_inicial, prop = .75)

base_treino <- training(quebra_em_treino_e_teste)
base_teste <- testing(quebra_em_treino_e_teste)

# Definir o esqueleto do modelo -------------------------------------------

especificacao_arvore <- decision_tree(
  cost_complexity = 0.001,
  min_n = 5,
  tree_depth = tune()
) %>% 
  set_mode("regression") %>% 
  set_engine("rpart")

# Quebrar a base em treino e validacao ------------------------------------

quebra_treino_validacao <- validation_split(base_treino, prop = .75)

# Ajustar vários valores do hiperparametro na base treino e calcul --------

resultado_varios_testes <- tune_grid(
  especificacao_arvore,
  price ~ x, 
  quebra_treino_validacao,
  grid = 20,
  metrics = metric_set(rmse, rsq, mae)
)

collect_metrics(resultado_varios_testes) %>% 
  arrange(tree_depth) %>% 
  count(tree_depth)

autoplot(resultado_varios_testes)

show_best(resultado_varios_testes, metric = "rmse", n = 10)

# especificacao do modelo final -------------------------------------------

especificacao_modelo_final <- finalize_model(especificacao_arvore,
                               select_best(resultado_varios_testes, "rmse"))

# ajuste final ------------------------------------------------------------

modelo_final <- fit(especificacao_modelo_final,
    y ~ x,
    data = base_treino)

library(rpart.plot)

prp(modelo_final$fit)

# calculo final do erro ---------------------------------------------------

base_teste_com_pred <- base_teste %>% 
  mutate(
    price_pred = predict(modelo_final, base_teste)$.pred
  )

base_teste_com_pred %>% 
  rmse(truth = price, estimate = price_pred)

base_teste_com_pred %>% 
  rsq(truth = price, estimate = price_pred)

base_teste_com_pred %>% 
  mae(truth = price, estimate = price_pred)


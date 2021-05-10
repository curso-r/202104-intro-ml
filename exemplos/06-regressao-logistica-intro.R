library(tidymodels)
library(tidyverse)

dados <- tribble(
  ~Pressão,	~Glicose,	~Diabetes,
  "hipertensao" ,92  ,    "nao",
  'normal'	    ,130 ,    "sim",
  "normal"	    ,130 ,    "nao",
  "normal"      ,55  ,    "nao",
  "hipertensao"	,220 ,    "sim",
  "normal"	    ,195 ,    "sim"
) %>%
  mutate(
    Diabetes = as.factor(Diabetes)
  )

# regressão logística LASSO
modelo <- logistic_reg(penalty = 0.1, mixture = 1) %>% 
  set_engine("glmnet")

ajuste <- fit(modelo, Diabetes ~ ., data = dados)

broom::tidy(ajuste)

dados_com_pred <- dados %>%
  mutate(
    preditor_linear = -3.80 + 0 * (`Pressão` == "normal") + 0.0280 * Glicose,
    probabilidade_predita = 1/(1 + exp(-preditor_linear)),
    classe_predita = ifelse(probabilidade_predita > 0.90, "sim", "nao")
  )

logistica <- function(x) 1/(1 + exp(-x))
ggplot(dados_com_pred) + 
  geom_point(aes(x = preditor_linear, y = probabilidade_predita)) + 
  geom_function(fun = logistica)

dados_com_pred %>% conf_mat(Diabetes, classe_predita)

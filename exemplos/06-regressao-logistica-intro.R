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

# dataprep
receita <- recipe(Diabetes ~ ., data = dados) %>% 
  step_normalize(all_numeric_predictors()) %>% # (x - mean(x))/sd(x)
  step_dummy(all_nominal_predictors())

# bake(prep(receita), new_data = NULL)

# fit
wf <- workflow() %>% add_model(modelo) %>% add_recipe(receita)
ajuste <- fit(wf, data = dados)




broom::tidy(ajuste)

dados_com_pred <- dados %>%
  mutate(
    # B0 + B1x + B2x2
    preditor_linear = 0.0404 + 1.73*(Glicose - mean(Glicose))/(sd(Glicose)) + 0 * (`Pressão` == "normal"),
    # 1/(1 + exp(-(B0 + B1x + B2x2)))
    probabilidade_predita = 1/(1 + exp(-preditor_linear)),
    classe_predita = ifelse(probabilidade_predita > 0.5, "sim", "nao"),
    D = ifelse(Diabetes == "sim", -log(probabilidade_predita), -log(1 - probabilidade_predita) )
  )

logistica <- function(x) 1/(1 + exp(-x))

logistica(0)

ggplot(dados_com_pred) + 
  geom_point(aes(x = preditor_linear, y = probabilidade_predita)) + 
  geom_function(fun = logistica)





ggplot(dados_com_pred) + geom_density(aes(x = probabilidade_predita, fill = Diabetes), alpha = 0.4) +
  geom_vline(xintercept = 0.5)








dados_com_pred %>% conf_mat(Diabetes, classe_predita)









vip(ajuste$fit$fit)
rec <- extract_recipe(ajuste)


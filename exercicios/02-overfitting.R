# Pacotes ------------------------------------------------------------------

library(ggplot2)
library(patchwork)
library(skimr)
library(tidymodels)

# Dados -------------------------------------------------------------------
data("diamonds")

set.seed(10)
diamondsinho <- diamonds %>%
  filter(x > 0) %>% 
  group_by(x) %>%
  sample_n(1) %>%
  ungroup()

# Exercício: O codigo abaixo ajusta um modelo de árvore com os hiperparâmetros definidos 
# no bloco "definicao do modelo". Dado o que estiver preenchido nessas linhas, o codigo
# gera uma serie de graficos e uma tabela de metrica do modelo nas bases de treino e
# tambem em uma base de dados novos. Experimente alguns conjuntos de hiperparâmetros
# (cost_complexity, min_n, tree_depth)  editando o códigos nas minhas 28, 29 e 30
# e defina qual é melhor em termos de RMSE ou RSQ. 

# (EDITE O CÓDIGO ABAIXO) #################################################
# definicao do modelo -----------------------------------------------------
especificacao_modelo1 <- decision_tree(
  cost_complexity = 0, # varia de 0 a 0.1
  min_n = 40, # varia de 2 a N
  tree_depth = 15, # varia de 1 a 30
  mode = "regression"
) %>% set_engine("rpart")

# (A EDIÇÃO DE CÓDIGOS TERMINA AQUI!!) #######################################

# ajuste do modelo --------------------------------------------------------
ajuste_modelo1 <- especificacao_modelo1 %>% fit(price ~ x, data = diamondsinho)

# predicoes ---------------------------------------------------------------
diamondsinho_com_previsao <- diamondsinho %>% 
  mutate(
    price_pred1 = predict(ajuste_modelo1, new_data = diamondsinho)$.pred
  )

# qualidade dos ajustes e graficos ----------------------------------------
# Métricas de erro
diamondsinho_com_previsao_longo <- diamondsinho_com_previsao %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"), 
    names_to = "modelo", 
    values_to = "price_pred"
  ) 

# Pontos observados + curva da f
diamondsinho_com_previsao_g1 <- diamondsinho_com_previsao %>%
  ggplot() +
  geom_point(aes(x, price), size = 3) +
  #geom_step(aes(x, price_pred2, color = 'modelo2'), size = 1) +
  geom_step(aes(x, price_pred1, color = 'modelo1'), size = 1) +
  theme_bw() +
  ggtitle("f estimada")

# Observado vs Esperado
diamondsinho_com_previsao_g2 <- diamondsinho_com_previsao %>%
  filter(x > 0) %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"), 
    names_to = "modelo", 
    values_to = "price_pred"
  ) %>%
  ggplot() +
  geom_point(aes(price_pred, price, colour = modelo), size = 3) +
  geom_abline(slope = 1, intercept = 0, colour = "purple", size = 1) +
  theme_bw()+
  ggtitle("Ajustado vs Real")

# resíduos vs Esperado
diamondsinho_com_previsao_g3 <- diamondsinho_com_previsao %>%
  filter(x > 0) %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"), 
    names_to = "modelo", 
    values_to = "price_pred"
  ) %>%
  ggplot() +
  geom_point(aes(price_pred, price - price_pred, colour = modelo), size = 3) +
  geom_abline(slope = 0, intercept = 0, colour = "purple", size = 1) +
  ylim(c(-10000,10000)) +
  labs(y = "resíduo (y - y_chapeu)") +
  theme_bw() +
  ggtitle("Ajustado vs Resíduo")

# Resumo do ajuste do modelo
library(patchwork)
pw1 <- (diamondsinho_com_previsao_g1 + diamondsinho_com_previsao_g2) / (diamondsinho_com_previsao_g3)

############################################################################
############################################################################
############################################################################
# Agora vamos fingir que estamos em produção!

set.seed(3)
# "dados novos chegaram..."
diamondsinho_novos <- diamonds %>%
  filter(x > 0) %>% 
  sample_n(100)

# predicoes ---------------------------------------------------------------
diamondsinho_novos_com_previsao <- diamondsinho_novos %>% 
  mutate(
    price_pred1 = predict(ajuste_modelo1, new_data = diamondsinho_novos)$.pred
  )

# qualidade dos ajustes e graficos ----------------------------------------
# Métricas de erro
diamondsinho_novos_com_previsao_longo <- diamondsinho_novos_com_previsao %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"), 
    names_to = "modelo", 
    values_to = "price_pred"
  ) 

# Pontos observados + curva da f
diamondsinho_novos_com_previsao_g1 <- diamondsinho_novos_com_previsao %>%
  ggplot() +
  geom_point(aes(x, price), size = 3) +
  geom_step(aes(x, price_pred1, color = 'modelo1'), size = 1) +
  theme_bw() +
  ggtitle("f estimada (dados novos)")

# Observado vs Esperado
diamondsinho_novos_com_previsao_g2 <- diamondsinho_novos_com_previsao %>%
  filter(x > 0) %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"), 
    names_to = "modelo", 
    values_to = "price_pred"
  ) %>%
  ggplot() +
  geom_point(aes(price_pred, price, colour = modelo), size = 3) +
  geom_abline(slope = 1, intercept = 0, colour = "purple", size = 1) +
  theme_bw() +
  ggtitle("Ajustado vs Real (dados novos)")

# resíduos vs Esperado
diamondsinho_novos_com_previsao_g3 <- diamondsinho_novos_com_previsao %>%
  filter(x > 0) %>%
  tidyr::pivot_longer(
    cols = starts_with("price_pred"), 
    names_to = "modelo", 
    values_to = "price_pred"
  ) %>%
  ggplot() +
  geom_point(aes(price_pred, price - price_pred, colour = modelo), size = 3) +
  geom_abline(slope = 0, intercept = 0, colour = "purple", size = 1) +
  ylim(c(-10000,10000)) +
  labs(y = "resíduo (y - y_chapeu)") +
  theme_bw() +
  ggtitle("Ajustado vs Resíduo (dados novos)")

# Performance do modelo (base de treino e base de teste) ------------------

# performance numerica
mutate(diamondsinho_com_previsao_longo, tipo = "base de treino") %>% 
bind_rows(mutate(diamondsinho_novos_com_previsao_longo, tipo = "dados_novos")) %>% 
  group_by(tipo) %>% 
  summarise(
    rmse = rmse_vec(truth = price, estimate = price_pred),
    rsq = rsq_vec(truth = price, estimate = price_pred)
  )

pw1 <- (diamondsinho_com_previsao_g1 + diamondsinho_com_previsao_g2 + diamondsinho_com_previsao_g3) +
  plot_annotation(
  title = 'Base de treino'
)

pw2 <- (diamondsinho_novos_com_previsao_g1 + diamondsinho_novos_com_previsao_g2 + diamondsinho_novos_com_previsao_g3) +
  plot_annotation(
    title = 'Base de teste'
  )

# grafico de comparação
pw1 / pw2


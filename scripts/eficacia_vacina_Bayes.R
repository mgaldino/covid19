library(ggplot2)
library(tidyverse)

# o modelo para a eficácia usado é o seguinte
# existem probabilidades p e q de pessoas ficarem doentes graves 
# se tomarem vacina ou placebo (respectivamente)
# o número de pessoas doentes graves segue uma binomial
# A eficácia é dada por: (taxa de ataque no placebo - taxa de ataque no vacinado) / taxa de ataque no placebo
# E a taxa de ataque é dada por número de doentes graves / total da população (ou amostra)
# No estudo do Butantã, eficácia foi utilizada com um modelo de Cox de análise de sobreviência
# Portanto, este cálculo tem propósitos puramente pedagógicos

# os dados do estudo foram

n1 <- 4653 # amostra da vacina
n2 <- 4559 # amostra do placebo
s1 <- 0 # doentes graves no grupo da vacina
s2 <- 7 # doentes graves no grupo do placebo
pop <- 200000000 # população do Brasil

# Como escolher a priori?
# Vamos supor que a eficácia para casos graves, a priori, é de uns 50% aproximadamente.
# que priori representa isso?
# Em vez de ter uma priori sobre a eficácia, vou ter priori sobre p e q, e aí chegar na priori da eficácia

# priori para p
alpha1 <- 3
beta1 <- 250
prior1 <- rbeta(n_sim,alpha1, beta1) 
#truncando em 1,5%
prior1 <- ifelse(prior1 > .015, .015, prior1)
summary(prior1)

# distribuição preditiva a priori para o número de casos no grupo da vacina
prior_predictive_cases1 <- rbinom(n_sim, pop, prior1)

# taxa de ataque a priori na população se todos fossem vacinados
prior_atack_rate1 <- prior_predictive_cases1/pop
summary(prior_atack_rate1)

# priori para q
alpha2 <- 25
beta2 <- 1000
prior2 <- rbeta(n_sim,alpha2, beta2)
summary(prior2)
# distribuição preditiva a priori para o número de casos no grupo do placebo
prior_predictive_cases2 <- rbinom(n_sim, pop, prior2)

# taxa de ataque a priori na população se todos recebessem placebo
prior_atack_rate2 <- prior_predictive_cases2/pop
summary(prior_atack_rate2)

# priori da eficácia
prior_eficacy <- (prior_atack_rate2 - prior_atack_rate1)/prior_atack_rate2

summary(prior_eficacy) # eficácia 82% a priori (estimativa pontual a partir da mediana)

# Intervalo de credibilidade pra eficácia de 90%
quantile(prior_eficacy, c(.05, .95)) # 0.4197581 0.9529146 

# Probabilidade a priori da eficácia ser maior que 50%
sum(prior_eficacy > .5)/length(prior_eficacy) # 86%

#Parece uma priori razoável para a eficácia
# Relembrando ela deriva de ter prioris para p e q
# presentadas por alphas e betas da Distribuição Beta


# numero de simulações
n_sim <- 10000

# calculando posteriori, vamos seguir mesma lógia da prior
# como a Beta é conjugada da binomial, sabemos a fórmula da posteriori

# precisamos apenas dos tamanhos amostrais e número de "sucessos"(aqui, doença grave) em cada grupo amostral
#amostra vacina
n1 <- 4653

# amostra placebo
n2 <- 4559

# casos graves na vacina
s1 <- 0

#casos graves no placebo
s2 <- 7

# posteriori para p
posteriori_p <- rbeta(n_sim, alpha1 + s1 - 1, beta1 + n1 - s1 -1)

# posteriori para q
posteriori_q <- rbeta(n_sim, alpha1 + s2 - 1, beta1 + n2 - s2 -1)

# distribuição preditiva de casos graves a posteriori para vacina
predic_distrib_1 <- rbinom(n_sim, pop, posteriori_p)

# taxa de ataque na vacina se todos fossem vacinados
ratio1 <- predic_distrib_1/pop

# distribuição preditiva de casos graves a posteriori para vacina
predic_distrib_2 <- rbinom(n_sim, pop, posteriori_q)

# taxa de ataque no placebo se ninguém fossem vacinados (placebo)
ratio2 <- predic_distrib_2/pop

# eficácia a posteriori
eficacia_posteriori <- (ratio2 - ratio1)/ratio2
summary(eficacia_posteriori)
quantile(eficacia_posteriori, c(.05, .95))
quantile(prior_eficacy, c(.05, .95))
sum(eficacia_posteriori > .5)/length(eficacia_posteriori)
sum(eficacia_posteriori > .8)/length(eficacia_posteriori)
sum(prior_eficacy > .8)/length(prior_eficacy)

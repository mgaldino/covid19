library(tidyverse)
library(arm)
library(ggplot2)
num_days_100_lento <- 400
num_days_100_rapido <- 100
leitos_uti <- 31000
for(i in 1:2) {
  if(num_days_100_rapido == 100) {
    n_rowns <- num_days_100_lento } else {
      n_rowns <- num_days_100_rapido
    }
    dias_internacao_uti <- 7
    infectados <- c(1,seq(0, 100, by=20)[-1])*1e+6
    assintomaticos <- (1:3)/4
    sintomaticos <- 1 - assintomaticos
    prop_internacao <- c(1, 3, 5, 8, 10)/100
    evolucao <- invlogit(sort(runif(n_rowns, -5, 5)))
  }
}

df <- data.frame(evolucao = evolucao, dia=1:num_days_100_lento)

lista_df_i <- list()
j = 1
k=1

# 7 dias internacao
for ( i in 1:length(infectados)) {
  df$evol_infectados <- round(df$evolucao*infectados[i], 0)
  df$assintomaticos <- round(assintomaticos[j]*df$evol_infectados, 0)
  df$sintomaticos <- df$evol_infectados - df$assintomaticos
  df$internacao_pacientes <- round(prop_internacao[k]*df$sintomaticos)
  df <- df %>%
    mutate(lag_int_1 = dplyr::lag(internacao_pacientes, n=1, default = 0),
           lag_int_2 = dplyr::lag(internacao_pacientes, n=2, default = 0),
           lag_int_3 = dplyr::lag(internacao_pacientes, n=3, default = 0),
           lag_int_4 = dplyr::lag(internacao_pacientes, n=4, default = 0),
           lag_int_5 = dplyr::lag(internacao_pacientes, n=5, default = 0),
           lag_int_6 = dplyr::lag(internacao_pacientes, n=6, default = 0),
           lag_int_7 = dplyr::lag(internacao_pacientes, n=7, default = 0),
           internados = internacao_pacientes + lag_int_1 + lag_int_2 + lag_int_3 +
             lag_int_4 + lag_int_5 + lag_int_6 + lag_int_7)
  df$total_infectados <- infectados[i]
  lista_df_i[[i]] <- df
}
df_i <- bind_rows(lista_df_i)
head(df_i)
ggplot(df_i, aes(x=dia, y=internados)) + geom_line() +
  geom_hline(yintercept = 31000, colour = "red") + 
  facet_wrap( ~total_infectados, scales = "free_y")
  
df_i %>%
  filter(total_infectados==1000000) %>%
  ggplot( aes(x=dia, y=internados)) + geom_line() 

df <- df %>%
  mutate(id = 1:n())
## sim
evol_infectados <- 
  
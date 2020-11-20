library(tidyverse)

df <- readr::read_csv("herois_completo.zip")

df <- df %>% 
  filter(height > 0 & weight > 0)

df <- df %>% 
  mutate(height = height/100,
         weight = weight * 0.45)

#Crie um histograma da variável altura.
library(scales)

df %>% 
  filter(height < 3 & height > 1) %>% 
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 0.05) +
  scale_x_continuous(breaks = breaks_width(0.2))

0:10

# Analise a distribuição da variável peso em função da editora dos heróis.
df %>% 
  ggplot(aes(x = publisher,
             y = weight)) +
  geom_boxplot()

df %>% 
  group_by(publisher) %>% 
  summarise(mediana = median(weight),
            q1 = quantile(weight, 0.25),
            q3 = quantile(weight, 0.75))


head(economics)
?economics

economics %>% 
  select(date, uempmed, unemploy) %>% 
  pivot_longer(cols = c(uempmed, unemploy),
               names_to = "indicador",
               values_to = "valor") %>%
  ggplot(aes(x = date,
             y = valor,
             color = indicador)) +
  geom_line() +
  facet_wrap(vars(indicador), 
             scales = "free_y",
             #nrow =
             ncol = 1
               )

# Crie um gŕafico de barras mostrando a quantidade de heróis por editora.
# Ordene as barras em ordem descrescente. 
# Acrescente uma camada de texto nas barras mostrando a quantidade exata.
df %>% 
  count(publisher, name = "qtd") %>% 
  ggplot(aes(x = fct_reorder(publisher, qtd, .desc = TRUE),
             y = qtd)) +
  geom_col() +
  geom_text(aes(label = qtd),
            vjust = 1.2) +
  labs(x = NULL)


# MUDANDO AS BARRAS PARA HORIZONTAL
df %>% 
  count(publisher, name = "qtd") %>% 
  ggplot(aes(y = fct_reorder(publisher, qtd, .desc = TRUE),
             x = qtd)) +
  geom_col() +
  geom_text(aes(label = qtd),
            hjust = 1.2)



# Crie um gráfico de barras mostrando a quantidade de herois bons,
# maus ou neutros (variável alignment) por editora
df %>% 
  count(publisher, alignment, name = "qtd") %>% 
  ggplot(aes(x = publisher,
             y = qtd,
             fill = alignment)) +
  geom_col(position = "dodge")

df %>% 
  count(publisher, alignment, name = "qtd") %>% 
  ggplot(aes(x = publisher,
             y = qtd,
             fill = alignment)) +
  geom_col(position = "fill")


df %>% 
  count(publisher, alignment, name = "qtd") %>% 
  group_by(publisher) %>% 
  mutate(qtd_tt = sum(qtd),
         proporcao = qtd/qtd_tt) %>% 
  ggplot(aes(x = publisher, 
             y = proporcao,
             fill = alignment)) +
  geom_col(position = "dodge")


# Use o dplyr e o tidyr para criar um dataset chamado hero_agg,
# que contem a quantidade de poderes agregado por editora e heroi.
# Dica: transforme as colunas de super-poderes em numéricas, converta o dataframe 
# para formato tidy, agrupe os dados por editora e heroi e calcule a soma da
# coluna transformada de poder

glimpse(df)

hero_agg <- df %>% 
  select(publisher, name, agility:omniscient) %>% 
  pivot_longer(cols = agility:omniscient,
               names_to = "nome_poder",
               values_to = "tem_poder") %>% 
  group_by(publisher, name) %>% 
  summarise(qtd_poderes = sum(tem_poder))

hero_agg

# Faça um gráfico de barras mostrando os 10 herois de cada editora 
# que possuem mais poderes
hero_agg %>% 
  group_by(publisher) %>% 
  slice_max(n = 10, order_by = qtd_poderes,
            with_ties = FALSE) %>% 
  ggplot(aes(y = fct_reorder(name, qtd_poderes),
             x = qtd_poderes,
             fill = publisher))+
  geom_col()

hero_agg <- hero_agg %>% 
  mutate(name = case_when(
    name == "Captain Marvel" & publisher == "Marvel" ~ "Captain Marvel (Marvel)",
    name == "Captain Marvel" & publisher == "DC" ~ "Captain Marvel (DC)",
    TRUE ~ name
  ))

hero_agg %>% 
  group_by(publisher) %>% 
  slice_max(n = 10, order_by = qtd_poderes,
            with_ties = FALSE) %>% 
  ggplot(aes(y = fct_reorder(name, qtd_poderes),
             x = qtd_poderes,
             fill = publisher))+
  geom_col()


hero_agg %>% 
  group_by(publisher) %>% 
  slice_max(n = 10, order_by = qtd_poderes,
            with_ties = FALSE) %>% 
  ggplot(aes(y = fct_reorder(name, qtd_poderes),
             x = qtd_poderes)) +
  geom_col() +
  facet_wrap(vars(publisher), scales = "free_y", ncol = 1)






















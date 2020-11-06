library(tidyverse)
library(janitor) # install.packages("janitor")


hero_powers <- readr::read_csv("kaggle/super_hero_powers.csv")
hero_info <- readr::read_csv("kaggle/heroes_information.csv")
hero_powers
?read_csv
hero_info


hero_info <- readr::read_csv("kaggle/heroes_information.csv",
                             na = c("", "NA", "-", "-99"))
"-99" == -99

hero_info %>% 
  select(name, `Eye color`)

# camelCase
# EyeColor ou eyeColor
# eye_color

# clean_names()
?clean_names

hero_info <- clean_names(hero_info)
hero_powers <- clean_names(hero_powers)
glimpse(hero_powers)
glimpse(hero_info)

# 3
hero_info <- hero_info %>% 
  select(-x1)

glimpse(hero_info)

# 4 
glimpse(hero_powers)

hero_powers <- hero_powers %>% 
  mutate(agility = as.logical(agility),
         accelerated_healing = as.logical(accelerated_healing))

true_string <- "TRUE"
class(true_string)
is.logical(true_string)
as.logical(true_string)


hero_powers <- hero_powers %>% 
  mutate_at(vars(-1), as.logical)
glimpse(hero_powers)

hero_powers %>% mutate_at(vars(-1), as.numeric)


# 5
hero_info %>% distinct(publisher)

unique(hero_info$publisher)


hero_info %>% 
  filter(is.na(publisher)) %>% 
  mutate(publisher = ifelse(is.na(publisher),
                            "Outros",
                            ifelse(publisher == "Marvel Comics",
                                   "Marvel",
                                   ifelse(publisher == "DC Comics",
                                          "DC",
                                          "Outros"))))



hero_info %>% filter(is.na(publisher))
hero_info

hero_info <- hero_info %>% 
  mutate(publisher = case_when(
    # sintaxe do case_when:
    # TESTE LOGICO1 ~ RESULTADO QUANDO TRUE
    # TESTE LOGICO2 ~ RESULTADO QUANDO TRUE
    # ...
    # TRUE ~ RESULTADO QUANDO NENHUM DOS TESTES ANTERIORES É TRUE
    is.na(publisher) ~ "Outros",
    publisher == "Marvel Comics" ~ "Marvel",
    publisher == "DC Comics" ~ "DC",
    TRUE ~ "Outros"
  ))

hero_info

?case_when

# 6

hero_info %>% 
  select(publisher, race) %>% 
  filter(!is.na(race)) %>% 
  #distinct(publisher, race)
  distinct() %>% 
  count(race)

# primeira tabela: todas as combinações entre raça e editora
df_racas_e_editoras <- hero_info %>% 
  filter(!is.na(race)) %>% 
  distinct(publisher, race)

df_racas_e_editoras

df_qtd_editoras_por_raca <- df_racas_e_editoras %>% 
  count(race)

df_qtd_editoras_por_raca


left_join(df_racas_e_editoras, df_qtd_editoras_por_raca, by = "race") %>% 
  filter(n == 1)


# LHS ~ RHS
# y ~ x
# 7
hero_info %>% 
  select(gender, eye_color) %>% 
  na.omit() %>% 
  count(gender, eye_color) %>% 
  arrange(gender, desc(n)) %>% 
  group_by(gender) %>% 
  slice_head(n = 3)

x <- hero_info %>% 
  select(gender, eye_color) 

x[complete.cases(x), ]


hero_info %>% 
  select(gender, eye_color) %>% 
  na.omit() %>% 
  count(gender, eye_color, name = "qtd_personagens") %>% 
  group_by(gender) %>% 
  top_n(n = 3, wt = qtd_personagens)



hero_info %>% 
  select(gender, eye_color) %>% 
  na.omit() %>% 
  count(gender, eye_color, name = "qtd_personagens") %>% 
  group_by(gender) %>% 
  slice_max(order_by = qtd_personagens, n = 3)

# 8
hero_powers %>% 
  summarise(mean(agility))

mean(hero_powers$agility)

hero_powers %>% 
  summarise_if(is.logical, mean)

hero_powers_long <- hero_powers %>% 
  pivot_longer(cols = -hero_names,
               names_to = "nome_poder",
               values_to = "possui_poder")



hero_powers_long %>% 
  group_by(nome_poder) %>% 
  summarise(pct_tem_poder = mean(possui_poder)) %>% 
  arrange(pct_tem_poder)

# 10
hero <- inner_join(hero_info, hero_powers, by = c("name" = "hero_names"))
glimpse(hero)

# 11
hero %>% 
  select(publisher, telepathy) %>% 
  group_by(publisher) %>% 
  summarise(pct_telepatas = mean(telepathy))

# 12
hero %>% 
  select(name, publisher, flight, weight) %>% 
  filter(flight) %>% 
  #top_n(n = 10, wt = weight)
  slice_max(order_by = weight, n = 10)


# 13
readr::write_csv(hero, "herois_completo.csv")











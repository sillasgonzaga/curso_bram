library(tidyverse)

table1


?relig_income

# religion | renda | qtd_pessoas

relig_income
?pivot_longer

# pivot_longer(relig_income)
# ou
relig_income %>% 
  pivot_longer(cols = -religion,
               names_to = "renda",
               values_to = "qtd_pessoas")

relig_income %>% 
  pivot_longer(cols = `<$10k`:`Don't know/refused`)


relig_income %>% 
  pivot_longer(cols = 2:11)

relig_income %>% 
  pivot_longer(cols = -1)


## pivot_wider
us_rent_income %>% 
  select(GEOID, NAME, variable, estimate) %>% 
  pivot_wider(names_from = variable,
              values_from = estimate)

library(readxl)
exemplo <- read_excel("exemplo_tidyr.xlsx")
exemplo

exemplo %>% 
  pivot_longer(cols = -Mês,
               names_to = "ano",
               values_to = "vendas") %>% 
  group_by(Mês) %>% 
  summarise(qtd_media = mean(vendas))


# separate
table3 %>% 
  separate(col = rate,
           into = c("casos_tuberculose", "populacao"),
           sep = "/",
           convert = TRUE) %>% 
  mutate(taxa = casos_tuberculose / populacao)

?separate

df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))

exemplo_separate <- tibble(
  var1 = c("a1,b1", "a1,b1,c1", "a1,b2", "a1", "a1,a2,b1,c1")
)

exemplo_separate

exemplo_separate %>% 
  separate(col = var1,
           into = c("a", "b", "c"),
           sep = ",",
           extra = "merge",
           fill = "right")



exemplo_separate %>% 
  separate(col = var1,
           into = c("a", "b", "c"),
           sep = ",",
           extra = "merge",
           fill = "left")


### unite
table5

?unite

table5 %>% 
  unite(col = ano_com_seculo,
        century, year,
        sep = "",
        remove = FALSE)

exemplo <- tibble(grupo = c("a", "a", "b","b"),
                  y = c("1, 2", "3;4", "1,2,3", "4"))

exemplo
?separate_rows

exemplo %>% 
  separate_rows(y,
                sep = ",")


# table1 >> table2
table1
table2
?pivot_longer

table1 %>% 
  pivot_longer(cols = c(year, cases),
               names_to = "type",
               values_to = "count")

# table2 >> table1
table2 %>% 
  pivot_wider(names_from = type,
              values_from = count)


#### capitulo 06
dados2016 <- data.frame(ano = c(2016, 2016, 2016), 
                        valor = c(938, 113, 1748), 
                        produto = c('A', 'B', 'C'))

dados2017 <- data.frame(valor = c(8400, 837, 10983), 
                        produto = c('H', 'Z', 'X'),
                        ano = c(2017, 2017, 2017))

dados2016
dados2017

rbind(dados2016, dados2017)
bind_rows(dados2016, dados2017)

df_esquerda <- tibble(a = c(1, 2, 3),
                      b = c(4, 5, 6))

df_direita <- tibble(c = c(1, 2, 3),
                     d = c(4, 5, 6))


df_esquerda
df_direita

bind_cols(df_esquerda, df_direita)




band_members
band_instruments

?inner_join
inner_join()

inner_join(band_members, band_instruments, by = "name")

band_instruments2
band_members

# como renomear uma coluna
band_instruments2 %>% 
  # sintaxe de rename(): rename(novo nome = nome antigo)
  rename(instrumento = plays)

band_instruments2

# sintaxe do by: by = c("nome da chave no df da esq." = "nome da chave no df da dir") 
inner_join(band_members, band_instruments2, by = c("name" = "artist"))



band_members
left_join(band_members, band_instruments, by = "name")


df_vendas <- tibble(cod_vendedor = c(1, 2, 1, 2, 3, 1),
                    vendas = c(10, 20, 30, 40, 0, 5))



df_vendedor <- tibble(cod_vendedor = c(1, 2, 3, 4),
                      nome_vendedor = c("Paulo", "Pedro", "Maria", "Viviane"),
                      uf = c("SP", "BA", "RJ", "SP"))

df_vendedor


df_vendas
df_vendedor

inner_join(df_vendedor, df_vendas, by = "cod_vendedor") %>% 
  group_by(nome_vendedor) %>% 
  summarise(vendas_total = sum(vendas))


left_join(df_vendedor, df_vendas, by = "cod_vendedor") %>% 
  mutate(
    #vendas = ifelse(is.na(vendas), 0, vendas)
    vendas = replace_na(vendas, 0)
      ) %>% 
  group_by(nome_vendedor) %>% 
  summarise(vendas_total = sum(vendas))

band_members
band_instruments
full_join(band_members, band_instruments)


library(nycflights13)
flights
airports

flights %>% select(origin, dest)


flights %>% 
  group_by(origin, dest) %>% 
  summarise(qtd_voos = n())
df_aeroportos <- airports %>% 
  select(faa, name)


flights %>% 
  count(origin, dest, sort = TRUE, name = "qtd_voos") %>% 
  left_join(df_aeroportos, by = c("origin" = "faa")) %>% 
  left_join(df_aeroportos, by = c("dest" = "faa")) %>% 
  rename(Origem = name.x,
         Destino = name.y) %>% 
  select(Origem, Destino, qtd_voos)



flights %>% 
  count(origin, dest, sort = TRUE, name = "qtd_voos") %>% 
  left_join(airports, by = c("origin" = "faa")) %>% 
  rename(Origem = name) %>% 
  left_join(airports, by = c("dest" = "faa")) %>% 
  rename(Destino = name) %>% 
  select(Origem, Destino, qtd_voos)



?airports

glimpse(flights)




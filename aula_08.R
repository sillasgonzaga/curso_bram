library(tidyverse)

df_teste <- data.frame(x1 = c(1, 2, 3),
                       x2 = c(4, 5, 6))

df_teste

# arquivos csv
write_csv(df_teste, "dataframe de teste.csv")

# arquivos rds
vetor_nomes <- c("Ricardo", "Patricia", "Kleber")
write_rds(vetor_nomes, "vetor de nomes.rds") 

x <- read_rds("vetor de nomes.rds")
x

library(lubridate)
df_teste$data_hoje <- today()
df_teste

library(writexl)
write_xlsx(df_teste, "planilha excel.xlsx")

### WEB SCRAPING ####

library(rvest)

url <- "https://pt.wikipedia.org/wiki/Campeonato_Brasileiro_de_Futebol_de_2018_-_Série_A"
url <- read_html(url)

url

tb <- html_table(url, fill = TRUE)
tb <- tb[[6]]

tb <- as_tibble(tb)
tb

tb %>% 
  select(Equipes, P)

tb %>% 
  as_tibble() %>% 
  mutate(SG = ifelse(str_detect(SG,"\\+"),
                     readr::parse_number(SG),
                     -1 * readr::parse_number(SG)))



a <- c(1, 2, "c", 4)
a

minha_primeira_lista <- list(1, 2, "c", 4)
minha_primeira_lista

class(a)
class(minha_primeira_lista)

iris
data_frame <- head(iris)
data_frame
elemento_unico_inteiro <- 1
elemento_unico_inteiro
um_na <- NA
um_na
letters
vetor_string <- letters[1:5]
vetor_string
modelo_regressao <- lm(mpg ~ wt, data = mtcars)
modelo_regressao
?mtcars
vetor_nomes <- c("Fulano", "Ciclano")


minha_list <- list(tabela = data_frame,
                   elemento_unico_inteiro,
                   um_na,
                   vetor_string,
                   modelo_regressao,
                   nomes_dos_alunos = vetor_nomes)

minha_list

vetor_nomes[2]
minha_list[[2]]
minha_list[[6]]

minha_list$tabela
minha_list$nomes_dos_alunos
minha_list[[6]]

class(minha_list)

1:length(minha_list)

for(i in 1:length(minha_list)){
  
  print(
    class(minha_list[[i]])
    )
  
}

class(minha_list[[1]])
class(minha_list[[2]])

library(purrr)

# map()
# SINTAXE: map(lista, funcao)
map(minha_list, class)


vetor_numerico <- c(-4, 3, -1, 0)
lista_numerica <- list(-4, 3, -1, 0)

abs(vetor_numerico)
abs(lista_numerica)

map(lista_numerica, abs)
# é equivalente a
lista_numerica %>% map(abs)

lista_vetores <- list(v1 = c(1.5789, 2, 3.14578),
                      v2 = c(0, 2))

lista_vetores

map(lista_vetores, round, 2)
lista_vetores %>% map(round, 2)

# definir uma lista composta por vetores
v <- list(v1 = c(1, 3, 5, 10, 20), v2 = c(2, 4, 6), v3 = c(7, 8, 9))
v
# aplicar a raiz quadrada a todos os vetores
map(v, sqrt)

map(v, sum)
map(v, sum) %>% class()

unlist(map(v, sum))

map_dbl(v, sum)


sum(v$v1 ^ 2)

minha_funcao_aleatoria <- function(x){
  sum(x^2)
}

minha_funcao_aleatoria(v$v1)


map(v, minha_funcao_aleatoria)


map(v, function(z) sum(z^2))
# sintaxe do til 
map(v, ~ sum(. ^ 2))
map(v, ~ sum(.x ^ 2))

v
map(v, class)
map_chr(v, class)

v <- list(v1 = c(1, 3, 5), v2 = c(2, 4, 6), v3 = c(7, 8, 9))
map_dfc(v, function(x) x * 2)

bind_cols(v)


v %>% 
  map(sqrt) %>% 
  map(sum)


#### projeto purrr ####
library(lubridate)
df <- readr::read_csv("energia/AEP_hourly.csv")
df
nome_regiao <- colnames(df)[2]

str_remove(nome_regiao, "_MW")
nome_regiao <- str_sub(nome_regiao, 1, -4)

df %>% 
  rename(consumo = 2) %>% 
  mutate(regiao = nome_regiao,
         mes = month(Datetime)) %>% 
  group_by(mes, regiao) %>% 
  summarise(consumo_medio = mean(consumo))

ler_e_agregar_dados <- function(x){
  
  df <- readr::read_csv(x)
  
  nome_regiao <- colnames(df)[2]
  nome_regiao <- str_sub(nome_regiao, 1, -4)
  
  output <- df %>% 
    rename(consumo = 2) %>% 
    mutate(regiao = nome_regiao,
           mes = month(Datetime)) %>% 
    group_by(mes, regiao) %>% 
    summarise(consumo_medio = mean(consumo))
  
  return(output)
}

ler_e_agregar_dados("energia/AEP_hourly.csv")
ler_e_agregar_dados("energia/DAYTON_hourly.csv")

arquivos <- dir("energia", full.names = TRUE, pattern = "hourly.csv$")
arquivos

df_estacoes <- map_dfr(arquivos, ler_e_agregar_dados)
class(df_estacoes)
df_estacoes

#readr::read_csv("energia/pjm_hourly_est.csv")







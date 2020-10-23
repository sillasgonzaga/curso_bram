as.numeric("1")

as.numeric("Z")


vezes_10 <- function(x){
  # corpo da funcao
  #stopifnot(is.numeric(x))
  
  if(is.numeric(x)){
    return(x * 10)
  }else{
    stop("O objeto de input usado não é numérico")
  }
  
}

vezes_10(5)
vezes_10(0)
vezes_10(-6)
vezes_10("casa")
# stopifnot

#stopifnot(4 > 5, "Conta errada")


imc <- function(peso, altura){
  peso/altura^2
}

imc(70, 1.85)
imc(60, 1.90)
imc(1.65, 120)
imc(altura = 1.65, peso = 120)

library(tidyverse)

df <- readr::read_csv("senado.csv")
df

# sintaxe: nome do dataframe $ nome da coluna
length(df$SenatorUpper)
unique(df$SenatorUpper)
length(unique(df$SenatorUpper))

unique(df$Party)
length(unique(df$Party))

vetor_pares <- c(2, 4, 6, 8)

vetor_pares[2]
vetor_pares[3]
vetor_pares[3]
vetor_pares[3]
vetor_pares[4]
vetor_pares[c(1, 3)]

df[4, "SenatorUpper"]
df[4, 3]

10:20
?seq
seq(from = 10, to = 20, by = 1) # é o mesmo que 10:20


df[df$SenatorUpper == "MARCELO CRIVELLA", ]


# operadores de comparação
# ==
# >, >=, <=, <
# != (diferente de)
4 != 5
6 != 6
# | (OU)
3 > 5 | 6 > 5
3 > 5 | 1 > 5
# & (AND)
3 > 5 & 6 > 5
8 > 5 & 10 > 5 & 20 > 1 & 4 > 2 & 0 > 1





# g(f(x))
length(df$SenNumber)
# f(x)


# g(f(x)) ----- x _ f() _ g()
# %>% : ctrl shift M
df$Party %>% unique() %>% length() # length(unique(df$SenatorUpper))


df
select(df, SenatorUpper, Vote, Party)
df %>% select(SenatorUpper, Vote, Party)

df %>% select(-SenNumber)
df %>% select(-c(SenNumber, VoteNumber))

df_menor <- df %>% select(SenatorUpper, Party)

df_menor

#rm(list = ls())

df %>% select_at(vars(starts_with("S")))
df %>% select_at((starts_with("S")))

?starts_with


# select_if()
df %>% select_if(is.numeric)
is.numeric(df$VoteNumber)
is.numeric(df$SenatorUpper)
df %>% select_if(is.character)


### filter()
# sintaxe: df %>% filter(TESTE LOGICO)
df %>% filter(SenatorUpper == "MARCELO CRIVELLA")
df %>% filter(Party == "PSDB")
df %>% filter(Party == "PT" & State == "SP")
df %>% filter(Party == "PT", State == "SP") %>% distinct(SenatorUpper)
df %>% filter(Party == "PT", State == "SP") %>% select(SenatorUpper) %>%  unique()
df %>% filter(State == "BA" | State == "RJ")
df %>% filter(State == "SP" | State == "RJ" | State == "ES" | State == "MG")
# %in%
2 %in% c(1, 3, 5)
2 %in% c(1, 2, 3)
df %>% filter(State %in% c("SP", "RJ", "ES", "MG"))

df %>% 
  select(VoteNumber, PercentYes) %>% 
  filter(PercentYes > 90)

# mutate()
# sintaxe: mutate(dataframe, nome da coluna = f(...))

df %>% 
  select(SenatorUpper) %>% 
  mutate(senador_nome_minusculo = tolower(SenatorUpper))

unique(df$Party)

df %>%
  distinct(SenatorUpper, Party) %>%
  mutate(eh_de_esquerda = ifelse(Party %in% c("PT", "PSOL", "PCdoB"),
                                 1, 0)) %>%
  filter(eh_de_esquerda == 1)

df %>% 
  distinct(VoteNumber, PercentYes) %>% 
  mutate(PercentYes = PercentYes/100)

# <- é equivalente a =

df %>% filter(Party = "PT")



# 10 > 3 & 4 > 1
# 10 > 3 & 4 > 5
# 10 > 3 , 4 > 5

df %>% select_if(is.numeric) 

df %>% mutate_if(is.character, tolower)

library(writexl)
#write_xlsx(df, "arquivo do senado.xlsx")

df %>%
  distinct(SenatorUpper, Party) %>%
  mutate(eh_de_esquerda = ifelse(Party %in% c("PT", "PSOL", "PCdoB"),
                                 1, 0)) %>%
  filter(eh_de_esquerda == 1) %>% 
  write_xlsx("senadores de esquerda.xlsx")


library(nycflights13)
glimpse(flights)

flights

mean(flights$dep_delay, na.rm = TRUE)
.63 * 60
unique(flights$month)


flights %>% 
  select(month, dep_delay) %>% 
  group_by(month) %>% 
  mutate(atraso_medio_decolagem = mean(dep_delay, na.rm = TRUE))


flights %>% 
  select(month, dep_delay) %>% 
  group_by(month) %>% 
  summarise(atraso_medio = mean(dep_delay, na.rm = TRUE),
            soma_atraso = sum(dep_delay, na.rm = TRUE))


vetor_qualquer <- c(1 , 5, NA, 10)
mean(vetor_qualquer, na.rm = TRUE)

flights %>% 
  group_by(carrier) %>% 
  summarise(atraso_medio = mean(dep_delay, na.rm = TRUE),
            soma_atraso = sum(dep_delay, na.rm = TRUE)) %>% 
  # ordenar os dados
  arrange(atraso_medio)



flights %>% 
  group_by(carrier) %>% 
  summarise(atraso_medio = mean(dep_delay, na.rm = TRUE),
            soma_atraso = sum(dep_delay, na.rm = TRUE)) %>% 
  # ordenar os dados
  arrange(desc(atraso_medio))

flights %>% 
  group_by(month, carrier)


# contagem com  summarise
flights %>% 
  group_by(carrier) %>% 
  summarise(qtd_voos = n()) %>% 
  arrange(desc(qtd_voos))

# contagem com count()
flights %>% 
  count(carrier, sort = TRUE)


flights %>% 
  filter(carrier %in% c("UA", "B6", "EV")) %>% 
  group_by(month, carrier) %>% 
  summarise(atraso_medio = mean(dep_delay, na.rm = TRUE)) %>% 
  arrange(month, desc(atraso_medio)) %>% 
  print(n = 50)











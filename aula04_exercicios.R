library(tidyverse)

senado <- readr::read_csv("senado.csv")

# 1
senado %>% filter(is.na(State))
# is.finite()
senado2 <- senado %>% filter(!is.na(State))
!TRUE

# 2
senado2 %>% 
  distinct(Party, GovCoalition) %>% 
  arrange(GovCoalition)

senado2 %>% 
  distinct(Party, GovCoalition) %>% 
  filter(!GovCoalition)



# 3
#length(unique())
senado2 %>% 
  group_by(Party) %>% 
  summarise(qtd_senadores = n_distinct(SenatorUpper),
            qtd_senadores_2 = length(unique(SenatorUpper))
            )

senado2 %>% 
  select(GovCoalition, Party, Vote) %>% 
  mutate(votou_sim = ifelse(Vote == "S", 1, 0)) %>% 
  group_by(GovCoalition, Party) %>% 
  summarise(proporcao_sim = mean(votou_sim),
            qtd_votos = n()) %>% 
  arrange(proporcao_sim)

senado2 %>% 
  select(Party, Vote) %>% 
  group_by(Party) %>% 
  summarise(YesVote = sum(Vote == "S" ),
            NoVote = sum(Vote == "N" )) %>%
  mutate(propocao_yes = YesVote / (YesVote + NoVote))

senado2 %>% 
  distinct(Vote)


vetor_binario <- c(1, 0, 1, 1, 0)
sum(vetor_binario)
mean(vetor_binario)

# exemplo de group_by() + mutate()
ex <- tibble(
  grupo = c("a", "a", "a", "b", "b", "b"),
  x = c(1, 2, 3, 1, 2, 3)
)

ex %>% 
  mutate(valor_anterior = lag(x))

ex %>% 
  group_by(grupo) %>% 
  mutate(valor_anterior = lag(x))


letras <- c("a", "b", "c", "d")
letras
lag(letras)
lag(c(4, 6, 8, 1, 0, -5))

letras
lead(letras)



#### ----------- parte 2 - airbnb
# 1
df_anuncios <- readr::read_csv("listings.csv.gz")

# 2
summary(df_anuncios)
glimpse(df_anuncios)

# 3

# 4
mean(df_anuncios$price)


as.numeric("14.7%")
as.numeric("$2,000.15")

x <- "$2,000.15"
as.numeric(x)
parse_number(x)
parse_number("14.7%")


df_anuncios %>% 
  select(price) %>% 
  mutate(price = parse_number(price))

class(df_anuncios$price)

# alterar a coluna price
df_anuncios <- df_anuncios %>% 
  mutate(price = parse_number(price))

# read_csv() --- read_csv2()
glimpse(df_anuncios)

# df_anuncios %>% 
#   mutate(weekly_price = parse_number(weekly_price),
#          monthly_price = parse_number(monthly_price))


df_anuncios <- df_anuncios %>% 
  mutate_at(vars(weekly_price, monthly_price, security_deposit,
                 cleaning_fee, extra_people),
            parse_number)

glimpse(df_anuncios)

# 5
df_anuncios %>% 
  select(listing_url, price) %>% 
  arrange(desc(price))

# 6
glimpse(df_anuncios)

df_anuncios %>% 
  distinct(host_name, host_listings_count) %>% 
  arrange(desc(host_listings_count))

# 7
library(lubridate)

df_anuncios %>% 
  select(host_since) %>% 
  mutate(ano = year(host_since)) %>% 
  count(ano)

# resultado correto sem duplicatas
df_anuncios %>% 
  distinct(host_since, host_id) %>% 
  mutate(ano = year(host_since)) %>% 
  count(ano)

exemplo <- tibble(
  var1 = c("a", "a", "a", "b", "b", "b"),
  var2 = c(1, 2, 1, 1, 2, 1)
)
exemplo


exemplo %>% 
  distinct(var1)

exemplo %>% 
  distinct(var1, var2)


# 8
str_detect(c("Paulo", "Fulano", "Cicleno"), "e")
str_detect(c("Laranja", "banana", "pera"), "l")

df_anuncios %>% 
  select(name, space) %>% 
  filter(str_detect(space, "praia"))

# 9
df_anuncios %>% 
  select(space, price) %>% 
  mutate(tem_praia = str_detect(space, "praia")) %>% 
  group_by(tem_praia) %>% 
  summarise(preco_medio = mean(price))

df_anuncios %>% 
  select(space, price) %>% 
  mutate(space_minusculo = tolower(space),
         tem_praia_ou_beach = str_detect(space_minusculo, "praia|beach")#,
         #tem_praia_ou_beach = str_detect(space_minusculo, "praia") | str_detect(space_minusculo, "beach")
         )%>% 
  group_by(tem_praia_ou_beach) %>% 
  summarise(preco_medio = mean(price))


# 10
df_anuncios <- df_anuncios %>% 
  mutate(esgotado = availability_30 == 0)

glimpse(df_anuncios)

# 11
df_anuncios %>% 
  group_by(neighbourhood) %>% 
  summarise(qtd_anuncios = n(),
            taxa_esgotado = mean(esgotado)) %>% 
  filter(qtd_anuncios > 100) %>% 
  arrange(desc(taxa_esgotado)) %>% 
  head(5)


# 12
df_anuncios %>% 
  group_by(neighbourhood) %>% 
  summarise(qtd_anuncios = n(),
            qtd_reviews = sum(number_of_reviews)) %>% 
  arrange(desc(qtd_anuncios)) %>% 
  mutate(taxa_review_por_bairro = qtd_reviews / qtd_anuncios)

# 13
df_anuncios %>% 
  distinct(room_type)

unique(df_anuncios$room_type)

# 14
df_anuncios %>% 
  select(room_type, bedrooms, price) %>% 
  filter(room_type == "Entire home/apt") %>% 
  group_by(bedrooms) %>% 
  summarise(preco_medio = mean(price),
            preco_mediano = median(price),
            qtd_anuncios = n())


pessoas_bar <- c(1000, 800, 900, 2000, 1100)
mean(pessoas_bar)
median(pessoas_bar)
pessoas_bar_2 <- c(pessoas_bar, 1000000)
mean(pessoas_bar_2)
median(pessoas_bar_2)




# 15
df_anuncios %>% 
  select(listing_url, neighbourhood, host_neighbourhood,
         availability_30, minimum_nights, 
         security_deposit, instant_bookable,
         guests_included, price, room_type, number_of_reviews,
         cleaning_fee, review_scores_rating) %>% 
  filter(
    neighbourhood %in% c("Copacabana", "Ipanema", "Leblon"),
    #host_neighbourhood %in% c("Copacabana", "Ipanema", "Leblon")
    host_neighbourhood == neighbourhood,
    security_deposit == 0,
    instant_bookable,
    room_type == "Entire home/apt",
    guests_included >= 2
  ) %>% 
  mutate(preco_total_viagem = cleaning_fee + price * 5) %>% 
  select(listing_url, preco_total_viagem) %>% 
  arrange(preco_total_viagem)

df_anuncios %>% 
  select(guests_included)








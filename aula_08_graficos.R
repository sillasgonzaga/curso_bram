library(readxl)
library(tidyverse)
library(ISLR)
library(RColorBrewer)
library(ggthemes)
library(hrbrthemes)
library(treemapify)
library(gapminder)
library(sf)
library(geobr)
library(janitor)
# novos:
library(ggrepel)
library(countrycode)
library(patchwork)
library(wpp2019)
library(rbcb) # remotes::install_github('wilsonfreitas/rbcb')

# baixe o arquivo do endereco:
download.file("https://s3.amazonaws.com/happiness-report/2018/WHR2018Chapter2OnlineData.xls",
              destfile = "felicidade.xls",
              mode = "wb")

#install.packages(c("countrycode", "patchwork", "wpp2019"))
# remotes::install_github('wilsonfreitas/rbcb')
library(readxl)
df_feliz <- read_excel("felicidade.xls")
library(janitor)
df_feliz <- df_feliz %>% clean_names()
df_feliz

df_continente <- countrycode::codelist  %>% 
  # selecionar colunas importantes
  select(country = country.name.en,
         continent, 
         country_code = iso3n) %>% 
  # filtrar fora os paises sem continentes
  filter(!is.na(continent))

df_feliz$country[df_feliz$country == "Bosnia and Herzegovina"] <- "Bosnia & Herzegovina"
df_feliz$country[df_feliz$country == "Czech Republic"] <- "Czechia"
df_feliz$country[df_feliz$country == "Hong Kong S.A.R. of China"] <- "Czechia"
df_feliz$country[df_feliz$country == "Taiwan Province of China"] <- "Taiwan"

df_feliz <- inner_join(df_feliz, df_continente, by = "country")

glimpse(df_feliz)

library(wpp2019)
data(pop)
head(pop)

df_populacao <- pop %>% 
  select(country_code, `2020`) %>% 
  rename(populacao_2020 = 2)

head(df_populacao)

df_feliz <- df_feliz %>% 
  left_join(df_populacao, by = 'country_code')

glimpse(df_feliz)

writexl::write_xlsx(df_feliz, "indice_felicidade.xlsx")

library(readxl)
df_feliz <-read_excel("indice_felicidade.xlsx")
df_feliz




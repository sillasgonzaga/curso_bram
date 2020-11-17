library(tidyverse)
library(readxl)

df_feliz <- read_excel("indice_felicidade.xlsx")

df_feliz

# como criar um grafico com o pacote ggplot2

ggplot(data = df_feliz)

glimpse(df_feliz)

# dados de quais anos?
sort(unique(df_feliz$year))

df_2017 <- df_feliz %>% 
  filter(year == max(year))

glimpse(df_2017)

# como calcular correl
temp <- df_2017 %>% 
  select(log_gdp_per_capita, healthy_life_expectancy_at_birth) %>% 
  na.omit()


cor(temp$log_gdp_per_capita, temp$healthy_life_expectancy_at_birth)

# Y = f(X)

ggplot(data = df_2017,
       aes(x = log_gdp_per_capita,
           y = healthy_life_expectancy_at_birth)) +
  geom_point()

# é equivalente a
ggplot(df_2017) +
  geom_point(aes(x = log_gdp_per_capita,
                 y = healthy_life_expectancy_at_birth,
                 color = continent))

ggplot(data = df_2017,
       aes(x = log_gdp_per_capita,
           y = healthy_life_expectancy_at_birth,
           color = continent)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(data = df_2017,
       aes(x = log_gdp_per_capita, 
           y = healthy_life_expectancy_at_birth)) +
  geom_point(aes(color = continent)) +
  geom_smooth(method = "lm")

# geom_smooth()

# alpha
# colour
# fill
# shape
# size
# stroke

ggplot(data = df_2017,
       aes(x = log_gdp_per_capita,
           y = healthy_life_expectancy_at_birth)) +
  geom_point(color = "purple",
             alpha = 1,
             #fill = "red",
             #shape = 8
             size = 5
             )

df_2017 %>% 
  ggplot(aes(x = log_gdp_per_capita,
             y = healthy_life_expectancy_at_birth)) +
  geom_point(aes(shape = continent))


df_2017 %>% 
  ggplot(aes(x = log_gdp_per_capita,
             y = healthy_life_expectancy_at_birth)) +
  geom_point(aes(size = populacao_2020))

df_2017 %>% 
  select(country, populacao_2020) %>% 
  arrange(desc(populacao_2020))

df_2017 %>% 
  group_by(continent) %>% 
  summarise(expec_media = mean(healthy_life_expectancy_at_birth)) %>% 
  ggplot(aes(x = continent,
             y = expec_media)) +
  geom_col(#color = "red",
           fill = "purple",
           alpha = 0.4)



df_2017 %>% 
  group_by(continent) %>% 
  summarise(expec_media = mean(healthy_life_expectancy_at_birth)) %>% 
  ggplot(aes(x = continent,
             y = expec_media)) +
  geom_col(aes(fill = continent))


?fct_reorder

df_2017 %>% 
  group_by(continent) %>% 
  summarise(expec_media = mean(healthy_life_expectancy_at_birth)) %>% 
  ggplot(aes(x = fct_reorder(continent, expec_media, .desc = TRUE),
             y = expec_media)) +
  geom_col(size = 0)

min(df_feliz$year)

df_feliz %>% 
  filter(year %in% c(2005, 2017)) %>% 
  group_by(continent, year) %>% 
  summarise(expec_media = mean(healthy_life_expectancy_at_birth)) %>% 
  ggplot(aes(x = continent, y = expec_media, fill = as.character(year))) +
  geom_col(position = "dodge")

#### grafico de linhas ####
df_feliz %>% 
  filter(year > 2006) %>% 
  select(year, life_expec = healthy_life_expectancy_at_birth, continent) %>% 
  group_by(year) %>% 
  summarise(life_expec_media = mean(life_expec)) %>% 
  ggplot(aes(x = year, y = life_expec_media)) +
  geom_line(alpha = 1,
            size = 0.8,
            color = "red",
            linetype = "dashed") +
  geom_point()



df_feliz %>% 
  select(year, life_expec = healthy_life_expectancy_at_birth, continent) %>% 
  group_by(year) %>% 
  summarise(life_expec_media = mean(life_expec),
            qtd_paises = n())


df_feliz %>% 
  filter(year > 2006) %>% 
  select(year, life_expec = healthy_life_expectancy_at_birth, continent) %>% 
  group_by(year, continent) %>% 
  summarise(life_expec_media = mean(life_expec)) %>% 
  ggplot(aes(x = year, 
             y = life_expec_media, 
             color = continent)) +
  geom_line() +
  geom_point()

library(rbcb)

lista_datasets <- rbcb::get_series(code = c(ipca = 433, selic = 4390))

lista_datasets$ipca
lista_datasets$selic

df_st <- left_join(lista_datasets$ipca,
                   lista_datasets$selic,
                   by = "date")
writexl::write_xlsx(df_st, "series_temporais_bacen.xlsx")

df_st
tail(df_st)

library(readxl)

df_st <- read_excel("series_temporais_bacen.xlsx")

df_st <- df_st %>% filter(date >= as.Date("1995-01-01"))

df_st <- df_st %>% 
  mutate(date = as.Date(date))

df_st %>%
  ggplot(aes(x = date, y = ipca)) +
  geom_line()

df_st

# date | indicador | taxa
df_st %>% 
  pivot_longer(cols = c(ipca, selic),
               #cols = -date
               names_to = "indicador",
               values_to = "taxa") %>% 
  ggplot(aes(x = date,
             y = taxa,
             color = indicador)) +
  geom_line() +
  scale_x_date(date_breaks = "5 years",
               date_label = "%Y-%m")
  

df_st %>% 
  pivot_longer(cols = c(ipca, selic),
               #cols = -date
               names_to = "indicador",
               values_to = "taxa") %>% 
  ggplot(aes(x = date,
             y = taxa,
             color = indicador)) +
  geom_line() +
  scale_x_date(date_breaks = "2 years",
               date_label = "%Y")


df_st %>% 
  filter(date >= as.Date("2019-01-01")) %>% 
  pivot_longer(cols = c(ipca, selic),
               #cols = -date
               names_to = "indicador",
               values_to = "taxa") %>% 
  ggplot(aes(x = date,
             y = taxa,
             color = indicador)) +
  geom_line() +
  scale_x_date(date_breaks = "2 months",
               date_labels = "%y/%m")


#### histogramas ----
df_2017 %>% 
  ggplot(aes(x = healthy_life_expectancy_at_birth)) +
  #geom_histogram(bins = 10, color = "red")
  geom_histogram(binwidth = 5, color = "red", boundary = 5)
  
df_2017 %>% 
  ggplot(aes(x = healthy_life_expectancy_at_birth)) +
  #geom_histogram(bins = 10, color = "red")
  geom_histogram(binwidth = 0.5, color = "red", boundary = 5)


df_2017 %>% 
  ggplot(aes(x = healthy_life_expectancy_at_birth,
             fill = continent)) +
  geom_histogram(binwidth = 5,  boundary = 5)

#### boxplot
df_2017 %>% 
  ggplot(aes(x = continent, y = healthy_life_expectancy_at_birth)) +
  geom_boxplot()






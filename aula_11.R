library(tidyverse)

df <- readr::read_csv("https://raw.githubusercontent.com/sillasgonzaga/curso_series_temporais/master/data/Bike-Sharing-Dataset/day.csv")

df_transf <- df %>% 
  # remover colunas irrelevantes
  select(-c(instant, workingday)) %>% 
  # renomear algumas colunas
  rename(
    estacao = season,
    total = cnt,
    year = yr, 
    month = mnth
  ) %>% 
  # mudar weekday, que começa a contar do zero
  mutate(weekday = weekday + 1) %>% 
  # transformar a variavel de feriado para texto
  mutate(holiday = as.character(holiday)) %>% 
  # mudar os valores de algumas variaveis
  mutate(
    # substituir o codigo do ano  pelo ano real
    year = lubridate::year(dteday),
    # adicionar um leading zero no mês
    month = str_pad(month, width = 2, side = "left", pad = "0"),
    # converter weathersit para variavel do tipo factor
    weathersit = factor(weathersit,
                        levels = 1:4,
                        labels = c("muito bom", "bom", "ruim", "muito ruim")),
    # converter dia da semana para variavel do tipo factor
    weekday = factor(weekday, 
                     levels = 1:7,
                     labels = c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sab")),
    # fazer o mesmo para estacao
    estacao = factor(estacao, 
                     levels = 1:4,
                     labels = c("Inverno", "Primavera", "Verao", "Outono")),
    # converter colunas numericas para escala normal (não-normalizada)
    temp = temp * 41,
    atemp = atemp * 50,
    hum = hum * 100,
    windspeed = windspeed * 67
  )

df_transf <- df_transf %>% 
  select(-casual, - registered)


glimpse(df_transf)

df_transf %>% 
  ggplot(aes(x = dteday, y = total)) +
  geom_line(aes(color = estacao,
                group = "Qualquer coisa")) +
  # adicionar curva de tendencia
  geom_smooth(se = FALSE) +
  theme_bw() +
  # quebrar eixo x em 1 mes
  scale_x_date(date_breaks = "2 month",
               date_labels = "%m/%Y",
               minor_breaks = NULL) +
  # inverter eixos
  theme(axis.text.x = element_text(angle = 90))

library(scales)
  df_transf %>% 
    ggplot(aes(x = temp, y = total)) +
    geom_point() +
    scale_y_continuous(breaks =  breaks_width(1000))

cor(df_transf$temp, df_transf$total)

# variavel resposta ~ variavel de input
# # variavel resposta ~ variavel de input1 + variavel de input 2 + ... 
modelo_simples <- lm(total ~ temp, data = df_transf)

modelo_simples

# Y = B0 + B1(X)
# y = 1215 + 162X

1215 + 162 * 20

df_transf %>% 
  ggplot(aes(x = temp, y = total, color = estacao)) +
  geom_point() +
  scale_y_continuous(breaks =  breaks_width(1000))

modelo_2 <- lm(total ~ temp + estacao, data = df_transf)
modelo_2

summary(modelo_simples)
summary(modelo_2)




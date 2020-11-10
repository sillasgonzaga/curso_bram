library(stringr)


vetor_nomes <- c("Alana", "Alberto", "Fulano.Silva", "Fred")

str_replace(vetor_nomes, "a", "@")
"A" == "a"

str_replace_all(vetor_nomes, "a", "@")

# subsituir tanto a como A por @
str_replace_all(tolower(vetor_nomes), "a", "@")
# via regex
#str_replace(vetor_nomes, "\\.", "#")
str_replace_all(vetor_nomes, "a|A|o", "@")

# usando str_replace_all() para detectar se um string contem um substring
str_replace_all(vetor_nomes, "a", "") == vetor_nomes

vetor_nomes
str_count(vetor_nomes, "a|A")
str_count(vetor_nomes, "a|A") > 0

str_detect(vetor_nomes, "a|A")


cpfs <- c(1234, 01833827570, 45614814570, 4, 4000001111, 12345678912345)
cpfs
as.character(cpfs)

str_length(cpfs)
length(cpfs)

?str_pad

vetor_cpfs_arrumado = str_pad(cpfs, width = 11, pad = "0", side = "left")
str_pad(cpfs, width = 11, pad = "0", side = "right")

str_length(vetor_cpfs_arrumado)


arrumar_cpfs <- function(x, contar_do_inicio = TRUE){
  
  if (contar_do_inicio){
    ifelse(
      # teste logico
      str_length(x) <= 11,
      # se TRUE
      str_pad(x, width = 11, pad = "0", side = "left"),
      # se FALSE
      str_sub(x, 1 , 11)
    )
  } else{
    ifelse(
      # teste logico
      str_length(x) <= 11,
      # se TRUE
      str_pad(x, width = 11, pad = "0", side = "left"),
      # se FALSE
      str_sub(x, start = -11)
    )
  }
}
# Como remover notaçao cientifica
options(scipen = 999)
cpfs
arrumar_cpfs(cpfs, contar_do_inicio = TRUE)
arrumar_cpfs(cpfs, contar_do_inicio = FALSE)

str_pad(cpfs, 11, side = 'both', pad = "#")

x <- c("      inicio", 
       "final      ",
       "      inicio e final      ",
       "    no       meio       de         2020 ")
x
str_trim(x)
str_squish(x)


textos <- c("Fulano", "fulano", "abcdeF", "01584", 
            "abc456", "123def", "OI", "meuemail@gmail.com",
            "www.google.com", "Meu nome é Fulano")


textos
# detectar strings que contem F maiusculo
str_detect(textos, "F")

#
str_replace_all(textos, "F", "#")
str_detect(textos, "F")

# começa com F
str_detect(textos, "^F")
str_replace_all(textos, "^F", "#")


str_detect(textos, regex("^F", ignore_case = TRUE))


textos %>% tolower() %>% str_detect("^f")

# termina com "o"
str_detect(textos, "o")
str_detect(textos, "o$")

str_subset(textos, "o$")

# contem algarismo
str_subset(textos, "\\d")
str_subset(textos, "[0-9]")


#######################


# YYYY-MM-DD
minha_data <- "2020-11-05"
class(minha_data)
minha_data + 7


minha_data <- as.Date("2020-11-05")
minha_data
class(minha_data)
minha_data + 7



library(bizdays)
install.packages("bizdays")

library(bizdays)
# criar um objeto de calendario
install.packages("ctv")
library("ctv")

#(cal <- Calendar(holidaysANBIMA, weekdays=c('saturday', 'sunday'), name='ANBIMA'))
?create.calendar
calendario <- create.calendar(name = "Brasil",
                              holidays = holidaysANBIMA,
                              weekdays = c("saturday", "sunday"))
calendario
holidaysANBIMA

?add.bizdays()
minha_data + 7 # dias corridos
add.bizdays(minha_data, 7, calendario)

ISOdate(2020, 11, 5)

x <- c("2014-07-15", "2018/03/20", "2019-12-31", "20170511")
as.Date(x)

library(lubridate)
as_date(x)
class(as_date(x))

library(tidyverse)

?seq.Date

seq.Date(from = as.Date("2020-01-01"),
         # by: numero + unidade de tempo em ingles
         by = "1 month",
         length.out = 12)

seq.Date(from = as.Date("1996-02-01"),
         to = as.Date("1996-03-01") - 1,
         by = "1 day")



datas_brasil <- c("01/12/2019", "20/11/2018", "30011990", "17-03-2000")

datas_brasil
class(datas_brasil)

dmy(datas_brasil)
?mdy()

library(lubridate)
today()

now()
class(now())

dmy_hms("05-11-2020 21:07:03")

datas_brasil <- dmy_hms(c("01/12/2019 13:51:15", "20/11/2018 00:00:00", "30011990 080000", "17-03-2000 203000"))
datas_brasil

day(datas_brasil)
month(datas_brasil)
month(datas_brasil, label = TRUE, abbr = FALSE)
year(datas_brasil)
quarter(datas_brasil)
week(datas_brasil)
wday(datas_brasil)
wday(datas_brasil, label = TRUE, abbr = FALSE)
?wday
mday(datas_brasil)
yday(datas_brasil)

hour(datas_brasil)
minute(datas_brasil)
second(datas_brasil)
datas_brasil

datas_brasil + 7

# readxl::read_excel()
as.Date(datas_brasil) + 7

datas_brasil + ddays(8)
datas_brasil - ddays(8)


?ddays()
datas_brasil
datas_brasil + dmonths(3)

datas_brasil
datas_brasil + dhours(4)

data1 <- dmy_hms("01/09/1993 20:00:00")
data2 <- dmy_hms("24-06-2018 17:00:00")

data2 - data1

?difftime

difftime(time1 = data2, time = data1, units = "days")/365

class(difftime(time1 = data2, time = data1, units = "days"))

as.numeric(difftime(time1 = data2, time = data1, units = "days"))/365


datas_brasil
floor_date(datas_brasil, "month")
floor_date(today(), "week")


ceiling_date(datas_brasil, "month") - ddays(1)
floor_date(as_date("2020-11-25"), "week")


# desafio 1
# para uma data de nascimento, conte quantos aniversarios caíram numa segunda
niver <- as_date("1993-09-01")

vetor_datas_niver <- seq.Date(from = niver,
                              by = "1 year",
                              to = today())

vetor_datas_niver
x <- wday(vetor_datas_niver)
sum(x == 2)
mean(x == 2)


proporcao_nivers_na_segunda <- function(dta){
  
  dta = as_date(dta)
  vetor_datas_niver <- seq.Date(from = dta,
                                by = "1 year",
                                to = today())
  
  x <- wday(vetor_datas_niver)
  mean(x == 2)
  
}

proporcao_nivers_na_segunda("1993-09-01")
proporcao_nivers_na_segunda("1994-03-27")

proporcao_nivers_no_dia_semana <- function(dta, dia_semana){
  
  dta = as_date(dta)
  vetor_datas_niver <- seq.Date(from = dta,
                                by = "1 year",
                                to = today())
  
  x <- wday(vetor_datas_niver)
  mean(x == dia_semana)
  
}

proporcao_nivers_no_dia_semana("1993-09-01", 7)


# desafio 2
# para uma data qualquer, retorne todos os dias do mês da data que caem 
# numa segunda-feira

retornar_datas <- function(dta, dia_semana = 2){
  
  dta = as_date(dta)
  vetor_datas_mes <- seq.Date(from = floor_date(dta, "month"),
                              to = ceiling_date(dta, "month") - ddays(1),
                              by = "1 day"
                              ) 
  
  vetor_datas_mes[wday(vetor_datas_mes) == dia_semana]
  
}


retornar_datas("1993-09-01", 7)
retornar_datas(today(), 4)
retornar_datas("2020-02-15", 4)



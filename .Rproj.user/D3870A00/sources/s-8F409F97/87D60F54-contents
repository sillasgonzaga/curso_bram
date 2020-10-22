# ctrl L
library(tidyverse)

arquivo_senado <- "senado.csv"
df <- readr::read_csv("senado.csv")
# é equivalente a 
df <- read_delim("senado.csv", delim = ",")
df


# read_csv2()
# é equivalente a 
# read_delim("senado.csv", delim = ";)

df <- readr::read_csv(arquivo_senado)


df

x <- 3.5

class(x)
class(df)

glimpse(df)

summary(df)
# importando arquivo excel
library(readxl)
?read_excel()

read_excel("planilha.xlsx", sheet = 2)

readxl::excel_sheets("planilha.xlsx")

read_excel("planilha.xlsx", sheet = "Planilha2")

glimpse(df)


inteiro <- 928L
outro.inteiro <- 5e2 # 5 x 10^2
decimal <- 182.93
caracter <- 'exportação'
outro.caracter <- "199"
logico <- TRUE
outro.logico <- FALSE

class(logico)
class(caracter)
class(outro.caracter)

inteiro + outro.inteiro
inteiro + outro.caracter

class(decimal)
class(inteiro)


"Meu nome é 'Sillas', prazer"
'Meu nome é "Sillas", prazer'

impares <- c(1, 3, 5, 7, 9)
class(impares)

vetor_nomes <- c("Sillas", "Roberto", "Rodrigo")
class(vetor_nomes)

vetor_misturado <- c("a", 2)
class(vetor_misturado)
vetor_misturado

as.character(2)
as.numeric("a")


vetor_misturado_2 <- c(TRUE, 8)
class(vetor_misturado_2)
4 == 5
2 == 2

TRUE == 1
FALSE == 0

glimpse(df)

sum(vetor_misturado_2)
mean(vetor_misturado_2)
sum(vetor_nomes)

vetor_idades <- c(20, 25, 18, "Não informado", 30)
mean(vetor_idades)
vetor_idades_num <- as.numeric(vetor_idades)
mean(vetor_idades_num)


vetor_idades_num[2]
vetor_idades_num[4]

length(vetor_idades_num)
vetor_idades_num[length(vetor_idades_num)] # vetor_idades_num[5]
vetor_idades_num[length(vetor_idades_num) - 1]


vetor_idades_num[4] <- 50
vetor_idades_num



#cria-se diferentes vetores
nome <- c('João', 'José', 'Maria', 'Joana')
idade <- c(45, 12, 28)
adulto <- c(TRUE, FALSE, TRUE, TRUE)
uf <- c('DF', 'SP', 'RJ', 'MG')
cor <- c("Branca", "Negra", "Branca", NA)

#cada vetor é uma combinação de elementos de um MESMO tipo de dados
#sendo assim, cada vetor pode ser uma coluna de um data.frame
clientes <- data.frame(nome, idade, adulto, uf, cor)
clientes
glimpse(clientes)

7 + TRUE
2015 > "2016"
"2015" > "2016"
"2014" < 2017
# em alguns casos a coerção irá falhar ou dar resultado indesejado
6 > 100
6 > "100" # "6" > "100"
"b" > "a"
"a" > "b"
1 + "1"

# aeb
# abz
"abz" < "aeb"

# 6
# 100
"6" > "100"
"6" > "1"

6 > 100


x1 <- c(10, NA, 20, NA, 30, 50, NA, NA, NA, 100)

x1[1] <- 5
x1

is.na(x1)
which(is.na(x1))
#x1[is.na(x1)]
x1 > 15
x1[x1 > 15]

x1[is.na(x1)] <- 0 # x1[c(2, 4, ...)] <- 0
x1


# imprimir algo na tela
print("Oi")
print("Tchau")


x = 800
print("Valor é menor que 500")

x2 = 600
print("Valor é maior ou igual a 500")


x3 < 500

x3 = 1000
if(x3 < 500){
  print("Valor é menor que 500")
  x4 = x3 + 100
  #print(str_glue("O valor de x é: {x3}"))
} else{
  print("Valor é maior ou igual a 500")
  x4 = x3 - 100
  stop("Valor é maior. Pare aqui")
  #print(str_glue("O valor de x é: {x3}"))
}

print("O valor de x é :", x3)

str_c(c("O valor de x é: ", x3), collapse = "")
str_glue("O valor de x é: {x3}")

x3 <- c(100, 600, 800)
x3 < 500
ifelse(x3 < 500, "Menor", "Maior ou igual")
ifelse(x3 < 500, x3 / 3, x3 * 9)


# stringr


# sintaxe do for lopp

for(i in c(1, 3, 5, 7)){
  print(i)
  print(i * 10)
  print("Olá")
  print('--------------------')
}




















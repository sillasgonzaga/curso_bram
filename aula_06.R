library(stringr)

a <- 'texto 1'
b <- 'texto 2'
c <- 'texto 3'
a
b
c

d <-  paste(a, b, c, sep = "@")
?paste
d

nomes <- c("Fulano", "Ciclano", "Beltrano")
sobrenomes <- c("Silva", "Oliveira", "Moreira")

paste(nomes, sobrenomes, sep = "_")


cnae.texto <- c('10 Fabricação de produtos alimentícios',
                '11 Fabricação de bebidas',
                '12 Fabricação de produtos do fumo',
                '13 Fabricação de produtos têxteis',
                '14 Confecção de artigos do vestuário e acessórios',
                '15 Preparação de couros e fabricação de artefatos de couro, artigos para viagem e calçados',
                '16 Fabricação de produtos de madeira',
                '17 Fabricação de celulose, papel e produtos de papel')
# extraindo os codigos
str_sub(cnae.texto, start = 1, end = 2)
# extraindo as descrições
str_sub(cnae.texto, start = 4, -1)
?str_sub

telefone <- "11 3232 6055"
str_sub(telefone, -4)

telefones <- c('9931-9572', '11-8591-5772', '8562-1923')

str_replace(telefones, "-", " ")
str_replace_all(telefones, "-", " ")

cnpj <- c('19.702.231/9999-98', '19.498.482/9999-05', '19.499.583/9999-50', '19.500.999/9999-46', '19.501.139/9999-90')

telefones
str_replace_all(telefones, "-", "")
str_remove_all(telefones, "-")

str_remove_all(cnpj, "-")

str_remove_all(cnpj, "\\.")
str_remove_all(cnpj, "/")


?str_remove_all

cnpj
parse_number(cnpj)


nome <- c("Fulano", "Ciclano")

"Oi eu sou o Fulano, prazer"

paste("Oi eu sou o ", nome, ", prazer", sep = "")

str_glue("Oi eu sou o {nome}, prazer")










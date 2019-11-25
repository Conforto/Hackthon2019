#Autor: Lucas Jomes Conforto
#Data de criação:24/11/2019
#Descrição:Algoritimo desenvolvido com o intuito de receber uma frase, e encontrar todos os anagramas
#possíveis com as palavras nela contida, comparando com a lista dada pela positivo.
#Essa aplicação foi feita para a segunda etapa do Hackathon de carreiras - edição TI


#Instalação de pacotes necessários para a aplicação

install.packages('stringr')
install.packages("tibble")
install.packages('readr')
install.packages('tidyverse')
install.packages('mafrittr')
install.packages('dplyr')
install.packages('wfindr')


#Carregamento de libraries 

library(tidyverse)
library(magrittr)
library(stringr)
library(tibble)
library(readr)
library(dplyr)
library(wfindr)

#Checagem de work directory
getwd()


#lendo as palavras do arquivo palavras.txt e guardando em stopwords_in

stopwords_in <- read_delim("palavras.txt",
                           ";",escape_double = FALSE, trim_ws = TRUE, 
                           col_types = cols(
                             A = col_character()
                           ))
#Como o essa leitura utiliza a primeira palavra como nome de coluna, aqui eu adiciono novamente a letra A
stopwords_in <- add_row(stopwords_in, A = "A")

#transformando a lista de palavras em vetor

vetorpalavras<-unlist(stopwords_in)


#recebendo a palavra ou frase

frase <- readline(prompt="Digite a frase desejada: ")
                   
                   
                   
# uppercase
frase <- toupper(frase)
#função para remover os acentos
fa <- function(x) iconv(x, to = "ASCII//TRANSLIT")
frase<-fa(c(frase))
#retirar caracteres especiais
frase<-str_replace_all(frase, "[[:punct:]]", "")

#dividir a frase como lista, e depois trasformar em um vetor
                   
listafrase<-strsplit(frase, " ")
                   
vetorfrase <- unlist(listafrase)
                   
#contando quantas letras há no vetor
                   
i<-length(vetorfrase)
                   
while(i>0){
 varr<-find_word(allow = vetorfrase[i] ,type = "anagram",words = vetorpalavras)
 attr(varr, "names") <- NULL
 cat("anagrama ",vetorfrase[i],"\n" )
 print(varr)
 i<-i-1
}
                   
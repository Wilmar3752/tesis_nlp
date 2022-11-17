rm(list = ls()) #Clear workspace
gc() #Free unused R memory
cat("\014") #Clear console

#Librerias
library('pdftools') #Manipulacion de PDFs
library('dplyr') #Manipulacion de datos y pipes
library('stringr') #Manipulacion de strings
library('ggplot2') #Herramientas graficas
library('stopwords') #Manejo de stopwords
library('SnowballC')
library('tidytext') #Tokenizacion
library('topicmodels') #LDA
library("pdfsearch")

txt<-pdf_text('ejemplo.pdf') 

prueba1<-txt[12]#Conociendo el numero de pagina
prueba11<-txt[13]

txt<-unlist(txt)
txt<-tolower(txt) #texto a minusculas

#txt<-tolower(stri_trans_general(txt,"Latin-ASCII")) #minusculas sin acentos


res<-data.frame(str_detect(txt,"introduccio"))
colnames(res)<-"Result"
res<-subset(res,res$Result==TRUE)
pagina<-as.integer(row.names(res)) #sin conocer el numero de pagina
paginai<-pagina[2]+1


prueba2<-txt[pagina[2]]
prueba2

prueba2<-prueba2 %>% str_split("\n") %>% 
  str_squish() %>%  strsplit(split= "\\,\\s\\\"")


prueba2[[1]]
prueba2df<-as_tibble(prueba2[[1]])
head(prueba2df)

texto<-prueba2[[1]]
texto

head(result)

result <- heading_search(txt, headings = 'Introducción')

result
result$line_text[2]

 result$lre
 
rm(list = ls()) #Clear workspace
gc() #Free unused R memory
cat("\014") #Clear console

#Librerias
suppressMessages(suppressWarnings(library('pdftools')))    #Manipulacion de PDFs
suppressMessages(suppressWarnings(library('dplyr')))       #Manipulacion de datos y pipes
suppressMessages(suppressWarnings(library('stringr')))     #Manipulacion de strings
suppressMessages(suppressWarnings(library('ggplot2')))     #Herramientas graficas
suppressMessages(suppressWarnings(library('stopwords')))   #Manejo de stopwords
suppressMessages(suppressWarnings(library('tidytext')))    #Tokenizacion
suppressMessages(suppressWarnings(library('SnowballC')))   #Diccionario stopwords español
suppressMessages(suppressWarnings(library('topicmodels'))) #LDA
suppressMessages(suppressWarnings(library('stringi')))     #quitar acentos
suppressMessages(suppressWarnings(library("tm")))          #para minería de texto
suppressMessages(suppressWarnings(library("wordcloud2")))  #nube de palabras


##Lista de documentos

list_tesis = list.files("./../data/")
txt <- c()
for(tesis in list_tesis){
  txt[[tesis]]<-pdf_text(sprintf("./../data/%s",tesis))
  txt[[tesis]]<-paste(txt,collapse = '') #todo el texto en una linea 
}


for (i in 1:length(txt) ) {
  txt[i]<-gsub(pattern ="[0-9]+",'',txt[i])#se eliminan numeros
  txt[i]<-tolower(stri_trans_general(txt[i],"Latin-ASCII")) #minusculas sin acentos
}

#####  Practica con un trabajo #####

txt<-pdf_text("./../data/ejemplo.pdf")
txt<-gsub(pattern ="[0-9]+",'',txt)#se eliminan numeros
txt<-tolower(stri_trans_general(txt,"Latin-ASCII")) #minusculas sin acentos
txt<-paste(txt,collapse = '') #todo el texto en una linea 
txt<-as_tibble(txt) #se cambia el tipo de objeto para manipulacion

# Tokenizacion
tokens <-txt %>% unnest_tokens(word,value)

#Stop words
stop_words_es <- stopwords::stopwords("es")#español

stop_words_en <-  stopwords::stopwords("en") #ingles

#Se agregan las letras del abecedario, ea y espacio en blanco"
stop_words_es <- append(stop_words_es,c(letters, "ea",'',"µ","et","al","θ","π","λ","σ","τ","distribucion")) #stopwords español
stop_words_en <- append(stop_words_en,c("abstract")) #stopwords ingles

#Eliminacion de stopwords en español
dflimpio <- tokens %>% filter(!(word %in% stop_words_es))

#Eliminacion de stopwords en ingles
dflimpio1 <- dflimpio %>% filter(!(word %in% stop_words_en))

dflimpio1

#lematizacion
dffinal <- dflimpio1 %>% mutate(word=SnowballC::wordStem(word,language = 'es'))
length(unique(dffinal$word))


corpuss <- Corpus(VectorSource(dflimpio1))


#descriptivas

mtd <- TermDocumentMatrix(corpuss)
m <- as.matrix(mtd)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


wordcloud2(data = d, size = 0.5, shape = "cloud",
           color="random-dark", ellipticity = 0.5)
 

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
  txt[[tesis]]<-gsub(pattern ="[0-9]+",'',txt[[tesis]])#se eliminan numeros
  txt[[tesis]]<-tolower(stri_trans_general(txt[[tesis]],"Latin-ASCII")) #minusculas sin acentos
}


x<-txt #copia de la lista

txt<-x #restaurar lista

# Tokenizacion

tokens<-c()
for (tesis in list_tesis) {
  tokens[[tesis]] <-as_tibble(txt[[tesis]]) %>% unnest_tokens(word,value)
  txt[[tesis]]<-gsub(pattern ="[0-9]+",'',txt[[tesis]])#se eliminan numeros
}
#Stop words
stop_words_es <- stopwords::stopwords("es")#español

stop_words_en <-  stopwords::stopwords("en") #ingles

#Se agregan las letras del abecedario, ea y espacio en blanco"
stop_words_es <- append(stop_words_es,c(letters, "ea",'',"µ","et","al","θ","π","λ","σ","τ","distribucion")) #stopwords español
stop_words_en <- append(stop_words_en,c("abstract")) #stopwords ingles

#Eliminacion de stopwords en español e ingles
dflimpio<-c()
dflimpio1<-c()
for (tesis in list_tesis) {
  dflimpio[[tesis]] <- tokens[[tesis]] %>% filter(!(word %in% stop_words_es))
  dflimpio1[[tesis]] <- dflimpio[[tesis]] %>% filter(!(word %in% stop_words_en))
}

#lematizacion
dffinal<-c()
for (tesis in list_tesis) {
dffinal[[tesis]] <- dflimpio1[[tesis]] %>% mutate(word=SnowballC::wordStem(word,language = 'es'))
}


corpuss <- Corpus(VectorSource(dffinal))

#Modelo LDA (pendiente de revisar mejor)

DTM <- DocumentTermMatrix(corpuss)
ap_lda <- LDA(x = DTM, k = 2, control = list(seed = 1234))
ap_topics <- tidy(x = ap_lda, matrix = "beta")#topicos

#descriptivas

mtd <- TermDocumentMatrix(corpuss)
m <- as.matrix(mtd)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


wordcloud2(data = d, size = 0.5, shape = "cloud",
           color="random-dark", ellipticity = 0.5)
 

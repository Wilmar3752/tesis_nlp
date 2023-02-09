
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
suppressMessages(suppressWarnings(library("wordcloud")))   #nube de palabras
suppressMessages(suppressWarnings(library("reshape2")))    #para lda


#Stop words
stop_words_es <- stopwords::stopwords("es")#español
stop_words_en <-  stopwords::stopwords("en") #ingles

#Se agregan las palabras y simbolos a la lista de stopwords
stop_words_es <- append(stop_words_es,c(letters, "ea","et","al","distribucion", "media","varianza")) #stopwords español
stop_words_en <- append(stop_words_en,c("abstract",'fitted')) #stopwords ingles


#carga de los documentos
start<-Sys.time()
txt <- VCorpus(DirSource("tg prueba"), 
               readerControl = list(reader = readPDF)) #3:40 portatil
end<-Sys.time(); end-start # 6.423168 mins core 2 duo all docs
#2 tg prueba 1.01982 secs


copia<-txt
list_tesis = list.files("tg prueba") #nombres de pdf
start<-Sys.time()
#Procesamiento de los documentos
for(tesis in list_tesis){
  txt[[tesis]][["content"]] <- tolower(stri_trans_general(txt[[tesis]][["content"]],"Latin-ASCII")) #Minusculas sin acentos
  txt[[tesis]][["content"]] <- stri_replace_all_regex(txt[[tesis]][["content"]],pattern=c('[0-9]+','\\n','τ','∇','𝝯','𝜵','∑','σ','µ','π','𝟲','𝟮','∗','ф','𝞥','θ','𝞴','λ','�','𝞤','𝞠','√','𝝰','𝜥','𝜡','𝜠','𝕲','𝕰','𝕘','𝔩','𝓮','𝒮','●'),replacement=c(' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '),vectorize=FALSE)
  txt[[tesis]][["content"]] <- removePunctuation(txt[[tesis]][["content"]]) #Puntuacion 
  txt[[tesis]][["content"]] <- removeWords(txt[[tesis]][["content"]], words = stop_words_es) #Stopwords
  txt[[tesis]][["content"]] <- removeWords(txt[[tesis]][["content"]], words = stop_words_en) #Stopwords
  txt[[tesis]][["content"]] <- stripWhitespace(txt[[tesis]][["content"]]) #Espacios en blanco a uno solo
  txt[[tesis]][["content"]] <- Boost_tokenizer(txt[[tesis]][["content"]]) #Tokenizacion
  txt[[tesis]][["content"]] <- wordStem(txt[[tesis]][["content"]],language = 'es') #lematiza
} #3:08 portatil alldocs
end<-Sys.time(); end-start
#1.114548 secs 2tgprueba



#Modelo LDA 
set.seed(123)
DTM <- DocumentTermMatrix(txt)
ap_lda <- LDA(x = DTM, k = 4, control = list(seed = 1234))
ap_topics <- tidy(x = ap_lda, matrix = "beta") #topicos
#4:40

#12 mins total

View(ap_topics)



#nube de palabras
x <- tm_map(txt, PlainTextDocument)
x11()
wordcloud(x, max.words = 80, random.order = F, colors = brewer.pal(name = "Dark2", n = 8),use.r.layout=FALSE)

 

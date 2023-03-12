
rm(list = ls()) #Clear workspace
gc() #Free unused R memory
cat("\014") #Clear console


#Librerias
suppressMessages(suppressWarnings(library(pdftools)))    #Manipulacion de PDFs
suppressMessages(suppressWarnings(library(dplyr)))       #Manipulacion de datos y pipes
suppressMessages(suppressWarnings(library(stringr)))     #Manipulacion de strings
suppressMessages(suppressWarnings(library(ggplot2)))     #Herramientas graficas
suppressMessages(suppressWarnings(library(stopwords)))   #Manejo de stopwords
suppressMessages(suppressWarnings(library(tidytext)))    #Tokenizacion
suppressMessages(suppressWarnings(library(SnowballC)))   #Diccionario stopwords español
suppressMessages(suppressWarnings(library(topicmodels))) #LDA
suppressMessages(suppressWarnings(library(stringi)))     #quitar acentos
suppressMessages(suppressWarnings(library(tm)))          #para minería de texto
suppressMessages(suppressWarnings(library(wordcloud)))   #nube de palabras
suppressMessages(suppressWarnings(library(reshape2)))    #para lda
suppressMessages(suppressWarnings(library(udpipe)))      #Lematizador


#Stop words
stop_words_es <- stopwords::stopwords("es")#español
stop_words_en <-  stopwords::stopwords("en") #ingles

#Se agregan las palabras a la lista de stopwords
stop_words_es <- append(stop_words_es,c(letters, "ea","et","al","distribucion", "media","varianza","variable","variables","modelo","modelos","tabla","cada","dos","mas",
                                        "dia","estadistico","estadistica","cual","ser","cali","figura","total","fica","variabilidad","distribu","selec","tiga","decir",
                                        "ales","marco","grafico","dias","acuerdo","solo","tam","ves","encon","encuen","ano","tres","cuenta","debido","asi","tambien",
                                        "presenta","edad","colombia","segun","anos","puede","mayor","base","debe","tra","mediante","punto","alto","funcion","presentar",
                                        "ingenieria","cion","valle","bajo","dado","cual","valores","numero","diferentes","ademas","parte","respecto","mejor","resultados",
                                        "tipo","partir","matriz","siguiente","uso","puntos","metodologia","pueden","universidad","tener","mismo","trabajo","igual","normal",
                                        "realizar","dentro","medio","menor","forma","cuales","estan","traves","grupos","grupo","estudiante","individuo","estudio","curva",
                                        "hora","nivel","ver","mes","posible","teorico","existir","vez","presntar","usar","hora","promedio","hacer","tal","indecir","tomar",
                                        "usar","zona","considerar","segundo","manera","conjunto","representar","supuesto","angie","alexis","dar","porcentaje","metodo",
                                        "primero","permitir","minimo","entonces","fin","inter","gran","basado","cero","cuatro","cinco","siete","ocho","nueve","diez","nino",
                                        "aquel","buen","raz","indiz","sero","dim","patr","datar","var","iii", "introduccion","resultado","conclusion","bibliografia","lista",
                                        "graficar","bay","agradecer","agradecimiento","item","vol","bio","ene","enero","febrero","marzo","abril","mayo","junio","julio",
                                        "agosto","septiembre","octubre","noviembre","diciembre","lunes","martes","miercoles","jueves","viernes","sabado","domingo","fecha")) #stopwords español
stop_words_en <- append(stop_words_en,c("abstract",'fitted')) #stopwords ingles


#carga de los documentos   #1.484694 mins nPC
txt <- VCorpus(DirSource("Base sin separar por años"),readerControl = list(reader = readPDF)) 
list_tesis = list.files("Base sin separar por años") #nombres de pdf
save(txt,list_tesis, file = "copiaAllDocs.RData")# se guarda un archivo que contiene los TG cargados y los nombres
load("copiaAllDocs.RData")#cargar el archivo de TG y lista de nombres



txtL<-as.list(txt)# Lista de documentos para aplicar la funcion de limpieza


#Funcion limpieza
funcion<- function(x){
  f<-tolower(stri_trans_general(x,"Latin-ASCII")) #minusculas y sin acentos
  f<-stri_replace_all_regex(f,pattern=c('[0-9]+','\\n','τ','∇','𝝯','𝜵','∑','σ','µ','π','𝟲','𝟮','∗','ф','𝞥','θ','𝞴','λ','�','𝞤','𝞠','√','𝝰','𝜥','𝜡','𝜠','𝕲','𝕰','𝕘','𝔩','𝓮','𝒮','●','linea'),replacement=c(' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ','lineal'),vectorize=FALSE)
  f<-removePunctuation(f)
  f<-removeWords(f, words = stop_words_es)#stop words español
  f<-removeWords(f, words = stop_words_en)#stop words ingles
  f<-stripWhitespace(f) #reduce los espacios en blanco
  f<-Boost_tokenizer(f) #Tokenizar
}

#Funcion limpieza aplicada
h<-lapply(txtL,FUN=funcion) #54.62 secs casi un 25% mas rapido que un for
save(h, file = "copiaLimpieza.RData")
load("copiaLimpieza.RData")#cargar el archivo de TG con limpieza



#Lematizacion
L<-lapply(h,FUN=function(x) udpipe(x,object = "spanish-gsd")) #lematizar 14.40628 mins



#retomar lista de documentos
for(tesis in list_tesis){
h[[tesis]]<-L[[tesis]][["lemma"]]
}
save(h,txt,list_tesis,funcion,stop_words_es,stop_words_en, file = "copiaLematizacion.RData")
load("copiaLematizacion.RData")#cargar el archivo de TG con limpieza y lematizacion


#realizar de nuevo el proceso de eliminacion de stopwords/limpieza luego de lematizar
h<-lapply(h,FUN=funcion) #funcion limpieza

#si se quieren eliminar mas palabras, sino, pasar a definir la DTM y correr LDA
#stop_words_es <- append(stop_words_es,c(""))#agregar palabras a stopwords y sino, no ejecutar
#h<-lapply(h,FUN=funcion)

#retomar corpus, es para que las palabras aparezcan sin /
for(tesis in list_tesis){
  txt[[tesis]][["content"]]<-h[[tesis]]
}


#se guardan los archivos necesarios para correr LDA
save(txt,list_tesis,funcion,stop_words_es,stop_words_en, file = "copiaArchivosParaLDA.RData")






#######carga de archivos para LDA
load("copiaArchivosParaLDA.RData")




#Se define la DTM y se hace limpieza de terminos no frecuentes
DTM <- TermDocumentMatrix(txt) 
DTM <- removeSparseTerms(DTM, 0.98) #eliminar palabras/terms que no aparezcan en al menos el 98% de los documentos


#consultar una palabra en la dtm para ver si se elimina
df_tdm<-data.frame(word=names(m),freq=sort(rowSums(as.matrix(tdm)),decreasing = TRUE))
df_tdm[df_tdm[1]=="no"] #consulta


### Modelo LDA ejecutar
start<-Sys.time()
set.seed(123)
ap_lda <- LDA(x = DTM, k = 4, method = "VEM",control = list(seed = 1234))
ap_topics <- tidy(x = ap_lda, matrix = "beta") #topicos
end<-Sys.time(); end-start #1.274259 mins nPC


#nube de palabras
x <- tm_map(txt, PlainTextDocument)
x11()
wordcloud(x, max.words = 100, random.order = F, colors = brewer.pal(name = "Dark2", n = 8),use.r.layout=FALSE)



#Representancion grafica del modelo LDA
#fterm es igual a los 10 topicos de mayor valor beta de cada termino
fterm <- ap_topics %>% 
  group_by(topic) %>%
  slice_max(beta,n=10,with_ties=F)%>%
  ungroup() %>%
  arrange (topic,-beta)

#grafico de topicos
fterm %>%
  mutate(term = reorder_within(term,beta,topic)) %>%
  group_by(topic,term) %>%
  arrange(desc(beta)) %>%
  ungroup %>%
  ggplot(aes(beta,term,fill=as.factor(topic)))+
  geom_col(show.legend = F)+
  scale_y_reordered()+
  labs(title='Top 10 terminos en cada topico de LDA',
       x=expression(beta), y=NULL)+
  facet_wrap(~topic,ncol=3,scales = 'free')

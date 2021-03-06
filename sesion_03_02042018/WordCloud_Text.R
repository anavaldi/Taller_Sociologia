####################################################
#
#        # WORD CLOUD TEXT
#
# Author: Ana Valdivia
# Date: 4th October 2017
#
################

# Libraries
install.packages("readr")
install.packages("rvest")
install.packages("beepr")
install.packages("stringr")
install.packages("RXKCD")
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("data.table")
install.packages("wordcloud2") 

library(readr)
library(rvest)
library(beepr)
library(stringr)
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(data.table)
library(wordcloud2) 


discursoDelRey <- c("Buenas noches, Estamos viviendo momentos muy graves para nuestra 
                    vida democrática. Y en estas circunstancias, quiero dirigirme directamente 
                    a todos los españoles. Todos hemos sido testigos de los hechos que se han ido 
                    produciendo en Cataluña, con la pretensión final de la Generalitat de que sea 
                    proclamada -ilegalmente-la independencia de Cataluña. Desde hace ya tiempo, 
                    determinadas autoridades de Cataluña, de una manera reiterada, consciente y 
                    deliberada, han venido incumpliendo la Constitución y su Estatuto de Autonomía,
                    que es la Ley que reconoce, protege y ampara sus instituciones históricas y su 
                    autogobierno. Con sus decisiones han vulnerado de manera sistemática las normas
                    aprobadas legal y legítimamente, demostrando una deslealtad inadmisible hacia
                    los poderes del Estado. Un Estado al que, precisamente, esas autoridades 
                    representan en Cataluña. Han quebrantado los principios democráticos de todo 
                    Estado de Derecho y han socavado la armonía y la convivencia en la propia 
                    sociedad catalana, llegando -desgraciadamente- a dividirla. Hoy la sociedad 
                    catalana está fracturada y enfrentada. Esas autoridades han menospreciado los 
                    afectos y los sentimientos de solidaridad que han unido y unirán al conjunto 
                    de los españoles; y con su conducta irresponsable incluso pueden poner en 
                    riesgo la estabilidad económica y social de Cataluña y de toda España. En 
                    definitiva, todo ello ha supuesto la culminación de un inaceptable intento
                    de apropiación de las instituciones históricas de Cataluña. Esas autoridades, 
                    de una manera clara y rotunda, se han situado totalmente al margen del derecho
                    y de la democracia. Han pretendido quebrar la unidad de España y la soberanía 
                    nacional, que es el derecho de todos los españoles a decidir democráticamente 
                    su vida en común. Por todo ello y ante esta situación de extrema gravedad, 
                    que requiere el firme compromiso de todos con los intereses generales, 
                    es responsabilidad de los legítimos poderes del Estado asegurar el orden 
                    constitucional y el normal funcionamiento de las instituciones, 
                    la vigencia del Estado de Derecho y el autogobierno de Cataluña, 
                    basado en la Constitución y en su Estatuto de Autonomía. Hoy quiero, además, 
                    transmitir varios mensajes a todos los españoles, particularmente a los 
                    catalanes. A los ciudadanos de Cataluña -a todos- quiero reiterarles que desde
                    hace décadas vivimos en un Estado democrático que ofrece las vías 
                    constitucionales para que cualquier persona pueda defender sus ideas dentro 
                    del respeto a la ley. Porque, como todos sabemos, sin ese respeto no hay 
                    convivencia democrática posible en paz y libertad, ni en Cataluña, 
                    ni en el resto de España, ni en ningún lugar del mundo. En la España 
                    constitucional y democrática, saben bien que tienen un espacio de concordia 
                    y de encuentro con todos sus conciudadanos. Sé muy bien que en Cataluña 
                    también hay mucha preocupación y gran inquietud con la conducta de las 
                    autoridades autonómicas. A quienes así lo sienten, les digo que no están solos,
                    ni lo estarán; que tienen todo el apoyo y la solidaridad del resto de los 
                    españoles, y la garantía absoluta de nuestro Estado de Derecho en la defensa 
                    de su libertad y de sus derechos. Y al conjunto de los españoles, que viven 
                    con desasosiego y tristeza estos acontecimientos, les transmito un mensaje de 
                    tranquilidad, de confianza y, también, de esperanza. Son momentos difíciles, 
                    pero los superaremos. Son momentos muy complejos, pero saldremos adelante. 
                    Porque creemos en nuestro país y nos sentimos orgullosos de lo que somos. 
                    Porque nuestros principios democráticos son fuertes, son sólidos. Y lo son 
                    porque están basados en el deseo de millones y millones de españoles de 
                    convivir en paz y en libertad. Así hemos ido construyendo la España de las 
                    últimas décadas. Y así debemos seguir ese camino, con serenidad y con 
                    determinación. En ese camino, en esa España mejor que todos deseamos, 
                    estará también Cataluña. Termino ya estas palabras, dirigidas a todo el 
                    pueblo español, para subrayar una vez más el firme compromiso de la Corona 
                    con la Constitución y con la democracia, mi entrega al entendimiento y 
                    la concordia entre españoles, y mi compromiso como Rey con la unidad y 
                    la permanencia de España.")

# Clean text function
clean.text = function(x)
{
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  # set Spanish symbols
  x = gsub("ã¡", "a", x)
  x = gsub("ã©", "e", x)
  x = gsub("ã³", "o", x)
  x = gsub("ãº", "u", x)
  x = gsub("ã±", "ñ", x)
  x = gsub("ã¨", "e", x)
  x = gsub("ã²", "o", x)
  x = gsub("ã", "i", x)
  return(x)
}

# Text mining
discursoDelRey <- clean.text(discursoDelRey)


# Build Corpus
corpus <- Corpus(VectorSource(discursoDelRey))
d  <- tm_map(corpus, tolower)
d  <- tm_map(d, stripWhitespace)
d <- tm_map(d, removePunctuation)
# d <- tm_map(d, removeWords,  stopwords("spanish"))
tdm <- TermDocumentMatrix(d)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
df <- data.frame(word = names(v),freq=v)
rownames(df) <- c(1:nrow(df))
# Delete some words
df <- df[-c(1:3, 5, 8, 11, 15, 23, 24),]


# Word Clouds
wordcloud2(df, color=c("orange","red", "purple"))

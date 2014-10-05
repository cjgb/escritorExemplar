##########################################################
# escritor exemplar
# cjgb, 2013-05-10
##########################################################

options(stringsAsFactors = F)

library(plyr)
library(data.table)

### funciones auxiliares

leer.novela <- function(file.name){
  txt <- readLines(file.name)
  txt <- tolower(txt)
  txt <- paste(txt, collapse = " ")
  #txt <- gsub('»|,|—|-|–|¿|¡|\\*|«|‘|’|\\"', "", txt)
  txt <- gsub('»|—|-|–|¿|¡|\\*|«|‘|’|\\"', "", txt)
  txt <- gsub("\\?|;|\\:|!|\\(|\\)|\\[|\\]", ".", txt)
  
  txt <- gsub("  *", " ", txt)
  
  frases <- strsplit(txt, "\\.")[[1]]
  frases <- gsub("^\\s+|\\s+$", "", frases)
}

get.ngrams <- function(frases, n = 2){
  
  foo <- function(frase, n){
    if(length(frase) < n)
      return(NULL)
    
    do.call(cbind, sapply(1:n, function(x) frase[x:(length(frase) - n +x)], simplify = F) )
    
  }
  
  res <- ldply( strsplit(frases, " "), foo, n)
}

### ngramas

frases.novela <- leer.novela("txt/novelas_ejemplares.txt")
n.gramas.2 <- data.table(get.ngrams(frases.novela, 2), key = c("1"))
n.gramas.3 <- data.table(get.ngrams(frases.novela, 3), key = c("1", "2"))

### escritor exemplar

escritor.exemplar <- function(palabra, longitud = 10){
  
  # buscamos la palabra previa
  # frase <- c("*", palabra)
  tmp <- n.gramas.2$"1"[n.gramas.2$"2" == palabra]
  if(length(tmp) == 0)
    return(null)
  
  frase <- c(sample(tmp, 1), palabra)
  
  for(i in 2:longitud){
  
    # buscamos la palabra siguiente buscando en la base de datos de trigramas
    # palabras (la tercera) precedidas de las dos últimas de nuestra frase
    
    tmp <- n.gramas.3[ as.list(tail(frase,2) ) ] [["3"]]
    tmp <- tmp[!is.na(tmp)]
    
    # si encontramos el trigrama, pegamos una palabra al azar
    # y volvemos al principio
    
    if(length(tmp) > 0){                      
      frase <- c(frase, sample(tmp, 1))
      next
    }
    
    # si no hemos encontrado un candidato entre los trigramas, 
    # buscamos entre los bigramas
    
    tmp <- n.gramas.2[J(tail(frase,1))][["2"]]
    
    # si no encontramos nada entre los bigramas, ¡nos rendimos!
    
    if(length(tmp) == 0)
      break
    
    frase <- c(frase, sample(tmp, 1))
  }
  
  #paste("...", paste(frase[-1], collapse = " "), "...", collapse = " ")
  paste("...", paste(frase, collapse = " "), "...", collapse = " ")
  
}

palabra <- "caballo"

escritor.exemplar("caballo")
escritor.exemplar("queso")
escritor.exemplar("mujer")

escritor.exemplar("gitanilla")



#tmp <- subset(n.gramas.2)

write.table(n.gramas.2, file = "2gramas.csv", col.names = F, row.names = F, sep = "\t")
write.table(n.gramas.3, file = "3gramas.csv", col.names = F, row.names = F, sep = "\t")
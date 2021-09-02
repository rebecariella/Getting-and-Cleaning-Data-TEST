# SEMANA 4------------------


fruits <- c("one apple", "two pears", "three bananas")
fruits
str_replace(fruits, "[aeiou]", "-")

str_replace_all(fruits, "[aeiou]", "-")
sub(" ","-",fruits)
sub(" ","",fruits)
fruits %>% sub(" ", "~", .)

a = c("100,5","200,5","100,5")
a
sub(",", "~", a)
as.numeric(sub(",", ".", a))*2

sub("0", "~", a)
gsub("0", "~", a)



tolower(names(base)) #pasa a minuscula los nombres de las variables
strsplit(names(base),"\\.") #divide los nombres de las variables en dos nombres distintos, segùn el punto en este caso
      # ejemplo: "Location.1" se transforma en "Location" "1"
sub("_", "", names(base)) #saca los guiones bajos de los nombres de las variables (si aparecen una vez)
gsub("_", "", names(base)) #saca los guiones bajos de los nombres de las variables (todos)
grep("palabra buscada", variable) #busca una palabra/caracteres a lo largo de todas las entradas de una variable. 
      # devuelve la posiciòn de las observaciones que cumplen la condiciòn
grep("palabra buscada", variable, value = T) #muestra la variable no  la posiciòn
table(grepl("palabra buscada", variable)) #muestra cuántos son los casos de arriba
nchar() #cuenta número de caracteres
substr(1,7) #se queda con los primeros 7 caracteres
paste()
paste0() #pega separando con nada
paste("Hola","Rebeca")
paste("Hola","Rebeca",sep="#")
paste0("Hola","Rebeca")
str_trim("Sol    ") #Saca espacios del final y del principio
str_trim("    Sol    ")
str_trim("    S   ol    ") #pero no del medio


#Expresiones regulares ----
#Buscar en textos cosas más generales, por ejemplo, buscar oraciones que
^i think #Empiecen con "i think"
morning$#Terminan con morning
[Bb][Uu][Sh][Hh] #Busca la palabra BUSH escrita con cualquier min o may en cualquier combinación
^[Ii] am #que empiece con "I am" o con "i am"
^[0-9][a-zA-Z] # que empiece con un número y una letra, ej "1ro" "2DO" "3am"
[^?.]$ #que no termine en signo de interrog ni en punto
  
9.11 #el punto indica que puede haber cualquier character. "9-11", "9:11", "9/11", "911"
flood|fire #busca todas las entradas que tienen alguna de los dos problemas, innunda o incendi
[Gg]ood|[Bb]ad #busca todas las entradas que tienen "good" o "bad" empezando o no con mayus
^([Gg]ood|[Bb]ad ) #idem pero solo empezando 
[Gg]eorge( [Ww]\.)? [Bb]ush #con o sin el W. (el punto si o si, la \ avisa que el punto no es un metacaracter en este caso )
(.*) #cualquier elemento que tenga parentesis
[0-9]+(.*)[0-9]+ #uno o varios numeros, characteres, luego uno o varios numeros
[Bb]ush ( +[^ ]+ +){1,5} debate #entre corchetes numero min y max de repetición de una expresión:
  #entre bush y debate hay al menos un espacio y al menos algo que no es un espacio
^s (.*)s$ #busca algo que empiece por s, tenga varios caracteres y espacios, y luego otra s
  

  
  
#DATES -------------------------
d1 = date()
d1  
class(d1)    

d2 = Sys.Date()
d2
class(d2)

format(d2,"%a %b %d")

vector = c("11mar1990","12mar1980")
class(vector)  
z = as.Date(vector,"%d%b%Y")
z


month(d2)
weekdays(d2)
julian(d2)


library(lubridate)
ymd("20140108")
mdy("08/04/2020")
dmy("02/04/1990")
dmy("02-04-1990")

ymd_hms("2011-09-11 10:15:20")
ymd_hms("2011-09-11 10:15:20", tz="Pacific/Auckland")

?Sys.timezone


vector = c("11mar1990","12mar1980")
dmy(vector)
dmy(vector[1])
wday(dmy(vector)[1])
wday(dmy(vector)[1],label=TRUE)



# QUIZ ------------------





#1 -----
# The American Community Survey distributes downloadable data about United States communities. 
# Download the 2006 microdata survey about housing for the state of Idaho using download.file() from here: 
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
# 
# and load the data into R. The code book, describing the variable names is here:
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf 
# 
# Apply strsplit() to split all the names of the data frame on the characters "wgtp". 
# What is the value of the 123 element of the resulting list?

setwd("C:/Users/rriella/Desktop/Curso Coursera Data Science/3. Getting and Cleaning data")

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv", "quiz4.1.csv") 
base <-read.csv("quiz4.1.csv", colClasses = "character") 
splitnames = strsplit(names(base),"\\wgtp")
splitnames[[123]]
names(base)
class(base)

#2 -----
GDP <- read_csv("getdata_data_GDP.csv",col_names = F)
GDP = GDP %>% mutate(X2 = as.numeric(X2)) %>% filter(is.na(X2)==F)
dim(GDP)

GDP$X5_ = gsub(",", "", GDP$X5)
mean(as.numeric(GDP$X5_),na.rm=T)

#3 ---------
grep("^United",GDP$X4)
grep("^United",GDP$X4,value=TRUE)


#4 ---------
EDSTATS <- read_csv("getdata_data_EDSTATS_Country.csv") %>% mutate(X1=CountryCode)
JOIN = inner_join(EDSTATS,GDP,by="X1")  
grep("Fiscal",names(JOIN))
JOIN$`Special Notes`
grep("[Jj]une",JOIN$`Special Notes`)
grep("[Jj]une",JOIN$`Special Notes`,value=TRUE) 

grep("end+(.*)[Jj]une",JOIN$`Special Notes`)
grep("end+(.*)[Jj]une",JOIN$`Special Notes`) %>% length()
grep("end+(.*)[Jj]une",JOIN$`Special Notes`,value=TRUE)

grep("Fiscal year end+(.*)[Jj]une",JOIN$`Special Notes`)
grep("Fiscal year end+(.*)[Jj]une",JOIN$`Special Notes`) %>% length()
grep("Fiscal year end+(.*)[Jj]une",JOIN$`Special Notes`,value=TRUE)


#5 ---------
library(quantmod)

amzn=getSymbols("AMZN",auto.assign = FALSE)
sampleTimes=index(amzn)

y=(year(sampleTimes))
table(y)
w=(wday(sampleTimes)==2&year(sampleTimes)==2012)
table(w)
Sys.getlocale("LC_TIME")



install.packages("quantmod")
library(quantmod)

a = getSymbols("AMZN",auto.assign = F)


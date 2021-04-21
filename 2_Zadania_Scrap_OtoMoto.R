library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)
library(rvest)
library(xlsx)

# pobieranie maksymalnej liczby stron
url <- "https://www.otomoto.pl/motocykle-i-quady/"
strona<-read_html(url)
liczbaStron <- strona%>%html_node(xpath='/html/body/div[4]/div[2]/section/div[2]/div[2]/ul')%>% html_elements("li")
w <- max(liczbaStron %>% html_text2())

# sterowanie ilosćią stron
maxStron <- min(as.numeric(w),2)

# pobieranie linkóW
wektorLinkow<-c()
for(i in 1:maxStron){
  print(i)
  newUrl<- paste0("https://www.otomoto.pl/motocykle-i-quady/?search%5Border%5D=created_at%3Adesc&page=",i)
  #print(newUrl)
  page<-read_html(newUrl)
  #print(page)
  result<-page%>%html_nodes(xpath='/html/body/div[4]/div[2]/section/div[2]/div[1]/div/div[1]/div[6]/article[*]/div[2]/div[1]/div[1]/h2/a')
  #result<-page%>%html_nodes(xpath='/html/body/div[4]/div[2]/section/div[2]/div[1]/div/div[1]/div[6]')
  #print(result)
  wektorLinkow<-c(wektorLinkow,xml_attr(result,"href"))
}
wektorLinkowU<-wektorLinkow%>%unique()

# funkcja zczytujaca atrybuty
zrobWierszRvest<-function(w,wektorLinkow){
  newUrl<-wektorLinkow[w]
  page<-read_html(newUrl)
  
  cena<-page%>%html_node(xpath='/html/body/div[4]/main/div[2]/div[1]/div[1]/div/div[5]/div[2]/div[2]/div')%>%xml_attr("data-price")
  
  gdzie<- page%>%html_node(xpath='/html/body/div[4]/main/div[2]/div[2]/div[1]/div[2]/div[3]')%>%html_elements(".seller-box__seller-address__label")
  gdzie<-html_text2(gdzie)
  kto<- page%>%html_node(xpath='/html/body/div[4]/main/div[2]/div[2]/div[1]/div[2]/div[1]')%>%html_elements(".seller-box__seller-name")
  kto<-html_text2(kto)
  
  parameters <- page%>%html_node(xpath='/html/body/div[4]/main/div[2]/div[1]/div[2]/div[1]/div[1]/div[3]/div[1]')%>% html_elements("li")
  dlPar <- length(parameters)
  #w <- parameters %>% html_text2()
  cecha <- c("Cena","Kto","Gdzie")
  atrybut <- c(cena,kto,gdzie)
  dlVek <- 0
  for(i in 1:dlPar) {
    p2 <- html_children(parameters[[i]])
    nazwaCechy <- html_text2(p2[[1]])
    atrybutCechy <- html_text2(p2[[2]])
    
    if ( nazwaCechy != "" ) {
      dlVek <- dlVek+1
      cecha <- c(cecha, nazwaCechy)
      atrybut <- c(atrybut, atrybutCechy)
    }
  }
  
  df1 <- data.frame(matrix(atrybut, ncol = length(atrybut), nrow = 1))
  names(df1) <- cecha
  return(df1)
}  

# maly test
motocykle<-NULL 
motocykle<-smartbind(zrobWierszRvest(1,wektorLinkowU),zrobWierszRvest(2,wektorLinkowU))

# pobieranie atrybutów
motocykle<-NULL
liczbaLinkow<-length(wektorLinkowU)
for(w in 1: liczbaLinkow ){
  print(paste0(w," / ",liczbaLinkow ) )
  skip<-FALSE
  tryCatch(
    df1<-zrobWierszRvest(w,wektorLinkowU),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(motocykle)){
    motocykle<-df1
  }else{
    motocykle<-smartbind(motocykle,df1)
  }
}  

# zapis do excela

getwd()
write.xlsx(motocykle,'motocykle_oto_moto.xlsx')

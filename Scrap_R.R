#java -jar selenium-server-standalone-3.0.1.jar -port 4445
#najpierw odpalamy selenium


#install.packages( c("RSelenium","seleniumPipes","dplyr") )
#install.packages("stringr")
#install.packages("gtools")

#install.packages("rvest")
#install.packages("xml2")

library(RSelenium)
library(seleniumPipes)
library(dplyr)
library(stringr)
library(gtools)

library(rvest)
library(xml2)


remDr<- remoteDr(remoteServerAddr = "http://localhost",
                 port=4445,
                 browserName = "chrome",
                 newSession = TRUE)
remDr%>% go("http://otodom.pl")

wektorLinkow<-c()

for(i in 1:2){
  newUrl<-paste0("https://www.otodom.pl/sprzedaz/mieszkanie/?page=",i)
  remDr%>%go(newUrl)
  elems<-remDr%>%findElements("tag name", "h3")
  for(j in 1: length(elems)){
    e<- findElementsFromElement(elems[[j]],using="tag name", "a")
    if( length(e)>0){
      link<- e[[1]]%>%getElementAttribute("href")
      wektorLinkow<-c(wektorLinkow,link)
    }
  }
}

wektorLinkowU<-wektorLinkow%>%unique()
mieszkania<-NULL
for(w in 1: length(wektorLinkowU)){
  skip<-FALSE
  tryCatch(
    df1<-zrobWierszRvest(w,wektorLinkowU,remDr),error=function(e){skip<<-TRUE}
    #df1<-zrobWiersz(w,wektorLinkowU,remDr),error=function(e){skip<<-TRUE}
  )
  if(skip){next}
  if(is.null(mieszkania)){
    mieszkania<-df1
  }else{
    mieszkania<-smartbind(mieszkania,df1)
  }
}

zrobWiersz<-function(w,wektorLinkowU,remDr){
  szczegoly<-NULL
  szczegoly<- remDr%>%go(wektorLinkowU[w])%>%findElements("class name","css-18h1kfv")
  print( wektorLinkowU[w] )
  listaSzczegolyOpis<-c()
  listaSzczegolyWartosci<-c()
  for(i in 1: length(szczegoly)){
    listaSzczegolyOpis<- c(listaSzczegolyOpis,szczegoly[[i]]%>%findElementsFromElement("class name","css-o4i8bk"))
    listaSzczegolyWartosci<-c(listaSzczegolyWartosci,szczegoly[[i]]%>%findElementsFromElement("class name","css-1ytkscc"))
  }
  nazwyKolumn<- unlist( lapply(listaSzczegolyOpis,getElementText)%>% str_replace_all(":","") )
  wartosci<- unlist( lapply(listaSzczegolyWartosci,getElementText) )
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
  df1
  
}

# sama petal nie potrzbne

for(w in 1:length(wektorLinkowU)){
  szczegoly<- remDr%>%go(wektorLinkowU[w])%>%findElements("class name","css-18h1kfv")
  listaSzczegolyOpis<-c()
  listaSzczegolyWartosci<-c()
  for(i in 1: length(szczegoly)){
    listaSzczegolyOpis<- c(listaSzczegolyOpis,szczegoly[[i]]%>%findElementsFromElement("class name","css-o4i8bk"))
    listaSzczegolyWartosci<-c(listaSzczegolyWartosci,szczegoly[[i]]%>%findElementsFromElement("class name","css-1ytkscc"))
  }
  nazwyKolumn<- unlist( lapply(listaSzczegolyOpis,getElementText)%>% str_replace_all(":","") )
  wartosci<- unlist( lapply(listaSzczegolyWartosci,getElementText) )
  df1<- data.frame( matrix(wartosci,nrow=1,ncol=length(wartosci)))
  names(df1)<-nazwyKolumn
}

# Drugi sposób ----------------------------- Druga Funkcja do scrapowania --------

wektorLinkow<-c()
for(i in 1:10){
  newUrl<- paste0("https://www.otodom.pl/sprzedaz/mieszkanie/?page=",i)
  page<-read_html(newUrl)
  result<-page%>%html_nodes(xpath='/html/body/div[3]/main/section[2]/div/div/div[1]/div/article[*]/div[1]/header/h3/a')
  wektorLinkow<-c(wektorLinkow, xml_attr(result,"href"))
}
wektorLinkow

zrobWierszRvest<- function(w,wektorLinkow,remDr){
  
  newUrl<-wektorLinkow[w]
  page<-read_html(newUrl)
  cena<-html_node(page,".css-srd1q3")%>%html_text()
  
  v<-page %>% xml_find_all('/html/body/div[1]/main/div/div[3]/div[1]/*/*')%>%html_attr("title")%>%na.omit()
  indexy<- seq(1,length(v),1)
  nazwyKolumn<- (v[indexy%%2==1])
  wartosci<-(v[indexy%%2==0])
  df1<- data.frame (matrix(wartosci,nrow = 1,ncol=length(wartosci)) )
  names(df1) <- nazwyKolumn
  df1<-cbind(cena,df1)
  df1
}

# copy full Xpath - /html/body/div[1]/main/div/div[3]/div[1]/div[1]/div[2]

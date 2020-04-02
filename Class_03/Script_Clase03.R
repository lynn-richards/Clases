###Class 03 - Data Management & Visualization###
## Author: Esteban Lopez
## Course: Spatial Analytics 
## Program: Master in Business Analytics
## Institution: Universidad Adolfo Ibáñez


#---- Part 1: Data Management  -------------------

# Reading an exporting data

library(readxl)
library(data.table)
casos<-data.table(read_excel("Class_02/2020-03-17-Casos-confirmados.xlsx",na = "—",trim_ws = TRUE,col_names = TRUE),stringsAsFactors = FALSE)

casos<-casos[Región=="Metropolitana",] #

saveRDS(casos,"Class_03/CasosCovid_RM.rds")

write.csv(casos,file = 'Class_03/CasosCovid_RM.csv',fileEncoding = 'UTF-8')

#install.packages("writexl")
library(writexl)
writexl::write_xlsx

library(foreign)

write.dta

casosRM<-fread("Class_03/CasosCovid_RM.csv",header = T, showProgress = T,data.table = T)

casosRM[,table(Sexo)]
casosRM[Sexo=="Fememino",Sexo:="Femenino"]


# Creating (factor) variables, variables que son str pero tienen leyendas


class(casosRM$Sexo)
casosRM[,Sexo:=factor(Sexo)]
casosRM[,Sexo:=factor(Sexo,nmax = 2)]

head(casosRM$Sexo)
head(as.numeric(casosRM$Sexo))
levels(casosRM$Sexo)

table(casosRM$Sexo)
casosRM[,.N,by=.(Sexo)]
casosRM[,.N,by=.(Sexo,`Centro de salud`)]

#Collapsing by Centro de Salud 

casosRM[,sum(`Casos confirmados`,na.rm = T),by=.(`Centro de salud`)][,V1/sum(V1)] #me entrega los porcentajes de los casos confirmados por clinica en la region metropolitana.
obj1<-casosRM[,mean(`Casos confirmados`,na.rm = T),by=.(`Centro de salud`)]
obj1[,V1/sum(V1,na.rm = T)]

# collapsing by average age
casosRM[`Casos confirmados`]

A<-casosRM[,.(AvAge=mean(Edad,na.rm = T)),by=.(`Centro de salud`)]

B<-casosRM[,.(Total_centro=.N),by=.(`Centro de salud`)]

C<-casosRM[Sexo=="Femenino",.(Total_Centro_Mujeres=.N),by=.(`Centro de salud`)]

D<-casosRM[Sexo=="Masculino",.(Total_Centro_Hombres=.N),by=.(`Centro de salud`)]
dim(A)


#merging data sets
AB_fake<-cbind(A,B$Total_centro)
AB<-merge(A,B,by ="Centro de salud",all = T,sort = F)
ABC<-merge(AB,C,by = "Centro de salud",all = T,sort = F)
ABCD<-merge(ABC,D,by = "Centro de salud",all = T,sort = F)
ABCD[is.na(porc_mujeres),porc_mujeres:=0]
ABCD[,porc_mujeres:=Total_Centro_Mujeres/Total_centro]

# reshaping

E<-casosRM[,.(AvAge=mean(Edad,na.rm = T),`Casos confirmados`=sum(`Casos confirmados`,na.rm = T)),by=.(`Centro de salud`,Sexo)]
E[`Centro de salud`== "Clinica Alemana", `Centro de salud`:="Clínica Alemana"]
G<-reshape(E,direction = 'wide',timevar = 'Sexo',v.names = c('AvAge','Casos confirmados'),idvar = 'Centro de salud') #redistribuye el resultado, wide direccion hacia el lado, , average age quiero que ea ancha y los casos confirmados 


#---- Part 2: Visualization  -------------------

#Scatter plot
  #Base R 
plot(G$`Casos confirmados.Femenino`,G$`Casos confirmados.Masculino`)
text(x =G$`Casos confirmados.Femenino`,y=G$`Casos confirmados.Masculino`, G$`Centro de salud`,cex=0.5)

#ggplot2
library(ggplot2)
p1<-ggplot(G,aes(x=`Casos confirmados.Femenino`,y=`Casos confirmados.Masculino`))+geom_point(aes(size=AvAge.Femenino,colour=AvAge.Masculino))+geom_text(aes(label=`Centro de salud`),size=2,check_overlap = T)
p1

#plotly
#install.packages("plotly")
library(plotly)
ggplotly(p1)

# other useful ways to show data

#high charter
# http://jkunst.com/highcharter/index.html


#---- Part 3: Intro to Mapping  -------------------
install.packages("chilemapas")
library(chilemapas)
library(data.table)
library(ggplot2)

View(mapa_zonas)

zonas_censo<-data.table(censo_2017_zonas,stringsAsFactors = F)

poblacion_adulto_mayor_zonas<-zonas_censo[edad=="65 y mas",.(AdultosMayores=sum(poblacion)),by=.(geocodigo)]

zonas_valparaiso<-mapa_zonas[mapa_zonas$codigo_region=="05",]

zonas_valparaiso<-merge(zonas_valparaiso,codigos_territoriales[,c("codigo_comuna","nombre_comuna")],by="codigo_comuna",all.x=TRUE,sort=F)

zonas_valparaiso<-zonas_valparaiso[zonas_valparaiso$codigo_comuna%in%c("05101","05109"),]

zonas_valparaiso<-merge(zonas_valparaiso,poblacion_adulto_mayor_zonas,by="geocodigo",all.x=TRUE,sort=F)


#plotting
library(RColorBrewer)
paleta <- rev(brewer.pal(n = 9,name = "PuRd"))


ggplot(zonas_valparaiso) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 11)

# creating a fake spatial distribution of adult population in space
zonas_valparaiso2<-cbind(zonas_valparaiso[,c("geocodigo","codigo_comuna","codigo_provincia","codigo_region","geometry")],"AdultosMayores"=sample(zonas_valparaiso$AdultosMayores,size = length(zonas_valparaiso$AdultosMayores)))


ggplot(zonas_valparaiso2) + 
  geom_sf(aes(fill = AdultosMayores, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(paleta), name = "Poblacion\nadulto mayor") +
  labs(title = "Poblacion de 65 años y más", subtitle = "Valparaíso y Viña del Mar") +
  theme_minimal(base_size = 13)

#comparing histograms of the same variable

hist(zonas_valparaiso$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo")

hist(zonas_valparaiso2$AdultosMayores,main = "Histograma Adultos Mayores Viña-Valpo")


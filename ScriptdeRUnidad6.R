#Analisis de datos para histograma
x=c(2.5,4,5.5,7,8.5,10)
sample(x,5,T)
y=1:10
for (i in 1:10) {
  z=sample(x,5,T)
  y[i]=mean(z)
}
y
mean(y)




#C?mo dibujar funciones en R
y=function(x){x}
curve(y, -3, 3, xname = "linear fuction")

#Propiedades de la mediana

x=c(167, 168, 139, 144, 163, 163, 199, 206, 207, 232, 242, 224, 253, 186, 163, 158, 160, 165, 166, 159, 183, 181, 175, 190, 207, 187, 189, 176, 166)
mean(x)
x
median(x)
(141*2+163*11+185*8+207*4+229*2+251*2)/(29)
#Para la media ponderada
y=c(824,750,550,275,300)
mean(y)

#Media Aritmetica
x=c(23,24,27,42,42)
mean(x)
y=c(20,25,25,40,48)
mean(y)
qvec=c(125,55,55,55,115,50)
mean(vec)
vec1=vec+20
vec1
mean(vec1)
vec2=1.1*(vec)
vec2
mean(vec2)
1.1*(mean(vec))




#########################################################################
#PARA HACER UN HISTOGRAMA Y UN TABLA DE DISTRIBUCI?N DE FRECUENCIAS. 
x=read.csv("cosumo.csv",TRUE,",")
x
attach(x)
h1<-hist(kw,border=FALSE,xlab="Consumo", ylab="N? de meses",main = "Poligono de frecuencias del consumo de energ?a el?ctrica en kw/h",sub="De octubre del 2013 a febrero del 2016 ")
hist(kw,col = "Yellow",ylab ="Frecuencia",xlab="Consumo",main="Consumo de kw/h")
polygon.freq(h1,frequency=1,lwd=2,type="o",col="black")
#endgraph
#startgraph
h2<-graph.freq(height,frequency=2,col="yellow",xlim=c(6,14))
polygon.freq(h2,frequency=2,col="red")
print(table.freq(h1),row.names=FALSE)
#########################################################################
#Ojivas
fre=c(0,1,4,10,7,3,2,2,0)
sumaacumulativa<-cumsum(fre)
sumaacumulativa
gr<-1:9;for(i in 9:1){gr[i]<-sum(fre[9:i])};gr
dis<-seq(120,260,20);dis
aum<-seq(120,260,20);aum
plot(aum,sumaacumulativa[1:8],type = "o",xlab="Consumo", ylab="N? de meses",main="Ojivas para el consumo de energ?a electrica en kw/h",sub="De octubre del 2013 a febrero del 2016",lwd=2)
lines(dis,gr[2:9],type="o",lty=2,lwd=2)
##############################################################
#Relaci?n emprica de las medidas de tendencia central
#Dibujar histogramas
#simetrico
library("agricolae", lib.loc="~/R/win-library/3.2")
h1=hist(r)
hist(r,xlab = "Valores",ylab = "Frecuencia",main= "Histograma de primera distribuci?n.", col = "Gray",border = "Black")
print(table.freq(h1),row.names=FALSE)
#asimetrico izquierdo
e=as.vector(AirPassengers)
e[1]=160
h2=hist(e,xlab = "Valores",ylab = "Frecuencia",main= "Histograma de segunda distribuci?n.", col = "Gray",border = "Black")
print(table.freq(h2),row.names=FALSE)
media=(125*24+175*24+225*21+275*13+325*21+375*13+425*13+475*8
       +525*4+575+625*2)/(24+24+21+13+21+13+13+8+4+1+2)
media
mean(e)
length(e)
#asimetrico derecho
A1=sample(100:149,2,replace = TRUE);A1
A2=sample(150:199,1,replace = TRUE);A2
A3=sample(200:249,4,replace = TRUE);A3
A4=sample(250:299,8,replace = TRUE);A4
A5=sample(300:349,13,replace = TRUE);A5
A6=sample(350:399,13,replace = TRUE);A6
A7=sample(400:449,21,replace = TRUE);A7
A8=sample(450:499,13,replace = TRUE);A8
A9=sample(500:549,21,replace = TRUE);A9
A10=sample(550:599,25,replace = TRUE);A10
A11=sample(600:649,23,replace = TRUE);A11
e1=c(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11)
e1
h2=hist(e1, xlab = "Valores",ylab = "Frecuencia",main= "Histograma de tercera distribuci?n.", col = "Gray",border = "Black")
print(table.freq(h2),row.names=FALSE)
media2=(125*2+175+225*4+275*8+325*14+375*12+425*21+
          475*14+525*21+575*24+625*23)/length(e1)
media2
#*****************************************************************#
#Distribución de pesos para prolemas del uso de las medidas de tendencia central 
#Ya no ejecutar este código para que no se altere
pesos=rnorm(50,102,3)
################################
pesos
h3=hist(pesos, xlab = "Valores",ylab = "Frecuencia",main= "Histograma de tercera distribuci?n.", col = "Gray",border = "Black")
print(table.freq(h3),row.names=FALSE)


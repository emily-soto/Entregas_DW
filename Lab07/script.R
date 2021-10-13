library(readr)
library(dplyr)
library(highcharter)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(leaflet)
library(sf)
library(qcc)


data<- read_csv("c1.csv")
data$Fecha=dmy(data$Fecha)

data$Camion_5=ifelse(test = str_detect(data$Camion_5, "\\d"), yes =as.numeric(sub('.*Q', '', data$Camion_5)), no = 0)
data$Pickup=ifelse(test = str_detect(data$Pickup, "\\d"), yes =as.numeric(sub('.*Q', '', data$Pickup)), no = 0)
data$Moto=ifelse(test = str_detect(data$Moto, "\\d"), yes =as.numeric(sub('.*Q', '', data$Moto)), no = 0)
data$factura=as.numeric(sub('.*Q', '', data$factura))

data$directoCamion_5=ifelse(test = str_detect(data$directoCamion_5, "\\d"), yes =as.numeric(sub('.*Q', '', data$directoCamion_5)), no = 0)
data$directoPickup=ifelse(test = str_detect(data$directoPickup, "\\d"), yes =as.numeric(sub('.*Q', '', data$directoPickup)), no = 0)
data$directoMoto=ifelse(test = str_detect(data$directoMoto, "\\d"), yes =as.numeric(sub('.*Q', '', data$directoMoto)), no = 0)

data$fijoCamion_5=ifelse(test = str_detect(data$fijoCamion_5, "\\d"), yes =as.numeric(sub('.*Q', '', data$fijoCamion_5)), no = 0)
data$fijoPickup=ifelse(test = str_detect(data$fijoPickup, "\\d"), yes =as.numeric(sub('.*Q', '', data$fijoPickup)), no = 0)
data$fijoMoto=ifelse(test = str_detect(data$fijoMoto, "\\d"), yes =as.numeric(sub('.*Q', '', data$fijoMoto)), no = 0)

data$costo_total=(data$Camion_5+data$Pickup+data$Moto)
data$utilidad=data$factura-data$costo_total

ingresos=data %>%
  mutate(mes = format(Fecha, "%m")) %>%
  group_by(mes) %>% 
  summarise(utilidades=sum(utilidad), costos=sum(costo_total), facturacion=sum(factura), 
            margen=100*utilidades/facturacion, costos_fijos=sum(fijoCamion_5)+sum(fijoPickup)+sum(fijoMoto), 
            costos_directos=sum(directoCamion_5)+sum(directoPickup)+sum(directoMoto))
ingresos$porc_fijos=ingresos$costos_fijos/ingresos$costos
ingresos$porc_firectos=ingresos$costos_directos/ingresos$costos


ingresos %>% select(mes,facturacion,costos) %>% 
  gather("key", "value", facturacion, costos) %>%
  hchart(type='line', hcaes(x=mes, y='value', group='key')) %>% 
  hc_title(text="<b>Facturacion vs costo<b>") %>% 
  hc_subtitle(text="Mensual 2017")

highchart() %>% 
  hc_yAxis_multiples(list(lineWidth = 3),list(showLastLabel = FALSE, opposite = TRUE)) %>% 
  hc_add_series(name = "utilidades", data = ingresos$utilidades, type = "column") %>%
  hc_add_series(name = "margen", data = ingresos$margen, type = "spline", yAxis = 1) %>% 
  hc_title(text="<b>Ganancias 2017<b>")


maping= data%>% select(Lat,Long,utilidad) %>% filter(utilidad==100)

m = leaflet(maping) %>% addTiles()
m %>% addCircleMarkers(maping$Long, maping$Lat, radius=0.2, fillOpacity = 0.5)%>% 
  addMarkers(lng=maping$Long, lat=maping$Lat)

highchart() %>%
  hc_title(text = "Desglose de costos por mes") %>% 
  hc_subtitle(text = "Año 2017") %>% 
  hc_chart(type = "column") %>% 
  hc_xAxis(categories = c("jan","feb","mar","apr","may","jun","jul","aug","sept","oct","nov","dec")) %>% 
  hc_add_series(name = "Costos directos", data = ingresos$costos_directos) %>% 
  hc_add_series(name = "Costos fijos", data = ingresos$costos_fijos) %>% 
  hc_plotOptions(series = list(stacking = "normal")) %>% 
  hc_colors(c("#0073C2FF", "#EFC000FF"))


tableID <- table(data$ID)
Pareto<- pareto.chart(tableID,main = "80-20", col=heat.colors(length(ventas$count)))
Pareto

postes=data %>% group_by(ID) %>% summarise(utilidad=sum(utilidad),)
postes=postes[order(postes$utilidad, decreasing = T),]

porID=data %>%
  group_by(ID) %>% 
  summarise(utilidades=sum(utilidad), costos=sum(costo_total), facturacion=sum(factura), 
            margen=100*utilidades/facturacion, costos_fijos=sum(fijoCamion_5)+sum(fijoPickup)+sum(fijoMoto), 
            costos_directos=sum(directoCamion_5)+sum(directoPickup)+sum(directoMoto))

porID=porID[order(porID$utilidades, decreasing = T),]

TopporID=porID[1:10,]
TopporID$ID
TopporID$share=100*TopporID$utilidades/sum(porID$utilidades)
TopporID$ID=as.character(TopporID$ID)


sum(porID[1:10,8])

porID=porID[order(porID$utilidades, decreasing = F),]

tailporID=porID[1:10,]
tailporID$ID
tailporID$share=100*tailporID$utilidades/sum(porID$utilidades)
tailporID$ID=as.character(tailporID$ID)

sum(porID[1:10,8])

porcod=data %>%
  group_by(Cod) %>% 
  summarise(utilidades=sum(utilidad), costos=sum(costo_total), facturacion=sum(factura), 
            margen=100*utilidades/facturacion, costos_fijos=sum(fijoCamion_5)+sum(fijoPickup)+sum(fijoMoto), 
            costos_directos=sum(directoCamion_5)+sum(directoPickup)+sum(directoMoto))
porcod$MU=100*porcod$utilidades/porcod$costos

TopporID %>% 
  hchart('column',hcaes(ID,share)) %>% 
  hc_title(text = "Share por top poste") %>% 
  hc_subtitle(text = "Año 2017") %>% 
  hc_add_theme(hc_theme_google())

tailporID %>% 
  hchart('column',hcaes(ID,share)) %>% 
  hc_title(text = "Share por peores postes") %>% 
  hc_subtitle(text = "Año 2017") %>% 
  hc_add_theme(hc_theme_538())

porcod %>% 
  filter(!is.na(Cod)) %>% 
  hchart('treemap', hcaes(x = Cod, value = MU, color=MU)) %>% 
  hc_colorAxis(stops = color_stops(colors = viridis::inferno(10)))

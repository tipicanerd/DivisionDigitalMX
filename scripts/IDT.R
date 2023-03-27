library(dplyr)
library(plyr)
library(tidyr)

endutihVivienda <- read.csv("/home/jazmin/Documentos/Escuela/Universidad/Tesis/ConjuntosDatos/conjunto_de_datos_endutih_2020_csv/conjuntos_de_datos/tr_endutih_vivienda_anual_2020.csv")
endutihHogar <- read.csv("/home/jazmin/Documentos/Escuela/Universidad/Tesis/ConjuntosDatos/conjunto_de_datos_endutih_2020_csv/conjuntos_de_datos/tr_endutih_hogar_anual_2020.csv")
endutihResidente <- read.csv("/home/jazmin/Documentos/Escuela/Universidad/Tesis/ConjuntosDatos/conjunto_de_datos_endutih_2020_csv/conjuntos_de_datos/tr_endutih_residente_anual_2020.csv")
ent_names <- read.csv("/home/jazmin/Documentos/Escuela/Universidad/Tesis/ConjuntosDatos/conjunto_de_datos_ecovid_ed_2020_csv/conjunto_de_datos_tvivienda_ecovid_ed_2020/catalogos/ent.csv")

get_ent_id <- function(edo){
  clave <- filter(ent_names,descrip==edo)$cve
  return(clave)
}


acceso <- function(estado){
  endutihHogarEdo <- filter(endutihHogar,ENT==estado)
  
  tel_fija <- sum(filter(endutihHogarEdo,P5_5==1)$FAC_HOG)/
    sum(filter(endutihHogarEdo,!is.na(P5_5))$FAC_HOG)
  
  celu <- sum(filter(endutihHogarEdo,P4_1_6==1)$FAC_HOG)/
    sum(filter(endutihHogarEdo,!is.na(P4_1_6))$FAC_HOG)
  
  compu <- sum(filter(endutihHogarEdo,P4_2_1==1|P4_2_2==1)$FAC_HOG)/
    sum(filter(endutihHogarEdo,!is.na(P4_2_1)|!is.na(P4_2_2))$FAC_HOG)
  
  internet <- sum(filter(endutihHogarEdo,P4_4==1)$FAC_HOG)/
    sum(filter(endutihHogarEdo,!is.na(P4_4))$FAC_HOG)
  
  sF <- c(tel_fija,celu,compu,internet)
  
  Fk <- sum(0.25*sF)
  return(Fk)
}

utilizacion <- function(estado){
  endutihREdo <- filter(endutihResidente,ENT==estado)
  endutihHogarEdo <- filter(endutihHogar,ENT==estado)
  endutihVEdo <- filter(endutihVivienda,ENT==estado)
  
  internet <- sum(filter(endutihREdo,P3_9_2==1)$FAC_HOG)/
    sum(filter(endutihREdo,!is.na(P3_9_2))$FAC_HOG)
 
  endutihHogarEdo <- endutihHogarEdo %>%  inner_join(endutihVEdo[c("VIV_SEL","P2_1")])
  aux <- filter(endutihHogarEdo,!is.na(P4_5))
  
  al <-  filter(endutihHogarEdo,P4_5==1|P4_5==3)
  alambrica <- sum(al$FAC_HOG*al$P2_1)/
    sum(aux$FAC_HOG*aux$P2_1)
  
  ina <-  filter(endutihHogarEdo,P4_5==2|P4_5==3)
  inalambrica <- sum(ina$FAC_HOG*ina$P2_1)/
    sum(aux$FAC_HOG*aux$P2_1)
  
  sF <- c(internet,alambrica,inalambrica)
  
  Fk <- sum(0.33*sF)
  return(Fk)
}

aptitudes <- function(estado){
  endutihREdo <- filter(endutihResidente,ENT==16)
  
  mayores18 <- filter(endutihREdo,EDAD>=18)
  
  alfabetas <- sum(filter(mayores18,NIVEL>=2)$FAC_HOGAR)/
    sum(mayores18$FAC_HOG)
  
  bachillerato <- sum(filter(mayores18,NIVEL>=6)$FAC_HOGAR)/
    sum(mayores18$FAC_HOG)
  
  mayores23 <- filter(endutihREdo,EDAD>=23)
  universidad <- sum(filter(mayores23,NIVEL>=8)$FAC_HOG)/
    sum(mayores23$FAC_HOG)
  
  sF <- c(alfabetas,bachillerato,universidad)
  
  Fk <- sum(0.33*sF)
  return(Fk)
}


IDTMX <- function(estado){
  acc <- acceso(estado)
  uti <- utilizacion(estado)
  apt <- aptitudes(estado)
  
  Fe <- (0.4*acc + 0.4*uti + 0.2*uti)*10
  
  return(Fe)
}

get_IDTMX <- function(edo){
  estado <- get_ent_id(edo)
  idtMX <- IDTMX(estado)
  
  return(idtMX)
}

IDTedL<- round(laply(ent_names$descrip, get_IDTMX),2)

IDTed <- as.data.frame(cbind(ent_names$descrip, IDTedL))

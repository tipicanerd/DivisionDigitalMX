#install.packages("pacman")
library(pacman)

p_load(
    dplyr,
    ggExtra,
    ggplot2,
    hrbrthemes,
    tidyr,
    tidyverse,
    treemap,
    paletteer,
    plyr,
    reshape2,
    scales
)

#### LECTURA DE DATOS #####
base_path <- "../ConjuntosDatos/"
endutih_path <- paste(base_path,"ENDUTIH2020/conjuntos_de_datos/", sep="")

#HOGARES Y VIVIENDA
endutih_vivhogar <- read.csv(
    paste(endutih_path,"viv_hog_agrupados.csv", sep=""), 
    stringsAsFactors = TRUE
    )


#RESIDENTES
endutih_res <- read.csv(
    paste(endutih_path,"residente_agrupados.csv", sep=""),
    stringsAsFactors = TRUE
    )


#USUARIOS
endutih_usu <- read.csv(
    paste(endutih_path,"usuario_agrupados.csv", sep=""),
    stringsAsFactors = TRUE
)

endutih_usu2 <- read.csv(
    paste(endutih_path,"usuario2_agrupados.csv", sep=""),
    stringsAsFactors = TRUE
)

#ESTADOS
estados <- read.csv("../ConjuntosDatos/CodigoEdos.csv", stringsAsFactors = TRUE)

#COMPLETA
endutih_completa <- merge(endutih_vivhogar, select(endutih_res, select= -contains(c("UPM_DIS","ESTRATO","ENT","DOMINIO", "EST_DIS", "TLOC"))), by = c("UPM","VIV_SEL", "HOGAR") )

#IDTMex ESTADO
IDTMex_grupo_edo <- read.csv("../ConjuntosDatos/IDTMex_grupos_edo.csv", stringsAsFactors = TRUE)

#IDTMex ESTADO DOMINIO
IDTMex_dominio_grupo_edo <- read.csv("../ConjuntosDatos/long_IDTMex_desglosado_dominio_grupos_edo.csv", stringsAsFactors = TRUE)

#Jefees del hogar
jefe_hog <- read.csv("../ConjuntosDatos/jefes_delhogar.csv", stringsAsFactors = TRUE)

#Variables universales
nota <- "Nota: Elaboración propia con datos de la ENDUTIH (2020)"
#colores
main <- "dodgerblue2"
lmain <- "dodgerblue4"

lcolor <- "royalblue4"
fcolor <- "royalblue"

nocolor <- "indianred3"
sicolor <- "steelblue2"
nosabecolor <- "lightgray"

masccolor <- "#087E8B"
femcolor <- "#FF5A5F"

palette<-"ggsci::signature_substitutions_cosmic"
cpalette <- "ggthemes::Classic Blue"

#### PRIMARIA DEFINICION ####

path <- "./Graficas/EDU/PRIMARIA"
#DOMINIO Grupo Codigo

PRIMARIAres <- jefe_hog %>% filter(maxNIVEL==2)
PRIMARIAres$Grupo<- paste("Grupo", PRIMARIAres$Grupo)
PRIMARIAres$ENT <-sapply(PRIMARIAres$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

PRIMARIAusu <- inner_join(endutih_usu, PRIMARIAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
PRIMARIAusu$Grupo<- paste("Grupo", PRIMARIAusu$Grupo)
PRIMARIAusu$ENT <-sapply(PRIMARIAusu$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

#tmp <- PRIMARIAusu %>% group_by(UPM,VIV_SEL, HOGAR, NUM_REN) %>% dplyr::summarise(tot=n())

PRIMARIAusu2 <- merge(endutih_usu2, PRIMARIAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
PRIMARIAusu2$Grupo<- paste("Grupo", PRIMARIAusu2$Grupo)
PRIMARIAusu2$ENT <-sapply(PRIMARIAusu2$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


#### PRIMARIA EQUIPOS ####
equipos <- PRIMARIAres[,c(paste0("P3_9_",1:3),"FAC_HOGAR")] %>%
    pivot_longer(-c(FAC_HOGAR), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE)
    


ggplot(equipos, aes(x = as.factor(Equipo), y = FAC_HOGAR, fill = as.factor(respuesta))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Uso de principales TIC de los jefes del hogar",
        subtitle = "Hogares con la primaria como educación máxima",
        caption = nota,
        y="% Jefes", x="Grupo", fill= "Uso"
    )+
    scale_x_discrete(labels=c("Computadora","Internet", "Celular"))+
    scale_y_continuous(breaks = 10*(1:10)/100, labels = 10*(1:10)) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= c("Sí", "NO","No sabe"))+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "PRIMARIA_uso_equipos.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#### PRIMARIA ACTIVIDAD LABORAL ####

actividad <- c(
    "1"="Trabajó","2"="No fue al trabajo","3"= "Buscó trabajo", 
    "4" = "Está jubilado","5"="Se dedicó a estudiar", "6"="Quehaceres del hogar", 
    "7"="Limitación para trabajar", "8"="No trabajó")

ggplot(PRIMARIAres) + 
    geom_bar(aes(x = as.factor(P3_11), y = FAC_HOGAR),stat = "identity", color=main) +
    labs(
        title = "Actividad económica ",
        subtitle = "Hogares con la primaria como educación máxima",
        caption = nota,
        y="Jefes", x="Actividad laboral"
    )+
    scale_x_discrete(labels=actividad)+
    scale_y_continuous(labels = label_number())+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "PRIMARIA_actividad_laboral.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#### PRIMARIA TIPO DE CELULAR ####
pregs <- paste("P8_4", 1:2, sep="_")

tipo_cel <- PRIMARIAusu2 %>% 
    select(all_of(c("FAC_PER", pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "tipo",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    filter(respuesta == 1)%>%
    group_by(tipo) %>% 
    dplyr::summarise(tot = sum(FAC_PER))

tipos <- c(
    "Común", "Inteligente"
)


ggplot(tipo_cel, mapping = aes(x = tipo, y = tot)) + 
    geom_bar(stat = "identity", fill=main)+
    labs(
        title = "Tipo de celular de los jefes del hogar",
        subtitle = "Hogares con la primaria como educación máxima",
        caption = nota,
        y="Cantidad", x="Jefes"
    )+
    scale_x_discrete(labels=tipos)+
    scale_y_continuous(n.breaks = 11,labels = label_number()) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= tipos)+
    theme_light()+
    theme()


ggsave(
    path=path, filename= "PRIMARIA_tipo_cel.pdf",
    device="pdf", dpi="retina",
    height=18, width=25, units="cm"
)


#### PRIMARIA TIPO DE CONTRATACION CELULAR ####
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- PRIMARIAusu2 %>% 
    select(all_of(c("FAC_PER","Grupo","ENT", pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo, ENT,FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo, ENT), names_to = "Plan",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Planes", x="Jefes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10, labels = label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "PRIMARIA_plan_cel.pdf",
    device="pdf", dpi="retina",
    height=18, width=25, units="cm"
)



#### PRIMARIA USO DE APPS DE CELULAR ####
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- PRIMARIAusu2 %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Uso",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Usos", x="Jefes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10, labels=label_number())+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "PRIMARIA_uso_cel.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)



##### PRIMARIA LUGARES INTERNET ####
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- PRIMARIAusu %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Lugar",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Lugares", x="Jefes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10, labels=label_number())+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "PRIMARIA_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)


### PRIMARIA EQUIPO INTERNET ####
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- PRIMARIAusu %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Equipos", x="Jefes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PRIMARIA_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=18, height=25, units="cm"
)


#### SECUNDARIA DEFINICION ####

path <- "./Graficas/EDU/SECUNDARIA"
#DOMINIO Grupo Codigo

SECUNDARIAres <- jefe_hog %>% filter(maxNIVEL==3)
SECUNDARIAres$Grupo<- paste("Grupo", SECUNDARIAres$Grupo)
SECUNDARIAres$ENT <-sapply(SECUNDARIAres$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

SECUNDARIAusu <- merge(endutih_usu, SECUNDARIAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
SECUNDARIAusu$Grupo<- paste("Grupo", SECUNDARIAusu$Grupo)
SECUNDARIAusu$ENT <-sapply(SECUNDARIAusu$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

SECUNDARIAusu2 <- merge(endutih_usu2, SECUNDARIAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
SECUNDARIAusu2$Grupo<- paste("Grupo", SECUNDARIAusu2$Grupo)
SECUNDARIAusu2$ENT <-sapply(SECUNDARIAusu2$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

#### SECUNDARIA EQUIPOS ####
equipos <- SECUNDARIAres[,c(paste0("P3_9_",1:3),"FAC_HOGAR")] %>%
    pivot_longer(-c(FAC_HOGAR), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE)



ggplot(equipos, aes(x = as.factor(Equipo), y = FAC_HOGAR, fill = as.factor(respuesta))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Uso de principales TIC de los jefes del hogar",
        subtitle = "Hogares con la SECUNDARIA como educación máxima",
        caption = nota,
        y="% Jefes", x="Grupo", fill= "Uso"
    )+
    scale_x_discrete(labels=c("Computadora","Internet", "Celular"))+
    scale_y_continuous(breaks = 10*(1:10)/100, labels = 10*(1:10)) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= c("Sí", "NO","No sabe"))+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "SECUNDARIA_uso_equipos.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#### SECUNDARIA ACTIVIDAD LABORAL ####

actividad <- c(
    "1"="Trabajó","2"="No fue al trabajo","3"= "Buscó trabajo", 
    "4" = "Está jubilado","5"="Se dedicó a estudiar", "6"="Quehaceres del hogar", 
    "7"="Limitación para trabajar", "8"="No trabajó")

ggplot(SECUNDARIAres) + 
    geom_bar(aes(x = as.factor(P3_11), y = FAC_HOGAR),stat = "identity", color=main) +
    labs(
        title = "Actividad económica ",
        subtitle = "Hogares con la SECUNDARIA como educación máxima",
        caption = nota,
        y="Jefes", x="Actividad laboral"
    )+
    scale_x_discrete(labels=actividad)+
    scale_y_continuous(labels = label_number())+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "SECUNDARIA_actividad_laboral.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)



#### SECUNDARIA TIPO DE CONTRATACION CELULAR ####
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- SECUNDARIAusu2 %>% 
    select(all_of(c("FAC_PER","Grupo","ENT", pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo, ENT,FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo, ENT), names_to = "Plan",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Planes", x="Jefes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10, labels = label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "SECUNDARIA_plan_cel.pdf",
    device="pdf", dpi="retina",
    width=18, height=25, units="cm"
)



#### SECUNDARIA USO DE APPS DE CELULAR ####
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- SECUNDARIAusu2 %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Uso",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Usos", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10, labels=label_number())+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "SECUNDARIA_uso_cel.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)



##### SECUNDARIA LUGARES INTERNET ####
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- SECUNDARIAusu %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Lugar",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Lugares", x="Jefes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10, labels=label_number())+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "SECUNDARIA_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)


### SECUNDARIA EQUIPO INTERNET ####
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- SECUNDARIAusu %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Equipos", x="Jefes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "SECUNDARIA_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=18, height=25, units="cm"
)



#### PREPARATORIA DEFINICION ####

path <- "./Graficas/EDU/PREPARATORIA"
#DOMINIO Grupo Codigo

PREPARATORIAres <- jefe_hog %>% filter(maxNIVEL==6)
PREPARATORIAres$Grupo<- paste("Grupo", PREPARATORIAres$Grupo)
PREPARATORIAres$ENT <-sapply(PREPARATORIAres$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

PREPARATORIAusu <- merge(endutih_usu, PREPARATORIAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
PREPARATORIAusu$Grupo<- paste("Grupo", PREPARATORIAusu$Grupo)
PREPARATORIAusu$ENT <-sapply(PREPARATORIAusu$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

PREPARATORIAusu2 <- merge(endutih_usu2, PREPARATORIAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
PREPARATORIAusu2$Grupo<- paste("Grupo", PREPARATORIAusu2$Grupo)
PREPARATORIAusu2$ENT <-sapply(PREPARATORIAusu2$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

#### PREPARATORIA EQUIPOS ####
equipos <- PREPARATORIAres[,c(paste0("P3_9_",1:3),"FAC_HOGAR")] %>%
    pivot_longer(-c(FAC_HOGAR), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE)



ggplot(equipos, aes(x = as.factor(Equipo), y = FAC_HOGAR, fill = as.factor(respuesta))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Uso de principales TIC de los jefes del hogar",
        subtitle = "Hogares con la PREPARATORIA como educación máxima",
        caption = nota,
        y="% Jefes", x="Grupo", fill= "Uso"
    )+
    scale_x_discrete(labels=c("Computadora","Internet", "Celular"))+
    scale_y_continuous(breaks = 10*(1:10)/100, labels = 10*(1:10)) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= c("Sí", "NO","No sabe"))+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "PREPARATORIA_uso_equipos.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#### PREPARATORIA ACTIVIDAD LABORAL ####

actividad <- c(
    "1"="Trabajó","2"="No fue al trabajo","3"= "Buscó trabajo", 
    "4" = "Está jubilado","5"="Se dedicó a estudiar", "6"="Quehaceres del hogar", 
    "7"="Limitación para trabajar", "8"="No trabajó")

ggplot(PREPARATORIAres) + 
    geom_bar(aes(x = as.factor(P3_11), y = FAC_HOGAR),stat = "identity", color=main) +
    labs(
        title = "Actividad económica ",
        subtitle = "Hogares con la PREPARATORIA como educación máxima",
        caption = nota,
        y="Jefes", x="Actividad laboral"
    )+
    scale_x_discrete(labels=actividad)+
    scale_y_continuous(labels = label_number())+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "PREPARATORIA_actividad_laboral.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)



#### PREPARATORIA TIPO DE CONTRATACION CELULAR ####
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- PREPARATORIAusu2 %>% 
    select(all_of(c("FAC_PER","Grupo","ENT", pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo, ENT,FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo, ENT), names_to = "Plan",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Planes", x="Jefes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10, labels = label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "PREPARATORIA_plan_cel.pdf",
    device="pdf", dpi="retina",
    width=18, height=25, units="cm"
)



#### PREPARATORIA USO DE APPS DE CELULAR ####
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- PREPARATORIAusu2 %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Uso",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Usos", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10, labels=label_number())+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "PREPARATORIA_uso_cel.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)



##### PREPARATORIA LUGARES INTERNET ####
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- PREPARATORIAusu %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Lugar",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Lugares", x="Jefes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10, labels=label_number())+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "PREPARATORIA_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)


### PREPARATORIA EQUIPO INTERNET ####
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- PREPARATORIAusu %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Equipos", x="Jefes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PREPARATORIA_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=18, height=25, units="cm"
)



#### LICENCIATURA DEFINICION ####

path <- "./Graficas/EDU/LICENCIATURA"
#DOMINIO Grupo Codigo

LICENCIATURAres <- jefe_hog %>% filter(maxNIVEL==8)
LICENCIATURAres$Grupo<- paste("Grupo", LICENCIATURAres$Grupo)
LICENCIATURAres$ENT <-sapply(LICENCIATURAres$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

LICENCIATURAusu <- merge(endutih_usu, LICENCIATURAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
LICENCIATURAusu$Grupo<- paste("Grupo", LICENCIATURAusu$Grupo)
LICENCIATURAusu$ENT <-sapply(LICENCIATURAusu$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

LICENCIATURAusu2 <- merge(endutih_usu2, LICENCIATURAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
LICENCIATURAusu2$Grupo<- paste("Grupo", LICENCIATURAusu2$Grupo)
LICENCIATURAusu2$ENT <-sapply(LICENCIATURAusu2$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

#### LICENCIATURA EQUIPOS ####
equipos <- LICENCIATURAres[,c(paste0("P3_9_",1:3),"FAC_HOGAR")] %>%
    pivot_longer(-c(FAC_HOGAR), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE)



ggplot(equipos, aes(x = as.factor(Equipo), y = FAC_HOGAR, fill = as.factor(respuesta))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Uso de principales TIC de los jefes del hogar",
        subtitle = "Hogares con la LICENCIATURA como educación máxima",
        caption = nota,
        y="% Jefes", x="Grupo", fill= "Uso"
    )+
    scale_x_discrete(labels=c("Computadora","Internet", "Celular"))+
    scale_y_continuous(breaks = 10*(1:10)/100, labels = 10*(1:10)) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= c("Sí", "NO","No sabe"))+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "LICENCIATURA_uso_equipos.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#### LICENCIATURA ACTIVIDAD LABORAL ####

actividad <- c(
    "1"="Trabajó","2"="No fue al trabajo","3"= "Buscó trabajo", 
    "4" = "Está jubilado","5"="Se dedicó a estudiar", "6"="Quehaceres del hogar", 
    "7"="Limitación para trabajar", "8"="No trabajó")

ggplot(LICENCIATURAres) + 
    geom_bar(aes(x = as.factor(P3_11), y = FAC_HOGAR),stat = "identity", color=main) +
    labs(
        title = "Actividad económica ",
        subtitle = "Hogares con la LICENCIATURA como educación máxima",
        caption = nota,
        y="Jefes", x="Actividad laboral"
    )+
    scale_x_discrete(labels=actividad)+
    scale_y_continuous(labels = label_number())+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "LICENCIATURA_actividad_laboral.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)



#### LICENCIATURA TIPO DE CONTRATACION CELULAR ####
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- LICENCIATURAusu2 %>% 
    select(all_of(c("FAC_PER","Grupo","ENT", pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo, ENT,FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo, ENT), names_to = "Plan",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Planes", x="Jefes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10, labels = label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "LICENCIATURA_plan_cel.pdf",
    device="pdf", dpi="retina",
    width=18, height=25, units="cm"
)



#### LICENCIATURA USO DE APPS DE CELULAR ####
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- LICENCIATURAusu2 %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Uso",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Usos", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10, labels=label_number())+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "LICENCIATURA_uso_cel.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)



##### LICENCIATURA LUGARES INTERNET ####
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- LICENCIATURAusu %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Lugar",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Lugares", x="Jefes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10, labels=label_number())+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "LICENCIATURA_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)


### LICENCIATURA EQUIPO INTERNET ####
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- LICENCIATURAusu %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Población potencial a incrementar su IDTMex",
        caption = nota,
        y="Equipos", x="Jefes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "LICENCIATURA_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=18, height=25, units="cm"
)








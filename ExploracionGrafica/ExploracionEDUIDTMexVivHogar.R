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
endutih_hogar <- read.csv(
    paste0(endutih_path,"tr_endutih_hogar_anual_2020.csv"),
    stringsAsFactors = TRUE
    )
cols_eliminadas <- c(
    "P4_3A", "P4_6A", "P4_7A", "P4_8A", "P5_4A",
    "P5_8_1", "P5_9_1", "P5_10_1", "P5_9_3", "P5_10_3", "P5_10_4", 
    "P5_9_5", "P5_10_5", "P5_9_6", "P5_9_7", "P5_9_8"
    )

endutih_vivhogar <-
    inner_join(
        read.csv(
            paste(endutih_path,"viv_hog_agrupados.csv", sep=""), 
            stringsAsFactors = TRUE
        ), endutih_hogar[,c("UPM","VIV_SEL", "HOGAR", cols_eliminadas)], 
        by = c("UPM","VIV_SEL", "HOGAR")
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

PRIMARIAres <- jefe_hog %>% filter(maxNIVEL==2)
PRIMARIAres$Grupo<- paste("Grupo", PRIMARIAres$Grupo)
PRIMARIAres$ENT <-sapply(PRIMARIAres$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

PRIMARIAvivhog <- inner_join(endutih_vivhogar, PRIMARIAres[,c("UPM","VIV_SEL", "HOGAR",)], by = c("UPM","VIV_SEL", "HOGAR")) %>%  distinct()
PRIMARIAvivhog$Grupo<- paste("Grupo", PRIMARIAvivhog$Grupo)
PRIMARIAvivhog$ENT <-sapply(PRIMARIAvivhog$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

PRIMARIAusu <- inner_join(endutih_usu, PRIMARIAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
PRIMARIAusu$Grupo<- paste("Grupo", PRIMARIAusu$Grupo)
PRIMARIAusu$ENT <-sapply(PRIMARIAusu$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


PRIMARIAusu2 <- merge(endutih_usu2, PRIMARIAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
PRIMARIAusu2$Grupo<- paste("Grupo", PRIMARIAusu2$Grupo)
PRIMARIAusu2$ENT <-sapply(PRIMARIAusu2$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


##### PRIMARIA EQUIPOS ####
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


##### PRIMARIA ACTIVIDAD LABORAL ####

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


##### PRIMARIA TIPO DE CELULAR ####
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


##### PRIMARIA TIPO DE CONTRATACION CELULAR ####
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
        subtitle = "Hogares con la primaria como educación máxima",
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



##### PRIMARIA USO DE APPS DE CELULAR ####
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
        subtitle = "Hogares con la primaria como educación máxima",
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
        subtitle = "Hogares con la primaria como educación máxima",
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


##### PRIMARIA EQUIPO INTERNET ####
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
        subtitle = "Hogares con la primaria como educación máxima",
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
    width=25, height=18, units="cm"
)


##### PRIMARIA BIENES Y CONECTIVIDAD ####
pregs <- paste("P4_1",1:6, sep="_")
bienes <- PRIMARIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Bien",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


bienes_desc <- c(
    "Radio", "TV analógica", "TV digital", 
    "Pantalla plana", "Consola", "Celular"
)


ggplot(bienes, aes(y=Bien, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Disponibilidad de bienes y conectividad",
        subtitle = "Hogares donde la máxima educación es la primaria",
        caption = nota,
        y="Bienes", x="Hogares"
    )+
    scale_y_discrete(labels=bienes_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PRIMARIA_bienes_conectividad.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)


##### PRIMARIA Disp computadora ####
pregs <- paste("P4_2",1:3, sep="_")
compus <- PRIMARIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Tipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


compus_desc <- c(
    "Ordenador","Laptop", "Tablet"
)


ggplot(compus, aes(y=Tipo, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Disponibilidad de computadora",
        subtitle = "Hogares donde la máxima educación es la primaria",
        caption = nota,
        y="Tipo de computadora", x="Hogares"
    )+
    scale_y_discrete(labels=compus_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PRIMARIA_dis_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)


##### PRIMARIA motivo no compu ####


motivos <- c(
    "0" = "NA",
    "1"	= "Falta de recursos económicos",
    "2"	 = "No les interesa o no la necesitan",
    "3"	= "No saben usarla",
    "4" = "Utilizan un celular inteligente",
    "5" = "Porque está descompuesta",
    "6" = "Por razones relacionadas con la privacidad o seguridad",
    "7" = "Otra razón",
    "8" = "No responde"
)


ggplot(PRIMARIAvivhog, aes(y=as.factor(P4_3), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Motivos de la falta de una computadora (ordenador, laptop o tableta)",
        subtitle = "Hogares donde la máxima educación es la primaria",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PRIMARIA_motivo_no_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### PRIMARIA MODO DE CONEXION ####



pregs <- paste("P4_6",1:6, sep="_")
medios_conexion <- PRIMARIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Medio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


medios <- c(
    "P4_6_1" = "Línea telefónica",
    "P4_6_2" = "Internet por cable",
    "P4_6_3" = "Conexión satelital ",
    "P4_6_4" = "Señal abierta de WIFI",
    "P4_6_5" = "Línea telefónica por marcación",
    "P4_6_6" = "Otro medio"
)


ggplot(medios_conexion, aes(y=Medio, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Medios de conexión a Internet en el hogar",
        subtitle = "Hogares donde la máxima educación es la primaria",
        caption = nota,
        y="Medio", x="Hogares"
    )+
    scale_y_discrete(labels=medios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PRIMARIA_medios_conexion.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### PRIMARIA motivo no Internet ####


motivos <- c(
    "0" = "NA",
    "1" = "Falta de recursos económicos",
    "2" = "No les interesa o no lo necesitan",
    "3" = "No sabe usarlo", 
    "4" = "Desconoce su utilidad", 
    "5" = "Equipo insuficiente o sin capacidad", 
    "6" = "No hay proveedor o infraestructura en su localidad",
    "7" = "Tienen acceso a Internet en otros lugares",
    "8" = "Por razones relacionadas con la privacidad o seguridad",
    "9" = "Otra razón",
    "10" = "No responde"

)


ggplot(PRIMARIAvivhog, aes(y=as.factor(P4_8), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Motivos de la falta de conexión a Internet",
        subtitle = "Hogares donde la máxima educación es la primaria",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PRIMARIA_motivo_no_Internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)




##### PRIMARIA SERVICIOS ####

pregs <- paste("P5_6",1:5, sep="_")
servs <- PRIMARIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Servicio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


servicios <- c(
    "P5_6_1" = "Internet",
    "P5_6_2" = "TV de paga",
    "P5_6_3" = "Telefonía fija",
    "P5_6_4" = "Telefonía móvil",
    "P5_6_5" = "Ninguno"
)


ggplot(servs, aes(y=Servicio, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Servicios disponibles en el hogar",
        subtitle = "Hogares donde la máxima educación es la primaria",
        caption = nota,
        y="Servicios", x="Hogares"
    )+
    scale_y_discrete(labels=servicios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PRIMARIA_servicios.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### PRIMARIA PAQUETES SERVICIOS ####

pregs <- paste("P5_7",1:8, sep="_")

paquetes_serv <- PRIMARIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Paquete",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


paquetes <- c(
    "P5_7_1" = "TV de paga, telefonía fija, internet y telefonía móvil",
    "P5_7_2" = "TV de paga, Telefonía fija e Internet",
    "P5_7_3" = "TV de paga y Telefonía fija",
    "P5_7_4" = "TV de paga e Internet",
    "P5_7_5" = "Telefonía fija e Internet",
    "P5_7_6" = "Solo TV de paga",
    "P5_7_7" = "Solo telefonía fija",
    "P5_7_8" = "Solo Internet"
)


ggplot(paquetes_serv, aes(y=Paquete, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Paquetes de servicios contratados en el hogar",
        subtitle = "Hogares donde la máxima educación es la primaria",
        caption = nota,
        y="Paquete", x="Hogares"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PRIMARIA_paquete_servicios.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### PRIMARIA PAGO PAQUETES SERVICIOS ####

pregs <- paste("P5_8",1:8, sep="_")

pago_paquetes_serv <- PRIMARIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Paquete",  values_to = "monto", values_drop_na = TRUE) %>%
    filter((monto!=9999.00)&(monto>0)) %>%
    transform(Paquete=as.factor(Paquete))



paquetes <- c(
    "P5_8_1" = "TV de paga, telefonía fija, internet y telefonía móvil",
    "P5_8_2" = "TV de paga, Telefonía fija e Internet",
    "P5_8_3" = "TV de paga y Telefonía fija",
    "P5_8_4" = "TV de paga e Internet",
    "P5_8_5" = "Telefonía fija e Internet",
    "P5_8_6" = "Solo TV de paga",
    "P5_8_7" = "Solo telefonía fija",
    "P5_8_8" = "Solo Internet"
)


ggplot(pago_paquetes_serv)+
    geom_violin(aes(x = monto, y = Paquete, weight = FAC_HOG, group=Paquete), fill=main)+
    labs(
        title = "Montos de pago de los paquetes de servicios contratados en el hogar",
        subtitle = "Hogares donde la máxima educación es la primaria",
        caption = nota,
        y="Paquete", x="Monto (MXN)"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_dollar())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PRIMARIA_pago_paquete_servicios.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)

##### PRIMARIA PISO####
materiales = c(
    "1" = "Tierra", 
    "2" = "Cemento o firme", 
    "3" = "Madera, mosaico u otro recubrimiento"
    
)


ggplot(PRIMARIAvivhog, aes(y=as.factor(P1_1), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Principal material del piso de la vivienda",
        subtitle = "Hogares donde la máxima educación es la primaria",
        caption = nota,
        y="Material", x="Viviendas"
    )+
    scale_y_discrete(labels=materiales)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PRIMARIA_material_piso.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)

##### PRIMARIA AGUA ####
agua = c(
    "1" = "Agua entubada dentro de la vivienda", 
    "2" = "Agua entubada fuera de la vivienda, pero dentro del terreno" ,
    "3" = "Agua entubada de llave pública (o hidrante)", 
    "4" = "Agua entubada que acarrean de otra vivienda",
    "5" = "Agua de pipa", 
    "6" = "Agua de un pozo, río, arroyo, lago u otro"
        
)


ggplot(PRIMARIAvivhog, aes(y=as.factor(P1_2), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Situación del agua potable en la vivienda",
        subtitle = "Hogares donde la máxima educación es la primaria",
        caption = nota,
        y="Situación", x="Viviendas"
    )+
    scale_y_discrete(labels=agua)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PRIMARIA_agua.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### PRIMARIA DRENAJE ####
drenaje = c(
    "1" = "Red pública", 
    "2" = "Fosa séptica", 
    "3" = "Tubería que va a dar a una barranca o grieta", 
    "4" = "Tubería que va a dar a un río, lago o mar", 
    "5" =  "No tiene drenaje"
)


ggplot(PRIMARIAvivhog, aes(y=as.factor(P1_3), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Conexión del drenaje en la vivienda",
        subtitle = "Hogares donde la máxima educación es la primaria",
        caption = nota,
        y="Conexión", x="Viviendas"
    )+
    scale_y_discrete(labels=drenaje)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PRIMARIA_drenaje.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



#### SECUNDARIA DEFINICION ####

path <- "./Graficas/EDU/SECUNDARIA"

SECUNDARIAres <- jefe_hog %>% filter(maxNIVEL==3)
SECUNDARIAres$Grupo<- paste("Grupo", SECUNDARIAres$Grupo)
SECUNDARIAres$ENT <-sapply(SECUNDARIAres$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

SECUNDARIAvivhog <- inner_join(endutih_vivhogar, SECUNDARIAres[,c("UPM","VIV_SEL", "HOGAR")], by = c("UPM","VIV_SEL", "HOGAR")) %>%  distinct()
SECUNDARIAvivhog$Grupo<- paste("Grupo", SECUNDARIAvivhog$Grupo)
SECUNDARIAvivhog$ENT <-sapply(SECUNDARIAvivhog$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})



SECUNDARIAusu <- inner_join(endutih_usu, SECUNDARIAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
SECUNDARIAusu$Grupo<- paste("Grupo", SECUNDARIAusu$Grupo)
SECUNDARIAusu$ENT <-sapply(SECUNDARIAusu$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


SECUNDARIAusu2 <- merge(endutih_usu2, SECUNDARIAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
SECUNDARIAusu2$Grupo<- paste("Grupo", SECUNDARIAusu2$Grupo)
SECUNDARIAusu2$ENT <-sapply(SECUNDARIAusu2$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


##### SECUNDARIA EQUIPOS ####
equipos <- SECUNDARIAres[,c(paste0("P3_9_",1:3),"FAC_HOGAR")] %>%
    pivot_longer(-c(FAC_HOGAR), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE)



ggplot(equipos, aes(x = as.factor(Equipo), y = FAC_HOGAR, fill = as.factor(respuesta))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Uso de principales TIC de los jefes del hogar",
        subtitle = "Hogares con la secundaria como educación máxima",
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


##### SECUNDARIA ACTIVIDAD LABORAL ####

actividad <- c(
    "1"="Trabajó","2"="No fue al trabajo","3"= "Buscó trabajo", 
    "4" = "Está jubilado","5"="Se dedicó a estudiar", "6"="Quehaceres del hogar", 
    "7"="Limitación para trabajar", "8"="No trabajó")

ggplot(SECUNDARIAres) + 
    geom_bar(aes(x = as.factor(P3_11), y = FAC_HOGAR),stat = "identity", color=main) +
    labs(
        title = "Actividad económica ",
        subtitle = "Hogares con la secundaria como educación máxima",
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


##### SECUNDARIA TIPO DE CELULAR ####
pregs <- paste("P8_4", 1:2, sep="_")

tipo_cel <- SECUNDARIAusu2 %>% 
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
        subtitle = "Hogares con la secundaria como educación máxima",
        caption = nota,
        y="Cantidad", x="Jefes"
    )+
    scale_x_discrete(labels=tipos)+
    scale_y_continuous(n.breaks = 11,labels = label_number()) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= tipos)+
    theme_light()+
    theme()


ggsave(
    path=path, filename= "SECUNDARIA_tipo_cel.pdf",
    device="pdf", dpi="retina",
    height=18, width=25, units="cm"
)


##### SECUNDARIA TIPO DE CONTRATACION CELULAR ####
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
        subtitle = "Hogares con la secundaria como educación máxima",
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
    height=18, width=25, units="cm"
)



##### SECUNDARIA USO DE APPS DE CELULAR ####
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
        subtitle = "Hogares con la secundaria como educación máxima",
        caption = nota,
        y="Usos", x="Jefes"
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
        subtitle = "Hogares con la secundaria como educación máxima",
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


##### SECUNDARIA EQUIPO INTERNET ####
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
        subtitle = "Hogares con la secundaria como educación máxima",
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
    width=25, height=18, units="cm"
)


##### SECUNDARIA BIENES Y CONECTIVIDAD ####
pregs <- paste("P4_1",1:6, sep="_")
bienes <- SECUNDARIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Bien",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


bienes_desc <- c(
    "Radio", "TV analógica", "TV digital", 
    "Pantalla plana", "Consola", "Celular"
)


ggplot(bienes, aes(y=Bien, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Disponibilidad de bienes y conectividad",
        subtitle = "Hogares donde la máxima educación es la secundaria",
        caption = nota,
        y="Bienes", x="Hogares"
    )+
    scale_y_discrete(labels=bienes_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "SECUNDARIA_bienes_conectividad.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)


##### SECUNDARIA Disp computadora ####
pregs <- paste("P4_2",1:3, sep="_")
compus <- SECUNDARIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Tipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


compus_desc <- c(
    "Ordenador","Laptop", "Tablet"
)


ggplot(compus, aes(y=Tipo, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Disponibilidad de computadora",
        subtitle = "Hogares donde la máxima educación es la secundaria",
        caption = nota,
        y="Tipo de computadora", x="Hogares"
    )+
    scale_y_discrete(labels=compus_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "SECUNDARIA_dis_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)


##### SECUNDARIA motivo no compu ####


motivos <- c(
    "0" = "NA",
    "1"	= "Falta de recursos económicos",
    "2"	 = "No les interesa o no la necesitan",
    "3"	= "No saben usarla",
    "4" = "Utilizan un celular inteligente",
    "5" = "Porque está descompuesta",
    "6" = "Por razones relacionadas con la privacidad o seguridad",
    "7" = "Otra razón",
    "8" = "No responde"
)


ggplot(SECUNDARIAvivhog, aes(y=as.factor(P4_3), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Motivos de la falta de una computadora (ordenador, laptop o tableta)",
        subtitle = "Hogares donde la máxima educación es la secundaria",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "SECUNDARIA_motivo_no_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### SECUNDARIA MODO DE CONEXION ####


pregs <- paste("P4_6",1:6, sep="_")
medios_conexion <- SECUNDARIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Medio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


medios <- c(
    "P4_6_1" = "Línea telefónica",
    "P4_6_2" = "Internet por cable",
    "P4_6_3" = "Conexión satelital ",
    "P4_6_4" = "Señal abierta de WIFI",
    "P4_6_5" = "Línea telefónica por marcación",
    "P4_6_6" = "Otro medio"
)


ggplot(medios_conexion, aes(y=Medio, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Medios de conexión a Internet en el hogar",
        subtitle = "Hogares donde la máxima educación es la secundaria",
        caption = nota,
        y="Medio", x="Hogares"
    )+
    scale_y_discrete(labels=medios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "SECUNDARIA_medios_conexion.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### SECUNDARIA motivo no Internet ####


motivos <- c(
    "0" = "NA",
    "1" = "Falta de recursos económicos",
    "2" = "No les interesa o no lo necesitan",
    "3" = "No sabe usarlo", 
    "4" = "Desconoce su utilidad", 
    "5" = "Equipo insuficiente o sin capacidad", 
    "6" = "No hay proveedor o infraestructura en su localidad",
    "7" = "Tienen acceso a Internet en otros lugares",
    "8" = "Por razones relacionadas con la privacidad o seguridad",
    "9" = "Otra razón",
    "10" = "No responde"
    
)


ggplot(SECUNDARIAvivhog, aes(y=as.factor(P4_8), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Motivos de la falta de conexión a Internet",
        subtitle = "Hogares donde la máxima educación es la secundaria",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "SECUNDARIA_motivo_no_Internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)




##### SECUNDARIA SERVICIOS ####

pregs <- paste("P5_6",1:5, sep="_")
servs <- SECUNDARIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Servicio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


servicios <- c(
    "P5_6_1" = "Internet",
    "P5_6_2" = "TV de paga",
    "P5_6_3" = "Telefonía fija",
    "P5_6_4" = "Telefonía móvil",
    "P5_6_5" = "Ninguno"
)


ggplot(servs, aes(y=Servicio, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Servicios disponibles en el hogar",
        subtitle = "Hogares donde la máxima educación es la secundaria",
        caption = nota,
        y="Servicios", x="Hogares"
    )+
    scale_y_discrete(labels=servicios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "SECUNDARIA_servicios.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### SECUNDARIA PAQUETES SERVICIOS ####

pregs <- paste("P5_7",1:8, sep="_")

paquetes_serv <- SECUNDARIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Paquete",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


paquetes <- c(
    "P5_7_1" = "TV de paga, telefonía fija, internet y telefonía móvil",
    "P5_7_2" = "TV de paga, Telefonía fija e Internet",
    "P5_7_3" = "TV de paga y Telefonía fija",
    "P5_7_4" = "TV de paga e Internet",
    "P5_7_5" = "Telefonía fija e Internet",
    "P5_7_6" = "Solo TV de paga",
    "P5_7_7" = "Solo telefonía fija",
    "P5_7_8" = "Solo Internet"
)


ggplot(paquetes_serv, aes(y=Paquete, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Paquetes de servicios contratados en el hogar",
        subtitle = "Hogares donde la máxima educación es la secundaria",
        caption = nota,
        y="Paquete", x="Hogares"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "SECUNDARIA_paquete_servicios.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### SECUNDARIA PAGO PAQUETES SERVICIOS ####

pregs <- paste("P5_8",1:8, sep="_")

pago_paquetes_serv <- SECUNDARIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Paquete",  values_to = "monto", values_drop_na = TRUE) %>%
    filter((monto!=9999.00)&(monto>0)) %>%
    transform(Paquete=as.factor(Paquete))



paquetes <- c(
    "P5_8_1" = "TV de paga, telefonía fija, internet y telefonía móvil",
    "P5_8_2" = "TV de paga, Telefonía fija e Internet",
    "P5_8_3" = "TV de paga y Telefonía fija",
    "P5_8_4" = "TV de paga e Internet",
    "P5_8_5" = "Telefonía fija e Internet",
    "P5_8_6" = "Solo TV de paga",
    "P5_8_7" = "Solo telefonía fija",
    "P5_8_8" = "Solo Internet"
)


ggplot(pago_paquetes_serv)+
    geom_violin(aes(x = monto, y = Paquete, weight = FAC_HOG, group=Paquete), fill=main)+
    labs(
        title = "Montos de pago de los paquetes de servicios contratados en el hogar",
        subtitle = "Hogares donde la máxima educación es la secundaria",
        caption = nota,
        y="Paquete", x="Monto (MXN)"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_dollar())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "SECUNDARIA_pago_paquete_servicios.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)


##### SECUNDARIA PISO####
materiales = c(
    "1" = "Tierra", 
    "2" = "Cemento o firme", 
    "3" = "Madera, mosaico u otro recubrimiento"
    
)


ggplot(SECUNDARIAvivhog, aes(y=as.factor(P1_1), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Principal material del piso de la vivienda",
        subtitle = "Hogares donde la máxima educación es la secundaria",
        caption = nota,
        y="Material", x="Viviendas"
    )+
    scale_y_discrete(labels=materiales)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "SECUNDARIA_material_piso.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)

##### SECUNDARIA AGUA ####
agua = c(
    "1" = "Agua entubada dentro de la vivienda", 
    "2" = "Agua entubada fuera de la vivienda, pero dentro del terreno" ,
    "3" = "Agua entubada de llave pública (o hidrante)", 
    "4" = "Agua entubada que acarrean de otra vivienda",
    "5" = "Agua de pipa", 
    "6" = "Agua de un pozo, río, arroyo, lago u otro"
    
)


ggplot(SECUNDARIAvivhog, aes(y=as.factor(P1_2), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Situación del agua potable en la vivienda",
        subtitle = "Hogares donde la máxima educación es la secundaria",
        caption = nota,
        y="Situación", x="Viviendas"
    )+
    scale_y_discrete(labels=agua)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "SECUNDARIA_agua.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### SECUNDARIA DRENAJE ####
drenaje = c(
    "1" = "Red pública", 
    "2" = "Fosa séptica", 
    "3" = "Tubería que va a dar a una barranca o grieta", 
    "4" = "Tubería que va a dar a un río, lago o mar", 
    "5" =  "No tiene drenaje"
)


ggplot(SECUNDARIAvivhog, aes(y=as.factor(P1_3), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Conexión del drenaje en la vivienda",
        subtitle = "Hogares donde la máxima educación es la secundaria",
        caption = nota,
        y="Conexión", x="Viviendas"
    )+
    scale_y_discrete(labels=drenaje)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "SECUNDARIA_drenaje.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)


#### PREPARATORIA DEFINICION ####

path <- "./Graficas/EDU/PREPARATORIA"

PREPARATORIAres <- jefe_hog %>% filter(maxNIVEL==6)
PREPARATORIAres$Grupo<- paste("Grupo", PREPARATORIAres$Grupo)
PREPARATORIAres$ENT <-sapply(PREPARATORIAres$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

PREPARATORIAvivhog <- inner_join(endutih_vivhogar, PREPARATORIAres[,c("UPM","VIV_SEL", "HOGAR")], by = c("UPM","VIV_SEL", "HOGAR")) %>%  distinct()
PREPARATORIAvivhog$Grupo<- paste("Grupo", PREPARATORIAvivhog$Grupo)
PREPARATORIAvivhog$ENT <-sapply(PREPARATORIAvivhog$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})



PREPARATORIAusu <- inner_join(endutih_usu, PREPARATORIAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
PREPARATORIAusu$Grupo<- paste("Grupo", PREPARATORIAusu$Grupo)
PREPARATORIAusu$ENT <-sapply(PREPARATORIAusu$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


PREPARATORIAusu2 <- merge(endutih_usu2, PREPARATORIAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
PREPARATORIAusu2$Grupo<- paste("Grupo", PREPARATORIAusu2$Grupo)
PREPARATORIAusu2$ENT <-sapply(PREPARATORIAusu2$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


##### PREPARATORIA EQUIPOS ####
equipos <- PREPARATORIAres[,c(paste0("P3_9_",1:3),"FAC_HOGAR")] %>%
    pivot_longer(-c(FAC_HOGAR), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE)



ggplot(equipos, aes(x = as.factor(Equipo), y = FAC_HOGAR, fill = as.factor(respuesta))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Uso de principales TIC de los jefes del hogar",
        subtitle = "Hogares con la preparatoria como educación máxima",
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


##### PREPARATORIA ACTIVIDAD LABORAL ####

actividad <- c(
    "1"="Trabajó","2"="No fue al trabajo","3"= "Buscó trabajo", 
    "4" = "Está jubilado","5"="Se dedicó a estudiar", "6"="Quehaceres del hogar", 
    "7"="Limitación para trabajar", "8"="No trabajó")

ggplot(PREPARATORIAres) + 
    geom_bar(aes(x = as.factor(P3_11), y = FAC_HOGAR),stat = "identity", color=main) +
    labs(
        title = "Actividad económica ",
        subtitle = "Hogares con la preparatoria como educación máxima",
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


##### PREPARATORIA TIPO DE CELULAR ####
pregs <- paste("P8_4", 1:2, sep="_")

tipo_cel <- PREPARATORIAusu2 %>% 
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
        subtitle = "Hogares con la preparatoria como educación máxima",
        caption = nota,
        y="Cantidad", x="Jefes"
    )+
    scale_x_discrete(labels=tipos)+
    scale_y_continuous(n.breaks = 11,labels = label_number()) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= tipos)+
    theme_light()+
    theme()


ggsave(
    path=path, filename= "PREPARATORIA_tipo_cel.pdf",
    device="pdf", dpi="retina",
    height=18, width=25, units="cm"
)


##### PREPARATORIA TIPO DE CONTRATACION CELULAR ####
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
        subtitle = "Hogares con la preparatoria como educación máxima",
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
    height=18, width=25, units="cm"
)



##### PREPARATORIA USO DE APPS DE CELULAR ####
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
        subtitle = "Hogares con la preparatoria como educación máxima",
        caption = nota,
        y="Usos", x="Jefes"
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
        subtitle = "Hogares con la preparatoria como educación máxima",
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


##### PREPARATORIA EQUIPO INTERNET ####
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
        subtitle = "Hogares con la preparatoria como educación máxima",
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
    width=25, height=18, units="cm"
)


##### PREPARATORIA BIENES Y CONECTIVIDAD ####
pregs <- paste("P4_1",1:6, sep="_")
bienes <- PREPARATORIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Bien",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


bienes_desc <- c(
    "Radio", "TV analógica", "TV digital", 
    "Pantalla plana", "Consola", "Celular"
)


ggplot(bienes, aes(y=Bien, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Disponibilidad de bienes y conectividad",
        subtitle = "Hogares donde la máxima educación es la preparatoria",
        caption = nota,
        y="Bienes", x="Hogares"
    )+
    scale_y_discrete(labels=bienes_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PREPARATORIA_bienes_conectividad.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)


##### PREPARATORIA Disp computadora ####
pregs <- paste("P4_2",1:3, sep="_")
compus <- PREPARATORIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Tipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


compus_desc <- c(
    "Ordenador","Laptop", "Tablet"
)


ggplot(compus, aes(y=Tipo, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Disponibilidad de computadora",
        subtitle = "Hogares donde la máxima educación es la preparatoria",
        caption = nota,
        y="Tipo de computadora", x="Hogares"
    )+
    scale_y_discrete(labels=compus_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PREPARATORIA_dis_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)


##### PREPARATORIA motivo no compu ####


motivos <- c(
    "0" = "NA",
    "1"	= "Falta de recursos económicos",
    "2"	 = "No les interesa o no la necesitan",
    "3"	= "No saben usarla",
    "4" = "Utilizan un celular inteligente",
    "5" = "Porque está descompuesta",
    "6" = "Por razones relacionadas con la privacidad o seguridad",
    "7" = "Otra razón",
    "8" = "No responde"
)


ggplot(PREPARATORIAvivhog, aes(y=as.factor(P4_3), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Motivos de la falta de una computadora (ordenador, laptop o tableta)",
        subtitle = "Hogares donde la máxima educación es la preparatoria",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PREPARATORIA_motivo_no_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### PREPARATORIA MODO DE CONEXION ####



pregs <- paste("P4_6",1:6, sep="_")
medios_conexion <- PREPARATORIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Medio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


medios <- c(
    "P4_6_1" = "Línea telefónica",
    "P4_6_2" = "Internet por cable",
    "P4_6_3" = "Conexión satelital ",
    "P4_6_4" = "Señal abierta de WIFI",
    "P4_6_5" = "Línea telefónica por marcación",
    "P4_6_6" = "Otro medio"
)


ggplot(medios_conexion, aes(y=Medio, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Medios de conexión a Internet en el hogar",
        subtitle = "Hogares donde la máxima educación es la preparatoria",
        caption = nota,
        y="Medio", x="Hogares"
    )+
    scale_y_discrete(labels=medios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PREPARATORIA_medios_conexion.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### PREPARATORIA motivo no Internet ####


motivos <- c(
    "0" = "NA",
    "1" = "Falta de recursos económicos",
    "2" = "No les interesa o no lo necesitan",
    "3" = "No sabe usarlo", 
    "4" = "Desconoce su utilidad", 
    "5" = "Equipo insuficiente o sin capacidad", 
    "6" = "No hay proveedor o infraestructura en su localidad",
    "7" = "Tienen acceso a Internet en otros lugares",
    "8" = "Por razones relacionadas con la privacidad o seguridad",
    "9" = "Otra razón",
    "10" = "No responde"
    
)


ggplot(PREPARATORIAvivhog, aes(y=as.factor(P4_8), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Motivos de la falta de conexión a Internet",
        subtitle = "Hogares donde la máxima educación es la preparatoria",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PREPARATORIA_motivo_no_Internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)




##### PREPARATORIA SERVICIOS ####

pregs <- paste("P5_6",1:5, sep="_")
servs <- PREPARATORIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Servicio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


servicios <- c(
    "P5_6_1" = "Internet",
    "P5_6_2" = "TV de paga",
    "P5_6_3" = "Telefonía fija",
    "P5_6_4" = "Telefonía móvil",
    "P5_6_5" = "Ninguno"
)


ggplot(servs, aes(y=Servicio, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Servicios disponibles en el hogar",
        subtitle = "Hogares donde la máxima educación es la preparatoria",
        caption = nota,
        y="Servicios", x="Hogares"
    )+
    scale_y_discrete(labels=servicios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PREPARATORIA_servicios.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### PREPARATORIA PAQUETES SERVICIOS ####

pregs <- paste("P5_7",1:8, sep="_")

paquetes_serv <- PREPARATORIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Paquete",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


paquetes <- c(
    "P5_7_1" = "TV de paga, telefonía fija, internet y telefonía móvil",
    "P5_7_2" = "TV de paga, Telefonía fija e Internet",
    "P5_7_3" = "TV de paga y Telefonía fija",
    "P5_7_4" = "TV de paga e Internet",
    "P5_7_5" = "Telefonía fija e Internet",
    "P5_7_6" = "Solo TV de paga",
    "P5_7_7" = "Solo telefonía fija",
    "P5_7_8" = "Solo Internet"
)


ggplot(paquetes_serv, aes(y=Paquete, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Paquetes de servicios contratados en el hogar",
        subtitle = "Hogares donde la máxima educación es la preparatoria",
        caption = nota,
        y="Paquete", x="Hogares"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PREPARATORIA_paquete_servicios.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### PREPARATORIA PAGO PAQUETES SERVICIOS ####

pregs <- paste("P5_8",1:8, sep="_")

pago_paquetes_serv <- PREPARATORIAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Paquete",  values_to = "monto", values_drop_na = TRUE) %>%
    filter((monto!=9999.00)&(monto>0)) %>%
    transform(Paquete=as.factor(Paquete))



paquetes <- c(
    "P5_8_1" = "TV de paga, telefonía fija, internet y telefonía móvil",
    "P5_8_2" = "TV de paga, Telefonía fija e Internet",
    "P5_8_3" = "TV de paga y Telefonía fija",
    "P5_8_4" = "TV de paga e Internet",
    "P5_8_5" = "Telefonía fija e Internet",
    "P5_8_6" = "Solo TV de paga",
    "P5_8_7" = "Solo telefonía fija",
    "P5_8_8" = "Solo Internet"
)


ggplot(pago_paquetes_serv)+
    geom_violin(aes(x = monto, y = Paquete, weight = FAC_HOG, group=Paquete), fill=main)+
    labs(
        title = "Montos de pago de los paquetes de servicios contratados en el hogar",
        subtitle = "Hogares donde la máxima educación es la preparatoria",
        caption = nota,
        y="Paquete", x="Monto (MXN)"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_dollar())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PREPARATORIA_pago_paquete_servicios.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### PREPARATORIA PISO####
materiales = c(
    "1" = "Tierra", 
    "2" = "Cemento o firme", 
    "3" = "Madera, mosaico u otro recubrimiento"
    
)


ggplot(PREPARATORIAvivhog, aes(y=as.factor(P1_1), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Principal material del piso de la vivienda",
        subtitle = "Hogares donde la máxima educación es la preparatoria",
        caption = nota,
        y="Material", x="Viviendas"
    )+
    scale_y_discrete(labels=materiales)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PREPARATORIA_material_piso.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)

##### PREPARATORIA AGUA ####
agua = c(
    "1" = "Agua entubada dentro de la vivienda", 
    "2" = "Agua entubada fuera de la vivienda, pero dentro del terreno" ,
    "3" = "Agua entubada de llave pública (o hidrante)", 
    "4" = "Agua entubada que acarrean de otra vivienda",
    "5" = "Agua de pipa", 
    "6" = "Agua de un pozo, río, arroyo, lago u otro"
    
)


ggplot(PREPARATORIAvivhog, aes(y=as.factor(P1_2), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Situación del agua potable en la vivienda",
        subtitle = "Hogares donde la máxima educación es la preparatoria",
        caption = nota,
        y="Situación", x="Viviendas"
    )+
    scale_y_discrete(labels=agua)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PREPARATORIA_agua.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### PREPARATORIA DRENAJE ####
drenaje = c(
    "1" = "Red pública", 
    "2" = "Fosa séptica", 
    "3" = "Tubería que va a dar a una barranca o grieta", 
    "4" = "Tubería que va a dar a un río, lago o mar", 
    "5" =  "No tiene drenaje"
)


ggplot(PREPARATORIAvivhog, aes(y=as.factor(P1_3), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Conexión del drenaje en la vivienda",
        subtitle = "Hogares donde la máxima educación es la preparatoria",
        caption = nota,
        y="Conexión", x="Viviendas"
    )+
    scale_y_discrete(labels=drenaje)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "PREPARATORIA_drenaje.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)


#### LICENCIATURA DEFINICION ####

path <- "./Graficas/EDU/LICENCIATURA"

LICENCIATURAres <- jefe_hog %>% filter(maxNIVEL==8)
LICENCIATURAres$Grupo<- paste("Grupo", LICENCIATURAres$Grupo)
LICENCIATURAres$ENT <-sapply(LICENCIATURAres$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

LICENCIATURAvivhog <- inner_join(endutih_vivhogar, LICENCIATURAres[,c("UPM","VIV_SEL", "HOGAR")], by = c("UPM","VIV_SEL", "HOGAR")) %>%  distinct()
LICENCIATURAvivhog$Grupo<- paste("Grupo", LICENCIATURAvivhog$Grupo)
LICENCIATURAvivhog$ENT <-sapply(LICENCIATURAvivhog$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})



LICENCIATURAusu <- inner_join(endutih_usu, LICENCIATURAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
LICENCIATURAusu$Grupo<- paste("Grupo", LICENCIATURAusu$Grupo)
LICENCIATURAusu$ENT <-sapply(LICENCIATURAusu$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


LICENCIATURAusu2 <- merge(endutih_usu2, LICENCIATURAres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
LICENCIATURAusu2$Grupo<- paste("Grupo", LICENCIATURAusu2$Grupo)
LICENCIATURAusu2$ENT <-sapply(LICENCIATURAusu2$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


##### LICENCIATURA EQUIPOS ####
equipos <- LICENCIATURAres[,c(paste0("P3_9_",1:3),"FAC_HOGAR")] %>%
    pivot_longer(-c(FAC_HOGAR), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE)



ggplot(equipos, aes(x = as.factor(Equipo), y = FAC_HOGAR, fill = as.factor(respuesta))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Uso de principales TIC de los jefes del hogar",
        subtitle = "Hogares con la licenciatura como educación máxima",
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


##### LICENCIATURA ACTIVIDAD LABORAL ####

actividad <- c(
    "1"="Trabajó","2"="No fue al trabajo","3"= "Buscó trabajo", 
    "4" = "Está jubilado","5"="Se dedicó a estudiar", "6"="Quehaceres del hogar", 
    "7"="Limitación para trabajar", "8"="No trabajó")

ggplot(LICENCIATURAres) + 
    geom_bar(aes(x = as.factor(P3_11), y = FAC_HOGAR),stat = "identity", color=main) +
    labs(
        title = "Actividad económica ",
        subtitle = "Hogares con la licenciatura como educación máxima",
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


##### LICENCIATURA TIPO DE CELULAR ####
pregs <- paste("P8_4", 1:2, sep="_")

tipo_cel <- LICENCIATURAusu2 %>% 
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
        subtitle = "Hogares con la licenciatura como educación máxima",
        caption = nota,
        y="Cantidad", x="Jefes"
    )+
    scale_x_discrete(labels=tipos)+
    scale_y_continuous(n.breaks = 11,labels = label_number()) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= tipos)+
    theme_light()+
    theme()


ggsave(
    path=path, filename= "LICENCIATURA_tipo_cel.pdf",
    device="pdf", dpi="retina",
    height=18, width=25, units="cm"
)


##### LICENCIATURA TIPO DE CONTRATACION CELULAR ####
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
        subtitle = "Hogares con la licenciatura como educación máxima",
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
    height=18, width=25, units="cm"
)



##### LICENCIATURA USO DE APPS DE CELULAR ####
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
        subtitle = "Hogares con la licenciatura como educación máxima",
        caption = nota,
        y="Usos", x="Jefes"
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
        subtitle = "Hogares con la licenciatura como educación máxima",
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


##### LICENCIATURA EQUIPO INTERNET ####
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
        subtitle = "Hogares con la licenciatura como educación máxima",
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
    width=25, height=18, units="cm"
)


##### LICENCIATURA BIENES Y CONECTIVIDAD ####
pregs <- paste("P4_1",1:6, sep="_")
bienes <- LICENCIATURAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Bien",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


bienes_desc <- c(
    "Radio", "TV analógica", "TV digital", 
    "Pantalla plana", "Consola", "Celular"
)


ggplot(bienes, aes(y=Bien, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Disponibilidad de bienes y conectividad",
        subtitle = "Hogares donde la máxima educación es la licenciatura",
        caption = nota,
        y="Bienes", x="Hogares"
    )+
    scale_y_discrete(labels=bienes_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "LICENCIATURA_bienes_conectividad.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)


##### LICENCIATURA Disp computadora ####
pregs <- paste("P4_2",1:3, sep="_")
compus <- LICENCIATURAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Tipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


compus_desc <- c(
    "Ordenador","Laptop", "Tablet"
)


ggplot(compus, aes(y=Tipo, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Disponibilidad de computadora",
        subtitle = "Hogares donde la máxima educación es la licenciatura",
        caption = nota,
        y="Tipo de computadora", x="Hogares"
    )+
    scale_y_discrete(labels=compus_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "LICENCIATURA_dis_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)


##### LICENCIATURA motivo no compu ####


motivos <- c(
    "0" = "NA",
    "1"	= "Falta de recursos económicos",
    "2"	 = "No les interesa o no la necesitan",
    "3"	= "No saben usarla",
    "4" = "Utilizan un celular inteligente",
    "5" = "Porque está descompuesta",
    "6" = "Por razones relacionadas con la privacidad o seguridad",
    "7" = "Otra razón",
    "8" = "No responde"
)


ggplot(LICENCIATURAvivhog, aes(y=as.factor(P4_3), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Motivos de la falta de una computadora (ordenador, laptop o tableta)",
        subtitle = "Hogares donde la máxima educación es la licenciatura",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "LICENCIATURA_motivo_no_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### LICENCIATURA MODO DE CONEXION ####



pregs <- paste("P4_6",1:6, sep="_")
medios_conexion <- LICENCIATURAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Medio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


medios <- c(
    "P4_6_1" = "Línea telefónica",
    "P4_6_2" = "Internet por cable",
    "P4_6_3" = "Conexión satelital ",
    "P4_6_4" = "Señal abierta de WIFI",
    "P4_6_5" = "Línea telefónica por marcación",
    "P4_6_6" = "Otro medio"
)


ggplot(medios_conexion, aes(y=Medio, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Medios de conexión a Internet en el hogar",
        subtitle = "Hogares donde la máxima educación es la licenciatura",
        caption = nota,
        y="Medio", x="Hogares"
    )+
    scale_y_discrete(labels=medios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "LICENCIATURA_medios_conexion.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### LICENCIATURA motivo no Internet ####


motivos <- c(
    "0" = "NA",
    "1" = "Falta de recursos económicos",
    "2" = "No les interesa o no lo necesitan",
    "3" = "No sabe usarlo", 
    "4" = "Desconoce su utilidad", 
    "5" = "Equipo insuficiente o sin capacidad", 
    "6" = "No hay proveedor o infraestructura en su localidad",
    "7" = "Tienen acceso a Internet en otros lugares",
    "8" = "Por razones relacionadas con la privacidad o seguridad",
    "9" = "Otra razón",
    "10" = "No responde"
    
)


ggplot(LICENCIATURAvivhog, aes(y=as.factor(P4_8), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Motivos de la falta de conexión a Internet",
        subtitle = "Hogares donde la máxima educación es la licenciatura",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "LICENCIATURA_motivo_no_Internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)




##### LICENCIATURA SERVICIOS ####

pregs <- paste("P5_6",1:5, sep="_")
servs <- LICENCIATURAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Servicio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


servicios <- c(
    "P5_6_1" = "Internet",
    "P5_6_2" = "TV de paga",
    "P5_6_3" = "Telefonía fija",
    "P5_6_4" = "Telefonía móvil",
    "P5_6_5" = "Ninguno"
)


ggplot(servs, aes(y=Servicio, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Servicios disponibles en el hogar",
        subtitle = "Hogares donde la máxima educación es la licenciatura",
        caption = nota,
        y="Servicios", x="Hogares"
    )+
    scale_y_discrete(labels=servicios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "LICENCIATURA_servicios.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### LICENCIATURA PAQUETES SERVICIOS ####

pregs <- paste("P5_7",1:8, sep="_")

paquetes_serv <- LICENCIATURAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Paquete",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


paquetes <- c(
    "P5_7_1" = "TV de paga, telefonía fija, internet y telefonía móvil",
    "P5_7_2" = "TV de paga, Telefonía fija e Internet",
    "P5_7_3" = "TV de paga y Telefonía fija",
    "P5_7_4" = "TV de paga e Internet",
    "P5_7_5" = "Telefonía fija e Internet",
    "P5_7_6" = "Solo TV de paga",
    "P5_7_7" = "Solo telefonía fija",
    "P5_7_8" = "Solo Internet"
)


ggplot(paquetes_serv, aes(y=Paquete, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Paquetes de servicios contratados en el hogar",
        subtitle = "Hogares donde la máxima educación es la licenciatura",
        caption = nota,
        y="Paquete", x="Hogares"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "LICENCIATURA_paquete_servicios.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### LICENCIATURA PAGO PAQUETES SERVICIOS ####

pregs <- paste("P5_8",1:8, sep="_")

pago_paquetes_serv <- LICENCIATURAvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Paquete",  values_to = "monto", values_drop_na = TRUE) %>%
    filter((monto!=9999.00)&(monto>0)) %>%
    transform(Paquete=as.factor(Paquete))



paquetes <- c(
    "P5_8_1" = "TV de paga, telefonía fija, internet y telefonía móvil",
    "P5_8_2" = "TV de paga, Telefonía fija e Internet",
    "P5_8_3" = "TV de paga y Telefonía fija",
    "P5_8_4" = "TV de paga e Internet",
    "P5_8_5" = "Telefonía fija e Internet",
    "P5_8_6" = "Solo TV de paga",
    "P5_8_7" = "Solo telefonía fija",
    "P5_8_8" = "Solo Internet"
)


ggplot(pago_paquetes_serv)+
    geom_violin(aes(x = monto, y = Paquete, weight = FAC_HOG, group=Paquete), fill=main)+
    labs(
        title = "Montos de pago de los paquetes de servicios contratados en el hogar",
        subtitle = "Hogares donde la máxima educación es la licenciatura",
        caption = nota,
        y="Paquete", x="Monto (MXN)"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_dollar())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "LICENCIATURA_pago_paquete_servicios.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)


##### LICENCIATURA PISO####
materiales = c(
    "1" = "Tierra", 
    "2" = "Cemento o firme", 
    "3" = "Madera, mosaico u otro recubrimiento"
    
)


ggplot(LICENCIATURAvivhog, aes(y=as.factor(P1_1), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Principal material del piso de la vivienda",
        subtitle = "Hogares donde la máxima educación es la licenciatura",
        caption = nota,
        y="Material", x="Viviendas"
    )+
    scale_y_discrete(labels=materiales)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "LICENCIATURA_material_piso.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)

##### LICENCIATURA AGUA ####
agua = c(
    "1" = "Agua entubada dentro de la vivienda", 
    "2" = "Agua entubada fuera de la vivienda, pero dentro del terreno" ,
    "3" = "Agua entubada de llave pública (o hidrante)", 
    "4" = "Agua entubada que acarrean de otra vivienda",
    "5" = "Agua de pipa", 
    "6" = "Agua de un pozo, río, arroyo, lago u otro"
    
)


ggplot(LICENCIATURAvivhog, aes(y=as.factor(P1_2), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Situación del agua potable en la vivienda",
        subtitle = "Hogares donde la máxima educación es la licenciatura",
        caption = nota,
        y="Situación", x="Viviendas"
    )+
    scale_y_discrete(labels=agua)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "LICENCIATURA_agua.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)



##### LICENCIATURA DRENAJE ####
drenaje = c(
    "1" = "Red pública", 
    "2" = "Fosa séptica", 
    "3" = "Tubería que va a dar a una barranca o grieta", 
    "4" = "Tubería que va a dar a un río, lago o mar", 
    "5" =  "No tiene drenaje"
)


ggplot(LICENCIATURAvivhog, aes(y=as.factor(P1_3), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Conexión del drenaje en la vivienda",
        subtitle = "Hogares donde la máxima educación es la licenciatura",
        caption = nota,
        y="Conexión", x="Viviendas"
    )+
    scale_y_discrete(labels=drenaje)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "LICENCIATURA_drenaje.pdf",
    device="pdf", dpi="retina",
    width=25, height=18, units="cm"
)




#### TABLAS CON AGRUPAMIENTOS ####


##### ELECTRICIDAD ####
electricidad <- jefe_hog %>% 
    filter(maxNIVEL %in% c(2,3,6,8)) %>%
    mutate(
        P1_4 = recode(P1_4, '1' = 'si', '2' = 'no')
        )%>%
    group_by(maxNIVEL,P1_4) %>% 
    dplyr::summarise(tot = sum(FAC_VIV)) %>%
    pivot_wider(names_from = P1_4, values_from = tot) %>%
    mutate(
        perc_si = round(100*si / (si+no), 2)
    )











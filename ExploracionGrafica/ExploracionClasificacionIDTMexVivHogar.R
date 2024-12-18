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
    scales,
    forcats,
    grid
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

# Definir rangos de edad
rangos_edad <- c("00-04", "05-09", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85 y más")

# Crear la nueva columna categórica rgo_edad
endutih_res <- endutih_res %>%
    mutate(rgo_edad = case_when(
        EDAD <= 4 ~ "00-04",
        EDAD >= 85 ~ "85 y más",
        EDAD >= 5 & EDAD <= 9 ~ "05-09",
        EDAD >= 10 & EDAD <= 14 ~ "10-14",
        EDAD >= 15 & EDAD <= 19 ~ "15-19",
        EDAD >= 20 & EDAD <= 24 ~ "20-24",
        EDAD >= 25 & EDAD <= 29 ~ "25-29",
        EDAD >= 30 & EDAD <= 34 ~ "30-34",
        EDAD >= 35 & EDAD <= 39 ~ "35-39",
        EDAD >= 40 & EDAD <= 44 ~ "40-44",
        EDAD >= 45 & EDAD <= 49 ~ "45-49",
        EDAD >= 50 & EDAD <= 54 ~ "50-54",
        EDAD >= 55 & EDAD <= 59 ~ "55-59",
        EDAD >= 60 & EDAD <= 64 ~ "60-64",
        EDAD >= 65 & EDAD <= 69 ~ "65-69",
        EDAD >= 70 & EDAD <= 74 ~ "70-74",
        EDAD >= 75 & EDAD <= 79 ~ "75-79",
        EDAD >= 80 & EDAD <= 84 ~ "80-84"
    )) %>%
    mutate(rgo_edad = factor(rgo_edad, levels = rangos_edad))




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

#CLASIFICACION
clasificacion <- read.csv("../ConjuntosDatos/long_IDTMex_desglosado_grupos_clasificados.csv", stringsAsFactors = TRUE)


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


#### BAJO DEFINICION ####

BAJOpath <- "./Graficas/CLAS/BAJO/"

BAJOres <- endutih_res %>% 
    inner_join(clasificacion) %>%
    filter((PAREN==1) & (clasificacion=="Bajo"))
BAJOres$Grupo<- paste("Grupo", BAJOres$Grupo)
BAJOres$ENT <-sapply(BAJOres$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

BAJOvivhog <- inner_join(endutih_vivhogar, BAJOres[,c("UPM","VIV_SEL", "HOGAR")], by = c("UPM","VIV_SEL", "HOGAR")) %>%  distinct()
BAJOvivhog$Grupo<- paste("Grupo", BAJOvivhog$Grupo)
BAJOvivhog$ENT <-sapply(BAJOvivhog$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

BAJOusu <- inner_join(endutih_usu, BAJOres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
BAJOusu$Grupo<- paste("Grupo", BAJOusu$Grupo)
BAJOusu$ENT <-sapply(BAJOusu$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


BAJOusu2 <- merge(endutih_usu2, BAJOres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
BAJOusu2$Grupo<- paste("Grupo", BAJOusu2$Grupo)
BAJOusu2$ENT <-sapply(BAJOusu2$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


##### BAJO PIRAMIDE ####


poblacion_sexo <-  BAJOres %>%
    group_by(rgo_edad,SEXO) %>%
    mutate(
        perc = FAC_HOGAR/sum(FAC_HOGAR)
    )%>%
    dplyr::summarise(
        FAC_HOGAR=sum(FAC_HOGAR),
        perc = sum(perc)
        )


abs_label_number <- function(x) { label_number()(abs(x)) }
fill_labs <- c(
    paste0("Masculino (",label_percent(accuracy = 0.01)(sum(poblacion_sexo[poblacion_sexo$SEXO==1,"perc"])),")"),
    paste0("Femenino (",label_percent(accuracy = 0.01)(sum(poblacion_sexo[poblacion_sexo$SEXO==2,"perc"])),")")
)


ggplot(poblacion_sexo, aes(x = rgo_edad, y = ifelse(SEXO == 1, -FAC_HOGAR, FAC_HOGAR), fill = as.factor(SEXO))) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_continuous(n.breaks = 15, labels = abs_label_number) +
    labs(
        title = "Pirámide Poblacional de los jefes del hogar",
        subtitle = "Hogares con un IDTMex bajo",
        x = "Rango de Edad",
        y = "Jefes",
        fill = "Sexo"
    ) +
    scale_fill_manual( values=c(masccolor, femcolor) ,labels= fill_labs)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))


ggsave(
    path=BAJOpath, filename= "BAJO_piramides.png",
    device="png", dpi="retina",
    width=25, height=14, units="cm"
)



##### BAJO EQUIPOS ####
equipos <- BAJOres[,c(paste0("P3_9_",1:3),"FAC_HOGAR")] %>%
    pivot_longer(-c(FAC_HOGAR), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    group_by(Equipo,respuesta)%>%
    dplyr::summarise(FAC_HOGAR=sum(FAC_HOGAR))
    


ggplot(equipos, aes(x = as.factor(Equipo), y = FAC_HOGAR, fill = as.factor(respuesta))) + 
    geom_bar(stat = "identity",position="stack")+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..y../sum(BAJOres$FAC_HOGAR))), fun = sum, 
                  geom = "text", hjust=0.5, vjust=2 ,color = "white", position = "stack" ) +
    labs(
        title = "Uso de principales TIC de los jefes del hogar",
        subtitle = "Hogares con un IDTMex bajo",
        caption = nota,
        y="Jefes", x="Grupo", fill= "Uso"
    )+
    scale_x_discrete(labels=c("Computadora","Internet", "Celular"))+
    scale_y_continuous(n.breaks = 11, labels = label_number()) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= c("Sí", "NO","No sabe"))+
    theme_light()+
    theme()

ggsave(
    path=BAJOpath, filename= "BAJO_uso_equipos.png",
    device="png", dpi="retina",
    width=25, height=14, units="cm"
)


##### BAJO ACTIVIDAD LABORAL ####

actividades <- c(
    "1"="Trabajó","2"="No fue al trabajo","3"= "Buscó trabajo", 
    "4" = "Está jubilado","5"="Se dedicó a estudiar", "6"="Quehaceres del hogar", 
    "7"="Limitación para trabajar", "8"="No trabajó")

actividad <- BAJOres %>%
    group_by(P3_11)%>%
    dplyr::summarise(FAC_HOGAR=sum(FAC_HOGAR))

ggplot(actividad,aes(x = as.factor(P3_11), y = FAC_HOGAR)) + 
    geom_bar(stat = "identity", fill=main) +
    stat_summary( aes(label = label_percent(accuracy=0.01)(..y../sum(BAJOusu2$FAC_PER))), fun = sum, geom = "text", vjust= -0.05, color = "darkgray" ) +
    labs(
        title = "Actividad económica ",
        subtitle = "Hogares con un IDTMex bajo",
        caption = nota,
        y="Jefes", x="Actividad laboral"
    )+
    scale_x_discrete(labels=actividades)+
    scale_y_continuous(labels = label_number(), n.breaks = 11)+
    theme_light()+
    theme()

ggsave(
    path=BAJOpath, filename= "BAJO_actividad_laboral.png",
    device="png", dpi="retina",
    width=25, height=14, units="cm"
)


##### BAJO TIPO DE CELULAR ####
pregs <- paste("P8_4", 1:2, sep="_")

tipo_cel <- BAJOusu2 %>% 
    select(all_of(c("FAC_PER", pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "tipo",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    filter(respuesta == 1)%>%
    group_by(tipo) %>% 
    dplyr::summarise(tot = sum(FAC_PER))

tipos <- c(
    "Común", "Inteligente"
)

#label_percent(accuracy=0.01)(tot/sum(BAJOusu2$FAC_PER))
ggplot(tipo_cel, mapping = aes(x = tipo, y = tot)) + 
    geom_bar(stat = "identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..y../sum(BAJOusu2$FAC_PER))), fun = sum, geom = "text", vjust= -0.05, color = "darkgray" ) +
    labs(
        title = "Tipo de celular de los jefes del hogar",
        subtitle = "Hogares con un IDTMex bajo",
        caption = nota,
        y="Cantidad", x="Jefes"
    )+
    scale_x_discrete(labels=tipos)+
    scale_y_continuous(n.breaks = 11,labels = label_number()) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= tipos)+
    theme_light()+
    theme()


ggsave(
    path=BAJOpath, filename= "BAJO_tipo_cel.png",
    device="png", dpi="retina",
    height=18, width=25, units="cm"
)


##### BAJO TIPO DE CONTRATACION CELULAR ####
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- BAJOusu2 %>% 
    select(all_of(c("FAC_PER", pregs))) %>%
    mutate(
        Ninguno = 2 - as.numeric(rowSums(is.na(select(., all_of(pregs)))) == length(pregs) )
            
    )%>%
    pivot_longer(-c(FAC_PER), names_to = "Plan",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    mutate(
        Plan = factor(Plan, levels=c(pregs,"Ninguno"))
    ) %>%
    group_by(Plan)%>%
    dplyr::summarise(FAC_PER=sum(FAC_PER))


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOusu2$FAC_PER))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Hogares con un IDTMex bajo",
        caption = nota,
        y="Planes", x="Jefes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 11, labels = label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=BAJOpath, filename= "BAJO_plan_cel.png",
    device="png", dpi="retina",
    height=18, width=25, units="cm"
)



##### BAJO USO DE APPS DE CELULAR ####
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- BAJOusu2 %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "Uso",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Uso)%>%
    dplyr::summarise(FAC_PER=sum(FAC_PER))

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOusu2$FAC_PER))), fun = sum, 
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Hogares con un IDTMex bajo",
        caption = nota,
        y="Usos", x="Jefes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 11, labels=label_number())+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=BAJOpath, filename= "BAJO_uso_cel.png",
    device="png", dpi="retina",
    width=36, height=20, units="cm"
)



##### BAJO LUGARES INTERNET ####
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- BAJOusu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "Lugar",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Lugar)%>%
    dplyr::summarise(FAC_PER=sum(FAC_PER))


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOusu$FAC_PER))), fun = sum, 
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Hogares con un IDTMex bajo",
        caption = nota,
        y="Lugares", x="Jefes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 11, labels=label_number())+
    theme_light()+
    theme()

ggsave(
    path=BAJOpath, filename= "BAJO_lugar_internet.png",
    device="png", dpi="retina",
    width=48, height=27, units="cm"
)


##### BAJO EQUIPO INTERNET ####
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- BAJOusu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Equipo)%>%
    dplyr::summarise(FAC_PER=sum(FAC_PER))


equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOusu$FAC_PER))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Hogares con un IDTMex bajo",
        caption = nota,
        y="Equipos", x="Jefes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=BAJOpath, filename= "BAJO_equipo_internet.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)


##### BAJO BIENES Y CONECTIVIDAD ####
pregs <- paste("P4_1",1:6, sep="_")
bienes <- BAJOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Bien",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    group_by(Bien)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


bienes_desc <- c(
    "Radio", "TV analógica", "TV digital", 
    "Pantalla plana", "Consola", "Celular"
)


ggplot(bienes, aes(y=Bien, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Disponibilidad de bienes y conectividad",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Bienes", x="Hogares"
    )+
    scale_y_discrete(labels=bienes_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=BAJOpath, filename= "BAJO_bienes_conectividad.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)


##### BAJO Disp computadora ####
pregs <- paste("P4_2",1:3, sep="_")
compus <- BAJOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Tipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Tipo)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


compus_desc <- c(
    "Ordenador","Laptop", "Tablet"
)


ggplot(compus, aes(y=Tipo, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Disponibilidad de computadora",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Tipo de computadora", x="Hogares"
    )+
    scale_y_discrete(labels=compus_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=BAJOpath, filename= "BAJO_dis_compu.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)


##### BAJO motivo no compu ####


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

motivos_no_compu <- BAJOvivhog %>%
    group_by(P4_3)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


ggplot(motivos_no_compu, aes(y=as.factor(P4_3), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Motivos de la falta de una computadora (ordenador, laptop o tableta)",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=BAJOpath, filename= "BAJO_motivo_no_compu.png",
    device="png", dpi="retina",
    width=30, height=18, units="cm"
)



##### BAJO MODO DE CONEXION ####

pregs <- paste("P4_6",1:6, sep="_")
medios_conexion <- BAJOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Medio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Medio)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


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
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Medios de conexión a Internet en el hogar",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Medio", x="Hogares"
    )+
    scale_y_discrete(labels=medios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=BAJOpath, filename= "BAJO_medios_conexion.png",
    device="png", dpi="retina",
    width=30, height=18, units="cm"
)



##### BAJO motivo no Internet ####


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

motivos_no_int <- BAJOvivhog %>%
    group_by(P4_8)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))




ggplot(motivos_no_int, aes(y=as.factor(P4_8), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Motivos de la falta de conexión a Internet",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=BAJOpath, filename= "BAJO_motivo_no_Internet.png",
    device="png", dpi="retina",
    width=30, height=18, units="cm"
)




##### BAJO SERVICIOS ####

pregs <- paste("P5_6",1:5, sep="_")
servs <- BAJOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Servicio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Servicio)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


servicios <- c(
    "P5_6_1" = "Internet",
    "P5_6_2" = "TV de paga",
    "P5_6_3" = "Telefonía fija",
    "P5_6_4" = "Telefonía móvil",
    "P5_6_5" = "Ninguno"
)


ggplot(servs, aes(y=Servicio, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Servicios disponibles en el hogar",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Servicios", x="Hogares"
    )+
    scale_y_discrete(labels=servicios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=BAJOpath, filename= "BAJO_servicios.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)



##### BAJO PAQUETES SERVICIOS ####

pregs <- paste("P5_7",1:8, sep="_")

paquetes_serv <- BAJOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Paquete",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Paquete)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


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
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Paquetes de servicios contratados en el hogar",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Paquete", x="Hogares"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=BAJOpath, filename= "BAJO_paquete_servicios.png",
    device="png", dpi="retina",
    width=30, height=18, units="cm"
)



##### BAJO PAGO PAQUETES SERVICIOS ####

pregs <- paste("P5_8",1:8, sep="_")

pago_paquetes_serv <- BAJOvivhog %>% 
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
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Paquete", x="Monto (MXN)"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_dollar())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=BAJOpath, filename= "BAJO_pago_paquete_servicios.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)

##### BAJO PISO####
materiales = c(
    "1" = "Tierra", 
    "2" = "Cemento o firme", 
    "3" = "Madera, mosaico u otro recubrimiento"
    
)


pisos <- BAJOvivhog %>%
    group_by(P1_1)%>%
    dplyr::summarise(FAC_VIV=sum(FAC_VIV))



ggplot(pisos, aes(y=as.factor(P1_1), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Principal material del piso de la vivienda",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Material", x="Viviendas"
    )+
    scale_y_discrete(labels=materiales)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=BAJOpath, filename= "BAJO_material_piso.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)

##### BAJO AGUA ####
agua = c(
    "1" = "Agua entubada dentro de la vivienda", 
    "2" = "Agua entubada fuera de la vivienda, pero dentro del terreno" ,
    "3" = "Agua entubada de llave pública (o hidrante)", 
    "4" = "Agua entubada que acarrean de otra vivienda",
    "5" = "Agua de pipa", 
    "6" = "Agua de un pozo, río, arroyo, lago u otro"
        
)


sit_agua <- BAJOvivhog %>%
    group_by(P1_2)%>%
    dplyr::summarise(FAC_VIV=sum(FAC_VIV))

ggplot(sit_agua, aes(y=as.factor(P1_2), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Situación del agua potable en la vivienda",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Situación", x="Viviendas"
    )+
    scale_y_discrete(labels=agua)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=BAJOpath, filename= "BAJO_agua.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)



##### BAJO DRENAJE ####
drenaje = c(
    "1" = "Red pública", 
    "2" = "Fosa séptica", 
    "3" = "Tubería que va a dar a una barranca o grieta", 
    "4" = "Tubería que va a dar a un río, lago o mar", 
    "5" =  "No tiene drenaje"
)


sit_drenaje <- BAJOvivhog %>%
    group_by(P1_3)%>%
    dplyr::summarise(FAC_VIV=sum(FAC_VIV))

ggplot(sit_drenaje, aes(y=as.factor(P1_3), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(BAJOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Conexión del drenaje en la vivienda",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Conexión", x="Viviendas"
    )+
    scale_y_discrete(labels=drenaje)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=BAJOpath, filename= "BAJO_drenaje.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)



#### MEDIO DEFINICION ####

MEDIOpath <- "./Graficas/CLAS/MEDIO/"

MEDIOres <- endutih_res %>% 
    inner_join(clasificacion) %>%
    filter((PAREN==1) & (clasificacion=="Medio"))
MEDIOres$Grupo<- paste("Grupo", MEDIOres$Grupo)
MEDIOres$ENT <-sapply(MEDIOres$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

MEDIOvivhog <- inner_join(endutih_vivhogar, MEDIOres[,c("UPM","VIV_SEL", "HOGAR")], by = c("UPM","VIV_SEL", "HOGAR")) %>%  distinct()
MEDIOvivhog$Grupo<- paste("Grupo", MEDIOvivhog$Grupo)
MEDIOvivhog$ENT <-sapply(MEDIOvivhog$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

MEDIOusu <- inner_join(endutih_usu, MEDIOres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
MEDIOusu$Grupo<- paste("Grupo", MEDIOusu$Grupo)
MEDIOusu$ENT <-sapply(MEDIOusu$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


MEDIOusu2 <- merge(endutih_usu2, MEDIOres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
MEDIOusu2$Grupo<- paste("Grupo", MEDIOusu2$Grupo)
MEDIOusu2$ENT <-sapply(MEDIOusu2$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


##### MEDIO PIRAMIDE ####


poblacion_sexo <-  MEDIOres %>%
    group_by(rgo_edad,SEXO) %>%
    mutate(
        perc = FAC_HOGAR/sum(FAC_HOGAR)
    )%>%
    dplyr::summarise(
        FAC_HOGAR=sum(FAC_HOGAR),
        perc = sum(perc)
    )


abs_label_number <- function(x) { label_number()(abs(x)) }
fill_labs <- c(
    paste0("Masculino (",label_percent(accuracy = 0.01)(sum(poblacion_sexo[poblacion_sexo$SEXO==1,"perc"])),")"),
    paste0("Femenino (",label_percent(accuracy = 0.01)(sum(poblacion_sexo[poblacion_sexo$SEXO==2,"perc"])),")")
)


ggplot(poblacion_sexo, aes(x = rgo_edad, y = ifelse(SEXO == 1, -FAC_HOGAR, FAC_HOGAR), fill = as.factor(SEXO))) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_continuous(n.breaks = 15, labels = abs_label_number) +
    labs(
        title = "Pirámide Poblacional de los jefes del hogar",
        subtitle = "Hogares con un IDTMex medio",
        x = "Rango de Edad",
        y = "Jefes",
        fill = "Sexo"
    ) +
    scale_fill_manual( values=c(masccolor, femcolor) ,labels= fill_labs)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))


ggsave(
    path=MEDIOpath, filename= "MEDIO_piramides.png",
    device="png", dpi="retina",
    width=25, height=14, units="cm"
)



##### MEDIO EQUIPOS ####
equipos <- MEDIOres[,c(paste0("P3_9_",1:3),"FAC_HOGAR")] %>%
    pivot_longer(-c(FAC_HOGAR), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    group_by(Equipo,respuesta)%>%
    dplyr::summarise(FAC_HOGAR=sum(FAC_HOGAR))



ggplot(equipos, aes(x = as.factor(Equipo), y = FAC_HOGAR, fill = as.factor(respuesta))) + 
    geom_bar(stat = "identity",position="stack")+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..y../sum(MEDIOres$FAC_HOGAR))), fun = sum, 
                  geom = "text", hjust=0.5, vjust=2 ,color = "white", position = "stack" ) +
    labs(
        title = "Uso de principales TIC de los jefes del hogar",
        subtitle = "Hogares con un IDTMex medio",
        caption = nota,
        y="Jefes", x="Grupo", fill= "Uso"
    )+
    scale_x_discrete(labels=c("Computadora","Internet", "Celular"))+
    scale_y_continuous(n.breaks = 11, labels = label_number()) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= c("Sí", "NO","No sabe"))+
    theme_light()+
    theme()

ggsave(
    path=MEDIOpath, filename= "MEDIO_uso_equipos.png",
    device="png", dpi="retina",
    width=25, height=14, units="cm"
)


##### MEDIO ACTIVIDAD LABORAL ####

actividades <- c(
    "1"="Trabajó","2"="No fue al trabajo","3"= "Buscó trabajo", 
    "4" = "Está jubilado","5"="Se dedicó a estudiar", "6"="Quehaceres del hogar", 
    "7"="Limitación para trabajar", "8"="No trabajó")

actividad <- MEDIOres %>%
    group_by(P3_11)%>%
    dplyr::summarise(FAC_HOGAR=sum(FAC_HOGAR))

ggplot(actividad,aes(x = as.factor(P3_11), y = FAC_HOGAR)) + 
    geom_bar(stat = "identity", fill=main) +
    stat_summary( aes(label = label_percent(accuracy=0.01)(..y../sum(MEDIOusu2$FAC_PER))), fun = sum, geom = "text", vjust= -0.05, color = "darkgray" ) +
    labs(
        title = "Actividad económica ",
        subtitle = "Hogares con un IDTMex medio",
        caption = nota,
        y="Jefes", x="Actividad laboral"
    )+
    scale_x_discrete(labels=actividades)+
    scale_y_continuous(labels = label_number(), n.breaks = 11)+
    theme_light()+
    theme()

ggsave(
    path=MEDIOpath, filename= "MEDIO_actividad_laboral.png",
    device="png", dpi="retina",
    width=25, height=14, units="cm"
)


##### MEDIO TIPO DE CELULAR ####
pregs <- paste("P8_4", 1:2, sep="_")

tipo_cel <- MEDIOusu2 %>% 
    select(all_of(c("FAC_PER", pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "tipo",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    filter(respuesta == 1)%>%
    group_by(tipo) %>% 
    dplyr::summarise(tot = sum(FAC_PER))

tipos <- c(
    "Común", "Inteligente"
)

#label_percent(accuracy=0.01)(tot/sum(MEDIOusu2$FAC_PER))
ggplot(tipo_cel, mapping = aes(x = tipo, y = tot)) + 
    geom_bar(stat = "identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..y../sum(MEDIOusu2$FAC_PER))), fun = sum, geom = "text", vjust= -0.05, color = "darkgray" ) +
    labs(
        title = "Tipo de celular de los jefes del hogar",
        subtitle = "Hogares con un IDTMex medio",
        caption = nota,
        y="Cantidad", x="Jefes"
    )+
    scale_x_discrete(labels=tipos)+
    scale_y_continuous(n.breaks = 11,labels = label_number()) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= tipos)+
    theme_light()+
    theme()


ggsave(
    path=MEDIOpath, filename= "MEDIO_tipo_cel.png",
    device="png", dpi="retina",
    height=18, width=25, units="cm"
)


##### MEDIO TIPO DE CONTRATACION CELULAR ####
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- MEDIOusu2 %>% 
    select(all_of(c("FAC_PER", pregs))) %>%
    mutate(
        Ninguno = 2 - as.numeric(rowSums(is.na(select(., all_of(pregs)))) == length(pregs) )
        
    )%>%
    pivot_longer(-c(FAC_PER), names_to = "Plan",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    mutate(
        Plan = factor(Plan, levels=c(pregs,"Ninguno"))
    ) %>%
    group_by(Plan)%>%
    dplyr::summarise(FAC_PER=sum(FAC_PER))


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOusu2$FAC_PER))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Hogares con un IDTMex medio",
        caption = nota,
        y="Planes", x="Jefes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 11, labels = label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=MEDIOpath, filename= "MEDIO_plan_cel.png",
    device="png", dpi="retina",
    height=18, width=25, units="cm"
)



##### MEDIO USO DE APPS DE CELULAR ####
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- MEDIOusu2 %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "Uso",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Uso)%>%
    dplyr::summarise(FAC_PER=sum(FAC_PER))

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOusu2$FAC_PER))), fun = sum, 
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Hogares con un IDTMex medio",
        caption = nota,
        y="Usos", x="Jefes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 11, labels=label_number())+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=MEDIOpath, filename= "MEDIO_uso_cel.png",
    device="png", dpi="retina",
    width=36, height=20, units="cm"
)



##### MEDIO LUGARES INTERNET ####
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- MEDIOusu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "Lugar",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Lugar)%>%
    dplyr::summarise(FAC_PER=sum(FAC_PER))


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOusu$FAC_PER))), fun = sum, 
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Hogares con un IDTMex medio",
        caption = nota,
        y="Lugares", x="Jefes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 11, labels=label_number())+
    theme_light()+
    theme()

ggsave(
    path=MEDIOpath, filename= "MEDIO_lugar_internet.png",
    device="png", dpi="retina",
    width=48, height=27, units="cm"
)


##### MEDIO EQUIPO INTERNET ####
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- MEDIOusu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Equipo)%>%
    dplyr::summarise(FAC_PER=sum(FAC_PER))


equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOusu$FAC_PER))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Hogares con un IDTMex medio",
        caption = nota,
        y="Equipos", x="Jefes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=MEDIOpath, filename= "MEDIO_equipo_internet.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)


##### MEDIO BIENES Y CONECTIVIDAD ####
pregs <- paste("P4_1",1:6, sep="_")
bienes <- MEDIOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Bien",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    group_by(Bien)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


bienes_desc <- c(
    "Radio", "TV analógica", "TV digital", 
    "Pantalla plana", "Consola", "Celular"
)


ggplot(bienes, aes(y=Bien, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Disponibilidad de bienes y conectividad",
        subtitle = "Hogares con IDTMex medio",
        caption = nota,
        y="Bienes", x="Hogares"
    )+
    scale_y_discrete(labels=bienes_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=MEDIOpath, filename= "MEDIO_bienes_conectividad.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)


##### MEDIO Disp computadora ####
pregs <- paste("P4_2",1:3, sep="_")
compus <- MEDIOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Tipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Tipo)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


compus_desc <- c(
    "Ordenador","Laptop", "Tablet"
)


ggplot(compus, aes(y=Tipo, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Disponibilidad de computadora",
        subtitle = "Hogares con IDTMex medio",
        caption = nota,
        y="Tipo de computadora", x="Hogares"
    )+
    scale_y_discrete(labels=compus_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=MEDIOpath, filename= "MEDIO_dis_compu.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)


##### MEDIO motivo no compu ####


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

motivos_no_compu <- MEDIOvivhog %>%
    group_by(P4_3)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


ggplot(motivos_no_compu, aes(y=as.factor(P4_3), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Motivos de la falta de una computadora (ordenador, laptop o tableta)",
        subtitle = "Hogares con IDTMex medio",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=MEDIOpath, filename= "MEDIO_motivo_no_compu.png",
    device="png", dpi="retina",
    width=30, height=18, units="cm"
)



##### MEDIO MODO DE CONEXION ####

pregs <- paste("P4_6",1:6, sep="_")
medios_conexion <- MEDIOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Medio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Medio)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


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
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Medios de conexión a Internet en el hogar",
        subtitle = "Hogares con IDTMex medio",
        caption = nota,
        y="Medio", x="Hogares"
    )+
    scale_y_discrete(labels=medios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=MEDIOpath, filename= "MEDIO_medios_conexion.png",
    device="png", dpi="retina",
    width=30, height=18, units="cm"
)



##### MEDIO motivo no Internet ####


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

motivos_no_int <- MEDIOvivhog %>%
    group_by(P4_8)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))




ggplot(motivos_no_int, aes(y=as.factor(P4_8), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Motivos de la falta de conexión a Internet",
        subtitle = "Hogares con IDTMex medio",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=MEDIOpath, filename= "MEDIO_motivo_no_Internet.png",
    device="png", dpi="retina",
    width=30, height=18, units="cm"
)




##### MEDIO SERVICIOS ####

pregs <- paste("P5_6",1:5, sep="_")
servs <- MEDIOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Servicio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Servicio)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


servicios <- c(
    "P5_6_1" = "Internet",
    "P5_6_2" = "TV de paga",
    "P5_6_3" = "Telefonía fija",
    "P5_6_4" = "Telefonía móvil",
    "P5_6_5" = "Ninguno"
)


ggplot(servs, aes(y=Servicio, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Servicios disponibles en el hogar",
        subtitle = "Hogares con IDTMex medio",
        caption = nota,
        y="Servicios", x="Hogares"
    )+
    scale_y_discrete(labels=servicios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=MEDIOpath, filename= "MEDIO_servicios.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)



##### MEDIO PAQUETES SERVICIOS ####

pregs <- paste("P5_7",1:8, sep="_")

paquetes_serv <- MEDIOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Paquete",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Paquete)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


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
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Paquetes de servicios contratados en el hogar",
        subtitle = "Hogares con IDTMex medio",
        caption = nota,
        y="Paquete", x="Hogares"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=MEDIOpath, filename= "MEDIO_paquete_servicios.png",
    device="png", dpi="retina",
    width=30, height=18, units="cm"
)



##### MEDIO PAGO PAQUETES SERVICIOS ####

pregs <- paste("P5_8",1:8, sep="_")

pago_paquetes_serv <- MEDIOvivhog %>% 
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
        subtitle = "Hogares con IDTMex medio",
        caption = nota,
        y="Paquete", x="Monto (MXN)"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_dollar())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=MEDIOpath, filename= "MEDIO_pago_paquete_servicios.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)

##### MEDIO PISO####
materiales = c(
    "1" = "Tierra", 
    "2" = "Cemento o firme", 
    "3" = "Madera, mosaico u otro recubrimiento"
    
)


pisos <- MEDIOvivhog %>%
    group_by(P1_1)%>%
    dplyr::summarise(FAC_VIV=sum(FAC_VIV))



ggplot(pisos, aes(y=as.factor(P1_1), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Principal material del piso de la vivienda",
        subtitle = "Hogares con IDTMex medio",
        caption = nota,
        y="Material", x="Viviendas"
    )+
    scale_y_discrete(labels=materiales)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=MEDIOpath, filename= "MEDIO_material_piso.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)

##### MEDIO AGUA ####
agua = c(
    "1" = "Agua entubada dentro de la vivienda", 
    "2" = "Agua entubada fuera de la vivienda, pero dentro del terreno" ,
    "3" = "Agua entubada de llave pública (o hidrante)", 
    "4" = "Agua entubada que acarrean de otra vivienda",
    "5" = "Agua de pipa", 
    "6" = "Agua de un pozo, río, arroyo, lago u otro"
    
)


sit_agua <- MEDIOvivhog %>%
    group_by(P1_2)%>%
    dplyr::summarise(FAC_VIV=sum(FAC_VIV))

ggplot(sit_agua, aes(y=as.factor(P1_2), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Situación del agua potable en la vivienda",
        subtitle = "Hogares con IDTMex medio",
        caption = nota,
        y="Situación", x="Viviendas"
    )+
    scale_y_discrete(labels=agua)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=MEDIOpath, filename= "MEDIO_agua.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)



##### MEDIO DRENAJE ####
drenaje = c(
    "1" = "Red pública", 
    "2" = "Fosa séptica", 
    "3" = "Tubería que va a dar a una barranca o grieta", 
    "4" = "Tubería que va a dar a un río, lago o mar", 
    "5" =  "No tiene drenaje"
)


sit_drenaje <- MEDIOvivhog %>%
    group_by(P1_3)%>%
    dplyr::summarise(FAC_VIV=sum(FAC_VIV))

ggplot(sit_drenaje, aes(y=as.factor(P1_3), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(MEDIOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Conexión del drenaje en la vivienda",
        subtitle = "Hogares con IDTMex medio",
        caption = nota,
        y="Conexión", x="Viviendas"
    )+
    scale_y_discrete(labels=drenaje)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=MEDIOpath, filename= "MEDIO_drenaje.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)




#### ALTO DEFINICION ####

ALTOpath <- "./Graficas/CLAS/ALTO/"

ALTOres <- endutih_res %>% 
    inner_join(clasificacion) %>%
    filter((PAREN==1) & (clasificacion=="Alto"))
ALTOres$Grupo<- paste("Grupo", ALTOres$Grupo)
ALTOres$ENT <-sapply(ALTOres$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

ALTOvivhog <- inner_join(endutih_vivhogar, ALTOres[,c("UPM","VIV_SEL", "HOGAR")], by = c("UPM","VIV_SEL", "HOGAR")) %>%  distinct()
ALTOvivhog$Grupo<- paste("Grupo", ALTOvivhog$Grupo)
ALTOvivhog$ENT <-sapply(ALTOvivhog$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

ALTOusu <- inner_join(endutih_usu, ALTOres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
ALTOusu$Grupo<- paste("Grupo", ALTOusu$Grupo)
ALTOusu$ENT <-sapply(ALTOusu$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})


ALTOusu2 <- merge(endutih_usu2, ALTOres[,c("UPM","VIV_SEL", "HOGAR", "NUM_REN")], by = c("UPM","VIV_SEL", "HOGAR", "NUM_REN"))
ALTOusu2$Grupo<- paste("Grupo", ALTOusu2$Grupo)
ALTOusu2$ENT <-sapply(ALTOusu2$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})



##### ALTO PIRAMIDE ####


poblacion_sexo <-  ALTOres %>%
    group_by(rgo_edad,SEXO) %>%
    mutate(
        perc = FAC_HOGAR/sum(FAC_HOGAR)
    )%>%
    dplyr::summarise(
        FAC_HOGAR=sum(FAC_HOGAR),
        perc = sum(perc)
    )


abs_label_number <- function(x) { label_number()(abs(x)) }
fill_labs <- c(
    paste0("Masculino (",label_percent(accuracy = 0.01)(sum(poblacion_sexo[poblacion_sexo$SEXO==1,"perc"])),")"),
    paste0("Femenino (",label_percent(accuracy = 0.01)(sum(poblacion_sexo[poblacion_sexo$SEXO==2,"perc"])),")")
)


ggplot(poblacion_sexo, aes(x = rgo_edad, y = ifelse(SEXO == 1, -FAC_HOGAR, FAC_HOGAR), fill = as.factor(SEXO))) + 
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_y_continuous(n.breaks = 15, labels = abs_label_number) +
    labs(
        title = "Pirámide Poblacional de los jefes del hogar",
        subtitle = "Hogares con un IDTMex alto",
        x = "Rango de Edad",
        y = "Jefes",
        fill = "Sexo"
    ) +
    scale_fill_manual( values=c(masccolor, femcolor) ,labels= fill_labs)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))


ggsave(
    path=ALTOpath, filename= "ALTO_piramides.png",
    device="png", dpi="retina",
    width=25, height=14, units="cm"
)


##### ALTO EQUIPOS ####
equipos <- ALTOres[,c(paste0("P3_9_",1:3),"FAC_HOGAR")] %>%
    pivot_longer(-c(FAC_HOGAR), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    group_by(Equipo,respuesta)%>%
    dplyr::summarise(FAC_HOGAR=sum(FAC_HOGAR))



ggplot(equipos, aes(x = as.factor(Equipo), y = FAC_HOGAR, fill = as.factor(respuesta))) + 
    geom_bar(stat = "identity",position="stack")+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..y../sum(ALTOres$FAC_HOGAR))), fun = sum, 
                  geom = "text", hjust=0.5, vjust=2 ,color = "white", position = "stack" ) +
    labs(
        title = "Uso de principales TIC de los jefes del hogar",
        subtitle = "Hogares con un IDTMex alto",
        caption = nota,
        y="Jefes", x="Grupo", fill= "Uso"
    )+
    scale_x_discrete(labels=c("Computadora","Internet", "Celular"))+
    scale_y_continuous(n.breaks = 11, labels = label_number()) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= c("Sí", "NO","No sabe"))+
    theme_light()+
    theme()

ggsave(
    path=ALTOpath, filename= "ALTO_uso_equipos.png",
    device="png", dpi="retina",
    width=25, height=14, units="cm"
)


##### ALTO ACTIVIDAD LABORAL ####

actividades <- c(
    "1"="Trabajó","2"="No fue al trabajo","3"= "Buscó trabajo", 
    "4" = "Está jubilado","5"="Se dedicó a estudiar", "6"="Quehaceres del hogar", 
    "7"="Limitación para trabajar", "8"="No trabajó")

actividad <- ALTOres %>%
    group_by(P3_11)%>%
    dplyr::summarise(FAC_HOGAR=sum(FAC_HOGAR))

ggplot(actividad,aes(x = as.factor(P3_11), y = FAC_HOGAR)) + 
    geom_bar(stat = "identity", fill=main) +
    stat_summary( aes(label = label_percent(accuracy=0.01)(..y../sum(ALTOusu2$FAC_PER))), fun = sum, geom = "text", vjust= -0.05, color = "darkgray" ) +
    labs(
        title = "Actividad económica ",
        subtitle = "Hogares con un IDTMex alto",
        caption = nota,
        y="Jefes", x="Actividad laboral"
    )+
    scale_x_discrete(labels=actividades)+
    scale_y_continuous(labels = label_number(), n.breaks = 11)+
    theme_light()+
    theme()

ggsave(
    path=ALTOpath, filename= "ALTO_actividad_laboral.png",
    device="png", dpi="retina",
    width=25, height=14, units="cm"
)


##### ALTO TIPO DE CELULAR ####
pregs <- paste("P8_4", 1:2, sep="_")

tipo_cel <- ALTOusu2 %>% 
    select(all_of(c("FAC_PER", pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "tipo",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    filter(respuesta == 1)%>%
    group_by(tipo) %>% 
    dplyr::summarise(tot = sum(FAC_PER))

tipos <- c(
    "Común", "Inteligente"
)

#label_percent(accuracy=0.01)(tot/sum(ALTOusu2$FAC_PER))
ggplot(tipo_cel, mapping = aes(x = tipo, y = tot)) + 
    geom_bar(stat = "identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..y../sum(ALTOusu2$FAC_PER))), fun = sum, geom = "text", vjust= -0.05, color = "darkgray" ) +
    labs(
        title = "Tipo de celular de los jefes del hogar",
        subtitle = "Hogares con un IDTMex alto",
        caption = nota,
        y="Cantidad", x="Jefes"
    )+
    scale_x_discrete(labels=tipos)+
    scale_y_continuous(n.breaks = 11,labels = label_number()) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= tipos)+
    theme_light()+
    theme()


ggsave(
    path=ALTOpath, filename= "ALTO_tipo_cel.png",
    device="png", dpi="retina",
    height=18, width=25, units="cm"
)


##### ALTO TIPO DE CONTRATACION CELULAR ####
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- ALTOusu2 %>% 
    select(all_of(c("FAC_PER", pregs))) %>%
    mutate(
        Ninguno = 2 - as.numeric(rowSums(is.na(select(., all_of(pregs)))) == length(pregs) )
        
    )%>%
    pivot_longer(-c(FAC_PER), names_to = "Plan",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    mutate(
        Plan = factor(Plan, levels=c(pregs,"Ninguno"))
    ) %>%
    group_by(Plan)%>%
    dplyr::summarise(FAC_PER=sum(FAC_PER))


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOusu2$FAC_PER))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Hogares con un IDTMex alto",
        caption = nota,
        y="Planes", x="Jefes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 11, labels = label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=ALTOpath, filename= "ALTO_plan_cel.png",
    device="png", dpi="retina",
    height=18, width=25, units="cm"
)



##### ALTO USO DE APPS DE CELULAR ####
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- ALTOusu2 %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "Uso",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Uso)%>%
    dplyr::summarise(FAC_PER=sum(FAC_PER))

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOusu2$FAC_PER))), fun = sum, 
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Hogares con un IDTMex alto",
        caption = nota,
        y="Usos", x="Jefes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 11, labels=label_number())+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=ALTOpath, filename= "ALTO_uso_cel.png",
    device="png", dpi="retina",
    width=36, height=20, units="cm"
)



##### ALTO LUGARES INTERNET ####
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- ALTOusu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "Lugar",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Lugar)%>%
    dplyr::summarise(FAC_PER=sum(FAC_PER))


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOusu$FAC_PER))), fun = sum, 
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Hogares con un IDTMex alto",
        caption = nota,
        y="Lugares", x="Jefes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 11, labels=label_number())+
    theme_light()+
    theme()

ggsave(
    path=ALTOpath, filename= "ALTO_lugar_internet.png",
    device="png", dpi="retina",
    width=48, height=27, units="cm"
)


##### ALTO EQUIPO INTERNET ####
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- ALTOusu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-c(FAC_PER), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Equipo)%>%
    dplyr::summarise(FAC_PER=sum(FAC_PER))


equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOusu$FAC_PER))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Hogares con un IDTMex alto",
        caption = nota,
        y="Equipos", x="Jefes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=ALTOpath, filename= "ALTO_equipo_internet.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)


##### ALTO BIENES Y CONECTIVIDAD ####
pregs <- paste("P4_1",1:6, sep="_")
bienes <- ALTOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Bien",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    group_by(Bien)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


bienes_desc <- c(
    "Radio", "TV analógica", "TV digital", 
    "Pantalla plana", "Consola", "Celular"
)


ggplot(bienes, aes(y=Bien, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Disponibilidad de bienes y conectividad",
        subtitle = "Hogares con IDTMex alto",
        caption = nota,
        y="Bienes", x="Hogares"
    )+
    scale_y_discrete(labels=bienes_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=ALTOpath, filename= "ALTO_bienes_conectividad.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)


##### ALTO Disp computadora ####
pregs <- paste("P4_2",1:3, sep="_")
compus <- ALTOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Tipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Tipo)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


compus_desc <- c(
    "Ordenador","Laptop", "Tablet"
)


ggplot(compus, aes(y=Tipo, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Disponibilidad de computadora",
        subtitle = "Hogares con IDTMex alto",
        caption = nota,
        y="Tipo de computadora", x="Hogares"
    )+
    scale_y_discrete(labels=compus_desc)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=ALTOpath, filename= "ALTO_dis_compu.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)


##### ALTO motivo no compu ####


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

motivos_no_compu <- ALTOvivhog %>%
    group_by(P4_3)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


ggplot(motivos_no_compu, aes(y=as.factor(P4_3), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Motivos de la falta de una computadora (ordenador, laptop o tableta)",
        subtitle = "Hogares con IDTMex alto",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=ALTOpath, filename= "ALTO_motivo_no_compu.png",
    device="png", dpi="retina",
    width=30, height=18, units="cm"
)



##### ALTO MODO DE CONEXION ####

pregs <- paste("P4_6",1:6, sep="_")
medios_conexion <- ALTOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Medio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Medio)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


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
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Medios de conexión a Internet en el hogar",
        subtitle = "Hogares con IDTMex alto",
        caption = nota,
        y="Medio", x="Hogares"
    )+
    scale_y_discrete(labels=medios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=ALTOpath, filename= "ALTO_medios_conexion.png",
    device="png", dpi="retina",
    width=30, height=18, units="cm"
)



##### ALTO motivo no Internet ####


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

motivos_no_int <- ALTOvivhog %>%
    group_by(P4_8)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))




ggplot(motivos_no_int, aes(y=as.factor(P4_8), x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Motivos de la falta de conexión a Internet",
        subtitle = "Hogares con IDTMex alto",
        caption = nota,
        y="Motivo", x="Hogares"
    )+
    scale_y_discrete(labels=motivos)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=ALTOpath, filename= "ALTO_motivo_no_Internet.png",
    device="png", dpi="retina",
    width=30, height=18, units="cm"
)




##### ALTO SERVICIOS ####

pregs <- paste("P5_6",1:5, sep="_")
servs <- ALTOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Servicio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Servicio)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


servicios <- c(
    "P5_6_1" = "Internet",
    "P5_6_2" = "TV de paga",
    "P5_6_3" = "Telefonía fija",
    "P5_6_4" = "Telefonía móvil",
    "P5_6_5" = "Ninguno"
)


ggplot(servs, aes(y=Servicio, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Servicios disponibles en el hogar",
        subtitle = "Hogares con IDTMex alto",
        caption = nota,
        y="Servicios", x="Hogares"
    )+
    scale_y_discrete(labels=servicios)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=ALTOpath, filename= "ALTO_servicios.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)



##### ALTO PAQUETES SERVICIOS ####

pregs <- paste("P5_7",1:8, sep="_")

paquetes_serv <- ALTOvivhog %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-c(FAC_HOG), names_to = "Paquete",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    group_by(Paquete)%>%
    dplyr::summarise(FAC_HOG=sum(FAC_HOG))


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
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Paquetes de servicios contratados en el hogar",
        subtitle = "Hogares con IDTMex alto",
        caption = nota,
        y="Paquete", x="Hogares"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=ALTOpath, filename= "ALTO_paquete_servicios.png",
    device="png", dpi="retina",
    width=30, height=18, units="cm"
)



##### ALTO PAGO PAQUETES SERVICIOS ####

pregs <- paste("P5_8",1:8, sep="_")

pago_paquetes_serv <- ALTOvivhog %>% 
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
        subtitle = "Hogares con IDTMex alto",
        caption = nota,
        y="Paquete", x="Monto (MXN)"
    )+
    scale_y_discrete(labels=paquetes)+
    scale_x_continuous(n.breaks = 11, label=label_dollar())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=ALTOpath, filename= "ALTO_pago_paquete_servicios.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)

##### ALTO PISO####
materiales = c(
    "1" = "Tierra", 
    "2" = "Cemento o firme", 
    "3" = "Madera, mosaico u otro recubrimiento"
    
)


pisos <- ALTOvivhog %>%
    group_by(P1_1)%>%
    dplyr::summarise(FAC_VIV=sum(FAC_VIV))



ggplot(pisos, aes(y=as.factor(P1_1), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Principal material del piso de la vivienda",
        subtitle = "Hogares con IDTMex alto",
        caption = nota,
        y="Material", x="Viviendas"
    )+
    scale_y_discrete(labels=materiales)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=ALTOpath, filename= "ALTO_material_piso.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)

##### ALTO AGUA ####
agua = c(
    "1" = "Agua entubada dentro de la vivienda", 
    "2" = "Agua entubada fuera de la vivienda, pero dentro del terreno" ,
    "3" = "Agua entubada de llave pública (o hidrante)", 
    "4" = "Agua entubada que acarrean de otra vivienda",
    "5" = "Agua de pipa", 
    "6" = "Agua de un pozo, río, arroyo, lago u otro"
    
)


sit_agua <- ALTOvivhog %>%
    group_by(P1_2)%>%
    dplyr::summarise(FAC_VIV=sum(FAC_VIV))

ggplot(sit_agua, aes(y=as.factor(P1_2), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Situación del agua potable en la vivienda",
        subtitle = "Hogares con IDTMex alto",
        caption = nota,
        y="Situación", x="Viviendas"
    )+
    scale_y_discrete(labels=agua)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=ALTOpath, filename= "ALTO_agua.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)



##### ALTO DRENAJE ####
drenaje = c(
    "1" = "Red pública", 
    "2" = "Fosa séptica", 
    "3" = "Tubería que va a dar a una barranca o grieta", 
    "4" = "Tubería que va a dar a un río, lago o mar", 
    "5" =  "No tiene drenaje"
)


sit_drenaje <- ALTOvivhog %>%
    group_by(P1_3)%>%
    dplyr::summarise(FAC_VIV=sum(FAC_VIV))

ggplot(sit_drenaje, aes(y=as.factor(P1_3), x=FAC_VIV))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(..x../sum(ALTOvivhog$FAC_HOG))), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    labs(
        title = "Conexión del drenaje en la vivienda",
        subtitle = "Hogares con IDTMex alto",
        caption = nota,
        y="Conexión", x="Viviendas"
    )+
    scale_y_discrete(labels=drenaje)+
    scale_x_continuous(n.breaks = 11, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=ALTOpath, filename= "ALTO_drenaje.png",
    device="png", dpi="retina",
    width=25, height=18, units="cm"
)






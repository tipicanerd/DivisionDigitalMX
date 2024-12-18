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

path <- "./Graficas/CLAS/BAJO/"

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


#### BAJO EQUIPOS ####
equipos <- BAJOres[,c(paste0("P3_9_",1:3),"FAC_HOGAR")] %>%
    pivot_longer(-c(FAC_HOGAR), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE)
    


ggplot(equipos, aes(x = as.factor(Equipo), y = FAC_HOGAR, fill = as.factor(respuesta))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Uso de principales TIC de los jefes del hogar",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="% Jefes", x="Grupo", fill= "Uso"
    )+
    scale_x_discrete(labels=c("Computadora","Internet", "Celular"))+
    scale_y_continuous(breaks = 10*(1:10)/100, labels = 10*(1:10)) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= c("Sí", "NO","No sabe"))+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_uso_equipos.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#### BAJO ACTIVIDAD LABORAL ####

actividad <- c(
    "1"="Trabajó","2"="No fue al trabajo","3"= "Buscó trabajo", 
    "4" = "Está jubilado","5"="Se dedicó a estudiar", "6"="Quehaceres del hogar", 
    "7"="Limitación para trabajar", "8"="No trabajó")

ggplot(BAJOres) + 
    geom_bar(aes(x = as.factor(P3_11), y = FAC_HOGAR),stat = "identity", color=main) +
    labs(
        title = "Actividad económica ",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Jefes", x="Actividad laboral"
    )+
    scale_x_discrete(labels=actividad)+
    scale_y_continuous(labels = label_number())+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_actividad_laboral.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#### BAJO TIPO DE CELULAR ####
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


ggplot(tipo_cel, mapping = aes(x = tipo, y = tot)) + 
    geom_bar(stat = "identity", fill=main)+
    labs(
        title = "Tipo de celular de los jefes del hogar",
        subtitle = "Hogares con IDTMex bajo",
        caption = nota,
        y="Cantidad", x="Jefes"
    )+
    scale_x_discrete(labels=tipos)+
    scale_y_continuous(n.breaks = 11,labels = label_number()) +
    scale_fill_manual( values=c(sicolor,nocolor,nosabecolor) ,labels= tipos)+
    theme_light()+
    theme()


ggsave(
    path=path, filename= "BAJO_tipo_cel.pdf",
    device="pdf", dpi="retina",
    height=18, width=25, units="cm"
)


#### BAJO TIPO DE CONTRATACION CELULAR ####
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- BAJOusu2 %>% 
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
        subtitle = "Población con IDTMex bajo",
        caption = nota,
        y="Planes", x="Jefes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10, labels = label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "BAJO_plan_cel.pdf",
    device="pdf", dpi="retina",
    height=18, width=25, units="cm"
)



#### BAJO USO DE APPS DE CELULAR ####
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- BAJOusu2 %>% 
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
        subtitle = "Población con IDTMex bajo",
        caption = nota,
        y="Usos", x="Jefes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10, labels=label_number())+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "BAJO_uso_cel.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)



##### BAJO LUGARES INTERNET ####
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- BAJOusu %>% 
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
        subtitle = "Población con IDTMex bajo",
        caption = nota,
        y="Lugares", x="Jefes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10, labels=label_number())+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)


### BAJO EQUIPO INTERNET ####
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- BAJOusu %>% 
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
        subtitle = "Población con IDTMex bajo",
        caption = nota,
        y="Equipos", x="Jefes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10, label=label_number())+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "BAJO_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=18, height=25, units="cm"
)



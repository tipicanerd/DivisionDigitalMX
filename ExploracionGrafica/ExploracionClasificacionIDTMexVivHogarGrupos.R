#install.packages("pacman")
library(pacman)

p_load(
    dplyr,
    ggExtra,
    ggstats,
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
base_bajo_path <- "../ConjuntosDatos/"
endutih_bajo_path <- paste(base_bajo_path,"ENDUTIH2020/conjuntos_de_datos/", sep="")

#HOGARES Y VIVIENDA
endutih_hogar <- read.csv(
    paste0(endutih_bajo_path,"tr_endutih_hogar_anual_2020.csv"),
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
            paste(endutih_bajo_path,"viv_hog_agrupados.csv", sep=""), 
            stringsAsFactors = TRUE
        ), endutih_hogar[,c("UPM","VIV_SEL", "HOGAR", cols_eliminadas)], 
        by = c("UPM","VIV_SEL", "HOGAR")
        )



#RESIDENTES
endutih_res <- read.csv(
    paste(endutih_bajo_path,"residente_agrupados.csv", sep=""),
    stringsAsFactors = TRUE
    )


#USUARIOS
endutih_usu <- read.csv(
    paste(endutih_bajo_path,"usuario_agrupados.csv", sep=""),
    stringsAsFactors = TRUE
)

endutih_usu2 <- read.csv(
    paste(endutih_bajo_path,"usuario2_agrupados.csv", sep=""),
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

bajo_path <- "./Graficas/CLAS/BAJO/"

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




##### BAJO EQUIPOS ####
agrupado <- BAJOres[,c(paste0("P3_9_",1:3),"FAC_HOGAR", "Grupo")] %>%
    pivot_longer(-c(FAC_HOGAR, Grupo), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    group_by(Grupo,Equipo) %>%
    dplyr::summarise(
        tot = sum(FAC_HOGAR)
    )


equipos <- BAJOres[,c(paste0("P3_9_",1:3),"FAC_HOGAR", "Grupo")] %>%
    pivot_longer(-c(FAC_HOGAR, Grupo), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_HOGAR/tot) %>%
    group_by(Grupo,Equipo,respuesta) %>%
    dplyr::summarise(
        FAC_HOGAR = sum(FAC_HOGAR),
        perc = sum(perc)
    )



ggplot(equipos, aes(x = as.factor(Equipo), y = FAC_HOGAR, fill = as.factor(respuesta))) + 
    geom_bar(stat = "identity",position="stack")+
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_y")+
    geom_text(aes(label = scales::percent(perc, accuracy = 0.01)), 
              position = position_stack(vjust = 0.5), 
              color = "white", size = 3) +
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
    path=bajo_path, filename= "BAJO_uso_equiposGrupos.png",
    device="png", dpi="retina",
    width=25*2, height=14*2, units="cm"
)


##### BAJO ACTIVIDAD LABORAL ####

actividades <- c(
    "1"="Trabajó","2"="No fue al trabajo","3"= "Buscó trabajo", 
    "4" = "Está jubilado","5"="Se dedicó a estudiar", "6"="Quehaceres del hogar", 
    "7"="Limitación para trabajar", "8"="No trabajó")

agrupado <- BAJOres %>%
    group_by(Grupo) %>%
    dplyr::summarise(
        tot = sum(FAC_HOGAR)
    )


actividad <- BAJOres%>%
    left_join(agrupado) %>%
    mutate(perc=FAC_HOGAR/tot) %>%
    group_by(Grupo,P3_11) %>%
    dplyr::summarise(
        FAC_HOGAR = sum(FAC_HOGAR),
        perc = sum(perc)
    )


ggplot(actividad,aes(x = as.factor(P3_11), y = FAC_HOGAR)) + 
    geom_bar(stat = "identity", fill=main) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_y")+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), 
                  fun = sum, geom = "text", vjust= -0.05, color = "darkgray" ) +
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
    path=bajo_path, filename= "BAJO_actividad_laboralGrupos.png",
    device="png", dpi="retina",
    width=25*2, height=14*2, units="cm"
)


##### BAJO TIPO DE CELULAR ####
pregs <- paste("P8_4", 1:2, sep="_")


agrupado <- BAJOusu2 %>%
    select(all_of(c("FAC_PER","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER,Grupo), names_to = "tipo",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    group_by(Grupo,tipo) %>%
    dplyr::summarise(
        tot = sum(FAC_PER)
    )


tipo_cel <- BAJOusu2 %>% 
    select(all_of(c("FAC_PER","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER,Grupo), names_to = "tipo",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    filter(respuesta == 1)%>%
    left_join(agrupado) %>%
    mutate(perc=FAC_PER/tot) %>%
    group_by(tipo,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_PER), perc = sum(perc))

tipos <- c(
    "Común", "Inteligente"
)

ggplot(tipo_cel, mapping = aes(x = tipo, y = tot)) + 
    geom_bar(stat = "identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum, 
                      geom = "text", vjust= -0.05, color = "darkgray" ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_y")+
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
    path=bajo_path, filename= "BAJO_tipo_celGrupos.png",
    device="png", dpi="retina",
    height=18*2, width=25*2, units="cm"
)


##### BAJO TIPO DE CONTRATACION CELULAR ####
pregs <- paste("P8_7", 1:3, sep="_")

agrupado <- BAJOusu2 %>%
    select(all_of(c("FAC_PER","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER,Grupo), names_to = "Plan",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    group_by(Grupo,Plan) %>%
    dplyr::summarise(
        tot = sum(FAC_PER)
    )



plan_cel <- BAJOusu2 %>% 
    select(all_of(c("FAC_PER", "Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER,Grupo), names_to = "Plan",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_PER/tot) %>%
    group_by(Plan,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_PER), perc = sum(perc))


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_plan_celGrupos.png",
    device="png", dpi="retina",
    height=18*2, width=25*2, units="cm"
)





##### BAJO USO DE APPS DE CELULAR ####

pregs <- paste("P8_12", 1:9, sep="_")

agrupado <- BAJOusu2 %>%
    select(all_of(c("FAC_PER","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER,Grupo), names_to = "Uso",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    group_by(Grupo,Uso) %>%
    dplyr::summarise(
        tot = sum(FAC_PER)
    )


uso_cel <- BAJOusu2 %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Uso",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_PER/tot) %>%
    group_by(Uso,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_PER), perc = sum(perc))

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum, 
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_uso_celGrupos.png",
    device="png", dpi="retina",
    width=36*2, height=20*2, units="cm"
)



##### BAJO LUGARES INTERNET ####

pregs <- paste("P7_7",1:8, sep="_")

agrupado <- BAJOusu %>%
    select(all_of(c("FAC_PER","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER,Grupo), names_to = "Lugar",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    group_by(Grupo,Lugar) %>%
    dplyr::summarise(
        tot = sum(FAC_PER)
    )



lugar_internet <- BAJOusu %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Lugar",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_PER/tot) %>%
    group_by(Lugar,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_PER), perc = sum(perc))


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum, 
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales="free_x")+
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
    path=bajo_path, filename= "BAJO_lugar_internetGrupos.png",
    device="png", dpi="retina",
    width=48*2, height=27*2, units="cm"
)


##### BAJO EQUIPO INTERNET ####
pregs <- paste("P7_5",1:7, sep="_")

agrupado <- BAJOusu %>%
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    group_by(Equipo,Grupo) %>%
    dplyr::summarise(
        tot = sum(FAC_PER)
    )



#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- BAJOusu %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Equipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_PER/tot) %>%
    group_by(Equipo,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_PER), perc = sum(perc))


equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum,
                  geom = "text", hjust= 1, color = lmain) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_equipo_internetGrupos.png",
    device="png", dpi="retina",
    width=25*2, height=18*2, units="cm"
)


##### BAJO BIENES Y CONECTIVIDAD ####
pregs <- paste("P4_1",1:6, sep="_")

agrupado <- BAJOvivhog %>%
    select(all_of(c("FAC_HOG","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_HOG,Grupo), names_to = "Bien",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    group_by(Grupo,Bien) %>%
    dplyr::summarise(
        tot = sum(FAC_HOG)
)



bienes <- BAJOvivhog %>% 
    select(all_of(c("FAC_HOG", "Grupo",pregs))) %>%
    pivot_longer(-c(FAC_HOG, Grupo), names_to = "Bien",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_HOG/tot) %>%
    group_by(Bien,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_HOG), perc = sum(perc))


bienes_desc <- c(
    "Radio", "TV analógica", "TV digital", 
    "Pantalla plana", "Consola", "Celular"
)


ggplot(bienes, aes(y=Bien, x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_bienes_conectividadGrupos.png",
    device="png", dpi="retina",
    width=25*2, height=18*2, units="cm"
)


##### BAJO Disp computadora ####
pregs <- paste("P4_2",1:3, sep="_")


agrupado <- BAJOvivhog %>%
    select(all_of(c("FAC_HOG","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_HOG,Grupo), names_to = "Tipo",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    group_by(Grupo,Tipo) %>%
    dplyr::summarise(
        tot = sum(FAC_HOG)
    )


compus <- BAJOvivhog %>% 
    select(all_of(c("FAC_HOG","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_HOG, Grupo), names_to = "Tipo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_HOG/tot) %>%
    group_by(Tipo,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_HOG), perc = sum(perc))


compus_desc <- c(
    "Ordenador","Laptop", "Tablet"
)


ggplot(compus, aes(y=Tipo, x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_dis_compuGrupos.png",
    device="png", dpi="retina",
    width=25*2, height=18*2, units="cm"
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


agrupado <- BAJOvivhog %>%
    group_by(Grupo) %>%
    dplyr::summarise(
        tot = sum(FAC_HOG)
    )

motivos_no_compu <- BAJOvivhog  %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_HOG/tot) %>%
    group_by(P4_3,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_HOG), perc = sum(perc))


ggplot(motivos_no_compu, aes(y=as.factor(P4_3), x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_motivo_no_compuGrupos.png",
    device="png", dpi="retina",
    width=30*2, height=18*2, units="cm"
)



##### BAJO MODO DE CONEXION ####

pregs <- paste("P4_6",1:6, sep="_")

agrupado <- BAJOvivhog %>%
    select(all_of(c("FAC_HOG","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_HOG,Grupo), names_to = "Medio",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    group_by(Grupo,Medio) %>%
    dplyr::summarise(
        tot = sum(FAC_HOG)
    )




medios_conexion <- BAJOvivhog %>% 
    select(all_of(c("FAC_HOG","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_HOG,Grupo), names_to = "Medio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_HOG/tot) %>%
    group_by(Medio,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_HOG), perc = sum(perc))


medios <- c(
    "P4_6_1" = "Línea telefónica",
    "P4_6_2" = "Internet por cable",
    "P4_6_3" = "Conexión satelital ",
    "P4_6_4" = "Señal abierta de WIFI",
    "P4_6_5" = "Línea telefónica por marcación",
    "P4_6_6" = "Otro medio"
)


ggplot(medios_conexion, aes(y=Medio, x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_medios_conexionGrupos.png",
    device="png", dpi="retina",
    width=30*2, height=18*2, units="cm"
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


agrupado <- BAJOvivhog %>%
    group_by(Grupo) %>%
    dplyr::summarise(
        tot = sum(FAC_HOG)
    )

motivos_no_int <- BAJOvivhog  %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_HOG/tot) %>%
    group_by(P4_8,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_HOG), perc = sum(perc))



ggplot(motivos_no_int, aes(y=as.factor(P4_8), x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_motivo_no_InternetGrupos.png",
    device="png", dpi="retina",
    width=30*2, height=18*2, units="cm"
)




##### BAJO SERVICIOS ####

pregs <- paste("P5_6",1:5, sep="_")


agrupado <- BAJOvivhog %>%
    select(all_of(c("FAC_HOG","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_HOG,Grupo), names_to = "Servicio",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    group_by(Grupo,Servicio) %>%
    dplyr::summarise(
        tot = sum(FAC_HOG)
    )


servs <- BAJOvivhog %>% 
    select(all_of(c("FAC_HOG", "Grupo",pregs))) %>%
    pivot_longer(-c(FAC_HOG,Grupo), names_to = "Servicio",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_HOG/tot) %>%
    group_by(Servicio,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_HOG), perc = sum(perc))


servicios <- c(
    "P5_6_1" = "Internet",
    "P5_6_2" = "TV de paga",
    "P5_6_3" = "Telefonía fija",
    "P5_6_4" = "Telefonía móvil",
    "P5_6_5" = "Ninguno"
)


ggplot(servs, aes(y=Servicio, x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_serviciosGrupos.png",
    device="png", dpi="retina",
    width=25*2, height=18*2, units="cm"
)



##### BAJO PAQUETES SERVICIOS ####

pregs <- paste("P5_7",1:8, sep="_")


agrupado <- BAJOvivhog %>%
    select(all_of(c("FAC_HOG","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_HOG,Grupo), names_to = "Paquete",  values_to = "respuesta", values_drop_na = TRUE) %>% 
    group_by(Grupo,Paquete) %>%
    dplyr::summarise(
        tot = sum(FAC_HOG)
    )

paquetes_serv <- BAJOvivhog %>% 
    select(all_of(c("FAC_HOG","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_HOG,Grupo), names_to = "Paquete",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)  %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_HOG/tot) %>%
    group_by(Paquete,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_HOG), perc = sum(perc))


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


ggplot(paquetes_serv, aes(y=Paquete, x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_paquete_serviciosGrupos.png",
    device="png", dpi="retina",
    width=30*2, height=18*2, units="cm"
)



##### BAJO PAGO PAQUETES SERVICIOS ####

pregs <- paste("P5_8",1:8, sep="_")

pago_paquetes_serv <- BAJOvivhog %>% 
    select(all_of(c("FAC_HOG","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_HOG,Grupo), names_to = "Paquete",  values_to = "monto", values_drop_na = TRUE) %>%
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
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_pago_paquete_serviciosGrupos.png",
    device="png", dpi="retina",
    width=25*2, height=18*2, units="cm"
)

##### BAJO PISO####
materiales = c(
    "1" = "Tierra", 
    "2" = "Cemento o firme", 
    "3" = "Madera, mosaico u otro recubrimiento"
    
)


agrupado <- BAJOvivhog %>%
    group_by(Grupo) %>%
    dplyr::summarise(
        tot = sum(FAC_HOG)
    )

pisos <- BAJOvivhog  %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_HOG/tot) %>%
    group_by(P1_1,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_HOG), perc = sum(perc))



ggplot(pisos, aes(y=as.factor(P1_1), x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_material_pisoGrupos.png",
    device="png", dpi="retina",
    width=25*2, height=18*2, units="cm"
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


agrupado <- BAJOvivhog %>%
    group_by(Grupo) %>%
    dplyr::summarise(
        tot = sum(FAC_HOG)
    )

sit_agua <- BAJOvivhog  %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_HOG/tot) %>%
    group_by(P1_2,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_HOG), perc = sum(perc))


ggplot(sit_agua, aes(y=as.factor(P1_2), x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_aguaGrupos.png",
    device="png", dpi="retina",
    width=25*2, height=18*2, units="cm"
)



##### BAJO DRENAJE ####
drenaje = c(
    "1" = "Red pública", 
    "2" = "Fosa séptica", 
    "3" = "Tubería que va a dar a una barranca o grieta", 
    "4" = "Tubería que va a dar a un río, lago o mar", 
    "5" =  "No tiene drenaje"
)


agrupado <- BAJOvivhog %>%
    group_by(Grupo) %>%
    dplyr::summarise(
        tot = sum(FAC_HOG)
    )

sit_drenaje <- BAJOvivhog  %>%
    left_join(agrupado) %>%
    mutate(perc=FAC_HOG/tot) %>%
    group_by(P1_3,Grupo) %>% 
    dplyr::summarise(tot = sum(FAC_HOG), perc = sum(perc))



ggplot(sit_drenaje, aes(y=as.factor(P1_3), x=tot))+
    geom_bar(stat="identity", fill=main)+
    stat_summary( aes(label = label_percent(accuracy=0.01)(perc)), fun = sum,
                  geom = "text", hjust= 1, color = lmain ) +
    facet_wrap(vars(Grupo), nrow = 2, scales = "free_x")+
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
    path=bajo_path, filename= "BAJO_drenajeGrupos.png",
    device="png", dpi="retina",
    width=25*2, height=18*2, units="cm"
)


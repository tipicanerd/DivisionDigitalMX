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
    reshape2
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


#Variables universales
nota <- "Nota: Elaboración propia con datos de la ENDUTIH (2020)"
#colores
main <- "dodgerblue2"
lmain <- "dodgerblue4"

lcolor <- "royalblue4"
fcolor <- "royalblue"

nocolor <- "indianred3"
sicolor <- "steelblue2"

palette<-"ggthemes::excel_Gallery"
cpalette <- "ggthemes::Classic Blue"

#### NACIONAL ####

path <- "./Graficas/NACIONAL/"

# INTERNET

##Dominio-Estrato
estratos <- c("1"="Bajo", "2"="Medio Bajo", "3"="Medio Alto", "4"="Alto")
dominios <- c("R"="Rural", "U"="Urbano")

dom_est_internet <- read.csv(paste(base_path,"estrato_dominio_estado_internet.csv", sep="")) %>%
    pivot_longer(-c(ESTRATO,DOMINIO), values_to = "Internet", names_to = "Entidad") %>%
    filter(Internet>0) %>%
    mutate(
        combo = paste(unlist(lapply(DOMINIO, function(x){dominios[x]})),unlist(lapply(ESTRATO, function(x){estratos[x]})), sep="-")
    )

ggplot(dom_est_internet, aes(x=Entidad,y=combo, fill=Internet))+
    geom_tile()+
    coord_equal()+
    labs(
        title = "Porcentaje de hogares con Internet por entidad",
        subtitle = "Según su dominio y estrato socioeconómico",
        caption = nota,
        y = "Dominio-Estrato", x = "Entidad", fill="% Internet"
        
    )+
    scale_fill_paletteer_c("ggthemes::Classic Blue")+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90),
    )

ggsave(
    path=path, filename= "NACIONALdominio_estrato_entidad_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

##Educacion
niveles <- c("Ninguno", "Preescolar", "Primaria", "Secundaria","Normal básica", "Estudio técnico", "Preparatoria", "Estudio técnico superior", "Licenciatura o ingeniería", "Especialidad", "Maestría", "Doctorado", "No sabe")

edu_internet <- read.csv(paste(base_path,"edu_estado_internet.csv", sep="")) %>%
    pivot_longer(-NIVEL, values_to = "Internet", names_to = "Entidad") %>%
    filter(Internet>0)

ggplot(edu_internet, aes(x=Entidad,y=factor(NIVEL), fill=Internet))+
    geom_tile()+
    coord_equal()+
    labs(
        title = "Porcentaje de usuarios de Internet por entidad",
        subtitle = "Según su último nivel de estudios",
        caption = nota,
        y = "Nivel", x = "Entidad", fill="% Internet"
        
    )+
    scale_y_discrete(labels=niveles)+
    scale_fill_paletteer_c("ggthemes::Classic Blue")+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90),
    )

ggsave(
    path=path, filename= "NACIONALedu_entidad_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


# IDTMEX

##Dominio-Estrato
estratos <- c("1"="Bajo", "2"="Medio Bajo", "3"="Medio Alto", "4"="Alto")
dominios <- c("R"="Rural", "U"="Urbano")

dom_est_idtmex <- read.csv(paste(base_path,"estrato_dominio_estado_IDTMex.csv", sep="")) %>%
    pivot_longer(-c(ESTRATO,DOMINIO), values_to = "IDTMex", names_to = "Entidad") %>%
    filter(IDTMex>0) %>%
    mutate(
        combo = paste(unlist(lapply(dom_est_idtmex$DOMINIO, function(x){dominios[x]})),unlist(lapply(dom_est_idtmex$ESTRATO, function(x){estratos[x]})), sep="-")
    )

ggplot(dom_est_idtmex, aes(x=Entidad,y=combo, fill=IDTMex))+
    geom_tile()+
    coord_equal()+
    labs(
        title = "Valor del IDTMex por entidad",
        subtitle = "Según su dominio y estrato socioeconómico",
        caption = nota,
        y = "Dominio-Estrato", x = "Entidad", fill="IDTMex"
        
    )+
    scale_fill_paletteer_c("ggthemes::Classic Blue")+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90),
    )

ggsave(
    path=path, filename= "NACIONALdominio_estrato_entidad_IDTMex.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

##Educacion
niveles <- c("Ninguno", "Preescolar", "Primaria", "Secundaria","Normal básica", "Estudio técnico", "Preparatoria", "Estudio técnico superior", "Licenciatura o ingeniería", "Especialidad", "Maestría", "Doctorado", "No sabe")

edu_idtmex <- read.csv(paste(base_path,"edu_estado_IDTMex.csv", sep="")) %>%
    pivot_longer(-NIVEL, values_to = "IDTMex", names_to = "Entidad") %>%
    filter(IDTMex>0)

ggplot(edu_idtmex, aes(x=Entidad,y=factor(NIVEL), fill=IDTMex))+
    geom_tile()+
    coord_equal()+
    labs(
        title = "Valor del IDTMex por entidad",
        subtitle = "Según su último nivel de estudios",
        caption = nota,
        y = "Nivel", x = "Entidad", fill="IDTMex"
        
    )+
    scale_y_discrete(labels=niveles)+
    scale_fill_paletteer_c("ggthemes::Classic Blue")+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90),
    )

ggsave(
    path=path, filename= "NACIONALedu_entidad_IDTMex.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


# PISOS

pisos <- c("1"="Tierra","2"="Cemento, firme", "3"="Madera,mosaico,\notro")

ggplot(endutih_vivhogar, mapping = aes(y=factor(P1_1),weight=FAC_VIV), )+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),fill=main, color=lmain)+
    labs(
        title = "Material de piso de las viviendas",
        subtitle = "Nacional",
        caption = nota,
        y="Material", x="% Viviendas"
    )+
    scale_x_continuous(n.breaks = 10)+
    scale_y_discrete(
        labels=pisos
    )+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path = path,
    filename= "NACIONAL_material_piso.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# EDAD 1+ 3.322*log10(sum(endutih_res$FAC_HOGAR))
ggplot(endutih_res, aes(EDAD, weight=FAC_HOGAR))+
    geom_histogram(aes(y=after_stat(density)*(100/27)*100), color=lmain, fill=main, bins = 28)+
    geom_vline( xintercept =sum(endutih_res$FAC_HOGAR*endutih_res$EDAD)/sum(endutih_res$FAC_HOGAR), color=lmain)+
    labs(
        title = "Distribución de edad",
        subtitle = "Nacional",
        caption = nota,
        x="Edad", y="% Residentes"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 15)+
    theme_light()+
    theme()

ggsave(
    path = path,
    filename= "NACIONAL_hist_edad.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#MODALIDAD INTERNET
pregs <- paste("P5_7",1:8, sep="_")
modalidad_servicio <- endutih_vivhogar %>% 
    filter(P5_6_5!=1) %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-FAC_HOG, names_to = "Modalidad",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

modalidades <- c(
    "TV de paga, telefonía fija,\nInternet y telefonía móvil",
    "TV de paga, telefonía fija\ne Internet",
    "TV de paga y telefonía fija",
    "TV de paga e Internet",
    "Telefonía fija e Internet",
    "Solo TV de paga",
    "Solo telefonía fija",
    "Solo Internet"
)

ggplot(modalidad_servicio, aes(y=Modalidad, weight=FAC_HOG))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Modalidad de contratación de servicios",
        subtitle = "Nacional",
        caption = nota,
        y="Modalidad", x="% Hogares"
    )+
    scale_y_discrete(labels=modalidades)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme()

ggsave(
    path = path,
    filename= "NACIONAL_modalidad_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#Aprendizaje compu
pregs <- paste("P6_6",1:7, sep="_")
aprendizaje_compu <- endutih_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Metodo",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

modos <- c("Cuenta propia", "Trabajo", "Escuela", "Cursos pagados", "Cursos gratuitos", "Parientes", "Otro")

ggplot(aprendizaje_compu, aes(y=Metodo, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Modo de aprendizaje para el uso de computadora",
        subtitle = "Nacional",
        caption = nota,
        y="Modo", x="% Residentes"
    )+
    scale_y_discrete(labels=modos)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme()

ggsave(
    path = path,
    filename= "NACIONAL_modo_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#Lugares
pregs <- paste("P6_7",1:8, sep="_")

lugares_compu <- endutih_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Lugar",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

lugares <- c(
    "Hogar", "Trabajo", "Escuela",
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra persona", "Cualquier otro lugar\ncon una laptop",
    "Otro"
)


ggplot(lugares_compu, aes(y=Lugar, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Lugares de uso de computadora",
        subtitle = "NACIONAL",
        caption = nota,
        y="Lugar", x="% Residentes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path = path,
    filename= "NACIONAL_lugar_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#Habilidades
pregs <- paste("P6_8",1:10, sep="_")
habilidades_compu <- endutih_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Habilidad",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

habilidades <- c(
    "Enviar y recibir\ncorreo", "Descargar contenidos\nde Internet",
    "Copiar archivos entre directorios", "Crear archivos de texto",
    "Crear hojas de cáculo", "Crear presentaciones",
    "Instalar dispositivos\nperiféricos", "Crear o usar\nbases de datos",
    "Programar en lenguaje\nespecializado", "Otras"
)


ggplot(habilidades_compu, aes(y=Habilidad, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)), color=lmain, fill=main)+
    labs(
        title = "Habilidades con la computadora",
        subtitle = "Nacional",
        caption = nota,
        y="Habilidad", x="% Residentes"
    )+
    scale_y_discrete(labels=habilidades)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path,
    filename= "NACIONAL_habilidad_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#Uso
pregs <- paste("P6_9",1:6, sep="_")
usos_compu <- endutih_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Uso",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

usos <- c(
    "Actividades laborales", "Labores escolares",
    "Medio de capacitación", "Entretenimiento",
    "Acceso a Internet", "Otro"
)


ggplot(usos_compu, aes(y=Uso, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Usos de la computadora",
        subtitle = "Nacional",
        caption = nota,
        y="Usos", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path,
    filename= "NACIONAL_uso_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# INTENSIDAD DE USO DE INTERNET
ggplot(endutih_usu %>% filter(P7_1==1), aes(P7_4, weight=FAC_PER))+
    geom_histogram(aes(y=after_stat(density)*100),color=lmain, fill=main, bins = 12)+
    labs(
        title = "Distribución de la intensidad de uso de Internet",
        subtitle = "Nacional",
        caption = nota,
        x="Horas de uso en un día", y="% Residentes"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme()

ggsave(
    path=path,
    filename= "NACIONAL_hist_intensidad_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#EQUIPO INTERNET
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- endutih_usu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Equipo",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Nacional",
        caption = nota,
        y="Equipos", x="% Residentes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path,
    filename= "NACIONAL_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#LUGARES INTERNET
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- endutih_usu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Lugar",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)), color=lmain, fill=main)+
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Nacional",
        caption = nota,
        y="Lugares", x="% Residentes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "NACIONAL_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


# PROBLEMAS INTERNET
pregs <- paste("P7_16",1:8, sep="_")

problema_internet <- endutih_usu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Problema",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


problemas <- c(
    "Infección por virus", "Exceso de información\nno deseada",
    "Interrupciones en el servicio", "Lentitud en la transferencia\nde información",
    "Fraudes con información", "Violación a la privacidad",
    "Mensajes de personas desconocidas", "Otro"
)


ggplot(problema_internet, aes(y=Problema, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)), color=lmain, fill=main)+
    labs(
        title = "Problemas de navegación en Internet",
        subtitle = "Nacional",
        caption = nota,
        y="Lugares", x="% Residentes"
    )+
    scale_y_discrete(labels=problemas)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "NACIONAL_problema_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# TIPO DE CONTRATACION CELULAR
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- endutih_usu2 %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Plan",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)), color=lmain, fill=main)+
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Nacional",
        caption = nota,
        y="Planes", x="% Residentes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "NACIONAL_plan_cel.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# USO DE APPS DE CELULAR
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- endutih_usu2 %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Uso",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)), color=lmain, fill=main)+
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Nacional",
        caption = nota,
        y="Planes", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "NACIONAL_uso_cel.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#-----------------------------------------------------------#

#### TODOS LOS ESTRATOS ####
path = "./Graficas/GRUPOS/"

# DISTRIBUCION EN LOS ESTADOS
## Relativo a grupos
grupos_estado_rel_grupo <- dcast(endutih_vivhogar, Grupo~ENT, fun.aggregate = sum, value.var = "FAC_HOG")
grupos_estado_rel_grupo[,2:33] <- grupos_estado_rel_grupo[,2:33] / rowSums(grupos_estado_rel_grupo[,2:33])
grupos_estado_rel_grupo <- grupos_estado_rel_grupo %>% 
    pivot_longer(-Grupo, names_to = "ENT", values_to = "FAC_HOG") %>%
    filter(FAC_HOG>0)

ggplot(grupos_estado_rel_grupo, aes(as.character(ENT),as.character(Grupo), fill=FAC_HOG*100))+
    geom_tile()+
    coord_equal()+
    labs(
        title = "Distribución de grupos en las entidades",
        subtitle = "Relativo a los grupos",
        caption = nota,
        y = "Grupo", x = "Entidad", fill="%"
        
    )+
    scale_x_discrete(labels=estados$ENT)+
    scale_fill_paletteer_c("ggthemes::Classic Blue")+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90),
    )

ggsave(
    path=path, filename= "distribucion_entidad_grupos.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

## Relativo a estados
grupos_estado_rel_edo <- dcast(endutih_vivhogar, Grupo~ENT, fun.aggregate = sum, value.var = "FAC_HOG")
grupos_estado_rel_edo[,2:33] <- t(t(grupos_estado_rel_edo[,2:33])/colSums(grupos_estado_rel_edo[,2:33]))
grupos_estado_rel_edo <- grupos_estado_rel_edo %>% 
    pivot_longer(-Grupo, names_to = "ENT", values_to = "FAC_HOG") %>%
    filter(FAC_HOG>0)

ggplot(grupos_estado_rel_grupo, aes(as.character(ENT),as.character(Grupo), fill=FAC_HOG*100))+
    geom_tile()+
    coord_equal()+
    labs(
        title = "Distribución de grupos en las entidades",
        subtitle = "Relativo a las entidades",
        caption = nota,
        y = "Grupo", x = "Entidad", fill="%"
        
    )+
    scale_x_discrete(labels=estados$ENT)+
    scale_fill_paletteer_c("ggthemes::Classic Blue")+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90)
    )

ggsave(
    path=path, filename= "distribucion_grupo_entidad.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


# DISTRIBUCION EN LOS ESTADOS
## Relativo a grupos
grupos_trabajo_rel_grupo <- dcast(endutih_res, Grupo~P3_10, fun.aggregate = sum, value.var = "FAC_HOGAR")
grupos_trabajo_rel_grupo[,2:10] <- grupos_trabajo_rel_grupo[,2:10] / rowSums(grupos_trabajo_rel_grupo[,2:10])
grupos_trabajo_rel_grupo <- grupos_trabajo_rel_grupo %>% 
    pivot_longer(-Grupo, names_to = "P3_10", values_to = "FAC_HOGAR") %>% 
    filter(P3_10!="NA"&FAC_HOGAR>0)

ggplot(grupos_trabajo_rel_grupo, aes(y=as.character(P3_10),x=as.character(Grupo), fill=FAC_HOGAR*100))+
    geom_tile()+
    labs(
        title = "Actividad laboral en los grupos la semana pasada",
        subtitle = "Relativo a los grupos",
        caption = nota,
        y = "Actividad Laboral", x = "Grupo", fill="%"
        
    )+
    scale_y_discrete(labels=c("Trabajó","No fue al trabajo", "Buscó trabajo", "Está jubilado", "Se dedicó a estudiar", "Quehaceres del hogar", "Limitación para trabajar", "No trabajó"))+
    scale_fill_paletteer_c("ggthemes::Classic Blue")+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "distribucion_grupos_trabajo.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# EDUCACION
## Relativo a grupos
grupos_edu_rel_grupo <- dcast(endutih_res, Grupo~NIVEL, fun.aggregate = sum, value.var = "FAC_HOGAR")
grupos_edu_rel_grupo[,2:15] <- grupos_edu_rel_grupo[,2:15] / rowSums(grupos_edu_rel_grupo[,2:15])
grupos_edu_rel_grupo <- grupos_edu_rel_grupo %>% 
    pivot_longer(-Grupo, names_to = "NIVEL", values_to = "FAC_HOGAR") %>% 
    filter(NIVEL!="NA" & FAC_HOGAR>0)

ggplot(grupos_edu_rel_grupo, aes(y=as.character(NIVEL),x=as.character(Grupo), fill=FAC_HOGAR*100))+
    geom_tile()+
    labs(
        title = "Último nivel de estudio en los grupos",
        subtitle = "Relativo a los grupos",
        caption = nota,
        y = "Nivel Educativo", x = "Grupo", fill="%"
        
    )+
    scale_y_discrete(labels=c("Ninguno", "Preescolar", "Primaria", "Secundaria","Normal básica", "Estudio técnico", "Preparatoria", "Estudio técnico superior", "Licenciatura o ingeniería", "Especialidad", "Maestría", "Doctorado", "No sabe"))+
    scale_fill_paletteer_c("ggthemes::Classic Blue")+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "distribucion_grupos_edu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

## Personas que aprendieron a usar la compu en la escuela
#P6_6_3

compu_edu <- endutih_usu[endutih_usu$P6_6_3==1, c("UPM", "VIV_SEL", "HOGAR","NUM_REN", "FAC_PER")] %>%
    merge(endutih_res[, c("UPM", "VIV_SEL", "HOGAR","NUM_REN", "NIVEL","Grupo")]) %>%
    dcast(Grupo~NIVEL, fun.aggregate = sum, value.var = "FAC_PER") %>%
    select(-c("99"))

compu_edu[,2:13] <- compu_edu[,2:13]/ rowSums(compu_edu[,2:13])
compu_edu <- compu_edu %>% 
    pivot_longer(-Grupo, names_to = "NIVEL", values_to = "FAC_PER") %>% 
    filter(NIVEL!="NA" & FAC_PER>0)



ggplot(compu_edu, aes(y=as.character(NIVEL),x=as.character(Grupo), fill=FAC_PER*100))+
    geom_tile()+
    labs(
        title = "Último nivel de estudio en los grupos",
        subtitle = "Usuarios que aprendieron a usar la computadora en la escuela",
        caption = nota,
        y = "Nivel Educativo", x = "Grupo", fill="%"
        
    )+
    scale_y_discrete(labels=c("Ninguno", "Preescolar", "Primaria", "Secundaria","Normal básica", "Estudio técnico", "Preparatoria", "Estudio técnico superior", "Licenciatura o ingeniería", "Especialidad", "Maestría", "Doctorado", "No sabe"))+
    scale_fill_paletteer_c("ggthemes::Classic Blue")+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "grupos_edu_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# IDTMex POR ESTADO Y GRUPO

IDTMex_grupo_edo <- IDTMex_grupo_edo %>% 
    pivot_longer(-c(ESTRATO,Grupo),names_to = "ENT", values_to = "IDTMex") %>%
    filter(IDTMex>0)

ggplot(IDTMex_grupo_edo, aes(x=ENT, y=factor(Grupo), fill=IDTMex))+
    geom_tile()+
    labs(
        title = "Valor del IDTMex en los grupos por entidad",
        caption = nota,
        y = "Grupo", x = "Entidad", fill="IDTMex"
        
    )+
    #scale_y_discrete(labels=c("Ninguno", "Preescolar", "Primaria", "Secundaria","Normal básica", "Estudio técnico", "Preparatoria", "Estudio técnico superior", "Licenciatura o ingeniería", "Especialidad", "Maestría", "Doctorado", "No sabe"))+
    scale_fill_paletteer_c("ggthemes::Classic Blue")+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "IDTMex_edo_grupo.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#


#### ESTRATO BAJO ####

path <- "./Graficas/BAJO/"

estrato1_viv <- endutih_vivhogar %>% filter(ESTRATO==1)
estrato1_res <- endutih_res %>% filter(ESTRATO==1)
estrato1_usu <- endutih_usu %>% filter(ESTRATO==1)
estrato1_usu2 <- endutih_usu2 %>% filter(ESTRATO==1)


# MATERIAL DE PISOS
pisos <- c("1"="Tierra","2"="Cemento, firme", "3"="Madera,mosaico,\notro")

ggplot(estrato1_viv, mapping = aes(y=factor(P1_1),weight=FAC_VIV), )+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),fill=main, color=lmain)+
    labs(
        title = "Material de piso de las viviendas",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Material", x="% Viviendas"
        )+
    scale_x_continuous(n.breaks = 10)+
    scale_y_discrete(
        labels=pisos
        )+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "BAJO_material_piso.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# EDAD
ggplot(estrato1_res, aes(EDAD, weight=FAC_HOGAR))+
    geom_histogram(aes(y=after_stat(density)*(100/25)*100),color=lmain, fill=main, bins = 26)+
    geom_vline( xintercept =sum(estrato1_res$FAC_HOGAR*estrato1_res$EDAD)/sum(estrato1_res$FAC_HOGAR), color=lmain)+
    labs(
        title = "Distribución de edad",
        subtitle = "Estrato Bajo",
        caption = nota,
        x="Edad", y="% Residentes"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_hist_edad.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#MODALIDAD INTERNET
pregs <- paste("P5_7",1:8, sep="_")
modalidad_servicio <- estrato1_viv %>% 
    filter(P5_6_5!=1) %>% 
    select(all_of(c("FAC_HOG",pregs))) %>%
    pivot_longer(-FAC_HOG, names_to = "Modalidad",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

modalidades <- c(
    "TV de paga, telefonía fija,\nInternet y telefonía móvil",
    "TV de paga, telefonía fija\ne Internet",
    "TV de paga y telefonía fija",
    "TV de paga e Internet",
    "Telefonía fija e Internet",
    "Solo TV de paga",
    "Solo telefonía fija",
    "Solo Internet"
)

ggplot(modalidad_servicio, aes(y=Modalidad, weight=FAC_HOG))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Modalidad de contratación de servicios",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Modalidad", x="% Hogares"
    )+
    scale_y_discrete(labels=modalidades)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_modalidad_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#Aprendizaje compu
pregs <- paste("P6_6",1:7, sep="_")
aprendizaje_compu <- estrato1_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Metodo",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

modos <- c("Cuenta propia", "Trabajo", "Escuela", "Cursos pagados", "Cursos gratuitos", "Parientes", "Otro")
    
ggplot(aprendizaje_compu, aes(y=Metodo, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Modo de aprendizaje para el uso de computadora",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Modo", x="% Residentes"
    )+
    scale_y_discrete(labels=modos)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_modo_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#Lugares
pregs <- paste("P6_7",1:8, sep="_")

lugares_compu <- estrato1_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Lugar",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

lugares <- c(
    "Hogar", "Trabajo", "Escuela",
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra persona", "Cualquier otro lugar\ncon una laptop",
    "Otro"
)


ggplot(lugares_compu, aes(y=Lugar, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Lugares de uso de computadora",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Lugar", x="% Residentes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_lugar_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#Habilidades
pregs <- paste("P6_8",1:10, sep="_")
habilidades_compu <- estrato1_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Habilidad",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

habilidades <- c(
    "Enviar y recibir\ncorreo", "Descargar contenidos\nde Internet",
    "Copiar archivos entre directorios", "Crear archivos de texto",
    "Crear hojas de cáculo", "Crear presentaciones",
    "Instalar dispositivos\nperiféricos", "Crear o usar\nbases de datos",
    "Programar en lenguaje\nespecializado", "Otras"
)


ggplot(habilidades_compu, aes(y=Habilidad, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Habilidades con la computadora",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Habilidad", x="% Residentes"
    )+
    scale_y_discrete(labels=habilidades)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_habilidad_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#Uso
pregs <- paste("P6_9",1:6, sep="_")
usos_compu <- estrato1_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Uso",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

usos <- c(
    "Actividades laborales", "Labores escolares",
    "Medio de capacitación", "Entretenimiento",
     "Acceso a Internet", "Otro"
)


ggplot(usos_compu, aes(y=Uso, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)), color=lmain, fill=main)+
    labs(
        title = "Usos de la computadora",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Usos", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_uso_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# INTENSIDAD DE USO DE INTERNET
ggplot(estrato1_usu %>% filter(P7_1==1), aes(P7_4, weight=FAC_PER))+
    geom_histogram(aes(y = after_stat(density)*100),color=lmain, fill=main, bins = 12)+
    labs(
        title = "Distribución de la intensidad de uso de Internet",
        subtitle = "Estrato Bajo",
        caption = nota,
        x="Horas de uso en un día", y="% Residentes"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_hist_intensidad_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#EQUIPO INTERNET
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- estrato1_usu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Equipo",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 
    

equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Equipos", x="% Residentes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#LUGARES INTERNET
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- estrato1_usu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Lugar",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Lugares", x="% Residentes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


# PROBLEMAS INTERNET
pregs <- paste("P7_16",1:8, sep="_")

problema_internet <- estrato1_usu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Problema",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


problemas <- c(
    "Infección por virus", "Exceso de información\nno deseada",
    "Interrupciones en el servicio", "Lentitud en la transferencia\nde información",
    "Fraudes con información", "Violación a la privacidad",
    "Mensajes de personas desconocidas", "Otro"
)


ggplot(problema_internet, aes(y=Problema, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Problemas de navegación en Internet",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Lugares", x="% Residentes"
    )+
    scale_y_discrete(labels=problemas)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_problema_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# TIPO DE CONTRATACION CELULAR
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- estrato1_usu2 %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Plan",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Planes", x="% Residentes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_plan_cel.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# USO DE APPS DE CELULAR
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- estrato1_usu2 %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Uso",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Planes", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "BAJO_uso_cel.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#-----------------------------------------------------------#
path <- "./Graficas/MEDIOBAJO/"

#### ESTRATO MEDIO BAJO ####
estrato2_viv <- endutih_vivhogar %>% filter(ESTRATO==2)
estrato2_res <- endutih_res %>% filter(ESTRATO==2)
estrato2_usu <- endutih_usu %>% filter(ESTRATO==2)
estrato2_usu2 <- endutih_usu2 %>% filter(ESTRATO==2)


# MATERIAL DE PISOS
pisos <- c("1"="Tierra","2"="Cemento, firme", "3"="Madera,mosaico,\notro")

ggplot(estrato2_viv, mapping = aes(y=as.factor(P1_1),x=FAC_VIV), )+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain,fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Material de piso de las viviendas",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Material", x="% Viviendas"
    )+
    scale_y_discrete(
        labels=pisos
    )+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90), 
        axis.text.y  = element_text(angle = 0)
    )

ggsave(
    path=path, filename= "MEDIOBAJO_material_piso.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# EDAD 1+ 3.322*log10(sum(estrato2_res$FAC_HOGAR))
ggplot(estrato2_res, aes(EDAD, weight=FAC_HOGAR))+
    geom_histogram(aes(y=after_stat(density)*(100/26)*100),color=lmain, fill=main, bins = 27)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    geom_vline( xintercept =sum(estrato2_res$FAC_HOGAR*estrato2_res$EDAD)/sum(estrato2_res$FAC_HOGAR), color=lmain)+
    labs(
        title = "Distribución de edad",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        x="Edad", y="% Residentes"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOBAJO_hist_edad.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#MODALIDAD INTERNET
pregs <- paste("P5_7",1:8, sep="_")
modalidad_servicio <- estrato2_viv %>% 
    filter(P5_6_5!=1) %>% 
    select(all_of(c("FAC_HOG", "Grupo",pregs))) %>%
    pivot_longer(-c(FAC_HOG,Grupo), names_to = "Modalidad",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

modalidades <- c(
    "TV de paga, telefonía fija,\nInternet y telefonía móvil",
    "TV de paga, telefonía fija\ne Internet",
    "TV de paga y telefonía fija",
    "TV de paga e Internet",
    "Telefonía fija e Internet",
    "Solo TV de paga",
    "Solo telefonía fija",
    "Solo Internet"
)

ggplot(modalidad_servicio, aes(y=Modalidad, weight=FAC_HOG))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)), color=lmain, fill=main)+
    labs(
        title = "Modalidad de contratación de servicios",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Modalidad", x="% Hogares"
    )+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    scale_y_discrete(labels=modalidades)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOBAJO_modalidad_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#Aprendizaje compu
pregs <- paste("P6_6",1:7, sep="_")
aprendizaje_compu <- estrato2_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Metodo",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

modos <- c("Cuenta propia", "Trabajo", "Escuela", "Cursos pagados", "Cursos gratuitos", "Parientes", "Otro")

ggplot(aprendizaje_compu, aes(y=Metodo, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Modo de aprendizaje para el uso de computadora",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Modo", x="% Residentes"
    )+
    scale_y_discrete(labels=modos)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOBAJO_modo_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#Lugares
pregs <- paste("P6_7",1:8, sep="_")

lugares_compu <- estrato2_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Lugar",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

lugares <- c(
    "Hogar", "Trabajo", "Escuela",
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra persona", "Cualquier otro lugar\ncon una laptop",
    "Otro"
)


ggplot(lugares_compu, aes(y=Lugar, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Lugares de uso de computadora",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Lugar", x="% Residentes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOBAJO_lugar_compu.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)


#Habilidades
pregs <- paste("P6_8",1:10, sep="_")
habilidades_compu <- estrato2_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Habilidad",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

habilidades <- c(
    "Enviar y recibir\ncorreo", "Descargar contenidos\nde Internet",
    "Copiar archivos entre directorios", "Crear archivos de texto",
    "Crear hojas de cáculo", "Crear presentaciones",
    "Instalar dispositivos\nperiféricos", "Crear o usar\nbases de datos",
    "Programar en lenguaje\nespecializado", "Otras"
)


ggplot(habilidades_compu, aes(y=Habilidad, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Habilidades con la computadora",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Habilidad", x="% Residentes"
    )+
    scale_y_discrete(labels=habilidades)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOBAJO_habilidad_compu.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)

#Uso
pregs <- paste("P6_9",1:6, sep="_")
usos_compu <- estrato2_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Uso",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

usos <- c(
    "Actividades laborales", "Labores escolares",
    "Medio de capacitación", "Entretenimiento",
    "Acceso a Internet", "Otro"
)


ggplot(usos_compu, aes(y=Uso, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Usos de la computadora",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Usos", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOBAJO_uso_compu.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)

# INTENSIDAD DE USO DE INTERNET
ggplot(estrato2_usu %>% filter(P7_1==1), aes(P7_4, weight=FAC_PER))+
    geom_histogram(aes(y=after_stat(density)*100),color=lmain, fill=main, bins = 12)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Distribución de la intensidad de uso de Internet",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        x="Horas de uso en un día", y="% Residentes"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOBAJO_hist_intensidad_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#EQUIPO INTERNET
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- estrato2_usu %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Equipo",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Equipos", x="% Residentes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "MEDIOBAJO_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#LUGARES INTERNET
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- estrato2_usu %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Lugar",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Lugares", x="% Residentes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOBAJO_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)


# PROBLEMAS INTERNET
pregs <- paste("P7_16",1:8, sep="_")

problema_internet <- estrato2_usu %>% 
    select(all_of(c("FAC_PER", "Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Problema",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


problemas <- c(
    "Infección por virus", "Exceso de información\nno deseada",
    "Interrupciones en el servicio", "Lentitud en la transferencia\nde información",
    "Fraudes con información", "Violación a la privacidad",
    "Mensajes de personas desconocidas", "Otro"
)


ggplot(problema_internet, aes(y=Problema, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Problemas de navegación en Internet",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Lugares", x="% Residentes"
    )+
    scale_y_discrete(labels=problemas)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOBAJO_problema_internet.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)

# TIPO DE CONTRATACION CELULAR
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- estrato2_usu2 %>% 
    select(all_of(c("FAC_PER","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Plan",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Planes", x="% Residentes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOBAJO_plan_cel.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# USO DE APPS DE CELULAR
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- estrato2_usu2 %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Uso",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Planes", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOBAJO_uso_cel.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)


#-----------------------------------------------------------#

#### ESTRATO MEDIO ALTO ####

path <- "./Graficas/MEDIOALTO/"

estrato3_viv <- endutih_vivhogar %>% filter(ESTRATO==3)
estrato3_res <- endutih_res %>% filter(ESTRATO==3)
estrato3_usu <- endutih_usu %>% filter(ESTRATO==3)
estrato3_usu2 <- endutih_usu2 %>% filter(ESTRATO==3)


# MATERIAL DE PISOS
pisos <- c("1"="Tierra","2"="Cemento, firme", "3"="Madera,mosaico,\notro")

ggplot(estrato3_viv, mapping = aes(y=as.factor(P1_1),x=FAC_VIV), )+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)), color=lmain,fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Material de piso de las viviendas",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        y="Material", x="% Viviendas"
    )+
    scale_y_discrete(
        labels=pisos
    )+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90), 
        axis.text.y  = element_text(angle = 0)
    )

ggsave(
    path=path, filename= "MEDIOALTO_material_piso.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# EDAD 1+ 3.322*log10(sum(estrato3_res$FAC_HOGAR))
ggplot(estrato3_res, aes(EDAD, weight=FAC_HOGAR))+
    geom_histogram(aes(y=after_stat(density)*(100/25)*100), color=lmain, fill=main, bins = 26)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    geom_vline( xintercept =sum(estrato2_res$FAC_HOGAR*estrato2_res$EDAD)/sum(estrato2_res$FAC_HOGAR), color=lmain)+
    labs(
        title = "Distribución de edad",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        x="Edad", y="% Residentes"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOALTO_hist_edad.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#MODALIDAD INTERNET
pregs <- paste("P5_7",1:8, sep="_")
modalidad_servicio <- estrato3_viv %>% 
    filter(P5_6_5!=1) %>% 
    select(all_of(c("FAC_HOG", "Grupo",pregs))) %>%
    pivot_longer(-c(FAC_HOG,Grupo), names_to = "Modalidad",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

modalidades <- c(
    "TV de paga, telefonía fija,\nInternet y telefonía móvil",
    "TV de paga, telefonía fija\ne Internet",
    "TV de paga y telefonía fija",
    "TV de paga e Internet",
    "Telefonía fija e Internet",
    "Solo TV de paga",
    "Solo telefonía fija",
    "Solo Internet"
)

ggplot(modalidad_servicio, aes(y=Modalidad, weight=FAC_HOG))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Modalidad de contratación de servicios",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        y="Modalidad", x="% Hogares"
    )+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    scale_y_discrete(labels=modalidades)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOALTO_modalidad_compu.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)

#Aprendizaje compu
pregs <- paste("P6_6",1:7, sep="_")
aprendizaje_compu <- estrato3_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Metodo",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

modos <- c("Cuenta propia", "Trabajo", "Escuela", "Cursos pagados", "Cursos gratuitos", "Parientes", "Otro")

ggplot(aprendizaje_compu, aes(y=Metodo, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Modo de aprendizaje para el uso de computadora",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        y="Modo", x="% Residentes"
    )+
    scale_y_discrete(labels=modos)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOALTO_modo_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#Lugares
pregs <- paste("P6_7",1:8, sep="_")

lugares_compu <- estrato3_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Lugar",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

lugares <- c(
    "Hogar", "Trabajo", "Escuela",
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra persona", "Cualquier otro lugar\ncon una laptop",
    "Otro"
)


ggplot(lugares_compu, aes(y=Lugar, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Lugares de uso de computadora",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        y="Lugar", x="% Residentes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "MEDIOALTO_lugar_compu.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)


#Habilidades
pregs <- paste("P6_8",1:10, sep="_")
habilidades_compu <- estrato3_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Habilidad",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

habilidades <- c(
    "Enviar y recibir\ncorreo", "Descargar contenidos\nde Internet",
    "Copiar archivos entre directorios", "Crear archivos de texto",
    "Crear hojas de cáculo", "Crear presentaciones",
    "Instalar dispositivos\nperiféricos", "Crear o usar\nbases de datos",
    "Programar en lenguaje\nespecializado", "Otras"
)


ggplot(habilidades_compu, aes(y=Habilidad, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Habilidades con la computadora",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        y="Habilidad", x="% Residentes"
    )+
    scale_y_discrete(labels=habilidades)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOALTO_habilidad_compu.pdf",
    device="pdf", dpi="retina",
    width=59, height=33, units="cm"
)

#Uso
pregs <- paste("P6_9",1:6, sep="_")
usos_compu <- estrato3_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Uso",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

usos <- c(
    "Actividades laborales", "Labores escolares",
    "Medio de capacitación", "Entretenimiento",
    "Acceso a Internet", "Otro"
)


ggplot(usos_compu, aes(y=Uso, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Usos de la computadora",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        y="Usos", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "MEDIOALTO_uso_compu.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)

# INTENSIDAD DE USO DE INTERNET
ggplot(estrato3_usu %>% filter(P7_1==1), aes(P7_4, weight=FAC_PER))+
    geom_histogram(aes(y=after_stat(density)*100),color=lmain, fill=main, bins = 12)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Distribución de la intensidad de uso de Internet",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        x="Horas de uso en un día", y="% Residentes"
    )+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOALTO_hist_intensidad_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#EQUIPO INTERNET
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- estrato3_usu %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Equipo",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        y="Equipos", x="% Residentes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "MEDIOALTO_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#LUGARES INTERNET
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- estrato3_usu %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Lugar",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        y="Lugares", x="% Residentes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOALTO_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)


# PROBLEMAS INTERNET
pregs <- paste("P7_16",1:8, sep="_")

problema_internet <- estrato3_usu %>% 
    select(all_of(c("FAC_PER", "Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Problema",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


problemas <- c(
    "Infección por virus", "Exceso de información\nno deseada",
    "Interrupciones en el servicio", "Lentitud en la transferencia\nde información",
    "Fraudes con información", "Violación a la privacidad",
    "Mensajes de personas desconocidas", "Otro"
)


ggplot(problema_internet, aes(y=Problema, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Problemas de navegación en Internet",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        y="Lugares", x="% Residentes"
    )+
    scale_y_discrete(labels=problemas)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "MEDIOALTO_problema_internet.pdf",
    device="pdf", dpi="retina",
    width=59, height=33, units="cm"
)

# TIPO DE CONTRATACION CELULAR
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- estrato3_usu2 %>% 
    select(all_of(c("FAC_PER","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Plan",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        y="Planes", x="% Residentes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "MEDIOALTO_plan_cel.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# USO DE APPS DE CELULAR
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- estrato3_usu2 %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Uso",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Estrato Medio Alto",
        caption = nota,
        y="Planes", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "MEDIOALTO_uso_cel.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)

#-----------------------------------------------------------#

#### ESTRATO ALTO ####
path <- "./Graficas/ALTO/"
estrato4_viv <- endutih_vivhogar %>% filter(ESTRATO==4)
estrato4_res <- endutih_res %>% filter(ESTRATO==4)
estrato4_usu <- endutih_usu %>% filter(ESTRATO==4)
estrato4_usu2 <- endutih_usu2 %>% filter(ESTRATO==4)


# MATERIAL DE PISOS
pisos <- c("1"="Tierra","2"="Cemento, firme", "3"="Madera,mosaico,\notro")

ggplot(estrato4_viv, mapping = aes(y=as.factor(P1_1),weight=FAC_VIV), )+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Material de piso de las viviendas",
        subtitle = "Estrato Alto",
        caption = nota,
        y="Material", x="% Viviendas"
    )+
    scale_y_discrete(
        labels=pisos
    )+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90), 
        axis.text.y  = element_text(angle = 0)
    )

ggsave(
    path=path, filename= "ALTO_material_piso.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# EDAD 1+ 3.322*log10(sum(estrato4_res$FAC_HOGAR))
ggplot(estrato4_res, aes(EDAD, weight=FAC_HOGAR))+
    geom_histogram(aes(y=after_stat(density)*(100/23)*100),color=lmain, fill=main, bins = 24)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    geom_vline( xintercept =sum(estrato2_res$FAC_HOGAR*estrato2_res$EDAD)/sum(estrato2_res$FAC_HOGAR), color=lmain)+
    labs(
        title = "Distribución de edad",
        subtitle = "Estrato Alto",
        caption = nota,
        x="Edad", y="% Residentes"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "ALTO_hist_edad.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#MODALIDAD INTERNET
pregs <- paste("P5_7",1:8, sep="_")
modalidad_servicio <- estrato4_viv %>% 
    filter(P5_6_5!=1) %>% 
    select(all_of(c("FAC_HOG", "Grupo",pregs))) %>%
    pivot_longer(-c(FAC_HOG,Grupo), names_to = "Modalidad",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

modalidades <- c(
    "TV de paga, telefonía fija,\nInternet y telefonía móvil",
    "TV de paga, telefonía fija\ne Internet",
    "TV de paga y telefonía fija",
    "TV de paga e Internet",
    "Telefonía fija e Internet",
    "Solo TV de paga",
    "Solo telefonía fija",
    "Solo Internet"
)

ggplot(modalidad_servicio, aes(y=Modalidad, weight=FAC_HOG))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    labs(
        title = "Modalidad de contratación de servicios",
        subtitle = "Estrato Alto",
        caption = nota,
        y="Modalidad", x="% Hogares"
    )+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    scale_y_discrete(labels=modalidades)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "ALTO_modalidad_compu.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)

#Aprendizaje compu
pregs <- paste("P6_6",1:7, sep="_")
aprendizaje_compu <- estrato4_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Metodo",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

modos <- c("Cuenta propia", "Trabajo", "Escuela", "Cursos pagados", "Cursos gratuitos", "Parientes", "Otro")

ggplot(aprendizaje_compu, aes(y=Metodo, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Modo de aprendizaje para el uso de computadora",
        subtitle = "Estrato Alto",
        caption = nota,
        y="Modo", x="% Residentes"
    )+
    scale_y_discrete(labels=modos)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "ALTO_modo_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#Lugares
pregs <- paste("P6_7",1:8, sep="_")

lugares_compu <- estrato4_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Lugar",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

lugares <- c(
    "Hogar", "Trabajo", "Escuela",
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra persona", "Cualquier otro lugar\ncon una laptop",
    "Otro"
)


ggplot(lugares_compu, aes(y=Lugar, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Lugares de uso de computadora",
        subtitle = "Estrato Alto",
        caption = nota,
        y="Lugar", x="% Residentes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "ALTO_lugar_compu.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)


#Habilidades
pregs <- paste("P6_8",1:10, sep="_")
habilidades_compu <- estrato4_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Habilidad",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

habilidades <- c(
    "Enviar y recibir\ncorreo", "Descargar contenidos\nde Internet",
    "Copiar archivos entre directorios", "Crear archivos de texto",
    "Crear hojas de cáculo", "Crear presentaciones",
    "Instalar dispositivos\nperiféricos", "Crear o usar\nbases de datos",
    "Programar en lenguaje\nespecializado", "Otras"
)


ggplot(habilidades_compu, aes(y=Habilidad, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Habilidades con la computadora",
        subtitle = "Estrato Alto",
        caption = nota,
        y="Habilidad", x="% Residentes"
    )+
    scale_y_discrete(labels=habilidades)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "ALTO_habilidad_compu.pdf",
    device="pdf", dpi="retina",
    width=59, height=33, units="cm"
)

#Uso
pregs <- paste("P6_9",1:6, sep="_")
usos_compu <- estrato4_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Uso",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

usos <- c(
    "Actividades laborales", "Labores escolares",
    "Medio de capacitación", "Entretenimiento",
    "Acceso a Internet", "Otro"
)


ggplot(usos_compu, aes(y=Uso, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Usos de la computadora",
        subtitle = "Estrato Alto",
        caption = nota,
        y="Usos", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "ALTO_uso_compu.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)

# INTENSIDAD DE USO DE INTERNET
ggplot(estrato4_usu %>% filter(P7_1==1), aes(P7_4, weight=FAC_PER))+
    geom_histogram(aes(y=after_stat(density)*100),color=lmain, fill=main, bins = 12)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Distribución de la intensidad de uso de Internet",
        subtitle = "Estrato Alto",
        caption = nota,
        x="Horas de uso en un día", y="% Residentes"
    )+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "ALTO_hist_intensidad_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#EQUIPO INTERNET
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- estrato4_usu %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Equipo",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Estrato Alto",
        caption = nota,
        y="Equipos", x="% Residentes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "ALTO_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

#LUGARES INTERNET
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- estrato4_usu %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Lugar",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Estrato Alto",
        caption = nota,
        y="Lugares", x="% Residentes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "ALTO_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=48, height=27, units="cm"
)


# PROBLEMAS INTERNET
pregs <- paste("P7_16",1:8, sep="_")

problema_internet <- estrato4_usu %>% 
    select(all_of(c("FAC_PER", "Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Problema",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


problemas <- c(
    "Infección por virus", "Exceso de información\nno deseada",
    "Interrupciones en el servicio", "Lentitud en la transferencia\nde información",
    "Fraudes con información", "Violación a la privacidad",
    "Mensajes de personas desconocidas", "Otro"
)


ggplot(problema_internet, aes(y=Problema, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Problemas de navegación en Internet",
        subtitle = "Estrato Alto",
        caption = nota,
        y="Lugares", x="% Residentes"
    )+
    scale_y_discrete(labels=problemas)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "ALTO_problema_internet.pdf",
    device="pdf", dpi="retina",
    width=59, height=33, units="cm"
)

# TIPO DE CONTRATACION CELULAR
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- estrato4_usu2 %>% 
    select(all_of(c("FAC_PER","Grupo", pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Plan",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)


ggplot(plan_cel, aes(y=Plan, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Estrato Alto",
        caption = nota,
        y="Planes", x="% Residentes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "ALTO_plan_cel.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)

# USO DE APPS DE CELULAR
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- estrato4_usu2 %>% 
    select(all_of(c("FAC_PER","Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Uso",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1) 

usos <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


ggplot(uso_cel, aes(y=Uso, weight=FAC_PER))+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, nrow = 2,  scales = "fixed",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Estrato Alto",
        caption = nota,
        y="Planes", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "ALTO_uso_cel.pdf",
    device="pdf", dpi="retina",
    width=36, height=20, units="cm"
)




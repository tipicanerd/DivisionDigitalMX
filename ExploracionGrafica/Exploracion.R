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
estados

#COMPLETA
endutih_completa <- merge(endutih_vivhogar, select(endutih_res, select= -contains(c("UPM_DIS","ESTRATO","ENT","DOMINIO", "EST_DIS", "TLOC"))), by = c("UPM","VIV_SEL", "HOGAR") )

#rm(endutih_res, endutih_vivhogar)


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


#### TODOS LOS ESTRATOS ####

# DISTRIBUCION EN LOS ESTADOS
## Relativo a grupos
grupos_estado_rel_grupo <- dcast(endutih_vivhogar, Grupo~ENT, fun.aggregate = sum, value.var = "FAC_HOG")
grupos_estado_rel_grupo[,2:33] <- grupos_estado_rel_grupo[,2:33] / rowSums(grupos_estado_rel_grupo[,2:33])
grupos_estado_rel_grupo <- grupos_estado_rel_grupo %>% 
    pivot_longer(-Grupo, names_to = "ENT", values_to = "FAC_HOG")

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
        aspect.ratio=0.46875
    )

ggsave(
    filename= "./Graficas/distribucion_entidad_grupos.pdf",
    device="pdf", dpi="retina",
    width=26, height=14, units="cm"
)

## Relativo a estados
grupos_estado_rel_edo <- dcast(endutih_vivhogar, Grupo~ENT, fun.aggregate = sum, value.var = "FAC_HOG")
grupos_estado_rel_edo[,2:33] <- t(t(grupos_estado_rel_edo[,2:33])/colSums(grupos_estado_rel_edo[,2:33]))
grupos_estado_rel_edo <- grupos_estado_rel_edo %>% 
    pivot_longer(-Grupo, names_to = "ENT", values_to = "FAC_HOG")

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
        axis.text.x = element_text(angle=90),
        aspect.ratio=0.46875
    )

ggsave(
    filename= "./Graficas/distribucion_grupos_entidad.pdf",
    device="pdf", dpi="retina",
    width=26, height=14, units="cm"
)


# DISTRIBUCION EN LOS ESTADOS
## Relativo a grupos
grupos_trabajo_rel_grupo <- dcast(endutih_res, Grupo~P3_10, fun.aggregate = sum, value.var = "FAC_HOGAR")
grupos_trabajo_rel_grupo[,2:10] <- grupos_trabajo_rel_grupo[,2:10] / rowSums(grupos_trabajo_rel_grupo[,2:10])
grupos_trabajo_rel_grupo <- grupos_trabajo_rel_grupo %>% 
    pivot_longer(-Grupo, names_to = "P3_10", values_to = "FAC_HOGAR") %>% 
    filter(P3_10!="NA")

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
    theme(axis.text.x = element_text(angle=90), aspect.ratio=0.6667)

ggsave(
    filename= "./Graficas/distribucion_grupos_trabajo.pdf",
    device="pdf", dpi="retina",
    width=21, height=14, units="cm"
)

# EDUCACION
## Relativo a grupos
grupos_edu_rel_grupo <- dcast(endutih_res, Grupo~NIVEL, fun.aggregate = sum, value.var = "FAC_HOGAR")
grupos_edu_rel_grupo[,2:15] <- grupos_edu_rel_grupo[,2:15] / rowSums(grupos_edu_rel_grupo[,2:15])
grupos_edu_rel_grupo <- grupos_edu_rel_grupo %>% 
    pivot_longer(-Grupo, names_to = "NIVEL", values_to = "FAC_HOGAR") %>% 
    filter(NIVEL!="NA")

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
    theme(axis.text.x = element_text(angle=90), aspect.ratio=1)

ggsave(
    filename= "./Graficas/distribucion_grupos_edu.pdf",
    device="pdf", dpi="retina",
    width=21, height=14, units="cm"
)


#### ESTRATO BAJO ####
grupo11_viv <- endutih_vivhogar %>% dplyr::filter(Grupo==11)
grupo11_res <- endutih_res %>% dplyr::filter(Grupo==11)
grupo11_usu <- endutih_usu %>% dplyr::filter(Grupo==11)
grupo11_usu2 <- endutih_usu2 %>% dplyr::filter(Grupo==11)


# MATERIAL DE PISOS
g11_pisos <- grupo11_viv %>% group_by(P1_1) %>% dplyr::summarise(Total= sum(FAC_VIV))
g11_pisos$P1_1 <- as.factor(g11_pisos$P1_1)

ggplot(g11_pisos, mapping = aes(x=P1_1,y=Total/1e3), )+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Material de piso de las viviendas en el Grupo 11",
        caption = nota,
        x="Material", y="Viviendas (en miles)"
        )+
    scale_y_continuous(n.breaks = 7)+
    scale_x_discrete(
        labels=c("1"="Tierra","2"="Cemento, firme", "3"="Madera,mosaico,\notro")
        )+
    theme_light()+
    theme(axis.text.x = element_text(angle=90), aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/G11_material_piso.pdf",
    device="pdf", dpi="retina",
    width=19, height=14, units="cm"
)

# EDAD
ggplot(grupo11_res, aes(EDAD, weight=FAC_HOGAR/1e3))+
    geom_histogram(color=lmain, fill=main, bins = 26)+
    geom_vline( xintercept =sum(grupo11_res$FAC_HOGAR*grupo11_res$EDAD)/sum(grupo11_res$FAC_HOGAR), color=lmain)+
    labs(
        title = "Distribución de edad en el grupo 11",
        caption = nota,
        x="Edad", y="Residentes (en miles)"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme(aspect.ratio = 0.76)

ggsave(
    filename= "./Graficas/G11_hist_edad.pdf",
    device="pdf", dpi="retina",
    width=18.5, height=14, units="cm"
)

#Aprendizaje compu
pregs <- paste("P6_6",1:7, sep="_")
aprendizaje_compu <- grupo11_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Metodo",  values_to = "total", values_drop_na = TRUE) %>%
    filter(total==1) %>%
    mutate(total = total*FAC_PER)

ggplot(aprendizaje_compu, aes(y=Metodo, weight=total/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Modo de aprendizaje para el uso de computadora del grupo 11",
        caption = nota,
        y="Modo", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=c("Cuenta propia", "Trabajo", "Escuela", "Cursos pagados", "Cursos gratuitos", "Parientes", "Otro"))+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/G11_modo_apr_compu.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
)

#Lugares
pregs <- paste("P6_7",1:8, sep="_")

lugares_compu <- grupo11_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Lugar",  values_to = "total", values_drop_na = TRUE) %>%
    filter(total==1) %>%
    mutate(total = total*FAC_PER)

lugares <- c(
    "Hogar", "Trabajo", "Escuela",
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra persona", "Cualquier otro lugar\ncon una laptop",
    "Otro"
)


ggplot(lugares_compu, aes(y=Lugar, weight=total/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Lugares de uso de computadora del grupo 11",
        caption = nota,
        y="Lugar", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/G11_lugares_compu.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
)


#Habilidades
pregs <- paste("P6_8",1:10, sep="_")
habilidades_compu <- grupo11_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Habilidad",  values_to = "total", values_drop_na = TRUE) %>%
    filter(total==1) %>%
    mutate(total = total*FAC_PER)

habilidades <- c(
    "Enviar y recibir\ncorreo", "Descargar contenidos\nde Internet",
    "Copiar archivos entre directorios", "Crear archivos de texto",
    "Crear hojas de cáculo", "Crear presentaciones",
    "Instalar dispositivos\nperiféricos", "Crear o usar\nbases de datos",
    "Programar en lenguaje\nespecializado", "Otras"
)


ggplot(habilidades_compu, aes(y=Habilidad, weight=total/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Habilidades con la computadora del grupo 11",
        caption = nota,
        y="Habilidad", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=habilidades)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/G11_habilidad_compu.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
)

#Uso
pregs <- paste("P6_9",1:6, sep="_")
usos_compu <- grupo11_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Uso",  values_to = "total", values_drop_na = TRUE) %>%
    filter(total==1) %>%
    mutate(total = total*FAC_PER)

usos <- c(
    "Actividades laborales", "Labores escolares",
    "Medio de capacitación", "Entretenimiento",
     "Acceso a Internet", "Otro"
)


ggplot(usos_compu, aes(y=Uso, weight=total/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Usos de la computadora del grupo 11",
        caption = nota,
        y="Usos", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/G11_uso_compu.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
)

# INTENSIDAD DE USO DE INTERNET
ggplot(grupo11_usu %>% filter(P7_1==1), aes(P7_4, weight=FAC_PER/1e3))+
    geom_histogram(color=lmain, fill=main, bins = 12)+
    labs(
        title = "Distribución de la intensidad de uso de Internet en el grupo 11",
        caption = nota,
        x="Horas de uso en un día", y="Residentes (en miles)"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme(aspect.ratio = 0.76)

ggsave(
    filename= "./Graficas/G11_hist_intensidad_internet.pdf",
    device="pdf", dpi="retina",
    width=18.5, height=14, units="cm"
)

#EQUIPO INTERNET
pregs <- paste("P7_5",1:7, sep="_")
#grupo11_usu[,pregs] <- as.character(grupo11_usu[,pregs])
equipo_internet <- grupo11_usu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Equipo",  values_to = "total", values_drop_na = TRUE) %>%
    filter(total==1) 
    

equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)


ggplot(equipo_internet, aes(y=Equipo, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Equipos donde utiliza Internet el grupo 11",
        caption = nota,
        y="Equipos", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/G11_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
)

#LUGARES INTERNET
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- grupo11_usu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Lugar",  values_to = "total", values_drop_na = TRUE) %>%
    filter(total==1) 


lugares <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)


ggplot(lugar_internet, aes(y=Lugar, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Lugares donde utiliza Internet el grupo 11",
        caption = nota,
        y="Lugares", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/G11_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
)


# PROBLEMAS INTERNET
pregs <- paste("P7_16",1:8, sep="_")

problema_internet <- grupo11_usu %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Problema",  values_to = "total", values_drop_na = TRUE) %>%
    filter(total==1) 


problemas <- c(
    "Infección por virus", "Exceso de información\nno deseada",
    "Interrupciones en el servicio", "Lentitud en la transferencia\nde información",
    "Fraudes con información", "Violación a la privacidad",
    "Mensajes de personas desconocidas", "Otro"
)


ggplot(problema_internet, aes(y=Problema, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Problemas de navegación en Internet que presenta el grupo 11",
        caption = nota,
        y="Lugares", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=problemas)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/G11_problema_internet.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
)













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
    filename= "./Graficas/distribucion_grupo_entidad.pdf",
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
estrato1_viv <- endutih_vivhogar %>% filter(ESTRATO==1)
estrato1_res <- endutih_res %>% filter(ESTRATO==1)
estrato1_usu <- endutih_usu %>% filter(ESTRATO==1)
estrato1_usu2 <- endutih_usu2 %>% filter(ESTRATO==1)


# MATERIAL DE PISOS
g11_pisos <- estrato1_viv %>% group_by(P1_1) %>% dplyr::summarise(Total= sum(FAC_VIV))
g11_pisos$P1_1 <- as.factor(g11_pisos$P1_1)
pisos <- c("1"="Tierra","2"="Cemento, firme", "3"="Madera,mosaico,\notro")

ggplot(g11_pisos, mapping = aes(y=P1_1,x=Total/1e3), )+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Material de piso de las viviendas",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Material", x="Viviendas (en miles)"
        )+
    scale_x_continuous(n.breaks = 7)+
    scale_y_discrete(
        labels=pisos
        )+
    theme_light()+
    theme(axis.text.x = element_text(angle=90), aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/BAJO_material_piso.pdf",
    device="pdf", dpi="retina",
    width=19, height=14, units="cm"
)

# EDAD
ggplot(estrato1_res, aes(EDAD, weight=FAC_HOGAR/1e3))+
    geom_histogram(color=lmain, fill=main, bins = 26)+
    geom_vline( xintercept =sum(estrato1_res$FAC_HOGAR*estrato1_res$EDAD)/sum(estrato1_res$FAC_HOGAR), color=lmain)+
    labs(
        title = "Distribución de edad",
        subtitle = "Estrato Bajo",
        caption = nota,
        x="Edad", y="Residentes (en miles)"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme(aspect.ratio = 0.76)

ggsave(
    filename= "./Graficas/BAJO_hist_edad.pdf",
    device="pdf", dpi="retina",
    width=18.5, height=14, units="cm"
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

ggplot(modalidad_servicio, aes(y=Modalidad, weight=FAC_HOG/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Modalidad de contratación de servicios",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Modalidad", x="Hogares (en miles)"
    )+
    scale_y_discrete(labels=modalidades)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/BAJO_modo_compu.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
)

#Aprendizaje compu
pregs <- paste("P6_6",1:7, sep="_")
aprendizaje_compu <- estrato1_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER",pregs))) %>%
    pivot_longer(-FAC_PER, names_to = "Metodo",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

modos <- c("Cuenta propia", "Trabajo", "Escuela", "Cursos pagados", "Cursos gratuitos", "Parientes", "Otro")
    
ggplot(aprendizaje_compu, aes(y=Metodo, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Modo de aprendizaje para el uso de computadora",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Modo", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=modos)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/BAJO_modo_compu.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
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


ggplot(lugares_compu, aes(y=Lugar, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Lugares de uso de computadora",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Lugar", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/BAJO_lugar_compu.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
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


ggplot(habilidades_compu, aes(y=Habilidad, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Habilidades con la computadora",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Habilidad", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=habilidades)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/BAJO_habilidad_compu.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
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


ggplot(usos_compu, aes(y=Uso, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Usos de la computadora",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Usos", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/BAJO_uso_compu.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
)

# INTENSIDAD DE USO DE INTERNET
ggplot(estrato1_usu %>% filter(P7_1==1), aes(P7_4, weight=FAC_PER/1e3))+
    geom_histogram(color=lmain, fill=main, bins = 12)+
    labs(
        title = "Distribución de la intensidad de uso de Internet",
        subtitle = "Estrato Bajo",
        caption = nota,
        x="Horas de uso en un día", y="Residentes (en miles)"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme(aspect.ratio = 0.76)

ggsave(
    filename= "./Graficas/BAJO_hist_intensidad_internet.pdf",
    device="pdf", dpi="retina",
    width=18.5, height=14, units="cm"
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


ggplot(equipo_internet, aes(y=Equipo, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Equipos donde utiliza Internet1",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Equipos", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/BAJO_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
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


ggplot(lugar_internet, aes(y=Lugar, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Lugares", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/BAJO_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
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


ggplot(problema_internet, aes(y=Problema, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Problemas de navegación en Internet",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Lugares", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=problemas)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/BAJO_problema_internet.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
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


ggplot(plan_cel, aes(y=Plan, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Planes", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/BAJO_plan_cel.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
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


ggplot(uso_cel, aes(y=Uso, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Estrato Bajo",
        caption = nota,
        y="Planes", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/BAJO_uso_cel.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
)

#-----------------------------------------------------------#

#### ESTRATO MEDIO BAJO ####
estrato2_viv <- endutih_vivhogar %>% filter(ESTRATO==2)
estrato2_res <- endutih_res %>% filter(ESTRATO==2)
estrato2_usu <- endutih_usu %>% filter(ESTRATO==2)
estrato2_usu2 <- endutih_usu2 %>% filter(ESTRATO==2)


# MATERIAL DE PISOS
pisos <- c("1"="Tierra","2"="Cemento, firme", "3"="Madera,mosaico,\notro")

ggplot(estrato2_viv, mapping = aes(y=as.factor(P1_1),x=FAC_HOG/1e3), )+
    geom_col(fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Material de piso de las viviendas",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Material", x="Viviendas (en miles)"
    )+
    scale_y_discrete(
        labels=pisos
    )+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90), 
        axis.text.y  = element_text(angle = 0),
        aspect.ratio = 0.73
    )

ggsave(
    filename= "./Graficas/MEDIOBAJO_material_piso.pdf",
    device="pdf", dpi="retina",
    width=19, height=14, units="cm"
)

# EDAD 1+ 3.322*log10(sum(estrato2_res$FAC_HOGAR))
ggplot(estrato2_res, aes(EDAD, weight=FAC_HOGAR/1e3))+
    geom_histogram(color=lmain, fill=main, bins = 27)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    geom_vline( xintercept =sum(estrato2_res$FAC_HOGAR*estrato2_res$EDAD)/sum(estrato2_res$FAC_HOGAR), color=lmain)+
    labs(
        title = "Distribución de edad",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        x="Edad", y="Residentes (en miles)"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme(aspect.ratio = 0.76)

ggsave(
    filename= "./Graficas/MEDIOBAJO_hist_edad.pdf",
    device="pdf", dpi="retina",
    width=18.5, height=14, units="cm"
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

ggplot(modalidad_servicio, aes(y=Modalidad, weight=FAC_HOG/1e3))+
    geom_bar(color=lmain, fill=main)+
    labs(
        title = "Modalidad de contratación de servicios",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Modalidad", x="Hogares (en miles)"
    )+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    scale_y_discrete(labels=modalidades)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/MEDIOBAJO_modo_compu.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
)

#Aprendizaje compu
pregs <- paste("P6_6",1:7, sep="_")
aprendizaje_compu <- estrato2_usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo",pregs))) %>%
    pivot_longer(-c(FAC_PER, Grupo), names_to = "Metodo",  values_to = "resultado", values_drop_na = TRUE) %>%
    filter(resultado==1)

modos <- c("Cuenta propia", "Trabajo", "Escuela", "Cursos pagados", "Cursos gratuitos", "Parientes", "Otro")

ggplot(aprendizaje_compu, aes(y=Metodo, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Modo de aprendizaje para el uso de computadora",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Modo", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=modos)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/MEDIOBAJO_modo_compu.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
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


ggplot(lugares_compu, aes(y=Lugar, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Lugares de uso de computadora",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Lugar", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/MEDIOBAJO_lugar_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=20, units="cm"
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


ggplot(habilidades_compu, aes(y=Habilidad, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Habilidades con la computadora",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Habilidad", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=habilidades)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/MEDIOBAJO_habilidad_compu.pdf",
    device="pdf", dpi="retina",
    width=33, height=27, units="cm"
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


ggplot(usos_compu, aes(y=Uso, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Usos de la computadora",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Usos", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/MEDIOBAJO_uso_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=20, units="cm"
)

# INTENSIDAD DE USO DE INTERNET
ggplot(estrato2_usu %>% filter(P7_1==1), aes(P7_4, weight=FAC_PER/1e3))+
    geom_histogram(color=lmain, fill=main, bins = 12)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Distribución de la intensidad de uso de Internet",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        x="Horas de uso en un día", y="Residentes (en miles)"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme(aspect.ratio = 0.76)

ggsave(
    filename= "./Graficas/MEDIOBAJO_hist_intensidad_internet.pdf",
    device="pdf", dpi="retina",
    width=18.5, height=14, units="cm"
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


ggplot(equipo_internet, aes(y=Equipo, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Equipos donde utiliza Internet1",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Equipos", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73, axis.text.x = element_text(angle=90) )

ggsave(
    filename= "./Graficas/MEDIOBAJO_equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
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


ggplot(lugar_internet, aes(y=Lugar, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Lugares", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/MEDIOBAJO_lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=33, height=27, units="cm"
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


ggplot(problema_internet, aes(y=Problema, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Problemas de navegación en Internet",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Lugares", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=problemas)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/MEDIOBAJO_problema_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=20, units="cm"
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


ggplot(plan_cel, aes(y=Plan, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Planes", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/MEDIOBAJO_plan_cel.pdf",
    device="pdf", dpi="retina",
    width=19.17, height=14, units="cm"
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


ggplot(uso_cel, aes(y=Uso, weight=FAC_PER/1e3))+
    geom_bar(color=lmain, fill=main)+
    facet_wrap(
        ~Grupo, ncol = 2,  scales = "free",
        labeller = as_labeller(function(x){paste("Grupo",x)})
    )+
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Estrato Medio Bajo",
        caption = nota,
        y="Planes", x="Residentes (en miles)"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(aspect.ratio = 0.73)

ggsave(
    filename= "./Graficas/MEDIOBAJO_uso_cel.pdf",
    device="pdf", dpi="retina",
    width=25, height=20, units="cm"
)








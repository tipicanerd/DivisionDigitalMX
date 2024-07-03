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
base_path <- "../../ConjuntosDatos/"
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
estados <- read.csv("../../ConjuntosDatos/CodigoEdos.csv", stringsAsFactors = TRUE)

#COMPLETA
endutih_completa <- merge(endutih_vivhogar, select(endutih_res, select= -contains(c("UPM_DIS","ESTRATO","ENT","DOMINIO", "EST_DIS", "TLOC"))), by = c("UPM","VIV_SEL", "HOGAR") )

#IDTMex ESTADO
IDTMex_grupo_edo <- read.csv("../../ConjuntosDatos/IDTMex_grupos_edo.csv", stringsAsFactors = TRUE)


#### Variables universales ####
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


#### Significados de Variables que salen directas####

##Dominio-Estrato
estratos <- c("1"="Bajo", "2"="Medio Bajo", "3"="Medio Alto", "4"="Alto")
dominios <- c("R"="Rural", "U"="Urbano")


##Educacion
niveles <- c("Ninguno", "Preescolar", "Primaria", "Secundaria","Normal básica", "Estudio técnico", "Preparatoria", "Estudio técnico superior", "Licenciatura o ingeniería", "Especialidad", "Maestría", "Doctorado", "No sabe")

## Pisos
pisos <- c("1"="Tierra","2"="Cemento, firme", "3"="Madera,mosaico,\notro")


#### Preguntas y significados de variables que requieren pre-procesamiento####

#MODALIDAD INTERNET
pregs_modalidad <- paste("P5_7",1:8, sep="_")
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

#Aprendizaje compu
pregs_mod_aprendizaje <- paste("P6_6",1:7, sep="_")
modos <- c("Cuenta propia", "Trabajo", "Escuela", "Cursos pagados", "Cursos gratuitos", "Parientes", "Otro")

#Lugares Compu
pregs_lugs_compu <- paste("P6_7",1:8, sep="_")
lugs_compu <- c(
    "Hogar", "Trabajo", "Escuela",
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra persona", "Cualquier otro lugar\ncon una laptop",
    "Otro"
)

#Habilidades computadora
pregs_hab_compu <- paste("P6_8",1:10, sep="_")
habilidades_compu <- c(
    "Enviar y recibir\ncorreo", "Descargar contenidos\nde Internet",
    "Copiar archivos entre directorios", "Crear archivos de texto",
    "Crear hojas de cáculo", "Crear presentaciones",
    "Instalar dispositivos\nperiféricos", "Crear o usar\nbases de datos",
    "Programar en lenguaje\nespecializado", "Otras"
)

#Uso computadora
pregs_uso_compu <- paste("P6_9",1:6, sep="_")

usos_compu <- c(
    "Actividades laborales", "Labores escolares",
    "Medio de capacitación", "Entretenimiento",
    "Acceso a Internet", "Otro"
)

#Equipos Internet
pregs_equip <- paste("P7_5",1:7, sep="_")

equipos <- c(
    "Ordenador", "Laptop", "Tablet",
    "Smartphone", "Televisión", "Consola",
    "Otro"
)

#LUGARES INTERNET
pregs_lugs_internet <- paste("P7_7",1:8, sep="_")

lugs_internet <- c(
    "Hogar", "Trabajo", "Escuela", 
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra\npersona", "Cualquier lugar mediante\nsmartphone",
    "Otro"
)

# PROBLEMAS INTERNET
pregs_probs <- paste("P7_16",1:8, sep="_")

problemas <- c(
    "Infección por virus", "Exceso de información\nno deseada",
    "Interrupciones en el servicio", "Lentitud en la transferencia\nde información",
    "Fraudes con información", "Violación a la privacidad",
    "Mensajes de personas desconocidas", "Otro"
)

# TIPO DE CONTRATACION CELULAR
pregs_contrata_cel <- paste("P8_7", 1:3, sep="_")


planes <- c(
    "Recarga tiempo aire", "Paquetes", "Plan tarifario"
)



# USO DE APPS DE CELULAR
pregs_uso_cel <- paste("P8_12", 1:9, sep="_")

usos_cel <- c(
    "Mensajería instantánea", "Contenidos de audio y video", "Adquirir bienes o servicios",
    "Tránsito y navegación asistida", "Jugar", "Redes sociales", "Banca móvil",
    "Editar fotos o videos", "Otros"
)


####FUNCIONES PARA GRAFICAS####

barras_uniplot <- function(base, variable, peso, titulo, estrato, etiqueta_x, etiqueta_y){
    #' Funcion que crea un plot que no se encuentra dividido por alguna variable
    #' -------------------------------------------------------------------------
    #' PARAMETROS:
    #' @ base: base filtrada con la informacion
    #' @ variable: nombre de la varible de la base de interes
    #' @ peso: nombre de la variable donde se encuentra el peso
    #' @ titulo: Titulo de la grafica
    #' @ estrato: Estrato que se esta graficando
    #' @ etiqueta_x, etiqueta_y: Etiquetas de los ejes
    #' REGRESA:
    #' Plot con una grafica de barras
    
    ggplot(estrato1_viv, mapping = aes(y=factor(variable),weight=peso), )+
        geom_bar(aes(x=after_stat(prop)*100, group=factor(0)),fill=main, color=lmain)+
        labs(
            title = titulo,
            subtitle = estrato,
            caption = nota,
            y = etiqueta_y, x = etiqueta_x
        )+
        scale_x_continuous(n.breaks = 10)+
        scale_y_discrete(
            labels=pisos
        )+
        theme_light()+
        theme(axis.text.x = element_text(angle=90))
}

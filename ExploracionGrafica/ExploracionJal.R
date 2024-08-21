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

#Lista de edos 
edos <- c(2,  9, 14, 26)

#Variables universales
nota <- "Nota: Elaboración propia con datos de la ENDUTIH (2020)"
#colores
main <- "dodgerblue2"
lmain <- "dodgerblue4"

lcolor <- "royalblue4"
fcolor <- "royalblue"

nocolor <- "indianred3"
sicolor <- "steelblue2"

palette<-"ggsci::signature_substitutions_cosmic"
cpalette <- "ggthemes::Classic Blue"

#### JALISCO detalle####
edos <- 14
path <- "./Graficas/EDOSALTOS/Jal/"
#DOMINIO Grupo Codigo
viv <- endutih_vivhogar %>% filter(ENT %in% edos)
viv$Grupo<- paste("Grupo", viv$Grupo)
viv$ENT <-sapply(viv$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

res <- endutih_res %>% filter(ENT %in% edos)
res$Grupo<- paste("Grupo", res$Grupo)
res$ENT <-sapply(res$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

usu <- endutih_usu %>% filter(ENT %in% edos)
usu$Grupo<- paste("Grupo", usu$Grupo)
usu$ENT <-sapply(usu$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})

usu2 <- endutih_usu2 %>% filter(ENT %in% edos)
usu2$Grupo<- paste("Grupo", usu2$Grupo)
usu2$ENT <-sapply(usu2$ENT, function(x){gsub(x, estados[estados$Codigo==x, 3], x)})



# MATERIAL DE PISOS
pisos <- c("1"="Tierra","2"="Cemento, firme", "3"="Madera,mosaico,\notro")

ggplot(viv, mapping = aes(y=as.factor(P1_1),x=FAC_VIV), )+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)), color=lmain,fill=main)+
    facet_wrap(
         ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Material de piso de las viviendas",
        subtitle = "Jalisco",
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
    path=path, filename= "material_piso.pdf",
    device="pdf", dpi="retina",
    width=44, height=28, units="cm"
)

# EDAD 1+ 3.322*log10(sum(res$FAC_HOGAR))
ggplot(res, aes(EDAD, weight=FAC_HOGAR))+
    geom_histogram(aes(y=after_stat(density)*(100/25)*100), ,fill=main, bins = 26)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    geom_vline( xintercept =sum(res$FAC_HOGAR*res$EDAD)/sum(res$FAC_HOGAR), color=lmain)+
    labs(
        title = "Distribución de edad",
        subtitle = "Jalisco",
        caption = nota,
        x="Edad", y="% Residentes"
    )+
    scale_y_continuous(n.breaks = 10)+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "hist_edad.pdf",
    device="pdf", dpi="retina",
    width=44, height=28, units="cm"
)

#MODALIDAD INTERNET
pregs <- paste("P5_7",1:8, sep="_")
modalidad_servicio <- viv %>% 
    filter(P5_6_5!=1) %>% 
    select(all_of(c("FAC_HOG", "Grupo","ENT",pregs))) %>%
    transform(
        FAC_HOG = ave(FAC_HOG,Grupo, ENT,FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_HOG,Grupo, ENT), names_to = "Modalidad",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 

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

ggplot(modalidad_servicio, aes(y=Modalidad, x=FAC_HOG))+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Modalidad de contratación de servicios",
        subtitle = "Jalisco",
        caption = nota,
        y="Modalidad", x="% Hogares"
    )+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    scale_y_discrete(labels=modalidades)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "modalidad_compu.pdf",
    device="pdf", dpi="retina",
    width=44, height=28, units="cm"
)


#Uso internet

ggplot(res, aes(x = as.factor(Grupo), y = FAC_HOGAR, fill = as.factor(P3_9_2))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Uso de Internet",
        subtitle = "Jalisco",
        caption = nota,
        y="% Residentes", x="Grupo", fill= "Uso"
    )+
    scale_y_continuous(breaks = 10*(1:10)/100, labels = 10*(1:10)) +
    scale_fill_manual(values = c(sicolor, nocolor, "darkgray","gray"), labels= c("Sí", "No", "No sabe", "No respondió"))+
theme_light()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(
    path=path, filename= "uso_sino_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#Tipo de conexión a Internet

ggplot(viv, aes(x = as.factor(Grupo), y = FAC_HOG, fill = as.factor(P4_5))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Tipo de conexión a Internet",
        subtitle = "Jalisco",
        caption = nota,
        y="% HOGARES", x="Grupo", fill= "Tipo de conexión"
    )+
    scale_y_continuous(breaks = 10*(1:10)/100, labels = 10*(1:10)) +
    scale_fill_paletteer_d(palette , direction = 1,labels= c( "NA", "Fija", "Inalámbrica", "Ambas", "No sabe"))+
theme_light()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(
    path=path, filename= "conexion_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


ggplot(viv %>% filter(P4_4==1), aes(x = as.factor(Grupo), y = FAC_HOG, fill = as.factor(P4_5))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Tipo de conexión de Internet en hogares que disponen del recurso",
        subtitle = "Jalisco",
        caption = nota,
        y="% Residentes", x="Grupo", fill= "Tipo de conexión"
    )+
    scale_y_continuous(breaks = 10*(1:10)/100, labels = 10*(1:10)) +
    scale_fill_paletteer_d(palette ,labels= c("Fija", "Inalámbrica", "Ambas","No sabe"))+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(
    path=path, filename= "conexion_internet_hog_internet.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#Uso celular

ggplot(res, aes(x = as.factor(Grupo), y = FAC_HOGAR, fill = as.factor(P3_9_3))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Uso de celular",
        subtitle = "Jalisco",
        caption = nota,
        y="% Residentes", x="Grupo", fill= "Uso"
    )+
    scale_y_continuous(breaks = 10*(1:10)/100, labels = 10*(1:10)) +
    scale_fill_manual(values = c(sicolor, nocolor, "darkgray","gray"), labels= c("Sí", "No", "No sabe", "No respondió"))+
theme_light()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(
    path=path, filename= "uso_sino_cel.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#Uso compu

ggplot(usu, aes(x = as.factor(Grupo), y = FAC_PER, fill = as.factor(P6_1))) + 
    geom_bar(stat = "identity",position="fill")+
    labs(
        title = "Uso de computadora",
        subtitle = "Jalisco",
        caption = nota,
        y="% Usuarios", x="Grupo", fill= "Uso"
    )+
    scale_y_continuous(breaks = 10*(1:10)/100, labels = 10*(1:10)) +
    scale_fill_manual(values = c(sicolor, nocolor), labels= c("Sí", "No"))+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave(
    path=path, filename= "uso_sino_compu.pdf",
    device="pdf", dpi="retina",
    width=25, height=14, units="cm"
)


#Motivo no uso compu
razones <- c("1"="No tiene acceso a una, aunque sabe utilizarla", 
             "2"="No sabe utilizarla",
             "3"="No le interesa o no la necesita", 
             "4"="Por falta de recursos económicos", 
             "5"="Usa su teléfono celular inteligente (Smartphone)", 
             "6"="Desconoce su utilidad",
             "7"="Por discapacidad física o mental", 
             "8"="Otra razón"
             )

ggplot(usu[usu$P6_1==2,], mapping = aes(y=as.factor(P6_3),x=FAC_PER), )+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)), color=lmain,fill=main)+
    labs(
        title = "Motivo del no uso de computadora",
        subtitle = "Jalisco",
        caption = nota,
        y="Motivos", x="% Usuarios que no usan computadora"
    )+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    scale_y_discrete(labels=razones)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "razon_no_uso_compu.pdf",
    device="pdf", dpi="retina",
    width=44, height=35, units="cm"
)


#Aprendizaje compu
pregs <- paste("P6_6",1:7, sep="_")
aprendizaje_compu <- usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo, ENT), names_to = "Metodo",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)

modos <- c("Cuenta propia", "Trabajo", "Escuela", "Cursos pagados", "Cursos gratuitos", "Parientes", "Otro")

ggplot(aprendizaje_compu, aes(y=Metodo, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Modo de aprendizaje para el uso de computadora",
        subtitle = "Jalisco",
        caption = nota,
        y="Modo", x="% Residentes"
    )+
    scale_y_discrete(labels=modos)+
    scale_x_continuous(n.breaks = 8)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "modo_compu.pdf",
    device="pdf", dpi="retina",
    width=44, height=35, units="cm"
)

#Lugares
pregs <- paste("P6_7",1:8, sep="_")

lugares_compu <- usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo","ENT", pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo, ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo, ENT), names_to = "Lugar",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)

lugares <- c(
    "Hogar", "Trabajo", "Escuela",
    "Sitio público\ncon costo", "Sitio público\nsin costo",
    "Casa de otra persona", "Cualquier otro lugar\ncon una laptop",
    "Otro"
)


ggplot(lugares_compu, aes(y=Lugar, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Lugares de uso de computadora",
        subtitle = "Jalisco",
        caption = nota,
        y="Lugar", x="% Residentes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "lugar_compu.pdf",
    device="pdf", dpi="retina",
    width=44, height=28, units="cm"
)


#Habilidades
pregs <- paste("P6_8",1:10, sep="_")
habilidades_compu <- usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Habilidad",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 

habilidades <- c(
    "Enviar y recibir\ncorreo", "Descargar contenidos\nde Internet",
    "Copiar archivos entre directorios", "Crear archivos de texto",
    "Crear hojas de cáculo", "Crear presentaciones",
    "Instalar dispositivos\nperiféricos", "Crear o usar\nbases de datos",
    "Programar en lenguaje\nespecializado", "Otras"
)


ggplot(habilidades_compu, aes(y=Habilidad, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Habilidades con la computadora",
        subtitle = "Jalisco",
        caption = nota,
        y="Habilidad", x="% Residentes"
    )+
    scale_y_discrete(labels=habilidades)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "habilidad_compu.pdf",
    device="pdf", dpi="retina",
    width=40, height=45, units="cm"
)

#Usos compu
pregs <- paste("P6_9",1:6, sep="_")
usos_compu <- usu %>% 
    filter(P6_1==1) %>% 
    select(all_of(c("FAC_PER", "Grupo","ENT", pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Uso",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1)

usos <- c(
    "Actividades laborales", "Labores escolares",
    "Medio de capacitación", "Entretenimiento",
    "Acceso a Internet", "Otro"
)


ggplot(usos_compu, aes(y=Uso, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Usos de la computadora",
        subtitle = "Jalisco",
        caption = nota,
        y="Usos", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "uso_compu.pdf",
    device="pdf", dpi="retina",
    width=44, height=20, units="cm"
)

# INTENSIDAD DE USO DE INTERNET
ggplot(usu %>% filter(P7_1==1), aes(P7_4, weight=FAC_PER))+
    geom_histogram(aes(y=after_stat(density)*100),fill=main, bins = 12)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Distribución de la intensidad de uso de Internet",
        subtitle = "Jalisco",
        caption = nota,
        x="Horas de uso en un día", y="% Residentes"
    )+
    scale_x_continuous(n.breaks = 13)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "hist_intensidad_internet.pdf",
    device="pdf", dpi="retina",
    width=44, height=35, units="cm"
)

#EQUIPO INTERNET
pregs <- paste("P7_5",1:7, sep="_")
#estrato1_usu[,pregs] <- as.character(estrato1_usu[,pregs])
equipo_internet <- usu %>% 
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
    geom_bar(stat="identity",fill=main)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Equipos donde utiliza Internet",
        subtitle = "Jalisco",
        caption = nota,
        y="Equipos", x="% Residentes"
    )+
    scale_y_discrete(labels=equipos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90) )

ggsave(
    path=path, filename= "equipo_internet.pdf",
    device="pdf", dpi="retina",
    width=44, height=35, units="cm"
)

#LUGARES INTERNET
pregs <- paste("P7_7",1:8, sep="_")

lugar_internet <- usu %>% 
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
    geom_bar(stat="identity",fill=main)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Lugares donde utiliza Internet",
        subtitle = "Jalisco",
        caption = nota,
        y="Lugares", x="% Residentes"
    )+
    scale_y_discrete(labels=lugares)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "lugar_internet.pdf",
    device="pdf", dpi="retina",
    width=44, height=28, units="cm"
)


# PROBLEMAS INTERNET
pregs <- paste("P7_16",1:8, sep="_")

problema_internet <- usu %>% 
    select(all_of(c("FAC_PER", "Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo, ENT), names_to = "Problema",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 


problemas <- c(
    "Infección por virus", "Exceso de información\nno deseada",
    "Interrupciones en el servicio", "Lentitud en la transferencia\nde información",
    "Fraudes con información", "Violación a la privacidad",
    "Mensajes de personas desconocidas", "Otro"
)


ggplot(problema_internet, aes(y=Problema, x=FAC_PER))+
    geom_bar(stat="identity",fill=main)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Problemas de navegación en Internet",
        subtitle = "Jalisco",
        caption = nota,
        y="Lugares", x="% Residentes"
    )+
    scale_y_discrete(labels=problemas)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme()

ggsave(
    path=path, filename= "problema_internet.pdf",
    device="pdf", dpi="retina",
    width=59, height=33, units="cm"
)

# TIPO DE CONTRATACION CELULAR
pregs <- paste("P8_7", 1:3, sep="_")

plan_cel <- usu2 %>% 
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
    geom_bar(stat="identity",fill=main)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Planes de contratación de servicio de celular",
        subtitle = "Jalisco",
        caption = nota,
        y="Planes", x="% Residentes"
    )+
    scale_y_discrete(labels=planes)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "plan_cel.pdf",
    device="pdf", dpi="retina",
    width=44, height=35, units="cm"
)

# USO DE APPS DE CELULAR
pregs <- paste("P8_12", 1:9, sep="_")


uso_cel <- usu2 %>% 
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
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Uso de aplicaciones móviles",
        subtitle = "Jalisco",
        caption = nota,
        y="Usos", x="% Residentes"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "uso_cel.pdf",
    device="pdf", dpi="retina",
    width=44, height=20, units="cm"
)


# USO DE INTERNET CAPACITACION
pregs <- paste("P7_9", 1:4, sep="_")
usos <- c(
    "Capacitación para el trabajo",
    "Complementar la educación",
    "Tomar tutoriales sobre temas de interés",
    "Otro tipo de capacitaciones"
    )

uso_int <- usu %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Uso",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 



ggplot(uso_int, aes(y=Uso, x=FAC_PER))+
    geom_bar(stat="identity", fill=main)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Uso de Internet como medio de capacitación",
        subtitle = "Jalisco",
        caption = nota,
        y="Usos", x="% Usuarios"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "uso_internet_capacitacion.pdf",
    device="pdf", dpi="retina",
    width=44, height=20, units="cm"
)



# USO DE INTERNET comunicacion
pregs <- paste("P7_10", 1:4, sep="_")
usos <- c(
    "Enviar correos electrónicos",
    "Realizar conversaciones telefónicas",
    "Enviar mensajes instantáneos",
    "Otro medio de comunicación"
)

uso_int <- usu %>% 
    select(all_of(c("FAC_PER","Grupo","ENT",pregs))) %>%
    transform(
        FAC_PER = ave(FAC_PER,Grupo,ENT, FUN = prop.table)*100
    )%>%
    pivot_longer(-c(FAC_PER, Grupo,ENT), names_to = "Uso",  values_to = "respuesta", values_drop_na = TRUE) %>%
    filter(respuesta==1) 



ggplot(uso_int, aes(y=Uso, x=FAC_PER))+
    geom_bar(stat="identity",fill=main)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Uso de Internet como medio de comunicación",
        subtitle = "Jalisco",
        caption = nota,
        y="Usos", x="% Usuarios"
    )+
    scale_y_discrete(labels=usos)+
    scale_x_continuous(n.breaks = 10)+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90))

ggsave(
    path=path, filename= "uso_internet_comunicacion.pdf",
    device="pdf", dpi="retina",
    width=44, height=20, units="cm"
)

# SITUACION LABORAL
sit_trabajo <- c(
    "Trabajó","No fue al trabajo", 
    "Buscó trabajo", "Está jubilado", 
    "Se dedicó a estudiar", "Quehaceres del hogar", 
    "Limitación para trabajar", "No trabajó"
    )

ggplot(res%>%filter(P3_10!="NA"), mapping = aes(y=as.factor(P3_10),x=FAC_HOGAR), )+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)), color=lmain,fill=main)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Actividad laboral en la semana pasada",
        subtitle = "Jalisco",
        caption = nota,
        y="Actividad Laboral", x="% Residentes"
    )+
    scale_y_discrete(
        labels=sit_trabajo
    )+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90), 
        axis.text.y  = element_text(angle = 0)
    )


ggsave(
    path=path, filename= "trabajo.pdf",
    device="pdf", dpi="retina",
    width=44, height=35, units="cm"
)


#ESCOLARIDAD
niveles <- c("Ninguno", "Preescolar", "Primaria", "Secundaria","Normal básica", "Estudio técnico", "Preparatoria", "Estudio técnico superior", "Licenciatura o ingeniería", "Especialidad", "Maestría", "Doctorado", "No sabe")

ggplot(usu%>%filter(NIVEL!="NA"), mapping = aes(y=as.factor(NIVEL),x=FAC_PER), )+
    geom_bar(aes(x=after_stat(prop)*100, group=factor(0)), color=lmain,fill=main)+
    facet_wrap(
        ~Grupo, nrow = 4, scales = "fixed",
    )+
    labs(
        title = "Nivel educativo",
        subtitle = "Jalisco",
        caption = nota,
        y="Nivel educativo", x="% Usuarios"
    )+
    scale_y_discrete(
        labels=niveles
    )+
    theme_light()+
    theme(
        axis.text.x = element_text(angle=90), 
        axis.text.y  = element_text(angle = 0)
    )


ggsave(
    path=path, filename= "edu.pdf",
    device="pdf", dpi="retina",
    width=44, height=35, units="cm"
)


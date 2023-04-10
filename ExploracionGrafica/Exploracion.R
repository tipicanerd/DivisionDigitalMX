#install.packages("pacman")
library(pacman)

p_load(
    dplyr,
    ggplot2,
    tidyr,
    paletteer,
    plyr
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
    paste(endutih_path,"tr_endutih_residente_anual_2020.csv", sep=""),
    stringsAsFactors = TRUE
    )
endutih_res <- select(endutih_res, select= -contains(c("UPM_DIS","ESTRATO","ENT","DOMINIO", "EST_DIS", "TLOC")))


#COMPLETA
endutih_completa <- merge(endutih_vivhogar, endutih_res, by = c("UPM","VIV_SEL", "HOGAR") )

rm(endutih_res, endutih_vivhogar)


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
cpalette <- "ggthemes::Blue"



#### ESTRATO BAJO ####
grupo11 <- endutih_completa %>% dplyr::filter(Grupo==11)

# -[] Material de los pisos
g11_pisos <- grupo11 %>% group_by(P1_1) %>% dplyr::summarise(Total= sum(FAC_VIV))
g11_pisos$P1_1 <- as.factor(g11_pisos$P1_1)

ggplot(g11_pisos, mapping = aes(x=P1_1,y=Total/1e5), )+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Material de piso de las viviendas",
        subtitle = "Grupo 11\n(Cada 10,000 viviendas)",
        caption = nota,
        x="Material", y="Viviendas"
        )+
    scale_y_continuous(n.breaks = 7)+
    scale_x_discrete(
        labels=c("1"="Tierra","2"="Cemento, firme", "3"="Madera,mosaico,\notro")
        )+
    theme_light()+
    theme(axis.text.x = element_text(angle=90))

# -[] Drenaje
g11_drenaje <- grupo11 %>% group_by(P1_3) %>% dplyr::summarise(Total= sum(FAC_VIV))
g11_drenaje$P1_3 <- as.factor(g11_drenaje$P1_3)

ggplot(g11_drenaje, mapping = aes(x=P1_3,y=Total/1e5), )+
    geom_bar(stat="identity", fill=main)+
    labs(
        title = "Situación del drenaje de las viviendas",
        subtitle = "Grupo 11\n(Cada 10,000 viviendas)",
        caption = nota,
        x="Material", y="Viviendas"
    )+
    scale_y_continuous(n.breaks = 7)+
    scale_x_discrete(
        labels=c(
            "1"="Red pública", "2"="Fosa séptica",
            "3"="Tubería que da\n a una barranca", "4"="Tubería que da\na un río",
            "5"="Sin drenaje"
        )
    )+
    theme_light()+
    theme(axis.text.x = element_text(angle=90, hjust = 0.5))

# -[] Electricidad
g11_electricidad <- grupo11 %>% group_by(P1_4) %>% dplyr::summarise(Total= sum(FAC_VIV))

ggplot(g11_electricidad, aes(x = "", y = Total, fill = as.factor(P1_4))) +
    geom_col(color = "black") +
    coord_polar(theta = "y",start=0) +
    geom_text(aes(label = paste(round(Total/sum(Total)*100,2),"%") ),
              position = position_stack(vjust = 0.5), size=5, color=c("black","black")) +
    scale_fill_manual(values=c(sicolor,nocolor),labels=c('Sí','No'),"") +
    labs(title = "Disponibilidad de electricidad de las viviendas",
         subtitle = "Grupo 11",
         caption=nota) +
    theme_void()
    
    






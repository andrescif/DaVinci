library(tidyverse)
#Cargar datos
Cursos_Galileo <- read.csv("~/R Projects/DaVinci/Datos/Cursos_Carreras_Galileo.csv")
#Establecer variables
class(Cursos_Galileo$Carrera)
Cursos_Galileo$Carrera <- as.factor(Cursos_Galileo$Carrera)
levels(Cursos_Galileo$Carrera)

class(Cursos_Galileo$Curso)
Cursos_Galileo$Curso <- as.factor(Cursos_Galileo$Curso)
levels(Cursos_Galileo$Curso)

class(Cursos_Galileo$Cantidad.de.créditos)
class(Cursos_Galileo$Trimestre)
class(Cursos_Galileo$Año)
Cursos_Galileo$Año <- as.factor(Cursos_Galileo$Año)
class(Cursos_Galileo$Tipo.de.curso)
Cursos_Galileo$Tipo.de.curso <- as.factor(Cursos_Galileo$Tipo.de.curso)
levels(Cursos_Galileo$Tipo.de.curso)
#Cursos
x <- Cursos_Galileo %>%
        group_by(Carrera,Tipo.de.curso) %>%
        summarise(n(),Creditos=sum(Cantidad.de.créditos))
write.csv(x,
          file = "Tabla de Carreras, cursos y créditos Galileo")        
rm(x)

#Hacer subgrupos
levels(Cursos_Galileo$Carrera)
CienciaYTec <- subset.data.frame(Cursos_Galileo,
                                 Cursos_Galileo$Carrera=="Licenciatura en Ciencia y Tecnología del Deporte")
GestionEnt <- subset.data.frame(Cursos_Galileo,
                                Cursos_Galileo$Carrera=="Licenciatura en Gestión de Entidades Deportivas")
TecnicoAdminDepor <- subset.data.frame(Cursos_Galileo,
                                       Cursos_Galileo$Carrera=="Técnico Universitario en Administración de Instituciones Deportivas")

#Graficar
ggplot(CienciaYTec,
       aes(Año,fill=Tipo.de.curso))+
        theme_bw()+
        geom_bar(position = "dodge")+
        ggtitle("Tipos de Cursos por Año
Licenciatura en Cienica y Tecnología del Deporte")+
        guides(fill=guide_legend(title="Tipo de curso"))+
        ylab("Cantidad de cursos")+
        scale_y_continuous(breaks=c(0,2,4,6,8,10))

ggplot(GestionEnt,
       aes(Año,fill=Tipo.de.curso))+
        theme_bw()+
        geom_bar(position = "dodge")+
        ggtitle("Tipos de Cursos por Año
Licenciatura en Gestión de Entidades Deportivas")+
        guides(fill=guide_legend(title="Tipo de curso"))+
        ylab("Cantidad de cursos")+
        scale_y_continuous(breaks=c(0,2,4,6,8))

ggplot(TecnicoAdminDepor,
       aes(Año,fill=Tipo.de.curso))+
        theme_bw()+
        geom_bar(position = "dodge")+
        ggtitle("Tipos de Cursos por Año
Técnico Universitario en Administración de Instituciones Deportivas")+
        guides(fill=guide_legend(title="Tipo de curso"))+
        ylab("Cantidad de cursos")+
        scale_y_continuous(breaks=c(0,2,4,6,8,10))




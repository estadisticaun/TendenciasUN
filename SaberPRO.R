# ANÁLISIS DE RESULTADOS DE LOS EXAMENES SABER PRO

# LIBRERÍAS REQUERIDAS
library(readxl)
library(tidyverse)
library(ggrepel)
library(plotly)


# IMPORTAR DATOS SABER PRO 2017

SBPRO_2017_GEN <- read_excel("Datos/SBPRO-2017-GEN.xlsx")

# SELECCIONAR VARIABLES DE INTERÉS

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% select(c(INST_COD_INSTITUCION:GRUPOREFERENCIA, 
                                              MOD_RAZONA_CUANTITAT_PUNT, MOD_RAZONA_CUANTITATIVO_PNAL, MOD_RAZONA_CUANTITATIVO_PGREF,
                                              MOD_LECTURA_CRITICA_PUNT, MOD_LECTURA_CRITICA_PNAL, MOD_LECTURA_CRITICA_PGREF,
                                              MOD_COMPETEN_CIUDADA_PUNT, MOD_COMPETEN_CIUDADA_PNAL, MOD_COMPETEN_CIUDADA_PGREF,
                                              MOD_INGLES_PUNT, MOD_INGLES_PNAL, MOD_INGLES_PGREF, MOD_COMUNI_ESCRITA_PUNT,
                                              MOD_COMUNI_ESCRITA_PNAL, MOD_COMUNI_ESCRITA_PGREF, PUNT_GLOBAL, PERCENTIL_GLOBAL))

# CREAR VARIABLES DE INTERÉS

# Crear Variable Unal

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(Unal =  case_when(.$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104, 1124, 1125, 1126, 9920) ~ "UN",
                           TRUE ~ "Resto IES"))
# Crear Sedes Unal

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(Sedes =  case_when(.$INST_COD_INSTITUCION == 1101 ~ "UN-Bogotá",
                            .$INST_COD_INSTITUCION == 1102 ~ "UN-Medellín",
                            .$INST_COD_INSTITUCION == 1103 ~ "UN-Manizales",
                            .$INST_COD_INSTITUCION == 1104 ~ "UN-Palmira",
                            .$INST_COD_INSTITUCION == 1124 ~ "UN-Orinoquía",
                            .$INST_COD_INSTITUCION == 1125 ~ "UN-Amazonía",
                            .$INST_COD_INSTITUCION == 1126 ~ "UN-Caribe",
                            .$INST_COD_INSTITUCION == 9920 ~ "UN-Tumaco",
                            TRUE ~ "Resto IES"))

# Crear G12

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(G12 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1222, 1223, 9125) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION %in% c(1101, 1102, 1103, 1104)  ~ "U. Nacional",
                          TRUE ~ "Resto IES"))

# Crear G16 (sedes Unal)
SBPRO_2017_GEN <- SBPRO_2017_GEN %>% 
  mutate(G15 =  case_when(.$INST_COD_INSTITUCION == 1813  ~ "U. de los Andes",
                          .$INST_COD_INSTITUCION == 1714  ~ "U. del Rosario",
                          .$INST_COD_INSTITUCION == 1706  ~ "U. Externado",
                          .$INST_COD_INSTITUCION == 1828  ~ "ICESI",
                          .$INST_COD_INSTITUCION %in% c(1201, 1219, 1222, 1223, 9125) ~ "U. de Antioquia",
                          .$INST_COD_INSTITUCION == 1712  ~ "EAFIT",
                          .$INST_COD_INSTITUCION == 1203  ~ "U. del Valle",
                          .$INST_COD_INSTITUCION %in% c(1701, 1702)  ~ "U. Javeriana",
                          .$INST_COD_INSTITUCION == 1204  ~ "UIS",
                          .$INST_COD_INSTITUCION == 1713  ~ "U. del Norte",
                          .$INST_COD_INSTITUCION %in% c(1710, 1723, 1727, 1730)  ~ "U. Bolivariana",
                          .$INST_COD_INSTITUCION == 1101  ~ "UN-Bogotá",
                          .$INST_COD_INSTITUCION == 1102  ~ "UN-Medellín",
                          .$INST_COD_INSTITUCION == 1103  ~ "UN-Manizales",
                          .$INST_COD_INSTITUCION == 1104  ~ "UN-Palmira",
                          TRUE ~ "Resto IES"))

# Remover datos faltantes de grupos de referencia

SBPRO_2017_GEN <- SBPRO_2017_GEN %>% filter(!is.na(GRUPOREFERENCIA))

#######################################################
################# ANÁLISIS POR IES
#######################################################

################ BOX PLOT DE PUNTAJES GLOBALES 

# Puntos mínimos y máximos (SaberPRO - Interpretación)

ggplot(data = SBPRO_2017_GEN, aes(y = PUNT_GLOBAL, x = Unal)) +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1)+
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") 

# Distribución puntajes globales 

ggplot(data = SBPRO_2017_GEN, aes(y = PUNT_GLOBAL, x = 1)) + geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1)+
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") 

# Distribución puntajes globales y Unal

ggplot(data = SBPRO_2017_GEN, aes(y = PUNT_GLOBAL, x = Unal)) + geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") 

# Distribución puntajes globales, unal y sedes

### crear base de datos 

Unal <- SBPRO_2017_GEN %>% select(PUNT_GLOBAL, Unal) %>% rename(Categoria = Unal)
sedes <- SBPRO_2017_GEN %>% select(PUNT_GLOBAL, Sedes) %>% 
  filter(Sedes != "Resto IES") %>% rename(Categoria = Sedes)
Global <- bind_rows(Unal, sedes)

### Puntajes Sedes Unal y Resto

ggplot(data = Global, aes(y = PUNT_GLOBAL, x = fct_reorder(Categoria, PUNT_GLOBAL, fun = median, .desc =TRUE))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Categorías")


# Distribución G12

ggplot(data = SBPRO_2017_GEN, aes(y = PUNT_GLOBAL, x = fct_reorder(G12, PUNT_GLOBAL, fun = median))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()

# Distribución G15

ggplot(data = SBPRO_2017_GEN, aes(y = PUNT_GLOBAL, x = fct_reorder(G15, PUNT_GLOBAL, fun = median))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()

###### DISTRIBUCIÓN POR PRUEBAS GENÉRICAS

### Competencias Ciudadanas

ggplot(data = SBPRO_2017_GEN, aes(y = MOD_COMPETEN_CIUDADA_PUNT, x = fct_reorder(G15, MOD_COMPETEN_CIUDADA_PUNT, fun = median(na.rm = TRUE)))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()+
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", color="black", size=16)
  )

### Lectura Crítica

ggplot(data = SBPRO_2017_GEN, aes(y = MOD_LECTURA_CRITICA_PUNT, x = fct_reorder(G15, MOD_LECTURA_CRITICA_PUNT, fun = median(na.rm = TRUE)))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()+
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", color="black", size=16)
  )

### Razonamiento Cuantitativo

ggplot(data = SBPRO_2017_GEN, aes(y = MOD_RAZONA_CUANTITAT_PUNT, x = fct_reorder(G15, MOD_RAZONA_CUANTITAT_PUNT, fun = median(na.rm = TRUE)))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()+
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", color="black", size=16)
  )

### Comunicación Escrita

SBPRO_2017_GEN1 <- SBPRO_2017_GEN %>% filter(!is.na(MOD_COMUNI_ESCRITA_PUNT))

ggplot(data = SBPRO_2017_GEN1, aes(y = MOD_COMUNI_ESCRITA_PUNT, x = fct_reorder(G15, MOD_COMUNI_ESCRITA_PUNT, fun = median(na.rm = TRUE)))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()+
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", color="black", size=16)
  )

### Inglés

ggplot(data = SBPRO_2017_GEN, aes(y = MOD_INGLES_PUNT, x = fct_reorder(G15, MOD_INGLES_PUNT, fun = median(na.rm = TRUE)))) + 
  geom_boxplot(outlier.color = "green", fill = "gray") +
  ylim(0, 300) + geom_hline(yintercept = 180, col = "red", size = 1) +
  geom_hline(yintercept = c(0, 300), col = "blue", size = 1, linetype="dashed") +
  ylab("Puntaje Promedio Global SaberPRO 2017")+
  xlab("Universidades")+coord_flip()+
  theme(axis.text.y = element_text(size = 13, face = "bold"),
        axis.title = element_text(face="bold", color="black", size=16)
  )

####################################################################
################# ANÁLISIS POR FACULTADES Y PROGRAMAS ACADÉMICOS
####################################################################


# RAZONAMIENTO CUANTITATIVO

QCuantitativo <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_RAZONA_CUANTITATIVO_PNAL <= 20  ~ "Quintil1",
                                .$MOD_RAZONA_CUANTITATIVO_PNAL > 20 & .$MOD_RAZONA_CUANTITATIVO_PNAL <= 40 ~ "Quintil2",
                                .$MOD_RAZONA_CUANTITATIVO_PNAL > 40 & .$MOD_RAZONA_CUANTITATIVO_PNAL <= 60 ~ "Quintil3",
                                .$MOD_RAZONA_CUANTITATIVO_PNAL > 60 & .$MOD_RAZONA_CUANTITATIVO_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
                                )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", ""))
 
# Insertar Facultades

Facultades <- read_excel("Datos/Facultades.xlsx")

QCuantitativo <- left_join(QCuantitativo, Facultades, by = "ESTU_SNIES_PRGMACADEMICO")


RC_Facul <- QCuantitativo %>%  group_by(SEDE, FACULTAD) %>% 
            summarise(Quintil1 = sum(Quintil1), Quintil2 = sum(Quintil2),
                      Quintil3 = sum(Quintil3), Quintil4 = sum(Quintil4),
                      Quintil5 = sum(Quintil5)) %>% 
            mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
            mutate(Quintil1P = round((Quintil1/Total)*100, 0),
            Quintil2P = round((Quintil2/Total)*100, 0),
            Quintil3P = round((Quintil3/Total)*100, 0),
            Quintil4P = round((Quintil4/Total)*100, 0),
            Quintil5P = round((Quintil5/Total)*100, 0))
# Gráfico

p <- ggplot(RC_Facul, aes(x = Quintil1P, y = Quintil5P , color = SEDE, label = FACULTAD)) + 
      geom_point(size = 6) + xlim(0, 30) + ylim(0, 100)+
      geom_text_repel(col = "black", size = 4, force = 5) +
      geom_hline(yintercept = 50, col = "green", size = 2) +
      geom_vline(xintercept = 20, col = "#e34a33", size = 2) +
      annotate("text", x = 12, y = 75, label = "Escenario ideal", colour = "gray", size = 10) +
      annotate("text", x = 12, y = 20, label = "Escenario aceptable", colour = "gray", size = 10) +
      annotate("text", x = 27, y = 20, label = "Escenario negativo", colour = "gray", size = 10) +
      annotate("text", x = 27, y = 75, label = "Escenario atípico", colour = "gray", size = 10) +
      scale_colour_brewer(palette = "Set1") +
      ylab("Proporción de estudiantes en el quintil 5 (%)  \n") +
      xlab("\n Proporción de estudiantes en el quintil 1 (%)") +
      theme(
      axis.title.x = element_text(size=14, face="bold"),
      axis.title.y = element_text(size=14, face="bold"),
      axis.text = element_text(size = 12)
      )
  
ggplotly(p)

# COMPETENCIAS CIUDADANAS


QCuantitativo <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_COMPETEN_CIUDADA_PNAL <= 20  ~ "Quintil1",
                                .$MOD_COMPETEN_CIUDADA_PNAL > 20 & .$MOD_COMPETEN_CIUDADA_PNAL <= 40 ~ "Quintil2",
                                .$MOD_COMPETEN_CIUDADA_PNAL > 40 & .$MOD_COMPETEN_CIUDADA_PNAL <= 60 ~ "Quintil3",
                                .$MOD_COMPETEN_CIUDADA_PNAL > 60 & .$MOD_COMPETEN_CIUDADA_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", ""))



# Insertar Facultades

Facultades <- read_excel("Datos/Facultades.xlsx")

QCuantitativo <- left_join(QCuantitativo, Facultades, by = "ESTU_SNIES_PRGMACADEMICO")


RC_Facul <- QCuantitativo %>%  group_by(SEDE, FACULTAD) %>% 
  summarise(Quintil1 = sum(Quintil1), Quintil2 = sum(Quintil2),
            Quintil3 = sum(Quintil3), Quintil4 = sum(Quintil4),
            Quintil5 = sum(Quintil5)) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0))

# Gráfico

ggplot(RC_Facul, aes(x = Quintil1P, y = Quintil5P , color = SEDE, label = FACULTAD)) + 
  geom_point(size = 6) + xlim(0, 30) + ylim(0, 100)+
  geom_text_repel(col = "black", size = 4, force = 5) +
  geom_hline(yintercept = 50, col = "green", size = 2) +
  geom_vline(xintercept = 20, col = "#e34a33", size = 2) +
  annotate("text", x = 12, y = 75, label = "Escenario ideal", colour = "gray", size = 10) +
  annotate("text", x = 12, y = 20, label = "Escenario aceptable", colour = "gray", size = 10) +
  annotate("text", x = 27, y = 20, label = "Escenario negativo", colour = "gray", size = 10) +
  annotate("text", x = 27, y = 75, label = "Escenario atípico", colour = "gray", size = 10) +
  scale_colour_brewer(palette = "Set1") +
  ylab("Proporción de estudiantes en el quintil 5 (%)  \n") +
  xlab("\n Proporción de estudiantes en el quintil 1 (%)") +
  theme(
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text = element_text(size = 12)
  )



# LECTURA CRÍTICA


QCuantitativo <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_LECTURA_CRITICA_PNAL <= 20  ~ "Quintil1",
                                .$MOD_LECTURA_CRITICA_PNAL > 20 & .$MOD_LECTURA_CRITICA_PNAL <= 40 ~ "Quintil2",
                                .$MOD_LECTURA_CRITICA_PNAL > 40 & .$MOD_LECTURA_CRITICA_PNAL <= 60 ~ "Quintil3",
                                .$MOD_LECTURA_CRITICA_PNAL > 60 & .$MOD_LECTURA_CRITICA_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", ""))



# Insertar Facultades

Facultades <- read_excel("Datos/Facultades.xlsx")

QCuantitativo <- left_join(QCuantitativo, Facultades, by = "ESTU_SNIES_PRGMACADEMICO")


RC_Facul <- QCuantitativo %>%  group_by(SEDE, FACULTAD) %>% 
  summarise(Quintil1 = sum(Quintil1), Quintil2 = sum(Quintil2),
            Quintil3 = sum(Quintil3), Quintil4 = sum(Quintil4),
            Quintil5 = sum(Quintil5)) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0))

# Gráfico

ggplot(RC_Facul, aes(x = Quintil1P, y = Quintil5P , color = SEDE, label = FACULTAD)) + 
  geom_point(size = 6) + xlim(0, 30) + ylim(0, 100)+
  geom_text_repel(col = "black", size = 4, force = 5) +
  geom_hline(yintercept = 50, col = "green", size = 2) +
  geom_vline(xintercept = 20, col = "#e34a33", size = 2) +
  annotate("text", x = 12, y = 75, label = "Escenario ideal", colour = "gray", size = 10) +
  annotate("text", x = 12, y = 20, label = "Escenario aceptable", colour = "gray", size = 10) +
  annotate("text", x = 27, y = 20, label = "Escenario negativo", colour = "gray", size = 10) +
  annotate("text", x = 27, y = 75, label = "Escenario atípico", colour = "gray", size = 10) +
  scale_colour_brewer(palette = "Set1") +
  ylab("Proporción de estudiantes en el quintil 5 (%)  \n") +
  xlab("\n Proporción de estudiantes en el quintil 1 (%)") +
  theme(
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text = element_text(size = 12)
  )




# COMUNICACIÓN ESCRITA


QCuantitativo <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_COMUNI_ESCRITA_PNAL <= 20  ~ "Quintil1",
                                .$MOD_COMUNI_ESCRITA_PNAL > 20 & .$MOD_COMUNI_ESCRITA_PNAL <= 40 ~ "Quintil2",
                                .$MOD_COMUNI_ESCRITA_PNAL > 40 & .$MOD_COMUNI_ESCRITA_PNAL <= 60 ~ "Quintil3",
                                .$MOD_COMUNI_ESCRITA_PNAL > 60 & .$MOD_COMUNI_ESCRITA_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", ""))



# Insertar Facultades

Facultades <- read_excel("Datos/Facultades.xlsx")

QCuantitativo <- left_join(QCuantitativo, Facultades, by = "ESTU_SNIES_PRGMACADEMICO")


RC_Facul <- QCuantitativo %>%  group_by(SEDE, FACULTAD) %>% 
  summarise(Quintil1 = sum(Quintil1), Quintil2 = sum(Quintil2),
            Quintil3 = sum(Quintil3), Quintil4 = sum(Quintil4),
            Quintil5 = sum(Quintil5)) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0))

# Gráfico

ggplot(RC_Facul, aes(x = Quintil1P, y = Quintil5P , color = SEDE, label = FACULTAD)) + 
  geom_point(size = 6) + xlim(0, 30) + ylim(0, 100)+
  geom_hline(yintercept = 50, col = "green", size = 2) +
  geom_vline(xintercept = 20, col = "#e34a33", size = 2) +
  geom_text_repel(col = "black", size = 4, force = 5) +
  annotate("text", x = 12, y = 75, label = "Escenario ideal", colour = "gray", size = 10) +
  annotate("text", x = 12, y = 20, label = "Escenario aceptable", colour = "gray", size = 10) +
  annotate("text", x = 27, y = 20, label = "Escenario negativo", colour = "gray", size = 10) +
  annotate("text", x = 27, y = 75, label = "Escenario atípico", colour = "gray", size = 10) +
  scale_colour_brewer(palette = "Set1") +
  ylab("Proporción de estudiantes en el quintil 5 (%)  \n") +
  xlab("\n Proporción de estudiantes en el quintil 1 (%)") +
  theme(
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text = element_text(size = 12)
  )

# INGLÉS


QCuantitativo <- SBPRO_2017_GEN %>% 
  mutate(QRazCuant =  case_when(.$MOD_INGLES_PNAL <= 20  ~ "Quintil1",
                                .$MOD_INGLES_PNAL > 20 & .$MOD_INGLES_PNAL <= 40 ~ "Quintil2",
                                .$MOD_INGLES_PNAL > 40 & .$MOD_INGLES_PNAL <= 60 ~ "Quintil3",
                                .$MOD_INGLES_PNAL > 60 & .$MOD_INGLES_PNAL <= 80 ~ "Quintil4",
                                TRUE ~ "Quintil5"
  )) %>% 
  select(G12,G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  filter(G12 == "U. Nacional") %>% 
  group_by(G15, ESTU_SNIES_PRGMACADEMICO, ESTU_PRGM_ACADEMICO, QRazCuant) %>% 
  summarise(Total = n()) %>% spread(QRazCuant, Total) %>% 
  replace(., is.na(.), 0) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0)) %>% 
  ungroup() %>% 
  mutate(G15 = str_replace(G15, "UN-", ""))



# Insertar Facultades

Facultades <- read_excel("Datos/Facultades.xlsx")

QCuantitativo <- left_join(QCuantitativo, Facultades, by = "ESTU_SNIES_PRGMACADEMICO")


RC_Facul <- QCuantitativo %>%  group_by(SEDE, FACULTAD) %>% 
  summarise(Quintil1 = sum(Quintil1), Quintil2 = sum(Quintil2),
            Quintil3 = sum(Quintil3), Quintil4 = sum(Quintil4),
            Quintil5 = sum(Quintil5)) %>% 
  mutate(Total = Quintil1 + Quintil2 + Quintil3 + Quintil4  + Quintil5) %>% 
  mutate(Quintil1P = round((Quintil1/Total)*100, 0),
         Quintil2P = round((Quintil2/Total)*100, 0),
         Quintil3P = round((Quintil3/Total)*100, 0),
         Quintil4P = round((Quintil4/Total)*100, 0),
         Quintil5P = round((Quintil5/Total)*100, 0))

# Gráfico

ggplot(RC_Facul, aes(x = Quintil1P, y = Quintil5P , color = SEDE, label = FACULTAD)) + 
  geom_point(size = 6) + xlim(0, 30) + ylim(0, 100)+
  geom_hline(yintercept = 50, col = "green", size = 2) +
  geom_vline(xintercept = 20, col = "#e34a33", size = 2) +
  geom_text_repel(col = "black", size = 4, force = 5) +
  annotate("text", x = 12, y = 75, label = "Escenario ideal", colour = "gray", size = 10) +
  annotate("text", x = 12, y = 20, label = "Escenario aceptable", colour = "gray", size = 10) +
  annotate("text", x = 27, y = 20, label = "Escenario negativo", colour = "gray", size = 10) +
  annotate("text", x = 27, y = 75, label = "Escenario atípico", colour = "gray", size = 10) +
  scale_colour_brewer(palette = "Set1") +
  ylab("Proporción de estudiantes en el quintil 5 (%)  \n") +
  xlab("\n Proporción de estudiantes en el quintil 1 (%)") +
  theme(
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"),
    axis.text = element_text(size = 12)
  )



library(tidyverse)
library(readxl)
library(skimr)
library(corrplot)
library(broom)
datos <- read_excel("EC1116C dataf.xlsx")
base <- read_excel("EC1116C dataf_dicc.xlsx")


df <- datos %>% 
         select_if(is.numeric)
df[is.na(df)] <- 0

str(df)
unique(base$`GLOSA PREGUNTA`)

a <- base %>% 
  filter(CÓDIGO == 'D180')
a$GLOSA

base[base$`GLOSA PREGUNTA` == "Respecto a su empresa, indique:" ,]


df1 <- df[,1:25]
df2 <- df[,26:50]
df3 <- df[,51:75]
df4 <- df[,76:100]
df5 <- df[,101:125]
df6 <- df[,126:150]
df7 <- df[,151:175]
df8 <- df[,176:200]
df9 <- df[,201:225]
df10 <- df[,226:250]
df11 <- df[,251:275]
df12 <- df[,276:300]
df13 <- df[,301:325]
df14 <- df[,326:350]
df15 <- df[,351:375]
df16 <- df[,376:400]
df17 <- df[,401:425]
df18 <- df[,425:446]

df1$GASTOS <- datos$C045
df2$GASTOS <- datos$C045
df3$GASTOS <- datos$C045
df4$GASTOS <- datos$C045
df5$GASTOS <- datos$C045
df6$GASTOS <- datos$C045
df7$GASTOS <- datos$C045
df8$GASTOS <- datos$C045
df9$GASTOS <- datos$C045
df10$GASTOS <- datos$C045
df11$GASTOS <- datos$C045
df12$GASTOS <- datos$C045
df13$GASTOS <- datos$C045
df14$GASTOS <- datos$C045
df15$GASTOS <- datos$C045
df16$GASTOS <- datos$C045
df17$GASTOS <- datos$C045
df18$GASTOS <- datos$C045

#Asociaciones gremiales
Gremio <- df$D082

#EXportaciones
Exportaciones <- df$D180

#c041 0.43 Total Ingresos 2019
Ingresos_2019 <- df$C041

#c037 0.43 ingresos por actividad principal 2019
Ingresos_AP2019 <- df$C037

#c006 0.22 costo de venta 2018
Costo_v2018 <- df$C006

#c005 0.42 total ingresos 2018
Ingresos_2018 <- df$C005

#c001 0.41 ingresos por actividad principal 2018
IngresosAP2018 <- df$C001

#c010 0.35 gastos generales 2018
GastosG_2018 <- df$C010

#c012 0.34 remuneraciones 2018
Remun_2018 <- df$C012

#c020 0.42 total gastos 2018
Gastos_2018 <- df$C020

#c022 0.47 activos circulantes 2018
ActivosCir_2018 <- df$C022

#c027 0.48 cuentas por pagar corto plazo 2018
ctasxpagar_2018 <- df$C027 

#c046 0.35 gastos generales 2019
GastosG_2019 <- df$C046

#c048 0.38 remuneraciones 2019
Remuner_2019 <- df$C048

#c056 0.43 total gastos 2019
Gastos_2019 <- df$C056

#c058 0.47 activos circulantes 2019
Activoscir_2019 <- df$C058

#c063 0.47 cuentas por pagar corto plazo 2019
ctasxpagar_2019 <- df$C063

#c072 0.35 total pasivos 2019
Pasivos_2019 <- df$C072

#c065 0.35 pasivos circulantes 2019
PasivosCir_2019 <- df$C065

#i161 0.39 REMUNERACIONES DIRECTIVOS Y GERENTES HOMBRES
Remun_directivos_H <- df$I161

#i162 0.35 REMUNERACIONES PROFESIONALES HOMBRES
Remun_prof_H <- df$I162

#i169 0.24 REMUNERACIONES TOTALES HOMBRES
Remun_T_H <- df$I169

#i170 0.45 REMUNERACIONES DIRECTIVOS Y GERENTES MUJERES 
Remun_directivos_M <- df$I170

#i171 0.50 REMUNERACIONES Profesionales MUJERES
Remun_prof_M <- df$I171

#i178 0.47 REMUNERACIONES TOTALES MUJERES
Remun_T_M <- df$I178

#i131 0.52 PERSONAS SUBCONTRATADAS MUJERES
Subcontrato_M <- df$I131

#i130 0.45 PERSONAS SUBCONTRATADAS HOMBRES
Subcontrato_H <- df$I130

#Anio inicio
Anio_inicio <- df$A068

#Teletrabajo
Teletrabajo <- df$I114

#Equipos computacionales
Equipos <- df$B092

#Sofrware
Software <- df$B093


Gastos_comunicacion <- df$C045






data <- data.frame(Gastos_comunicacion,Ingresos_2019
                   ,Ingresos_2018,
                   Gastos_2018
                   ,Gastos_2019,Activoscir_2019,Pasivos_2019,
                   Remun_directivos_H,Remun_prof_H,Remun_T_H,Remun_directivos_M,Remun_prof_M
                   ,Remun_T_M,Subcontrato_M,Subcontrato_H,Gremio,Anio_inicio,Exportaciones
                   ,Teletrabajo,Equipos,Software)


Gastos_comunicacion_D <- data$Gastos_comunicacion
Gastos_comunicacion_D[Gastos_comunicacion_D > 0] <- 1



data$Gastos_comunicacion_D <- Gastos_comunicacion_D
head(data)


data %>% 
  select(where(is.numeric)) %>%
  cor(use = "complete.obs") %>% 
  corrplot(method = 'number' , type = 'upper', order = 'alphabet')

str(data)
round(cor(data),2) 


#1. Explicar la decision de las empresas de gastar o no en servicios de comunicacion y TICs en el a˜no 2019

#GASTOS COMUNICACION D
#VARIABLE DICOTOMICA QUE REPRESENTA LA DECICION DE GASTAR EN COMUNICACION Y TIC

#Variables que tienen una correlacion mas alta con Gastos de comunicacion D

#Software 0.19
#Equipos  0.25
#Anio inicio -0.16
#Gremio  0.16
#Exportaciones -0.15
#Teletrabajo 0.11

mdl1 <- lm(Gastos_comunicacion_D ~ Software + Equipos + Anio_inicio, data = df)
summary(mdl1)

mdl2 <- lm(Gastos_comunicacion_D ~ Software + Equipos + Gremio, data = df)
summary(mdl2)

mdl3 <- lm(Gastos_comunicacion_D ~ Software + Equipos + Exportaciones, data = df)
summary(mdl3)

mdl4 <- lm(Gastos_comunicacion_D ~ Software:Equipos + Anio_inicio, data = df)
summary(mdl4)

mdl5 <- lm(Gastos_comunicacion_D ~ Software:Equipos + Gremio, data = df)
summary(mdl5)

mdl6 <- lm(Gastos_comunicacion_D ~ I(Software*Equipos) + Anio_inicio, data = df)
summary(mdl6)

mdl7 <- lm(Gastos_comunicacion_D ~ Software*Equipos + Gremio, data = df)
summary(mdl7)

Modelo1 <- glance(mdl1) %>% 
  select(r.squared,df,AIC,BIC) %>% 
  mutate(Modelo = "mdl1")

Modelo2 <- glance(mdl2) %>% 
  select(r.squared,df,AIC,BIC) %>% 
  mutate(Modelo = "mdl2")

Modelo3 <- glance(mdl3) %>% 
  select(r.squared,df,AIC,BIC) %>% 
  mutate(Modelo = "mdl3")

Modelo4 <- glance(mdl4) %>% 
  select(r.squared,df,AIC,BIC) %>% 
  mutate(Modelo = "mdl4")

Modelo5 <- glance(mdl5) %>% 
  select(r.squared,df,AIC,BIC) %>% 
  mutate(Modelo = "mdl5")

Modelo6 <- glance(mdl6) %>% 
  select(r.squared,df,AIC,BIC) %>% 
  mutate(Modelo = "mdl6")

Modelo7 <- glance(mdl7) %>% 
  select(r.squared,df,AIC,BIC) %>% 
  mutate(Modelo = "mdl7") 
 

  
bind_rows(Modelo1,Modelo2,Modelo3,Modelo4,Modelo5,Modelo6, Modelo7)

tidy(mdl1)
tidy(mdl2)
tidy(mdl3)
tidy(mdl4)
tidy(mdl5)
tidy(mdl6)
tidy(mdl7) 

#MODELOS PROBIT


mdlp1<-glm(Gastos_comunicacion_D ~ Software + Equipos + Anio_inicio, data = data, family = binomial(link = "probit"))

mdlp2<-glm(Gastos_comunicacion_D ~ Software + Equipos + Gremio, data = data, family = binomial(link = "probit"))

mdlp3<-glm(Gastos_comunicacion_D ~ Software + Equipos + Exportaciones, data = data, family = binomial(link = "probit"))

mdlp4<-glm(Gastos_comunicacion_D ~ Software:Equipos + Anio_inicio, data = data, family = binomial(link = "probit"))

mdlp5<-glm(Gastos_comunicacion_D ~ Software:Equipos + Gremio, data = data, family = binomial(link = "probit"))

mdlp6<-glm(Gastos_comunicacion_D ~ Software*Equipos + Anio_inicio, data = data, family = binomial(link = "probit"))

mdlp7<-glm(Gastos_comunicacion_D ~ Software*Equipos + Gremio, data = data, family = binomial(link = "probit"))


MP1 <- glance(mdlp1) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl1")

MP2 <- glance(mdlp2) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl2")

MP3 <- glance(mdlp3) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl3")

MP4 <- glance(mdlp4) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl4")

MP5 <- glance(mdlp5) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl5")

MP6 <- glance(mdlp6) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl6")

MP7 <- glance(mdlp7) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl7") 



bind_rows(MP1,MP2,MP3,MP4,MP5,MP6,MP7)

tidy(mdlp1)
tidy(mdlp2)
tidy(mdlp3)
tidy(mdlp4)
tidy(mdlp5)
tidy(mdlp6)
tidy(mdlp7) 

#MODELOS LOGIT

mdlL1<-glm(Gastos_comunicacion_D ~ Software + Equipos + Anio_inicio, data = data, family = "binomial")

mdlL2<-glm(Gastos_comunicacion_D ~ Software + Equipos + Gremio, data = data, family = "binomial")

mdlL3<-glm(Gastos_comunicacion_D ~ Software + Equipos + Exportaciones, data = data, family = "binomial")

mdlL4<-glm(Gastos_comunicacion_D ~ Software:Equipos + Anio_inicio, data = data, family = "binomial")

mdlL5<-glm(Gastos_comunicacion_D ~ Software:Equipos + Gremio, data = data, family = "binomial")

mdlL6<-glm(Gastos_comunicacion_D ~ Software*Equipos + Anio_inicio, data = data, family = "binomial")

mdlL7<-glm(Gastos_comunicacion_D ~ Software*Equipos + Gremio, data = data, family = "binomial")

AIC(mdlL1)

ML1 <- glance(mdlL1) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl1")

ML2 <- glance(mdlL2) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl2")

ML3 <- glance(mdlL3) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl3")

ML4 <- glance(mdlL4) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl4")

ML5 <- glance(mdlL5) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl5")

ML6 <- glance(mdlL6) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl6")

ML7 <- glance(mdlL7) %>% 
  select(AIC,BIC) %>% 
  mutate(Modelo = "mdl7") 



bind_rows(ML1,ML2,ML3,ML4,ML5,ML6,ML7)

tidy(mdlL1)
tidy(mdlL2)
tidy(mdlL3)
tidy(mdlL4)
tidy(mdlL5)
tidy(mdlL6)
tidy(mdlL7)



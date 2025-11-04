#Lets go
    ###Librerias
library(misty) #datos perdidos}
library(mice) #ver patron de datos perdidos
library(ggplot2) #GRAFICOS
library(psych) #algunos descriptivos
library(corrplot) #graficos correlaciones
library(dplyr) #pq es bacan tenerla abierta
library(MVN) #normalidad multivariada para LPA
library(poLCA) #clases latentes
library(gmodels) #tabla de contingencias y chi cuadrado

base<-read.csv("bd/base_93.csv", header=T)
#Vamos a hacer una seleccion de las variables que nos interesan
#VD: Apoyo a democracia (democracia_21)
#VI: Situacion economica presente (percepcion_2) y futura (percepcion_3), situacion economica personal presente (percepcion_5) y futura (percepcion_6)
#confianza en instituciones: congreso (confianza_6_k)   d k i o p
  #gobierno (confianza_6_i)
  #municpalidades (confianza_6_o)
  #tribunales de justicia (confianza_6_d)
  #y fiscalia (confianza_6_p) EVALUAR SI MANTENER DEPENDIENDO DE CORRELACION CON TRIBUNALES DE JUSTICIA
#Caracterizacion: sexo (sexo, 1 hombre ),  edad (edad) Y no incluire NSE pues para grupo mas pobre hay muy pocos casos
#Otras revelantes segun literatura
  ##Aprobacion a gobierno (eval_gob_1)
  ##Percepcion progreso del pais (percepcion_4)
  
df <- base[, c("id_bu", "sexo", "edad", "eval_gob_1", "percepcion_2", "percepcion_3", "percepcion_4",  "percepcion_5", "percepcion_6",  "democracia_21","confianza_6_d","confianza_6_i","confianza_6_k","confianza_6_o", "confianza_6_p")]


##si valor es -8 o -9 remplazar por NA
df[df == -8 | df == -9 | df ==88 | df==99] <- NA


#Datos perdidos
na.test(df) #difernecias son significativas por lo que datos perdidos no son aletaorios
md.pattern(df, plot=T) #a la rapida pareciera que hay hartos datos perdisos en preguntas de confianza y creencia en democracia


#quiero ver cantidad de NA para cada columna
sapply(df, function(x) sum(is.na(x))) 
#hacer un grafico con estos valores

na_counts <- sapply(df, function(x) sum(is.na(x)))
na_df <- data.frame(variable = names(na_counts), na_count = na_counts)
ggplot(na_df, aes(x = reorder(variable, -na_count), y = na_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Cantidad de datos perdidos por variable",
       x = "Variable",
       y = "Cantidad de datos perdidos") +
  theme_minimal()

rm(list=c("na_df","na_counts"))


#podria realizar imputaciones pero creo que voy a elegir la opcion sencilla y eliminarlos.

df <- na.omit(df) #sacar datos perdidos

#Ver valores antes de inversion
describe(df)
#democracia_21 mean 1.91
#percepcion_4 2.19 como referencias


#Items a invertir (1 es positivo)
  #percepcion_3: 1 mejroara, 2 no cambiara, 3 empeorara
  #percepcion_6: 1 mejorara, 2 no cambiara, 3 empeorara
  #percepcion_4: 1 progresando, 2 estancado, 3 en decadencia
  #eval_gob_1: 1 aprueba, 2 desaprueba, 3 ni fu ni fa
  #todas las confianza :1 mucha confianza, 2 algo de confianza, 3 poca confianza, 4 ninguna confianza
  #democracia_21: 1 democracia es preferible a cualquier otra forma de gobierno, 2 en algunas circunstancias un gobierno autoritario puede ser preferible a la democracia, 3 no importa si es democracia o autoritario mientras el gobierno haga lo correcto

      ###Ojo que esta parece mas bien variables categoricas

#Inversiones

#Percepcion futuro economico del pais
df$percepcion_3 <- ifelse(df$percepcion_3 == 1, 3,
                          ifelse(df$percepcion_3 == 2, 2,
                                 ifelse(df$percepcion_3 == 3, 1, df$percepcion_3)))


#Percepcion progreso del pais
#tambien invertir percepcion_4 que es eprcepcion de progresod e pais
df$percepcion_4 <- ifelse(df$percepcion_4 == 1, 3,
                          ifelse(df$percepcion_4 == 2, 2,
                                 ifelse(df$percepcion_4 == 3, 1, df$percepcion_4)))



#Percepcion futuro economico PERSONAL
#hay que invetir percepcion_5 1=5, 2=4, 3=3, 4=2, 5=1, ignorar valores -8 y -9
df$percepcion_6 <- ifelse(df$percepcion_6 == 1, 5,
                          ifelse(df$percepcion_6 == 2, 4,
                                 ifelse(df$percepcion_6 == 3, 3,
                                        ifelse(df$percepcion_6 == 4, 2,
                                               ifelse(df$percepcion_6 == 5, 1, df$percepcion_6)))))


#APOYO A DEMOCRACIA DEMOCRACIA
df$democracia_21 <- ifelse(df$democracia_21 == 1, 3,
                          ifelse(df$democracia_21 == 2, 2,
                                 ifelse(df$democracia_21 == 3, 1, df$democracia_21)))

#Aprobacion a gobierno
df$eval_gob_1 <- ifelse(df$eval_gob_1 == 1, 3, #APRUEBA ES 3
                        ifelse(df$eval_gob_1 == 2, 1, #DESAPRUEBA ES 1
                               ifelse(df$eval_gob_1 == 3, 2, df$eval_gob_1))) #NO APRUEBA NI DESAPRUEAB ES 2


##Dado que en confianza en institucioens todas estan invertidas no se hara inversion. Tener esto en consdieracion al interpretar


describe(df)
#democracia_21 mean 2.09
#percepcion_4 1.81 como referencias

#tabla de frecuencia de percepcion
table(df$percepcion_2)
table(df$percepcion_3)
table(df$percepcion_5)
table(df$percepcion_6)


# Correlacion entre variables de confianza --------------------------------



#matriz de correlaciones entre variables de confianza
cor_matrix <- round(cor(df[, c("confianza_6_d","confianza_6_i","confianza_6_k","confianza_6_o", "confianza_6_p")], use="pairwise.complete.obs"), 2)
print(cor_matrix)

#d Tribunales, i gobierno, k congreso, o municipalidades, p fiscales

#grafico de correlacioens
#grafico de correlacioens
color_breaks_full <- c(
  # Lado Negativo
  -1.00, -0.51, -0.41, -0.31, -0.21, -0.11,
  # Cero
  0.00, 
  # Lado Positivo (Tus rangos deseados)
  0.11, 0.21, 0.31, 0.41, 0.51, 1.00
)
custom_colors_full <- c(
  # Colores para la correlación Negativa (Ejemplo: Usando tonos verdes como contraste)
  "darkgreen",   # -1.00 a -0.51
  "green4",      # -0.51 a -0.41
  "yellowgreen", # -0.41 a -0.31
  "cyan",        # -0.31 a -0.21
  "darkcyan",    # -0.21 a -0.11
  "lightgray",   # -0.11 a 0.00 
  
  # Colores para la correlación Positiva (Tus colores originales)
  "lightgray",   # 0.00 a 0.11
  "lightblue",   # 0.11 a 0.21
  "blue",        # 0.21 a 0.31
  "purple",      # 0.31 a 0.41
  "red4",        # 0.41 a 0.51
  "red"          # 0.51 a 1.00
)

corrplot(cor_matrix, 
         method = "circle", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45,
         title = "Matriz de Correlaciones entre Confianza en Instituciones",
         mar = c(0,0,1,0),
         
         # --- Nuevos Parámetros de color ---
         col = custom_colors_full,       # Paleta de colores extendida
         cl.lim = c(-1, 1),              # Límite extendido para incluir el rango negativo
         breaks = color_breaks_full      # Puntos de cambio de color extendidos
)


rm(list=c("color_breaks","custom_colors"))

#Son correlaciones alta pero nada que haga pensar que no pueden ir juntas

#alfa de cronbach de confianza
confianza_items <- df[, c("confianza_6_d","confianza_6_i","confianza_6_k","confianza_6_o", "confianza_6_p")]
alpha(confianza_items)

#construir variable de confianza como promedio de las 5
df$confianza_i <- rowMeans(confianza_items)

#RECORDAR QUE VALROES MAS ALTOS ES MAYOR DESCONFIANZA!
#INVERTIR VALORES CONTINUOS DE CONFIANZA_1 antes de invertir promedio es 3.21

df$confianza_i <- (4 + 1) - df$confianza_i #despues de invertir es 1.79



#CONSTRUIR MATRIZ DE CORRELACIONES ENTRE VARIABLES de percepciob, democracia_21 y confianza_i
cor_matrix2 <- round(cor(df[, c("percepcion_2", "percepcion_3", "percepcion_4", "percepcion_5", "percepcion_6", "democracia_21", "confianza_i")], use="pairwise.complete.obs"), 2)
print(cor_matrix2)



color_breaks_full <- c(
  # Lado Negativo
  -1.00, -0.51, -0.41, -0.31, -0.21, -0.11,
  # Cero
  0.00, 
  # Lado Positivo (Tus rangos deseados)
  0.11, 0.21, 0.31, 0.41, 0.51, 1.00
)
custom_colors_full <- c(
  # Colores para la correlación Negativa (Ejemplo: Usando tonos verdes como contraste)
  "darkgreen",   # -1.00 a -0.51
  "green4",      # -0.51 a -0.41
  "yellowgreen", # -0.41 a -0.31
  "cyan",        # -0.31 a -0.21
  "darkcyan",    # -0.21 a -0.11
  "lightgray",   # -0.11 a 0.00 
  
  # Colores para la correlación Positiva (Tus colores originales)
  "lightgray",   # 0.00 a 0.11
  "lightblue",   # 0.11 a 0.21
  "blue",        # 0.21 a 0.31
  "purple",      # 0.31 a 0.41
  "red4",        # 0.41 a 0.51
  "red"          # 0.51 a 1.00
)

corrplot(cor_matrix2, 
         method = "circle", 
         type = "upper", 
         tl.col = "black", 
         tl.srt = 45,
         title = "Matriz de Correlaciones entre Confianza en Instituciones",
         mar = c(0,0,1,0),
         
         # --- Nuevos Parámetros de color ---
         col = custom_colors_full,       # Paleta de colores extendida
         cl.lim = c(-1, 1),              # Límite extendido para incluir el rango negativo
         breaks = color_breaks_full      # Puntos de cambio de color extendidos
)

rm(list=c("color_breaks_full","custom_colors_full"))

#crear archivo cvs con df
write.csv(df, "bd_limpia/base_93_limpia.csv", row.names = FALSE)


# ANALISIS DE CLASES ----------------------------------------------------

str(df)
    #la verdad es que por caracteristicas de escalas usaria LCA con variables categoricas
df$percepcion_2<-as.numeric(df$percepcion_2)
df$percepcion_3<-as.numeric(df$percepcion_3)
df$percepcion_5<-as.numeric(df$percepcion_5)
df$percepcion_6<-as.numeric(df$percepcion_6)
df$sexo<-as.factor(df$sexo)

##Recomendaciones de Spurk et al. (2020) para perfiles latentes
#1. Seleccionar variables observadas y supuestos
#Muestra >500, normalidad multivariada. Si no se cumple usar errores robustos

#Evaluacion de normalidad multivariada
#Primero hace un subset de las variables de interes: percepcion_2, percepcion_3, percepcion_5, percepcion_6
lpa_data <- df[, c("percepcion_2", "percepcion_3", "percepcion_5", "percepcion_6")]
mvn(lpa_data)#no hay normalidad multivarida, asi que usar errores robustos

#2. Estimar modelos con diferentes numeros de perfiles (1-6)

variables<- cbind(percepcion_2, percepcion_3, percepcion_5, percepcion_6) ~ 1 #(esto ultimo le quita la columna de intercepto)


#ATENCION, CORRER ESTE CODIGOS VA A RALENTIZAR MUCHO SU COMPUTADOR
#PROCEDA CON CONSIDERACION A ESO

set.seed(3141)
objetoLCA_2<-poLCA(variables, df, nclass=2, nrep=100, maxiter=1000, graphs=T)
set.seed(3141)
objetoLCA_3<-poLCA(variables, df, nclass=3, nrep=100, maxiter=1000, graphs=T)
set.seed(3141)
objetoLCA_4<-poLCA(variables, df, nclass=4, nrep=100, maxiter=1000, graphs=T)
set.seed(3141)
objetoLCA_5<-poLCA(variables, df, nclass=5, nrep=100, maxiter=1000, graphs=T) #clase con menos de 5
objetoLCA_6<-poLCA(variables, df, nclass=6, nrep=50, maxiter=1000, graphs=T) #no converge!!!

#3. Comparar ajuste de modelos usando AIC, BIC, SABIC, LMR-LRT, BLRT y Entropia
#Dato que 6 clases no covnerge podemso ignorarlo y comparar de 2 a 5 clases
#Modelo de 5 clases tiene un clase con solo 4%

valor = c(objetoLCA_2$aic,objetoLCA_3$aic,objetoLCA_4$aic,objetoLCA_5$aic,
          objetoLCA_2$bic,objetoLCA_3$bic,objetoLCA_4$bic, objetoLCA_5$bic)

indice = c("aic", "aic", "aic","aic",
           "bic", "bic", "bic", "bic")

cantidad = c("2c", "3c", "4c", "5c",
             "2c", "3c", "4c", "5c")

n_clases = cbind.data.frame(cantidad,indice,valor)
n_clases #bic castiga ams en muestras grandes

#comparar ajuste modelos de 2 a 5 clases




#4. Seleccionar modelo optimo considerando ajuste estadistico y interpretabilidad
#VALORES CON 500 REPETICIONES POR MODELO
#cantidad indice    valor
#1       2c    aic 11214.06
#2       3c    aic 11138.78
#3       4c    aic 11123.65
#4       5c    aic 11122.60
#5       2c    bic 11362.39
#6       3c    bic 11363.83
#7       4c    bic 11425.42
#8       5c    bic 11501.09

rm(list=c("cor_matrix", "cor_matrix2","objetoLCA_2","objetoLCA_5","objetoLCA_6", "mr"))

poLCA.entropy(objetoLCA_3) #valores no estandrizados ni normalizados, mas alto es mejor
poLCA.entropy(objetoLCA_4)

objetoLCA_3

#Modelos son muy parecidos, el Bic es sin duda mejor en el de 3.
#Ver que se gana en interpretabilidad

#5. Interpretar perfiles y asignar nombres

#Modelo de 3
#CLASE 1 24.47% -OPTIMISTA A FUTURO
#Clase 1 mayor probabilidad de situacion economica economica del pais promedio o ligeramente mala actual
  #Clase 1 mayor probbilidad de mejoramiento de situacion economica del pais futura
    #Clase 1 vision ni buena ni mala o de buena situacion economica personal actual
      #Clase 1 vision ligeramente optimista de situacion economica futura

#CLASE 2 49.27% -SOSTENIMIENTO
#Clase 2 mayor probabilidad de situacion economica promedio o ligeramente mala del pais
#Clase 2 mayoritariamente situaciob del pais se mantendr
#Clase 2 vision economica personal regular o ligeramente mala actual
#Clase 2 vision de mantinimiento de situacion economica futura

#CLASE 3 26.26% -PESIMISTAS
#Clase 3 mayor probabilidad de situacion economica economica del pais mala actual
  #Clase 3 mayor probbilidad de empeoramientio de situacion economica a futuro
    #Clase 3 vision ni buena ni mala o ligeramente mala economia personal actual
      #Clase 3 vision de mantinimiento de situacio economica futura



objetoLCA_4
#SOBRE MODELO DE 4 CLASES
#CLASE 1 16.67% OPTIMISTAS
#CLASE 2 16.02% PAIS MAL, YO MEJOR , pero en este gusto la vision futura del pais se divide equitativametne
#clase 2  **parece** componerse de lo que en otro modelo seria optimistas y pesimistas
#CLASE 3 47.56% SOSTENIMIENTO
#CLASE 4 19.76% PESIMISTAS

#para ver fuerza de asociacion entre variables

library(vcd)
tabla_clase<- table(df$clase_3, df$clase_4)
cramerv <- assocstats(tabla_clase)
print(cramerv$cramer) #fuertisima asociacion de 0.89

rm(list=c("cramerv", "tabla_clase"))


#6. Validar perfiles usando variables externas (opcional)

#Crear una columna en df con las clases a que pertenecede cada individuo
df$clase_3 <- objetoLCA_3$predclass
df$clase_4 <- objetoLCA_4$predclass



#Caracterizacion de sexo  de clase_3 y 4
table(df$sexo, df$clase_3)
#usar chi cuadrado para ver distribucion de clases segun sexo
ct_3<-xtabs(~sexo + clase_3, data=df)
CrossTable(ct_3,expected=T, prop.c=F, prop.r=F,prop.t=F,chisq=TRUE)


table(df$sexo, df$clase_4)
ct<-xtabs(~sexo + clase_4, data=df)
CrossTable(ct,expected=T, prop.c=F, prop.r=F,prop.t=F,chisq=TRUE)

#en ambos grupos dsitribucion es igual a esperada



# ANOVA ----------------------------------------------------
#ojo que aca democracia_12 se toma como variable numerica, que en verdad no lo es
df$clase_3<-as.factor(df$clase_3)
df$clase_4<-as.factor(df$clase_4)

anova_3 <- aov(democracia_21 ~ clase_3, data = df)
summary(anova_3) #vviendo las suma de cuadrado no explica nada

ph_3 <- TukeyHSD(anova_3, "clase_3", conf.level = 0.95)
print(ph_3) #diferencias son entre grupo 3 -pesimistas con el resto

anova_4 <- aov(democracia_21 ~ clase_4, data = df)
summary(anova_4) #vviendo las suma de cuadrado no explica nada

ph_4 <- TukeyHSD(anova_4, "clase_4", conf.level = 0.95)
print(ph_4)
#grupos 3(mantenimiento)y2(yo bien) son iguales y 4(pesimistas)y2(yo bien) son iguales

#explican misma varianza, pero clase_3 lo hace con mas grados de libertad. 

#Mantener ese modelo de 3 apra siguientes



# REGRESION LOGISTICA -----------------------------------------------------
#para regresion logistica primero hare una version simple y luego otro multinomial
#en la sencilla los valores de democracia_21 seran dicotimizados, apoyar siempre democracia o no

df <- df|>
  mutate(
    democracia_21_d = case_when(
      democracia_21 == 1 ~ 1,  
      democracia_21 == 2 ~ 1,  
      democracia_21 == 3 ~ 2, 
      TRUE ~ NA_integer_       # For any other value (e.g., 4, NA), assign NA
    )
  )
#factorizarla
df$democracia_21_d<-as.factor(df$democracia_21_d)
str(df)

#Modelo nulo
rgl_null <- glm(democracia_21_d ~ 1, data = df, family = "binomial")   #aca el ~1 se pone siempre para crear modelo nulo
summary(rgl_null)

#Usando solo clase como predictor
rgl_c <- glm(democracia_21_d ~ clase_3, data = df, family = "binomial")
summary(rgl_c)
#beta de clase 3 es significativa -0.89
#en chance esto es que pertenecer a clase pesimista implica un chance de 0.41 de rechazar democracia comaprado con optimistas
exp(-0.89)


exp(-0.89)/(1+exp(-0.89))
#pertenencia a clase negativos comaprado con optimistas aumenta un 29% probabilidad de rechazo a democracia








################################
######################################################
################################
######################################################
# Mismo analisis tratando datos como continuos -LPA -----------------------
library(tidyLPA)
library(mclust)
library(robustbase)

#lo calculare con error estandar y promedio robusto
mr<-covMcd(lpa_data)
mr$center #estimacion roubusta de vector promedio, mas apropiado que cov
#ya que lpa usa promedios
#The warning is telling you that a substantial portion of your data is 
#concentrated on a single value for Variable 2 
#(likely 0, or just one specific category code like '1' 
#if the data wasn't centered). 
#This violates the assumptions of the MCD estimator, 
#which expects continuous data or at least enough variability to avoid 
#perfect collinearity in the selected subset.

#En resumen, no vale la pena hacerlo con supuesto de que datos son continuos


set.seed(324)


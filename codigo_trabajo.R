#Lets go
    ###Librerias
library(misty) #datos perdidos}
library(mice) #ver patron de datos perdidos
library(ggplot2) #GRAFICOS
library(psych) #algunos descriptivos

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
  ##Percepcion futuro del pais (percepcion_4)
  
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
#percepcion_4 2.81 como referencias
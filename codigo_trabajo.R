#Lets go
    ###Librerias
library(misty) #datos perdidos}
library(mice) #ver patron de datos perdidos
library(ggplot2) #GRAFICOS

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


#Items a invertir (1 es positivo)
  #percepcion_3: 1 mejroara, 2 no cambiara, 3 empeorara
  #percepcion_6: 1 mejorara, 2 no cambiara, 3 empeorara
  #percepcion_4: 1 progresando, 2 estancado, 3 en decadencia
  #eval_gob_1: 1 aprueba, 2 desaprueba, 3 ni fu ni fa
  #todas las confianza :1 mucha confianza, 2 algo de confianza, 3 poca confianza, 4 ninguna confianza
  #democracia_21: 1 democracia es preferible a cualquier otra forma de gobierno, 2 en algunas circunstancias un gobierno autoritario puede ser preferible a la democracia, 3 no importa si es democracia o autoritario mientras el gobierno haga lo correcto

      ###Ojo que esta parece mas bien variables categoricas
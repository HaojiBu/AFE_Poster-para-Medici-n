library(psych)
library(GPArotation)
library(detectnorm)

rm(list=ls())

set.seed(1111)




n <- 1000 #tamaño muestral
F1 <- rnonnorm(n, mean = 0, sd = 1, skew = +1)$dat
F2 <- rnonnorm(n, mean = 0, sd = 1, skew = +1)$dat

Lambda1 <- c(0.7)
Error1 <- c(sqrt(1-(Lambda1^2)))

Lambda2 <- c(0.5)
Error2 <- c(sqrt(1-(Lambda2^2)))

### Se simulan los items ###
i1 <- (Lambda1 * F1) + (Error1 * rnonnorm(n, mean = 0, sd = 1, skew = +1)$dat)
i2 <- (Lambda1 * F1) + (Error1 * rnonnorm(n, mean = 0, sd = 1, skew = +1)$dat)
i3 <- (Lambda1 * F1) + (Error1 * rnonnorm(n, mean = 0, sd = 1, skew = +1)$dat)
i4 <- (Lambda1 * F1) + (Error1 * rnonnorm(n, mean = 0, sd = 1, skew = +1)$dat)
i5 <- (Lambda1 * F1) + (Error1 * rnonnorm(n, mean = 0, sd = 1, skew = +1)$dat)

i6 <- (Lambda2 * F2) + (Error2 * rnonnorm(n, mean = 0, sd = 1, skew = +1)$dat)
i7 <- (Lambda2 * F2) + (Error2 * rnonnorm(n, mean = 0, sd = 1, skew = +1)$dat)
i8 <- (Lambda2 * F2) + (Error2 * rnonnorm(n, mean = 0, sd = 1, skew = +1)$dat)
i9 <- (Lambda2 * F2) + (Error2 * rnonnorm(n, mean = 0, sd = 1, skew = +1)$dat)
i10 <- (Lambda2 * F2) + (Error2 * rnonnorm(n, mean = 0, sd = 1, skew = +1)$dat)



### Se convierten a ordinales de 5 categorias ###
## Definir puntos de corte

breaks5 <- c(-Inf, -1, 0, 1, 2, Inf)
labels5 <- c(1, 2, 3, 4, 5)

i1o5 <- as.numeric(cut(i1, breaks = breaks5,labels = labels5))
i2o5 <- as.numeric(cut(i2, breaks = breaks5,labels = labels5))
i3o5 <- as.numeric(cut(i3, breaks = breaks5,labels = labels5))
i4o5 <- as.numeric(cut(i4, breaks = breaks5,labels = labels5))
i5o5 <- as.numeric(cut(i5, breaks = breaks5,labels = labels5))

i6o5 <- as.numeric(cut(i6, breaks = breaks5,labels = labels5))
i7o5 <- as.numeric(cut(i7, breaks = breaks5,labels = labels5))
i8o5 <- as.numeric(cut(i8, breaks = breaks5,labels = labels5))
i9o5 <- as.numeric(cut(i9, breaks = breaks5,labels = labels5))
i10o5 <- as.numeric(cut(i10, breaks = breaks5,labels = labels5))



### 4 categorías ###
breaks4 <- c(-Inf, -1, 0.3, 1.5, Inf)
labels_4 <- c(1, 2, 3, 4)

i1o4 <- as.numeric(cut(i1, breaks = breaks4, labels = labels_4))
i2o4 <- as.numeric(cut(i2, breaks = breaks4, labels = labels_4))
i3o4 <- as.numeric(cut(i3, breaks = breaks4, labels = labels_4))
i4o4 <- as.numeric(cut(i4, breaks = breaks4, labels = labels_4))
i5o4 <- as.numeric(cut(i5, breaks = breaks4, labels = labels_4))

i6o4 <- as.numeric(cut(i6, breaks = breaks4, labels = labels_4))
i7o4 <- as.numeric(cut(i7, breaks = breaks4, labels = labels_4))
i8o4 <- as.numeric(cut(i8, breaks = breaks4, labels = labels_4))
i9o4 <- as.numeric(cut(i9, breaks = breaks4, labels = labels_4))
i10o4 <- as.numeric(cut(i10, breaks = breaks4, labels = labels_4))


### 3 categorías ###
breaks3 <- c(-Inf, -1.2, 0.4, Inf)
hist(F1)
labels_3 <- c(1, 2, 3)

i1o3 <- as.numeric(cut(i1, breaks = breaks3, labels = labels_3))
i2o3 <- as.numeric(cut(i2, breaks = breaks3, labels = labels_3))
i3o3 <- as.numeric(cut(i3, breaks = breaks3, labels = labels_3))
i4o3 <- as.numeric(cut(i4, breaks = breaks3, labels = labels_3))
i5o3 <- as.numeric(cut(i5, breaks = breaks3, labels = labels_3))

i6o3 <- as.numeric(cut(i6, breaks = breaks3, labels = labels_3))
i7o3 <- as.numeric(cut(i7, breaks = breaks3, labels = labels_3))
i8o3 <- as.numeric(cut(i8, breaks = breaks3, labels = labels_3))
i9o3 <- as.numeric(cut(i9, breaks = breaks3, labels = labels_3))
i10o3 <- as.numeric(cut(i10, breaks = breaks3, labels = labels_3))

datos10c <- cbind(i1, i2, i3, i4, i5, 
                  i6, i7, i8, i9, i10)

datos10o5 <- cbind(i1o5, i2o5, i3o5, i4o5, i5o5,
                   i6o5, i7o5, i8o5, i9o5, i10o5) 

datos10o4 <- cbind(i1o4, i2o4, i3o4, i4o4, i5o4,
                   i6o4, i7o4, i8o4, i9o4, i10o4) 


datos10o3 <- cbind(i1o3, i2o3, i3o3, i4o3, i5o3,
                   i6o3, i7o3, i8o3, i9o3, i10o3) 


hist(datos10c)
hist(datos10o5)
hist(datos10o4)
hist(datos10o3)

## Comprobar normalidad ##

mardia(datos10c)
mardia(datos10o3)
mardia(datos10o4)
mardia(datos10o5)

## Ninguno pasa la prueba, efectivamente se han simulado datos no normales

loadings_c <- fa(datos10c, nfactors = 2, fm = "ml", cor = "cor", rotate = "oblimin")$loadings
loadings_o3 <- fa(datos10o3, nfactors = 2, fm = "ml", cor = "cor", rotate = "oblimin")$loadings
loadings_o4 <- fa(datos10o4, nfactors = 2, fm = "ml", cor = "cor", rotate = "oblimin")$loadings
loadings_o5 <- fa(datos10o5, nfactors = 2, fm = "ml", cor = "cor", rotate = "oblimin")$loadings
loadings_poly3 <- fa(datos10o3, nfactors = 2, fm = "ml", cor = "poly", rotate = "oblimin")$loadings
loadings_poly4 <- fa(datos10o4, nfactors = 2, fm = "ml", cor = "poly", rotate = "oblimin")$loadings
loadings_poly5 <- fa(datos10o5, nfactors = 2, fm = "ml", cor = "poly", rotate = "oblimin")$loadings

# Convertir los loadings a data frames
df_c <- data.frame(Item = rownames(loadings_c), 
                   ML1C = loadings_c[, 1], 
                   ML2C = loadings_c[,2])

df_o3 <- data.frame(Item = rownames(loadings_o3), 
                    ML1O3 = loadings_o3[, 1], 
                    ML2O3 = loadings_o3[,2])

df_o4 <- data.frame(Item = rownames(loadings_o4), 
                    ML1O4 = loadings_o4[, 1],
                    ML2O4 = loadings_o4[,2])

df_o5 <- data.frame(Item = rownames(loadings_o5), 
                    ML1O5 = loadings_o5[, 1], 
                    ML2O5 = loadings_o5[,2])

df_p3 <- data.frame(Item = rownames(loadings_poly3), 
                    ML1P3 = loadings_poly3[, 1], 
                    ML2P3 = loadings_poly3[,2])

df_p4 <- data.frame(Item = rownames(loadings_poly4), 
                    ML1P4 = loadings_poly4[, 1], 
                    ML2P4 = loadings_poly4[,2])

df_p5 <- data.frame(Item = rownames(loadings_poly5), 
                    ML1P5 = loadings_poly5[, 1], 
                    ML2P5 = loadings_poly5[,2])

# Combinar ambos data frames
df_all <- round(cbind(df_c[, -1], df_o3[, -1], df_o4[, -1], df_o5[, -1], df_p3[, -1], df_p4[, -1], df_p5[, -1]),3)

# Crear las nuevas columnas combinadas con otro formato
df_all$ML1_3 <- paste0(df_all$ML1O3, "(", df_all$ML1P3, ")")
df_all$ML2_3 <- paste0(df_all$ML2O3, "(", df_all$ML2P3, ")")

df_all$ML1_4 <- paste0(df_all$ML1O4, "(", df_all$ML1P4, ")")
df_all$ML2_4 <- paste0(df_all$ML2O4, "(", df_all$ML2P4, ")")

df_all$ML1_5 <- paste0(df_all$ML1O5, "(", df_all$ML1P5, ")")
df_all$ML2_5 <- paste0(df_all$ML2O5, "(", df_all$ML2P5, ")")

# Combinar todo de nuevo
df_result <- df_all[, c("ML1C", "ML2C", "ML1_3", "ML2_3", "ML1_4", "ML2_4", "ML1_5", "ML2_5")]

# Exportarlo 
write.csv(df_result, file = "pesos_asimetria+1.csv", row.names = FALSE)


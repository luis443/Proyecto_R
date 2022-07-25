library(dplyr) 
library(ggplot2)
moda <- function(vector){
  f.abs <- table(vector) # frecuencias absolutas
  max.f.abs <- max(f.abs) # obtenemos la máxima frecuencia absoluta
  pos.max <- which(f.abs == max.f.abs) # posición de la máxima frecuencia absoluta
  print("La moda es: ")
  print(names(f.abs[pos.max]))
  paste("Con una frecuencia de: ", unique(f.abs[pos.max]))
}



getwd()
setwd("C:/Users/lxv81/Documents/BankChurnerss")

df_bank <- read.csv("BankChurners.csv", sep = ",", header = TRUE)

class(df_bank)
str(df_bank)#muestra la estructura de un objeto e información acerca de la clase y contenido de cada columna

head(df_bank);#muestra las primeras filas del dataframe

tail(df_bank) #muestra las ultimas filas del dataframe

dim(df_bank) # verifico la dimensión del dataframe

summary(df_bank)  # calcula estadísticas básicas para cada columna



df_new <- select(df_bank,CLIENTNUM , Customer_Age , Gender,Dependent_count , Education_Level,Marital_Status,Income_Category,Card_Category,Months_on_book,Total_Relationship_Count,Credit_Limit )

df1 <- select(filter(df_new),Income_Category, Education_Level)
df2 <- select(filter(df_new),Gender, Credit_Limit)
resp <- cbind(df1, df2)

CustomerAge <- df_new[,2] 
CreditLimit <- df_new [,11]
table(CustomerAge)# obtenemos las frecuencias absolutas de los valores de la muestra
moda(CustomerAge)
resultado_media <- format(round(mean(CreditLimit), 2), nsmall = 2)
paste("La media del los limites de credito es: ",resultado_media)


filtro1 <- dplyr::filter(df_new, Customer_Age > 17 & Customer_Age < 31 & ( Income_Category =="Less than $40K" |  Income_Category == "$40K - $60K")) # Se filtra todas las personas entre las edades 18 y 30 anios y se selecciona nada mas los valores limites de credito menores a 60K 
paste("El limite de credito maximo para este segmento de edad es: ",max(filtro1[,11]))
paste("El limite de credito minimo para este segmento de edad es: ",min(filtro1[,11]))

#numero de personas que se encuentran en el rango de esos ingresos
data11 <- filtro1 %>%  
  group_by(Income_Category) %>%
  tally() %>%
  arrange(desc(n))


filtro2 <- dplyr::filter(df_new, Customer_Age > 31 & Customer_Age < 41 & (  Income_Category ==  "$60K - $80K")) # Se filtra todas las personas entre las edades 30 y 40 anios y se selecciona nada mas los valores limites de credito entre $60K - $80K
paste("El limite de credito maximo para este segmento de edad es: ",max(filtro2[,11]))
paste("El limite de credito minimo para este segmento de edad es: ",min(filtro2[,11]))

data2 <- filtro2 %>% 
  group_by(Income_Category) %>%
  tally() %>%
  arrange(desc(n))

filtro3 <- dplyr::filter(df_new, Customer_Age > 50 & (  Income_Category !=  "Unknown")) # Se filtra todas las personas entre las edades 30 y 40 anios y se selecciona nada mas los valores limites de credito entre $60K - $80K
paste("El limite de credito maximo para este segmento de edad es: ",max(filtro3[,11]))
paste("El limite de credito minimo para este segmento de edad es: ",min(filtro3[,11]))

data3 <- filtro3 %>% 
  group_by(Income_Category) %>%
  tally() %>%
  arrange(desc(n))


#Grafico de dispersion en base a la categoria de las tarjetas y la edad

data4 <- filtro2 %>% 
  group_by(Card_Category,Customer_Age) %>%
  tally() 

colnames(data4) <- c('Card_Category','Customer_Age','Total')
ggplot(data4, aes(x = Customer_Age, y = Total, color = Card_Category)) +
  geom_point()


#Grafico de pastel en base a el estado civil de una persona

data5 <- filtro3 %>% 
  group_by(Marital_Status ) %>%
  tally()%>%
  arrange(desc(n))

data5 <- dplyr::filter(data5, Marital_Status !=  "Unknown")
ggplot(data5, aes(x = "" , y = n, fill =(Marital_Status))) +
  geom_col(color = "black") +
  guides(fill = guide_legend(title = "respuesta")) +
  scale_fill_viridis_d() +
  coord_polar(theta = "y") + 
  theme_void()



#Grafico de barras en base a la categoria de las tarjetas  
data6 <- df_new %>% 
  group_by(Card_Category ) %>%
  tally()%>%
  arrange(desc(n))

ggplot(data6, aes(x = Card_Category, y = n, fill = Card_Category)) +
  geom_bar(stat = "identity")


########################################################################################################################################################################
analisis_1 <- select(filter(df_new, Gender =="M"), Credit_Limit) # Se filtra todas las personas del sexo masculino y se selecciona nada mas los valores limites de credito 
analisis_2 <- select(filter(df_new, Gender =="F"), Credit_Limit)#Se filtra todas las personas del sexo Femenino y se selecciona nada mas los valores limites de credito 

paste("el limite de credito maximo de las personas de sexo Masculino: ",max(analisis_1))
paste("el limite de credito maximo de las personas de sexo Femenino: ",max(analisis_2))
paste("el limite de credito minimo de las personas de sexo Masculino: ",min(analisis_1))
paste("el limite de credito minimo de las personas de sexo Femenino: ",min(analisis_2))

#Grafico de dispersion  en base a la gategoria de ingresos
data <- df_new %>% 
  group_by(Income_Category) %>%
  tally() 

colnames(data) <- c('Income_Category','Total')

p <- ggplot(data, aes(x=Income_Category, y=Total)) + 
  geom_line( color="blue") + 
  geom_point() +
  labs(x = "Income_Category", 
       y = "Total",
       title = paste("Categorias vs Total Personas:"
         )) +
  theme(plot.title = element_text(size=12))  +
  theme(axis.text.x = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(face = "bold", color="#993333" , 
                                   size = 10, angle = 45, 
                                   hjust = 1))  # color, ángulo y estilo de las abcisas y ordenadas 


#Grafico basado en la educacion y categoria de ingresos
data1 <- df_new %>% 
  group_by(Education_Level,Income_Category) %>%
  tally() 

colnames(data1) <- c('Education_Level','Income_Category','Total')

data1 <- dplyr::filter(data1, Income_Category !="Unknown" & Education_Level!="Unknown" )

ggplot(data1, aes(x = Education_Level, y = Total, fill = Income_Category)) + geom_boxplot() +
    ggtitle("Education_Level vs Total") +
    xlab("Education_Level") +
    ylab("Total")

#Grafico de histograma en base a edades vs total de registros
ggplot(df_new,aes(x=Customer_Age))+geom_histogram(binwidth = 2, col="black", fill = "blue",alpha = 0.4) +
  ggtitle("Histograma de Edades") + theme_light()+ ylab("Total de personas") +
  xlab("Edades") 


df3 <- select(filter(df_bank),Credit_Limit ,Total_Revolving_Bal, Avg_Open_To_Buy ,Total_Trans_Amt)
colnames(df3) <- c('Price','Total1','Total2','Total3')

CreditLimit1 <- analisis_1 [,1]
CreditLimit2 <- analisis_2 [,1]


m1 <- rexp(n = 4769, rate = mean(CreditLimit1)); 1/mean(CreditLimit1) # media real de la población
tail(as.data.frame(m1))
m2 <- rexp(n = 5358, rate = mean(CreditLimit2)); 1/mean(CreditLimit2) # media real de la población
tail(as.data.frame(m2))
z0 <- (mean(m1)-mean(m2)-0)/sqrt(var(m1)/4769 + var(m2)/5358)# estamos interesados en contrastar las hipótesis H0: mu1-mu2 = 0 vs H1: mu1-mu2 diferente de 0 (contraste de dos colas)
(z.025 <- qnorm(p = 0.025, lower.tail = FALSE))# que proviene de una distribución normal estándar aproximadamente.
(z0 < -z.025) | (z0 > z.025)
(pvalue <- 2*pnorm(z0, lower.tail = FALSE))
x <- seq(-4, 4, 0.01)
y <- dnorm(x)
plot(x, y, type = "l", xlab="", ylab="")
title(main = "Densidad normal estándar", sub = expression(paste(mu == 0, " y ", sigma == 1)))
polygon(c(min(x), x[x<=-z0], -z0), c(0, y[x<=-z0], 0), col="yellow")
axis(side = 1, at = -z0, font = 2, padj = 1, lwd = 2)

polygon(c(z0, x[x>=z0], max(x)), c(0, y[x>=z0], 0), col="blue")
axis(side = 1, at = z0, font = 2, padj = 1, lwd = 2)

### También podemos usar la función t.test para llevar a cabo el procedimiento 
# de contraste de hipótesis

t.test(x = m1, y = m2,
       alternative = "two.sided",
       mu = 0, paired = FALSE, var.equal = TRUE)

#//////////////////////////////////////////////////clase1
4 + 8

20 -8 

4^2

x <- 86
x

Oficina <- 7
Platzi <- 1
Transporte <- 1.5
Tiempo_al_dia <- Oficina + Platzi + Transporte
Tiempo_al_dia
#////////////////////////////////////////////////clase2
Corte_1 <- 0.3
Corte_2 <- 0.3
Corte_3 <- 0.4

Nota_1 <- 4.0
Nota_2 <- 4.6
Nota_3 <- 3.0

Nota_c_1 <- Nota_1 * Corte_1
Nota_c_2 <- Nota_2 * Corte_2
Nota_c_3 <- Nota_3 * Corte_3

Nota_final <- Nota_c_1 + Nota_c_2 + Nota_c_3

Nota_c_1
Nota_c_2
Nota_c_3
Nota_final
#//////////////////////////////////////////////////clase3
str(mtcars)
class(mtcars$vs)

mtcars$vs = as.logical(mtcars$vs)
mtcars$am = as.logical(mtcars$am)

str(mtcars)

str(orangeec)
#//////////////////////////////////////////////////clase4

summary(orangeec)

summary(mtcars)

wt <- (mtcars$wt*1000)/2
wt

mtcars.new <- transform(mtcars,wt=wt*1000/2)
mtcars.new
summary(mtcars.new)
#//////////////////////////////////////////////////clase5

tiempo_platzi <- c(25, 5, 10, 15, 10)
tiempo_lecturas <- c(30, 10, 5, 10, 15)
tiempo_aprendizaje <- tiempo_platzi + tiempo_lecturas
tiempo_aprendizaje

dias_aprendizaje <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes")
dias_aprendizaje

dias_mas_20min <- c(TRUE, FALSE, FALSE, TRUE, TRUE)
dias_mas_20min

total_tiempo_platzi <- sum(tiempo_platzi)
total_tiempo_platzi
total_tiempo_lecturas <- sum(tiempo_lecturas)
total_tiempo_lecturas
total_tiempo_adicional <- total_tiempo_platzi + total_tiempo_lecturas
total_tiempo_adicional
#//////////////////////////////////////////////////clase6

#matriz
tiempo_matriz <- matrix(c(tiempo_platzi,tiempo_lecturas),
                        nrow = 2,byrow=TRUE)

dias <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes")
Tiempo <- c("tiempo platzi", "tiempo_lecturas")

colnames(tiempo_matrix) <- dias
rownames(tiempo_matrix) <- Tiempo

tiempo_matrix

colSums(tiempo_matrix) 
#//////////////////////////////////////////////////clase7

final_matrix <- rbind(tiempo_matrix, c(10,15,30,5,0))
final_matrix

colSums(final_matrix)

final_matrix[1,5]

#Reto:

Sabado = c(30,25,27)
final_matrix_sabado <- cbind(final_matrix, Sabado)
rownames(final_matrix_sabado)<-c("tiempo Platzi","tiempo lecturas","Tiempo podcast")
final_matrix_sabado
#//////////////////////////////////////////////////clase8

mtcars[mtcars$cyl<6,]

orangeec[orangeec$GDP.PC>=15000,]

orangeec[orangeec$Creat.Ind...GDP<=2,]

neworangeec <- subset(orangeec, Internet.penetration...population > 80
                      & Education.invest...GDP >= 4.5)
neworangeec

neworangeec <- subset(orangeec, Internet.penetration...population > 80
                      & Education.invest...GDP >= 4.5,
                      select = Creat.Ind...GDP)
neworangeec

rename(orangeec,c("Creat.Ind..GDP"="AporteEcNja"))

#//////////////////////////////////////////////////clase9
Nivel_Curso <- c("Basico", "Intermedio", "Avanzado")
Nivel_Curso

head(mtcars)
head(orangeec)

tail(mtcars)
tail(orangeec)

glimpse(orangeec)

my_vector <- 1:8
my_matrix <- matrix(1:9, ncol=3)
my_df <- mtcars[1:4, ]
my_df
my_list <- list(my_vector, my_matrix, my_df)
my_list
#//////////////////////////////////////////////////clase10
#EDA Scatter plot mtcars
plot(mtcars$mpg ~ mtcars$cyl,
     xlab="cilindros", ylab = "millas por galon",
     main= "Relacion cilindros y millas por galon")
plot(mtcars$mpg ~ mtcars$hp,
     xlab="caballos de fuerza", ylab = "millas por galon",
     main= "Relacion de caballos de fuerza y millas por galon")
# EDA orangeec
plot(orangeec$Unemployment ~ orangeec$Education.invest...GDP,
     xlab="Inversion educacion (%PIB)",
     ylab="Desempleo",
     main="Relacion inversion en educacion y desempleo")

plot(orangeec$GDP.PC ~ orangeec$Creat.Ind...GDP,
     xlab="Aporte economia naranja al PIB(%)",
     ylab="PIB per capita",
     main="Relacion economia naranja y PIB per capita")
#//////////////////////////////////////////////////clase11
hist(mtcars$hp, 
     xlab = "caballos de fuerza", 
     main = "Carros según caballos de fuerza")

dev.new()
hist(mtcars$hp, 
     xlab = "caballos de fuerza", 
     main = "Carros según caballos de fuerza")

ggplot(mtcars, aes(x=hp))+
  geom_histogram()+
  labs(x="Caballos de fuerza", y="Cantidad de carros",
       title="Caballos de fuerza y Cantidad de carros seleccionados")+
  theme(legend.position= "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(mtcars, aes(x=hp))+
  geom_histogram(binwidth = 30)+
  labs(x="Caballos de fuerza", y="Cantidad de carros",
       title="Caballos de fuerza y Cantidad de carros seleccionados")+
  theme(legend.position= "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot()+geom_histogram(data=mtcars,
                        aes(x=hp),fill="blue",color="red",
                        bindwidth = 20)+
  labs(x="Caballos de fuerza", y="Cantidad de carros",
       title="Caballos de fuerza en carros seleccionados")+
  xlim(c(80,200))+
  theme(legend.position= "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#//////////////////////////////////////////////////clase12
#hist orangeec
hist(mtcars$hp, 
     xlab = "caballos de fuerza", 
     main = "Carros según caballos de fuerza")

dev.new()
hist(mtcars$hp, 
     xlab = "caballos de fuerza", 
     main = "Carros según caballos de fuerza")

ggplot(mtcars, aes(x=hp))+
  geom_histogram()+
  labs(x="Caballos de fuerza", y="Cantidad de carros",
       title="Caballos de fuerza y Cantidad de carros seleccionados")+
  theme(legend.position= "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot(mtcars, aes(x=hp))+
  geom_histogram(binwidth = 30)+
  labs(x="Caballos de fuerza", y="Cantidad de carros",
       title="Caballos de fuerza y Cantidad de carros seleccionados")+
  theme(legend.position= "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggplot()+geom_histogram(data=orangeec,
                        aes(x=GDP),fill="blue",color="red",
                        bindwidth = 2000)+
  labs(x="pib per capita", y="Cantidad de paises ",
       title="Pib per capita en paises de latam")+
  xlim(c(80,200))+
  theme(legend.position= "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


ggplot()+geom_histogram(data=orangeec,
                        aes(x=Creat.Ind...GDP),fill="blue",color="red",
                        bindwidth = 1)+
  labs(x="Aporte Economico Naranja al Pib(%)", y="Cantidad de paises ",
       title="Contribucion de economia naranja en paises de latam")+
  xlim(c(80,200))+
  theme(legend.position= "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#
ggplot()+geom_histogram(data=orangeec,
                        aes(x=Internet.penetration...population),fill="red",color="yellow",
                        bindwidth = 5)+
  labs(x="Penetracion internet (%) poblacion", y="Cantidad de paises ",
       title="Penetracion de internet paises de latam")+
  xlim(c(80,200))+
  theme(legend.position= "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
 #//////////////////////////////////////////////////clase13
 #BoxPlot
boxplot(mtcars$hp,
        ylab="caballos de fuerza",
        main="Caballos de fuerza en carros mtcars")
#
ggplot(mtcars,aes(x=as.factor(cyl),y=hp, fill=cyl))+
  geom_boxplot(alpha=0.6)+
  labs(x="cilindros", y="caballos de fuerza",
       title="Caballos de fuerza segun cilindros en mtcars")+
  theme(legend.position= "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#
ggplot(mtcars, aes(x=am, y=mpg, fill=am))+
  geom_boxplot()+
  labs(x="Tipo de caja", y="millas por galon",
       title="Millas por galon segun tipo de caja mtcars")+
  theme(legend.position= "none")+
  theme(panel.background = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#
mtcars$am <- factor(mtcars$am, levels=c(TRUE,FALSE),
                    labels=c("Manual", "Automatico"))
#RETO
#
ggplot(mtcars, aes(x=am, y=mpg, fill=am))+
  geom_boxplot()+
  labs(x="Tipos de caja", y="millas por galón",
       title = "Millas por galón según tipos de caja mtcars")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

mtcars$am <- factor(mtcars$am, levels=c(TRUE,FALSE),
                    labels = c("Manual", "Automático")
)
 #//////////////////////////////////////////////////clase14
economy <- mean(orangeec$GDP.PC)
#
orangeec<- orangeec %>% 
  mutate(Strong_economy =ifelse(GDP.PC< economy, "Por debajo del promedio pib per capital"," Sobra arriba promedio pib per capital"))
#
ggplot(orangeec, aes(x=Strong_economy, y=Creat.Ind...GDP,
                     fill=Strong_economy))+
  geom_boxplot(alpha=0.4)
labs(x="Tipos de pais", y= "Aporte economia naranja al pib",
     title= "Aporte economia naranja en pib paises latam con alto y bajo pib per capita ")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
#
ggplot(orangeec, aes(x=Strong_economy, y=Internet.penetration...population,
                     fill=Strong_economy))+
  geom_boxplot(alpha=0.4)
labs(x="Tipos de pais", y= "Penetracion de Internet(%)",
     title= "Penetracion en internet paises latam con alto y bajo pib per capita ")+
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
 #//////////////////////////////////////////////////clase15
ggplot(mtcars, aes(hp,mpg))+
  geom_point()+
  labs(x="caballos fuerza", y="millas por galon",
    title="Relacion caballos de fuerza y millas por galon")
  theme(legend.position = "none")+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
  
  #
  ggplot(mtcars, aes(wt,hp))+
    geom_point()+
    labs(x="peso", y="potencia",
         title="Relacion peso y potencia")
  theme(legend.position = "none")+
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  #
  ggplot(mtcars, aes(hp,qsec))+
    geom_point(aes(color=am, size=cyl))+
    labs(s="caballos de fuerza", y="tiempo en 1/4 milla",
         title= "caballos velocidad segun cilindraje y tipo de caja")
#//////////////////////////////////////////////////clase16
  ggplot(orangeec, aes(Internet.penetration...population,Creat.Ind...GDP))+
    geom_point(aes(color=factor(Strong_economy), size=GDP.Growth..))+
    labs(s="Penetracion Internet", y="Aporte economia naranja al PIB",
         title= "Internet y aporte economia naranja segun economia y crecimiento pib")
#
my_graph <- ggplot(orangeec, aes(Internet.penetration...population,
                                 Creat.Ind...GDP, label=row.names(orangeec)))+
  geom_point()+
  labs(x="Penetracion Internet", y="Aporte economia naranja",
       title="Penetracion Internet y aporte economia naranja")
my_graph

p = ggplot(my_graph)
p
#///////////////
  library(plotly)
my_graph <- ggplot(orangeec, aes(Internet.penetration...population,
                                 Creat.Ind...GDP, label=orangeec$Country))+
  geom_point()+
  labs(x="Penetración Internet", y="Aporte economia naranja",
       title = "Penetración Internet y aporte economia naranja")
my_graph
p = ggplotly(my_graph)
p
#Reto:
my_graph <- ggplot(orangeec, aes(x=Education.invest...GDP, y=Unemployment, label=Country))+
  geom_point(aes(color=factor(Strong_economy), size=X..pop.below.poverty.line)) +
  labs(x="Inversion de la Edución PIB %", 
       y="Desempleo %", 
       title="Inversion en Educación y Desempleo según Economía y población por debajo de la linea de pobreza")

p = ggplotly(my_graph)
#//////////////////////////////////////////////////clase17
#
pairs(mtcars[,2:6])
pairs(mtcars)
#
newdata <- subset(mtcars, select=c(1,6:7,10,11)) 
pairs(newdata)

#
pairs(mtcars[,-c(1,3,4,5,6,9,10)])
#
Eficientes <- filter(mtcars, mpg >= 30)
Eficientes
#
pairs(Eficientes[,2:6])

#
merc <- mtcars %>% 
  filter(str_detect(model,"Merc"))
merc

#
pairs(merc[,2:6])
#//////////////////////////////////////////////////clase18


cor(mtcars[,2:6])
#
cor(newdata)

cor(merc[,2:6])
#//////////////////////////////////////////////////clase19

pairs(orangeec[,2:6])

#
pairs(orangeec[,5:10])
#
newdata <- subset(orangeec,select=c(5,6,10,11,12,13))
newdata

pairs(newdata)
#//////////////////////////////////////////////////clase20
cor(orangeec[,2:6])
#
cor(orangeec[,2:6],use="complete.obs")
#
cor(orangeec[,5:10],use="complete.obs")
#
cor(newdata, use="complete.obs")
#//////////////////////////////////////////////////clase21
summary(mtcars)

#
sd(mtcars$mpg)
desv <- sd(mtcars$mpg)
mean(mtcars$mpg)
#

prom <- mean(mtcars$mpg)
prom
#
CoefVar <- (desv/prom)*100
CoefVar
#//////////////////////////////////////////////////clase22
sd(orangeec$Internet.penetration...population)
desv <- sd(orangeec$Internet.penetration...population)
desv

mean(orangeec$Internet.penetration...population)
prom <- sd(orangeec$Internet.penetration...population)
prom
#
CoefVar <- (desv/prom)*100
CoefVar

summary(orangeec)
#
mean(orangeec$Creat.Ind...GDP)
mean(orangeec$Creat.Ind...GDP, na.rm=TRUE)
prom <- mean(orangeec$Creat.Ind...GDP, na.rm=TRUE)




#
sd(orangeec$Creat.Ind...GDP)
sd(orangeec$Creat.Ind...GDP, na.rm=TRUE)
desv <- sd(orangeec$Creat.Ind...GDP, na.rm=TRUE)

#
CoefVar <- (desv/prom)*100
CoefVar
#//////////////////////////////////////////////////clase23
eficientes <- mean(mtcars$mpg)
eficientes

mtcars <- mtcars %>% 
  mutate(Mas_eficientes=ifelse(mpg<eficientes,
                               "bajo promedio", "en o sobre promedio"))

Mas_veloces <- mtcars[mtcars$qsec<16,]
Mas_veloces

mtcars <- mtcars %>%
  mutate(Velocidad_Cuarto_milla=ifelse(qsec < 16,
                                       "Menos de 16 segs",
                                       "Mas de 16 segs"))
#
mtcars <- mtcars %>% 
  mutate(Peso_kilos=(wt/2)*1000)
mtcars <- mtcars %>% 
  mutate(Peso=ifelse(Peso_kilos <= 1500,
                     "Livianos", "Pesados"))
#//////////////////////////////////////////////////clase24
orangeec <- orangeec %>%
  mutate(Crecimiento_GDP = ifelse(GDP.Growth.. >= 2.5,
                                  "2.5% o mas", "Menos de 2.5%"))
#
orangeec <- orangeec %>%
  mutate(Anaranjados=ifelse(Creat.Ind...GDP >= 2.5,
         "Mas anaranjados", "Menos anaranjados"))
#ranking
orangeec %>% 
  arrange(desc(Creat.Ind...GDP))

TopNaranjas <- orangeec %>%
  filter(Country %in% c("Mexico", "Panama", "Argentina", "Colombia", "Brazil"))
TopNaranjas
#
TopNaranjas %>%
  arrange(desc(Creat.Ind...GDP))
#//////////////////////////////////////////////////clase25
mtcars %>% 
  arrange(desc(Peso_kilos))

Mas_pesados <- ( mtcars %>% arrange( desc( mtcars$peso_kilos ) ) ) [1:4,]
Mas_pesados

#
ggplot(Mas_pesados, aes(x=hp, y=mpg))+
  geom_point()+
  facet_wrap(~model)
#
ggplot(mtcars, aes(x=cyl, y=mpg, size=Peso_kilos))+ geom_point(aes(color=Peso_kilos))+ facet_wrap(~ am)
colores <- c("rojo", "amarillo", "azul", "verde")
mtcars$Peso_kilos <- as.factor(mtcars$Peso_kilos) levels(mtcars$Peso_kilos) <- colores
ggplot(mtcars, aes(x = cyl, y = mpg, size = Peso_kilos, col = Peso_kilos)) + geom_point() + facet_wrap(~ am)

#Reto 
ggplot(mtcars, aes(x = cyl, y = mpg, size = Peso_kilos, color=Peso_kilos)) + 
  geom_point() +
  facet_wrap(~ am) +
  labs(x = "Cilindros",
       y = "Millas por galón",
       title = "Relación Millas por galón, cilindros y peso en kilos") +
  theme(legend.position = "bottom",
        plot.title=element_text(hjust=0.5, size=10, face='bold'),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )
#//////////////////////////////////////////////////clase26
ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                        y=Services...GDP, size=GDP.PC))+
  geom_point()+
  facet_wrap(~Country)
#
ggplot(TopNaranjas, aes(x=Education.invest...GDP,
                        y=Creat.Ind...GDP, size=Unemployment))+
  geom_point()+
  facet_wrap(~Country)

#
myColors <- brewer.pal(9,"Reds")

ggplot(TopNaranjas, aes(x=Internet.penetration...population,
                        y=GDP.PC, fill=Creat.Ind...GDP))+
  geom_tile()+
  facet_wrap(~Country)+
  scale_fill_gradientn(colors=myColors)
  #//////////////////////////////////////////////////clase27
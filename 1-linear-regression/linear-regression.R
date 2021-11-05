### CASO BEAUTY 1

# cargamos datos desde el working directory
library(readxl)
Beauty <- read_excel("../data/Beauty.xlsx")

summary(Beauty)

### distribucion de wage: asimetria derecha hay que tomar logaritmos

hist(Beauty$wage, breaks=15,col="red")

#matriz de correlaciones
round(cor(Beauty),3)

#modelos sencillos
modelo1<-lm(log(wage)~educ, data=Beauty)  #endogeneidad, beta sesgado por omision de variables relevantes, QUE ADEMAS estan correlacionadas con educ
# tmabi�n podra haber heterocedasticidad, pero la heterocedasticidad provoca que los betas estimados tengan mayor VARIANZA, pero siguen siendo INSESGADOS
summary(modelo1)


modelo2<-lm(log(wage)~educ+exper+looks, data=Beauty) # interpretacion de betas y en logs x en nivel
summary(modelo2)

# con cuadrados
modelo3<-lm(log(wage)~educ+looks+exper+I(exper^2), data=Beauty) # relacion en forma de U invertida
summary(modelo3)



# donde est� el efecto maximo de la experiencia en el salario
expermax<--modelo3$coefficients[4]/(2*modelo3$coefficients[5])
expermax

# interaccion entre sexo y educacion
modelo4<-lm(log(wage)~educ*female+exper+looks, data=Beauty) #mayor efecto de educacion en mujeres que en hombres, pero resultado con poca evidencia p<0.10

summary(modelo4)

#fullmodel
modelo5<-lm(log(wage)~., data=Beauty)

summary(modelo5)

# residuos, diferencia entre Y observada e Y ajustada, est�n ya calculados dentro del objeto modelo5

residuos<-modelo5$residuals

# suma de residuos redondeada a 3 decimales

round(sum(residuos),3) # vale 0 pero eso no indica nada, es una consecuencia de estimar el modelo mediante MCO, la suma de residuos SIEMPRE vale 0

# distincion entre guapos y feos
Beauty$Guapo<-ifelse(Beauty$looks>3,1,0)

Beauty$Feo<-ifelse(Beauty$looks<3,1,0)

modelo6<-lm(log(wage)~educ+exper+Guapo+Feo, data=Beauty) # hay "castigo" para feos no hay prima para guapos
summary(modelo6)

# numero de guapos
nrow(Beauty[Beauty$Guapo==1,])
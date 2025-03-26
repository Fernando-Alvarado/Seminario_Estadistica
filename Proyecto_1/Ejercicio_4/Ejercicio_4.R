

Datos <- read.csv("Preg4.csv")

str(Datos)

Datos$Age <- factor(Datos$Age)
Datos$City <- factor(Datos$City)

str(Datos)

plot(Cases / Pop ~ Age, data = Datos)


#====================================================================

library(ggplot2)

# Creamos el gráfico usando ggplot2
ggplot(Datos, aes(x = Age, y = Cases / Pop)) +
    # Boxplot
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(color = City), 
                width = 0,  # Controla la dispersión horizontal
                size = 2,     # Tamaño de los puntos
                alpha = 0.6   # Transparencia de los puntos
    ) +
    
    # Ajustes de etiquetas y formato
    labs(x = "Age", y = "Cases / Pop", 
         title = "Cancer de pulmon") +
    scale_fill_manual(values = c("white", "white", "white", "white", "white")) +
    scale_color_manual(values = c("blue", "red", "cyan3", "green", "purple")) +
    theme_minimal()


Datos$logPop <- log(Datos$Pop)


modelo_1 <- glm(Cases ~ Age * City + offset(logPop) , 
                family = poisson(link = "log"),
                data = Datos)

summary(modelo_1)


deviance(modelo_1)/df.residual(modelo_1)


library(DHARMa)  

modelo_1_res <- simulateResiduals(fittedModel = modelo_1)

plot(modelo_1_res )



library(multcomp)
K <- cbind(0, diag(19))
m <- rep(0, 19)
summary(glht(modelo_1, linfct = K, rhs = m), test = Chisqtest()) 
summary(glht(modelo_1, linfct = K, rhs = m)) 



modelo_2 <- glm(Cases ~ Age + City + offset(logPop) , 
                family = poisson(link = "log"),
                data = Datos)

summary(modelo_2)


deviance(modelo_2)/df.residual(modelo_2)



modelo_2_res <- simulateResiduals(fittedModel = modelo_2)

plot(modelo_2_res )



K <- cbind(0, diag(7))
m <- rep(0, 7)
summary(glht(modelo_2, linfct = K, rhs = m), test = Chisqtest()) 
summary(glht(modelo_2, linfct = K, rhs = m)) 

anova(modelo_1, modelo_2, test = "Chisq")



modelo_3 <- glm(Cases ~ Age + offset(logPop) , 
                family = poisson(link = "log"),
                data = Datos)

summary(modelo_3)


deviance(modelo_3)/df.residual(modelo_3)



modelo_3_res <- simulateResiduals(fittedModel = modelo_3)

plot(modelo_3_res )



K <- cbind(0, diag(4))
m <- rep(0, 4)
summary(glht(modelo_3, linfct = K, rhs = m), test = Chisqtest()) 
summary(glht(modelo_3, linfct = K, rhs = m)) 

anova(modelo_2, modelo_3, test = "Chisq")

#-----------------------------------------------------------------------

library(MASS)
Datos$AgePrima <- 0
Datos[Datos$Age == "40-54",]$AgePrima <- 52
Datos[Datos$Age == "55-59",]$AgePrima <- 57
Datos[Datos$Age == "60-64",]$AgePrima <- 62
Datos[Datos$Age == "65-69",]$AgePrima <- 67
Datos[Datos$Age == "70-74",]$AgePrima <- 72
Datos$AgePrima2 <- Datos$AgePrima^2

modelo_4 <- glm.nb(Cases ~ AgePrima + offset(logPop) , 
                   link = "log",
                   data = Datos)

summary(modelo_4)


deviance(modelo_4)/df.residual(modelo_4)



modelo_4_res <- simulateResiduals(fittedModel = modelo_4)

plot(modelo_4_res )



K <- cbind(0, diag(1))
m <- rep(0)
summary(glht(modelo_4, linfct = K, rhs = m), test = Chisqtest()) 
summary(glht(modelo_4, linfct = K, rhs = m)) 

c(AIC(modelo_1), AIC(modelo_2), AIC(modelo_3), AIC(modelo_4))
c(BIC(modelo_1), BIC(modelo_2), BIC(modelo_3), BIC(modelo_4))





modelo_5 <- glm.nb(Cases ~ AgePrima + AgePrima2 + offset(logPop) , 
                   link = "log",
                   data = Datos)

summary(modelo_5)


deviance(modelo_5)/df.residual(modelo_5)



modelo_5_res <- simulateResiduals(fittedModel = modelo_5)

plot(modelo_5_res )



K <- cbind(0, diag(1))
m <- rep(0)
summary(glht(modelo_4, linfct = K, rhs = m), test = Chisqtest()) 
summary(glht(modelo_4, linfct = K, rhs = m)) 

c(AIC(modelo_1), AIC(modelo_2), AIC(modelo_3), AIC(modelo_4), AIC(modelo_5))
c(BIC(modelo_1), BIC(modelo_2), BIC(modelo_3), BIC(modelo_4), BIC(modelo_5))

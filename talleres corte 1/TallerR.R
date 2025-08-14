## Calcular los cambios promedio 
##en la presión arterial sistólica y diastólica antes y después del programa.

TallerR <- read_excel("C:/Users/monic/Downloads/Taller de clase (2).xlsx", sheet = "Datos")

Pr_Inicial_PAS<-mean(TallerR$PAS_INICIAL)
Pr_Inicial_PAD<-mean(TallerR$PAD_INICIAL)
Pr_Final_PAS<-mean(TallerR$PAS_FINAL)
Pr_Final_PAD<-mean(TallerR$PAD_FINAL)

((Pr_Inicial_PAS - Pr_Final_PAS)/Pr_Inicial_PAS)*100

((Pr_Inicial_PAD - Pr_Final_PAD)/Pr_Inicial_PAD)*100

presion_fem<-subset(TallerR, TallerR$GENERO == "f" & TallerR$PAS_INICIAL)

summary(TallerR)
sum(TallerR$GENERO == "f")
sum(TallerR$GENERO == "m")


install.packages("dplyr")
library(dplyr)
install.packages("lme4")
library(lme4)
install.packages("ggplot2")
library(ggplot2)

modelo <- lmer(TallerR$PAS_INICIAL ~ TallerR$EDAD + (1 | TallerR$GENERO), data = TallerR)
summary(modelo)

TallerR2 = TallerR %>% 
  mutate(diferencia_PAS = PAS_FINAL - PAS_INICIAL) %>% 
  mutate(diferencia_PAD = PAD_FINAL - PAD_INICIAL)

View(TallerR2)

## Comparar PAS y PAD inicial vs final

## Sistolica
t.test(PAS_INICIAL ~ GENERO, data = TallerR, var.equal = TRUE)

## Diastolica
t.test(PAD_INICIAL ~ GENERO, data = TallerR, var.equal = TRUE)







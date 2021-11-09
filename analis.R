# tornout e voto compulsório
#Fonte 
#https://www.idea.int/data-tools/data/voter-turnout

# texto itnrodutório (paperzinho)


library(readxl)
base <- read_excel("D:/novas atualizações na pasta D/TORNOUT/idea_export_40_616830a0840fa.xls")
library(tidyverse)

# somente eleições parlamentares



str(base)#todos
str(base$Tornout)#Tornout nao tá numérico
base$Tornout <- as.numeric(base$Tornout) # recod
str(base$Tornout)#verificar se deu certo
dim(base) # numero de linhas


#todos os países da analise
tibble(base%>%
         arrange(desc(Tornout))%>%select(Country, Tornout, Year, Compulsory))

summary(base$Tornout)#dispersão Tornout
table(base$Region)
table(base$Compulsory)
summary(base$Year)
hist(base$Tornout)


by(base$Tornout, base$Region, mean)
by(base$Tornout, base$Compulsory, mean)
prop.table(table(base$Region, base$Compulsory),1)
boxplot(base$Tornout ~ base$Region, 
        col = "orange", ylab="Comparecimento", xlab="Região")
boxplot(base$Tornout ~ base$Compulsory, 
        col = "orange", ylab="Comparecimento", xlab="Voto Obrigatório?")

library(huxtable)
library(coefplot)
modelo_Obrig <- lm(Tornout ~ Compulsory, data = base)
summary(modelo_Obrig)
modelo_region <- lm(Tornout ~ Region, data = base)
summary(modelo_region)
modelo_full <- lm(Tornout ~ Compulsory + Region, data = base)
summary(modelo_full)
huxreg(modelo_Obrig, modelo_region, modelo_full, stars = c(`*` = 0.1, `**` = 0.05,
                                  `***` = 0.01), statistics = c("N. obs." = "nobs", "R2" = "r.squared",
                                                                "AIC" = "AIC"))
coefplot(modelo_full, intercept = F)

# diagnóstico

plot(modelo_full) # a segunda

library(olsrr)
ols_vif_tol(modelo_full)

resid <- (cbind(base$Tornout, predict(modelo_full), residuals(modelo_full)))

resid
#argelia e venzeula casos em que ocorreu maior imprevisbilidade engativa (menor participação que o previsto pelo modelo)

# Brasil caso 21



# os somente na região desejada ( medindo efeito de voto compulsório) ####

table(base$Region)
Americas <- base%>%
  filter(Region == "Americas")
table(Americas$Region)

Asia_Oceania <- base%>%
  filter(Region == "Asia"| Region == "Oceania")
table(Asia_Oceania$Region)


Europa <- base%>%
  filter(Region == "Leste Europeu"| Region == "Europa Ocidental")
table(Europa$Region)

Africa <- base%>%
  filter(Region == "Africa")
table(Africa$Region)


# Africa
modelo_Africa <- lm(Tornout ~ Compulsory, data = Africa)
summary(modelo_Africa)


# Americas
modelo_Americas <- lm(Tornout ~ Compulsory, data = Americas)
summary(modelo_Americas)


#Asia/Oceania
modelo_AsiaOceania1 <- lm(Tornout ~ Compulsory, data = Asia_Oceania)
summary(modelo_AsiaOceania1)
modelo_AsiaOceania2 <- lm(Tornout ~ Compulsory + Region, data = Asia_Oceania)
summary(modelo_AsiaOceania2)


#Europa
modelo_Europa1 <- lm(Tornout ~ Compulsory, data = Europa)
summary(modelo_Europa1)
modelo_Europa2 <- lm(Tornout ~ Compulsory + Region, data = Europa)
summary(modelo_Europa2)

huxreg(modelo_Africa, modelo_Americas, modelo_AsiaOceania2, modelo_Europa2, 
       stars = c(`*` = 0.1, `**` = 0.05,`***` = 0.01), 
       statistics = c("N. obs." = "nobs", "R2" = "r.squared","AIC" = "AIC"))





# modelo Rank (BAS)



#BAS priori 2####
base2 <- base  %>% select(Tornout, Region, Compulsory)

library(BAS)
#model bas
modelbas<-bas.lm(Tornout ~.,data=base2, prior="ZS-null", modelprior = uniform(), method = "MCMC")
plot(modelbas, which=1, add.smooth=F)
#2. cumulative probability
plot(modelbas, which=2)


#3. model dimension plot
plot(modelbas, which=2)
#4.PIP
plot(modelbas, which = 4, ask=FALSE, caption="", sub.caption="")

#model rank
image(modelbas, rotate = F)

summary(modelbas)



#OCenia YES
mun <- data.frame(Region = "Oceania" , Compulsory = "Yes")
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values

#Leste Europ No
mun <- data.frame(Region = "Leste Europeu" , Compulsory = "No")
pred = predict(modelbas, newdata = mun, estimator = "BPM",  se.fit=T)
pred$fit  # fitted values





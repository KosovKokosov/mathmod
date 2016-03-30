test
eddypro=read.csv("eddypro.csv", skip = 1)[-1, c(1:69, 71:77)] 
eddypro = eddypro[ , c(-32, -33, -37, -38, -63)] 
eddypro$daytime = as.double(eddypro$daytime) 
for(i in c(4, 6:length(eddypro))){ 
  eddypro[, i] = as.double(as.vector(eddypro[, i])) 
} 
eddypro[eddypro == -9999] = NA 
eddypro[2316:3803, ] -> eddypro 
eddypro[eddypro$daytime == "4", ] -> eddypro 
eddypro2=na.exclude(eddypro) 
cor.matrix = round(cor(eddypro2[, c(-1, -2, -3, -4)]), 2)
#в первую модель было выбранно более значимые факторы
model1=lm(RH~(air_temperature+es+LE+h2o_flux+rand_err_Tau+TKE+H+air_heat_capacity+e+co2_flux+h2o_molar_density+air_density)^12,data=eddypro)
anova(model1)
model2=lm(RH~(air_temperature+es+LE+h2o_flux+rand_err_Tau+e+co2_flux+h2o_molar_density)^8,data=eddypro)
anova(model2)
#после было убранно наименее значимый фактор
model3=lm(RH~(air_temperature+es+LE+h2o_flux+rand_err_Tau+e+h2o_molar_density)^7,data=eddypro)
anova(model3)
summary(model3)
summary(model2)
#после было убранно наименее значимый фактор
model4=lm(RH~(air_temperature+es+LE+rand_err_Tau+e)^5,data=eddypro)
anova(model4)
summary(model4)
#после было убранно наименее значимый фактор
model5=lm(RH~(air_temperature+es+LE+e)^4,data=eddypro)
anova(model5)
#после было убранно наименее значимый фактор
model6=lm(RH~(air_temperature+es+LE+e)^4-air_temperature:es-air_temperature:es:e-air_temperature:LE:e-es:LE:e,data=eddypro)
anova(model6)
#был проведен анализ anova и после было убранно группа факторов которые между собой давали высокую значимость
model7=lm(RH~(air_temperature+es+LE+e)^4-air_temperature:es-air_temperature:es:e-air_temperature:LE:e-es:LE:e-LE:e-air_temperature:es:LE:e,data=eddypro)
anova(model7)
#оканчательный вариант
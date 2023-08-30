
rm(list=ls())

library(dplyr)
library(ggplot2)
library(gridExtra)
library(googlesheets4)
library(emmeans)
library(lme4)
library(nlme)
library(Factoshiny)

load("C:/Users/sgarnier/OneDrive - Cirad/Stage/R/Data/data.Rdata")




# lecture produit sur google drive ----------------------------------------


### pour ouvrir le Google sheet "product_pesticide"
sheet_url<-"https://docs.google.com/spreadsheets/d/1hjXlk5_ckY_LBcDL0mu2k2NKmm6s_-OJGu0d_5qQQQo/edit#gid=0"
Name_found<-read_sheet(sheet_url,sheet=1)   


#tris a plat 

Farm<-data %>% 
  select(id_farmer,district, province, nb_cycle,plot_size,Total_size,nb_plot,Experience, Age,Nb_household,Nb_active_labor)%>%
  unique()

Farm2<-data %>%   # nb produit per cycle 
  select(id_farmer, cycl, id_application, How_many_product) %>%
  unique() %>% 
  group_by(id_farmer, cycl) %>%
  summarise(nb_prod_per_cycle = sum(How_many_product))%>% 
  group_by(id_farmer) %>%
  summarise(prod_per_cycle = mean(nb_prod_per_cycle))

Farm3<-data %>% #other info and mean per cycle per farmer
  select(id_farmer,  cycl, n_per_cycle,yield_hectar,fertilizer_amount,n_apl_fert,Kg_seed) %>%
  unique() %>% 
  group_by(id_farmer) %>%
  summarise(mean_n_p_per_cycle = mean(n_per_cycle),
            mean_yield_hectar=mean(na.omit(yield_hectar)),
            mean_fertilizer_amount=mean(na.omit(fertilizer_amount)),
            mean_n_apl_fert=mean(na.omit(n_apl_fert)),
            mean_Kg_seed=mean(na.omit(Kg_seed))) 

Final_Farm <- Farm %>%
  left_join(Farm2, by = "id_farmer") %>%
  left_join(Farm3, by = "id_farmer")


apply(na.omit(Final_Farm),2,min)
apply(na.omit(Final_Farm),2,max)

colMeans(na.omit(Final_Farm[4:17]))
unique(data$district)
#graph 

data %>%    #nombre de cycle par district 
  select(id_farmer, district, province, nb_cycle) %>%
  unique() %>% 
  ggplot(aes(x=reorder(district,as.numeric(nb_cycle)), y=as.numeric(nb_cycle), color=province)) +
  geom_boxplot()+geom_hline(yintercept=mean(as.numeric(Final_Farm$nb_cycle)))+
  labs(x = "District", y = "Nombre de cycle par fermier")

data %>%  # plot size par district
  select(id_farmer, district, province, plot_size) %>%
  unique() %>% 
  ggplot(aes(x=reorder(district,plot_size), y=plot_size, color=province)) +
  geom_boxplot()+geom_hline(yintercept=mean(na.omit(Final_Farm$plot_size)))+
  labs(x = "District", y = "Taille de la parcelle")


nb_prod<-data %>%   # nb produit par cycle 
  select(id_farmer, district,nb_cycle,province, cycl, id_application, How_many_product) %>%
  unique() %>% 
  group_by(id_farmer,district,province, nb_cycle, cycl) %>%
  summarise(nb_prod_per_cycle = sum(How_many_product))

ggplot(nb_prod,aes(x=reorder(district,nb_prod_per_cycle), y=nb_prod_per_cycle, color=province)) +
  geom_boxplot()+
  geom_hline(yintercept=mean(na.omit(nb_prod$nb_prod_per_cycle)))+
  labs(x = "District", y = "Nombre de produit par cycle")


nb_app<-data %>% #nb app par cycle 
  select(id_farmer, district,province, nb_cycle, cycl, n_per_cycle) %>%
  unique() %>% 
  group_by(id_farmer, district,province, nb_cycle) %>%
  summarise(mean_n_p_per_cycle = mean(n_per_cycle)) 

ggplot(nb_app, aes(x=reorder(district,mean_n_p_per_cycle), y = mean_n_p_per_cycle, color=province)) + geom_boxplot()+
  labs(x = "District", y = "Nombre d'application par cycle")+
  geom_hline(yintercept=mean(na.omit(nb_app$mean_n_p_per_cycle)))



 
 #### "tri croisée"

# nb app par cycle
data %>% 
select(id_farmer, nb_cycle, cycl, n_per_cycle) %>%
  unique() %>% 
  ggplot(aes(x=cycl, y=n_per_cycle, color=nb_cycle)) +
  labs(x = "Cycle", y = "Nombre d'application par cycle")+
  geom_boxplot()

test<-data %>% select(id_farmer, nb_cycle, cycl, n_per_cycle) %>%
  unique()

fit2 = lm(n_per_cycle ~ cycl,data = test)
summary(fit2)
emmeans(fit2, pairwise ~ cycl)

toto = data %>% 
  select(id_farmer, district, nb_cycle, cycl, id_application) %>%
  unique() %>% 
  group_by(id_farmer, district, nb_cycle, cycl) %>%
  summarise(nb_app = length(unique(id_application))) %>%
  group_by(id_farmer, district, nb_cycle) %>%
  summarise(mean_nb_app_per_cycle = mean(nb_app)) 

fit1 = lme(mean_nb_app_per_cycle ~ nb_cycle,
           random = ~1|district, 
           data = toto, method = "REML")

fit2 = gls(mean_nb_app_per_cycle ~ nb_cycle,
           data = toto, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2)


fit = lm(mean_nb_app_per_cycle ~ nb_cycle,data = toto)
summary(fit)

anova(fit)

emmeans(fit, pairwise ~ nb_cycle)






# nb produit par cycle catégorisé par le nb de cycle total 
data %>% 
  select(id_farmer, nb_cycle, cycl, id_application, How_many_product) %>%
  unique() %>% 
  group_by(id_farmer, nb_cycle, cycl) %>%
  summarise(nb_prod_per_cycle = sum(How_many_product)) %>%
  ggplot(aes(x=cycl, y=nb_prod_per_cycle, color=nb_cycle)) +
  geom_boxplot()



# nb produit moyen par cycle pour ceux qui font 1, 2 ou 3 cycles
toto = data %>% 
  select(id_farmer, district, nb_cycle, cycl, id_application, How_many_product) %>%
  unique() %>% 
  group_by(id_farmer, district, nb_cycle, cycl) %>%
  summarise(nb_prod_per_cycle = sum(How_many_product)) %>%
  group_by(id_farmer, district, nb_cycle) %>%
  summarise(mean_nb_prod_per_cycle = mean(nb_prod_per_cycle)) 

toto$nb_cycle <- factor(toto$nb_cycle , levels = c(1,2,3))
ggplot(toto, aes(x=district, y = mean_nb_prod_per_cycle)) + geom_boxplot()

#par cycle

etiquettes_df <- data.frame(
  nb_cycle = c(1, 2, 3),
  label = c("a", "b", "ab")
)

ggplot(toto, aes(x = nb_cycle, y = mean_nb_prod_per_cycle)) +
  geom_boxplot() +
  geom_text(data = etiquettes_df, aes(x = nb_cycle, y = 16 + 1, label = label), 
            position = position_dodge(width = 0.75)) +
  labs(x = "Nombres de cycle", y = "Nombres de produit par cycle")




#par district
ggplot(toto, aes(x=district, y = mean_nb_prod_per_cycle)) + geom_boxplot()



## test effet aleatoir
fit1 = lme(mean_nb_prod_per_cycle ~ nb_cycle,
           random = ~1|district, 
           data = toto, method = "REML")

fit2 = gls(mean_nb_prod_per_cycle ~ nb_cycle,
           data = toto, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2)

#mieux sans 
fit = lm(mean_nb_prod_per_cycle ~ nb_cycle, data = toto)
summary(fit)
emmeans(fit, pairwise ~ nb_cycle) # difference entre 1 et 2 mais c'est tout



# rendement moyen par cycle pour ceux qui font 1, 2 ou 3 cycles
toto = data %>% 
  select(id_farmer, district, nb_cycle, cycl, yield_hectar) %>%
  unique() %>% 
  group_by(id_farmer, district, nb_cycle) %>%
  summarise(yield_hectar = mean(yield_hectar)) %>%
  mutate(yield_hectar = case_when(yield_hectar >= 40 ~ NA, 
                                  yield_hectar < 40 ~ yield_hectar)) #une valeur extreme =50

toto$nb_cycle <- factor(toto$nb_cycle , levels = c(1,2,3))

etiquettes_df <- data.frame(
  nb_cycle = c(1, 2, 3),
  label = c("a", "ab", "b")
)

ggplot(toto, aes(x = nb_cycle, y = yield_hectar)) +
  geom_boxplot() +
  geom_text(data = etiquettes_df, aes(x = nb_cycle, y = 6.4 + 1, label = label), 
            position = position_dodge(width = 0.75)) +
  labs(x = "Nombres de cycle", y = "Rendement moyen par cycle et par hectar")


##test effet aleatoire district 
toto2 <- na.omit(toto)
fit1 = lme(yield_hectar ~ nb_cycle,
           random = ~1|district, 
           data = toto2, method = "REML")

fit2 = gls(yield_hectar ~ nb_cycle,
           data = toto2, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) #mieux sans


fit = lm(yield_hectar ~ nb_cycle, data = toto)
summary(fit)
emmeans(fit, pairwise ~ nb_cycle) #dif entre 1 et 3 

## test niveau d'education 
data$Education

#nombre d'application
Ed = data %>% 
  select(id_farmer, district, Education, cycl, id_application) %>%
  unique() %>% 
  group_by(id_farmer, district, Education, cycl) %>%
  summarise(nb_app = length(unique(id_application))) %>%
  group_by(id_farmer, district, Education) %>%
  summarise(mean_nb_app_per_cycle = mean(nb_app)) 

ggplot(Ed, aes(x=Education, y = mean_nb_app_per_cycle)) + geom_boxplot()+
  labs(x = "Niveau d'education", y = "Nombre d'application par cycle")

fit = lm(mean_nb_app_per_cycle ~ Education, data = Ed)
summary(fit)
emmeans(fit, pairwise ~ Education) #pas de diff

fit1 = lme(mean_nb_app_per_cycle ~ Education,
           random = ~1|district, 
           data = Ed, method = "REML")

fit2 = gls(mean_nb_app_per_cycle ~ Education,
           data = Ed, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) #mieux sans


# nombre de produit 
Ed = data %>% 
  select(id_farmer, district, Education, cycl, id_application, How_many_product) %>%
  unique() %>% 
  group_by(id_farmer, district, Education, cycl) %>%
  summarise(nb_prod_per_cycle = sum(How_many_product)) %>%
  group_by(id_farmer, district, Education) %>%
  summarise(mean_nb_prod_per_cycle = mean(nb_prod_per_cycle)) 

ggplot(Ed, aes(x=Education, y = mean_nb_prod_per_cycle)) + geom_boxplot()+
  labs(x = "Niveau d'education", y = "Nombre de produit moyen par cycle")


fit1 = lme(mean_nb_prod_per_cycle ~ Education,
           random = ~1|district, 
           data = Ed, method = "REML")

fit2 = gls(mean_nb_prod_per_cycle ~ Education,
           data = Ed, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) #mieux avec

fit = lme(mean_nb_prod_per_cycle ~ Education,
           random = ~1|district, 
           data = Ed, method = "REML")
summary(fit)
emmeans(fit, pairwise ~ Education) #pas de diff


## test varieté de riz 
data$Rice_variety

table(data$Rice_variety)
#keep only variety use more than 10 time 

data$Rice_variety[data$Rice_variety %in% c("Krahorm thngun", "Pkar_Romdoul","Pkar_khney")] = "Other"

#nombre d'application
RV = data %>% 
  select(id_farmer, district, Rice_variety, cycl, id_application) %>%
  unique() %>% 
  group_by(id_farmer, district, Rice_variety, cycl) %>%
  summarise(nb_app = length(unique(id_application))) %>%
  group_by(id_farmer, district, Rice_variety) %>%
  summarise(mean_nb_app_per_cycle = mean(nb_app)) 

ggplot(RV, aes(x=Rice_variety, y = mean_nb_app_per_cycle)) + geom_boxplot()+
  labs(x = "Rice variety", y = "Nombre d'application par cycle")

fit = lm(mean_nb_app_per_cycle ~ Rice_variety, data = RV)
summary(fit)                                                ##la ya des diff significative 
emmeans(fit, pairwise ~ Rice_variety) # mais ici pas de diff ??


fit1 = lme(mean_nb_app_per_cycle ~ Rice_variety,
           random = ~1|district, 
           data = RV, method = "REML")

fit2 = gls(mean_nb_app_per_cycle ~ Rice_variety,
           data = RV, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) #mieux sans



# nombre de produit 
RV = data %>% 
  select(id_farmer, district, Rice_variety, cycl, id_application, How_many_product) %>%
  unique() %>% 
  group_by(id_farmer, district, Rice_variety, cycl) %>%
  summarise(nb_prod_per_cycle = sum(How_many_product)) %>%
  group_by(id_farmer, district, Rice_variety) %>%
  summarise(mean_nb_prod_per_cycle = mean(nb_prod_per_cycle)) 

ggplot(RV, aes(x=Rice_variety, y = mean_nb_prod_per_cycle)) + geom_boxplot()+
  labs(x = "Rice variety", y = "Nombre de produit moyen par cycle")


fit1 = lme(mean_nb_prod_per_cycle ~ Rice_variety,
           random = ~1|district, 
           data = RV, method = "REML")

fit2 = gls(mean_nb_prod_per_cycle ~ Rice_variety,
           data = RV, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) #mieux avec


fit = lme(mean_nb_prod_per_cycle ~ Rice_variety,
           random = ~1|district, 
           data = RV, method = "REML")
summary(fit)                                             ##idem la diff
emmeans(fit, pairwise ~ Rice_variety)                    # mais pas ici  ??

##ajoute effet aleatoire du fermier 


## test plot size
data$plot_size

#nombre d'application
PS = data %>% 
  select(id_farmer, district, plot_size, cycl, id_application) %>%
  unique() %>% 
  group_by(id_farmer, district, plot_size, cycl) %>%
  summarise(nb_app = length(unique(id_application))) %>%
  group_by(id_farmer, district, plot_size) %>%
  summarise(mean_nb_app_per_cycle = mean(nb_app)) 

ggplot(PS, aes(x=plot_size, y = mean_nb_app_per_cycle)) + geom_point()+
  labs(x = "plot size", y = "Nombre d'application par cycle")

fit = lm(mean_nb_app_per_cycle ~ plot_size, data = PS)
summary(fit)                            #pas de dif                     

PS<-na.omit(PS)
fit1 = lme(mean_nb_app_per_cycle ~ plot_size,
           random = ~1|district, 
           data = PS, method = "REML")

fit2 = gls(mean_nb_app_per_cycle ~ plot_size,
           data = PS, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) #mieux sans


# nombre de produit 
PS = data %>% 
  select(id_farmer, district, plot_size, cycl, id_application, How_many_product) %>%
  unique() %>% 
  group_by(id_farmer, district, plot_size, cycl) %>%
  summarise(nb_prod_per_cycle = sum(How_many_product)) %>%
  group_by(id_farmer, district, plot_size) %>%
  summarise(mean_nb_prod_per_cycle = mean(nb_prod_per_cycle)) 

ggplot(PS, aes(x=plot_size, y = mean_nb_prod_per_cycle)) + geom_point()+
  labs(x = "plot size", y = "Nombre de produit moyen par cycle")


PS<-na.omit(PS)
fit1 = lme(mean_nb_prod_per_cycle ~ plot_size,
           random = ~1|district, 
           data = PS, method = "REML")

fit2 = gls(mean_nb_prod_per_cycle ~ plot_size,
           data = PS, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) #mieux avec

fit = lme(mean_nb_prod_per_cycle ~ plot_size,
           random = ~1|district, 
           data = PS, method = "REML")

summary(fit)


## test KG seed
data$Kg_seed


#nombre d'application
KS = data %>% 
  select(id_farmer, district, Kg_seed, cycl, id_application) %>%
  unique() %>% 
  mutate(Kg_seed = case_when(Kg_seed >= 1000 ~  NA, 
                                  Kg_seed < 1000 ~ Kg_seed))%>%
  group_by(id_farmer, district, Kg_seed, cycl) %>%
  summarise(nb_app = length(unique(id_application))) %>%
  group_by(id_farmer, district, Kg_seed) %>%
  summarise(mean_nb_app_per_cycle = mean(nb_app)) 

ggplot(KS, aes(x=Kg_seed, y = mean_nb_app_per_cycle)) + geom_point()+
  labs(x = "Kg_seed", y = "Nombre d'application par cycle")

fit = lm(mean_nb_app_per_cycle ~ Kg_seed, data = KS)
summary(fit)                            #pas de dif  

KS<-na.omit(KS)
fit1 = lme(mean_nb_app_per_cycle ~ Kg_seed,
           random = ~1|district, 
           data = KS, method = "REML")

fit2 = gls(mean_nb_app_per_cycle ~ Kg_seed,
           data = KS, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) #mieux sans 

# nombre de produit 
KS = data %>% 
  select(id_farmer, district, Kg_seed, cycl, id_application, How_many_product) %>%
  unique() %>% 
  mutate(Kg_seed = case_when(Kg_seed >= 1000 ~  NA, 
                             Kg_seed < 1000 ~ Kg_seed))%>% 
  group_by(id_farmer, district, Kg_seed, cycl) %>%
  summarise(nb_prod_per_cycle = sum(How_many_product)) %>%
  group_by(id_farmer, district, Kg_seed) %>%
  summarise(mean_nb_prod_per_cycle = mean(nb_prod_per_cycle)) 

ggplot(KS, aes(x=Kg_seed, y = mean_nb_prod_per_cycle)) + geom_point()+
  labs(x = "Kg_seed", y = "Nombre de produit moyen par cycle")


KS<-na.omit(KS)
fit1 = lme(mean_nb_prod_per_cycle ~ Kg_seed,
           random = ~1|district, 
           data = KS, method = "REML")

fit2 = gls(mean_nb_prod_per_cycle ~ Kg_seed,
           data = KS, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) #effet aleatoire ++

summary(fit1) #pas d'effet 

## test fertilizer_amount
data$fertilizer_amount

#nombre d'application
FA = data %>% 
  select(id_farmer, district, cycl,fertilizer_amount, id_application) %>%
  unique() %>% mutate(fertilizer_amount = case_when(fertilizer_amount >= 1000 ~  NA, 
                                                    fertilizer_amount < 1000 ~ fertilizer_amount))%>% 
  group_by(id_farmer, district, fertilizer_amount, cycl) %>%
  summarise(nb_app = length(unique(id_application))) %>%
  group_by(id_farmer, district) %>%
  summarise(mean_nb_app_per_cycle = mean(nb_app),
            fert=mean(fertilizer_amount))



FA<-na.omit(FA)
fit1 = lme(mean_nb_app_per_cycle ~ fert,
           random = ~1|district, 
           data = FA, method = "REML")

fit2 = gls(mean_nb_app_per_cycle ~ fert,
           data = FA, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) #pas effet aleatoire

fit = lm(mean_nb_app_per_cycle ~ fert, data = FA)


summary(fit)                            #difference significative et positive !  

ggplot(FA, aes(x = fert, y = mean_nb_app_per_cycle)) +
  geom_point() +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "red") +
  labs(x = "Quantité de fertilisants", y = "Nombre d'application par cycle") +
  scale_y_continuous(limits = c(0, max(FA$mean_nb_app_per_cycle)))+
  scale_x_continuous(limits = c(0, max(FA$fert)))

# nombre de produit 
FA = data %>% 
  select(id_farmer, district, fertilizer_amount, cycl, id_application, How_many_product) %>%
  unique()  %>% mutate(fertilizer_amount = case_when(fertilizer_amount >= 1000 ~  NA, 
                                                     fertilizer_amount < 1000 ~ fertilizer_amount))%>% 
  group_by(id_farmer, district, fertilizer_amount, cycl) %>%
  summarise(nb_prod_per_cycle = sum(How_many_product)) %>%
  group_by(id_farmer, district) %>%
  summarise(mean_nb_prod_per_cycle = mean(nb_prod_per_cycle),
            fert=mean(fertilizer_amount)) 


                                             #difference significative et positive ! 

FA<-na.omit(FA)
fit1 = lme(mean_nb_prod_per_cycle ~ fert,
           random = ~1|district, 
           data = FA, method = "REML")

fit2 = gls(mean_nb_prod_per_cycle ~ fert,
           data = FA, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) # pas d'effet aleatoire

fit = lm(mean_nb_prod_per_cycle ~ fert, data = FA)
summary(fit1)


ggplot(FA, aes(x = fert, y = mean_nb_prod_per_cycle)) +
  geom_point() +
  geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], color = "red") +
  labs(x = "Quantité de fertilisants (Kg)", y = "Nombre de produit moyen par cycle")+
  scale_y_continuous(limits = c(0, max(FA$mean_nb_prod_per_cycle)))+
  scale_x_continuous(limits = c(0, max(FA$fert)))




## test Main_Water_Source
table(data$Main_Water_Source)

data$Main_Water_Source[data$Main_Water_Source %in% "Groundwater"] = "Eau souterraine" 
data$Main_Water_Source[data$Main_Water_Source %in% "Lake_River"] = "Lac ou rivière" 
data$Main_Water_Source[data$Main_Water_Source %in% "Rainwater"] = "Eau de pluie" 
data$Main_Water_Source[data$Main_Water_Source %in% "Other"] = "Autre" 




#nombre d'application
WS = data %>% 
  select(id_farmer, district, Main_Water_Source, cycl, id_application) %>%
  unique() %>% 
  group_by(id_farmer, district, Main_Water_Source, cycl) %>%
  summarise(nb_app = length(unique(id_application))) %>%
  group_by(id_farmer, district, Main_Water_Source) %>%
  summarise(mean_nb_app_per_cycle = mean(nb_app)) 

ggplot(WS, aes(x=Main_Water_Source, y = mean_nb_app_per_cycle)) + geom_boxplot()+
  labs(x = "Principale source d'eau", y = "Nombre d'application par cycle")+
  annotate("text", x = 3, y = 7, label = "p = 0.0325 * ")

fit = lm(mean_nb_app_per_cycle ~ Main_Water_Source, data = WS)
summary(fit)                                                ##la ya une diff significative 
emmeans(fit, pairwise ~ Main_Water_Source) # mais ici pas de diff ??


fit1 = lme(mean_nb_app_per_cycle ~ Main_Water_Source,
           random = ~1|district, 
           data = WS, method = "REML")

fit2 = gls(mean_nb_app_per_cycle ~ Main_Water_Source,
           data = WS, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) #mieux sans

# nombre de produit 
WS = data %>% 
  select(id_farmer, district, Main_Water_Source, cycl, id_application, How_many_product) %>%
  unique() %>% 
  group_by(id_farmer, district, Main_Water_Source, cycl) %>%
  summarise(nb_prod_per_cycle = sum(How_many_product)) %>%
  group_by(id_farmer, district, Main_Water_Source) %>%
  summarise(mean_nb_prod_per_cycle = mean(nb_prod_per_cycle)) 

ggplot(WS, aes(x=Main_Water_Source, y = mean_nb_prod_per_cycle)) + geom_boxplot()+
  labs(x = "Principale source d'eau ", y = "Nombre de produit moyen par cycle")+
  annotate("text", x = 3, y = 16, label = "p = 0.0181 * ")

fit = lm(mean_nb_prod_per_cycle ~ Main_Water_Source, data = WS)
summary(fit)                                             ##idem la diff
emmeans(fit, pairwise ~ Main_Water_Source)                    # mais pas ici  ??



fit1 = lme(mean_nb_prod_per_cycle ~ Main_Water_Source,
           random = ~1|district, 
           data = WS, method = "REML")

fit2 = gls(mean_nb_prod_per_cycle ~ Main_Water_Source,
           data = WS, method = "REML")

AIC(fit1, fit2)
BIC(fit1, fit2)
anova(fit1, fit2) #mieux sans



## test Cultivation_practices
data$Cultivation_practices

#nombre d'application
CP = data %>% 
  select(id_farmer, district, Cultivation_practices, cycl, id_application) %>%
  unique() %>% 
  group_by(id_farmer, district, Cultivation_practices, cycl) %>%
  summarise(nb_app = length(unique(id_application))) %>%
  group_by(id_farmer, district, Cultivation_practices) %>%
  summarise(mean_nb_app_per_cycle = mean(nb_app)) 

ggplot(CP, aes(x=Cultivation_practices, y = mean_nb_app_per_cycle)) + geom_boxplot()+
  labs(x = "Cultivation_practices", y = "Nombre d'application par cycle")

# nombre de produit 
CP = data %>% 
  select(id_farmer, district, Cultivation_practices, cycl, id_application, How_many_product) %>%
  unique() %>% 
  group_by(id_farmer, district, Cultivation_practices, cycl) %>%
  summarise(nb_prod_per_cycle = sum(How_many_product)) %>%
  group_by(id_farmer, district, Cultivation_practices) %>%
  summarise(mean_nb_prod_per_cycle = mean(nb_prod_per_cycle)) 

ggplot(CP, aes(x=Cultivation_practices, y = mean_nb_prod_per_cycle)) + geom_boxplot()+
  labs(x = "Cultivation_practices", y = "Nombre de produit moyen par cycle")

fit = lm(mean_nb_prod_per_cycle ~ Cultivation_practices, data = CP)
summary(fit)                                             ##pas de diff
emmeans(fit, pairwise ~ Cultivation_practices)                    # pas de diff


## test district
data$district

#nombre d'application
district = data %>% 
  select(id_farmer, district, cycl, id_application) %>%
  unique() %>% 
  group_by(id_farmer, district, cycl) %>%
  summarise(nb_app = length(unique(id_application))) %>%
  group_by(id_farmer, district) %>%
  summarise(mean_nb_app_per_cycle = mean(nb_app)) 

ggplot(district, aes(x=district, y = mean_nb_app_per_cycle)) + geom_boxplot()+
  labs(x = "district", y = "Nombre d'application par cycle")

fit = lm(mean_nb_app_per_cycle ~ district, data = district)
summary(fit)                                                ##quelque diff
emmeans(fit, pairwise ~ district) # une seule diff

# nombre de produit 
district = data %>% 
  select(id_farmer, district, cycl, id_application, How_many_product) %>%
  unique() %>% 
  group_by(id_farmer, district, cycl) %>%
  summarise(nb_prod_per_cycle = sum(How_many_product)) %>%
  group_by(id_farmer, district) %>%
  summarise(mean_nb_prod_per_cycle = mean(nb_prod_per_cycle)) 


ggplot(district, aes(x=district, y = mean_nb_prod_per_cycle)) + geom_boxplot()+
  labs(x = "district", y = "Nombre de produit moyen par cycle")

fit = lm(mean_nb_prod_per_cycle ~ district, data = district)
summary(fit)                                            ##quelque diff
emmeans(fit, pairwise ~ district)                  ##quelque diff



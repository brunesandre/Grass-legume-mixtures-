##############################
# ANALISES SILVIO POR ANO
# 
##############################
# Para uso do Silvio
# getwd()
# setwd("C:/Users/silvi/Desktop")
# dados_ano<-read.csv("C:/Users/silvi/Desktop/dados_Silvio_ano.csv",header=TRUE)
# str(dados_ano)


getwd()
##### VARIÁVEIS POR ANO
setwd("/Users/cleberhenriquelopesdesouza/Desktop/Data Analyses/Silvio/")
dados_ano<-read.csv("/Users/cleberhenriquelopesdesouza/Desktop/Data Analyses/Silvio/Dados_Silvio_by_ANO.csv",header=TRUE, sep=",")
dados_ano$bloco<-as.factor(dados_ano$bloco)
str(dados_ano)


# Reordering the Genotype level
levels(dados_ano$tratamento)
dados_ano$tratamento<-factor(dados_ano$tratamento, levels =  c("0","60","120","240","480","Leguminosa"))
levels(dados_ano$tratamento)

modelo_2<-aov(ms_forragem~bloco+bloco:tratamento+tratamento+genotipo+tratamento*genotipo, 
              data=dados_ano, na.action=na.omit)
summary(modelo_2) 


dados_leguminosas<-subset(dados_ano, tratamento=="Leguminosa")
str(dados_leguminosas)


modelo_3<-aov(ms_trevo~bloco+genotipo+genotipo, 
              data=dados_leguminosas, na.action=na.omit)
summary(modelo_3)
library(agricolae)
(tukey_genotipos<-(HSD.test(modelo_3,"genotipo", group=TRUE,alpha = 0.05))$groups)

# ##### TOLERANCIA AO FRIO
# setwd("/Users/cleberhenriquelopesdesouza/Desktop/Data Analyses/Silvio/")
# dados_frio<-read.csv("/Users/cleberhenriquelopesdesouza/Desktop/Data Analyses/Silvio/2019-03-19_Silvio_Dados_frio.csv",header=TRUE, sep=",")
# dados_frio$bloco<-as.factor(dados_frio$bloco)
# dados_frio$ano<-as.factor(dados_frio$ano)
# str(dados_frio)
# # Reordering the Genotype level
# levels(dados_frio$tratamento)
# dados_frio$tratamento<-factor(dados_frio$tratamento, levels =  c("0","60","120","240","480","Leguminosa"))
# levels(dados_frio$tratamento)
# 
# modelo_2<-aov(Tfrio~bloco+bloco:tratamento+tratamento+genotipo+ano*tratamento*genotipo, 
#               data=dados_frio, na.action=na.omit)
# summary(modelo_2) 
# 
# 


##### GENERATE THE TABLE WIHT SE
library(lsmeans)
library("multcompView")
(lsm_mdl_graph<-lsmeans(modelo_2, ~  genotipo*tratamento,  adjust='Tukey'))
(lsm_mdl_graph<-lsmeans(modelo_2, ~  tratamento,  adjust='Tukey'))
(lsm_mdl_graph<-lsmeans(modelo_3, ~  genotipo,  adjust='Tukey'))
lsm_genotype_year<-summary(lsm_mdl_graph)


# c(0,1000,2000,3000,4000,5000,6000,7000))
# "Weed Dry Matter  - kg DM/ha"
# scale_y_continuous(breaks=c(60,65,70,75,80,85,90,95,100))+
# c(0,5,10,15,20,30,40,50))+
# "Total Tillers Density  - Units per meter square")

######### CODE FOR GRAPH
library(ggplot2)
ggplot(lsm_genotype_year, aes(x=genotipo, y=lsmean, fill=tratamento)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.5) + scale_fill_brewer(palette = "Accent")+  #Set3    # Thinner lines 
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE),
                size=.4,    # Thinner lines
                width=.5,
                position=position_dodge(.9))+ 
  ylab("Produção de Matéria Seca de Forragem  - kg MS/ha")+ 
  #ylim(0,13000)+
  scale_y_continuous(breaks=c(2000,3000,4000,5000,6000,7000,8000,9000,10000,11000,12000, 13000,14000,15000))+
  xlab("Genótipos") +
  guides(fill=guide_legend(title="Tratamentos"))+
  theme_bw()+
  theme(axis.title=element_text(size = rel(1.5) , color="black", hjust = 0.5, vjust=0))+ 
  theme(axis.text.y = element_text(colour = "black", size="18", hjust = 0.9, vjust =0.5))+ 
  theme(axis.text.x = element_text(colour = "black", size="18",angle =0, hjust = 0.5,vjust =0.85)) +
  theme(legend.title = element_text(size = "18"))+
  theme(legend.text = element_text(size = "18"))+ 
  theme(plot.title = element_text(size = rel(3)))


########  APENAS FATOR GENOTIPO
library(ggplot2)
ggplot(lsm_genotype_year, aes(x=genotipo, y=lsmean, fill=genotipo)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.5) + scale_fill_brewer(palette = "Set3")+      # Thinner lines 
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE),
                size=.4,    # Thinner lines
                width=.5,
                position=position_dodge(.9))+ 
  ylab("Produção de Matéria Seca de Trevo Branco - kg MS/ha")+ 
  #ylim(0,13000)+
  # scale_y_continuous(breaks=c(2000,2500,3000,3500,4000))+
  xlab(" ") +
  guides(fill=guide_legend(title="Genótipos"))+
  theme_bw()+
  theme(axis.title=element_text(size = rel(1.5) , color="black", hjust = 0.5, vjust=0))+ 
  theme(axis.text.y = element_text(colour = "black", size="18", hjust = 0.9, vjust =0.5))+ 
  theme(axis.text.x = element_text(colour = "black", size="18",angle =0, hjust = 0.5,vjust =0.85)) +
  theme(legend.title = element_text(size = "18"))+
  theme(legend.text = element_text(size = "18"))+ 
  theme(plot.title = element_text(size = rel(3)))



########  APENAS FATOR TRATAMENTO
library(ggplot2)
ggplot(lsm_genotype_year, aes(x=tratamento, y=lsmean, fill=tratamento)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.5) + scale_fill_brewer(palette = "Set3")+      # Thinner lines 
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE),
                size=.4,    # Thinner lines
                width=.5,
                position=position_dodge(.9))+ 
  ylab("Notas de Tolerância ao Frio (1 a 5)")+ 
  #ylim(0,13000)+
  scale_y_continuous(breaks=c(2.5,3,3.5,4,4.5,5))+
  xlab(" ") +
  guides(fill=guide_legend(title="Tratamentos"))+
  theme_bw()+
  theme(axis.title=element_text(size = rel(1.5) , color="black", hjust = 0.5, vjust=0))+ 
  theme(axis.text.y = element_text(colour = "black", size="18", hjust = 0.9, vjust =0.5))+ 
  theme(axis.text.x = element_text(colour = "black", size="18",angle =0, hjust = 0.5,vjust =0.85)) +
  theme(legend.title = element_text(size = "18"))+
  theme(legend.text = element_text(size = "18"))+ 
  theme(plot.title = element_text(size = rel(3)))

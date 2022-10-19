
####### EUN

getwd()

setwd("/Users/cleberhenriquelopesdesouza/Desktop/Data Analyses/Silvio/")
dado_regressao<-read.csv("/Users/cleberhenriquelopesdesouza/Desktop/Data Analyses/Silvio/Silvio_eun.csv",header=TRUE, sep=",")

str(dado_regressao)
dado_regressao$bloco<-as.factor(dado_regressao$bloco)
dado_regressao$tratamento<-as.factor(dado_regressao$tratamento)


modelo_3<-aov(EUN~bloco+tratamento+bloco:tratamento+tratamento*genotipo,
              data=dado_regressao, na.action=na.omit)
summary(modelo_3)


lsm_mdl<-lsmeans(modelo_3, ~ genotipo, adjust='Tukey')
lsm_mdl<-lsmeans(modelo_3, ~ tratamento, adjust='Tukey')

lsm_genotype_year<-summary(lsm_mdl)


# c(0,1000,2000,3000,4000,5000,6000,7000))
# "Weed Dry Matter  - kg DM/ha"
# scale_y_continuous(breaks=c(60,65,70,75,80,85,90,95,100))+
# c(0,5,10,15,20,30,40,50))+
# "Total Tillers Density  - Units per meter square")



########  APENAS FATOR GENOTIPO
library(ggplot2)
ggplot(lsm_genotype_year, aes(x=tratamento, y=lsmean, fill=tratamento)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.5) + scale_fill_brewer(palette = "Set3")+      # Thinner lines 
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE),
                size=.4,    # Thinner lines
                width=.5,
                position=position_dodge(.9))+ 
  ylab("Eficiência do Uso de N - kg MS/kg N")+ 
  #ylim(0,13000)+
  scale_y_continuous(breaks=c(5,10,15,20,25))+
  xlab(" ") +
  guides(fill=guide_legend(title="Tratamentos"))+
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
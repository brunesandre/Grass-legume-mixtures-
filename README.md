# Grass-legume-mixtures-
##############################
# ANALISES
# 
##############################
# For
getwd()
setwd("C:/Users/silvi/Desktop")
dados_estacao<-read.csv("C:/Users/silvi/Desktop/dados_Silvio_ano.csv",header=TRUE)
str(dados_ano)


getwd()

setwd("/Users/cleberhenriquesouza/Desktop/Data Analyses/Silvio/")
dados_estacao<-read.csv("/Users/cleberhenriquesouza/Desktop/Data Analyses/Silvio/Dados_Silvio_by_ESTACAO.csv",header=TRUE)
dados_estacao$bloco<-as.factor(dados_estacao$bloco)

dados_legu<-subset(dados_ano, tratamento=="leguminosa")
str(dados_estacao)

levels(dados_estacao$tratamento)
dados_estacao$tratamento <- factor(dados_estacao$tratamento, levels = c('0', '60', '120', '240', '480', 'leguminosa'))
levels(dados_estacao$tratamento)

levels(dados_estacao$estacao)
dados_estacao$estacao <- factor(dados_estacao$estacao, levels = c('inverno', 'primavera', 'verao','outono'))
levels(dados_estacao$estacao)


hist(dados_estacao$ms_pasp)

boxplot(ms_pasp~tratamento, data=dados_estacao)
boxplot(ms_pasp~genotipo, data=dados_estacao)

Y<-dados_estacao$ms_forragem_total


modelo_2<-aov(Y~bloco+bloco:tratamento+estacao*tratamento*genotipo, 
              data=dados_estacao, na.action=na.omit)
summary(modelo_2) 



rstd<-residuals(modelo_2,type="response")  # Simple residuals
fitvalue<-fitted(modelo_2)
diagnostics(rstd,fitvalue)

install.packages("agricolae")
library("agricolae")

(tukey_genotipos<-(HSD.test(modelo_2,"genotipo", group=TRUE,alpha = 0.05))$groups)

(tukey_tratamentos<-(HSD.test(modelo_2,"tratamento", group=TRUE,alpha = 0.05))$groups)

install.packages("lsmeans")
library(lsmeans)

lsmeans(modelo_2, list(pairwise ~ tratamento*genotipo))
lsm_mdl<-summary(lsmeans(modelo_2, ~ tratamento*genotipo))
lsm_mdl<-summary(lsmeans(modelo_2, ~ estacao*genotipo))
lsm_mdl<-summary(lsmeans(modelo_2, ~ estacao*tratamento))

levels(lsm_mdl$estacao)
lsm_mdl$estacao <- factor(lsm_mdl$estacao, levels = c('inverno', 'primavera', 'verao','outono'))
levels(lsm_mdl$estacao)

########### GRAPH
### The same order used for forage graphs
# lsm_mdl_2$genotype<- factor(lsm_mdl_2$genotype, levels = 
#                               c("C15","C18","336","A16","V4","BAGUAL","437",
#                                 "10036","712","225","D3","PENSACOLA"))
# levels(lsm_mdl_2$genotype)

# =c(0,50,100,150,200,250,300,350,400,450,500))+
library(ggplot2)
ggplot(lsm_mdl, aes(x=tratamento, y=lsmean, fill=genotipo)) + 
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.5) + scale_fill_brewer(palette = "Set3")+      # Thinner lines 
  geom_errorbar(aes(ymin=lsmean-SE, ymax=lsmean+SE),
                size=.4,    # Thinner lines
                width=.5,
                position=position_dodge(.9))+ 
  #ylab("Seeds number per inflorescence \n (units/head)")+ 
  #ylim(0,13000)+
  #scale_y_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))+
 # xlab("Tratamentos") +
  #guides(fill=guide_legend(title="GenÃ³tipos"))+
  theme_bw()+
  theme(axis.title=element_text(size = rel(1.5) , color="black", hjust = 0.5, vjust=1))+ 
  theme(axis.text.y = element_text(colour = "black", size="12", hjust = 0.9))+ 
  theme(axis.text.x = element_text(colour = "black", size="10",angle =0, hjust = 0.5,vjust =0.85)) +
  theme(legend.title = element_text(size = "12"))+
  theme(legend.text = element_text(size = "12"))+ 
  theme(plot.title = element_text(size = rel(3)))
#theme(axis.title.x = element_blank())+
#theme(axis.title.x = element_text(margin=margin(1,1,1,1)))

#Polinomail regresions#
dados<-read.xlsx("C:/Users/andre/OneDrive/Área de Trabalho/silvio.xlsx", #não esquecer de trocar o sentido da barra e colocar o caminho e o nome do teu arquivo 
                 sheetName = "dados",header = T) #sheet name é o nome da aba do excel
attach(dados) #fixar os dados
names(dados)
#alterar tratamento e repetição para fatores
dados <- dados %>% mutate(
  genotipo = factor(genotipo), bloco=factor(bloco))
PS<-aov(pasp~tratamento*genotipo+Error(1/bloco),data=dados)
summary(PS)

int<-data.frame(gen=c("B26", "B43", "Bagual", "C22", "C9", "Pensacola"), leg=c(5293.8, 5994.4, 6264.7, 4538.5, 3656.2, 1633.7))
my_y_title <- expression(paste("Accumulated annual DM (kg/ha) of ", italic(" P. notatum")))

ggplot(data = dados, aes(x = tratamento, y = pasp, grp.label = genotipo)) +
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 2,
    shape = 20,
    fill = "red"
  )+
  coord_cartesian(ylim = c(0, 16000))+
  facet_wrap(~genotipo, scales = "free_x")+
  stat_regline_equation(data=subset(dados,genotipo=="B26"), size=3.5,label.y = 13500,formula = y~x)+
  stat_cor(data = subset(dados,genotipo=="B26"), size=3.5,label.y = 15500, )+
  stat_poly_line(data = subset(dados,genotipo=="B26"),formula = y~x) +
  geom_hline(aes(yintercept=5293.8), data=subset(dados,genotipo=="B26"),
             colour="#990000", linetype="dashed")+ 
  stat_regline_equation(data=subset(dados,genotipo=="B43"), size=3.5,label.y = 13500,formula = y~poly(x,2))+
  stat_cor(data = subset(dados,genotipo=="B43"), size=3.5,label.y = 15500)+
  stat_poly_line(data = subset(dados,genotipo=="B43"),formula = y~poly(x,2)) +
  geom_hline(aes(yintercept=5994.4), data=subset(dados,genotipo=="B43"),
             colour="#990000", linetype="dashed")+ 
  stat_regline_equation(data=subset(dados,genotipo=="Bagual"), size=3.5,label.y = 13500,formula = y~poly(x,2))+
  stat_cor(data = subset(dados,genotipo=="Bagual"), size=3.5,label.y = 15500)+
  stat_poly_line(data = subset(dados,genotipo=="Bagual"),formula = y~poly(x,2)) +
  geom_hline(aes(yintercept=6264.7), data=subset(dados,genotipo=="Bagual"),
             colour="#990000", linetype="dashed")+ 
  stat_regline_equation(data=subset(dados,genotipo=="C22"), size=3.5,label.y = 13500,formula = y~poly(x,2))+
  stat_cor(data = subset(dados,genotipo=="C22"), size=3.5,label.y = 15500)+
  stat_poly_line(data = subset(dados,genotipo=="C22"),formula = y~poly(x,2)) +
  geom_hline(aes(yintercept=4538.5), data=subset(dados,genotipo=="C22"),
             colour="#990000", linetype="dashed")+
  stat_regline_equation(data=subset(dados,genotipo=="C9"), size=3.5,label.y = 13500,formula = y~poly(x,2))+
  stat_cor(data = subset(dados,genotipo=="C9"), size=3.5,label.y = 15500)+
  stat_poly_line(data = subset(dados,genotipo=="C9"),formula = y~poly(x,2)) +
  geom_hline(aes(yintercept=3656.2), data=subset(dados,genotipo=="C9"),
             colour="#990000", linetype="dashed")+
  stat_regline_equation(data=subset(dados,genotipo=="Pensacola"), size=3.5,label.y = 13500,formula = y~poly(x,2))+
  stat_cor(data = subset(dados,genotipo=="Pensacola"), size=3.5,label.y = 15500)+
  stat_poly_line(data = subset(dados,genotipo=="Pensacola"),formula = y~poly(x,2)) +
  geom_hline(aes(yintercept=1633.7), data=subset(dados,genotipo=="Pensacola"),
             colour="#990000", linetype="dashed")+
    theme(legend.position = "bottom")+
    labs(x = "N fertilization rates (kg N/ha)", y = my_y_title, parse=TRUE)+
  theme(
    # Remove panel border
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Add axis line
    axis.line = element_line(colour = "grey"))
ggsave("DM Paspalum.JPEG", plot = last_plot(), device = NULL, path = "C:/Users/andre/OneDrive/Área de Trabalho",
       scale = 1, width = 30, height = 20, units = "cm")

###########################################################
################## Diversification rates ##################
###########################################################

#####Libraries
library("ggplot2")
library("wesanderson")
library("RevGadgets")
library("ggplot2")
library("ggtree")
library("devtools")
library("ape")
library(dplyr)




##Colors 
### Colors
Frugicols<-c("#f4a3a6","#7ac192","#8d2f3a","#264653")
Carocols<- c("#a6c958","#f4ac85", "#396742","#c16556")
Mussecols<-c("#a7cee3","#ffdf66","#d88b9a","#02a567","#1d79b5","#e6194f","#cdcccc","#f28c28")
Transcols<-c("#EC6666", "#ECEC66", "#ECA966", "#CD3E8E", "#CD3E46","#CD7D3E","#9B941F", "#8ECD3E", "#46CD3E","#471B8A")
CIDcols<-c("#9787AB","#1D0735", "#BD869B","#7A0C37", "#80A6A8","#004C51")



setwd("/Users/veronicari/Documentos/GitHub//PasserDiversification/Figures")
setwd("/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory/Output") 
source('~/Documentos/GitHub/PasserDiversification/Figures/multiplot.R')


###################################################
#################### Frugivory ####################
###################################################



###################### BiSSE ######################

output.sse<-read.table("/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory/Output/BiSSE_frug50ready.log", header=TRUE)
sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2),Type=rep(c("No Frugivorous","Frugivorous"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("No Frugivorous","Frugivorous"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2),Type=rep(c("No Frugivorous","Frugivorous"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2),Type=rep(c("No Frugivorous","Frugivorous"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21),Type=rep(c("q01","q10"),each=length(output.sse$rate_1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Frugicols[1],Frugicols[2]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Frugicols[1],Frugicols[2]))

p3.0<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Frugicols[1],Frugicols[2]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Frugicols[1],Frugicols[2]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(CIDcols[1:2]))
multiplot(p1,p3.0,p5,p2,p4, cols=2)
plot(p3.0)


###################### CID Frugivory ######################

output.sse<-read.table("/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory/Output/CID_HiSSE_Frug50Ready.log", header=TRUE)
# States 1=0A, 2=1A, 3=0B, 4=1B
# For CID-2 the assumption is that 0A=1A and 0B=1B  which means that 1=2 and 3=4

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.3),Type=rep(c("A","B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.3),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21),Type=rep(c("q_01","q_10"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(CIDcols[1:2]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(CIDcols[1:2]))

p3.1<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(CIDcols[1:2]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(CIDcols[1:2]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Transcols[1],Transcols[2]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Transcols[3],Transcols[4]))


multiplot(p1,p3.1,p5,p2,p4,p6, cols=2)


###################### HiSSE Frugivory ######################

output.sse<-read.table("/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory/Output/HiSSEReady_frug50.log", header=TRUE)
str(output.sse)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("No Frugivorous A","Frugivorous A", "No Frugivorous B", "Frugivorous B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("No Frugivorous A","Frugivorous A", "No Frugivorous B", "Frugivorous B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("No Frugivorous A","Frugivorous A", "No Frugivorous B", "Frugivorous B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("No Frugivorous A","Frugivorous A", "No Frugivorous B", "Frugivorous B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21) ,Type=rep(c("q_01", "q_10"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))



p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Frugicols[1],Frugicols[3],Frugicols[2],Frugicols[4]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(Frugicols[1],Frugicols[3],Frugicols[2],Frugicols[4]))

p3.2<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Frugicols[1],Frugicols[3],Frugicols[2],Frugicols[4]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Frugicols[1],Frugicols[3],Frugicols[2],Frugicols[4]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Transcols[1],Transcols[2]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(CIDcols[1:2]))

multiplot(p1,p3.2,p5,p2,p4,p6, cols=2)





###################################################
#################### Carotenoids ####################
###################################################



###################### BiSSE ######################

output.sse<-read.table("/Users/veronicari/Documentos/GitHub/PasserDiversification/Carotenoids/Output/BiSSE_passercar.log", header=TRUE)
sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2),Type=rep(c("Absence Carotenoids","Presence Carotenoids"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("Absence Carotenoids","Presence Carotenoids"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2),Type=rep(c("Absence Carotenoids","Presence Carotenoids"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2),Type=rep(c("Absence carotenoids","Presence Carotenoids"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21),Type=rep(c("q01","q10"),each=length(output.sse$rate_1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Carocols[1],Carocols[2]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(Carocols[1],Carocols[2]))

p3.3<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Carocols[1],Carocols[2]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Carocols[1],Carocols[2]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Transcols[4],Transcols[1]))
multiplot(p1,p3.3,p5,p2,p4, cols=2)

plot(p3.3)

###################### CID Carotenoids ######################

output.sse<-read.table("/Users/veronicari/Documentos/GitHub/PasserDiversification/Carotenoids/Output/CID_HiSSE_carReady.log", header=TRUE)
# States 1=0A, 2=1A, 3=0B, 4=1B
# For CID-2 the assumption is that 0A=1A and 0B=1B  which means that 1=2 and 3=4

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.3),Type=rep(c("A","B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.3),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.3-output.sse$extinction.3),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21),Type=rep(c("q_01","q_10"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual(values = c(CIDcols[3],CIDcols[4]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(CIDcols[3],CIDcols[4]))

p3.4<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(CIDcols[3],CIDcols[4]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(CIDcols[3],CIDcols[4]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Transcols[3],Transcols[4]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Transcols[5],Transcols[6]))


multiplot(p1,p3.4,p5,p2,p4,p6, cols=2)


###################### HiSSE Carotenoids ######################

output.sse<-read.table("/Users/veronicari/Documentos/GitHub/PasserDiversification/Carotenoids/Output/HiSSE_carotenoidsReady.log", header=TRUE)
str(output.sse)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("Absence Carotenoids A","Presence Carotenoids A", "Absence Carotenoids B", "Presence Carotenoids B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2,output.sse$speciation.3,output.sse$speciation.4),Type=rep(c("Absence Carotenoids A","Presence Carotenoids A", "Absence Carotenoids B", "Presence Carotenoids B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2, output.sse$speciation.3-output.sse$extinction.3,output.sse$speciation.4-output.sse$extinction.4),Type=rep(c("Absence Carotenoids A","Presence Carotenoids A", "Absence Carotenoids B", "Presence Carotenoids B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2,output.sse$extinction.3/output.sse$speciation.3,output.sse$extinction.3/output.sse$speciation.3),Type=rep(c("Absence Carotenoids A","Presence Carotenoids A", "Absence Carotenoids B", "Presence Carotenoids B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21) ,Type=rep(c("q_01", "q_10"),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Carocols[1],Carocols[3],Carocols[2],Carocols[4]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(Carocols[1],Carocols[3],Carocols[2],Carocols[4]))

p3.5<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Carocols[1],Carocols[3],Carocols[2],Carocols[4]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Carocols[1],Carocols[3],Carocols[2],Carocols[4]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Carocols[1],Carocols[2]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(CIDcols[3:4]))

multiplot(p1,p3.5,p5,p2,p4,p6, cols=2)




###################################################
############# Frugivory & Carotenoids #############
###################################################



###################### MuSSE ######################
output.sse<-read.table("/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory_Carotenoids/Output/MuSSE_dietncarfrug50.log", header=TRUE)
# States 1=F0 C0; 2=F0 C1; 3= F1 C1 A; 4= F1 C0

str(output.sse)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1.,output.sse$extinction.2.,output.sse$extinction.3,output.sse$extinction.4),Type=rep(c("F0 C0 ","F0 C1", "F1 C1", "F1 C0"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1.,output.sse$speciation.2.,output.sse$speciation.3.,output.sse$speciation.4.),Type=rep(c("F0 C0 ","F0 C1", "F1 C1", "F1 C0"),each=length(output.sse$speciation.1.)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1.-output.sse$extinction.1.,output.sse$speciation.2.-output.sse$extinction.2., output.sse$speciation.3.-output.sse$extinction.3.,output.sse$speciation.4.-output.sse$extinction.4.),Type=rep(c("F0 C0 ","F0 C1", "F1 C1", "F1 C0"),each=length(output.sse$speciation.1.)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1./output.sse$speciation.1.,output.sse$extinction.2./output.sse$speciation.2.,output.sse$extinction.3./output.sse$speciation.3.,output.sse$extinction.4./output.sse$speciation.4.),Type=rep(c("F0 C0 ","F0 C1", "F1 C1", "F1 C0"),each=length(output.sse$speciation.1.)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21,output.sse$rate_14,output.sse$rate_41,output.sse$rate_23,output.sse$rate_32,output.sse$rate_34,output.sse$rate_43) ,Type=rep(c("F0C0 to F0C1","F0C1 to F0C0", "F0C0 to F1C0", "F1C0 to F0C0", "F0C1 to F1C1", "F1C1 to F0C1", "F1C1 to F1C0", "F1C0 to F1C1" ),each=length(output.sse$rate_12)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Mussecols[1:4])) 

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Mussecols[1:4]))

p3.6<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Mussecols[1:4]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Mussecols[1:4]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.key.size = unit(0.4, 'cm'),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(Transcols[3:10]))

multiplot(p1,p3.6,p5,p2,p4, cols=2) 





############### CID MuHiSSE ###############

output.sse<-read.table("/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory_Carotenoids/Output/cidMuHiSSEReady.log", header=TRUE)
str(output.sse)
# States 1=F0 C0 A; 2=F0 C1 A; 3= F1 C1 A; 4= F1 C0 A;
#        5=F0 C0 B; 6=F0 C1 B; 7= F1 C1 B; 8= F1 C0 B. 

sse.extinction<-data.frame(dens=c(output.sse$extinction.1, output.sse$extinction.5),Type=rep(c("A","B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.5),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.5-output.sse$extinction.5),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.5/output.sse$speciation.5),Type=rep(c("A","B"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21,output.sse$rate_14,output.sse$rate_41,output.sse$rate_23,output.sse$rate_32,output.sse$rate_34,output.sse$rate_43) ,Type=rep(c("F0C0 to F1C0","F0C1 to F0C0", "F0C0 to F1C0", "F1C0 to F0C0", "F0C1 to F1C1", "F1C1 to F0C1", "F1C1 to F1C0", "F1C0 to F1C1" ),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))

p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(CIDcols[5],CIDcols[6]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(CIDcols[5],CIDcols[6]))

p3.7<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(CIDcols[5],CIDcols[6]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(CIDcols[5],CIDcols[6]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.key.size = unit(0.4, 'cm'),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Transcols[2:9]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Transcols[2],Transcols[3]))
plot(p6)

multiplot(p1,p3.7,p5,p2,p4,p6, cols=2)


############### MuHiSSE ###############
output.sse<-read.table("~/Downloads/MuHiSSE_dietncar2Ready.log", header=TRUE)
str(output.sse)
# States 1=F0 C0 A; 2=F0 C1 A; 3= F1 C1 A; 4= F1 C0 A;
#        5=F0 C0 B; 6=F0 C1 B; 7= F1 C1 B; 8= F1 C0 B. 

# States 1=F0 C0; 2=F0 C1; 3= F1 C1 A; 4= F1 C0

str(output.sse)

sse.extinction<-data.frame(dens=c(output.sse$extinction.1.,output.sse$extinction.2.,output.sse$extinction.3.,output.sse$extinction.4.,output.sse$extinction.5.,output.sse$extinction.6.,output.sse$extinction.7.,output.sse$extinction.8.),Type=rep(c("F0C0 A", "F0C1 A", "F1C1 A", "F1C0 A", "F0C0 B", "F0C1 B", "F1C1 B", "F1C0 B"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1.,output.sse$speciation.2.,output.sse$speciation.3.,output.sse$speciation.4.,output.sse$speciation.5.,output.sse$speciation.6.,output.sse$speciation.7.,output.sse$speciation.8.),Type=rep(c("F0C0 A", "F0C1 A", "F1C1 A", "F1C0 A", "F0C0 B", "F0C1 B", "F1C1 B", "F1C0 B"),each=length(output.sse$speciation.1.)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1.-output.sse$extinction.1.,output.sse$speciation.2.-output.sse$extinction.2., output.sse$speciation.3.-output.sse$extinction.3.,output.sse$speciation.4.-output.sse$extinction.4.,output.sse$speciation.5.-output.sse$extinction.5.,output.sse$speciation.6.-output.sse$extinction.6., output.sse$speciation.7.-output.sse$extinction.7.,output.sse$speciation.8.-output.sse$extinction.8.),Type=rep(c("F0C0 A", "F0C1 A", "F1C1 A", "F1C0 A", "F0C0 B", "F0C1 B", "F1C1 B", "F1C0 B"),each=length(output.sse$speciation.1.)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1./output.sse$speciation.1.,output.sse$extinction.2./output.sse$speciation.2.,output.sse$extinction.3./output.sse$speciation.3.,output.sse$extinction.4./output.sse$speciation.4.,output.sse$extinction.5./output.sse$speciation.5.,output.sse$extinction.6./output.sse$speciation.6.,output.sse$extinction.7./output.sse$speciation.7.,output.sse$extinction.8./output.sse$speciation.8.),Type=rep(c("F0C0 A", "F0C1 A", "F1C1 A", "F1C0 A", "F0C0 B", "F0C1 B", "F1C1 B", "F1C0 B"),each=length(output.sse$speciation.1.)))

trait.rates<-data.frame(dens=c(output.sse$rate_12, output.sse$rate_21, output.sse$rate_14, output.sse$rate_41, output.sse$rate_23, output.sse$rate_32, output.sse$rate_34, output.sse$rate_43), Type=rep(c("F0C0 to F0C1","F0C1 to F0C0", "F0C0 to F1C0", "F1C0 to F0C0", "F0C1 to F1C1", "F1C1 to F0C1", "F1C1 to F1C0", "F1C0 to F1C1" ),each=length(output.sse$rate_12)))

hidden.rate<-data.frame(dens=c(output.sse$hidden_rate1, output.sse$hidden_rate2) ,Type=rep(c("alpha","beta"),each=length(output.sse$hidden_rate1)))


###Violin Plots

netdiv_vioplot<- ggplot(sse.netdiv, aes(x=Type, y=dens,fill=Type)) + 
  geom_violin(trim=FALSE)+
  labs(title="Net Diversification",x="State", y = "Posterior Density")

netdiv_vioplot<-netdiv_vioplot + scale_fill_manual(values=c(Mussecols[1],Mussecols[3],Mussecols[4],Mussecols[2],Mussecols[5],Mussecols[6],Mussecols[7],Mussecols[8])) + theme_classic()

netdiv_vioplot

traitrates_vioplot<- ggplot(trait.rates, aes(x=Type, y=dens, fill=Type))+
  geom_violin(trim=FALSE)+
  labs(title="Transition Rates", x="Rates", y="Posterior Density")

traitrates_vioplot<-traitrates_vioplot + scale_fill_manual(values=Transcols) + theme_classic()
traitrates_vioplot
###



p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.key.size = unit(0.4, 'cm'),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Mussecols[1],Mussecols[5],Mussecols[2],Mussecols[8],Mussecols[3],Mussecols[6],Mussecols[4],Mussecols[7]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.key.size = unit(0.4, 'cm'),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Mussecols[1],Mussecols[5],Mussecols[2],Mussecols[8],Mussecols[3],Mussecols[6],Mussecols[4],Mussecols[7]))

p3.8<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.key.size = unit(0.4, 'cm'),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Mussecols[1],Mussecols[5],Mussecols[2],Mussecols[8],Mussecols[3],Mussecols[6],Mussecols[4],Mussecols[7]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.key.size = unit(0.4, 'cm'),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Mussecols[1],Mussecols[5],Mussecols[2],Mussecols[8],Mussecols[3],Mussecols[6],Mussecols[4],Mussecols[7]))

p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.key.size = unit(0.4, 'cm'),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Transcols[1:8]))

p6<-ggplot(hidden.rate, aes(x=dens, fill=Type))+labs(title="Hidden trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.key.size = unit(0.4, 'cm'),
                                                                                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Transcols[2],Transcols[3]))

multiplot(p1,p3.8,p5,p2,p4,p6, cols=2)



multiplot(p3.6,p3.8, cols=2)


#####Diversification figure
multiplot(p3.0,p3.1,p3.2,p3.3,p3.4,p3.5,p3.6,p3.7,p3.8, cols=3)


##################################################################
######################## Ancestral States ########################
##################################################################
library("ggplot2")
library("RevGadgets")
library("wesanderson")
library("ggtree")
library("treeio")

setwd("//Users/veronicari/Desktop/AncState")


##Colors 
### Colors
Frugicols<-c("#9a82c3","#b0d889","#3A3149","#2C3622")
Carocols<- c("#00ACBB","#CE003B", "#002B2F","#34000F")
Mussecols<-c("gold","violetred3","slateblue1", "aquamarine3","#C2AD50", "#773D5C", "#4B465B", "#284D40")
Transcols<-c("#EC6666", "#ECEC66", "#ECA966", "#CD3E8E", "#CD3E46","#CD7D3E","#9B941F", "#8ECD3E", "#46CD3E","#471B8A")
CIDcols<-c("#9787AB","#1D0735", "#BD869B","#7A0C37", "#80A6A8","#004C51")


###################################################
#################### Carotenoids ####################
###################################################



###################### BiSSE ######################


a<-processAncStates("anc_states_BiSSE_SummaryCar.tree", state_labels = c("0" = "No carotenoids", "1" = "Carotenoids"))


plotAncStatesMAP(t = a, tree_layout = "circular",
                 tip_labels = F, tip_labels_italics = F, node_labels_as = "state_posterior",
                 node_labels_size = 0.01, tip_labels_states_size = 1,
                 state_transparency = 0.50, 
                 node_size_as = "state_posterior", node_size = c(.05, 1.5),
                 node_color=c(Carocols[2],Carocols[1]))



###################### CID ######################
###States 1=0A, 2=1A, 3=0B, 4=1B

b<-processAncStates(path ="anc_states_CID_SummaryCar.tree", c("0" = "Without carotenoids A", "1" = "Carotenoids A", "2" = "Without carotenoids B", "3" = "Carotenoids B"))


plotAncStatesMAP(t = b, tree_layout = "circular",
                 tip_labels = F, tip_labels_italics = F, node_labels_as = "state_posterior",
                 node_labels_size = 0.01, tip_labels_states_size = 1,
                 state_transparency = 0.4, 
                 node_size_as = "state_posterior", node_size = c(.05, 2),
                 node_color=c(Carocols[2],Carocols[4],Carocols[1],Carocols[3]))


###################### HiSSE Carotenoids ######################
c<-processAncStates(path ="anc_states_HiSSE_SummaryCar.tree", c("0" = "Without carotenoids A", "1" = "Carotenoids A", "2" = "Without carotenoids B", "3" = "Carotenoids B"))


plotAncStatesMAP(t = c, tree_layout = "circular",
                 tip_labels = F, tip_labels_italics = F, node_labels_as = "state_posterior",
                 node_labels_size = 0.01, tip_labels_states_size = 1,
                 state_transparency = 0.50, 
                 node_size_as = "state_posterior", node_size = c(.05, 1.5),
                 node_color=c(Carocols[2],Carocols[4],Carocols[1],Carocols[3]))


###################################################
#################### Frugivory ####################
###################################################



###################### BiSSE ######################
d<-processAncStates(path ="anc_states_BiSSE_SummaryFrug.tree", c("0" = "Non-frugivorous", "1" = "Frugivorous"))

plotAncStatesMAP(t = d, tree_layout = "circular",
                 tip_labels = F, tip_labels_italics = F, node_labels_as = "state_posterior",
                 node_labels_size = 0.01, tip_labels_states_size = 1,
                 state_transparency = 0.50, 
                 node_size_as = "state_posterior", node_size = c(.05, 1.5),
                 node_color=c(Frugicols[1],Frugicols[2]))


###################### CID Frugivory ######################
e<-processAncStates(path ="anc_states_CID_SummaryFrugi.tree", c("0" = "Non-frugivorous A", "1" = "Frugivorous A", "2" = "Non-frugivorous B", "3" = "Frugivorous B"))


plotAncStatesMAP(t = e, tree_layout = "circular",
                 tip_labels = F, tip_labels_italics = F, node_labels_as = "state_posterior",
                 node_labels_size = 0.01, tip_labels_states_size = 1,
                 state_transparency = 0.55, 
                 node_size_as = "state_posterior", node_size = c(.05, 2),
                 node_color=c(Frugicols[1],Frugicols[3],Frugicols[2],Frugicols[4]))


###################### HiSSE Frugivory ######################
f<-processAncStates(path ="anc_states_HiSSE_Summaryfrug.tree", c("0" = "Non-frugivorous A", "1" = "Frugivorous A", "2" = "Non-frugivorous B", "3" = "Frugivorous B"))


plotAncStatesMAP(t = f, tree_layout = "circular",
                 tip_labels = F, tip_labels_italics = F, node_labels_as = "state_posterior",
                 node_labels_size = 0.01, tip_labels_states_size = 1,
                 state_transparency = 0.50, 
                 node_size_as = "state_posterior", node_size = c(.05, 1.5),
                 node_color=c(Frugicols[1],Frugicols[3],Frugicols[2],Frugicols[4]))



###################################################
############# Frugivory & Carotenoids #############
###################################################



###################### MuSSE ######################


g<-processAncStates(path ="anc_states_SummaryMuSSE.tree", c("0" = "F0 C0", "1" = "F0 C1", "2" = "F1 C1", "3" = "F1 C0"))
# States 1=F0 C0; 2=F0 C1; 3= F1 C1 A; 4= F1 C0

plotAncStatesMAP(t = g, tree_layout = "circular",
                 tip_labels = F, tip_labels_italics = F, node_labels_as = "state_posterior",
                 node_labels_size = 0.01, tip_labels_states_size = 1,
                 state_transparency = 0.50, 
                 node_size_as = "state_posterior", node_size = c(.05, 1.5),
                 node_color=c(Mussecols[1:4]))

############### CID MuHiSSE ###############
h<-processAncStates(path ="anc_states_CID_SummaryMuHiSSE.tree", c("0" = "F0 C0 A", "1" = "F0 C1 A", "2" = "F1 C1 A", "3" = "F1 C0 A", "4" = "F0 C0 B", "5" = "F0 C1 B", "6" = "F1 C1 B", "7" = "F1 C0 B"))

plotAncStatesMAP(t = h, tree_layout = "circular",
                 tip_labels = F, tip_labels_italics = F, node_labels_as = "state_posterior",
                 node_labels_size = 0.01, tip_labels_states_size = 1,
                 state_transparency = 0.50, 
                 node_size_as = "state_posterior", node_size = c(.05, 2),
                 node_color=c(Mussecols[1],Mussecols[5],Mussecols[2],Mussecols[6],Mussecols[3],Mussecols[7],Mussecols[4],Mussecols[8]))



############### MuHiSSE ###############
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("treeio")

library(RevGadgets)
anc<-"anc_states_MuHiSSE_Summary.tree"
i<-processAncStates(anc, state_labels=c("0" = "F0 C0 A", "1" = "F0 C1 A", "2" = "F1 C1 A", "3" = "F1 C0 A", "4" = "F0 C0 B", "5" = "F0 C1 B", "6" = "F1 C1 B", "7" = "F1 C0 B"))

plotAncStatesMAP(t = i, tree_layout = "circular",
                 tip_labels = F, tip_labels_italics = F, node_labels_as = "state_posterior",
                 node_labels_size = 0.01, tip_labels_states_size = 1,
                 state_transparency = 0.50, 
                 node_size_as = "state_posterior", node_size = c(.05, 2),
                 node_color=c(Mussecols[1],Mussecols[5],Mussecols[2],Mussecols[8],Mussecols[3],Mussecols[6],Mussecols[4],Mussecols[7]))




fiel<-system.file("anc_states_MuHiSSE_dietncarReady.log", "anc_states_MuHiSSE_Summary.tree", package="RevGadgets")
example <- processAncStates(file, state_labels = c("0" = "F0 C0 A", "1" = "F0 C1 A", "2" = "F1 C1 A", "3" = "F1 C0 A", "4" = "F0 C0 B", "5" = "F0 C1 B", "6" = "F1 C1 B", "7" = "F1 C0 B"))

###################################
########### BayesTraits ###########
###################################

library(ggplot2)

data<-read.csv("/Users/veronicari/Documentos/GitHub/PasserDiversification/DependienteYa.csv", header=TRUE)
str(data)
summary(data)
hist(data$F0C0)
hist(data$F0C1)
hist(data$F1C0)
hist(data$F1C1)

data245<-subset(data, Tree.No<=245)
summary(data245)
View(data245)

root1<-data.frame(dens=c(data245$F0C0,data245$F0C1,data245$F1C0,data245$F1C1) ,Type=rep(c("F0C0","F0C1", "F1C0", "F1C1"),each=length(data245$F1C1)))


p5<-ggplot(root1, aes(x=dens, fill=Type))+labs(title="Ancestral State",x="Posterior probability at the root", y="Frequency")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.key.size = unit(0.9, 'cm'),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(Mussecols[1:4]))
p5


marginal<-read.csv("/Users/veronicari/Documentos/GitHub/PasserDiversification/comparacion_modelos.csv", header=TRUE)
str(marginal)
summary(marginal)

# Make the histogram
marginal %>%
  ggplot(aes(x=LogBF, xmin=2, xmax=55, xlab="Log Bayes Factors")) +
  geom_density(fill=c("#69b3a2"), color="#e9ecef", alpha=0.8) +
  ggtitle("Log Bayes Factors") + theme_ipsum(base_family = "avenir", base_size = 13) +
  theme(panel.grid.major = element_line(size = 0.4, color = "#f8f3ea")) +
  theme(panel.grid.minor = element_line(size = 0.4, color = "#f8f3ea")) 



#Delhey

marginal2<-read.csv("/Users/veronicari/Documentos/GitHub/PasserDiversification/DelheyComparation.csv", header=TRUE)
str(marginal2)

# Make the histogram

library(hrbrthemes)
marginal2 %>%
  ggplot(aes(x=LogBF, xmin=2, xmax=55, xlab="Log Bayes Factors")) +
  geom_density(fill=c("#f6a6bb"), color="#e9ecef", alpha=0.8) +
  ggtitle("Log Bayes Factors") + theme_ipsum(base_family = "avenir", base_size = 13) +
  theme(panel.grid.major = element_line(size = 0.4, color = "#f8f3ea")) +
  theme(panel.grid.minor = element_line(size = 0.4, color = "#f8f3ea")) 




temporary_plot <- marginal %>%  
  ggplot() +
  geom_density(aes(x=LogBF))
build <- ggplot2::ggplot_build(temporary_plot)

df_breaks <- build$data[[1]] %>% 
  mutate(status = case_when(x >=10 ~ 'Very Strong evidence'))

df_breaks %>% 
  ggplot() +
  geom_area(
    aes(x = x,
        y = y,
        fill = status) + theme_ipsum())


c("#69b3a2","#F7d1d8","#f6a6bb")

### Density estimation
den <- density(marginal$Comparison, main=NULL)

# Plot
plot(den)

# Fill area for values greater or equal to 1
value1 <- 2
value2 <- 5
value3 <- 10

# Lower and higher indices on the X-axis
l <- min(which(den$x >= value1))
h <- max(which(den$x < value2))
j <- min(which(den$x >= value3))

lines(den, col="#f8f3ea")
polygon(c(den$x[c(l, l:h, h)]),
        c(0, den$y[l:h], 0),
        col = ("#Fae6e7"), lines(den, col="#f8f3ea"), border=NULL,lwd=0)


polygon(c(den$x[c(h, h:j, j)]),
        c(0, den$y[h:j], 0),
        col = ("#F7d1d8"), border=NULL, lwd=0)
lines(b, col="#f8f3ea")


polygon(c(den$x[den$x > value3], value3),
        c(den$y[den$x > value3 ], 0),
        col = "#f6a6bb",
        border = NULL, lwd=0)
lines(den, col="#f8f3ea")



###################################################
#################### Carotenoids ####################
###################################################



###################### BiSSE ######################

output.sse<-read.table("/Users/veronicari/Documentos/GitHub/PasserDiversification/Carotenoids/Output/BiSSE_passercar_Delhey.log", header=TRUE)
sse.extinction<-data.frame(dens=c(output.sse$extinction.1,output.sse$extinction.2),Type=rep(c("Absence Carotenoids","Presence Carotenoids"),each=length(output.sse$extinction.1)))

sse.speciation<-data.frame(dens=c(output.sse$speciation.1,output.sse$speciation.2),Type=rep(c("Absence Carotenoids","Presence Carotenoids"),each=length(output.sse$speciation.1)))

sse.netdiv<-data.frame(dens=c(output.sse$speciation.1-output.sse$extinction.1,output.sse$speciation.2-output.sse$extinction.2),Type=rep(c("Absence Carotenoids","Presence Carotenoids"),each=length(output.sse$speciation.1)))

sse.reldiv<-data.frame(dens=c(output.sse$extinction.1/output.sse$speciation.1,output.sse$extinction.2/output.sse$speciation.2),Type=rep(c("Absence carotenoids","Presence Carotenoids"),each=length(output.sse$speciation.1)))

trait.rates<-data.frame(dens=c(output.sse$rate_12,output.sse$rate_21),Type=rep(c("q01","q10"),each=length(output.sse$rate_1)))


p1<-ggplot(sse.speciation, aes(x=dens, fill=Type))+labs(title="Speciation",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Carocols[1],Carocols[2]))

p2<-ggplot(sse.extinction, aes(x=dens, fill=Type))+labs(title="Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                          panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values =c(Carocols[1],Carocols[2]))

p3.3<-ggplot(sse.netdiv, aes(x=dens, fill=Type))+labs(title="Net Diversification",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                                 panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Carocols[1],Carocols[2]))

p4<-ggplot(sse.reldiv, aes(x=dens, fill=Type))+labs(title="Relative Extinction",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                               panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Carocols[1],Carocols[2]))


p5<-ggplot(trait.rates, aes(x=dens, fill=Type))+labs(title="Trait change",x="Rate", y="Posterior Density")+geom_density(alpha=0.5)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                                                                                                         panel.background = element_blank(), axis.line = element_line(colour = "black"))+scale_fill_manual( values = c(Transcols[2],Transcols[3]))
multiplot(p1,p3.3,p5,p2,p4, cols=2)

plot(p3.3)



###################################################
#################### Formal test ####################
###################################################

setwd("~/Documentos/GitHub/PasserDiversification/FinalTest/")
# LOAD LIBRARIES
library(ggplot2)
library(dplyr)

# =========================
# HiSSE – FRUGIVORY
# =========================
hisse_frug <- read.table("HiSSEReady_frug50.log", header = TRUE)
hisse_frug <- hisse_frug[-seq(1, 10000), ]

# T_A y T_B (observable states)
T_A_frug <- (hisse_frug$speciation.1. - hisse_frug$extinction.1.) - (hisse_frug$speciation.2. - hisse_frug$extinction.2.)
T_B_frug <- (hisse_frug$speciation.3. - hisse_frug$extinction.3.) - (hisse_frug$speciation.4. - hisse_frug$extinction.4.)

T_dif_frug <- data.frame(dens = c(T_A_frug, T_B_frug), diferencia = rep(c("T_A", "T_B"), each = length(T_A_frug)))

ggplot(T_dif_frug, aes(x = diferencia, y = dens, fill = diferencia)) +
  geom_violin(trim = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "HiSSE (frugivory): Net diversification contrast (T_A, T_B)", y = "Net diversification (lambda - mu)", x = "Contrast") +
  scale_fill_manual(values = c("T_A" = "#FF006E", "T_B" = "#FFC2DC")) +
  theme_classic()

ci_TA_TB_frug <- T_dif_frug %>% group_by(diferencia) %>% reframe(`2.5%` = quantile(dens, 0.025), `97.5%` = quantile(dens, 0.975))
print(ci_TA_TB_frug)

# T_0 y T_1 (hidden states)
net_0A_frug <- hisse_frug$speciation.1. - hisse_frug$extinction.1.
net_0B_frug <- hisse_frug$speciation.3. - hisse_frug$extinction.3.
net_1A_frug <- hisse_frug$speciation.2. - hisse_frug$extinction.2.
net_1B_frug <- hisse_frug$speciation.4. - hisse_frug$extinction.4.

T_0_frug <- net_0A_frug - net_0B_frug
T_1_frug <- net_1A_frug - net_1B_frug

T_hidden_frug <- data.frame(dens = c(T_0_frug, T_1_frug), diferencia = rep(c("T_0", "T_1"), each = length(T_0_frug)))

ggplot(T_hidden_frug, aes(x = diferencia, y = dens, fill = diferencia)) +
  geom_violin(trim = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "HiSSE (frugivory): Hidden state contrast (T_0, T_1)", y = "Net diversification (A - B)", x = "State") +
  scale_fill_manual(values = c("T_0" = "#1DB32C", "T_1" = "#BFEEC3")) +
  theme_classic()

ci_T0_T1_frug <- T_hidden_frug %>% group_by(diferencia) %>% reframe(`2.5%` = quantile(dens, 0.025), `97.5%` = quantile(dens, 0.975))
print(ci_T0_T1_frug)


# =========================
# HiSSE – CAROTENOIDS
# =========================
hisse_carot <- read.table("HiSSE_carotenoidsReady.log", header = TRUE)
hisse_carot <- hisse_carot[-seq(1, 10000), ]

T_A_carot <- (hisse_carot$speciation.1. - hisse_carot$extinction.1.) - (hisse_carot$speciation.2. - hisse_carot$extinction.2.)
T_B_carot <- (hisse_carot$speciation.3. - hisse_carot$extinction.3.) - (hisse_carot$speciation.4. - hisse_carot$extinction.4.)

T_dif_carot <- data.frame(dens = c(T_A_carot, T_B_carot), diferencia = rep(c("T_A", "T_B"), each = length(T_A_carot)))

ggplot(T_dif_carot, aes(x = diferencia, y = dens, fill = diferencia)) +
  geom_violin(trim = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "HiSSE (carotenoids): Net diversification contrast (T_A, T_B)", y = "Net diversification (lambda - mu)", x = "Contrast") +
  scale_fill_manual(values = c("T_A" = "#FF006E", "T_B" = "#FFC2DC")) +
  theme_classic()

ci_TA_TB_carot <- T_dif_carot %>% group_by(diferencia) %>% reframe(`2.5%` = quantile(dens, 0.025), `97.5%` = quantile(dens, 0.975))
print(ci_TA_TB_carot)

net_0A_carot <- hisse_carot$speciation.1. - hisse_carot$extinction.1.
net_0B_carot <- hisse_carot$speciation.3. - hisse_carot$extinction.3.
net_1A_carot <- hisse_carot$speciation.2. - hisse_carot$extinction.2.
net_1B_carot <- hisse_carot$speciation.4. - hisse_carot$extinction.4.

T_0_carot <- net_0A_carot - net_0B_carot
T_1_carot <- net_1A_carot - net_1B_carot

T_hidden_carot <- data.frame(dens = c(T_0_carot, T_1_carot), diferencia = rep(c("T_0", "T_1"), each = length(T_0_carot)))

ggplot(T_hidden_carot, aes(x = diferencia, y = dens, fill = diferencia)) +
  geom_violin(trim = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "HiSSE (carotenoids): Hidden state contrast (T_0, T_1)", y = "Net diversification (A - B)", x = "State") +
  scale_fill_manual(values = c("T_0" = "#1DB32C", "T_1" = "#BFEEC3")) +
  theme_classic()

ci_T0_T1_carot <- T_hidden_carot %>% group_by(diferencia) %>% reframe(`2.5%` = quantile(dens, 0.025), `97.5%` = quantile(dens, 0.975))
print(ci_T0_T1_carot)
# COMBINE T_A, T_B, T_0, T_1 for both HiSSE models
hisse_plot_full <- rbind(
  # CAROTENOIDS
  data.frame(dens = T_A_carot, diferencia = "T_A", modelo = "HiSSE_Carotenoids"),
  data.frame(dens = T_B_carot, diferencia = "T_B", modelo = "HiSSE_Carotenoids"),
  data.frame(dens = T_0_carot, diferencia = "T_0", modelo = "HiSSE_Carotenoids"),
  data.frame(dens = T_1_carot, diferencia = "T_1", modelo = "HiSSE_Carotenoids"),
  
  # FRUGIVORY
  data.frame(dens = T_A_frug, diferencia = "T_A", modelo = "HiSSE_Frugivory"),
  data.frame(dens = T_B_frug, diferencia = "T_B", modelo = "HiSSE_Frugivory"),
  data.frame(dens = T_0_frug, diferencia = "T_0", modelo = "HiSSE_Frugivory"),
  data.frame(dens = T_1_frug, diferencia = "T_1", modelo = "HiSSE_Frugivory")
)

# CONTRAST COLOR PALETTE
difcols <- c(
  "T_A" = "#FF006E",
  "T_B" = "#FFC2DC",
  "T_0" = "#1DB32C",
  "T_1" = "#BFEEC3"
)

# PLOT
ggplot(hisse_plot_full, aes(x = diferencia, y = dens, fill = diferencia)) +
  geom_violin(trim = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.2) +
  facet_grid(. ~ modelo)  +
  scale_fill_manual(values = difcols) +
  labs(
    title = "Net diversification contrasts across HiSSE models",
    x = NULL,
    y = expression(Delta~"net diversification rate ("*lambda - mu*")")
  ) +
  theme_classic() +
  theme(
    strip.background = element_rect(fill = "white", color = NA),
    strip.text = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 14),
    legend.position = "right"
  )

hisse_plot_full$modelo <- factor(hisse_plot_full$modelo, levels = c("HiSSE_Carotenoids", "HiSSE_Frugivory"))

 
ggplot(hisse_plot_full, aes(x = diferencia, y = dens, fill = diferencia)) +
  geom_violin(trim = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.2) +
  facet_grid(. ~ modelo) +
  scale_fill_manual(values = difcols) +
  labs(
    title = "Net diversification contrasts across HiSSE models",
    x = NULL,
    y = expression(Delta~"net diversification rate ("*lambda - mu*")")
  ) +
  theme_classic(base_size = 12) +  # <-- CAMBIA ESTO
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    strip.text = element_text(face = "bold", size = 14),
    axis.text.x = element_text(size = 12),
    legend.position = "right"
  )



# =========================
# MuHiSSE – Frug x Carot x Hidden
# =========================
muhisse <- read.table("MuHiSSE_dietncar2Ready.log", header = TRUE)
muhisse <- muhisse[-seq(1, 15000), ]

# 1. Comparisons between observables within each hidden (T_A to T_D)
T_A <- (muhisse$speciation.1. - muhisse$extinction.1.) - (muhisse$speciation.2. - muhisse$extinction.2.)
T_B <- (muhisse$speciation.3. - muhisse$extinction.3.) - (muhisse$speciation.4. - muhisse$extinction.4.)
T_C <- (muhisse$speciation.5. - muhisse$extinction.5.) - (muhisse$speciation.6. - muhisse$extinction.6.)
T_D <- (muhisse$speciation.7. - muhisse$extinction.7.) - (muhisse$speciation.8. - muhisse$extinction.8.)

# Combine
T_obs_mu <- data.frame(
  dens = c(T_A, T_B, T_C, T_D),
  diferencia = rep(c("T_A", "T_B", "T_C", "T_D"), each = length(T_A))
)

# 2. Comparisons between hidden states within each observable (T_0 to T_3)
T_0 <- (muhisse$speciation.1. - muhisse$extinction.1.) - (muhisse$speciation.5. - muhisse$extinction.5.)
T_1 <- (muhisse$speciation.2. - muhisse$extinction.2.) - (muhisse$speciation.6. - muhisse$extinction.6.)
T_2 <- (muhisse$speciation.3. - muhisse$extinction.3.) - (muhisse$speciation.7. - muhisse$extinction.7.)
T_3 <- (muhisse$speciation.4. - muhisse$extinction.4.) - (muhisse$speciation.8. - muhisse$extinction.8.)

T_mu_all <- data.frame(
  dens = c(T_A, T_B, T_C, T_D, T_0, T_1, T_2, T_3),
  diferencia = rep(c("T_A", "T_B", "T_C", "T_D", "T_0", "T_1", "T_2", "T_3"), each = length(T_A))
)

T_0 <- (muhisse$speciation.1. - muhisse$extinction.1.) - (muhisse$speciation.5. - muhisse$extinction.5.)  # F0C0_A - F0C0_B
T_1 <- (muhisse$speciation.2. - muhisse$extinction.2.) - (muhisse$speciation.6. - muhisse$extinction.6.)  # F1C0_A - F1C0_B
T_2 <- (muhisse$speciation.3. - muhisse$extinction.3.) - (muhisse$speciation.7. - muhisse$extinction.7.)  # F0C1_A - F0C1_B
T_3 <- (muhisse$speciation.4. - muhisse$extinction.4.) - (muhisse$speciation.8. - muhisse$extinction.8.)  # F1C1_A - F1C1_B

# Combine
T_hid_mu <- data.frame(
  dens = c(T_0, T_1, T_2, T_3),
  diferencia = rep(c("T_0", "T_1", "T_2", "T_3"), each = length(T_0))
)

# Violin plot for observables
ggplot(T_obs_mu, aes(x = diferencia, y = dens, fill = diferencia)) +
  geom_violin(trim = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "MuHiSSE: Net diversification contrast (T_A to T_D)",
    x = "Contrast (observables)",
    y = expression(Delta~"net diversification rate ("*lambda - mu*")")
  ) +
  scale_fill_manual(values = c("T_A" = "#FF006E", "T_B" = "#FFC2DC", "T_C" = "#C85CF4", "T_D" = "#E5B3FE")) +
  theme_classic()

# Violin plot for hidden states
ggplot(T_hid_mu, aes(x = diferencia, y = dens, fill = diferencia)) +
  geom_violin(trim = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "MuHiSSE: Hidden state contrast (T_0 to T_3)",
    x = "Contrast (hidden)",
    y = expression(Delta~"net diversification rate ("*lambda - mu*")")
  ) +
  scale_fill_manual(values = c("T_0" = "#1DB32C", "T_1" = "#BFEEC3", "T_2" = "#57CC99", "T_3" = "#D8F3DC")) +
  theme_classic()

ci_obs_mu <- T_obs_mu %>%
  group_by(diferencia) %>%
  reframe(`2.5%` = quantile(dens, 0.025), `97.5%` = quantile(dens, 0.975))

ci_hid_mu <- T_hid_mu %>%
  group_by(diferencia) %>%
  reframe(`2.5%` = quantile(dens, 0.025), `97.5%` = quantile(dens, 0.975))


# Unite all the contrasts
T_mu_plot <- data.frame(
  dens = c(T_A, T_B, T_C, T_D, T_0, T_1, T_2, T_3),
  diferencia = rep(c("T_A", "T_B", "T_C", "T_D", "T_0", "T_1", "T_2", "T_3"), each = length(T_A)),
  modelo = rep("MuHiSSE", length.out = length(T_A) * 8)
)



# Colors for the 8 contrasts
colores_mu <- c(
  "T_A" = "#FF006E", "T_B" = "#FFC2DC", "T_C" = "#C85CF4", "T_D" = "#E5B3FE",
  "T_0" = "#1DB32C", "T_1" = "#BFEEC3", "T_2" = "#57CC99", "T_3" = "#D8F3DC"
)

# Make sure the model is a factor (in case we join it with others later)
T_mu_plot$modelo <- factor(T_mu_plot$modelo)

# Plot final
ggplot(T_mu_plot, aes(x = diferencia, y = dens, fill = diferencia)) +
  geom_violin(trim = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.2) +
  facet_grid(. ~ modelo) +
  scale_fill_manual(values = colores_mu) +
  labs(
    title = "MuHiSSE: Net diversification contrasts",
    x = "Contrast",
    y = expression(Delta~"net diversification rate ("*lambda - mu*")")
  ) +
  theme_light(base_size = 13) +
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 1),
    strip.text = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 12),
    legend.position = "none"
  )


ci_mu_all <- T_mu_plot %>%
  group_by(diferencia) %>%
  reframe(
    `2.5%` = quantile(dens, 0.025),
    `97.5%` = quantile(dens, 0.975)
  )

print(ci_mu_all)

######## ALL 
###### Label model in each df
T_frug_all <- rbind(
  data.frame(dens = T_A_frug, diferencia = "T_A", modelo = "HiSSE_Frugivory"),
  data.frame(dens = T_B_frug, diferencia = "T_B", modelo = "HiSSE_Frugivory"),
  data.frame(dens = T_0_frug, diferencia = "T_0", modelo = "HiSSE_Frugivory"),
  data.frame(dens = T_1_frug, diferencia = "T_1", modelo = "HiSSE_Frugivory")
)

T_carot_all <- rbind(
  data.frame(dens = T_A_carot, diferencia = "T_A", modelo = "HiSSE_Carotenoids"),
  data.frame(dens = T_B_carot, diferencia = "T_B", modelo = "HiSSE_Carotenoids"),
  data.frame(dens = T_0_carot, diferencia = "T_0", modelo = "HiSSE_Carotenoids"),
  data.frame(dens = T_1_carot, diferencia = "T_1", modelo = "HiSSE_Carotenoids")
)

# MuHiSSE already has all contrasts packed as T_mu_plot (from previous step)

# Combine all into one master df
T_all_models <- rbind(T_frug_all, T_carot_all, T_mu_plot)


# Set order for facets
T_all_models$modelo <- factor(
  T_all_models$modelo,
  levels = c("HiSSE_Frugivory", "HiSSE_Carotenoids", "MuHiSSE")
)

# Use consistent fill color across models
colores_all <- c(
  "T_A" = "#FF006E", "T_B" = "#FFC2DC", "T_C" = "#C85CF4", "T_D" = "#E5B3FE",
  "T_0" = "#1DB32C", "T_1" = "#BFEEC3", "T_2" = "#57CC99", "T_3" = "#D8F3DC"
)

ggplot(T_all_models, aes(x = diferencia, y = dens, fill = diferencia)) +
  geom_violin(trim = FALSE, scale = "width") +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  facet_grid(modelo ~ .) +  # One horizontal panel per model
  scale_fill_manual(values = colores_all) +
  labs(
    title = "Net diversification contrasts across all models",
    x = "Contrast",
    y = expression(Delta~"net diversification rate ("*lambda - mu*")")
  ) +
  theme_classic(base_size = 16) +
  theme(
    strip.background = element_rect(fill = "white", color = "black", linewidth = 1),
    strip.text.y = element_text(face = "bold", size = 13),
    axis.text.x = element_text(size = 16),
    legend.position = "none"
  )


# Table of credibility intervals by model and contrast
tabla_contrastes <- T_all_models %>%
  group_by(modelo, diferencia) %>%
  summarise(
    mean = mean(dens),
    median = median(dens),
    `2.5%` = quantile(dens, 0.025),
    `97.5%` = quantile(dens, 0.975),
    .groups = "drop"
  )

# Display as sorted table
print(tabla_contrastes)

tabla_contrastes <- tabla_contrastes %>%
  mutate(signif = ifelse(`2.5%` < 0 & `97.5%` > 0, "Includes 0", "≠ 0"))

library(knitr)
library(kableExtra)

tabla_contrastes %>%
  arrange(modelo, diferencia) %>%
  kbl(digits = 3, caption = "Net diversification contrasts across models") %>%
  kable_styling(full_width = FALSE) %>%
  row_spec(which(tabla_contrastes$signif == "≠ 0"), bold = TRUE, background = "#DFF0D8")


library(kableExtra)

tabla_contrastes %>%
  arrange(modelo, tipo, diferencia) %>%
  kbl(
    digits = 3,
    caption = "Net diversification contrasts across models",
    col.names = c("Model", "Contrast", "Mean", "Median", "2.5%", "97.5%", "Significance", "Type")
  ) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  row_spec(which(tabla_contrastes$signif == "≠ 0"), bold = TRUE, background = "#DFF0D8") %>%
  collapse_rows(columns = 8, valign = "top")  # agrupa por tipo (observable/hidden)

# Como .csv
write.csv(tabla_contrastes, "net_diversification_contrasts_summary.csv", row.names = FALSE)




# Required packages
# Install/update the development version
# Step 1: Ensure devtools is installed
install.packages("devtools")

# Step 2: Reinstall the correct version of RevGadgets
devtools::install_github("revbayes/RevGadgets@stochastic_map", force = TRUE)
library(RevGadgets)


# Read the annotated tree with ancestral state reconstructions
tree <- processAncStates("anc_states_summary_MUHISSE.tree")

# Plot posterior probabilities of hidden states at tips
# Load required libraries
library(RevGadgets)
library(ggplot2)

# Define your color palette for the 8 states
Mussecols <- c(
  "#8dd3c7",  # F0 C0 A
  "#ffffb3",  # F0 C1 A
  "#bebada",  # F1 C1 A
  "#fb8072",  # F1 C0 A
  "#80b1d3",  # F0 C0 B
  "#fdb462",  # F0 C1 B
  "#b3de69",  # F1 C1 B
  "#fccde5"   # F1 C0 B
)

# Step 1: Process the annotated tree
i <- processAncStates("anc_states_summary_MUHISSE.tree",
  state_labels = c(
    "0" = "F0 C0 A",
    "1" = "F0 C1 A",
    "2" = "F1 C1 A",
    "3" = "F1 C0 A",
    "4" = "F0 C0 B",
    "5" = "F0 C1 B",
    "6" = "F1 C1 B",
    "7" = "F1 C0 B"
  )
)

# Step 2: Plot the MAP states
plotAncStatesMAP(
  t = i,
  tree_layout = "circular",
  tip_labels = FALSE,
  tip_labels_italics = FALSE,
  node_labels_as = "state_posterior",
  node_labels_size = 0.01,
  tip_labels_states_size = 1,
  state_transparency = 0.5,
  node_size_as = "state_posterior",
  node_size = c(0.05, 2),
  node_color = Mussecols
)


# Custom color palette grouped by observed state
Mussecols_grouped <- c(
  "#fdae6b", "#9ecae1", # F0 C0 A
  "#fd8d3c", "#6baed6", # F0 C1 A
  "#f16913", "#3182bd",
  "#d94801", "#08519c"
)


plotAncStatesMAP(
  t = i,
  tree_layout = "circular",
  tip_labels = FALSE,
  tip_labels_italics = FALSE,
  node_labels_as = "state_posterior",
  node_labels_size = 0.01,
  tip_labels_states_size = 1,
  state_transparency = 0.5,
  node_size_as = "state_posterior",
  node_size = c(0.05, 3),
  node_color = Mussecols_grouped
)





# Extract the tip posterior data from the processed object
library(tibble)
# Convert to tibble
tip_data <- as_tibble(i)
View(tip_data)

# Convert to a regular tibble
tip_df <- as.data.frame(tip_data)
tip_df

# Filter just tips (nodes with labels)
tips_only <- tip_df %>% dplyr::filter(!is.na(label))

# How many unique labels?
length(unique(tips_only$label))  # Expected: ~5470

tips_only$anc_state_1_pp <- as.numeric(tips_only$anc_state_1_pp)
# Add categorical bin for posterior support
tips_only$support_category <- cut(
  tips_only$anc_state_1_pp,
  breaks = c(0, 0.5, 0.8, 1),
  labels = c("< 0.5", "0.5–0.8", "> 0.8")
)

# Table of number of tips in each category
support_summary <- tips_only %>%
  dplyr::count(support_category)

print(support_summary)



library(ggplot2)

ggplot(support_summary, aes(x = support_category, y = n, fill = support_category)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +
  labs(
    title = "Posterior support for inferred tip states (MuHiSSE)",
    x = "Posterior probability (MAP state)",
    y = "Number of tips"
  ) +
  scale_fill_manual(values = c("< 0.5" = "#F94144", "0.5–0.8" = "#F9C74F", "> 0.8" = "#43AA8B")) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")




library(ggtree)
library(ggplot2)
library(dplyr)
library(tidytree) 
tree_data <- i@phylo

# Ensure anc_state_1_pp is numeric
tips_only$posterior <- as.numeric(tips_only$anc_state_1_pp)

# Merge tips_only info with the tree
tree_tip_df <- left_join(
  ggtree::fortify(tree_data),          # get tree layout info
  tips_only[, c("label", "anc_state_1", "posterior")],  # tip info
  by = c("label")
)

# Example color palette for 8 combined states
state_colors <- c(
  "F0 C0 A" = "#fdae6b",
  "F1 C0 A" = "#fd8d3c",
  "F0 C1 A" = "#f16913",
  "F1 C1 A" = "#d94801",
  "F0 C0 B" = "#9ecae1",
  "F1 C0 B" = "#6baed6",
  "F0 C1 B" = "#3182bd",
  "F1 C1 B" = "#08519c"
)


ggtree(tree_data, layout = "circular") %<+% tips_only +
  geom_tippoint(
    aes(color = anc_state_1, size = posterior),
    alpha = 0.8
  ) +
  scale_color_manual(values = state_colors, name = "Inferred State") +
  scale_size(range = c(0.2, 1), name = "Posterior\nSupport") +
  theme_tree2() +
  theme(legend.position = "right") +
  ggtitle("Tip states and posterior support (MuHiSSE)") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
  )





library(ggplot2)
library(dplyr)

# Define the path to your stepping-stone files
# (adjust the path if needed)
stone_files <- list.files(path = "/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory_Carotenoids/Output/ML/MuHiSSE", pattern = "ppMuHiSSENORMAL_stone_.*\\.log$", full.names = TRUE)

# Read and store all log-likelihoods
all_stones <- lapply(seq_along(stone_files), function(i) {
  df <- read.table(stone_files[i], header = TRUE)
  df$stone <- i - 1  # stone number starts from 0
  df
})

# Combine into one dataframe
stone_data <- bind_rows(all_stones)

# Optional: filter last few samples of each stone to remove early noise
stone_summaries <- stone_data %>%
  group_by(stone) %>%
  summarize(mean_logL = mean(Likelihood, na.rm = TRUE),
            sd_logL = sd(Likelihood, na.rm = TRUE),
            n = n())

# Plot mean likelihood per stone
ggplot(stone_summaries, aes(x = stone, y = mean_logL)) +
  geom_line(color = "#1D3557", linewidth = 1) +
  geom_point(size = 2, color = "#E63946") +
  geom_errorbar(aes(ymin = mean_logL - sd_logL, ymax = mean_logL + sd_logL),
                width = 0.2, color = "#457B9D") +
  theme_minimal() +
  labs(title = "Stepping-Stone Marginal Likelihood Profile",
       x = "Stone Index", y = "Mean Log-Likelihood")



# Load necessary packages
library(coda)
library(ggplot2)

# Load the MCMC trace file
trace <- read.table("/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory_Carotenoids/Output/MuHiSSE_dietncar2Ready.log", header = TRUE)

# For example, check log-likelihood or posterior
head(names(trace))  # Look for "posterior" or "logLikelihood"

# Plot the trace of the posterior
plot(as.mcmc(trace$Posterior), main = "Posterior Trace")

ggplot(trace, aes(x = 1:nrow(trace), y = Posterior)) +
  geom_line(alpha = 0.6) +
  labs(title = "Posterior Trace Plot", x = "Iteration", y = "Posterior Probability") +
  theme_minimal()


# Define the path to your stepping-stone files
# (adjust the path if needed)
stone_files <- list.files(path = "/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory_Carotenoids/Output/ML/MuHiSSE", pattern = "ppMuHiSSENORMAL_stone_.*\\.log$", full.names = TRUE)

# Read and store all log-likelihoods
all_stones <- lapply(seq_along(stone_files), function(i) {
  df <- read.table(stone_files[i], header = TRUE)
  df$stone <- i - 1  # stone number starts from 0
  df
})

# Combine into one dataframe
stone_data <- bind_rows(all_stones)

# Optional: filter last few samples of each stone to remove early noise
stone_summaries <- stone_data %>%
  group_by(stone) %>%
  summarize(mean_logL = mean(Likelihood, na.rm = TRUE),
            sd_logL = sd(Likelihood, na.rm = TRUE),
            n = n())

# Plot mean likelihood per stone
ggplot(stone_summaries, aes(x = stone, y = mean_logL)) +
  geom_line(color = "#1D3557", linewidth = 1) +
  geom_point(size = 2, color = "#E63946") +
  geom_errorbar(aes(ymin = mean_logL - sd_logL, ymax = mean_logL + sd_logL),
                width = 0.2, color = "#457B9D") +
  theme_minimal() +
  labs(title = "Stepping-Stone Marginal Likelihood Profile",
       x = "Stone Index", y = "Mean Log-Likelihood")


# Define the path to your stepping-stone files
# (adjust the path if needed)
stone_files1 <- list.files(path = "/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory_Carotenoids/Output/ML/MuHiSSE", pattern = "ppMuHiSSENORMAL_stone_.*\\.log$", full.names = TRUE)

# Read and store all log-likelihoods
all_stones1 <- lapply(seq_along(stone_files), function(i) {
  df <- read.table(stone_files[i], header = TRUE)
  df$stone <- i - 1  # stone number starts from 0
  df
})

# Combine into one dataframe
stone_data1 <- bind_rows(all_stones1)

# Optional: filter last few samples of each stone to remove early noise
stone_summaries1 <- stone_data1 %>%
  group_by(stone) %>%
  summarize(mean_logL = mean(Likelihood, na.rm = TRUE),
            sd_logL = sd(Likelihood, na.rm = TRUE),
            n = n())

# Plot mean likelihood per stone
ggplot(stone_summaries1, aes(x = stone, y = mean_logL)) +
  geom_line(color = "#1D3557", linewidth = 1) +
  geom_point(size = 2, color = "#E63946") +
  geom_errorbar(aes(ymin = mean_logL - sd_logL, ymax = mean_logL + sd_logL),
                width = 0.2, color = "#457B9D") +
  theme_minimal() +
  labs(title = "Stepping-Stone Marginal Likelihood Profile",
       x = "Stone Index", y = "Mean Log-Likelihood")


#### SS MUHISSE 2
stone_files2<- list.files(path = "/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory_Carotenoids/Output/ML/MuHiSSE2", pattern = "ppMuHiSSENORMAL_stone_.*\\.log$", full.names = TRUE)

# Read and store all log-likelihoods
all_stones2 <- lapply(seq_along(stone_files2), function(i) {
  df <- read.table(stone_files2[i], header = TRUE)
  df$stone <- i - 1  # stone number starts from 0
  df
})

all_s
# Combine into one dataframe
stone_data2 <- bind_rows(all_stones2)
stone_data2

# Optional: filter last few samples of each stone to remove early noise
stone_summaries2 <- stone_data2 %>%
  group_by(stone) %>%
  summarize(mean_logL = mean(Likelihood, na.rm = TRUE),
            sd_logL = sd(Likelihood, na.rm = TRUE),
            n = n())
stone_summaries2 

# Plot mean likelihood per stone
ggplot(stone_summaries2, aes(x = stone, y = mean_logL)) +
  geom_line(color = "#1D3557", linewidth = 1) +
  geom_point(size = 2, color = "#E63946") +
  geom_errorbar(aes(ymin = mean_logL - sd_logL, ymax = mean_logL + sd_logL),
                width = 0.2, color = "#457B9D") +
  theme_minimal() +
  labs(title = "Stepping-Stone Marginal Likelihood Profile",
       x = "Stone Index", y = "Mean Log-Likelihood")



# Comparar
diff_ml <- abs(ml1 - ml2)
diff_ml

plot(stone_summaries1$mean_logL, stone_summaries2$mean_logL) +abline(1,1)


plot(stone_summaries1$mean_logL, stone_summaries3$mean_logL)
plot(stone_summaries2$mean_logL, stone_summaries3$mean_logL)




# Mostrar resultados
cat("Marginal Likelihood corrida 1:", ml1, "\n")
cat("Marginal Likelihood corrida 2:", ml2, "\n")
cat("Diferencia absoluta:", diff_ml, "\n")


library(ggplot2)
library(dplyr)

# Agrega una columna para identificar cada corrida
stone_summaries1$run <- "Run 1"
stone_summaries2$run <- "Run 2"

# Combina los dataframes
combined <- bind_rows(stone_summaries1, stone_summaries2)

# Graficar
# Plot
ggplot(combined, aes(x = stone, y = mean_logL, color = run, fill = run)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = mean_logL - sd_logL,
                  ymax = mean_logL + sd_logL),
              alpha = 0.2, color = NA) +
  theme_minimal() +
  labs(
    title = "Comparison of Stepping-Stone Mean Log-Likelihood Across Runs",
    x = "Stone Index",
    y = "Mean Log-Likelihood"
  ) +
  scale_color_manual(values = c("Run 1" = "#1D3557", "Run 2" = "#E63946")) +
  scale_fill_manual(values = c("Run 1" = "#1D3557", "Run 2" = "#E63946"))

# Número de stones
K <- nrow(stone_summaries1)
beta <- seq(0, 1, length.out = K)
delta_beta <- diff(beta)

# Vector de log-likelihood promedio por stone para cada corrida
logL1 <- stone_summaries1$mean_logL
logL2 <- stone_summaries2$mean_logL

# Cálculo con regla del trapecio
ml1 <- sum(delta_beta * (logL1[-1] + logL1[-K]) / 2)
ml2 <- sum(delta_beta * (logL2[-1] + logL2[-K]) / 2)

# Diferencia absoluta
diff_ml <- abs(ml1 - ml2)

# Mostrar resultados
cat("Marginal Likelihood Run 1:", ml1, "\n")
cat("Marginal Likelihood Run 2:", ml2, "\n")
cat("Absolute Difference:", diff_ml, "\n")

#### SS MUSSEEE
# Define the path to your stepping-stone files
# (adjust the path if needed)
stone_files2 <- list.files(path = "/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory_Carotenoids/Output/ML/MuSSE", pattern = "ppMuSSEpassercar_stone_.*\\.log$", full.names = TRUE)

# Read and store all log-likelihoods
all_stones2 <- lapply(seq_along(stone_files2), function(i) {
  df <- read.table(stone_files2[i], header = TRUE)
  df$stone <- i - 1  # stone number starts from 0
  df
})

# Combine into one dataframe
stone_data2 <- bind_rows(all_stones2)

# Optional: filter last few samples of each stone to remove early noise
stone_summaries2 <- stone_data2 %>%
  group_by(stone) %>%
  summarize(mean_logL = mean(Likelihood, na.rm = TRUE),
            sd_logL = sd(Likelihood, na.rm = TRUE),
            n = n())

# Plot mean likelihood per stone
ggplot(stone_summaries2, aes(x = stone, y = mean_logL)) +
  geom_line(color = "#1D3557", linewidth = 1) +
  geom_point(size = 2, color = "#E63946") +
  geom_errorbar(aes(ymin = mean_logL - sd_logL, ymax = mean_logL + sd_logL),
                width = 0.2, color = "#457B9D") +
  theme_minimal() +
  labs(title = "Stepping-Stone Marginal Likelihood Profile",
       x = "Stone Index", y = "Mean Log-Likelihood")


# Load necessary packages
library(coda)
library(ggplot2)

# Load the MCMC trace file
trace <- read.table("/Users/veronicari/Documentos/GitHub/PasserDiversification/Frugivory_Carotenoids/Output/MuHiSSE_dietncar2Ready.log", header = TRUE)

# For example, check log-likelihood or posterior
head(names(trace))  # Look for "posterior" or "logLikelihood"

# Plot the trace of the posterior
plot(as.mcmc(trace$Posterior), main = "Posterior Trace")

ggplot(trace, aes(x = 1:nrow(trace), y = Posterior)) +
  geom_line(alpha = 0.6) +
  labs(title = "Posterior Trace Plot", x = "Iteration", y = "Posterior Probability") +
  theme_minimal()


########Mean and Median


setwd("//Users/veronicari/Documentos/GitHub/PasserDiversification")

data<-read.csv("DependienteYa.csv", header=T)
str(data)

columns_to_summarize <- subset("q12", "q13", "q21","q24","q31","q34","q42","q43")
library(dplyr)

data %>%
  summarise(across(all_of(columns_to_summarize), 
                   list(mean = ~mean(.x, na.rm = TRUE), 
                        median = ~median(.x, na.rm = TRUE))))
summary(data[ , columns_to_summarize])


library(ggplot2)
library(tidyr)
library(dplyr)


columns_to_plot <- c("q12", "q13", "q21","q24","q31","q34","q42","q43")

data_long <- data %>%
  dplyr::select(all_of(columns_to_plot)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")


ggplot(data_long, aes(x = Value)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of Selected Parameters")

ggplot(data_long, aes(x = Value)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Selected Parameters", x = "Value", y = "Count")

data %>%
  summarise(across(
    all_of(c("q12", "q13", "q21","q24","q31","q34","q42","q43")),
    list(
      median = ~median(.x, na.rm = TRUE),
      IQR = ~IQR(.x, na.rm = TRUE),
      mean = ~mean(.x, na.rm = TRUE)
    )
  ))


# Count how many rows have No.Off.Zero == 0
num_zero <- sum(data$No.Off.Zero == 0)

# Total number of rows
total_rows <- nrow(data)

# Proportion of zeros
prop_zero <- num_zero / total_rows

# Print result
prop_zero


library(dplyr)
library(tidyr)



library(dplyr)
library(tidyr)

# Define rate names
rates <- c("q12", "q13", "q21", "q24", "q31", "q34", "q42", "q43")

# Build the summary table with all stats per rate
summary_table <- data %>%
  summarise(across(all_of(rates), list(
    median = ~round(median(.x, na.rm = TRUE), 5),
    mean = ~round(mean(.x, na.rm = TRUE), 5),
    IQR_low = ~round(quantile(.x, 0.25, na.rm = TRUE), 5),
    IQR_high = ~round(quantile(.x, 0.75, na.rm = TRUE), 5),
    prop_nonzero = ~round(mean(.x != 0), 3)
  ), .names = "{.col}_{.fn}"))

# Pivot into long format
summary_table_long <- summary_table %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Transition", "Statistic"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  pivot_wider(names_from = Statistic, values_from = value)

# View the final tidy table
print(summary_table_long)

library(ggplot2)

ggplot(summary_table_long, aes(x = Transition, y = z_score)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Z-scores of Transitions (Proportion Fixed to 0)",
       y = "Z-score", x = "Transition") +
  theme_minimal()




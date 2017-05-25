##Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(cowplot)

setwd("C:\\Users\\adminuser\\Desktop\\EVERYTHING THOMAS\\gitprojects\\30yr_Algae_Variability_Dorset")
dat1<-read.csv("01RELdorset.csv")
dat2<-read.csv("02ABSdorset.csv")
names(dat1)
names(dat2)

al_rel <- dat1[,c(1,4:10)]
al_abs <- dat2[,c(3:9)]
names(al_rel)
names(al_abs)


### 5 year rolling St. Dev. window for relative and absolute algal biomasses
df <- data.frame() # Create empty data frames
df1 <- data.frame()
for (i in unique(al_rel$Lake)){
  relsd_i <- rollapply(al_rel[al_rel$Lake==i,3:8],5,sd,fill=NA,by.column=TRUE)
  abssd_i <- rollapply(al_abs[al_abs$Lake==i,2:7],5,sd,fill=NA,by.column=TRUE)
  df <- rbind(df,relsd_i)
  df1<- rbind(df1, abssd_i)
}

new_dat <- data.frame(al_rel,df,al_abs,df1)
names(new_dat)
new_dat <- new_dat[, -c(15), drop=FALSE] ### Just dropping the extra "year" column
head(new_dat)


### 2 by 2 plot  of chrysophyte biomasses and their 5 year variabilities in 9 Dorset lakes
A <- ggplot(data=new_dat, aes(x=Year, y=Chrysophytes_REL))+geom_line(aes(colour=Lake), size=1.1)+
  scale_color_manual(values=c(6,"red2", "orange","yellow2","green","darkgreen","cyan","blue",1,8))+
  xlab("Year")+ylab("Relative Chrysophyte Biomass")+ylim(c(0,80))+
  theme_grey(base_size = 11, base_family = "Helvetica")+theme(legend.position="none")

C <- ggplot(data=new_dat, aes(x=Year, y=Chrysophytes_REL.1))+geom_line(aes(colour=Lake), size=1.1)+
  scale_color_manual(values=c(6,"red2", "orange","yellow2","green","darkgreen","cyan","blue",1,8))+
  xlab("Year")+ylab("5Yr Relative Variability")+ylim(c(0,30))+
  theme_grey(base_size = 11, base_family = "Helvetica")+theme(legend.position="none")

B <- ggplot(data=new_dat, aes(x=Year, y=Chrysophytes_BV))+geom_line(aes(colour=Lake), size=1.1)+
  scale_color_manual(values=c(6,"red2", "orange","yellow2","green","darkgreen","cyan","blue",1,8))+
  xlab("Year")+ylab("Total Chrysophyte Biovolume")+ylim(c(0,3600))+
  theme_grey(base_size = 11, base_family = "Helvetica")+theme(legend.position="none")

D <- ggplot(data=new_dat, aes(x=Year, y=Chrysophytes_BV.1))+geom_line(aes(colour=Lake), size=1.1)+
  scale_color_manual(values=c(6,"red2", "orange","yellow2","green","darkgreen","cyan","blue",1,8))+
  xlab("Year")+ylab("5Yr Biovolume Variability")+ylim(c(0,1500))+
  theme_grey(base_size = 11, base_family = "Helvetica")+theme(legend.position="none")

prow <- plot_grid( A + theme(legend.position="none",axis.text.x=element_blank(),
                             axis.title.x=element_blank()),
                   C + theme(legend.position="none",axis.text.x=element_blank(),
                             axis.title.x=element_blank()),
                   B + theme(legend.position="none"),
                   D + theme(legend.position="none"),
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)

legend <- get_legend(A + theme(legend.position="left"))
p <- plot_grid( prow, legend, rel_widths = c(3,0.85))
p

### create the same plots for the other species of Algae

### 3x3 plot format
#### Instead of having all variables for one species, instead you have
#### one variable for all species

Z <- ggplot(data=new_dat, aes(x=Year, y=Diatoms_REL.1))+geom_line(aes(colour=Lake), size=1.1)+
  scale_color_manual(values=c(6,"red2", "orange","yellow2","green","darkgreen","cyan","blue",1,8))+
  xlab("Year")+ylab("Relative Algal Biomass")+ylim(c(0,30))+
  theme_grey(base_size = 11, base_family = "Helvetica")+theme(legend.position="none")

Y <- ggplot(data=new_dat, aes(x=Year, y=Dinoflagellates_REL.1))+geom_line(aes(colour=Lake), size=1.1)+
  scale_color_manual(values=c(6,"red2", "orange","yellow2","green","darkgreen","cyan","blue",1,8))+
  xlab("Year")+ylab("Relative Algal Biomass")+ylim(c(0,30))+
  theme_grey(base_size = 11, base_family = "Helvetica")+theme(legend.position="none")

X <- ggplot(data=new_dat, aes(x=Year, y=Chrysophytes_REL.1))+geom_line(aes(colour=Lake), size=1.1)+
  scale_color_manual(values=c(6,"red2", "orange","yellow2","green","darkgreen","cyan","blue",1,8))+
  xlab("Year")+ylab("Relative Algal Biomass")+ylim(c(0,30))+
  theme_grey(base_size = 11, base_family = "Helvetica")+theme(legend.position="none")

W <- ggplot(data=new_dat, aes(x=Year, y=Cyanobacteria_REL.1))+geom_line(aes(colour=Lake), size=1.1)+
  scale_color_manual(values=c(6,"red2", "orange","yellow2","green","darkgreen","cyan","blue",1,8))+
  xlab("Year")+ylab("Relative Algal Biomass")+ylim(c(0,30))+
  theme_grey(base_size = 11, base_family = "Helvetica")+theme(legend.position="none")

V <- ggplot(data=new_dat, aes(x=Year, y=Chlorophytes_REL.1))+geom_line(aes(colour=Lake), size=1.1)+
  scale_color_manual(values=c(6,"red2", "orange","yellow2","green","darkgreen","cyan","blue",1,8))+
  xlab("Year")+ylab("Relative Algal Biomass")+ylim(c(0,30))+
  theme_grey(base_size = 11, base_family = "Helvetica")+theme(legend.position="none")

U <- ggplot(data=new_dat, aes(x=Year, y=Cryptophytes_REL.1))+geom_line(aes(colour=Lake), size=1.1)+
  scale_color_manual(values=c(6,"red2", "orange","yellow2","green","darkgreen","cyan","blue",1,8))+
  xlab("Year")+ylab("Relative Algal Biomass")+ylim(c(0,30))+
  theme_grey(base_size = 11, base_family = "Helvetica")+theme(legend.position="none")

#axis.text.y=element_blank(),
prow1 <- plot_grid(Z + theme(legend.position="none",axis.text.x=element_blank(),
                             axis.title.x=element_blank()),
                   Y + theme(legend.position="none",axis.text.x=element_blank(),
                             axis.title.x=element_blank(),axis.text.y=element_blank(),
                             axis.title.y=element_blank()),
                   X + theme(legend.position="none",axis.text.x=element_blank(),
                             axis.title.x=element_blank(),axis.text.y=element_blank(),
                             axis.title.y=element_blank()),
                   W + theme(legend.position="none"),
                   V + theme(legend.position="none",axis.text.y=element_blank(),
                             axis.title.y=element_blank()),
                   U + theme(legend.position="none",axis.text.y=element_blank(),
                             axis.title.y=element_blank()),
                   align = 'vh',
                   hjust = -1,
                   nrow = 2
)

legend1 <- get_legend(A + theme(legend.position="left"))
p1 <- plot_grid( prow1, legend1, rel_widths = c(3,0.85))
p1


### Like I said above, this will be repeated 4 times, once for each variable
### (Relative and absolute biomass and the 5 year variabilities for both as well)






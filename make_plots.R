library(tidyverse)

inInbr <- "GEB.LabradorRetriever.inbreeding_coef.csv"
inEBV <- "GEB.EBV_by_dog.csv"
inPheno <- "GEB.phenotype_counts_by_year.csv"

dInbr <- as_tibble(read.csv(inInbr,header=T))
dEBV <- as_tibble(read.csv(inEBV,header=T))
dPheno <- as_tibble(read.csv(inPheno,header=T))

odir <- "plots"
dir.create(odir, showWarnings = FALSE, recursive = FALSE)

year_selection_started <- dPheno %>% select(measure,row) %>% distinct() %>% arrange(row)
year_selection_started$start <- c(2016,2011,2017,2003,2003,2006,2008,2003,2003)

xaxis_years <- c(1990,1995,2000,2005,2010,2015,2020)

### Make plot #1: line plots of change by ear
pd <- dPheno %>% group_by(row,measure,status,year,gtype,order,tot_yr) %>% summarize(n_status=sum(value))
pd <-pd %>% arrange(measure,year,status) %>% mutate(frac=n_status/tot_yr) %>% filter(status!="un")
pd <- pd %>% inner_join(year_selection_started)
tots <- pd %>% group_by(measure,gtype,order) %>% summarize(allN=sum(tot_yr))
pd <- pd %>% left_join(tots)

pd <- pd %>% mutate(measure_str=paste(row," ",gtype,": ",measure," (N=",allN,")",sep=""))

new_theme <- theme_minimal()
new_theme <- new_theme + theme(panel.grid.minor = element_blank(),
                               legend.position="top",legend.title=element_blank(),
                               axis.title = element_blank(),axis.text  = element_text(size=6),
                               strip.text = element_text(size=8, face="bold",hjust=0),strip.background = element_rect(colour="white", fill="white"),
                               panel.background = element_blank(),
                               axis.ticks =element_line(size=0.25),
                               legend.text = element_text( size = 5),
                               legend.key.height=unit(0.5,"lines"),legend.key.width=unit(0.5,"lines"))
theme_set(new_theme)

maxes <- pd %>% filter(order!="Success") %>% group_by(measure) %>% summarize(frac=max(frac)) %>% inner_join(pd) %>% mutate(type="max")
maxes <- pd %>% group_by(measure) %>% summarize(year=max(year)) %>% inner_join(pd) %>% mutate(type="current") %>% bind_rows(maxes)
maxes <- pd %>% filter(start==year) %>% inner_join(pd) %>% mutate(type="onset") %>% bind_rows(maxes)
maxes <- pd %>% filter(order=="Success") %>% group_by(measure) %>% summarize(frac=min(frac)) %>% inner_join(pd) %>% mutate(type="max")  %>% bind_rows(maxes)
maxes <- maxes %>% select(measure,measure_str,gtype,year,frac) %>% distinct() %>% mutate(percent=paste(round(frac*100,1),"%",sep=""))



p <- ggplot(pd,aes(x=year,y=frac))
p <- p + geom_vline(aes(xintercept=start),color="black",size=0.5,alpha=0.6)
p <- p + geom_point(aes(color=gtype),data=maxes,size=1)
p <- p + geom_line(aes(color=gtype)) + facet_wrap(~measure_str,ncol=1,scales="free_y")
p <- p + geom_text(aes(label=percent),vjust=0.5,hjust=0,data=maxes,size=1.75,fontface="bold",color="grey20",nudge_x=0.5)
p <- p + scale_x_continuous(breaks=xaxis_years,limits=c(1990,2022))
p <- p + scale_color_manual(values=c("#b2182b","#b2182b","#08519c"))
p <- p + theme(legend.position="none", strip.text = element_text(size=5, face="bold",hjust=0)) #, panel.grid.minor = element_blank()) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave(plot=p,filename=paste(odir,"/change_by_year.line.pdf",sep=""),width=2.25,height=10.7)




### Make plot #2: bar plots of change by year for traits with scale of "Mild-Severe"

colors <- rev(c("#bdbdbd","#d9d9d9","#f0f0f0","#d6604d","#b2182b"))
pd <- dPheno  %>% inner_join(year_selection_started) %>% mutate(start=start-0.5)
pd <- pd %>% mutate(score=if_else(order=="Success",as.integer(score),as.integer(6-score)))

pdAll <- pd %>% mutate(scorelab=paste(score,desc)) %>% select(-status) %>% distinct()
pdAll <- pdAll %>% mutate(measure_str=paste(row," ",gtype,": ",measure,sep=""))

pd <- pdAll %>% filter(order=="Mild-Severe")
height=(0.86)*length(unique(pd$measure))+1


p <- ggplot(pd,aes(fill=scorelab,x=year,y=value)) + geom_bar(position="fill", stat="identity") 
p <- p + geom_vline(aes(xintercept=start),color="#b2182b",size=0.25,alpha=0.75)
p <- p + geom_text(aes(label=tot_yr),y=0.98,size=1.25,angle=90,vjust=0.5,hjust=1,color="grey20")
p <- p + scale_fill_manual(values=colors,breaks=unique(pd$scorelab),labels=unique(pd$desc))
p <- p + scale_x_continuous(breaks=xaxis_years,limits=c(1989,2021))
p <- p + scale_y_continuous(breaks=c(0,0.5,1))
p <- p + facet_wrap(~measure_str,ncol=1)

ggsave(plot=p,filename=paste(odir,"/scale5.change_by_year.bar.pdf",sep=""),width=3,height=height,useDingbats=FALSE)


### Make plot #3: bar plots of change by year for traits with scale of "Normal-Affected"

colors <- rev(c("#bdbdbd","#f4a582","#b2182b"))

pd <- pdAll %>% filter(order=="Normal-Affected")
height=(0.87)*length(unique(pd$measure))+0.75

p <- ggplot(pd,aes(fill=scorelab,x=year,y=value)) + geom_bar(position="fill", stat="identity") 
p <- p + geom_vline(aes(xintercept=start),color="#b2182b",size=0.25,alpha=0.75)
p <- p + geom_text(aes(label=tot_yr),y=0.98,size=1.25,angle=90,vjust=0.5,hjust=1,color="grey20")
p <- p + scale_fill_manual(values=colors,breaks=unique(pd$scorelab),labels=unique(pd$desc))
p <- p + facet_wrap(~measure_str,ncol=1)
p <- p + scale_x_continuous(breaks=xaxis_years,limits=c(1989,2021))
p <- p + scale_y_continuous(breaks=c(0,0.5,1))

ggsave(plot=p,filename=paste(odir,"/scale3.change_by_year.bar.pdf",sep=""),width=3,height=height,useDingbats=FALSE)

### Make plot #4: bar plots of change by year for traits with scale of "Success"

colors <- c("#969696","#bdbdbd","#08519c")

pd <- pdAll %>% filter(order=="Success")
height=(0.87)*length(unique(pd$measure))+0.75

p <- ggplot(pd,aes(fill=scorelab,x=year,y=value)) + geom_bar(position="fill", stat="identity") 
p <- p + geom_vline(aes(xintercept=start),color="#b2182b",size=0.25,alpha=0.75)
#p <- p + geom_text(aes(label=tot_yr),y=0.02,size=1.25,angle=90,vjust=0.5,hjust=0,color="white")
p <- p + scale_fill_manual(values=colors,breaks=unique(pd$scorelab),labels=unique(pd$desc))
p <- p + facet_wrap(~measure_str,ncol=1)
p <- p + scale_x_continuous(breaks=xaxis_years,limits=c(1989,2021))
p <- p + scale_y_continuous(breaks=c(0,0.5,1))

ggsave(plot=p,filename=paste(odir,"/success.change_by_year.bar.pdf",sep=""),width=3,height=height,useDingbats=FALSE)

### Make plot #5: bar plots of change by year for traits about elbows

colors <- rev(c("#bdbdbd","#fee5d9","#d6604d","#b2182b"))

pd <- pdAll %>% filter(order=="Elbow")
height=(0.805)*length(unique(pd$measure))+0.75
p <- ggplot(pd,aes(fill=scorelab,x=year,y=value)) + geom_bar(position="fill", stat="identity") 
p <- p + geom_vline(aes(xintercept=start),color="#b2182b",size=0.25,alpha=0.75)
p <- p + geom_text(aes(label=tot_yr),y=0.98,size=1.25,angle=90,vjust=0.5,hjust=1,color="grey20")
p <- p + scale_fill_manual(values=colors,breaks=unique(pd$scorelab),labels=unique(pd$desc))
p <- p + scale_x_continuous(breaks=xaxis_years,limits=c(1989,2021))
p <- p + scale_y_continuous(breaks=c(0,0.5,1))

p <- p + facet_wrap(~measure_str,ncol=1)
ggsave(plot=p,filename=paste(odir,"/elbows.change_by_year.bar.pdf",sep=""),width=3,height=height,useDingbats=FALSE)

### Make box plots of change by generation

pd <- dEBV
pd <- pd %>% mutate(label=paste(row," ",gtype,": ",EBVtrait," ",EBVname,sep=""))
nplots <- length(unique(pd$label))
pwidth=2.2*2
pheight=(0.9*nplots)*2
breeder <- pd %>% filter(breeder)
breeder <- breeder %>% group_by(GenCoeffRound,label) %>% summarize(EBVmin=quantile(EBV,0.25),EBVmax=quantile(EBV,0.75),EBV=median(EBV))

p <- ggplot(pd, aes(x = as.factor(GenCoeffRound), y = EBV))
p <- p + geom_boxplot(fill="grey80",alpha=0.5,outlier.size=0.5,outlier.color="grey30",width=0.75)
p <- p + geom_line(aes(x = as.factor(GenCoeffRound),y=EBV,group=label),data=breeder,color="#b2182b")
p <- p + geom_point(aes(x = as.factor(GenCoeffRound),y=EBV),data=breeder,color="#b2182b",size=1.5)
p <- p + geom_point(aes(x = as.factor(GenCoeffRound),y=EBV),data=breeder,color="white",size=0.5,alpha=0.9)
#p <- p + geom_ribbon(aes(x = as.factor(GenCoeffRound),ymin=EBVmin,ymax=EBVmax,group=label),data=breeder,fill="red",alpha=0.5)
p <- p + scale_x_discrete("generation") 
p <- p + scale_y_continuous("EBV")
p <- p + ggtitle("EBV change over last 17 generations")
p <- p + facet_wrap(~label,scales="free_y",ncol=1)
p <- p + theme_minimal()
p <- p + theme(panel.grid.minor = element_blank(),
                  panel.grid.major.x = element_blank(),
                  legend.position="top",legend.title=element_blank(),
                  axis.title = element_text(size=8),
                  strip.text = element_text(size=8, face="bold",hjust=0),
               strip.background = element_rect(colour="white", fill="white"),
                  panel.background = element_blank(),
                  axis.ticks.y =  element_line(size=0.25,color="black"),
               axis.ticks.x  = element_blank(),
               axis.text = element_text(size = 8),)
ggsave(plot=p,filename=paste(odir,"/EBVs.all.boxplot.pdf",sep=""),width=pwidth,height=pheight)

pd <- pd %>% filter(breeder)
p <- ggplot(pd, aes(x = as.factor(GenCoeffRound), y = EBV))
p <- p + geom_boxplot(fill="grey80",alpha=0.5,outlier.size=0.5,outlier.color="grey30",width=0.75)
p <- p + scale_x_discrete("generation") 
p <- p + scale_y_continuous("EBV")
p <- p + ggtitle("EBV change over last 17 generations")
p <- p + facet_wrap(~label,scales="free_y",ncol=1)
p <- p + theme_minimal()
p <- p + theme(panel.grid.minor = element_blank(),
               panel.grid.major.x = element_blank(),
               legend.position="top",legend.title=element_blank(),
               axis.title = element_text(size=8),
               strip.text = element_text(size=8, face="bold",hjust=0),
               strip.background = element_rect(colour="white", fill="white"),
               panel.background = element_blank(),
               axis.ticks.y =  element_line(size=0.25,color="black"),
               axis.ticks.x  = element_blank(),
               axis.text = element_text(size = 8),)
ggsave(plot=p,filename=paste(odir,"/EBVs.breeders.boxplot.pdf",sep=""),width=pwidth,height=pheight)

pd <- dInbr %>% mutate(generation_bin=(round(dog_GenerationCoefficient*2,0))/2) %>% filter(generation_bin<=16)
#breaks <- c(0:(2*max(pd$generation_bin)))/2
breaks <- c(0:(max(pd$generation_bin)/2))*2
labels <- breaks
cnts <- pd %>% group_by(generation_bin) %>% summarize(n=n())
avg <- pd %>% group_by(generation_bin) %>% summarize(n=n(),mean=mean(dog_InbreedingCoefficient))
prev <- avg %>% mutate(generation_bin=generation_bin+1) %>% rename(earlier_mean=mean,earlier_n=n)
chg <- avg %>% inner_join(prev) %>% filter(earlier_mean!=0) %>% mutate(diff=mean-earlier_mean) %>% mutate(perc=diff/earlier_mean) #perc=(mean-earlier_mean)/earlier_mean)

p <- ggplot(pd, aes(x = as.factor(generation_bin), y = dog_InbreedingCoefficient))
p <- p + geom_boxplot(fill="grey80",alpha=0.5,outlier.size=0.5,outlier.color="grey30",width=0.75)
p <- p + scale_x_discrete("generation",breaks=breaks,labels=labels) 
p <- p + scale_y_continuous("Inbreeding coefficient")
p <- p + ggtitle("Change in inbreeding over last 16 generations")
p <- p + theme_minimal()
p <- p + theme(panel.grid.minor = element_blank(),
               legend.position="top",legend.title=element_blank(),
               axis.title = element_text(size=8),
               strip.text = element_text(size=8, face="bold",hjust=0),
               strip.background = element_rect(colour="white", fill="white"),
               panel.background = element_blank(),
               axis.ticks.y =  element_line(size=0.25,color="black"),
               axis.ticks.x  = element_blank(),
               axis.text = element_text(size = 8),)
ggsave(plot=p,filename=paste(odir,"/GEB.LabradorRetriever.inbreeding_coef.boxplot.pdf",sep=""),width=100,height=100,units="mm")

chg <- chg %>% filter(generation_bin>6)
p <- ggplot(chg, aes(x = generation_bin, y = perc))
p <- p + geom_line() + geom_point() + geom_hline(yintercept=0.02,color="red")
p <- p + scale_x_continuous("generation",breaks=breaks,labels=labels) 
p <- p + scale_y_continuous("percent change")
p <- p + ggtitle("Change in inbreeding over last 17 generations")
p <- p + theme_minimal()
p <- p + theme(panel.grid.minor = element_blank(),
               legend.position="top",legend.title=element_blank(),
               axis.title = element_text(size=8),
               strip.text = element_text(size=8, face="bold",hjust=0),
               strip.background = element_rect(colour="white", fill="white"),
               panel.background = element_blank(),
               axis.ticks.y =  element_line(size=0.25,color="black"),
               axis.ticks.x  = element_blank(),
               axis.text = element_text(size = 8),)
ggsave(plot=p,filename=paste(odir,"/GEB.LabradorRetriever.COI_chg_per_generation.line.pdf",sep=""),width=100,height=100,units="mm")


p <- ggplot(cnts, aes(x = generation_bin, y = n))
p <- p + geom_line() + geom_point() 
p <- p + scale_x_continuous("generation",breaks=breaks,labels=labels) 
p <- p + scale_y_continuous("n dogs")
p <- p + ggtitle("n dogs per year (inbreeding calcs)")
p <- p + theme_minimal()
p <- p + theme(panel.grid.minor = element_blank(),
               legend.position="top",legend.title=element_blank(),
               axis.title = element_text(size=8),
               strip.text = element_text(size=8, face="bold",hjust=0),
               strip.background = element_rect(colour="white", fill="white"),
               panel.background = element_blank(),
               axis.ticks.y =  element_line(size=0.25,color="black"),
               axis.ticks.x  = element_blank(),
               axis.text = element_text(size = 8),)
ggsave(plot=p,filename=paste(odir,"/GEB.LabradorRetriever.dogs_per_generation.line.pdf",sep=""),width=100,height=100,units="mm")

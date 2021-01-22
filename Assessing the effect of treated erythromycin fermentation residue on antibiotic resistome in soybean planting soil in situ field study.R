data <- read.csv("clipboard",header = T, sep = "\t")
data
data1 <- melt(data,id.vars = c("Concentration","Time"))
head(data)
write.csv(data1,"tem.csv")
p <- ggplot(data,aes(Time,ave,group=Concentration))+
  geom_line(size=1)+
  geom_point(aes(fill=Concentration),shape=21,size=4)+
  geom_errorbar(aes(ymin=ave-std,ymax=ave+std),width=0.1)+
  facet_wrap(~variable,scales = "free",nrow = 3)

  
p
data1 <- melt(data,id.vars = "Concentration")
head(data1)
p1 <- ggplot(data1,aes(Concentration,value,fill=Concentration))+
  geom_violin(aes(color=Concentration))+
  geom_boxplot(width=0.2)+
  theme(axis.text.x = element_text(angle = 325))+
  facet_wrap(~variable,scales = "free")
p1

data <- read.csv("clipboard",header = T, sep = "\t")
data
p <- ggplot(data,aes(grp,SUM,fill=grp))+
  geom_violin(aes(color=grp))+
  geom_boxplot(width=0.2)+
  theme(axis.text.x = element_text(angle = 325))
p

data <- read.csv("clipboard",header = T, sep = "\t")
data
p <- ggplot(data,aes(X,AVE,fill=grp))+
  geom_bar(stat = "identity",position = "dodge",width = 0.6)+
  geom_errorbar(aes(ymin=AVE-STD,ymax=AVE+STD),width=0.2)+
  theme_bw()+
  theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 325))

p
data <- read.csv("clipboard",header = T, row.names=1, sep = "\t")
data
color=colorRampPalette(c("darkblue","white","red"))(100)
pheatmap(log(sqrt(data)+1),fontsize = 8,cluster_rows = F,cluster_cols = F,
         color=color)

data <- read.csv("clipboard",header = T, sep = "\t")
data
data <- melt(data)
head(data)
p <- ggplot(data,aes(variable,value,fill=X))+
  geom_bar(stat = "identity",position = "stack",width = 0.7)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3")+
  theme(axis.text.x = element_text(angle = 325))
p

data <- read.csv("clipboard",header = T,row.names = 1, sep = "\t")
data1 <-t(data) 
nmds1 <- metaMDS(data1,distance = "bray")
nmds1
summary(nmds1)
nmds1$points

data <- read.csv("clipboard",header = T, sep = "\t")
data
p <- ggscatter(data,x="MDS1",y="MDS2",fill = "group",shape = 21,size = 3,color = "group",
               ellipse = TRUE,
               mean.point = TRUE, star.plot = TRUE,
               ellipse.level = 0.95,
               ggtheme = theme_bw())
  
p

data <- read.csv("clipboard",header = T, sep = "\t")
head(data)
data1 <- melt(data,id.vars = "group")
head(data1)
p <- ggplot(data1,aes(group,value,fill=group))+
  geom_violin(aes(color=group))+
  geom_boxplot(width=0.2)+
  facet_wrap(~variable,scales = "free",ncol = 4)
p

a <- read.csv("clipboard",header = T, sep = "\t")
b <- read.csv("clipboard",header = T, sep = "\t")
a
b
a1 <- melt(a)
head(a1)
colnames(a1) <- c("Group","Type","AVE")
head(a1)
b1 <- melt(b)
colnames(b1) <- c("Group","Type","STD")
head(b1)
c <- merge(a1,b1)
head(c)
write.csv(c,"risk_score.csv")
c <- read.csv("clipboard",header = T,sep = "\t")
p <- ggplot(c,aes(Time,AVE,group=grp))+
  geom_line(size=1)+
  geom_point(shape=21,aes(fill=grp),size=4)+
  geom_errorbar(aes(ymin=AVE-STD,ymax=AVE+STD),width=0.2)+
  facet_wrap(~Type,scales = "free",ncol = 4)
p

n1 <- rep("CK",3)
n2 <- rep("ERM250",3)
n3 <- rep("ERM500",3)
n4 <- c(n1,n2,n3)
n4
n5 <- rep(n4,5)


c <- read.csv("clipboard",header = T,sep = "\t")
twano <- aov(SUM~grp*time,data = c)
twano
summary(twano)

plot(twano)

#MRPP
c <- read.csv("clipboard",header = T,sep = "\t",row.names = 1)
head(c)
c.dist = vegdist(subset(c, select = c(-group1,-group2)))
c.dist
c.mrpp1 = mrpp(c.dist, grouping = c$group1, permutations = 999)
c.mrpp2 = mrpp(c.dist, grouping = c$group2, permutations = 999)
c.mrpp1
c.mrpp2

c.anosim1 = anosim(c.dist, grouping = c$group1, permutations = 999)
c.anosim2 = anosim(c.dist, grouping = c$group2, permutations = 999)
c.anosim1
c.anosim2

c.adonis1 = adonis(c.dist ~ c$group1, permutations = 999)
c.adonis2 = adonis(c.dist ~ c$group2, permutations = 999)
c.adonis1
c.adonis2

#network
data <- read.table("clipboard",header = T,row.names = 1,sep = "\t")
head(data)
data <- t(data)
occor <- corr.test(x=data,use="pairwise",method="spearman",adjust="fdr",alpha=0.05)
occor.r = occor$r # 取相关性矩阵R值
occor.p = occor$p # 取相关性矩阵p值
occor.r[occor.p>0.05|abs(occor.r)<0.6] = 0
write.csv(occor.r,file="network-frame.csv")

#procrustes
env <- read.table("clipboard",header = T,row.names = 1,sep = "\t")
env <- t(env)
otu <- read.table("clipboard",header = T,row.names = 1,sep = "\t")
otu <- t(otu)
env_pca <- rda(env, scale = TRUE)
otu_hel <- decostand(otu, method = 'hellinger')
otu_pca <- rda(otu_hel, scale = FALSE)
par(mfrow = c(1, 2))
biplot(env_pca, choices = c(1, 2), scaling = 1, 
       main = 'BAC-PCA', col = c('red', 'blue'))
biplot(otu_pca, choices = c(1, 2), scaling = 1, 
       main = 'ARG-PCA', col = c('red', 'blue'))
site_env <- summary(otu_pca, scaling = 1)$site
site_otu <- summary(otu_pca, scaling = 1)$site
proc <- procrustes(X = env_pca, Y = otu_pca, symmetric = TRUE)
summary(proc)
plot(proc, kind = 1, type = 'text')
names(proc)
head(proc$Yrot)  #Procrustes 分析后 Y 的坐标
head(proc$X)  #Procrustes 分析后 X 的坐标
proc$ss  #偏差平方和 M2 统计量
proc$rotation  #通过该值可获得旋转轴的坐标位置
plot(proc, kind = 2)
residuals(proc)  #残差值
set.seed(123)
prot <- protest(X = env_pca, Y = otu_pca, permutations = how(nperm = 999))
prot
names(prot)
prot$signif  #p 值
prot$ss  #偏差平方和 M2 统计量
Y <- cbind(data.frame(proc$Yrot), data.frame(proc$X))
X <- data.frame(proc$rotation)
Y
X
group <- read.table("clipboard",header = T,sep = "\t")
Y$samples <- rownames(Y)
Y <- merge(Y, group, by = 'samples')
Y
p <- ggplot(Y) +
  geom_point(aes(X1, X2), size = 4, shape = 21, fill = "darkorchid1") +
  geom_point(aes(PC1, PC2), size = 4, shape = 21, fill = "lightblue1") +
  geom_segment(aes(x = X1, y = X2, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.1, 'cm')),
               color = 'slateblue', size = 1) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'),
        legend.key = element_rect(fill = 'transparent')) +
  labs(x = 'PcoA 1', y = 'PcoA 2', color = '') +
  geom_vline(xintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_hline(yintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_abline(intercept = 0, slope = X[1,2]/X[1,1], size = 0.3) +
  geom_abline(intercept = 0, slope = X[2,2]/X[2,1], size = 0.3) +
  annotate('text', label = sprintf('M^2 == 0.1435'),
           x = -0.21, y = 0.42, size = 3, parse = TRUE) +
  annotate('text', label = 'P < 0.001',
           x = -0.21, y = 0.38, size = 3, parse = TRUE)

p

#mge
data <- read.csv("clipboard",header = T, sep = "\t")
data
p <- ggplot(data,aes(grp,ppm,fill=grp))+
  geom_violin(aes(color=grp),width=0.8)+
  geom_boxplot(width=0.2)+
  theme(axis.text.x = element_text(angle = 325))+
  ylim(0,2)
p

data <- read.csv("clipboard",header = T, sep = "\t")
data
p <- ggplot(data,aes(time,ave,group=grp))+
  geom_line(size=1)+
  geom_point(aes(fill=grp),shape=21,size=4)+
  geom_errorbar(aes(ymin=ave-std,ymax=ave+std),width=0.1)+
  ylim(0,2)

p


data <- read.csv("clipboard",header = T,sep = "\t")
data
data1 <- data[1:6,]
data1
data2 <- data[c(1,2,3,7,8,9),]
data2
wilcox.test(Simpson~Time,data1,paired=FALSE)

#20200702
#diversity
data <- read.csv("clipboard",header = T, sep = "\t")
head(data)
p <- ggplot(data,aes(Concentration,Richness,fill=Concentration))+
  geom_violin(aes(color=Concentration),width=0.8)+
  geom_boxplot(width=0.2)+
  theme_bw()+
  ylim(25,125)
p
p <- ggplot(data,aes(Concentration,Shannon,fill=Concentration))+
  geom_violin(aes(color=Concentration),width=0.8)+
  geom_boxplot(width=0.2)+
  theme_bw()+
  ylim(2,4)
p
p <- ggplot(data,aes(Concentration,Simpson,fill=Concentration))+
  geom_violin(aes(color=Concentration),width=0.8)+
  geom_boxplot(width=0.2)+
  theme_bw()+
  ylim(0.8,1.0)
p

data <- read.csv("clipboard",header = T,sep = "\t")
head(data)
p <- ggplot(data,aes(Time,ave,group=Concentration))+
  geom_line(size=1)+
  geom_point(aes(fill=Concentration),shape=21,size=5)+
  geom_errorbar(aes(ymin=ave-std,ymax=ave+std),width=0.1)+
  ylim(25,125)+
  theme_bw()
p
data <- read.csv("clipboard",header = T,sep = "\t")
head(data)
p <- ggplot(data,aes(Time,ave,group=Concentration))+
  geom_line(size=1)+
  geom_point(aes(fill=Concentration),shape=21,size=5)+
  geom_errorbar(aes(ymin=ave-std,ymax=ave+std),width=0.1)+
  ylim(2,4)+
  theme_bw()
p
data <- read.csv("clipboard",header = T,sep = "\t")
head(data)
p <- ggplot(data,aes(Time,ave,group=Concentration))+
  geom_line(size=1)+
  geom_point(aes(fill=Concentration),shape=21,size=5)+
  geom_errorbar(aes(ymin=ave-std,ymax=ave+std),width=0.1)+
  ylim(0.8,1.0)+
  theme_bw()
p

#abundance
data <- read.csv("clipboard",header = T,sep = "\t")
head(data)
p <- ggplot(data,aes(Types,ave,fill=grp))+
  geom_bar(stat = "identity",position = "dodge",width = 0.6)+
  geom_errorbar(aes(ymin=ave-std,ymax=ave+std),width=0.2)+
  theme_bw()+
  #ylim(0,0.045)+
  theme(axis.text.x = element_text(angle = 325))
p

#Wilcox-test
##con
data <- read.csv("clipboard",header=F,sep = "\t")
data1 <- data[1:6,]
data2 <- data[c(1,2,3,7,8,9),]
data3 <- data[4:9,]
wilcox.test(V1~V2,data1,paired=FALSE)
wilcox.test(V1~V2,data2,paired=FALSE)
wilcox.test(V1~V2,data3,paired=FALSE) #---end---

#time
data <- read.csv("clipboard",header=F,sep = "\t")
data1 <- data[1:6,]
data2 <- data[c(1,2,3,7,8,9),]
data3 <- data[c(1,2,3,10,11,12),]
data4 <- data[c(1,2,3,13,14,15),]
wilcox.test(V1~V3,data1,paired=FALSE)
wilcox.test(V1~V3,data2,paired=FALSE)
wilcox.test(V1~V3,data3,paired=FALSE)
wilcox.test(V1~V3,data4,paired=FALSE)

#sub
#in time
data <- read.csv("clipboard",header=F,sep = "\t")
data1 <- data[1:6,]
data2 <- data[c(1,2,3,7,8,9),]
data3 <- data[4:9,]
wilcox.test(V8~V9,data1,paired=FALSE)
wilcox.test(V8~V9,data2,paired=FALSE)
wilcox.test(V8~V9,data3,paired=FALSE)

#out time
data <- read.csv("clipboard",header=F,sep = "\t")
head(data)
data1 <- data[1:6,]
data2 <- data[c(1,2,3,7,8,9),]
data3 <- data[c(1,2,3,10,11,12),]
data4 <- data[c(1,2,3,13,14,15),]
wilcox.test(V8~V10,data1,paired=FALSE)
wilcox.test(V8~V10,data2,paired=FALSE)
wilcox.test(V8~V10,data3,paired=FALSE)
wilcox.test(V8~V10,data4,paired=FALSE)

a <- c(1,2,3,40,41,42)
b <- c('a','a','a','b','b','b')
da <- data.frame(a,b)
da
wilcox.test(a~b,da)

rm(list = ls())
da <- read.csv("clipboard",header = T,sep = '\t')
da
da1 <- melt(da)
da1
wilcox.test(value~variable,da1,paired=TRUE)


da <- read.csv("clipboard",header = F,sep = '\t')
head(da)
mu <- wilcox.test(x = da$V3, y = da$V15, paired=TRUE)
write.table(mu$p.value,"mu4.txt",sep = '\t',append = TRUE,row.names = FALSE, col.names = FALSE)

da <- read.csv("clipboard",header = F,sep = '\t')
da
wilcox.test(x = da$V3, y = da$V15, paired=TRUE)


kruskal.test(da$V1~da$V2)
pp <- aov(da$V1~da$V2)
summary(pp
        )

da <- read.csv("clipboard",header = F,sep = '\t')
head(da)
#wilcox.test(da$V1~da$V9, paired=FALSE)
kruskal.test(da$V1~da$V2)
kruskal.test(da$V1~da$V3)

#chart
data <- read.csv("clipboard",header = T,row.names = 1,sep = '\t')
data
data <- t(data)
data
data <- melt(data)
data
data$Var1 <- factor(data$Var1,levels = c('aminoglycoside','rifamycin','tetracycline','bacitracin','macrolide.lincosamide.streptogramin','quinolone','vancomycin','multidrug'))
p <- ggplot(data,aes(Var2,Var1))+
  geom_point(aes(fill=value),shape=21,size=5)+
  scale_fill_gradient(low = "red",high = "green")+
  theme_bw()
p

#nmds-ggscatter
data <- read.csv("clipboard",header = T, sep = "\t")
data
p <- ggscatter(data,x="MDS1",y="MDS2",fill = "group",shape = 21,size = 3,color = "group",
               ellipse = TRUE,
               mean.point = TRUE, star.plot = TRUE,
               ellipse.level = 0.95,
               ggtheme = theme_bw())+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)

p


#mge
rm(list = ls())
data <- read.csv("clipboard",header = T, sep = "\t")
head(data)
p <- ggplot(data,aes(grp,p16s,fill=grp))+
  geom_violin(aes(color=grp),width=0.8)+
  geom_boxplot(width=0.2)+
  theme_bw()+
  ylim(0,0.005)
p

data <- read.csv("clipboard",header = T, sep = "\t")
head(data)
p <- ggplot(data,aes(time,ave,group=grp))+
  geom_line(size=1)+
  geom_point(aes(fill=grp),shape=21,size=5)+
  geom_errorbar(aes(ymin=ave-std,ymax=ave+std),width=0.1)+
  theme_bw()+
  ylim(0,0.005)
p

#risk score
rm(list = ls())
data <- read.csv("clipboard",header = T, sep = "\t")
head(data)
p <- ggplot(data,aes(group,Risk_Score,fill=group))+
  geom_violin(aes(color=group),width=0.8)+
  geom_boxplot(width=0.2)+
  theme_bw()+
  ylim(20,28)
p

data <- read.csv("clipboard",header = T, sep = "\t")
head(data)
p <- ggplot(data,aes(time,ave,group=grp))+
  geom_line(size=1)+
  geom_point(aes(fill=grp),shape=21,size=5)+
  geom_errorbar(aes(ymin=ave-std,ymax=ave+std),width=0.1)+
  theme_bw()+
  ylim(15,30)
p

#sem
#1.首先要根据分析需要设计模型，全为观测模型：
data <- read.csv("clipboard",header = T,sep = "\t")
head(data)
model <- '
mc ~ time + con + tem
mge ~ time + con + tem
arg ~ time + con + tem + mc + mge
'

#2.拟合模型
#library(lavaan)
## 加载SEM建模需要的R包：lavaan

fit <- sem(model, data = data) 
## 使用SEM函数拟合模型

summary(fit, standardized = TRUE)
## 查看拟合结果

#3.计算拟合系数
fitMeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))
write.csv(a,"fit.csv")
write.csv(b,"fit_par.csv")

#network-p-g
# 加载包
#library(igraph)
#library(psych)

# 读取otu-sample矩阵，行为sample，列为otu
otu <-  read.csv("clipboard",header = T,row.names = 1,sep = "\t")
otu <- t(otu)
# 计算OTU间两两相关系数矩阵
# 数据量小时可以用psych包corr.test求相关性矩阵，数据量大时，可应用WGCNA中corAndPvalue, 但p值需要借助其他函数矫正
occor = corr.test(otu,use="pairwise",method="spearman",adjust="fdr",alpha=.05)
occor.r = occor$r # 取相关性矩阵R值
occor.p = occor$p # 取相关性矩阵p值

# 确定物种间存在相互作用关系的阈值，将相关性R矩阵内不符合的数据转换为0
occor.r[occor.p>0.05|abs(occor.r)<0.6] = 0 

# 将occor.r保存为csv文件
write.csv(occor.r,file="network.csv") ##此时可以使用Gephi进行网络可视化


#KW test
da <- read.csv("clipboard",header = F,sep = '\t')
head(da)
#wilcox.test(da$V1~da$V9, paired=FALSE)
kruskal.test(da$V1~da$V2)
kruskal.test(da$V1~da$V3)
kruskal.test(da$V3~da$V4)



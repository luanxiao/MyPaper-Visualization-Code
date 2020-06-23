library(ggplot2) #加载程序包
data=read.csv(file=file.choose(),header=T)  #加载文件

##绘制基本图形
p=ggplot(data,aes(x=,y=),colour=)
p1=p+geom_point() #绘制散点图 defalt shape/shape=1 空心/shape="." 像素点 alpha:透明度
   ##只有shape=21时才可以使用fill填充
p=ggplot(data,aes(x=,y=)colour=)
p2=p+geom_bar(aes(fill=),width=0.8,position = "dodge")+
labs(title="")  #绘制并列柱状图 ##labs表头 
theme(plot.title = element_text(hjust = 0.5)) ##标题居中
p2=p+geom_bar(aes(fill=),stat = "identity",position="stack",width=0.8)+
labs(title="") #绘制堆叠柱状图
p2=p+geom_bar(aes(fill=),stat = "identity",position="fill",width=0.8)+
labs(title="") #绘制百分比柱状图
p2=p+geom_bar(aes(fill=item),stat = "identity",width=0.8)+facet_grid(#分组信息~.)+
labs(title="") #绘制分面柱状图
p3=ggplot(data,aes(x=,y=,colour=))+geom_boxplot(aes(fill=),alpha=0.2,
outlier.colour = "red",outlier.shape = 2,outlier.size = 5,coef=1.5)+
geom_jitter(width = 0.1) #绘制箱式图
stat_summary(fun.y=“mean”,geom=“point”,shape=23,size=3,fill=“white”) #箱式图加平均
p2=p1+geom_line(aes(group=),size=?)  ##绘制折线图

##绘制算点图加折线图时，因为多个映射关系，必须定义分组
ggplot(data,aes(x,y,group=group))


##误差线
geom_errorbar(aes(ymax = response + se, ymin = response -  se) ##绘制误差棒  ##自定义调色板
# 使用geom_errorbar()绘制带有误差棒的条形图
# 这里一定要注意position要与`geom_bar()`保持一致，由于系统默认dodge是0.9，
# 因此geom_errorbar()里面position需要设置0.9，width设置误差棒的大小
ggplot(data = df, aes(x = treatment, y = response, fill = group)) + 
geom_bar(stat = "identity", position = "dodge") + 
geom_errorbar(aes(ymax = response + se, ymin = response -  se), 
position = position_dodge(0.9), width = 0.15) + 
scale_fill_brewer(palette = "Set1")

##legend图例的修改
p+theme(legend.position='none')  #none为不加legend,"left"为左
p+theme(legend.title=element_blank())  #删除legend.title
p+scale_colour_hue("what does it eat?")
scale_fill_hue(guide = guide_legend(title=NULL))  #修改legend.title内容
theme(legend.background = element_blank()) #图例背景色的去除
scale_fill_discrete(limits=c("trt1", "trt2", "ctrl"))  ##图例顺序的修改
guides(fill=guide_legend(reverse=TRUE)) ##逆转图例



##theme主题修改
+theme_bw() #去除背景色
+theme(panel.grid.major=element_blank(),
panel.grid.minor?=?element_blank()) #去除网格线
+theme(panel.border?=?element_blank())? #去除边框线
+theme(panel.background?=?element_blank()) #移除背景色和边框
+theme(axis.line?=?element_line(colour?=?"black")) #添加坐标轴
+theme(strip.background = element_rect(fill = "black")) #facet背景黑色
+theme(strip.text = element_text(color = "white",face="bold")) #facet字体白色


##坐标轴及标题字体调整
p+labs(x="人口",y="失业率",title="经济调查报告") #修改xy以及总标题
theme(title=element_text(family="myFont",size=12,color="red",
face="italic",hjust=0.2,lineheight=0.2), #修改字体
axis.title.x=element_text(size=10,face="bold",color="blue",hjust=0.5),
axis.title.y=element_text(size=14,color="green",hjust=0.5,angle=45),
axis.text.x=element_text(family="myFont",size=8,color="red") )  ##修改xy标题
以及坐标轴字体
windowsFonts(HEL=windowsFont("Helvetica CE 55 Roman"),
             RMN=windowsFont("Times New Roman"),
             ARL=windowsFont("Arial"))
old_theme <- theme_update(
  plot.title=theme_text(family="ARL", size=18, face="bold", colour="black"),
  axis.title.x=theme_text(family="HEL", size=15, colour="black"),
  axis.title.y=theme_text(family="HEL", size=15, angle=90, colour="black"),
  axis.text.x=theme_text(family="RMN", size=11, colour="black"),
  axis.text.y=theme_text(family="RMN", size=11, colour="black"),
  axis.ticks=theme_segment(colour="black"),
  panel.grid.major=theme_blank(),
  panel.grid.minor=theme_blank(),
  panel.background=theme_blank(),
  axis.line=theme_segment(size=1)
)

##坐标轴间隔调整
scale_x_continuous(limits=c(1950,2000),breaks=seq(1950,2000,5))

##修改坐标轴顺序
data$column = factor(data$column, levels=c('D','B','C','A','E')) #修改坐标轴顺序

##散点图上文字的添加
geom_text(label=paste(data$date),colour="black",size=4,vjust=-0.8)

##加水平线或者垂直线
P+geom_hline(yintercept=0,linetype="dotted")

##一页多图
library(easyGgplot2)
ggplot2.multiplot(plot1,plot2,plot3,plot4, cols=2)

##图片分页
+facet_grid(.~variable,scales = "free") ##水平方向分割
facet_wrap() #图片更加美观

## X/Y 轴颠倒
+coord_flip()

##坐标轴类型的转换
#字符型转数字型
as.numeric()
#数字型转字符型
as.character()

##坐标轴顺序
data$sample=factor(data$sample,levels=c("d7-0","d14-0","d21-0",
"d28-0","d7-0.001","d14-0.001","d21-0.001","d28-0.001","d7-0.01",
"d14-0.01","d21-0.01","d28-0.01","d7-0.1","d14-0.1","d21-0.1","d28-0.1","d7-1","d14-1",
"d21-1","d28-1","d7-10","d14-10","d21-10","d28-10"))

##自定义调色板
p2=p1+scale_colour_manual(values=tiaose)
tiaose=c("springgreen","orange","blue","yellow","purple","red")
tiaose=c("slateblue","tomato","green")

##独立添加图例
ggplot(data = datos, aes(x = fecha)) +
  geom_line(aes(y = TempMax, colour = "TempMax")) +
  geom_line(aes(y = TempMedia, colour = "TempMedia")) +
  geom_line(aes(y = TempMin, colour = "TempMin")) +
  scale_colour_manual("", 
                      breaks = c("TempMax", "TempMedia", "TempMin"),
                      values = c("red", "green", "blue"))


##自定义颜色
p2=p1+scale_colour_brewer(palette = "Pastel1")
#library(RColorBrewer)
#display.brewer.all()查看所有颜色主题
tiaose=c("lightsalmon","lightskyblue","lightpink","lightgreen",
"lightgoldenrod","slateblue1","lightcoral","lightblue","orchid1","palegreen",
"steelblue1","rosybrown1","orchid3","cyan")
##文本的添加
p + annotate("text", x=3, y=48, label="Group 1")

##线性拟合
p+geom_smooth(method = "lm")
##相关性分析
library(ggpubr)
ggplot(data=dat, aes(x=gene, y=gene2))+
  geom_point(color="red")+stat_smooth(method="lm",se=FALSE)++
  stat_cor(data=dat, method = "pearson")
#stat_cor(data=dat, method = "pearson")意为用pearson相关进行相关性分析，可以自行更改方法

##PCA圈图并且添加置信限
p+stat_ellipse(level = 0.9, geom = "polygon")
##https://www.cnblogs.com/xudongliang/p/8203835.html
##PCA绘制多边形
library(plyr)
group_border <- ddply(data, 'group', function(df) df[chull(df[[3]], df[[4]]), ])
p+geom_polygon(data = group_border, alpha = 0.3, show.legend = F) 
##data原始数据,group为分组,3和4为坐标轴,
##http://blog.sciencenet.cn/blog-3406804-1155528.html




##melt函数
melt(data, id.vars, measure.vars, 
     variable.name = "variable", ..., na.rm = FALSE,
     value.name = "value", factorsAsStrings = TRUE)


##ggplot2热图
data=melt(data)
p=ggplot(data,aes(x,y))+geom_tile(fill=aes(value))
+scale_fill_gradient(low = "green", high = "red")

##ggfority画PCA圈图##-----------------------
autoplot(prcomp(df),data=iris,colour="Species",frame=T,frame.type="norm")










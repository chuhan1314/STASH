library(ggpubr)
library(readxl)
library(rjson)
library(utils)
library(BiocManager)
library('rcrossref')
library(httr)
library(jsonlite)
library(reticulate)
library(ggpubr)
library(tidyverse)
library(cowplot)
library(scales)

Sys.setlocale("LC_TIME", "English")
colorss= c("#E7E8EC",
  "#D7CADB",
  "#8E79A2",
  "#4F4F81",
  "#6785B4",
  "#BCCDDD")


if(F){
#cr_cn(dois = "10.1371/journal.pone.0112608", format = "text", style = "apa")
  rcran = available.packages()
  bioc = BiocManager::available()
  pypi = read.table("pypi.txt")
  save(rcran, bioc, pypi, file = "./script&data/packages.rda")
}else{
  #load(file = "./script&data/packages.rda")
}

#pypi = as.vector(pypi[,1])
overeview = read_xlsx("../STtools_pub/manuscript/STtools_last.xlsx", sheet = 1)
data = read_xlsx("../STtools_pub/manuscript/STtools_last.xlsx", sheet = 2)
#sort(unique(Reduce(append,strsplit(data$`function`,split = ";")))) 
#sort(overeview$classfation)
#data$pkg_pat = "github"
#
#data$pkg_pat[data$name %in% rcran[,1]] = "R-CRAN"
#data$pkg_pat[data$name %in% bioc] = "R-bioc"
#data$pkg_pat[data$name %in% pypi] = "pypi"
#
#write.table(data, file = "tmp", sep = "%")
#sapply(data$language, function(x){
#  tmp = data.frame(fromJSON(json_str = gsub("\'","\"", x))
#  df = data.frame(lan = colnames(tmp), freq = as.numeric(tmp[1,]), gr = "gr")
#  ggbarplot(data = df, x = "lan", y = "freq", position = position_stack())
#})
f1a = data.frame(name = data$tools, date = data$`Published Date`, num = 1)
f1a = f1a[order(f1a$date),]
f1a$date = as.Date(f1a$date, origin = "%Y-%m-%d")
f1are = f1a %>% group_by(date) %>% summarise(num = sum(num))
f1are$num = Reduce(sum,f1are$num, accumulate = T)
f1are = na.omit(f1are)
f1af = ggplot(data = f1are, aes(x = date, y = num)) + 
  geom_line(color = "#8E79A2", size = 2)+
  geom_area(fill = "#8E79A2", alpha = 0.5)+
  geom_vline(xintercept = as.Date("2020-12-31", origin = "%Y-%m-%d"), color = "#4F4F81")+
  theme_pubclean()+
  xlab("")+
  scale_x_date(date_labels = "%b %Y",breaks = date_breaks("6 month"))+
  labs(title ="",subtitle="",y=" ") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
f1af
ggsave(f1af, filename = "./manuscript/figs/f1af.pdf",width = 6, height = 4, dpi = 600)

f1b = unlist(sapply(data$development, function(x){
  if(x != "NA"){
        tmp = names(rjson::fromJSON(json_str = gsub("\'","\"", x)))[1]
  }else{
        tmp = "NA"}
  return(tmp)}))
f1br = data.frame(table(f1b))
f1br = f1br[order(f1br$Freq, decreasing = T),]
f1br= f1br[-c(3,4,5),]
f1br = rbind(f1br[1:4,],data.frame(f1b = "Other", Freq = sum(f1br$Freq[5:8])))
f1br$Freq = f1br$Freq/sum(f1br$Freq) * 100
f1br$f1b = factor(f1br$f1b,levels = f1br$f1b)
f1bf = ggplot(f1br, aes( x = f1b, weight = Freq, fill = f1b, color = f1b))+
  geom_bar(position="dodge")+
  geom_text(aes(label=f1b, y = Freq), vjust= 1.5, colour="white",position=position_dodge(.9), size=3)+
  scale_fill_manual(values  = c("#8E79A2","#D7CADB","#4F4F81","#6785B4","#BCCDDD"))+
  scale_color_manual(values  = c("#8E79A2","#D7CADB","#4F4F81","#6785B4","#BCCDDD"))+
  theme_pubclean()+
  xlab("")+
  labs(title ="",subtitle="",y=" ") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.position = "none") 
f1bf
ggsave(f1bf, filename = "./manuscript/figs/f1bf.pdf",width = 6, height = 4, dpi = 600)

f1c = data.frame(table(data$Application))
f1c = f1c[order(f1c$Freq),]
f1c = f1c[-c(1,5),]
f1c$Freq = f1c$Freq/sum(f1c$Freq) * 100
f1c$labs = paste0(f1c$Var1,"\n",round(f1c$Freq,2),"%")
f1c$ymax = cumsum(f1c$Freq)
f1c$ymin = c(0, head(f1c$ymax, n = -1))
f1cf = ggplot(data = f1c, aes(fill = Var1, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_rect(show.legend = F,alpha=0.8) +
  scale_fill_manual(values = c("command line" = "#E7E8EC",
                              "R" = "#D7CADB",
                              "python" = "#8E79A2",
                              "GUI" = "#4F4F81",
                              "MATLAB" = "#6785B4",
                              "webtools" = "#BCCDDD"))+ 
  coord_polar(theta = "y") +
  labs(x = "", y = "", title = "",fill='') + 
  xlim(c(1.5, 5)) +
  theme_light() +
  theme(panel.grid=element_blank()) + ## 去掉白色外框
  theme(axis.text=element_blank()) + ## 把图旁边的标签去掉
  theme(axis.ticks=element_blank()) + ## 去掉左上角的坐标刻度线
  theme(panel.border=element_blank()) + ## 去掉最外层的正方形边框
  geom_text(aes(x = 3.5, y = ((ymin+ymax)/2),label = labs) ,size= 1.5, color ="white")
f1cf
ggsave(f1cf, filename = "./manuscript/figs/f1cf.pdf",width = 8, height = 6, dpi = 600)

f1d = data.frame(table(data$`iST/sST`))
f1d = f1d[order(f1d$Freq),]
f1d = f1d[-c(1),]
f1d$Freq = f1d$Freq/sum(f1d$Freq) * 100
f1d$labs = paste0(f1d$Var1,"\n",round(f1d$Freq,2),"%")
f1d$ymax = cumsum(f1d$Freq)
f1d$ymin = c(0, head(f1d$ymax, n = -1))
f1df = ggplot(data = f1d, aes(fill = Var1, ymax = ymax, ymin = ymin, xmax = 4, xmin = 3)) +
  geom_rect(show.legend = F,alpha=0.8) +
  scale_fill_manual(values = c("#D7CADB",
                               "#8E79A2",
                               "#6785B4"))+
  coord_polar(theta = "y") +
  labs(x = "", y = "", title = "",fill='') + 
  xlim(c(1.5, 5)) +
  theme_light() +
  theme(panel.grid=element_blank()) + ## 去掉白色外框
  theme(axis.text=element_blank()) + ## 把图旁边的标签去掉
  theme(axis.ticks=element_blank()) + ## 去掉左上角的坐标刻度线
  theme(panel.border=element_blank()) + ## 去掉最外层的正方形边框
  geom_text(aes(x = 3.5, y = ((ymin+ymax)/2),label = labs) ,size= 1.5, color = "white")
f1df
ggsave(f1df, filename = "./manuscript/figs/f1df.pdf",width = 8, height = 6, dpi = 600)


func = as.data.frame(table(Reduce(append,strsplit(data$Categories,split = ";"))))

f3a = merge(x = func, y = overeview, by.x = "Var1", by.y = "Categories", all.y  = T)
f3a = f3a[order(f3a$Freq, decreasing = T),]
f3a$Var1 = factor(f3a$Var1, levels = rev(f3a$Var1))
f3a$Phase = factor(f3a$Phase, levels = c("Raw data processing","Preprocessing","Generic downstream tasks","Other downstream tasks","Multiple"))
f3a = f3a[!f3a$Var1 %in% c("dimensionality reduction","","Differential expression","Vignetting Correction"),]
f3af = ggplot(data = f3a, aes(x = Var1, y = Freq)) +
  geom_bar(aes(fill = Phase),stat="identity") +
  scale_fill_manual(values = c("Raw data processing" = "#D7CADB",
                               "Preprocessing" = "#4F4F81",
                               "Generic downstream tasks" = "#8E79A2",
                               "Other downstream tasks" = "#6785B4",
                               "Multiple"  = "#BCCDDD")) +
  theme_pubclean() + coord_flip() #+ theme(axis.text.y = element_text(angle = 45,hjust=1, vjust=1)) 
f3af
ggsave(f3af, filename = "./manuscript/figs/f3af.pdf",width = 13, height = 10, dpi = 600)


func2 = data.frame(name = data$tools, value = sapply(strsplit(data$Categories,split = ";"),length))
func2$value = factor(func2$value, levels = c(1:12))
f3bf = ggplot(data = func2, aes(x = value)) +
  geom_bar(color = "#8E79A2", fill =  "#8E79A2") +
  theme_pubclean()# + theme(axis.text.x = element_text(angle = 45,hjust=1, vjust=1))
f3bf
ggsave(f3bf, filename = "./manuscript/figs/f3bf.pdf",width = 6, height = 4, dpi = 600)

dict = overeview$Categories
names(dict) = overeview$Phase
data2 = data
data2$datamm = c(NA,NA,format(as.Date(data$`Published Date`[3:260], origin = "%Y-%m-%d"),format="%Y-%m"))
data2 = data2[order(data2$`Published Date`),]
f3e = sapply(strsplit(data2$Categories,split = ";"), function(x){
  #x =  strsplit(data$`function`,split = ";")[[16]]
  tmp2 = sapply(x, function(y){
    if (!y  %in% c("dimensionality reduction","","Differential expression","Vignetting Correction")) {
      tmp = names(dict[dict == y])
    }else{
      tmp = "NA"
    }
    return(tmp)
  })
  return(unique(tmp2))
})
names(f3e) = data2$datamm

tmp = c(0,0,0,0,0)
df = data.frame()
for (nam in names(f3e)) {
  #x = f3e[[17]]
  #x = append(x, "NA")
  x = f3e[[nam]]
  tmp2 = c("Raw data processing","Preprocessing","Generic downstream tasks","Other downstream tasks","Multiple")
  tmp3 = tmp2 %in% x
  tmp = tmp + tmp3
  df = rbind(df, tmp)
}
colnames(df) = c("Raw data processing","Preprocessing","Generic downstream tasks","Other downstream tasks","Multiple")
f3ee = apply(df,1,function(x){
  x/sum(x)
})
f3ee = data.frame(t(f3ee))
f3ee$date = names(f3e)
f3ee$rank = as.numeric(rownames(f3ee))
f3ee = f3ee[order(f3ee$rank, decreasing = T),]
f3ee = f3ee[!duplicated(f3ee$date),]
f3ee = na.omit(f3ee)
f3ee$date = paste0(f3ee$date,"-01")
f3ee$date = as.Date(f3ee$date, origin = "%Y-%m-%d")
f3ee = f3ee[1:54,]

f3ef1 = ggplot(data = f3ee) +
  geom_line(aes(x = date, y = Raw.data.processing),color = "blue", size = 2)+
  #geom_area(aes(x = date, y = Raw.data.processing),fill = "blue", alpha = 0.5)+
  geom_line(aes(x = date, y = Preprocessing),color = "red", size = 2)+
  #geom_area(aes(x = date, y = Preprocessing),fill = "red", alpha = 0.5)+
  geom_line(aes(x = date, y = Generic.downstream.tasks),color = "green", size = 2)+
  #geom_area(aes(x = date, y = generic.downstream.tasks),fill = "green", alpha = 0.5)+
  geom_line(aes(x = date, y = Other.downstream.tasks),color = "yellow", size = 2)+
  #geom_area(aes(x = date, y = other.downstream.tasks),fill = "yellow", alpha = 0.5)+
  geom_line(aes(x = date, y = Multiple),color = "grey", size = 2)+
  #geom_area(aes(x = date, y = Multiple),fill = "grey", alpha = 0.5)+
  #geom_vline(xintercept = as.Date("2020-12-31", origin = "%Y-%m-%d"), color = "darkgrey")+
  cowplot::theme_half_open()+
  xlab("")+
  scale_x_date(date_labels = "%b %Y")+
  labs(title ="",subtitle="",y=" ") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
f3ef1

f3ee2 = pivot_longer(f3ee[1:6],cols = !date, names_to = "type", values_to = "value")
f3ee2$type = factor(f3ee2$type, levels = c("Raw.data.processing","Preprocessing","Generic.downstream.tasks","Other.downstream.tasks","Multiple"))
f3ee2$date2 = format(as.Date(f3ee2$date, origin = "%Y-%m-%d"),format="%b %Y")
f3ee2$date2 = factor(f3ee2$date2, levels = rev(unique(f3ee2$date2)))

f3ef2 = ggplot(f3ee2, aes( x = date2, weight = value, fill = type, color = type))+
  geom_bar( position = "stack")+
  scale_fill_manual(values = c("Raw.data.processing" = "#D7CADB",
                               "Preprocessing" = "#4F4F81",
                               "Generic.downstream.tasks" = "#8E79A2",
                               "Other.downstream.tasks" = "#6785B4",
                               "Multiple"  = "#BCCDDD")) +
  scale_color_manual(values = c("Raw.data.processing" = "#D7CADB",
                                "Preprocessing" = "#4F4F81",
                                "generic.downstream.tasks" = "#8E79A2",
                                "other.downstream.tasks" = "#6785B4",
                                "Multiple"  = "#BCCDDD")) +
  theme_pubclean()+
  xlab("")+
  labs(title ="",subtitle="",y=" ") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 3))
f3ef2
ggsave(f3ef2, filename = "./manuscript/figs/f3ef.pdf",width = 7, height = 4, dpi = 600)

notype = c("dimensionality reduction","","differential expression","vignetting Correction")
data_part = data[!grepl("2021|2022|2023|NA",data$`Published Date`),]
func = as.data.frame(table(Reduce(append,strsplit(data$Categories,split = ";"))))
func = func[!func$Var1 %in% notype,]
func$Freq = func$Freq / sum(func$Freq) *100
func3 = as.data.frame(table(Reduce(append,strsplit(data_part$Categories,split = ";"))))
func3 = func3[!func3$Var1 %in% notype,]
func3$Freq = func3$Freq / sum(func3$Freq) *100
f3c = merge(x = func, y = func3, by.x = "Var1",by.y = "Var1", all.x = T)
f3c$Freq.y[is.na(f3c$Freq.y)] = 0
colnames(f3c) = c("type","new","old")
f3c = f3c[order(f3c$new, decreasing = T),]
f3c$type = factor(f3c$type, levels = rev(f3c$type))
f3c$group = ifelse(f3c$old < f3c$new, "gain", "loss")
f3cf = ggplot(f3c)+
  geom_point(aes(x = type, y = old), color = "#6785B4", size = 2.7)+
  geom_point(aes(x = type, y = new), color = "#8E79A2", size = 2.7)+
  geom_segment(aes(x = type, y = (old), xend = type, yend = (new), color = group), size = 2)+
  scale_color_manual(values = c("gain" = "#8E79A2", "loss" = "#6785B4")) +
  coord_flip() + theme_pubclean()
f3cf
ggsave(f3cf, filename = "./manuscript/figs/f3cf.pdf",width = 13, height = 10, dpi = 600)

top6 = c("spatial domain",
  "spatial variable genes",
  "visualization",
  "imputation and denoise",
  "spatial decomposition",
  "cell-cell/gene-gene interaction")
df = data.frame()
for (topp in top6) {
  tmp = data[grepl(topp,data$Categories),]
  tmp$`Cite Number` = as.numeric(tmp$`Cite Number`)
  tmp = tmp[order(tmp$`Cite Number`, decreasing = T),]
  tmp3 = data.frame(name = tmp$tools[1:12], type = topp, cite = tmp$`Cite Number`[1:12], func = tmp$Categories[1:12])
  df = rbind(df, tmp3)
}
f3d = df[c(3,4,11,15,19,20,37,38,40,50,51,52,63,64,66),]
f3d$type = factor(f3d$type, levels =  c("spatial domain",
                                        "spatial variable genes",
                                        "imputation and denoise",
                                        "spatial decomposition",
                                        "cell-cell/gene-gene interaction"))
f3d$name[length(f3d$name)] = "cellphoneDB\nV4.0"
f3d$name = factor(f3d$name, levels = f3d$name)
f3df = ggplot(f3d, aes( x = type, weight = cite, fill = name, color = type))+
  geom_bar(position="dodge")+
  geom_text(aes(label=name, y = cite), vjust= 1.5, colour="white",position=position_dodge(.9), size=3)+
  scale_fill_manual(values = c( "#D7CADB",
                                 "#4F4F81",
                                 "#8E79A2",
                                 "#6785B4",
                                 "#BCCDDD","#D7CADB",
                                "#4F4F81",
                                "#8E79A2",
                                "#6785B4",
                                "#BCCDDD","#D7CADB",
                                "#4F4F81",
                                "#8E79A2",
                                "#6785B4",
                                "#BCCDDD")) +
  theme_pubclean()+
  xlab("")+
  labs(title ="",subtitle="",y=" ") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.position = "right")
f3df 
ggsave(f3df, filename = "./manuscript/figs/f3df.pdf",width = 9, height = 6, dpi = 600)


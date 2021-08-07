#Load packages and dependencies
extrafont::loadfonts(device="win")

if (!require("ggplot2")) install.packages("ggplot2")
if (!require(multcompView)) install.packages('multcompView')


### LOAD DATA AND TIDY ###
# These ones for testing (raw values)
vwc <- read.csv("C:\\Users\\Alex Goke\\Desktop\\VWC ANOVA.csv", header=TRUE)
vwc$date  <- as.Date(vwc$date, format = "%m/%d/%Y")
vwc.20180722 <- vwc[ which(vwc$date=="2018/7/22"), ]
vwc.20180727 <- vwc[ which(vwc$date=="2018/7/27"), ]
vwc.20180803 <- vwc[ which(vwc$date=="2018/8/3"), ]
vwc.20180813 <- vwc[ which(vwc$date=="2018/8/13"), ]
vwc.20180819 <- vwc[ which(vwc$date=="2018/8/19"), ]
vwc.20180827 <- vwc[ which(vwc$date=="2018/8/27"), ]
vwc.20180902 <- vwc[ which(vwc$date=="2018/9/2"), ]
vwc.20190703 <- vwc[ which(vwc$date=="2019/7/3"), ]
vwc.20190722 <- vwc[ which(vwc$date=="2019/7/22"), ]
vwc.20190729 <- vwc[ which(vwc$date=="2019/7/29"), ]
vwc.20190807 <- vwc[ which(vwc$date=="2019/8/7"), ]
vwc.20190820 <- vwc[ which(vwc$date=="2019/8/20"), ]
vwc.20190826 <- vwc[ which(vwc$date=="2019/8/26"), ]
vwc.20190903 <- vwc[ which(vwc$date=="2019/9/3"), ]
vwc.20190909 <- vwc[ which(vwc$date=="2019/9/9"), ]
vwc.20190915 <- vwc[ which(vwc$date=="2019/9/15"), ]

# These ones for plotting (means and errors only)
vwc.plot <- read.csv("C:\\Users\\Alex Goke\\Desktop\\VWC Plot.csv", header=TRUE)
vwc.plot$date  <- as.Date(vwc.plot$date, format = "%m/%d/%Y")
vwc.plot.2018 <- vwc.plot[ which(vwc.plot$year=="2018"), ]
vwc.plot.2019 <- vwc.plot[ which(vwc.plot$year=="2019"), ]
vwc.deficit <- read.csv("C:\\Users\\Alex Goke\\Desktop\\VWC Deficit.csv", header=TRUE)
vwc.deficit$date  <- as.Date(vwc.deficit$date, format = "%m/%d/%Y")
vwc.deficit.2018 <- vwc.deficit[ which(vwc.deficit$year=="2018"), ]
vwc.deficit.2019 <- vwc.deficit[ which(vwc.deficit$year=="2019"), ]
precip <- read.csv("C:\\Users\\Alex Goke\\Desktop\\Precipitation.csv", header=TRUE)
precip$date  <- as.Date(precip$date, format = "%m/%d/%Y")





### ANOVA/TUKEY HSD TESTS ###

# 2018/07/22 #ns
vwc.20180722.aov <- aov(vwc~treatment, data=vwc.20180722)
summary(vwc.20180722.aov)
vwc.20180722.tukey <- TukeyHSD(vwc.20180722.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20180722.aov, vwc.20180722.tukey) # ns
rm(vwc.20180722, vwc.20180722.aov, vwc.20180722.tukey)

# 2018/07/27 #significant ambient=a, drought=b
vwc.20180727.aov <- aov(vwc~treatment, data=vwc.20180727)
summary(vwc.20180727.aov)
vwc.20180727.tukey <- TukeyHSD(vwc.20180727.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20180727.aov, vwc.20180727.tukey) 
rm(vwc.20180727, vwc.20180727.aov, vwc.20180727.tukey)

# 2018/08/03 #significant ambient=a, drought=b
vwc.20180803.aov <- aov(vwc~treatment, data=vwc.20180803)
summary(vwc.20180803.aov)
vwc.20180803.tukey <- TukeyHSD(vwc.20180803.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20180803.aov, vwc.20180803.tukey) 
rm(vwc.20180803, vwc.20180803.aov, vwc.20180803.tukey)

# 2018/08/13 #significant ambient=a, drought=b
vwc.20180813.aov <- aov(vwc~treatment, data=vwc.20180813)
summary(vwc.20180813.aov)
vwc.20180813.tukey <- TukeyHSD(vwc.20180813.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20180813.aov, vwc.20180813.tukey) 
rm(vwc.20180813, vwc.20180813.aov, vwc.20180813.tukey)

# 2018/08/19 #significant ambient=a, drought=b
vwc.20180819.aov <- aov(vwc~treatment, data=vwc.20180819)
summary(vwc.20180819.aov)
vwc.20180819.tukey <- TukeyHSD(vwc.20180819.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20180819.aov, vwc.20180819.tukey) 
rm(vwc.20180819, vwc.20180819.aov, vwc.20180819.tukey)

# 2018/08/27 #significant ambient=a, drought=b
vwc.20180827.aov <- aov(vwc~treatment, data=vwc.20180827)
summary(vwc.20180827.aov)
vwc.20180827.tukey <- TukeyHSD(vwc.20180827.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20180827.aov, vwc.20180827.tukey) 
rm(vwc.20180827, vwc.20180827.aov, vwc.20180827.tukey)

# 2018/09/02 #significant ambient=a, drought=b
vwc.20180902.aov <- aov(vwc~treatment, data=vwc.20180902)
summary(vwc.20180902.aov)
vwc.20180902.tukey <- TukeyHSD(vwc.20180902.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20180902.aov, vwc.20180902.tukey) 
rm(vwc.20180902, vwc.20180902.aov, vwc.20180902.tukey)

# 2019/07/03 #ns
vwc.20190703.aov <- aov(vwc~treatment, data=vwc.20190703)
summary(vwc.20190703.aov)
vwc.20190703.tukey <- TukeyHSD(vwc.20190703.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20190703.aov, vwc.20190703.tukey) 
rm(vwc.20190703, vwc.20190703.aov, vwc.20190703.tukey)

# 2019/07/22 #significant ambient=a, drought=b
vwc.20190722.aov <- aov(vwc~treatment, data=vwc.20190722)
summary(vwc.20190722.aov)
vwc.20190722.tukey <- TukeyHSD(vwc.20190722.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20190722.aov, vwc.20190722.tukey) 
rm(vwc.20190722, vwc.20190722.aov, vwc.20190722.tukey)

# 2019/07/29 #significant ambient=a, drought=b
vwc.20190729.aov <- aov(vwc~treatment, data=vwc.20190729)
summary(vwc.20190729.aov)
vwc.20190729.tukey <- TukeyHSD(vwc.20190729.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20190729.aov, vwc.20190729.tukey) 
rm(vwc.20190729, vwc.20190729.aov, vwc.20190729.tukey)

# 2019/08/07 #significant ambient=a, drought=b
vwc.20190807.aov <- aov(vwc~treatment, data=vwc.20190807)
summary(vwc.20190807.aov)
vwc.20190807.tukey <- TukeyHSD(vwc.20190807.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20190807.aov, vwc.20190807.tukey) 
rm(vwc.20190807, vwc.20190807.aov, vwc.20190807.tukey)

# 2019/08/20 #ns
vwc.20190820.aov <- aov(vwc~treatment, data=vwc.20190820)
summary(vwc.20190820.aov)
vwc.20190820.tukey <- TukeyHSD(vwc.20190820.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20190820.aov, vwc.20190820.tukey) 
rm(vwc.20190820, vwc.20190820.aov, vwc.20190820.tukey)

# 2019/08/26 #ns
vwc.20190826.aov <- aov(vwc~treatment, data=vwc.20190826)
summary(vwc.20190826.aov)
vwc.20190826.tukey <- TukeyHSD(vwc.20190826.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20190826.aov, vwc.20190826.tukey) 
rm(vwc.20190826, vwc.20190826.aov, vwc.20190826.tukey)

# 2019/09/03 #ns
vwc.20190903.aov <- aov(vwc~treatment, data=vwc.20190903)
summary(vwc.20190903.aov)
vwc.20190903.tukey <- TukeyHSD(vwc.20190903.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20190903.aov, vwc.20190903.tukey) 
rm(vwc.20190903, vwc.20190903.aov, vwc.20190903.tukey)

# 2019/09/09 #significant ambient=a, drought=b
vwc.20190909.aov <- aov(vwc~treatment, data=vwc.20190909)
summary(vwc.20190909.aov)
vwc.20190909.tukey <- TukeyHSD(vwc.20190909.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20190909.aov, vwc.20190909.tukey) 
rm(vwc.20190909, vwc.20190909.aov, vwc.20190909.tukey)

# 2019/09/15 #significant ambient=a, drought=b
vwc.20190915.aov <- aov(vwc~treatment, data=vwc.20190915)
summary(vwc.20190915.aov)
vwc.20190915.tukey <- TukeyHSD(vwc.20190915.aov, ordered = FALSE, conf.level = 0.95)
multcompLetters4(vwc.20190915.aov, vwc.20190915.tukey) 
rm(vwc.20190915, vwc.20190915.aov, vwc.20190915.tukey)




### 2018 LINE PLOT ###
ggplot(vwc.plot.2018) +
  geom_rect(data=precip, aes(xmin = as.Date("2018/07/15"), xmax = as.Date("2018/09/15"), ymin=0, ymax=15), fill="#eeeeee") +
  geom_bar(data=precip, aes(x=date, y=precip / 2), stat="identity", fill="#77aadd") +
  geom_line(aes(x=date, y=mean, color=treatment)) +
  geom_point(aes(x=date, y=mean, color=treatment), size = 1) +
  geom_errorbar(aes(x=date, ymin=mean-se, ymax=mean+se, color=treatment), width=0, size=0.5) +
  scale_color_manual(values=c("#bbcc33", "#ee8866"), labels=c("Ambient", "Drought")) +
  scale_fill_manual(values=c("#bbcc33", "#ee8866"), labels=c("Ambient", "Drought")) +
  scale_y_continuous(name= "Soil VWC (%)", limits = c(0,15), sec.axis = sec_axis(~.*2, name="Precipitation (mm)")) +
  scale_x_date(name = "2018", limits = as.Date(c("2018/07/01", "2018/09/30"))) +
  annotate(geom = "text", label="A", fontface="bold", family= "Open Sans", size=4, x=as.Date("2018/07/1"), y=15) +
  annotate(geom = "text", label="*", fontface="bold", family= "Open Sans", size=2, x=as.Date("2018/07/27"), y=14.5) + #7/27 significant difference
  annotate(geom = "text", label="*", fontface="bold", family= "Open Sans", size=2, x=as.Date("2018/08/03"), y=9.5) + #8/3 significant difference
  annotate(geom = "text", label="*", fontface="bold", family= "Open Sans", size=2, x=as.Date("2018/08/13"), y=6) + #8/13 significant difference
  annotate(geom = "text", label="*", fontface="bold", family= "Open Sans", size=2, x=as.Date("2018/08/19"), y=6) + #8/19 significant difference
  annotate(geom = "text", label="*", fontface="bold", family= "Open Sans", size=2, x=as.Date("2018/08/27"), y=5.5) + #8/27 significant difference
  annotate(geom = "text", label="*", fontface="bold", family= "Open Sans", size=2, x=as.Date("2018/09/02"), y=5) + #9/2 significant difference
  theme_classic() +
  theme(text = element_text(size=6, face="plain", family = "Open Sans", color="#000000"),
        axis.text = element_text(size=6, face="plain", family = "Open Sans", color="#000000"),
        axis.text.y.right = element_text(size=6, face="plain", family = "Open Sans", color="#77aadd"),
        axis.title.y.left = element_text(size=6, face="bold", family = "Open Sans", color="#000000"),
        axis.title.y.right = element_text(size=6, face="bold", family = "Open Sans", color="#77aadd"),
        axis.title.x = element_text(size=9, face="bold", family = "Open Sans", color="#000000"),
        axis.line.y.right = element_line(color="#77aadd"),
        axis.ticks.y.right = element_line(color="#77aadd"),
        legend.text = element_text(size=6, face="bold", family = "Open Sans", color="#000000"),
        legend.position = c(0.5, 1),
        legend.direction = "horizontal",
        legend.background = element_rect(fill=NA),
        legend.title = element_blank())
ggsave("vwc_2018.tiff", plot=last_plot(), width = 3, height = 2, units = "in", dpi = "print")





### 2019 LINE PLOT ###
ggplot(vwc.plot.2019) +
  geom_rect(data=precip, aes(xmin = as.Date("2019/07/15"), xmax = as.Date("2019/09/15"), ymin=0, ymax=15), fill="#eeeeee") +
  geom_bar(data=precip, aes(x=date, y=precip / 2), stat="identity", fill="#77aadd") +
  geom_line(aes(x=date, y=mean, color=treatment)) +
  geom_point(aes(x=date, y=mean, color=treatment), size = 1) +
  geom_errorbar(aes(x=date, ymin=mean-se, ymax=mean+se, color=treatment), width=0, size=0.5) +
  scale_color_manual(values=c("#bbcc33", "#ee8866"), labels=c("Ambient", "Drought")) +
  scale_fill_manual(values=c("#bbcc33", "#ee8866"), labels=c("Ambient", "Drought")) +
  scale_y_continuous(name= "Soil VWC (%)", limits = c(0,15), sec.axis = sec_axis(~.*2, name="Precipitation (mm)")) +
  scale_x_date(name = "2019", limits = as.Date(c("2019/07/01", "2019/09/30"))) +
  annotate(geom = "text", label="B", fontface="bold", family= "Open Sans", size=4, x=as.Date("2019/07/1"), y=15) +
  annotate(geom = "text", label="*", fontface="bold", family= "Open Sans", size=2, x=as.Date("2019/07/22"), y=13.5) + #7/22 significant difference
  annotate(geom = "text", label="*", fontface="bold", family= "Open Sans", size=2, x=as.Date("2019/07/29"), y=10) + #7/29 significant difference
  annotate(geom = "text", label="*", fontface="bold", family= "Open Sans", size=2, x=as.Date("2019/08/07"), y=11.5) + #8/7 significant difference
  annotate(geom = "text", label="*", fontface="bold", family= "Open Sans", size=2, x=as.Date("2019/09/09"), y=7) + #9/9 significant difference
  annotate(geom = "text", label="*", fontface="bold", family= "Open Sans", size=2, x=as.Date("2019/09/15"), y=5.5) + #9/15 significant difference
  theme_classic() +
  theme(text = element_text(size=6, face="plain", family = "Open Sans", color="#000000"),
        axis.text = element_text(size=6, face="plain", family = "Open Sans", color="#000000"),
        axis.text.y.right = element_text(size=6, face="plain", family = "Open Sans", color="#77aadd"),
        axis.title.y.left = element_text(size=6, face="bold", family = "Open Sans", color="#000000"),
        axis.title.y.right = element_text(size=6, face="bold", family = "Open Sans", color="#77aadd"),
        axis.title.x = element_text(size=9, face="bold", family = "Open Sans", color="#000000"),
        axis.line.y.right = element_line(color="#77aadd"),
        axis.ticks.y.right = element_line(color="#77aadd"),
        legend.text = element_text(size=6, face="bold", family = "Open Sans", color="#000000"),
        legend.position = c(0.5, 1),
        legend.direction = "horizontal",
        legend.background = element_rect(fill=NA),
        legend.title = element_blank())
ggsave("vwc_2019.tiff", plot=last_plot(), width = 3, height = 2, units = "in", dpi = "print")
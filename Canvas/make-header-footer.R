library(readr)
library(ggplot2)
#install.packages('showtext', dependencies = TRUE)
library(showtext)
font_add_google("Lato")

course_code <- "STATS369"
#coul = c(RColorBrewer::brewer.pal(7, "Blues")[-1],
#         RColorBrewer::brewer.pal(7, "Greys")[-1],
#         "#602065", "#3c153f", RColorBrewer::brewer.pal(7, "Greys")[6:7])

#### rest should work

set.seed(parse_number(course_code))

ngroup=16
coul <- colorRampPalette(c("#0039A6", "#188398", "#A1BE6A", "#E4C844", "#FF961C"))(ngroup)

names=paste("G_",seq(1,ngroup),sep="")
DAT=data.frame()

for(i in seq(1:30)){
  data=data.frame( matrix(0, ngroup , 3))
  data[,1]=i
  data[,2]=sample(names, nrow(data))
  data[,3]=prop.table(sample( c(rep(0,100),c(1:ngroup)) ,nrow(data)))
  DAT=rbind(DAT,data)
}
colnames(DAT)=c("Year","Group","Value")
DAT=DAT[order( DAT$Year, DAT$Group) , ]

ggplot(DAT, aes(x=Year, y=rev(Value), fill=Group )) +
  geom_area(alpha=1  )+
  theme_bw() +
  scale_fill_manual(values = coul)+
  theme(
    text = element_blank(),
    line = element_blank(),
    title = element_blank(),
    legend.position="none",
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(-1, -3.5, -1, -3.5, "cm"))

ggsave(paste0(course_code, "-footer.png"), width = 24, height = 2)

ggplot(DAT, aes(x=Year, y=Value, fill=Group )) +
  geom_area(alpha=1  )+
  theme_bw() +
  scale_fill_manual(values = coul)+
  theme(
    text = element_blank(),
    line = element_blank(),
    title = element_blank(),
    legend.position="none",
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.margin = margin(-1, -3.5, -1, -3.5, "cm"))

ggsave(paste0(course_code, "-header.png"), width = 24, height = 2)

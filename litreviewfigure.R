require(ggplot2)
require(extrafont)

litData <- read.csv('data & analysis/lit-review-data/lit-data.csv', stringsAsFactors = F)

# expanded form
ld <- expand.grid(litData$Publication, c("Expansion", "Compression", "Increased Influence of Relevant Dimension", "Decreased Influence of Irrelevant Dimension"))
ld$value <- c(litData[,2], litData[,3], litData[,4], litData[,5])

# custom fonts
font_import()

color <- rgb(135/255,35/255,40/255)

ggplot(data=ld, aes(x=Var1, y=Var2)) + 
  geom_point(aes(fill = value, color = value), size=10, pch=21)  +
  xlab("PUBLICATION") + 
  ylab("") + 
  scale_y_discrete(labels=c("EXPANSION", "COMPRESSION", "INCREASED INFLUENCE OF\nRELEVANT DIMENSION", "DECREASED INFLUENCE OF\nIRRELEVANT DIMENSION"))+
  scale_fill_manual(values=c('white', 'white', color), guide=F) +
  scale_color_manual(values=c('white',color, color), guide=F) +
  scale_shape(solid=F)+
  theme_bw() + 
  theme(text = element_text(family="Lato", color = rgb(0.2,0.2,0.2)),
        axis.title.x = element_text(family="Lato Black", size = 20),
        axis.title.y = element_text(family="Lato Black", size = 20),
        axis.text.y = element_text(family="Lato Black", size = 16),
        axis.text.x = element_text(angle=45, hjust=1),
        plot.background = element_rect(fill = "transparent",colour = NA))


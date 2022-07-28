lunaize <- function(p,ajust=.05){
  p$labels$y <- paste0(p$labels$y,"\n")
  p$labels$x <- paste0("\n",p$labels$x)
  p+ theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=20),
        axis.text.y = element_text(hjust = ajust,color='black'),
        axis.text.x = element_text(vjust = ajust,color='black')
        #plot.margin=unit(c(1,1,1.75,1.75),"cm"), # Weird spacing
        #panel.background = element_rect(fill='white'), # theme_bw does this
  )
}

lunaize_density <- function(p,ajust=.05){
  p$labels$y <- paste0(p$labels$y,"\n")
  p$labels$x <- paste0("\n",p$labels$x)
  p+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        text=element_text(size=20),
        axis.text.y = element_text(hjust = ajust,color='black'),
        axis.text.x = element_text(vjust = ajust,color='black')
        #plot.margin=unit(c(1,1,1.75,1.75),"cm"), # Weird spacing
        #panel.background = element_rect(fill='white'), # theme_bw does this
  )
}


lunaize_geomraster<-function(x){
  x+
   theme_bw()+
   theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title.y     = element_blank(),
    axis.ticks.y     = element_blank(),
    axis.text.y      = element_blank(),
    legend.position  = "none")
}

lunaize_geomrasteryticktowhite<-function(x){
  x+
   theme_bw()+
   theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
#    axis.title.y     = element_blank(),
#    axis.ticks.y     = element_text(colour="white"),
#    axis.text.y      = element_blank(),
    legend.position  = "none")
}

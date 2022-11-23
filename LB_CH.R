LB2 <- read.csv(file = "Boharadj.csv", stringsAsFactors = TRUE)
Location <- as.factor(LB2$Location)








#play with pallets: https://nanx.me/ggsci/articles/ggsci.html
mypal <- pal_futurama("planetexpress", alpha = 0.7)(5)
mypal
sp <- show_col(mypal)

# create convex hull function from this website:
#https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     
                     required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# plot out convex hulls for niche space of each species
LB_CH <- ggplot(LB2,aes(x = C, y = N, colour = Location, fill = Location))+
  # geom_point(size = 3.5)+
  stat_chull(size = 0.5, alpha = 0.3)+
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank(),
                     axis.line = element_line(),
                     axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
                     axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
                     axis.title.x = element_text(color="black", size=18, face="bold"),
                     axis.title.y = element_text(color="black", size=18, face="bold"),
                     legend.text = element_text (color = "black", size = 18, face = "italic"),
                     legend.title = element_text(color = "black", size = 18, face = "bold"),
                     text = element_text(size=18))+
  theme(plot.margin=grid::unit(c(1,1,1,1), "cm"))+
  scale_x_continuous(limits = c(-16, -8),breaks=seq(-16, -8, 2))+ 
  scale_y_continuous(limits = c(5,11),breaks=seq(5, 11, 2))+
  scale_color_manual(values =c("Chagos " = "#FF6F00B2", "SR" = "#C71000B2"), 
                     labels = c("Chagos", "Scott Reefs"))+ 
  scale_fill_manual(values =c("Chagos " = "#FF6F00B2", "SR" = "#C71000B2"), 
                    labels = c("Chagos", "Scott Reefs"))+ 
  # facet_wrap( ~ Trip, ncol = 1)+
  ylab(expression(atop(bold(~delta^15~"N " ("\u2030 " [vs]~"air"))))) + 
  xlab(expression(atop(bold(~delta^13~"C " ("\u2030 " [vs]~"VPDB"))))) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        legend.title=element_text(size=11), 
        legend.text=element_text(size=10),
        legend.position = "right") 
fig1 <- LB_CH
fig1
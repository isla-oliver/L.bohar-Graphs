#L.bohar



LB <- read.csv(file = "Bohar2.csv", stringsAsFactors = TRUE)
Location <- as.factor(LB$Location)

clrs <- c("Chagos" = "#FF6F00B2","SR" = "#C71000B2")
LB_means <- LB %>%
  group_by(Location) %>% 
  summarise(Nm = mean(N), 
            Nsd = sd(N), 
            Cm = mean(C), 
            Csd = sd(C))

LBplot <- ggplot()+ 
  geom_errorbar(data = LB_means, 
                aes(x = Cm, 
                    ymin = Nm - Nsd, ymax = Nm + Nsd, colour = Location), width = 0.2, cex = 1)+ 
  geom_errorbarh(data = LB_means, 
                 aes(y = Nm, 
                     xmin = Cm - Csd, xmax = Cm + Csd, colour = Location), cex = 1)+ 
  scale_x_continuous(limits = c(-18, -8),breaks=seq(-18, -8, 2))+ 
  scale_y_continuous(limits = c(8,16),breaks=seq(8, 16, 2))+
  scale_color_manual(values =c("Chagos " = "#FF6F00B2", "SR" = "#C71000B2"), 
                     labels = c("Chagos", "Scott Reefs"))+
  xlab(expression(atop(bold(~delta^13~"C " ("\u2030 " [vs]~"VPDB")))))+ 
  ylab(expression(atop(bold(~delta^15~"N " ("\u2030 " [vs]~"air")))))

LBplot + theme(text = element_text(size = 20))



LB2 <- LBplot +    geom_errorbar(aes(x = -14.513, 
                                     ymin = 13.434 - 1, ymax = 13.434 + 1), cex = 1, lty = 2, colour = "#C71000B2")+
  geom_errorbarh(aes(y = 13.434, 
                     xmin = -14.513 - -2.045, xmax = -14.513 + -2.045), cex =1, lty = 2, colour = "#C71000B2")
LB2



fig1 <- LB2 + theme(text = element_text(size = 20))
fig1
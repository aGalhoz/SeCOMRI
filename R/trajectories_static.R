# read hospital map
m <- png::readPNG("./translatum.png")

# 1: staff and patients (union) 
ggplot(data_staff_COVID_static_plot_new, aes(x, y,color = delta.fraction)) + 
  annotation_custom(xmin=1, ymin=1, xmax=2, ymax=2,    
                    rasterGrob(m,width = unit(1,"npc"), 
                               height = unit(1,"npc")))+
  geom_point(aes(size = abs(delta.fraction)),show.legend = T,alpha = 0.7) +
  scale_size_continuous(range = c(4.7, 17),name = "|Delta|",breaks = c(0.01,0.05,0.10),
                        labels = c("1%","5%","10%"),limits = c(0,0.12)) +
  scale_colour_gradient2(low = "blue",mid= "palegreen3",high = "red",midpoint = 0,
                         breaks = c(min(data_staff_COVID_static_plot_new$delta.fraction),0,
                                    max(data_staff_COVID_static_plot_new$delta.fraction)),
                         labels= c("Max. %COVID19 staff \n(-11.6%)",0,"Max. %COVID19 patients \n(10.3%)")) +
  scale_fill_distiller(palette=4, direction=1) +
  labs(title = "Overall union of staff and COVID patient distribution in hospital throughout the study",
       colour = "Delta = %COVID19 patients - %COVID19 staff") +
  xlim(1,2) +
  ylim(1,2) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom",legend.box="vertical") +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5,barwidth = 17,order = 1),
         size = guide_legend(title.position="top", title.hjust = 0.5,order = 2))

# 2: staff  
ggplot(data_staff_COVID_static_plot_new %>% filter(fraction.staff>0), aes(x, y)) + 
  annotation_custom(xmin=1, ymin=1, xmax=2, ymax=2,    
                    rasterGrob(m,width = unit(1,"npc"), 
                               height = unit(1,"npc")))+
  geom_point(aes(size = fraction.staff,colour=fraction.staff),alpha = 0.7)+
  scale_size_continuous(range = c(7.2, 18.5),name = "",breaks = c(0.04,0.08,0.12,0.16),
                        labels = c("4%","8%","12%","16%"))  +
  scale_colour_gradientn(colors = RColorBrewer::brewer.pal(9, "Blues")[4:9],
                         breaks = c(0.01,
                                    max(data_staff_COVID_static_plot_new$fraction.staff)),
                         labels=c("Minimum \n(0%)","Maximum \n(16.8%)")) +
  labs(title = "Overall staff distribution in hospital throughout the study",
       color = "Relative number of COVID19 staff") +
  xlim(1,2) +
  ylim(1,2) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom",legend.box="vertical",
        legend.margin = margin(10, 0, 0, 0)) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5,barwidth = 17,order = 1),
         size = guide_legend(title.position="top", title.hjust = 0.5,order = 2))

# 3: patients
ggplot(data_staff_COVID_static_plot_new %>% filter(fraction.patient>0), 
       aes(x, y,color = fraction.patient)) + 
  annotation_custom(xmin=1, ymin=1, xmax=2, ymax=2,    
                    rasterGrob(m,width = unit(1,"npc"), 
                               height = unit(1,"npc")))+
  geom_point(aes(size = fraction.patient),show.legend = T,alpha=0.7) +
  scale_size_continuous(range = c(5, 19.5),name = "",breaks = c(0.04,0.08,0.12,0.16),
                        labels = c("4%","8%","12%","16%"))  +
  scale_colour_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds")[3:9],
                         breaks = c(0.0015,max(data_staff_COVID_static_plot_new$fraction.patient)),
                         labels = c("Minimum \n(0%)","Maximum \n(16.2%)")) +
  #geom_tile(alpha=0.55) +
  labs(title = "Overall COVID patients distribution in hospital throughout the study",
       color = "Relative number of COVID patients") +
  xlim(1,2) +
  ylim(1,2) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom",legend.box="vertical",
        legend.margin = margin(10, 0, 0, 0)) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5,barwidth = 17,order = 1),
         size = guide_legend(title.position="top", title.hjust = 0.5,order = 2)) 

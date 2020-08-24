# read hospital map
m <- png::readPNG("./translatum.png")

# 1: staff and patients (union)
data_staff_COVID_timeline_plot_new <- data_staff_COVID_timeline_plot_new[!is.na(data_staff_COVID_timeline_plot_new$x),]
data_staff_COVID_timeline_plot_new$date <- as.Date(data_staff_COVID_timeline_plot_new$date,format = "%d-%m")
date.sequence <- na.omit(unique(data_staff_COVID_timeline_plot_new$date))
timeline.sequence.data <- list()

for (i in 1:length(date.sequence)) {
  data_staff_COVID_timeline_plot_now <- data.frame(data_staff_COVID_timeline_plot_new[data_staff_COVID_timeline_plot_new$date==date.sequence[i],])
  timeline.sequence.data[[i]] <- data_staff_COVID_timeline_plot_now
}
timeline.sequence.data <- bind_rows(timeline.sequence.data)

p <- ggplot(timeline.sequence.data, aes(x, y,color = delta.fraction)) + 
  annotation_custom(xmin=1, ymin=1, xmax=2, ymax=2,    
                    rasterGrob(m,width = unit(1,"npc"), 
                               height = unit(1,"npc")))+
  geom_point(aes(size = abs(delta.fraction)),show.legend = T,alpha = 0.7) +
  scale_size_continuous(range = c(10, 17),name = "|Delta|",breaks = c(0.0025,0.005,0.009),
                        labels = c("0.25%","0,5%","0,9%")) +
  scale_colour_gradientn(colours = c("blue","palegreen3", "red"),
                        values = rescale(c(min(data_staff_COVID_timeline_plot_new$delta.fraction),0,
                                     max(data_staff_COVID_timeline_plot_new$delta.fraction))),
                         breaks = c(min(data_staff_COVID_timeline_plot_new$delta.fraction),0,
                                    max(data_staff_COVID_timeline_plot_new$delta.fraction)),
                         labels=c("Max. %COVID19 staff",0,"Max. %COVID19 patients"),
                         limits = c(min(data_staff_COVID_timeline_plot_new$delta.fraction),
                                    max(data_staff_COVID_timeline_plot_new$delta.fraction))) +
  #geom_tile(alpha=0.55) +
  scale_fill_distiller(palette=4, direction=1) +
  labs(colour = "Delta = %COVID19 patients - %COVID19 staff") +
  xlim(1,2) +
  ylim(1,2) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom",legend.box="vertical",
        legend.margin = margin(10, 0, 0, 0)) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5,barwidth = 20,order = 1),
         size = guide_legend(title.position="top", title.hjust = 0.5,order = 2)) 

staff_animation <- p + transition_manual(date) + labs(title = 'Date: {current_frame}')
date.sequence <- na.omit(unique(data_staff_COVID_timeline_plot_new$date))
staff_animation <- animate(
  plot = staff_animation, 
  nframes = length(date.sequence), 
  fps = 4, 
  end_pause = 8
)

anim_save("staff_trajectories_timeline_staff_patients.gif",staff_animation)

# 2: staff 
p <- ggplot(data_staff_timeCOVID_timeline_plot, aes(x, y,color = fraction.staff)) + 
  annotation_custom(xmin=1, ymin=1, xmax=2, ymax=2,    
                    rasterGrob(m,width = unit(1,"npc"), 
                               height = unit(1,"npc")))+
  geom_point(aes(size = abs(fraction.staff)),show.legend = T,alpha = 0.7) +
  scale_size_continuous(range = c(7, 15),name = "|Delta|",breaks = c(0.001,0.002,0.004),
                        labels = c("0,1%","0,2%","0,4%")) +
  scale_colour_gradientn(colors = RColorBrewer::brewer.pal(9, "Blues")[4:9],
                         breaks = c(0,
                                    max(data_staff_timeCOVID_timeline_plot$fraction.staff)),
                         labels=c("Minimum","Maximum"))  +
  scale_fill_distiller(palette=4, direction=1) +
  labs(colour = "Relative number of infected COVID19 staff") +
  xlim(1,2) +
  ylim(1,2) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom",legend.box="vertical",
        legend.margin = margin(10, 0, 0, 0)) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5,barwidth = 20,order = 1),
         size = guide_legend(title.position="top", title.hjust = 0.5,order = 2)) 

staff_animation <- p + transition_manual(date) + labs(title = 'Date: {current_frame}')
date.sequence <- unique(data_staff_timeCOVID_timeline_plot$date)
staff_animation <- animate(
  plot = staff_animation, 
  nframes = length(date.sequence), 
  fps = 4, 
  end_pause = 8
)

anim_save("staff_trajectories_timeline_COVIDpositive_v2.gif",staff_animation)

# 3: patients
p <- ggplot(timeline.sequence.data %>% filter(fraction.patient>0), aes(x, y,color = fraction.patient)) + 
  annotation_custom(xmin=1, ymin=1, xmax=2, ymax=2,    
                    rasterGrob(m,width = unit(1,"npc"), 
                               height = unit(1,"npc")))+
  geom_point(aes(size = abs(fraction.patient)),show.legend = T,alpha = 0.7) +
  scale_size_continuous(range = c(10, 17),name = "|Delta|",breaks = c(0.0025,0.005,0.009),
                        labels = c("0,25%","0,5%","0,9%")) +
  scale_colour_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds")[3:9],
                         breaks = c(0,
                                    max(data_staff_COVID_timeline_plot_new$fraction.patient)),
                         labels=c("Minimum","Maximum"))  +
  #geom_tile(alpha=0.55) +
  scale_fill_distiller(palette=4, direction=1) +
  labs(colour = "Relative number of COVID19 patient") +
  xlim(1,2) +
  ylim(1,2) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), legend.position = "bottom",legend.box="vertical",
        legend.margin = margin(10, 0, 0, 0)) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5,barwidth = 20,order = 1),
         size = guide_legend(title.position="top", title.hjust = 0.5,order = 2)) 

staff_animation <- p + transition_manual(date) + labs(title = 'Date: {current_frame}')
date.sequence <- unique(data_staff_COVID_timeline_plot_new[data_staff_COVID_timeline_plot_new$freq.patient.per.area>0,]$date)
staff_animation <- animate(
  plot = staff_animation, 
  nframes = length(date.sequence), 
  fps = 4, 
  end_pause = 8
)

anim_save("patient_trajectories_timeline_v2.gif",staff_animation)

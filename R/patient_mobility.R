# COVID positive vs non-positive patients 

COVID_vs_nonCOVID.per.ward <- merge(Patient.per.ward,
                                    nonCOVIDPatient.per.ward,
                                    by.x = c("Beh. OE","COVID status"),by.y = c("ORGPF","COVID status"))
COVID_vs_nonCOVID.per.ward <- COVID_vs_nonCOVID.per.ward %>%
  mutate(diff.COVID.nonCOVID = fraction.patient-fraction.nonCOVIDpatient) %>% distinct()

lims <- c(0, 
          max(max(COVID_vs_nonCOVID.per.ward$fraction.patient),max(COVID_vs_nonCOVID.per.ward$fraction.nonCOVIDpatient)))

ggplot(COVID_vs_nonCOVID.per.ward,aes(x = fraction.patient,y=fraction.nonCOVIDpatient)) +
  geom_point(colour = "grey",size = 4,alpha = 0.7) + 
  geom_point(data = COVID_vs_nonCOVID.per.ward %>% filter(diff.COVID.nonCOVID>0.001 & fraction.patient>0.025),
             aes(colour = "Higher in COVID-19 patients"),size = 4,alpha = 0.7) + 
  geom_point(data = COVID_vs_nonCOVID.per.ward %>% 
               filter(diff.COVID.nonCOVID<(-0.001) & fraction.nonCOVIDpatient>0.025),
             aes(colour = "Higher in non-COVID-19 patients"),size = 4,alpha = 0.7) + 
  geom_text(data = COVID_vs_nonCOVID.per.ward %>% 
              filter((diff.COVID.nonCOVID<(-0.001) & fraction.nonCOVIDpatient>0.025) |
                       (diff.COVID.nonCOVID>0.001 & fraction.patient>0.025)),
            aes(x = fraction.patient-0.001,y=fraction.nonCOVIDpatient+0.002,label = `Beh. OE`),
            color = "grey50") +
  geom_abline(colour = "grey",alpha = 0.8,type="l", lty=2) +
  scale_color_manual(values = c("firebrick2","darkolivegreen3")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),plot.background = element_blank()) + 
  labs(title = "Beh. OE between COVID-19 vs non-COVID-19 patients",
       color = "Relative Frequency of \nTreatments and Locations") +
  scale_x_continuous("Relative frequency for COVID-19 patients", limits = lims) +
  scale_y_continuous("Relative frequency for non-COVID-19 patients", limits = lims) +
  theme(aspect.ratio = 1) +theme_classic()

### Figures of a study on baseline mortality in Norwegian salmon farming ###
# Author(s): Victor H. S. Oliveira, Katharine R. Dean, Lars Qviller, Carsten Kirkeby and Britt Bang Jensen

# Loading required packages
library(sf)
library(tmap)
library(leaflet)
library(gridExtra)

# Figure 1: map with study population

Coastline <- read_sf(dsn="path", 
                    layer="Hav_norge") 
Norway <- read_sf(dsn="path", 
                 layer="Norge_N5000")
Zones <- readRDS("path")

pop_noExc_map <- st_as_sf(pop_noExc %>%
                            group_by(location) %>%
                            slice(1) %>%
                            ungroup() %>%
                            filter(!is.na(n_geowgs84)) %>%
                            mutate(dot = "Salmon farm") %>%
                            select(location, dot, e_geowgs84, n_geowgs84),
                          coords=c("e_geowgs84", "n_geowgs84"), crs=4326)

palette_map <- c("Salmon farm"= "#009E73")

tmap_mode("plot")
map = tm_shape(Coastline) +
  tm_fill() +
  tm_shape(Norway) +
  tm_borders(lwd=1) +
  tm_shape(Zones) +
  tm_borders(lty = 2) +
  tm_shape(pop_noExc_map) +
  tm_dots(title = "",
          shape = 21,
          size = 0.15,
          col = "dot",
          palette= palette_map) +
  tm_legend(position = c("right", "center"), text.size=1) +
  tm_compass(type="radar",
             size=3,
             position=c("left", "top")) + 
  tm_scale_bar(breaks = c(0, 100, 200, 300), text.size=.7)
map

#Figure 3
dens <- density(dfanalysis$mort.rate*1000)
df_dens <- data.frame(x=dens$x, y=dens$y)
perc <- c(0.25, 0.5, 0.75)
quantiles <- quantile(dfanalysis$mort.rate*1000, prob=perc)
df_dens$quant <- factor(findInterval(df_dens$x,quantiles), levels = c("0", "1", "2", "3"),
                   labels = c("0-25%", "25.1-50%", "50.1-75%", "75.1-100%"))

ggplot(df_dens, aes(x,y)) +
  geom_line() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) +
  scale_fill_brewer(palette = "OrRd") +
  scale_x_continuous(limits = c(0,22), breaks=seq(0, 22, by=2.5), name="Mortality (deaths per 1,000 fish-months)", expand = c(0,0)) +
  scale_y_continuous(limits = c(0,0.22), breaks = seq(0, 0.2, by=0.05), name="Density", expand = c(0,0)) +
  theme_classic() +
  labs(fill= "Quantile") +
  geom_vline(xintercept= median(dfanalysis$mort.rate*1000), size = 1) +
  geom_vline(xintercept= mean(dfanalysis$mort.rate*1000), linetype="dashed", size = 1) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.justification=c(1,1),legend.position=c(1,1))

# figure 4: plot with descriptives of continuous explanatory variables

# Temperature
p1 <- ggplot (dfanalysis,  aes (temp, mort.rate*1000)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", color = "#D55E00", se = F, size = 2) +
  scale_x_continuous(name="Sea surface Temperature (°C)",
                     limits = c(1,21), 
                     breaks=seq(1, 21, by=5),
                     expand = c(0,0)) +
  scale_y_continuous(name="Deaths per 1,000 fish-months",
                     limits=c(0, 21), 
                     breaks = seq(0,21,5), 
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) 
p1

## Salinity
p2 <- ggplot (dfanalysis,  aes (salinity, mort.rate*1000)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", color = "#D55E00", se = F, size = 2) +
  scale_x_continuous(name="Sea surface salinity (‰)",
                     limits = c(3,35),
                     breaks=seq(3, 34, by=5), 
                     expand = c(0,0)) +
  scale_y_continuous(name="Deaths per 1,000 fish-months",
                     limits=c(0, 21),
                     breaks = seq(0,21,5),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_blank()) 
p2

# Weight upon stocking at sea
p3 <- ggplot (dfanalysis,  aes (w.start.fish, mort.rate*1000)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", color = "#D55E00", se = F, size = 2) +
  scale_x_continuous(name="Weight upon stocking at sea (g)", 
                     limits = c(40, 505), 
                     breaks = c(40, 100, 200, 300, 400, 500),
                     expand = c(0,0)) +
  scale_y_continuous(name="Deaths per 1,000 fish-months",
                     limits=c(0, 21), 
                     breaks = seq(0,21,5), 
                     expand = c(0,0))  +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_blank())

p3

# Fish weight
p4 <- ggplot (dfanalysis,  aes (w.fish, mort.rate*1000)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", color = "#D55E00", se = F, size = 2) +
  scale_x_continuous(name="Fish weight (g)",
                     limits = c(50, 6000), 
                     breaks = c(50, 1000, 2000, 3000, 4000, 5000),
                     expand = c(0,0)) +
  scale_y_continuous(name="Deaths per 1,000 fish-months", 
                     limits=c(0, 21), 
                     breaks = seq(0,21,5), 
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) 
p4

# local biomass density

# function for scientific notation in the plots
sci <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

p5 <- ggplot (dfanalysis,  aes (lbd, mort.rate*1000)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", color = "#D55E00", se = F, size = 2) +
  scale_x_continuous(name="Local biomass density*",
                     limits = c(5*10^5,2.4*10^9), 
                     breaks= c(5*10^5, 5*10^8, 1*10^9, 1.5*10^9, 2*10^9), 
                     expand = c(0,0), labels = sci) +
  scale_y_continuous(name="Deaths per 1,000 fish-months",
                     limits=c(0, 21),
                     breaks = seq(0,21,5),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_blank()) 
p5

# Sea lice count
p6 <- ggplot (dfanalysis,  aes (lice.count, mort.rate*1000)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess", color = "#D55E00", se = F, size = 2) +
  scale_x_continuous(name="Sea lice count",
                     limits = c(0, 9),
                     breaks = seq(0, 9, by=2),
                     expand = c(0,0)) +
  scale_y_continuous(name="Deaths per 1,000 fish-months", 
                     limits=c(0,21), 
                     breaks = seq(0,21,5),
                     expand = c(0,0)) +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_blank()) 
p6

p1_3 <- grid.arrange(p1, p2, p3, ncol = 3)

p4_6 <- grid.arrange(p4, p5, p6, ncol = 3)

p1_6 <-  grid.arrange(p1_3, p4_6, nrow = 2)

p1_6

# Figure 5: Plot with mortality rate ratios.

summary(m)

plotMRR <- data.frame(
  rbind(round(exp(confint(m)),4),
        # for each additional 5ppt increase in salinity and each additional 50g increase in the start weight
        rbind(round(exp(confint(m)[4, 1:3]*5),4),
              round(exp(confint(m)[27, 1:3]*50),4))
  )
) 

plotMRR$var <- rownames(plotMRR) 
plotMRR$var[plotMRR$var == "X"] <- "salinity5"
plotMRR$var[plotMRR$var == "X.1"] <- "w.start.fish50"

plotMRR <- data.frame(plotMRR [c(33, 5:15, 34, 18:26, 28:31), ])

plotMRR$var <- factor (plotMRR$var,
                       levels = c("salinity5", "cond.zone1", "cond.zone2", "cond.zone3", "cond.zone4", "cond.zone5",
                                  "cond.zone6", "cond.zone7", "cond.zone8", "cond.zone9", "cond.zone11",           
                                  "cond.zone12.13", "w.start.fish50", "cond.start.mthMar","cond.start.mthApr", 
                                  "cond.start.mthMay", "cond.start.mthJun", "cond.start.mthJul", 
                                  "cond.start.mthAug", "cond.start.mthOct", "cond.start.mthNov",
                                  "cond.start.mthWinter",  "cond.med.treat1", "cond.med.treat..2",
                                  "cond.nonmed.treat1", "cond.nonmed.treat..2"
                       ),
                       labels = c("Sea surface salinity, for additional 5 ‰",
                                  "Stocking at sea in production zone 1 vs zone 10",
                                  "Stocking at sea in production zone 2 vs zone 10",
                                  "Stocking at sea in production zone 3 vs zone 10",
                                  "Stocking at sea in production zone 4 vs zone 10",
                                  "Stocking at sea in production zone 5 vs zone 10",
                                  "Stocking at sea in production zone 6 vs zone 10",
                                  "Stocking at sea in production zone 7 vs zone 10",
                                  "Stocking at sea in production zone 8 vs zone 10",
                                  "Stocking at sea in production zone 9 vs zone 10",
                                  "Stocking at sea in production zone 11 vs zone 10",
                                  "Stocking at sea in production zone 12 & 13 vs zone 10",
                                  "Weight of upon stocking at sea, for additional 50 g",
                                  "First stocking at sea in March vs September",
                                  "First stocking at sea in April vs September",
                                  "First stocking at sea in May vs September",
                                  "First stocking at sea in June vs September",
                                  "First stocking at sea in July vs September",
                                  "First stocking at sea in August vs September",
                                  "First stocking at sea in October vs September",
                                  "First stocking at sea in November vs September",
                                  "First stocking at sea in Dec/Jan/Feb vs September",
                                  "1 H2O2/medicinal treatment per month vs none",
                                  "2 or more H2O2/medicinal treatments per month vs none",
                                  "1 non-medicinal treatment per month vs none",
                                  "2 or more non-medicinal treatments per month vs none"),
) 

levels(plotMRR$var)

plotMRR$group <- c("s","s","s","s","s","s","s","ns","ns","ns","s","s",
                   "s","s","s","s", "s","s","ns","ns","s",
                   "s","s","s","s","s")

plotMRR$var <- factor (plotMRR$var, levels=rev(plotMRR$var[order(plotMRR$var)]))

ggplot(plotMRR, aes(x=var, y=Estimate, group = group, color = group)) + 
  geom_pointrange(aes(ymin=X2.5.., ymax=X97.5..),
                  color=ifelse(plotMRR$group == "ns","#fbb4b9","#d7301f"),
                  shape=16, size=1) +
  geom_text(aes(label = round(Estimate, 3)),
            vjust = 1.3, hjust = 1.1,
            show.legend = FALSE, color = "black") +
  coord_flip() + 
  geom_hline(yintercept = 1 ,linetype= "dashed") +
  scale_y_continuous(name="Mortality rate ratio (95% CI)", breaks = seq (0.8, 2.5, 0.2)) +
  theme_bw() + 
  theme(plot.background = element_blank(),
        panel.grid.major.y  =  element_line(size=.1, color="#f0f0f0" ),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank() ,
        panel.background = element_blank()) +
  theme(axis.line = element_line(color = 'black')) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size=12, face = "bold"))

# Figure 6 and 7: Mortality predictions based on generated values for sea surface temperature (temp), weight and sea lice treatments.

# We refer to the following vignette for plots with predictions of mixed effects logistic regressions
# https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/

# df for predictions
df_pred <- dfanalysis[, c("d.count","temp", "salinity", "zone", "w.fish", "start.mth", "w.start.fish", 
                          "med.treat", "nonmed.treat", "location", "ar.count")]

# Setting the fish at risk at 1000
df_pred$ar.count <- 1000

# Mortality predictions based on sst

# Generating 100 values for temp predictions within the range of observed temp.
temp <- with(df_pred, seq(from = min(temp), to = max(temp), length.out = 100))

# Making the predictions for each month using the 100 temp values 
pred_Deaths <- lapply(temp, function(t) {
  df_pred$temp <- t
  predict(m, newdata = data.frame(df_pred), type = "response")
})

# Creating df for plotting
temp_Plot <- t(sapply(pred_Deaths, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))

temp_Plot <- as.data.frame(cbind(temp_Plot, temp))

colnames(temp_Plot) <- c("pred_Deaths", "Q1", "Q3", "temp")
temp_Plot

#figure 6: plot with temp predictions
fig5a <- ggplot(temp_Plot, aes(x = temp, y = pred_Deaths)) +
  geom_ribbon (aes (ymin = Q1, ymax = Q3, fill = "y"), alpha = 0.15) +
  geom_line(size = 2, color = "#000000") + 
  scale_fill_manual(values= c("#000000")) + 
  scale_x_continuous(name="Sea surface temperature (°C)", limits = c(1,20), breaks= c(1, 5, 10, 15, 20)) +
  scale_y_continuous(name="Mortality (deaths per 1,000 fish-months)", limits=c(0, 13), breaks = seq(0,13,2.5)) +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        legend.position = "none")

# Mortality predictions based on fish weight and sea lice treatments results

# Generating 100 values for fish weight predictions within the range of observed weight. 
w.fish <- with(df_pred, seq (from = min(df_pred$w.fish), to = max(df_pred$w.fish), length.out = 100))

pred_Deaths <- lapply(w.fish, function(w) {
  df_pred$w.fish <- w
  predict(m, newdata = data.frame(df_pred), type = "response")
})

w_Plot <- t(sapply(pred_Deaths, function(x) {
  c(M = mean(x), quantile(x, c(0.25, 0.75)))
}))

w_Plot <- as.data.frame(cbind(w_Plot, w.fish))

colnames(w_Plot) <- c("Pred_Deaths", "Q1", "Q3", "w.fish")

# Plot with weight predictions
ggplot(w_Plot, aes(x = w.fish, y = Pred_Deaths)) +
  geom_linerange(aes(ymin = Q1, ymax = Q3)) +
  geom_line(size = 2) + ylim(c(0, 25))

# Non-medicinal treatments predictions  
pred_Deaths_nm <- lapply(levels(df_pred$nonmed.treat), function(nm) {
  df_pred$nonmed.treat[] <- nm
  lapply(w.fish, function(w) {
    df_pred$w.fish <- w
    predict(m, newdata = data.frame(df_pred), type = "response")
  })
})

nm_Plot <- lapply(pred_Deaths_nm, function(X) {
  w.fish.P <- t(sapply(X, function(x) {
    c(M=mean(x), quantile(x, c(.25, .75)))
  }))
  w.fish.P <- as.data.frame(cbind(w.fish.P, w.fish))
  colnames(w.fish.P) <- c("Pred_Deaths", "Q1", "Q3", "w.fish")
  return(w.fish.P)
})

nm_Plot <- do.call(rbind, nm_Plot)

nm_Plot$nonmed.treat <- factor(rep(levels(df_pred$nonmed.treat), each = length(w.fish)))
nm_Plot$nonmed.treat <- factor(nm_Plot$nonmed.treat, levels = c("0", "1", ">=2"))

# Plot with non-medicinal treatments and fish weight. 

color_list <- c("#fdcc8a", "#fc8d59", "#e34a33")

ggplot(nm_Plot, aes(x = w.fish, y = Pred_Deaths)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = nonmed.treat), alpha = .15) +
  geom_line(aes(colour = nonmed.treat), size = 2) +
  scale_fill_manual(values=color_list) + scale_color_manual(values=color_list) +
  ylim(c(0, 25)) +
  xlim(c(50, 6000)) +
  facet_wrap(~nonmed.treat) 

# Same steps for medicinal + H2O2 plots
pred_Deaths_med <- lapply(levels(df_pred$med.treat), function(med) {
  df_pred$med.treat[] <- med
  lapply(w.fish, function(w) {
    df_pred$w.fish <- w
    predict(m, newdata = data.frame(df_pred), type = "response")
  })
})

med_Plot <- lapply(pred_Deaths_med, function(X) {
  w.fish.P <- t(sapply(X, function(x) {
    c(M=mean(x), quantile(x, c(.25, .75)))
  }))
  w.fish.P <- as.data.frame(cbind(w.fish.P, w.fish))
  colnames(w.fish.P) <- c("Pred_Deaths", "Q1", "Q3", "w.fish")
  return(w.fish.P)
})

med_Plot <- do.call(rbind, med_Plot)

med_Plot$med.treat <- factor(rep(levels(df_pred$med.treat), each = length(w.fish)))
med_Plot$med.treat <- factor(med_Plot$med.treat, levels = c("0", "1", ">=2"))

# Figure 7 

color_list <- c("#fdcc8a", "#fc8d59", "#e34a33")

ggplot(med_Plot %>% filter(med.treat != "0"), aes(x = w.fish, y = Pred_Deaths)) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3, fill = med.treat), alpha = .15) +
  geom_line(aes(colour = med.treat), size = 2) +
  scale_fill_manual(values=color_list) + scale_color_manual(values=color_list) +
  ylim(c(0, 25)) +
  xlim(c(50, 6000)) +
  facet_wrap(~med.treat) 

names(nm_Plot)[names(nm_Plot) == "nonmed.treat"] <- "treat"
nm_Plot$Type <- "Non-medicinal"
names(med_Plot)[names(med_Plot) == "med.treat"] <- "treat"
med_Plot$Type <- "H2O2/medicinal" 
treat_plot <- rbind (nm_Plot, med_Plot)
treat_plot$Type <- as.factor (treat_plot$Type)
treat_plot$treat <- factor (treat_plot$treat, labels = c("0", "1", ">= 2"))
treat_plot$Type <- relevel (treat_plot$Type, ref = "Non-medicinal")

ggplot(treat_plot %>% filter(treat != "0"), aes(x = w.fish, y = Pred_Deaths, fill = Type)) +
  facet_grid(. ~ treat) +
  geom_ribbon(aes(ymin = Q1, ymax = Q3), alpha = .15, show.legend = FALSE) +
  geom_line(aes(colour = Type, linetype = Type), size = 2) +
  scale_fill_manual(values= c("#e34a33", "#fdcc8a")) + 
  scale_color_manual(values= c("#e34a33", "#fdcc8a")) +
  scale_x_continuous(name="Fish weight (g)", limits=c(50, 6000), breaks = c(50, 1000, 2000, 3000, 4000, 5000)) +
  scale_y_continuous(name="Mortality (deaths per 1,000 fish-months)", limits=c(1, 22), breaks = seq(0,20,5)) +
  theme_classic() + 
  theme (strip.background = element_rect(color = "white", fill = "#f0f0f7", size = .6, linetype ="solid")) +
  theme (strip.text.x = element_text(size = 12)) +
  theme(panel.background = element_rect(fill = 'white', color ="#f0f0f7" )) +
  theme(panel.grid.major.x =  element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(title="Number of monthly sea lice treatments") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(size=20),
        axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  theme(legend.position="bottom")

# Figure S1 diagnostic plots

par(mfrow=c(3,3))

plotResiduals(res, rank = T)
plotResiduals(res, dfanalysis$temp)
plotResiduals(res, dfanalysis$salinity)
plotResiduals(res, dfanalysis$w.start.fish)
plotResiduals(res, dfanalysis$start.mth)
plotResiduals(res, dfanalysis$w.fish)
plotResiduals(res, dfanalysis$zone)
plotResiduals(res, dfanalysis$med.treat)
plotResiduals(res, dfanalysis$nonmed.treat)

## load libraries
library(data.table)
library(ggplot2)
library(gridExtra)

## load data
aa <- fread("output/caribou-FO-move.csv")
aa$Herd <- "Fogo"
bb <- fread("output/caribou-NL-move.csv")
bb$Herd <- "Middle Ridge"
cc <- fread("output/coyote-all-move.csv")
cc <- cc[,2:4]
cc$Predator <- "Coyote"
dd <- fread("output/bear-all-move.csv")
dd <- dd[,2:4]
dd$Predator <- "Bear"

#### FIGURE 1 #####

spring <- rbind(aa[season == "Spring"],
								bb[season == "Spring"])

winter <- rbind(aa[season == "Winter" & hr != 12],
								bb[season == "Winter"])

springPred <- rbind(cc[season == "Spring"],
								dd[season == "Spring"])

a <- ggplot() +
	geom_smooth(data = spring,
							aes(hr, V1, color = Herd,
									lty = Herd),
							method = "gam") +
	xlab("Hour of day") +
	ylim(-20, 300) +
	ylab("Predicted movement rate (m/hr)") +
	scale_color_manual(values = c("#e34a33", "#e34a33")) +
	geom_vline(aes(xintercept = 6), lty = 2, color = "black") +
	geom_vline(aes(xintercept = 21), lty = 2, color = "black") +
	ggtitle("A) Caribou (spring)") +
	theme(legend.position = c(0.78, 0.15),
		    legend.key = element_blank(),
	    	#legend.background=element_blank(),
				axis.title = element_text(size = 16, color = 'black'),
				axis.text.x = element_text(size = 12, color = 'black'),
				axis.text.y = element_text(size = 12, color = 'black'),
				panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				panel.border = element_rect(colour = "black", fill=NA, size = 1))


b <- ggplot() +
	geom_smooth(data = winter,
							aes(hr, V1, color = Herd,
									lty = Herd),
							method = "gam") +
	xlab("Hour of day") +
	ylim(-20, 300) +
	ylab("Predicted movement rate (m/hr)") +
	scale_color_manual(values = c("#2b8cbe","#2b8cbe")) +
	geom_vline(aes(xintercept = 7.5), lty = 2, color = "#2b8cbe") +
	geom_vline(aes(xintercept = 17.5), lty = 2, color = "#2b8cbe") +
	ggtitle("B) Caribou (winter)") +
	theme(legend.position = c(0.78, 0.15),
				strip.background = element_rect(color = "black",
																				fill = "white",
																				size = 1),
				axis.title = element_text(size = 16, color = 'black'),
				axis.text.x = element_text(size = 12, color = 'black'),
				axis.text.y = element_text(size = 12, color = 'black'),
				panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				panel.border = element_rect(colour = "black", fill=NA, size = 1))


c <- ggplot(data = springPred[season == "Spring"],
						aes(hr, V1, color = Predator, lty = Predator)) +
	geom_smooth(method = "gam") +
	xlab("Hour of day") +
	ylim(0, 600) +
	ylab("Predicted movement rate (m/hr)") +
	scale_color_manual(values = c("#e34a33", "#e34a33")) +
	scale_linetype_manual(values = c(3,4)) +
	geom_vline(aes(xintercept = 6), lty = 2, color = "black") +
	geom_vline(aes(xintercept = 21), lty = 2, color = "black") +
	ggtitle("C) Coyote and bear (spring)") +
	theme(legend.position = c(0.78, 0.15),
				strip.background = element_rect(color = "black",
																				fill = "white",
																				size = 1),
				axis.title = element_text(size = 16, color = 'black'),
				axis.text.x = element_text(size = 12, color = 'black'),
				axis.text.y = element_text(size = 12, color = 'black'),
				panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				panel.border = element_rect(colour = "black", fill=NA, size = 1))

d <- ggplot(cc[season == "Winter"],
						aes(hr, V1, color = season)) +
	geom_smooth(method = "gam") +
	xlab("Hour of day") +
	ylim(200, 600) +
	ylab("Predicted movement rate (m/hr)") +
	scale_color_manual(values = c("#2b8cbe")) +
	geom_vline(aes(xintercept = 7.5), lty = 2, color = "black") +
	geom_vline(aes(xintercept = 17.5), lty = 2, color = "black") +
	ggtitle("D) Coyote (winter)") +
	theme(legend.position = "none",
				strip.background = element_rect(color = "black",
																				fill = "white",
																				size = 1),
				axis.title = element_text(size = 16, color = 'black'),
				axis.text.x = element_text(size = 12, color = 'black'),
				axis.text.y = element_text(size = 12, color = 'black'),
				panel.grid.minor = element_blank(),
				panel.background = element_blank(),
				panel.border = element_rect(colour = "black", fill=NA, size = 1))


png("figures/fig1.png",
		width = 5000,
		height = 5000,
		res = 600,
		units = "px")

gridExtra::grid.arrange(a, b, c, d)

dev.off()

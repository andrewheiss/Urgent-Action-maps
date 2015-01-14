#-----------------
# Load libraries
#-----------------
library(dplyr)
library(magrittr)
library(ggplot2)
library(XLConnect)
library(countrycode)
library(grid)
library(Cairo)

# Map stuff
# You must install geos (http://trac.osgeo.org/geos/) and 
# gdal (http://www.gdal.org/) first. 
# Easy to do on OS X: `brew install geos gdal`
# Then install these packages from source
# install.packages(c("rgeos", "rgdal"), type="source")
library(rgeos)
library(maptools)


# Useful functions
clean.levels <- function(x) {
  separated <- unlist(strsplit(x, ","))
  
  clean.numbers <- function(y) {
    if(grepl("\\(", y)) {
      cleaned <- gsub("\\(", "", y)
      return(as.numeric(cleaned) + 1)
    } else {
      cleaned <- gsub("\\[|\\]", "", y)
      return(as.numeric(cleaned))
    }
  }
  
  return(paste(sapply(separated, FUN=clean.numbers), collapse="-"))
}

make.prob.labels <- function(x) {
  out <- NULL
  for(i in 1:(length(x) - 1)) {
    out <- c(out, paste("(", x[i] * 100, "-", x[i + 1] * 100, "%)", sep=""))
  }
  out
}

theme_blank_map <- function(base_size=12, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border=element_blank(), axis.line=element_blank(),
          panel.grid=element_blank(), axis.ticks=element_blank(),
          axis.title=element_blank(), axis.text=element_blank(),
          legend.text=element_text(size=rel(0.7), family="Source Sans Pro Light"),
          legend.title=element_text(size=rel(0.9), family="Source Sans Pro Semibold"))
  ret
}

theme_clean <- function(base_size=12, base_family="Source Sans Pro Light") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.x=element_text(vjust=-0.2), axis.title.y=element_text(vjust=1.5),
          title=element_text(vjust=1.2, family="Source Sans Pro Semibold"),
          panel.border = element_blank(), axis.line=element_blank(),
          panel.grid=element_blank(), axis.ticks=element_blank(),
          legend.position="none", 
          axis.title=element_text(size=rel(0.8), family="Source Sans Pro Semibold"),
          strip.text=element_text(size=rel(1), family="Source Sans Pro Semibold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.margin.y=unit(1.5, "lines"))
  ret
}


#------------
# Load data
#------------
# Load urgent action data
uas.workbook <- loadWorkbook("Urgent Actions by Country.13Jan14.xlsx")
uas <- readWorksheet(uas.workbook, sheet="Final") %>%
  mutate(id = countrycode(Country, "country.name", "iso3c")) %>%
  rename(uas = Number.of.UAs) %>% select(-Country)

# Load map information
world.map <- readShapeSpatial("map_data/ne_50m_admin_0_countries.shp")
world.ggmap <- ggplot2::fortify(world.map, region = "iso_a3")

# Make a data frame of all potential countries
all.countries <- data_frame(id = as.character(world.map$iso_a3)) %>%
  filter(id != "-99")

# Merge UAs with all countries (so that countries not included in the 
# urgent action data still get mapped)
uas.full <- all.countries %>% left_join(uas, by="id") %>%
  group_by(id) %>% summarize(uas = sum(uas)) %>%  # Aggregate duplicate rows
  mutate(country = countrycode(id, "iso3c", "country.name")) %>%
  filter(id != "ATA")# Get rid of Antarctica

# Quantile stuff
cut.probs <- c(seq(0, 0.75, 0.25), 0.95, 1)
cut.points <- quantile(uas.full$uas, na.rm=TRUE, probs=cut.probs)

# Add quantile bins to data frame
uas.full %<>% mutate(qnt = cut(uas, cut.points, dig.lab=2, 
                               include.lowest=TRUE, ordered_result=TRUE))

# Labels
prob.labels <- make.prob.labels(cut.probs)
cut.labels <- sapply(levels(uas.full$qnt), FUN=clean.levels)
final.labels <- paste(cut.labels, prob.labels)

# Add labels to quantile bins
uas.full %<>%
  mutate(qnt = factor(qnt, labels=final.labels))


#------------------
# Plot everything
#------------------
# Countries
plot.count <- ggplot(uas.full, aes(map_id=id)) +
  geom_map(aes(fill=uas), map=world.ggmap, colour="black", size=0.1) +
  expand_limits(x=world.ggmap$long, y=world.ggmap$lat) + 
  coord_map(xlim=c(-180,180), ylim=c(-60, 90)) + 
  scale_fill_gradient(high="#F02311", low="#EFD9D8", na.value="grey",
                      guide="colorbar", name="# of UAs") +
  theme_blank_map() + theme(legend.key.width=unit(0.55, "line"))
plot.count

ggsave(plot.count, filename="plots/ua_count.pdf", width=7, height=4.5, 
       units="in", device=cairo_pdf)


# Cutpoints
subtitle <- paste("Cut points at", paste0(cut.probs * 100, "th", collapse=", "), "percentiles")
plot.hist <- ggplot(uas.full, aes(x=uas)) + 
  geom_histogram(fill="#F02311", binwidth=25) + 
  labs(x="Urgent actions per country", y="Count") +
  ggtitle(bquote(atop("UA Distribution", atop(.(subtitle)), ""))) +
  geom_vline(xintercept=cut.points, colour="grey20", size=0.5, alpha=0.7) + 
  theme_clean()
plot.hist

ggsave(plot.hist, filename="plots/cutpoints.pdf", width=7, height=4.5, 
       units="in", device=cairo_pdf)


# Quantiles
# Create colour gradient by varying the saturation
qnt.gradient <- data.frame(s=seq(0.05, 0.9, length.out=length(levels(uas.full$qnt))), 
                           h=(4/365), l=0.94) %>%
  mutate(hex = hsv(h, s, l))

plot.quantiles <- ggplot(uas.full, aes(map_id=id)) +
  geom_map(aes(fill=qnt), map=world.ggmap) +
  geom_map(aes(fill=qnt), map=world.ggmap,  # Second layer to add borders and slash-less legend
           colour="black", size=0.1, show_guide=FALSE) +  
  expand_limits(x=world.ggmap$long, y=world.ggmap$lat) + 
  coord_map(xlim=c(-180,180), ylim=c(-60, 90)) + 
  scale_fill_manual(values=qnt.gradient$hex, na.value="grey", name="# of UAs (percentile)") + 
  theme_blank_map() + theme(legend.key.width=unit(0.55, "line"))
plot.quantiles

ggsave(plot.quantiles, filename="plots/ua_quantiles.pdf", width=8, height=4.5, 
       units="in", device=cairo_pdf)

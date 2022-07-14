##Map
library(tidyverse)
library(tigris)
library(sf)
library(ggrepel)
library(ggspatial)
library(tidyverse)
library(ggrepel)
library(usmap)
library(ggspatial)
library(usmap)
library(sf)
library(ggplot2)
library(viridis)
library(here)

set.seed(3)

source(here("Analyses/repelLayers.R"))
load(here("Data/dedoose/WS_dedooseR.RData"))

targetYear = '2020'

ds_long.regions$division <- as.character(ds_long.regions$division)
ds_long.regions$division <- ifelse(is.na(ds_long.regions$division) , "Canda", ds_long.regions$division)

ds_long.regions <- ds_long.regions[ds_long.regions$Time.Frame == targetYear,]

Divisions.freqs <- lapply(unique(ds_long$Topic_1), function(x){
  tsds <- ds_long.regions[ds_long.regions$Topic_1 == x & is.na(ds_long.regions$Topic_2),]
  targettf <- unique(tsds$division)
  tf <- sapply(targettf, function(y){
    tsds <- tsds[tsds$division == y & is.na(tsds$Topic_2),]
    100*length(which(tsds$value == 1))/nrow(tsds)
  })
  names(tf) <- targettf
  tf
})

Divisions.freqs <- do.call(rbind, Divisions.freqs)
Divisions.freqs <- as.data.frame(Divisions.freqs)
row.names(Divisions.freqs) <- freq.global$Topic
Divisions.freqs <- as.data.frame(Divisions.freqs)

write.csv(Divisions.freqs, here("Data/dedoose/Results/7. Divisions.freqs.2020.csv"))

##Generate a translation table names vs numbers (for the map)
divTrans <- cbind.data.frame(name=ds_long.regions$division, number=ds_long.regions$DIVISION)
regTrans <- cbind.data.frame(name=ds_long.regions$region, number=ds_long.regions$REGION)
divTrans <- divTrans[!duplicated(divTrans$name),]
regTrans <- regTrans[!duplicated(regTrans$name),]

# Get the states and information on the divisions and regions
sts <- tigris::states() %>%
  filter(!STUSPS %in% c('HI', 'AK', 'PR', 'GU', 'VI', 'AS', 'MP'))

div <- sts %>%
  group_by(DIVISION) %>%
  summarize()

##Estimate the number of letters per division
Lettercount <- merge(ts.areas, des[des$Time.Frame == targetYear,], by.x = "abb", by.y = "State_AB")
Lettercount <- as.data.frame(table(Lettercount$division))
colnames(Lettercount) <- c("Division", "NLetters")

##Get the top per division

Divisions.freqs$Topic <- row.names(Divisions.freqs)

TopDivision <- lapply(1:(ncol(Divisions.freqs)-1), function(x){
  target <- Divisions.freqs[ Divisions.freqs[,x] %in% head( sort(unique(Divisions.freqs[,x]), decreasing = T) ,3), ]
  target <- target[order(target[,x], decreasing = TRUE),]
  target <- target[target[,x] > 0,]

  cbind.data.frame(Division = names(Divisions.freqs)[x], Top3= paste(
    paste0("- ",target$Topic," (", round(target[,x],1) , "%)")
    , collapse = "\n"))
}
)
TopDivision <- do.call(rbind, TopDivision)

##(If We need to plot some frequencies per state...)
Divisions.freqs.reshaped <- Divisions.freqs %>%
  mutate("Topic" = rownames(Divisions.freqs)) %>%
  gather(key = Division, value=Freq, -Topic)
targetDiv <- Divisions.freqs.reshaped[Divisions.freqs.reshaped$Topic == "Organizational Commitment to Change",]
targetDiv <- merge(targetDiv, divTrans, by.x = "Division", by.y = "name")
div <- merge(div, targetDiv, by.x="DIVISION", by.y = "number")
div <- merge(div, TopDivision, by.x="Division", by.y = "Division")
div <- merge(div, Lettercount, by.x="Division", by.y = "Division")


##Get the centroids per region
centroids <- st_centroid(div) %>%
  st_geometry() %>% as(., "Spatial")

centroids <- cbind.data.frame(centroids, label = div$Top3)
colnames(centroids)[c(1:2)] <- c("lon", "lat")
centroids = cbind.data.frame(centroids, Freq = div$NLetters, division = div$Division)
d.coord <- usmap_transform(centroids)


##Generate the map
d   <- us_map("states")
sts <- tigris::states()
states <- as.data.frame(sts)
correspondence <- merge(dfRegion, ts.areas, by.x = "STUSPS", by.y = "abb")
states <- merge(states, correspondence, by.x = "STUSPS", by.y = "STUSPS")
d <- merge(d, states, by.x="abbr", by.y="STUSPS")

USS <- lapply(split(d, d$full), function(x) {
  if(length(table(x$piece)) == 1)
  {
    st_polygon(list(cbind(x$x, x$y)))
  }
  else
  {
    st_multipolygon(list(lapply(split(x, x$piece), function(y) cbind(y$x, y$y))))
  }
})

states2 <- states[which( states$NAME %in% unique(d$full)),]
USA  <- st_sfc(USS, crs = usmap_crs()@projargs)
states2 <- states2[order(match(states2$NAME,names(USA))),c("STUSPS","NAME","region", "division")]
USA  <- st_sf(data.frame(states2, geometry = USA))
USA$centroids <- st_centroid(USA$geometry)

cols <- palette.colors(n=9, palette = "Tableau 10")
cols[8] <- "#56B4E9"
cols[9] <- "#D55E00"
names(cols) <- sort(levels(USA$division))

divs <- as.character(unique(USA$division))
divs<-sort(divs)
USA$division <- factor(USA$division, levels = divs)

pdf(here("Data/dedoose/Results/9. Map.2020.pdf"), 18, 10)
ggplot(USA) +
  geom_sf(aes(fill = division))+
  ggspatial::geom_spatial_point(data = d.coord,
                                aes(x = lon, y = lat, size=Freq))+
  college_layers(d = filter(d.coord, x > 26e4), label = label, Threshold = 26e4)+
  college_layers(d = filter(d.coord, x < 26e4), label = label, Threshold = 26e4)+
  expand_limits(x = c(-10e6, 10e6),
                y = c(-7e6, 7e6))+
  scale_fill_manual(values = cols, name= "division")+
  theme_minimal()+
  theme(
    #text = element_text(family = "Ubuntu Regular", color = "#22211d"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.border = element_blank(),
    legend.position="bottom"
  )
dev.off()

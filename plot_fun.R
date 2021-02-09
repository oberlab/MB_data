initialize_polygons <- function(){
  gpclibPermit()
  polygons_exist <- file.exists("src/gadm36_DEU_3_sp.rds")
  if(polygons_exist){
    landkreise_rds <- readRDS("src/gadm36_DEU_3_sp.rds")
    landkreis_mb_shape <- landkreise_rds[landkreise_rds$NAME_2=='Miesbach',]
    return (landkreis_mb_shape)
  }
  
}

create_landkreis_plot <- function(location_data, maxFaelleThreshold){
  
  set_font <- "Roboto Mono" 
  
  if(!is.null(location_data)){
    landkreis_mb_shape <- initialize_polygons()
    gemeinden_in_mb <- landkreis_mb_shape[landkreis_mb_shape$NAME_2=='Miesbach',]
    
    oberlab_palette <- c("#00a4c4", "#3db8c1", "#fabb3a","#c7326c")
    normal_palette <- c("#339933", "#339933", "#fabb3a","#ff0000")
    
    used_palette <- oberlab_palette  
    
    
    gemeinden_fortification <- fortify(gemeinden_in_mb, region = "NAME_3")
    faelle_labels <- aggregate(cbind(long,lat) ~ id, data = gemeinden_fortification, FUN = mean)
    faelle_labels <-merge(location_data[,c("Gemeinde","cases")], faelle_labels, by.x="Gemeinde", by.y="id", all=T)
    faelle_labels <- mutate(faelle_labels,label=paste(faelle_labels$Gemeinde,"\n", faelle_labels$cases))
    
    
    einfache_lkr_map <- fortify(gemeinden_in_mb, region = "NAME_3")
    einfache_lkr_map <-merge(einfache_lkr_map,location_data[,c("Gemeinde","cases","einwohner")],by.x="id",by.y="Gemeinde")
    einfache_lkr_map <- mutate(einfache_lkr_map,quoten=einfache_lkr_map$cases/einfache_lkr_map$einwohner*100)
    
    einfache_lkr_ggplot_map <- ggplot(einfache_lkr_map) + 
      geom_polygon(aes(x = long, y = lat, group = group, fill=quoten), colour = "white") +
      labs(title = "",
           subtitle = "Lizenz: CC-BY-SA, FabLab Oberland e.V.") +
      theme_void()  +
      theme(legend.position = c(0.5,0),
            legend.direction = "horizontal",
            legend.key.width=unit(2,"cm"),
            legend.text = element_text(size = rel(1.0), color = "black", family = set_font),
            legend.title = element_text(size = rel(1.0), color = "black", family = set_font),
            legend.text.align = 0,
            legend.title.align = 0,
            plot.title = element_text(size = 12*1.2, color = "#c7326c", family = set_font, vjust = 5),  
            plot.margin = unit(c(2,10,4.5,0.1) , "lines"),
            plot.caption = element_text(size = 12, vjust = 5, hjust = 1))
    
    einfache_lkr_ggplot_map <- einfache_lkr_ggplot_map +
      geom_text(data = faelle_labels, aes(x=long, y=lat, label=label), fontface = "bold", fontfamily = "Roboto Mono") +
      annotation_custom(grob = textGrob("Erstellt mit \u2661\n in Zusammenarbeit von",
                                        gp=gpar(fontfamily = "Roboto Mono", fontsize = 12*0.9)),
                        xmin=11.85+logox, xmax=12.00+logox, ymin=47.91+logoy, ymax=Inf) +
      annotation_custom(logo, xmin=11.85+logox, xmax=11.95+logox, ymin=47.89+logoy, ymax=47.92+logoy) +
      annotation_custom(logo2, xmin=11.95+logox, xmax=12.0+logox, ymin=47.89+logoy, ymax=47.92+logoy)
    
    if(maxFaelleThreshold == 0){
      einfache_lkr_ggplot_map <- einfache_lkr_ggplot_map + 
        theme(legend.position = "none")
      
      used_palette <- c("#3db8c1")
    }
    
    #einfache_lkr_ggplot_map <- einfache_lkr_ggplot_map + scale_fill_distiller(name = "Aktive\nFälle", palette = "RdYlGn")
    einfache_lkr_ggplot_map <- einfache_lkr_ggplot_map + scale_fill_gradientn(name="Relative Betroffenheit nach\nGemeinde (in % der Einwohner)",colours = used_palette, limits=c(0,maxFaelleThreshold),oob=squish)
    return (einfache_lkr_ggplot_map)
  }
  
}
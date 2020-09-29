library(dplyr)
library(ggplot2)
library(plotly)
library(reshape2)

df <- readRDS("data/final.rds")

d <-  df %>% dplyr::select(sntl_station, state, dplyr::contains(c("nse_")))

d.m <- reshape2::melt(d)

## Calculates percent contribution of each variable across all
## the simulated scenarios
# d.m <- d.m %>%
#   group_by(variable) %>%
#   mutate(total = sum(value),
#          share = (value / total) * 100) %>%
#   ungroup()



d.m <- d.m %>% dplyr::filter(sntl_station == "1000_OR")

a <-
  ggplot(d.m, aes(variable, sntl_station,  fill = value)) +
  geom_tile(inherit.aes = TRUE)  +
  scale_fill_distiller(palette =  "RdBu", direction = -1) +
  theme(
    axis.text.x = element_text(angle = 90, colour = "Black"),
    axis.text.y = element_text(colour = "Black"),
    axis.title = element_blank(),
    legend.position='right')

ggplotly(a)





# # TEST To SEE if the dataframe from the reactive func is accessible
# output$tab1 <- renderTable(
#     d.m %>% head(100) )


b <- ggplot(d.m) +
  
  geom_bar(
    aes(
      y = value,
      x = variable,
      fill = reorder(state,-value)
    ),
    stat = "identity",
    position = "dodge"
  ) +
  theme_bw(base_rect_size = 0.1)+
  theme(
    axis.text.x = element_text(
      angle = 45,
      vjust = ,
      colour = "Black"
    ),
    axis.text.y = element_text(colour = "Black"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank()
  ) + coord_flip()  + 
  scale_y_continuous(labels = function(x) paste0(x*1, "%"))+ 
  theme(legend.position ="none") 
ggplotly(b)


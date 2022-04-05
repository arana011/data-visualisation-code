# rcode
#fusion chart R code
fusionPlot(data, x, y, type = "column2d", numberSuffix = NULL)

library(fusionchartsR)

# Single
df <- data.frame(label = c("Venezuela", "Saudi", "Canada", "Russia"), value = c(290, 260,180, 115))
df %>%
    fusionPlot(x = "label", y = "value", type = "pie2d") %>%
    fusionTheme(theme = "fusion")

library(fusionchartsR)

# Multiple charts
new.data <- data.frame(
    label = rep(x = c(2012:2016), times = 2),
    seriesname = c(rep("iOS App Store", 5), rep("Google Play Store", 5)),
    values = c(1:10)
)

new.data %>%
    fusionMultiPlot(
        x = "label",
        y = "values",
        col = "seriesname",
        type = "mscolumn2d",
    ) %>%
    fusionTheme(theme = "fusion")


library(fusionchartsR)
df <- data.frame(label = c("Venezuela", "Saudi", "Canada", "Russia"), value = c(290, 260,180, 115))

df %>%
    fusionPlot(x = "label", y = "value", type = "pie2d") %>%
    fusionTheme(theme = "gammel")

df %>%
    fusionPlot(x = "label", y = "value", type = "pie2d") %>%
    fusionPalette(palettecolors = c("5d62b5", "29c3be", "f2726f")) %>%
    fusionTheme(theme = "gammel")


library(fusionchartsR)
df <- data.frame(label = c("Venezuela", "Saudi", "Canada", "Russia"), value = c(290, 260,180, 115))
df %>%
    fusionPlot(x = "label", y = "value", type = "doughnut2d") %>%
    fusionLegend(legendCaption = "LegendCaption", legendCaptionFontSize = "24") %>%
    fusionTheme(theme = "fusion")

#sankey graph

library(networkD3)
nodes = data.frame("name" = 
                       c("Node A", # Node 0
                         "Node B", # Node 1
                         "Node C", # Node 2
                         "Node D"))# Node 3
links = as.data.frame(matrix(c(
    0, 1, 10, # Each row represents a link. The first number
    0, 2, 20, # represents the node being conntected from. 
    1, 3, 30, # the second number represents the node connected to.
    2, 3, 40),# The third number is the value of the node
    byrow = TRUE, ncol = 3))
names(links) = c("source", "target", "value")
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize= 12, nodeWidth = 30)

#install.packages("remotes")
#remotes::install_github("davidsjoberg/ggsankey")

library(ggsankey)

library(ggplot2)
library(dplyr)


#How to show the data labels with the values (count) of each node.
#Create data which can be used for Sankey
set.seed(111)

# Simple

t1 <- sample(x = c("Hosp A", "Hosp B", "Hosp C","Hosp D") , size = 100, replace=TRUE)
t2 <- sample(x = c("Male", "Female")   , size = 100, replace=TRUE)
t3 <- sample(x = c("Survived", "Died") , size = 100, replace=TRUE)

d <- data.frame(cbind(t1,t2,t3))
names(d) <- c('Hospital', 'Gender',  'Outcome')

head(d)

# Step 1
df <- d %>%
    make_long(Hospital, Gender, Outcome)
df

# Chart Sankey 1
pl <- ggplot(df, aes(x = x
                     , next_x = next_x
                     , node = node
                     , next_node = next_node
                     , fill = factor(node)
                     , label = node)
)
pl <- pl +geom_sankey(flow.alpha = 0.5
                      , node.color = "black"
                      ,show.legend = FALSE)
pl <- pl +geom_sankey_label(size = 3, color = "black", fill= "white", hjust = -0.5)
pl <- pl +  theme_bw()
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank())
pl <- pl + scale_fill_viridis_d(option = "inferno")
pl <- pl + labs(title = "Sankey diagram using ggplot")
pl <- pl + labs(subtitle = "using  David Sjoberg's ggsankey package")
pl <- pl + labs(caption = "@techanswers88")
pl <- pl + labs(fill = 'Nodes')
pl

#Create data which can be used for Sankey
set.seed(111)

# Simple

t1 <- sample(x = c("Hosp A", "Hosp B", "Hosp C","Hosp D") , size = 100, replace=TRUE)
t2 <- sample(x = c("Male", "Female")   , size = 100, replace=TRUE)
t3 <- sample(x = c("Survived", "Died") , size = 100, replace=TRUE)

d <- data.frame(cbind(t1,t2,t3))
names(d) <- c('Hospital', 'Gender',  'Outcome')


# Step 1
df <- d %>%
    make_long(Hospital, Gender, Outcome)

# Step 2
dagg <- df%>%
    dplyr::group_by(node)%>%
    tally()


# Step 3
df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)

# Chart 2
pl <- ggplot(df2, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      , fill = factor(node)
                      
                      , label = paste0(node," n=", n)
)
) 
pl <- pl +geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = TRUE)
pl <- pl +geom_sankey_label(size = 3, color = "white", fill= "gray40", hjust = -0.2)

pl <- pl +  theme_bw()
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank())
pl <- pl + scale_fill_viridis_d(option = "inferno")
pl <- pl + labs(title = "Sankey diagram using ggplot")
pl <- pl + labs(subtitle = "using  David Sjoberg's ggsankey package")
pl <- pl + labs(caption = "@techanswers88")
pl <- pl + labs(fill = 'Nodes')


pl

#Sankey with bit more complicated data

t1 <- sample(paste("Hosp", letters), size = 100, replace=TRUE)
t2 <- sample(x = c("Male", "Female")   , size = 100, replace=TRUE)
t3 <- floor(runif(100, min = 0, max = 110))
t4 <- sample(x = c("Survived", "Died") , size = 100, replace=TRUE)
t5  <- sample(paste("Facility ", letters), size = 100, replace=TRUE)

d <- data.frame(cbind(t1,t2,t3,t4, t5))
names(d) <- c('Hospital', 'Gender', 'AgeYears', 'Outcome', 'Dischargeto')

d$AgeYears <- as.integer(d$AgeYears)
d$AgeGroup <- cut(d$AgeYears, 
                  breaks = c(-Inf
                             ,5 ,10 ,15,20,25,30,35,40,45,50,55,60 ,65,70,75,80,85
                             , Inf), 
                  
                  labels = c("0-4 years"
                             ,"5-9 years","10-14 years","15-19 years","20-24 years"
                             ,"25-29 years","30-34 years","35-39 years","40-44 years"
                             ,"45-49 years","50-54 years","55-59 years","60-64 years"
                             ,"65-69 years","70-74 years","75-79 years","80-84 years"
                             ,"85+ years"),
                  right = FALSE)




# Step 1
df <- d %>%
    make_long(Hospital, Gender,AgeGroup, Outcome,Dischargeto)

# Step 2
dagg <- df%>%
    dplyr::group_by(node)%>%
    tally()


# Step 3
df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)


# Chart
pl <- ggplot(df2, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      , fill = factor(node)
                      , label = paste0(node," n=", n)
)
) 
pl <- pl +geom_sankey(flow.alpha = 0.5, node.color = "black",show.legend = TRUE)
pl <- pl +geom_sankey_text(size = 2, color = "blue", hjust = -0.5)
#pl <- pl +geom_sankey_label(size = 2, color = "black", fill= "white", hjust = -0.1)
pl <- pl +  theme_bw()
pl <- pl + theme_sankey(base_size = 16) 
pl <- pl + theme(legend.position = "none")
pl <- pl + labs(title = "Sankey diagram using ggplot")
pl <- pl + labs(subtitle = "using David Sjoberg's ggsankey package")
pl <- pl + labs(caption = "@techanswers88")
pl <- pl + labs(fill = 'Nodes')

pl <- pl +  theme(axis.title = element_blank()
                  , axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank())

pl <- pl + scale_fill_viridis_d(option = "inferno")
pl



#chart.js graph

install.packages("remotes")
remotes::install_github("JohnCoene/charter")
```

## Example

``` r
library(charter)

chart(cars, caes(speed, dist)) %>% 
    c_scatter()

remotes::install_github("allisonhorst/palmerpenguins")
data(penguins, package = 'palmerpenguins')

chart(data = penguins, caes(flipper_length_mm, body_mass_g)) %>% 
    c_scatter(caes(color = species, group = species)) %>% 
    c_colors( c("darkorange","darkorchid","darkcyan"))
```

#sigmajs graph

library(sigmajs)

# generate data
nodes <- sg_make_nodes()
edges <- sg_make_edges(nodes)

# visualise
sigmajs() %>%
    sg_nodes(nodes, id, label, size, color) %>%
    sg_edges(edges, id, source, target)

# from igraph 
data("lesmis_igraph")

layout <- igraph::layout_with_fr(lesmis_igraph)

sigmajs() %>%
    sg_from_igraph(lesmis_igraph, layout)

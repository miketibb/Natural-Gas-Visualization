library(plyr)
library(rCharts)
library(rMaps)
datm <- read.csv()

datm2 <- transform(datm,
                   State = state.abb[match(as.character(State), state.name)],
                   fillKey = cut(Prod, quantile(Prod, seq(0, 1, 1/5)), labels = LETTERS[1:5]),
                   Year = as.numeric(substr(Year, 1, 4))
)

dat2 <- dlply(na.omit(datm2), "Year", function(x){
  y = toJSONArray2(x, json = F)
  names(y) = lapply(y, '[[', 'State')
  return(y)
})

fills = setNames(
  c(RColorBrewer::brewer.pal(5, 'YlOrRd'), 'white'),
  c(LETTERS[1:5], 'defaultFill')
)

options(rcharts.cdn = TRUE)
map <- Datamaps$new()
map$set(
  dom = 'chart_1',
  scope = 'usa',
  fills = fills,
  data = dat2[[1]],
  legend = TRUE,
  labels = TRUE
)
map

source('ichoropleth.R')
ichoropleth(Prod ~ State,
            data = datm2[,1:3],
            pal = 'Reds',
            ncuts = 5,
            animate = 'Year',
            title = "Natural Gas Production (labelled in million cubic feet)"
)

htmlwidgets::saveWidget(as.widget(map), "index.html")

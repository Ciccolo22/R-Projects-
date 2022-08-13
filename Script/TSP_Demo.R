
### Solve TSP For Ten 7-11 Stores in the Richmond, VA area


#load libraries
library(leaflet) ## mapping
library(tidyverse)## wrangling
library(TSP)#modeling


#read data in
data <- read.csv("stores.csv")

#preprocess the data
data2 <- data %>%
  mutate(lng=as.numeric(lng)) %>% column_to_rownames("Store")

#create distance matrix between the stores
dist_matrix <- dist(data2 %>% select(lng,lat),method='euclidean')

#create TSP object
tsp_prob <- TSP(dist_matrix)
class(tsp_prob)
tsp_prob

#symetric problem, insert dummy, no start or end store specified, shortest
#hamiltonian path through the network
tsp_prob <- insert_dummy(tsp_prob,label = "dummy")

#solve the shortest path through the 7-11 store network using "two opt" algo
tour <- solve_TSP(tsp_prob, method = "two_opt")
#obtain the tour length
tour_length(tour)

#obtain the specific path through the network
path <- names(cut_tour(tour,"dummy"))
path

#visualize the path through the network
data %>% 
  mutate(id_order=order(as.integer(path))) %>% 
  arrange(id_order) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~lng,~lat, label = ~Store,fillColor = "tomato", fillOpacity = 0.8,
                   stroke = FALSE) %>% 
  addPolylines(~lng,~lat)


## asymmetric TSP problem - start at store 1 
atsp <- as.ATSP(dist_matrix)
store1 <- which(labels(data2[[1]]) == "1") 

#set distance from last store back to store 1 to 0, so as not to contribute to 
#the path length
atsp[, store1] <- 0

#solve TSP using two_opt method
initial_tour <- solve_TSP(atsp, method="nn")
initial_tour

#try and improve initial tour with two opt method
tour <- solve_TSP(atsp, method ="two_opt", control = list(tour = initial_tour))
tour

#obtain the path
path <- cut_tour(tour, store1, exclude_cut = FALSE)
path


#plot the data
data %>% 
  mutate(id_order=order(as.integer(path))) %>% 
  arrange(id_order) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~lng,~lat, label = ~Store,fillColor = "tomato", fillOpacity = 0.8,
                   stroke = FALSE) %>% 
  addPolylines(~lng,~lat)

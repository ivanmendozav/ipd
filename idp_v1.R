#IPD: Iterative POI Detection Version 1.0
#Algorithm to detect POI's from a stay points collection
#call by using source(idp_v1)

#normalization function
ipd_normalize <- function(x) (x-min(x))/(max(x)-min(x))

#distance function
ipd_euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#extract subset fraction
ipd_fraction <- function(df_trips,fraction=1){
  sub_trips <- df_trips[1:(floor(nrow(df_trips)*fraction)),]
  return(sub_trips)
}

#initialize IPD
#Parse the fractio of travel history to identify finest level of POIs with a radius based approach
ipd_initialize<-function(sub_trips, max_dist=100, fraction=1){
  sub_trips <- ipd_fraction(sub_trips, fraction)
  #1. Parse travel history
  centroids <- data.frame()
  poi_label <- 0
  sub_trips <- cbind(sub_trips,poi_label)
  new_label<-1
  
  for(i in 1:nrow(sub_trips)){
    trip <- sub_trips[i,]
    new_destination <- c(trip$destination_x,trip$destination_y)
    #look for known visited places
    flag <- FALSE
    if(i>1){ #3. For every new visit (destinations coordinates)
      closest_distance <- Inf
      for(j in 1:nrow(centroids)){
        known_poi <- centroids[j,]
        known_destination <- c(known_poi$average_x, known_poi$average_y)
        distance <- euc.dist(new_destination,known_destination)
        #3.2 Otherwise mark as closest known POI and recalculate POI centroid
        if(distance <= max_dist & distance < closest_distance){
          closest_distance <- distance
          sub_trips[i,]$poi_label <- centroids[j,]$poi_label
          flag <- TRUE
          
          #4. Calculate known POIs frequencies and probabilities (given relative frequency in travel history)
          centroids[j,]$n <- centroids[j,]$n+1
          centroids[j,]$prior <- (centroids[j,]$n)/nrow(sub_trips)
          #and update centroid :new average(xbar_i) = (n-1)*xbar_i-1 + xi) /n
          centroids[j,]$average_x <- (((centroids[j,]$n-1)*known_poi$average_x)+new_destination[1])/centroids[j,]$n
          centroids[j,]$average_y <- (((centroids[j,]$n-1)*known_poi$average_y)+new_destination[2])/centroids[j,]$n
        }
      }
    }
    #2. Mark a first visit location as a new POI
    #3.1 Mark as a new POI if no known POI is close enough (given distance tolerance: max_dist=150 )
    if(flag == FALSE){  
      sub_trips[i,]$poi_label <- new_label
      t <- cbind(new_label,trip$destination_x,trip$destination_y,1,1/nrow(sub_trips))
      colnames(t) <- c("poi_label","average_x","average_y","n","prior")
      centroids <- rbind(centroids,t)
      new_label <- new_label+1
    }
  }
  return(sub_trips)
}

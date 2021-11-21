#*************** Test procedures ***********
transfor2xy <- function(latitude, longitude, target_projection="31370"){
  #WGS to lambert1972 (31370)
  con_t <- connect_pgsql("transmob")
  query <- paste("select 
  round(st_x(st_transform(st_setsrid(st_makepoint(",longitude,",",latitude,"),4326),",target_projection,"))::numeric,3) as x,
  round(st_y(st_transform(st_setsrid(st_makepoint(",longitude,",",latitude,"),4326),",target_projection,"))::numeric,3) as y ")              
  df <- dbGetQuery(con_t, query)
  dbDisconnect(con_t)
  return(df[1,])
}


#POI's from real user's history (to test IPD)
#home 50.872546,4.6946545
#work 50.861128,4.6799898
#lunch (moete) 50.864172,4.6792044
#lunch (alma) 50.867585,4.6846013
#school (de grasmus) 50.876716,4.6886017
#supermarket (delhaize) 50.868599,4.6958644
#sportoase (philipsite) 50.867735,4.7096293
#doctor (ridderbuurt) 50.883492,4.6886554
generate_sample <- function(sampled_days=60){
  sd_error <- 30 #(meters)
  sd_error_large <- 70 #larger errors
  sd_time_error <- 10 #(minutes)
  sample_points <- 200
  
  #POIs
  home_coords <- transfor2xy(50.872546,4.6946545)
  home_x_samples <- rnorm(n = sample_points, mean=home_coords$x, sd=sd_error);  home_y_samples <- rnorm(n = sample_points, mean=home_coords$y, sd=sd_error);  home_time <- rnorm(n = sample_points, mean=19.5*60, sd=sd_time_error); home_lunch_time <- rnorm(n = sample_points, mean=12*60, sd=sd_time_error)
  work_coords <- transfor2xy(50.861128,4.6799898)
  work_x_samples <- rnorm(n = sample_points, mean=work_coords$x, sd=sd_error_large);  work_y_samples <- rnorm(n = sample_points, mean=work_coords$y, sd=sd_error_large);  work_time <- rnorm(n = sample_points, mean=9*60, sd=sd_time_error); work_lunch_time <- rnorm(n = sample_points, mean=14*60, sd=sd_time_error)
  moete_coords <- transfor2xy(50.864172,4.6792044)
  moete_x_samples <- rnorm(n = sample_points, mean=moete_coords$x, sd=sd_error);  moete_y_samples <- rnorm(n = sample_points, mean=moete_coords$y, sd=sd_error);  moete_time <- rnorm(n = sample_points, mean=13*60, sd=sd_time_error)
  school_coords <- transfor2xy(50.876716,4.6886017)
  school_x_samples <- rnorm(n = sample_points, mean=school_coords$x, sd=sd_error_large);  school_y_samples <- rnorm(n = sample_points, mean=school_coords$y, sd=sd_error_large);  school_time <- rnorm(n = sample_points, mean=8*60, sd=sd_time_error); school_afternoon_time <- rnorm(n = sample_points, mean=18*60, sd=sd_time_error)
  alma_coords <- transfor2xy(50.867585,4.6846013)
  alma_x_samples <- rnorm(n = sample_points, mean=alma_coords$x, sd=sd_error_large);  alma_y_samples <- rnorm(n = sample_points, mean=alma_coords$y, sd=sd_error_large);  alma_time <- rnorm(n = sample_points, mean=13.25*60, sd=sd_time_error)
  delhaize_coords <- transfor2xy(50.868599,4.6958644)
  delhaize_x_samples <- rnorm(n = sample_points, mean=delhaize_coords$x, sd=sd_error_large);  delhaize_y_samples <- rnorm(n = sample_points, mean=delhaize_coords$y, sd=sd_error_large);  delhaize_time <- rnorm(n = sample_points, mean=10*60, sd=sd_time_error)
  sportoase_coords <- transfor2xy(50.867735,4.7096293)
  sportoase_x_samples <- rnorm(n = sample_points, mean=sportoase_coords$x, sd=sd_error_large);  sportoase_y_samples <- rnorm(n = sample_points, mean=sportoase_coords$y, sd=sd_error_large);  sportoase_time <- rnorm(n = sample_points, mean=15*60, sd=sd_time_error)
  doctor_coords <- transfor2xy(50.883492,4.6886554)
  doctor_x_samples <- rnorm(n = sample_points, mean=doctor_coords$x, sd=sd_error);  doctor_y_samples <- rnorm(n = sample_points, mean=doctor_coords$y, sd=sd_error);  doctor_time <- rnorm(n = sample_points, mean=15*60, sd=sd_time_error)
  
  #centroids
  synthetic_data <- data.frame()
  synthetic_clusters <- rbind(data.frame(x=home_coords$x,y=home_coords$y,cluster="H",id=1),data.frame(x=work_coords$x,y=work_coords$y,cluster="W",id=2),data.frame(x=moete_coords$x,y=moete_coords$y,cluster="M",id=3),data.frame(x=school_coords$x,y=school_coords$y,cluster="S",id=4),data.frame(x=alma_coords$x,y=alma_coords$y,cluster="A",id=5),data.frame(x=delhaize_coords$x,y=delhaize_coords$y,cluster="D",id=6),data.frame(x=sportoase_coords$x,y=sportoase_coords$y,cluster="P",id=7),data.frame(x=doctor_coords$x,y=doctor_coords$y,cluster="Z",id=8))
  
  #Tours
  i<-0
  while(i<sampled_days){
    # day A:   S-W-M-W-S-H (Monday)
    i<-i+1
    synthetic_day <- rbind(data.frame(destination_x=school_x_samples[i],destination_y=school_y_samples[i],arrival_time=school_time[i],cluster="S",id=4), data.frame(destination_x=work_x_samples[i],destination_y=work_y_samples[i],arrival_time=work_time[i],cluster="W",id=2), data.frame(destination_x=moete_x_samples[i],destination_y=moete_y_samples[i],arrival_time=moete_time[i],cluster="M",id=3), data.frame(destination_x=work_x_samples[i],destination_y=work_y_samples[i],arrival_time=work_lunch_time[i],cluster="W",id=2), data.frame(destination_x=school_x_samples[i],destination_y=school_y_samples[i],arrival_time=school_afternoon_time[i],cluster="S",id=4), data.frame(destination_x=home_x_samples[i],destination_y=home_y_samples[i],arrival_time=home_time[i],cluster="H",id=1))
    synthetic_data <- rbind(synthetic_data, synthetic_day)
    
    # day B=A:   S-W-M-W-S-H (Tuesday)
    i<-i+1
    synthetic_day <- rbind(data.frame(destination_x=school_x_samples[i],destination_y=school_y_samples[i],arrival_time=school_time[i],cluster="S",id=4), data.frame(destination_x=work_x_samples[i],destination_y=work_y_samples[i],arrival_time=work_time[i],cluster="W",id=2), data.frame(destination_x=moete_x_samples[i],destination_y=moete_y_samples[i],arrival_time=moete_time[i],cluster="M",id=3), data.frame(destination_x=work_x_samples[i],destination_y=work_y_samples[i],arrival_time=work_lunch_time[i],cluster="W",id=2), data.frame(destination_x=school_x_samples[i],destination_y=school_y_samples[i],arrival_time=school_afternoon_time[i],cluster="S",id=4), data.frame(destination_x=home_x_samples[i],destination_y=home_y_samples[i],arrival_time=home_time[i],cluster="H",id=1))
    synthetic_data <- rbind(synthetic_data, synthetic_day)
    
    # day C:   S-W-A-W-S-H (Wednesday)
    i<-i+1
    synthetic_day <- rbind(data.frame(destination_x=school_x_samples[i],destination_y=school_y_samples[i],arrival_time=school_time[i],cluster="S",id=4), data.frame(destination_x=work_x_samples[i],destination_y=work_y_samples[i],arrival_time=work_time[i],cluster="W",id=2), data.frame(destination_x=alma_x_samples[i],destination_y=alma_y_samples[i],arrival_time=alma_time[i],cluster="A",id=5), data.frame(destination_x=work_x_samples[i],destination_y=work_y_samples[i],arrival_time=work_lunch_time[i],cluster="W",id=2), data.frame(destination_x=school_x_samples[i],destination_y=school_y_samples[i],arrival_time=school_afternoon_time[i],cluster="S",id=4), data.frame(destination_x=home_x_samples[i],destination_y=home_y_samples[i],arrival_time=home_time[i],cluster="H",id=1))
    synthetic_data <- rbind(synthetic_data, synthetic_day)
    
    # day D=A=B:   S-W-M-W-S-H (Thursday)
    i<-i+1
    synthetic_day <- rbind(data.frame(destination_x=school_x_samples[i],destination_y=school_y_samples[i],arrival_time=school_time[i],cluster="S",id=4), data.frame(destination_x=work_x_samples[i],destination_y=work_y_samples[i],arrival_time=work_time[i],cluster="W",id=2), data.frame(destination_x=moete_x_samples[i],destination_y=moete_y_samples[i],arrival_time=moete_time[i],cluster="M",id=3), data.frame(destination_x=work_x_samples[i],destination_y=work_y_samples[i],arrival_time=work_lunch_time[i],cluster="W",id=2), data.frame(destination_x=school_x_samples[i],destination_y=school_y_samples[i],arrival_time=school_afternoon_time[i],cluster="S",id=4), data.frame(destination_x=home_x_samples[i],destination_y=home_y_samples[i],arrival_time=home_time[i],cluster="H",id=1))
    synthetic_data <- rbind(synthetic_data, synthetic_day)
    
    # day E:   S-W-M-Z-S-H (Friday)
    i<-i+1
    synthetic_day <- rbind(data.frame(destination_x=school_x_samples[i],destination_y=school_y_samples[i],arrival_time=school_time[i],cluster="S",id=4), data.frame(destination_x=work_x_samples[i],destination_y=work_y_samples[i],arrival_time=work_time[i],cluster="W",id=2), data.frame(destination_x=moete_x_samples[i],destination_y=moete_y_samples[i],arrival_time=moete_time[i],cluster="M",id=3), data.frame(destination_x=doctor_x_samples[i],destination_y=doctor_y_samples[i],arrival_time=doctor_time[i],cluster="Z",id=8), data.frame(destination_x=school_x_samples[i],destination_y=school_y_samples[i],arrival_time=school_afternoon_time[i],cluster="S",id=4), data.frame(destination_x=home_x_samples[i],destination_y=home_y_samples[i],arrival_time=home_time[i],cluster="H",id=1))
    synthetic_data <- rbind(synthetic_data, synthetic_day)
    
    # day F:   D-H-P-H (Saturday)
    i<-i+1
    synthetic_day <- rbind(data.frame(destination_x=delhaize_x_samples[i],destination_y=delhaize_y_samples[i],arrival_time=delhaize_time[i],cluster="D",id=6), data.frame(destination_x=home_x_samples[i],destination_y=home_y_samples[i],arrival_time=home_lunch_time[i],cluster="H",id=1), data.frame(destination_x=sportoase_x_samples[i],destination_y=sportoase_y_samples[i],arrival_time=sportoase_time[i],cluster="P",id=7), data.frame(destination_x=home_x_samples[i],destination_y=home_y_samples[i],arrival_time=home_time[i],cluster="H",id=1))
    synthetic_data <- rbind(synthetic_data, synthetic_day)
    
    # day G:   D-H (Sunday)
    i<-i+1
    synthetic_day <- rbind(data.frame(destination_x=delhaize_x_samples[i],destination_y=delhaize_y_samples[i],arrival_time=delhaize_time[i],cluster="D",id=6), data.frame(destination_x=home_x_samples[i],destination_y=home_y_samples[i],arrival_time=home_lunch_time[i],cluster="H",id=1))
    synthetic_data <- rbind(synthetic_data, synthetic_day)
  }
  
  #jpeg("data-300.jpeg", width = 10, height = 7, units = 'in', res = 300)
  par(mfrow=c(1,2))
  plot(synthetic_data$destination_x, synthetic_data$destination_y, xlab="x_i", ylab="y_i", col=synthetic_data$id)
  plot(synthetic_clusters$x, synthetic_clusters$y,xlab="x_k", ylab="y_k", col=synthetic_clusters$id)
  text(synthetic_clusters$x, synthetic_clusters$y,  labels=synthetic_clusters$cluster, cex= 1, col=synthetic_clusters$id, pos = 4, offset = 0.5)
  #dev.off()
  return(synthetic_data[!is.na(synthetic_data$destination_x),])
}

generate_small_sample <- function(sampled_days=30){
  
  #synthetic data: 3 POIs (10 days): 1 home, 2 workmate, 3 job
  # day 1:    1-2-3-1 (take workmate in 2 and go to work)
  # day 2:    1-3-1 (directly to work)
  # day 3:    1-2-3-2-1 (also take workmate back in 2)
  # centroid 1: 500,500 (7pm)
  # centroid 2: 510,310 (8:30am,6:30pm)
  # centroid 3: 1500,530 (7:30am)
  sd_error <- 30 #(meters)
  sd_error2 <- 60
  sd_time_error <- 10 #(minutes)
  
  c1_x <- rnorm(n = sampled_days, mean=500, sd=sd_error)
  c1_y <- rnorm(n = sampled_days, mean=500, sd=sd_error)
  c2_x <- rnorm(n = sampled_days, mean=510, sd=sd_error)
  c2_y <- rnorm(n = sampled_days, mean=350, sd=sd_error)
  c3_x <- rnorm(n = sampled_days, mean=1500, sd=sd_error2)
  c3_y <- rnorm(n = sampled_days, mean=530, sd=sd_error2)
  c1_time <- rnorm(n = sampled_days, mean=19.5*60, sd=sd_time_error)
  c2_time <- rnorm(n = sampled_days, mean=8*60, sd=sd_time_error)
  c2_time2 <- rnorm(n = sampled_days, mean=18.5*60, sd=sd_time_error)
  c3_time <- rnorm(n = sampled_days, mean=9*60, sd=sd_time_error)
  
  synthetic_data <- data.frame()
  synthetic_clusters <- rbind(data.frame(x=500,y=500,cluster="H",id=1), data.frame(x=510,y=350,cluster="O",id=2), data.frame(x=1500,y=530,cluster="W",id=3))
  i<-0
  while(i<sampled_days){
    i<-i+1
    # day 1:    1-2-3-1 (take workmate in 2 and go to work)
    synthetic_day <- rbind(data.frame(destination_x=c2_x[i],destination_y=c2_y[i],arrival_time=c2_time[i],cluster="O",id=2), data.frame(destination_x=c3_x[i],destination_y=c3_y[i],arrival_time=c3_time[i],cluster="W",id=3), data.frame(destination_x=c1_x[i],destination_y=c1_y[i],arrival_time=c1_time[i],cluster="H",id=1))
    synthetic_data <- rbind(synthetic_data, synthetic_day)
    i<-i+1
    # day 2:    1-3-1 (directly to work)
    synthetic_day <- rbind(data.frame(destination_x=c3_x[i],destination_y=c3_y[i],arrival_time=c3_time[i],cluster="W",id=3), data.frame(destination_x=c1_x[i],destination_y=c1_y[i],arrival_time=c1_time[i],cluster="H",id=1))
    synthetic_data <- rbind(synthetic_data, synthetic_day)
    i<-i+1
    # day 3:    1-2-3-2-1 (also take workmate back in 2)
    synthetic_day <- rbind(data.frame(destination_x=c2_x[i-1],destination_y=c2_y[i-1],arrival_time=c2_time[i],cluster="O",id=2), data.frame(destination_x=c3_x[i],destination_y=c3_y[i],arrival_time=c3_time[i],cluster="W",id=3),  data.frame(destination_x=c2_x[i],destination_y=c2_y[i],arrival_time=c2_time2[i],cluster="O",id=2), data.frame(destination_x=c1_x[i],destination_y=c1_y[i],arrival_time=c1_time[i],cluster="H",id=1))
    synthetic_data <- rbind(synthetic_data, synthetic_day)
  }
  
  #jpeg("data-300.jpeg", width = 10, height = 7, units = 'in', res = 300)
  par(mfrow=c(1,2))
  plot(synthetic_data$destination_x, synthetic_data$destination_y, xlim = c(0,2000), ylim = c(0,1000),xlab="x_i", ylab="y_i", col=synthetic_data$id)
  plot(synthetic_clusters$x, synthetic_clusters$y, xlim = c(0,2000), ylim = c(0,1000),xlab="x_k", ylab="y_k", col=synthetic_clusters$id)
  text(synthetic_clusters$x, synthetic_clusters$y,  labels=synthetic_clusters$cluster, cex= 1, col=synthetic_clusters$id, pos = 2, offset = 0.5)
  #dev.off()
  return(synthetic_data[!is.na(synthetic_data$destination_x),])
}


#Cuenca
#POI's from real user's history log (* large area)
#a.casa -78.961 , -2.889
#b.suegros -78.959, -2.889
#c.panaderia -78.958, -2.886
#d.coral -78.977, -2.897
#e.papitos -79.005, -2.903
#f.hansel & gretel -79.004, -2.909
#g.uda (campus) -79.000, -2.918
#h.uda (cctt) -79.001, -2.919
#i. cedia -79.010, -2.907
#j. U Cuenca -79.009, -2.900
#k. Lucrecia -79.017, -2.910
#l. Casa mayo -79.028, -2.907
#m. Colombia pizza -79.028, -2.915
#n. Mideros -79.044, -2.922
#o. Jardines -79.036, -2.897
#p. Miraflores -79.988, -2.887

#taken from a google timeline
generate_real_sample <- function(writeToDB=FALSE){
  
  target_projection = "24877"; #UTM 17N
  #POIs (and coordinates) latitude, longitude
  a <- transfor2xy( -2.889, -78.961 ,target_projection)
  b <- transfor2xy( -2.889, -78.959,target_projection)
  c <- transfor2xy( -2.886, -78.958,target_projection)
  d <- transfor2xy( -2.897, -78.977,target_projection)
  e <- transfor2xy( -2.903, -79.005,target_projection)
  f <- transfor2xy( -2.909, -79.004,target_projection)
  g <- transfor2xy( -2.918, -79.000,target_projection)
  h <- transfor2xy( -2.919, -79.001,target_projection)
  i <- transfor2xy( -2.907, -79.010,target_projection)
  j <- transfor2xy( -2.900, -79.009,target_projection)
  k <- transfor2xy( -2.910, -79.017,target_projection)
  l <- transfor2xy( -2.907, -79.028,target_projection)
  m <- transfor2xy( -2.915, -79.028,target_projection)
  n <- transfor2xy( -2.922, -79.044,target_projection)
  o <- transfor2xy( -2.897, -79.036,target_projection)
  p <- transfor2xy( -2.887, -79.988,target_projection)
  
  sd_error <- 25 #(meters)
  sd_error_large <- 50
  sample_points <- 100 #at least equal to num days for simulation
  
  #number visits, coordinate, dispersion
  a_x<-rnorm(n=sample_points, mean=a$x, sd=sd_error); a_y<-rnorm(n=sample_points, mean=a$y, sd=sd_error)
  b_x<-rnorm(n=sample_points, mean=b$x, sd=sd_error); b_y<-rnorm(n=sample_points, mean=b$y, sd=sd_error)
  c_x<-rnorm(n=sample_points, mean=c$x, sd=sd_error); c_y<-rnorm(n=sample_points, mean=c$y, sd=sd_error)
  d_x<-rnorm(n=sample_points, mean=d$x, sd=sd_error_large); d_y<-rnorm(n=sample_points, mean=d$y, sd=sd_error_large)
  e_x<-rnorm(n=sample_points, mean=e$x, sd=sd_error); e_y<-rnorm(n=sample_points, mean=e$y, sd=sd_error)
  f_x<-rnorm(n=sample_points, mean=f$x, sd=sd_error); f_y<-rnorm(n=sample_points, mean=f$y, sd=sd_error)
  g_x<-rnorm(n=sample_points, mean=g$x, sd=sd_error_large); g_y<-rnorm(n=sample_points, mean=g$y, sd=sd_error_large)
  h_x<-rnorm(n=sample_points, mean=h$x, sd=sd_error_large); h_y<-rnorm(n=sample_points, mean=h$y, sd=sd_error_large)
  i_x<-rnorm(n=sample_points, mean=i$x, sd=sd_error); i_y<-rnorm(n=sample_points, mean=i$y, sd=sd_error)
  j_x<-rnorm(n=sample_points, mean=j$x, sd=sd_error_large); j_y<-rnorm(n=sample_points, mean=j$y, sd=sd_error_large)
  k_x<-rnorm(n=sample_points, mean=k$x, sd=sd_error); k_y<-rnorm(n=sample_points, mean=k$y, sd=sd_error)
  l_x<-rnorm(n=sample_points, mean=l$x, sd=sd_error); l_y<-rnorm(n=sample_points, mean=l$y, sd=sd_error)
  m_x<-rnorm(n=sample_points, mean=m$x, sd=sd_error); m_y<-rnorm(n=sample_points, mean=m$y, sd=sd_error)
  n_x<-rnorm(n=sample_points, mean=n$x, sd=sd_error); n_y<-rnorm(n=sample_points, mean=n$y, sd=sd_error)
  o_x<-rnorm(n=sample_points, mean=o$x, sd=sd_error); o_y<-rnorm(n=sample_points, mean=o$y, sd=sd_error)
  p_x<-rnorm(n=sample_points, mean=p$x, sd=sd_error_large); p_y<-rnorm(n=sample_points, mean=p$y, sd=sd_error_large)
  
  #dic 9 2019: home, udacctt, udacampus,panaderia, home
  day <- 1
  day_1 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=9*60,cluster="H"), data.frame(destination_x=g_x[day],destination_y=g_y[day],arrival_time=14*60,cluster="G") , data.frame(destination_x=c_x[day],destination_y=c_y[day],arrival_time=18.5*60,cluster="C") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=19*60,cluster="A"))
  #dic 10 2019: home, udacctt, udacampus, home
  day <- 2
  day_2 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=8.5*60,cluster="H"), data.frame(destination_x=g_x[day],destination_y=g_y[day],arrival_time=14.25*60,cluster="G") , data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=16.25*60,cluster="H") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=22.5*60,cluster="A"))
  #dic 11 2019: home, udacctt, udacampus, home, udacctt, home, casa mayo, home,
  day <- 3
  day_3 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=8*60,cluster="H"), data.frame(destination_x=g_x[day],destination_y=g_y[day],arrival_time=11*60,cluster="G") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=13*60,cluster="A"), data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=14*60,cluster="H"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=16.5*60,cluster="A"), data.frame(destination_x=l_x[day],destination_y=l_y[day],arrival_time=20.5*60,cluster="L"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=3.5*60,cluster="A"))
  #dic 12 2019: home, udacctt, home
  day <- 4
  day_4 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=9.5*60,cluster="H"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=20.5*60,cluster="A"))
  #dic 13 2019: home, udacctt, cedia, restaurant, udacctt, papitos, lucrecia, home
  day <- 5
  day_5 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=9*60,cluster="H"), data.frame(destination_x=i_x[day],destination_y=i_y[day],arrival_time=12.5*60,cluster="I"), data.frame(destination_x=f_x[day],destination_y=f_y[day],arrival_time=13*60,cluster="F"),data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=14.5*60,cluster="H"),data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=18*60,cluster="A"),data.frame(destination_x=e_x[day],destination_y=e_y[day],arrival_time=20.5*60,cluster="E"),data.frame(destination_x=k_x[day],destination_y=k_y[day],arrival_time=21*60,cluster="K"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=3.5*60,cluster="A"))
  #dic 14 2019: sabado (udacctt 9:15,papitos12:15,home17:15, suegros18:00, home19:00, casa mayo 20:30, home4:00)
  day <- 6
  day_6 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=9.25*60,cluster="H"), data.frame(destination_x=e_x[day],destination_y=e_y[day],arrival_time=12.5*60,cluster="E") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=17.25*60,cluster="H") , data.frame(destination_x=b_x[day],destination_y=b_y[day],arrival_time=18*60,cluster="B"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=19*60,cluster="A"), data.frame(destination_x=l_x[day],destination_y=l_y[day],arrival_time=20.5*60,cluster="L"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=4*60,cluster="A"))
  #dic 15: suegros, home
  day <- 7
  day_7 <- rbind(data.frame(destination_x=b_x[day],destination_y=b_y[day],arrival_time=12*60,cluster="B"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=17*60,cluster="A"))
  #dic 16: home, udacctt, udacampus,panaderia, home
  day <- 8
  day_8 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=9.25*60,cluster="H"), data.frame(destination_x=g_x[day],destination_y=g_y[day],arrival_time=14*60,cluster="G") , data.frame(destination_x=c_x[day],destination_y=c_y[day],arrival_time=18*60,cluster="C") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=19.25*60,cluster="A"))
  #dic 17: home, udacctt, udacampus, home
  day <- 9
  day_9 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=8.25*60,cluster="H"), data.frame(destination_x=g_x[day],destination_y=g_y[day],arrival_time=14*60,cluster="G") , data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=16*60,cluster="H") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=22.25*60,cluster="A"))
  #dic 18: home, udacctt, udacampus,panaderia, home
  day <- 10
  day_10 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=9.5*60,cluster="H"), data.frame(destination_x=g_x[day],destination_y=g_y[day],arrival_time=14*60,cluster="G") , data.frame(destination_x=c_x[day],destination_y=c_y[day],arrival_time=18.5*60,cluster="C") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=19*60,cluster="A"))
  #dic 9 2019: home, udacctt, udacampus,panaderia, home
  day <- 11
  day_11 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=9*60,cluster="H"), data.frame(destination_x=g_x[day],destination_y=g_y[day],arrival_time=14*60,cluster="G") , data.frame(destination_x=c_x[day],destination_y=c_y[day],arrival_time=18.5*60,cluster="C") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=19*60,cluster="A"))
  #dic 10 2019: home, udacctt, udacampus, home
  day <- 12
  day_12 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=8.5*60,cluster="H"), data.frame(destination_x=g_x[day],destination_y=g_y[day],arrival_time=14.25*60,cluster="G") , data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=16.25*60,cluster="H") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=22.5*60,cluster="A"))
  #dic 11 2019: home, udacctt, udacampus, home, udacctt, home, casa mayo, home,
  day <- 13
  day_13 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=8*60,cluster="H"), data.frame(destination_x=g_x[day],destination_y=g_y[day],arrival_time=11*60,cluster="G") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=13*60,cluster="A"), data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=14*60,cluster="H"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=16.5*60,cluster="A"), data.frame(destination_x=l_x[day],destination_y=l_y[day],arrival_time=20.5*60,cluster="L"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=3.5*60,cluster="A"))
  #dic 12 2019: home, udacctt, home
  day <- 14
  day_14 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=9.5*60,cluster="H"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=20.5*60,cluster="A"))
  #dic 13 2019: home, udacctt, cedia, restaurant, udacctt, papitos, lucrecia, home
  day <- 15
  day_15 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=9*60,cluster="H"), data.frame(destination_x=i_x[day],destination_y=i_y[day],arrival_time=12.5*60,cluster="I"), data.frame(destination_x=f_x[day],destination_y=f_y[day],arrival_time=13*60,cluster="F"),data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=14.5*60,cluster="H"),data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=18*60,cluster="A"),data.frame(destination_x=e_x[day],destination_y=e_y[day],arrival_time=20.5*60,cluster="E"),data.frame(destination_x=k_x[day],destination_y=k_y[day],arrival_time=21*60,cluster="K"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=3.5*60,cluster="A"))
  #dic 14 2019: sabado (udacctt 9:15,papitos12:15,home17:15, suegros18:00, home19:00, casa mayo 20:30, home4:00)
  day <- 16
  day_16 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=9.25*60,cluster="H"), data.frame(destination_x=e_x[day],destination_y=e_y[day],arrival_time=12.5*60,cluster="E") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=17.25*60,cluster="H") , data.frame(destination_x=b_x[day],destination_y=b_y[day],arrival_time=18*60,cluster="B"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=19*60,cluster="A"), data.frame(destination_x=l_x[day],destination_y=l_y[day],arrival_time=20.5*60,cluster="L"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=4*60,cluster="A"))
  #dic 15: suegros, home
  day <- 17
  day_17 <- rbind(data.frame(destination_x=b_x[day],destination_y=b_y[day],arrival_time=12*60,cluster="B"), data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=17*60,cluster="A"))
  #dic 16: home, udacctt, udacampus,panaderia, home
  day <- 18
  day_18 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=9.25*60,cluster="H"), data.frame(destination_x=g_x[day],destination_y=g_y[day],arrival_time=14*60,cluster="G") , data.frame(destination_x=c_x[day],destination_y=c_y[day],arrival_time=18*60,cluster="C") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=19.25*60,cluster="A"))
  #dic 17: home, udacctt, udacampus, home
  day <- 19
  day_19 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=8.25*60,cluster="H"), data.frame(destination_x=g_x[day],destination_y=g_y[day],arrival_time=14*60,cluster="G") , data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=16*60,cluster="H") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=22.25*60,cluster="A"))
  #dic 18: home, udacctt, udacampus,panaderia, home
  day <- 20
  day_20 <- rbind(data.frame(destination_x=h_x[day],destination_y=h_y[day],arrival_time=9.5*60,cluster="H"), data.frame(destination_x=g_x[day],destination_y=g_y[day],arrival_time=14*60,cluster="G") , data.frame(destination_x=c_x[day],destination_y=c_y[day],arrival_time=18.5*60,cluster="C") , data.frame(destination_x=a_x[day],destination_y=a_y[day],arrival_time=19*60,cluster="A"))
  
  
  
  synthetic_data <- rbind(day_1, day_2, day_3, day_4, day_5, day_6, day_7, day_8, day_9, day_10)
  trip_id <- seq(1:nrow(synthetic_data))
  synthetic_data <- cbind(trip_id,synthetic_data)
  
  if(writeToDB){
    backup_ipd(synthetic_data, extra_fields = FALSE, replace_table = FALSE, target_projection = target_projection)
  }
  return(synthetic_data)
}

#save table to database OSM
backup_ipd <- function(synthetic_data, extra_fields = TRUE, replace_table = TRUE, target_projection = 4386, table_name="synthetic_trips"){
    #Save to database
    con_s <- connect_pgsql("osm")
    if(replace_table){
      query <- paste("DROP TABLE IF EXISTS ",table_name,";")
      dbGetQuery(con_s, query)
    }
        
    if(extra_fields){
      dbWriteTable(con_s, c("public", table_name),value = synthetic_data , append=TRUE, row.names=FALSE,   
                 field.types=list(od_frequency = "numeric(3,0)", od_probability = "numeric(10,9)", origin_label = "numeric(3,0)", smaller_dbscan_label = "numeric(3,0)", dbscan_label = "numeric(3,0)", destination_label = "numeric(3,0)", initial_label = "numeric(3,0)", trip_id = "numeric(8,0)", destination_x="numeric(15,7)", destination_y="numeric(15,7)", arrival_time="numeric(5,0)", cluster="character(2)", renamed_destination_label="character(2)"))
    }else{
      dbWriteTable(con_s, c("public", table_name),value = synthetic_data , append=TRUE, row.names=FALSE,   
                   field.types=list(user_id = "character(30)", trip_id = "numeric(8,0)", destination_x="numeric(15,7)", destination_y="numeric(15,7)", arrival_time="numeric(5,0)", od_frequency="numeric(3,0)", od_probability="numeric(10,9)", origin_label = "numeric(3,0)", destination_label = "numeric(3,0)", departure_time="numeric(5,0)", origin_x="numeric(15,7)", origin_y="numeric(15,7)"))
    }
    
    if(replace_table){
      query <- paste("ALTER TABLE ",table_name," ADD COLUMN geom geometry(Point,",target_projection,");")
      dbGetQuery(con_s, query)
    }
    query <- paste("update ",table_name," set geom = st_setsrid(st_point(destination_x,destination_y),",target_projection,");")
    dbGetQuery(con_s, query)
    dbDisconnect(con_s)
}


#save table to database OSM
backup_ipd_initialization <- function(initialization_data, replace_table = TRUE, target_projection = 24877, table_name="ipd_initial_labels"){
  #Save to database
  con_s <- connect_pgsql("osm")
  if(replace_table){
    query <- paste("DROP TABLE IF EXISTS ",table_name,";")
    dbGetQuery(con_s, query)
  }
  
  dbWriteTable(con_s, c("public", table_name),value = initialization_data , append=TRUE, row.names=FALSE,   
                 field.types=list(trip_id = "numeric(3,0)", destination_x="numeric(15,7)", destination_y="numeric(15,7)", arrival_time="numeric(5,0)", cluster = "character(2)", destination_label = "numeric(3,0)"))

  if(replace_table){
    query <- paste("ALTER TABLE ",table_name," ADD COLUMN geom geometry(Point,",target_projection,");")
    dbGetQuery(con_s, query)
  }
  query <- paste("update ",table_name," set geom = st_setsrid(st_point(destination_x,destination_y),",target_projection,");")
  dbGetQuery(con_s, query)
  dbDisconnect(con_s)
}



ipd_dbscan <- function(synthetic_data, epsilon = 200, plots = TRUE){
  #VS. Density-based clustering (i.e. DBSCAN)
  library(dbscan)
  sample_points <- cbind(synthetic_data$destination_x, synthetic_data$destination_y) 
  res <- dbscan::dbscan(sample_points, eps = epsilon, minPts = 1) #at least two visits
  dbscan_subtrips <- synthetic_data
  dbscan_subtrips$destination_label <- NULL
  dbscan_subtrips$destination_label <- res$cluster
  table(dbscan_subtrips$destination_label)
  dbscan_subtrips <- ipd_chain(dbscan_subtrips)
  
  #jpeg("dbscan-35.jpeg", width = 10, height = 7, units = 'in', res = 300)
  if(plots){
    par(mfrow=c(1,2))
    plot(dbscan_subtrips$destination_x, dbscan_subtrips$destination_y, xlab= "x (meters)", ylab= "y (meters)",
         col= dbscan_subtrips$destination_label, pch = 19, cex = 1, lwd = 2)#,xlim=c(500000,550000),ylim=c(170000,200000))
    text(dbscan_subtrips$destination_x, dbscan_subtrips$destination_y,  labels=dbscan_subtrips$destination_label, cex= 1, col=dbscan_subtrips$destination_label, pos = 2, offset = 0.5)
   
     sub_dbscan <- data.frame(destination_x=dbscan_subtrips$destination_x, destination_y=dbscan_subtrips$destination_y,destination_label=dbscan_subtrips$destination_label)
    means <- data.frame()
    for(i in unique(sub_dbscan$destination_label)){
      means <- rbind(means,colMeans(sub_dbscan[sub_dbscan$destination_label==i,]))
    }
    colnames(means) <- c("average_x","average_y","poi_label")
    
    plot(means$average_x, means$average_y, xlab= "x (meters)", ylab= "y (meters)",
         col= means$poi_label, pch = 19, cex = 1, lwd = 2)#, xlim=c(500000,550000),ylim=c(170000,200000))
    text(means$average_x, means$average_y,  labels=means$poi_label, cex= 1, col=means$poi_label, pos = 2, offset = 0.5)
    
    #dev.off()
  }
  return(dbscan_subtrips)
}

#Funciones
#*****************************************************
#convert trips to POIs trips for the top "sample_users"
db_get_pois <- function(sample_users, max_trips_sample=NULL){
  source("ipd_transmob_functions.R") #load specific Transmob functions
  users <- db_top_users(limit=sample_users)
  map_sample_trips <- db_sample_trips(1000); #sample of same region trips
  map_center <- calc_map_center(map_sample_trips);#to calculate center
  populated_trips <- data.frame();
  centroids <- data.frame();
  for (i in 1:sample_users){
    top_user <- users[i,]$user_id; days <- users[i,]$days
    user_trips <- db_user_pois(top_user, radius = 170,growth_factor = 1.05, init_fraction = 0.4, max_iterations = 100, days=days, sample_size=max_trips_sample)
    user_centroids <- data.frame(table(user_trips$destination_label));user_centroids <- cbind(rep(top_user,nrow(user_centroids)),user_centroids)
    user_centroids <- user_centroids[order(user_centroids$Freq,decreasing = TRUE),]
    home_poi <- as.numeric(as.vector.factor(user_centroids[1,]$Var1))
    print(paste("Home POI label is: ",home_poi, sep=""))
    #infere other variables
    user_trips <- preprocess_data(user_trips,TRUE,map_center)
    populated_trips <- rbind(populated_trips,user_trips)
    centroids <- rbind(centroids,user_centroids)
  }
  populated_trips <- landuse_data(populated_trips)
  #plot distributions
  par(mfrow=c(2,2))
  barplot(table(populated_trips$mode), xlab="Mode")
  hist(populated_trips$departure_time, xlab="Departure time", main="")
  hist(populated_trips[populated_trips$travel_time>0,]$travel_time, xlab="Travel time", main="")
  hist(populated_trips$direct_trip_distance, xlab="Travel distance", main="")
  
  #report within cluster variance
  colnames(centroids) <- c("user_id","centroid_id","n")
  x<-0;y<-0;
  for(u in unique(centroids$user_id)){
    subtrips <- populated_trips[populated_trips$user_id==u,]
    subcentroids <- centroids[centroids$user_id==u,]
    for(i in unique(subcentroids$centroid_id)){
      z <- ipd_error(subtrips = subtrips, poi_label = i, printme = FALSE)
      x <- x + z[1]; y <- y + z[2];
    }
  }
  print(paste("Average POI error(meters) is:",x/nrow(centroids),",",y/nrow(centroids)));
  return (populated_trips)
}

learnRegularity <- function(x_trips, title=""){
  #define variables that define regularity
  model <- lm(data=x_trips, formula= od_probability  ~ departure_time+travel_time+radial_movement)
  summary(model)
  
  #use deep learning (KING!!!) to learn regularity
  #install.packages("h2o")
  library(h2o)
  h2o.init();
  dataset <- as.h2o(x_trips)
  dataset <- h2o.splitFrame(dataset, ratios = 0.80)
  dl1<-h2o.deeplearning(x=c("travel_time","departure_time","radial_movement"), y="od_probability",activation="RectifierWithDropout",training_frame = dataset[[1]], hidden=c(190,63,21,7),epochs=100,input_dropout_ratio=0.1 )
  predictions <- h2o.predict(dl1,dataset[[2]])
  c <- head(predictions,n=nrow(dataset[[2]]))
  plot(c$predict, as.vector(dataset[[2]]$od_probability), xlim=c(0,max(c$predict)), ylim=c(0,max(c$predict)), xlab="Predicted", ylab="Reference",main=title, cex.main=0.9)
  lines(c(0,max(c$predict)),c(0,max(c$predict)))
  return(dl1)
}

#split into regular and non_regular trips
splitRegular <- function(populated_trips){
  #check od_probability (regularity)
  par(mfrow=c(1,1))
  hist(populated_trips$regularity)
  #separate into regular and non-regular
  m <- kmeans(populated_trips$regularity,centers = 3)
  y <- cbind(populated_trips$regularity,m$cluster); y1 <- y[y[,2]==1,]; y2 <- y[y[,2]==2,]; y3 <- y[y[,2]==3,];
  #calculate threshold
  high_threshold <- max( min(y1[,1]), min(y2[,1]), min(y3[,1]))
  low_threshold <- min( max(y1[,1]), max(y2[,1]), max(y3[,1]))
  
  regular <- populated_trips[populated_trips$regularity>=high_threshold,]
  non_regular <- populated_trips[populated_trips$regularity<=low_threshold,]
  sample_size <- min(nrow(regular), nrow(non_regular))
  regular_sample <- regular[sample(1:nrow(regular), sample_size),]
  non_regular_sample <- non_regular[sample(1:nrow(non_regular), sample_size),]
  par(mfrow=c(2,2))
  plot(density(non_regular_sample$travel_time),main="Travel time", xlab="time(min)")
  lines(density(regular_sample$travel_time),col = "red")
  plot(density(regular_sample$departure_time/60),col = "red",main="Departure time", xlab="time(24-h)")
  lines(density(non_regular_sample$departure_time/60))
  plot(density((non_regular_sample$radial_movement),na.rm = TRUE), main="Radial movement", xlab="Ratio(radial-tan-radial)")
  lines(density((regular_sample$radial_movement),na.rm = TRUE),col = "red")
  plot(density(abs(regular_sample$work_source_distance/1000)),col = "red",main="Working sources proximity", xlab="distance(km)")
  lines(density(abs(non_regular_sample$work_source_distance/1000)))
  return (list(regular_sample, non_regular_sample))
}

#"Improve" trips split
purge_trips33 <- function(populated_trips){
  #8am - 10am, 5pm - 7pm
  expected_regular <- populated_trips[(populated_trips$departure_time > 480 & populated_trips$departure_time < 700) |  (populated_trips$departure_time > 1000 & populated_trips$departure_time < 1160),]
  hist(expected_regular$departure_time)
  expected_regular <- expected_regular[(expected_regular$travel_time > 8 & expected_regular$travel_time < 15) ,]
  hist(expected_regular$travel_time)
  expected_regular <- expected_regular[abs(expected_regular$radial_movement) > 0.8 ,]
  hist(expected_regular$radial_movement)
  expected_regular <- expected_regular[expected_regular$travel_distance > 400 &  expected_regular$travel_distance < 3000,]
  hist(expected_regular$travel_distance)
  
  non_expected <- populated_trips[!populated_trips$trip_id %in% expected_regular$trip_id,]
  some_non_expected_ids <- sample(replace = FALSE, x = non_expected$trip_id, size = floor(nrow(non_expected)/3))
  some_non_expected <- non_expected[non_expected$trip_id %in% some_non_expected_ids,]
  the_rest <- non_expected[!non_expected$trip_id %in% some_non_expected_ids,]
  
  expected_regular$regularity <- rnorm(n = nrow(expected_regular),mean = max(populated_trips$regularity), sd = sqrt(var(populated_trips$regularity)))
  some_non_expected$regularity <- abs(rnorm(n = nrow(some_non_expected),mean = min(populated_trips$regularity), sd = sqrt(var(populated_trips$regularity))))
  expected_regular$od_probability <- rnorm(n = nrow(expected_regular),mean = max(populated_trips$od_probability), sd = sqrt(var(populated_trips$od_probability)))
  some_non_expected$od_probability <- abs(rnorm(n = nrow(some_non_expected),mean = min(populated_trips$od_probability), sd = sqrt(var(populated_trips$od_probability))))
  
  
  
  all <- rbind(expected_regular, some_non_expected, the_rest)
  all <- all[!is.na(all$radial_movement),]
  return(all)
}

#split into OD regular
splitODRegular <- function(populated_trips){
  #check od_probability (regularity)
  par(mfrow=c(1,1))
  hist(populated_trips$od_probability)
  #separate into regular and non-regular
  m <- kmeans(populated_trips$od_probability,centers = 3)
  y <- cbind(populated_trips$od_probability,m$cluster); y1 <- y[y[,2]==1,]; y2 <- y[y[,2]==2,]; y3 <- y[y[,2]==3,];
  #calculate threshold
  high_threshold <- max( min(y1[,1]), min(y2[,1]), min(y3[,1]))
  low_threshold <- min( max(y1[,1]), max(y2[,1]), max(y3[,1]))
  
  regular <- populated_trips[populated_trips$od_probability>=high_threshold,]
  non_regular <- populated_trips[populated_trips$od_probability<=low_threshold,]
  sample_size <- min(nrow(regular), nrow(non_regular))
  regular_sample <- regular[sample(1:nrow(regular), sample_size),]
  non_regular_sample <- non_regular[sample(1:nrow(non_regular), sample_size),]
  
  library(ggplot2)
  library(gridExtra)
  p1 <- ggplot() + geom_density(data=non_regular_sample, aes(x=travel_time), color="blue") + geom_density(data=regular_sample, aes(x=travel_time), color="red") + labs(x="Travel time (min)", y="Density") + xlim(0,100)
  p2 <- ggplot() + geom_density(data=non_regular_sample, aes(x=departure_time/60), color="blue") + geom_density(data=regular_sample, aes(x=departure_time/60), color="red") + labs(x="Departure time (24-hour)", y="Density") + xlim(0,24)
  p3 <- ggplot() + geom_density(data=non_regular_sample, aes(x=trip_direction), color="blue") + geom_density(data=regular_sample, aes(x=trip_direction), color="red") + labs(x="Radial movement", y="Density") + xlim(-1,1)
  p4 <- ggplot() + geom_density(data=non_regular_sample, aes(x=work_source_distance/1000), color="blue") + geom_density(data=regular_sample, aes(x=work_source_distance/1000), color="red") + labs(x="Hub distance (km)", y="Density") + xlim(0,100)
  grid.arrange(p1, p2,p3,p4, nrow = 2,ncol=2)
  
  
  # par(mfrow=c(2,2))
  # plot(density(non_regular_sample$travel_time),main="Travel time", xlab="time(min)", ylim=c(0,max(density(regular_sample$travel_time)$y)))
  # lines(density(regular_sample$travel_time),col = "red")
  # plot(density(non_regular_sample$departure_time/60),main="Departure time", xlab="time(24-h)", ylim=c(0,max(density(regular_sample$departure_time/60)$y)))
  # lines(density(regular_sample$departure_time/60),col = "red")
  # plot(density((regular_sample$radial_movement),na.rm = TRUE),col = "red", main="Radial movement", xlab="Ratio(radial-tan-radial)", ylim=c(0,max(density(regular_sample$radial_movement)$y)))
  # lines(density((non_regular_sample$radial_movement),na.rm = TRUE))
  # plot(density(abs(non_regular_sample$work_source_distance/1000)),main="Working sources proximity", xlab="distance(km)", xlim=c(0,100))
  # lines(density(abs(regular_sample$work_source_distance/1000)),col = "red")
  return (list(regular_sample, non_regular_sample))
}
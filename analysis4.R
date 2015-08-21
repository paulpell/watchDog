
output_datetime_format <- "%d/%m/%Y %H:%M:%S";

graph_datetime_format <- "%d/%m/%y\n%H:%M";

# this function copies the file and removes the first UTF BOM bytes
# if they are present..
removeUTFBOM <- function(filename)
{
  #create a new file with similar name, with the new contents
  i <- 1;
  new_filename <- paste(filename, "tmp", i, sep="");
  while(file_test("-f", new_filename))
  {
    i <- i + 1;
    new_filename <- paste(filename, "tmp", i, sep="");
  }
  file.create(new_filename);
  
  n = 1024;
  vec <- c(1);
  conRead <- file(description=filename, open="rb", raw=TRUE);
  conWrite <- file(description=new_filename, open="wb", raw=TRUE);
  while (length(vec) != 0)
  {
    vec <- readBin(con=conRead, what="raw", n=n, size=1);
    while (vec[1] == 0xef & vec[2] == 0xbb & vec[3] == 0xbf)
    {
      vec <- vec [4:length(vec)];
    }
    writeBin(vec, conWrite, useBytes=TRUE)
  }
  
  close(conRead);
  close(conWrite);
  
  return(new_filename);
}

# take values only from max start timestamp to min end timestamp
sanitize <- function(dc, dm1, dm2)
{
  # find the biggest starting timestamp
  time_start <- max(dc[1,2], dm1[1,2], dm2[1,2]);
  # find the smallest ending timestamp
  time_stop <- min(dc[length(dc[,1]),2], dm1[length(dm1[,1]),2], dm2[length(dm2[,1]),2]);
  # return the values between the start and stop timestamps
  return (
    list(
      as.data.frame(dc [dc$Timestamp  >= time_start & dc$Timestamp  <= time_stop, ]),
      as.data.frame(dm1[dm1$Timestamp >= time_start & dm1$Timestamp <= time_stop, ]),
      as.data.frame(dm2[dm2$Timestamp >= time_start & dm2$Timestamp <= time_stop, ])
    ));
}

# uses 3 file names (dog, sheep1, sheep2) and returns the data
read_files <- function (file_c, file_m1, file_m2)
{
  file_c2 <- removeUTFBOM(file_c);
  file_m12 <- removeUTFBOM(file_m1);
  file_m22 <- removeUTFBOM(file_m2);
  tmp <- sanitize(
    read.csv(file=file_c2,  sep=DATA_SEP, row.names=NULL),
    read.csv(file=file_m12, sep=DATA_SEP, row.names=NULL),
    read.csv(file=file_m22, sep=DATA_SEP, row.names=NULL)
  );
  
  file.remove(file_c2);
  file.remove(file_m12);
  file.remove(file_m22);
  
  return(tmp);
}

# use a data frame to find the 3D position vectors
# we get (long, lat) positions and need to find a (x,y,z) position
# As described in http://en.wikipedia.org/wiki/Spherical_coordinate_system#Cartesian_coordinates
compute_pos <- function(x)
{
  # first use radians instead of degrees
  longs <- x[,"Longitude"] * pi / 180.;
  lats <-  x[,"Latitude"]  * pi / 180.;
  # initialize the empty result array
  res <- array(0,dim=c(nrow(x), 3));
  
  # fill with the [x,y,z] values given by the formulas below
  res[,1] <- cos(longs) * cos(lats) * RADIUS_EARTH;
  res[,2] <- sin(longs) * cos(lats) * RADIUS_EARTH;
  res[,3] <- sin(lats) * RADIUS_EARTH;
  
  # give the result back
  return(res);
}

# using a Nx3 matrix to compute a (N-1)x3 displacement matrix
# the formula is simply the difference between the consecutive positions:
# dx(t) = x(t+1) - x(t)
compute_dx <- function(x)
{
  # first create the empty result array
  res <- array(0, dim=c(nrow(x)-1,3))
  
  # for each value: the result[i-1] = x[i] - x[i-1]
  for (i in 2:nrow(x))
  {
    res[i-1,] <- x[i,] - x[i-1,];
  }
  return(res)
}

# 3D dot product, gives the alignment between two vectors
# http://en.wikipedia.org/wiki/Dot_product
dotprod3D <- function(x1,x2)
{
  # the dot product is given by the sum of the multiplied corresponding values from both vectors
  return (x1[,1]*x2[,1] + x1[,2]*x2[,2] + x1[,3]*x2[,3]);
}
# norms of 3D vectors
# http://en.wikipedia.org/wiki/Norm_%28mathematics%29#Euclidean_norm
norm3D <- function(x)
{
  # the norm is given by the square root of the sum of the squares of all 3 components of the vector x
  return(sqrt(x[,1]^2 + x[,2]^2 + x[,3]^2));
}

# in this function, we check each entry of both vectors, to see if the timestamp is also in the other vector,
# and we keep only the timestamp values that are present in both vectors!
unify_timestamp <- function (d1, d2)
{
  l1 <- length(d1$Timestamp)
  l2 <- length(d2$Timestamp)
  i1 <- i2 <- 1;
  r1 <- r2 <- c();
  while (i1 <= l1 & i2 <= l2)
  {
    v1 <- d1$Timestamp[i1];
    v2 <- d2$Timestamp[i2]
    if(v1 == v2)
    { 
      r1 <- c(r1,i1);
      r2 <- c(r2,i2);
      i1 <- i1+1;
      i2 <- i2+1;
    } else if (v1 < v2) {
      i1 <- i1 + 1;
    } else {
      i2 <- i2 + 1;
    }
  }
  return (list(d1[r1,], d2[r2,]));
}

# remove NaN values for the sums and means
f_no_nan <- function(x) !is.nan(x);
filter_no_nan <- function(x) return (Filter(f_no_nan, x));
len_no_nan <- function (x) return (length(filter_no_nan(x)));

# says if a value is bigger than 0
f_pos <- function(x) x > 0;

# says if a value is smaller than 0
f_neg <- function(x) x < 0;

# given one vector, remove the duplicated timestamps
remove_duplicates_timestamp <- function(data)
{
  if (is.null(data$timestamp))
    return (data);
  return (data[!duplicated(data$timestamp), ]);
}

#do the job!
handle_dog <- function(data_location)
{
  dog_name <- data_location[1];
  folder <- data_location[2];
  dog_file <- data_location[3];
  sheep1_f <- data_location[4];
  sheep2_f <- data_location[5];
  fixed_point_N <- data_location[6];
  fixed_point_E <- data_location[7];
  writeLines(paste("Traitement du chien ", dog_name, " ....", sep=""));
  
  writeLines(paste("Reading from folder " , folder,"\n: dog_file = ", dog_file));
  
  # get the data using the functions
  all_data <- read_files(dog_file, sheep1_f, sheep2_f);
  
  # separating the data for the 3 animals:
  # d_c contains the dog data, d_mX the data for the sheep X
  d_c <- as.data.frame(remove_duplicates_timestamp(all_data[1]));
  d_m1<- as.data.frame(remove_duplicates_timestamp(all_data[2]));
  d_m2<- as.data.frame(remove_duplicates_timestamp(all_data[3]));
  
  # not sure if this is needed:
  # It is needed when the timestamps are not the same in all the files.
  if (length(d_c[,1]) != length(d_m1[,1]) || length(d_c[,1]) != length(d_m2[,1]) || length(d_m1[,1]) != length(d_m2[,1]))
  {
    tmp1 <- unify_timestamp(d_c,d_m1);
    tmp2 <- unify_timestamp(tmp1[[1]], d_m2);
    d_c <- tmp2[[1]];
    tmp3 <- unify_timestamp(tmp1[[2]], tmp2[[2]]);
    d_m1 <- tmp3[[1]];
    d_m2 <- tmp3[[2]];
  }
  
  # get the positions
  pos_c  <- compute_pos(d_c [,c("Latitude","Longitude")]);
  pos_m1 <- compute_pos(d_m1[,c("Latitude","Longitude")]);
  pos_m2 <- compute_pos(d_m2[,c("Latitude","Longitude")]);
  # compute the displacements
  dist_c  <- compute_dx(pos_c);
  dist_m1 <- compute_dx(pos_m1);
  dist_m2 <- compute_dx(pos_m2);
  
  # distance from the dog to the fixed point
  pos_fp <- compute_pos(data.frame(Latitude=c(as.double(fixed_point_N), 0.0), Longitude=c(as.double(fixed_point_E), 0.0)))[1,];
  dist_c_fp <- array(0,dim=dim(pos_c));
  for (i in seq(1,dim(dist_c_fp)[1]))
    dist_c_fp[i,] <- pos_c[i,] - pos_fp;
  norms_c_fp <- norm3D(dist_c_fp);
  sum_dist_fp <- sum(norms_c_fp);
  mean_dist_fp <- mean(norms_c_fp);
  
  # one entry in these vectors gives how much displacement the animal made between two positions, by definition
  # of the norm as the euclidean distance
  norms_c  <- norm3D(dist_c);
  norms_m1 <- norm3D(dist_m1);
  norms_m2 <- norm3D(dist_m2);
  
  # total distances covered by dog and sheep
  # the total is given by the sum of all displacements
  dist_abs_c  <- sum(norms_c);
  dist_abs_m1 <- sum(norms_m1);
  dist_rel_m1 <- dist_abs_c / dist_abs_m1;
  dist_abs_m2 <- sum(norms_m2);
  dist_rel_m2 <- dist_abs_c / dist_abs_m2;
  
  # distance from dog to each sheep
  dist_c_m1_tmp <- pos_c - pos_m1;
  dist_c_m1     <- norm3D(dist_c_m1_tmp);
  dist_c_m2_tmp <- pos_c - pos_m2;
  dist_c_m2     <- norm3D(dist_c_m2_tmp);
  dist_c_m <- (dist_c_m1 + dist_c_m2) / 2; # distance to the middle of the 2 sheep
  mean_dist_c_m <- mean(dist_c_m);
  median_dist_c_m <- median(dist_c_m);  
  
  # distance between sheep
  dist_m1_m2 <- norm3D(pos_m1 - pos_m2);
  
  # distance to the closest sheep
  dist_c_closest <- pmin(dist_c_m1, dist_c_m2);
  mean_dist_c_closest <- mean(dist_c_closest);
  median_dist_c_closest <- median(dist_c_closest);         
  
  # dot product sheep-dog: gives the alignment one with the other (http://en.wikipedia.org/wiki/Dot_product)
  dp_c_m1 <- dotprod3D(dist_c, dist_m1);
  dp_c_m2 <- dotprod3D(dist_c, dist_m2);
  
  # alignment of dog relative to each sheep
  align_pos_c_m1 <- dp_c_m1 / norms_c / norms_m1;
  align_pos_c_m2 <- dp_c_m2 / norms_c / norms_m2;
  
  
  # position of dog relative to sheep: vector subtraction
  rel_pos_c_m1 <- (pos_c - pos_m1)[2:nrow(pos_c),];
  rel_pos_c_m2 <- (pos_c - pos_m2)[2:nrow(pos_c),];
  # is dog in front?
  c_front_m1 <- dotprod3D(rel_pos_c_m1, dist_c) / norms_c / norm3D(rel_pos_c_m1);
  c_front_m2 <- dotprod3D(rel_pos_c_m2, dist_c) / norms_c / norm3D(rel_pos_c_m2);
  
  # filter out when the distance between dog and sheep is too big
  for (i in 1:length(c_front_m1))
  {
    # if the distance is bigger than the filter distance, we put NaN, so the value will be ignored
    if (abs(dist_c_m[i]) > filter_dist)
    {
      c_front_m1[i]     <- NaN;
      c_front_m2[i]     <- NaN;
      align_pos_c_m1[i] <- NaN;
      align_pos_c_m2[i] <- NaN;
    }
  }
  
  c_front_m1_no_nan <- filter_no_nan (c_front_m1);
  c_front_m2_no_nan <- filter_no_nan (c_front_m2);
  
  c_align_m1_no_nan <- filter_no_nan (align_pos_c_m1);
  c_align_m2_no_nan <- filter_no_nan (align_pos_c_m2);
  
  mean_coord_m1 <- mean(c_front_m1 * align_pos_c_m1, na.rm=TRUE);
  mean_coord_m2 <- mean(c_front_m2 * align_pos_c_m2, na.rm=TRUE);
  
  # positive-negative alignment
  #
  coord_m1_positive_alignment <- align_pos_c_m1;
  coord_m2_positive_alignment <- align_pos_c_m2;
  coord_m1_negative_alignment <- align_pos_c_m1;
  coord_m2_negative_alignment <- align_pos_c_m2;
  for (i in 1:length(align_pos_c_m1))
  {
    coord_m1_positive_alignment[i] <- NaN;
    coord_m2_positive_alignment[i] <- NaN;
    coord_m1_negative_alignment[i] <- NaN;
    coord_m2_negative_alignment[i] <- NaN;
    
    #
    a <- c_front_m1[i];
    b <- align_pos_c_m1[i];
    c <- c_front_m2[i];
    d <- align_pos_c_m2[i];
    # dog - m1
    if (!is.nan(a) && !is.nan(b))
    {
      if (b >= 0) coord_m1_positive_alignment[i] <- a * b;
      if (b < 0)  coord_m1_negative_alignment[i] <- a * b;
    }
    # dog - m2
    if (!is.nan(c) && !is.nan(d))
    {
      if (d >= 0) coord_m2_positive_alignment[i] <- c * d;
      if (d < 0)  coord_m2_negative_alignment[i] <- c * d;
    }
    
  }
  
  
  mean_coord_align_pos_m1 <- mean(coord_m1_positive_alignment, na.rm=TRUE);
  mean_coord_align_neg_m1 <- mean(coord_m1_negative_alignment, na.rm=TRUE);
  mean_coord_align_pos_m2 <- mean(coord_m2_positive_alignment, na.rm=TRUE);
  mean_coord_align_neg_m2 <- mean(coord_m2_negative_alignment, na.rm=TRUE);
  
  mean_coord_align_pos_right_m1 <- mean(Filter(f_pos, coord_m1_positive_alignment), na.rm=T);
  mean_coord_align_pos_left_m1 <- mean(Filter(f_neg, coord_m1_positive_alignment), na.rm=T);
  mean_coord_align_neg_right_m1 <- mean(Filter(f_pos, coord_m1_negative_alignment), na.rm=T);
  mean_coord_align_neg_left_m1 <- mean(Filter(f_neg, coord_m1_negative_alignment), na.rm=T);
  mean_coord_align_pos_right_m2 <- mean(Filter(f_pos, coord_m2_positive_alignment), na.rm=T);
  mean_coord_align_pos_left_m2 <- mean(Filter(f_neg, coord_m2_positive_alignment), na.rm=T);
  mean_coord_align_neg_right_m2 <- mean(Filter(f_pos, coord_m2_negative_alignment), na.rm=T);
  mean_coord_align_neg_left_m2 <- mean(Filter(f_neg, coord_m2_negative_alignment), na.rm=T);
  
  mean_front_m1 <- mean(c_front_m1_no_nan);
  mean_front_m2 <- mean(c_front_m2_no_nan);
  
  
  mean_align_m1 <- mean(c_align_m1_no_nan);
  mean_align_m2 <- mean(c_align_m2_no_nan);
  
  sum_barking <- sum(d_c$Barking);
  
  barking_timestamps <- Filter(function(x)x!=F, ifelse(d_c$Barking == 1, d_c$Timestamp, F));
  barking_values <- rep(0, length(barking_timestamps));
  
  #################################
  # write some results out to the console
  fp_str <- paste(fixed_point_N,fixed_point_E);
  wrtr <- function ( key , args = c() )
  {
    writeLines ( get_translation (key, args) );
  }
  wrtr ("dist_dog",                        c(dist_abs_c));
  wrtr ("dist_sheep",                      c(1,dist_abs_m1));
  wrtr ("dist_sheep_rel",                  c(1,dist_rel_m1));
  wrtr ("dist_sheep",                      c(2,dist_abs_m2));
  wrtr ("dist_sheep_rel",                  c(2,dist_rel_m2));
  wrtr ("mean_dist_closest",               c(mean_dist_c_closest));
  wrtr ("median_dist_closest",             c(median_dist_c_closest));
  wrtr ("mean_dist_middle",                c(mean_dist_c_m));
  wrtr ("median_dist_middle",              c(median_dist_c_m));
  wrtr ("dog_in_front1000",                c(1, mean_front_m1 * 1000));
  wrtr ("dog_in_front1000",                c(2, mean_front_m2 * 1000));
  wrtr ("dog_aligned100",                  c(1, mean_align_m1 * 100));
  wrtr ("dog_aligned100",                  c(2, mean_align_m2 * 100));
  wrtr ("coord_align_pos",                 c(1, mean_coord_align_pos_m1));
  wrtr ("coord_align_neg",                 c(1, mean_coord_align_neg_m1));
  wrtr ("coord_align_pos",                 c(2, mean_coord_align_pos_m2));
  wrtr ("coord_align_neg",                 c(2, mean_coord_align_neg_m2));
  wrtr ("num_barkings",                    c(sum_barking));
  wrtr ("fixed_pt",                        c(fp_str));
  wrtr ("mean_dist_fixed_pt",              c(mean_dist_fp));
  wrtr ("res_mc1",                         c(mean_coord_align_pos_right_m1));
  wrtr ("res_mc2",	                       c(mean_coord_align_pos_left_m1));
  wrtr ("res_mc3",	                       c(mean_coord_align_neg_right_m1));
  wrtr ("res_mc4",	                       c(mean_coord_align_neg_left_m1));
  wrtr ("res_mc5",	                       c(mean_coord_align_pos_right_m2));
  wrtr ("res_mc6",	                       c(mean_coord_align_pos_left_m2));
  wrtr ("res_mc7",	                       c(mean_coord_align_neg_right_m2));
  wrtr ("res_mc8",	                       c(mean_coord_align_neg_left_m2));
  
  
  #################################
  # create graphs
  
  writeLines("\ncreating graphs, takes time!");

  # first format date and time for the axis
  datetime_input_format <-
    if (is.null(d_c$Time_Date))
      "%Y-%m-%d %H:%M:%S"
    else
      "%H:%M:%S-%d.%m.%Y";
  datetime_vector <-
    if (is.null(d_c$Time_Date))
      d_c$Date_Time
    else
      d_c$Time_Date;

  x_time_data <- as.POSIXct(datetime_vector, format=datetime_input_format);
  #start_date_time <- strptime(datetime_vector[1], format=datetime_input_format);
  start_date_time <- x_time_data[1];
  #end_date_time <- strptime(d_c$Date_Time[length(d_c$Date_Time)], format=datetime_input_format);
#  end_date_time <- strptime(tail(datetime_vector, n=1), format=datetime_input_format);
  end_date_time <- tail(x_time_data, n=1);
  axis_dates <<- seq.POSIXt(start_date_time, end_date_time, by=1.5*60*60);
  #  x_time_data <- as.POSIXct(d_c$Time_Date);
  #  start_date_time <- strptime(d_c$Time_Date[1], );
  #  end_date_time <- strptime(d_c$Time_Date[length(d_c$Time_Date)], "%H:%M:%S-%d.%m.%Y");
  #  axis_dates <<- seq.POSIXt(start_date_time, end_date_time, by=1.5*60*60);
  duration <- pretty_time(end_date_time - start_date_time);
  
  axis_labels <<- format(axis_dates, graph_datetime_format);

  

#  writeLines(("axis_dates="));#, range(axis_dates)));
#  print(axis_dates);
#  writeLines(("range(x_time_data)="));#, range(x_time_data)));
#  print(x_time_data);
#  writeLines(paste("rstart_date_time=", start_date_time));
#  writeLines(paste("end_date_time=", end_date_time));
#  writeLines(("axis_labels="));#,axis_labels));
#  print(axis_labels);

  #width <- 12;
  #
  #xlim_max <- 0.1;
  #hist_default_breaks <- seq(0, 2, xlim_max / HISTOGRAM_CLASSES);
  #hist_default_xlim <- c(0,xlim_max);

  draw_graphs_1dog <- function()
  {
  
    setPDFFolder ( folder );
    
    key <- "graph_dist_fp";
    args <- c(dog_name, fp_str);
    writeLines(paste("Key=",key,", args=", args));
    test1 <- get_trans_filename(key, args);
    writeLines(test1);
    test2 <- get_translation(key, args);
    writeLines(test2);
    makePlot( x=x_time_data, y=norms_c_fp, name_transl_key=key, name_transl_args=args );
    #CairoPDF(paste(folder,get_trans_filename("graph_dist_fp", c(dog_name, fp_str)),".pdf",sep=""), width=width);
    #plot(d_c$Timestamp, norms_c_fp, type="l", main=get_translation("graph_dist_fp", c(dog_name, fp_str)), ylab=get_translation("dist_km"), xlab=get_translation("date"), xaxt="n");
    #axis(1,at=axis_dates, labels=axis_labels, padj=0.5);
    #dev.off();
    writeLines("1: dist_chien_fixedpoint created");
  
    
    key <- "hist_dist_fp";
    args <- c(dog_name, fp_str);
    makeHist( x=norms_c_fp, name_transl_key=key, name_transl_args=args, useDefaultBreaks=FALSE );
    #CairoPDF(paste(folder,get_trans_filename("hist_dist_fp", c(dog_name, fp_str)),".pdf",sep=""), width=width);
    #med <- median(norms_c_fp);
    #breaks <- seq(0,2*med,2*med / HISTOGRAM_CLASSES);
    #hist(norms_c_fp, breaks=breaks, xlim=c(0,2*med), xlab=get_translation("dist_km"), ylab=get_translation("freq"), main=get_translation("hist_dist_fp",c(dog_name, fp_str)),sub=get_translation("red_median", c(signif(med,digits=4))));
    #abline(v=med, col="red", lwd=2); # add a red line for the median
    #dev.off();
    writeLines("2: dist_chien_fixedpoint created");
    
    key <- "graph_closest";
    args <- c(dog_name);
    makePlot( x=x_time_data, y=dist_c_closest, name_transl_key=key, name_transl_args=args );
    #CairoPDF(paste(folder,get_trans_filename("graph_closest", c(dog_name)),".pdf",sep=""), width=width);
    #plot(d_c$Timestamp, dist_c_closest, type="l", main=get_translation("graph_closest", c(dog_name)), ylab=get_translation("dist_km"), xlab=get_translation("date"), xaxt="n");
    #axis(1,at=axis_dates, labels=axis_labels, padj=0.5);
    #dev.off();
    writeLines("3: dist_chien created");
    
    # normalised distance dog-sheep mouton le plus proche
    key <- "graph_closest_norm";
    args <- c(dog_name);
    makePlot ( x=x_time_data, y=dist_c_closest, name_transl_key=key, name_transl_args=args, ylim=c(0,1));
    #CairoPDF(paste(folder,get_trans_filename("graph_closest_norm", c(dog_name)),".pdf",sep=""), width=width);
    #plot(d_c$Timestamp, dist_c_closest, type="l", main=get_translation("graph_closest_norm", c(dog_name)), ylab=get_translation("dist_km"), xlab=get_translation("date"), ylim=c(0,1), xaxt="n");
    ##points(barking_timestamps, barking_values, col="red");
    #axis(1,at=axis_dates, labels=axis_labels, padj=0.5);
    #dev.off();
    writeLines("4: dist_chien_normalised created");
     
    # histogram of the distances dog-sheep mouton le plus proche
    key <- "graph_hist_dist_closest";
    args <- c(dog_name);
    makeHist ( x=dist_c_closest, name_transl_key=key, name_transl_args=args, useDefaultBreaks=FALSE );
    #CairoPDF(paste(folder,get_trans_filename("graph_hist_dist_closest",c(dog_name)),".pdf",sep=""), width=width);
    #med <-median(dist_c_closest);
    #hist(dist_c_closest, breaks=hist_default_breaks, xlim=hist_default_xlim, xlab=get_translation("dist_km"), ylab=get_translation("freq"), main=get_translation("graph_hist_dist_closest",c(dog_name)),sub=get_translation("red_median", c(signif(med,digits=4))));
    #abline(v=median(dist_c_closest), col="red", lwd=2); # add a red line for the median
    #dev.off();
    writeLines("5: histogram distance dog-sheep created");
    
    # distance dog-sheep ? la position moyenne du mouton
    key <- "graph_dist_mean";
    args <- c(dog_name);
    makePlot( x=x_time_data, y=dist_c_m, name_transl_key=key, name_transl_args=args);
    #CairoPDF(paste(folder,get_trans_filename("graph_dist_mean", c(dog_name)), ".pdf",sep=""), width=width);
    #plot(d_c$Timestamp, dist_c_m, type="l", main=get_translation("graph_dist_mean", c(dog_name)), ylab=get_translation("dist_km"), xlab=get_translation("date"), xaxt="n");
    #axis(1,at=axis_dates, labels=axis_labels, padj=0.5);
    #dev.off();
    writeLines("6: dist_chien created");
    
    # normalised distance dog-sheep ? la position moyenne du mouton
    key <- "graph_dist_mean_norm";
    args <- c(dog_name);
    makePlot( x=x_time_data, y=dist_c_m, name_transl_key=key, name_transl_args=args, ylim=c(0,1));
    #CairoPDF(paste(folder,get_trans_filename("graph_dist_mean_norm", c(dog_name)), ".pdf",sep=""), width=width);
    #plot(d_c$Timestamp, dist_c_m, type="l", main=get_translation("graph_dist_mean_norm", c(dog_name)), ylab=get_translation("dist_km"), xlab=get_translation("date"), ylim=c(0,1), xaxt="n");
    #axis(1,at=axis_dates, labels=axis_labels, padj=0.5);
    #dev.off();
    writeLines("7: dist_chien_normalised created");
      
    # histogram of the distances dog-sheep ? la position moyenne du mouton
    key <- "graph_hist_dist_mean";
    args <- c(dog_name);
    makeHist ( x=dist_c_m, name_transl_key=key, name_transl_args=args, useDefaultBreaks=FALSE );
    #CairoPDF(paste(folder,get_trans_filename("graph_hist_dist_mean", c(dog_name)), ".pdf",sep=""), width=width);
    #med <-median(dist_c_m);
    #hist(dist_c_m, breaks=hist_default_breaks, xlim=hist_default_xlim, main=get_translation("graph_hist_dist_mean", c(dog_name)), sub=get_translation("red_median",c(signif(med,digits=4))), xlab=get_translation("dist_km"), ylab=get_translation("freq"));
    #abline(v=med, col="red", lwd=2); # add a red line for the median
    #dev.off();
    writeLines("8: histogram distance dog-meansheep created"); 
     
    # Distance CPT aux 2 moutons
  
    key <- "graph_dist_both_sheep";
    args <- c(dog_name);
    data_m1 <- as.data.frame(dist_c_m1);
    data_m2 <- as.data.frame(dist_c_m2);
    extraFn <- function ()
    {
      lines(x_time_data, data_m2[,1], type='l', col='green', xaxt='n');
    }
    makePlot ( x=x_time_data, y=data_m1[,1], name_transl_key=key, name_transl_args=args, col="blue", extraFn=extraFn );
    #CairoPDF(paste(folder,get_trans_filename("graph_dist_both_sheep", c(dog_name)),".pdf",sep=""), width=width);
    #{
    #  data_m1 <- as.data.frame(dist_c_m1);
    #  data_m2 <- as.data.frame(dist_c_m2);
    #  plot(d_c$Timestamp, data_m1[,1], main=get_translation("graph_dist_both_sheep", c(dog_name)), type="l", ylab=get_translation("dist_km"), xlab=get_translation("date"), xaxt="n", col="blue");
    #  lines(d_c$Timestamp, data_m2[,1], type='l', col='green', xaxt='n');
    #  axis(1,at=axis_dates, labels=axis_labels, padj=0.5);
    #}
    #dev.off();
    writeLines("9: plot distance dog-bothsheep created"); 
  


    # orientation + in front of
    
    #can_draw_align_front <- TRUE;
    #if (length(c_align_m1_no_nan) == 0 || length(c_align_m2_no_nan) == 0 || length(c_front_m1_no_nan) == 0 || length(c_front_m2_no_nan) == 0)
      #can_draw_align_front <- FALSE;
    can_draw_align_front <- 
      ! (
           length(c_align_m1_no_nan) == 0
        || length(c_align_m2_no_nan) == 0
        || length(c_front_m1_no_nan) == 0
        || length(c_front_m2_no_nan) == 0
      );
    
    if (can_draw_align_front)
    {
      has_coord_m1pos <- len_no_nan (coord_m1_positive_alignment) != 0;
      has_coord_m2pos <- len_no_nan (coord_m2_positive_alignment) != 0;
      has_coord_m1neg <- len_no_nan (coord_m1_negative_alignment) != 0;
      has_coord_m2neg <- len_no_nan (coord_m2_negative_alignment) != 0;
      max_freq <- max(
        if (has_coord_m1pos) max(hist(coord_m1_positive_alignment, plot=FALSE)$counts) else 0,
        if (has_coord_m2pos) max(hist(coord_m2_positive_alignment, plot=FALSE)$counts) else 0,
        if (has_coord_m1neg) max(hist(coord_m1_negative_alignment, plot=FALSE)$counts) else 0,
        if (has_coord_m2neg) max(hist(coord_m2_negative_alignment, plot=FALSE)$counts) else 0
      );
      
      
      # create the histogram with columns on top and bottom
      
      #red <- rgb(1,0,0,0.5);
      #yellow <- rgb(1,1,0,0.5);
      #blue <- rgb(0,0,1,0.5);
      #green <- rgb(0,1,0,0.5);
      text_x_left <- -0.5;
      text_x_right <- 0.5;
      max_pos_freq <- 0;
      bs <- seq(-1,1,1/HISTOGRAM_CLASSES);
      if (has_coord_m1pos) hist_coord_pos_m1 <- hist(coord_m1_positive_alignment, plot=FALSE, breaks=bs);
      if (has_coord_m1neg) hist_coord_neg_m1 <- hist(coord_m1_negative_alignment, plot=FALSE, breaks=bs);
      if (has_coord_m2pos) hist_coord_pos_m2 <- hist(coord_m2_positive_alignment, plot=FALSE, breaks=bs);
      if (has_coord_m2neg) hist_coord_neg_m2 <- hist(coord_m2_negative_alignment, plot=FALSE, breaks=bs);
      sum_m1 <- (if (has_coord_m1pos) sum(hist_coord_pos_m1$counts) else 0) + (if (has_coord_m1neg) sum(hist_coord_neg_m1$counts) else 0);
  
      if (has_coord_m1pos) hist_coord_pos_m1$counts <- hist_coord_pos_m1$counts / sum_m1;
      if (has_coord_m1neg) hist_coord_neg_m1$counts <- hist_coord_neg_m1$counts / sum_m1;
      max1 <- max(if (has_coord_m1pos) max(hist_coord_pos_m1$counts) else 0, if (has_coord_m1neg) max(hist_coord_neg_m1$counts) else 0);
      if (max1 > max_pos_freq) max_pos_freq <- max1;
      sum_m2 <- (if (has_coord_m2pos) sum(hist_coord_pos_m2$counts) else 0) + (if (has_coord_m2neg) sum(hist_coord_neg_m2$counts) else 0);
      if (has_coord_m2pos) hist_coord_pos_m2$counts <- hist_coord_pos_m2$counts / sum_m2;
      if (has_coord_m2neg) hist_coord_neg_m2$counts <- hist_coord_neg_m2$counts / sum_m2;
      max2 <- max(if (has_coord_m2pos) max(hist_coord_pos_m2$counts) else 0, if (has_coord_m2neg) max(hist_coord_neg_m2$counts) else 0);
      if (max2 > max_pos_freq) max_pos_freq <- max2;
  
      mean_pos_m1 <- if (has_coord_m1pos) mean(hist_coord_pos_m1$counts, na.rm=TRUE) else 0;
      mean_neg_m1 <- if (has_coord_m1neg) mean(hist_coord_neg_m1$counts, na.rm=TRUE) else 0;
      mean_pos_m2 <- if (has_coord_m2pos) mean(hist_coord_pos_m2$counts, na.rm=TRUE) else 0;
      mean_neg_m2 <- if (has_coord_m2neg) mean(hist_coord_neg_m2$counts, na.rm=TRUE) else 0;
      
      hist_posneg_labels_top <- c(get_translation("graph_align_pos_left_label"), get_translation("graph_align_pos_right_label"));
      hist_posneg_labels_bot <- c(get_translation("graph_align_neg_left_label"), get_translation("graph_align_neg_right_label"));
      text_y_top <- 1.03 * max_pos_freq;
      text_y_bottom <- 1.03 * -max_pos_freq;
      CairoPDF(paste(folder,get_translation("graph_hist_coord_both_sheep_filename",c(dog_name)),"_",1000*filter_dist,"m.pdf", sep=""), width=17);
      par(mfrow=c(1,2));
      if (has_coord_m1pos) plot(hist_coord_pos_m1, ylim=c(-max_pos_freq, max_pos_freq), col="red", xlab="", ylab=get_translation("rel_freq"), main=get_translation("sheep_i", c(1)));
      if (has_coord_m1neg)
      {
        hist_coord_neg_m1$counts <- -hist_coord_neg_m1$counts;
        if (has_coord_m1pos) lines(hist_coord_neg_m1, col="blue")
        else plot(hist_coord_neg_m1, ylim=c(-max_pos_freq, max_pos_freq), col="red", xlab="", ylab=get_translation("rel_freq"), main=get_translation("sheep_i", c(1)));
      }
      text(x=c(text_x_left, text_x_right), y=c(text_y_top), labels=hist_posneg_labels_top);
      text(x=c(text_x_left, text_x_right), y=c(text_y_bottom), labels=hist_posneg_labels_bot);
      abline(v=0);
      if (has_coord_m2pos) plot(hist_coord_pos_m2, ylim=c(-max_pos_freq, max_pos_freq), col="yellow", xlab="", ylab=get_translation("rel_freq"), main=get_translation("sheep_i", c(2)));
      if (has_coord_m2neg)
      {
        hist_coord_neg_m2$counts <- -hist_coord_neg_m2$counts;
        if (has_coord_m2pos) lines(hist_coord_neg_m2, col="green")
        else plot(hist_coord_neg_m2, ylim=c(-max_pos_freq, max_pos_freq), col="yellow", xlab="", ylab=get_translation("rel_freq"), main=get_translation("sheep_i", c(2)));
      }
      text(x=c(text_x_left, text_x_right), y=c(text_y_top), labels=hist_posneg_labels_top);
      text(x=c(text_x_left, text_x_right), y=c(text_y_bottom), labels=hist_posneg_labels_bot);
      abline(v=0);
      par(xpd=NA);
      par(fig=c(0.49,0.51,0.95,1));
      par(font=2);
      text(x=c(0),y=c(0), labels=c(get_translation("graph_hist_coord_both_sheep", c(dog_name))), cex=1.5);
      dev.off();
      writeLines("histogram separated alignments created");
      
    } else {
      writeLines("dog_sheep_orientation + in_front NOT created");
      writeLines("Coordination dog-sheep graph NOT created");
      writeLines("histogram separated alignments NOT created");
    }
    
    writeLines("");
    writeLines("");
  
  
  }

  makeGraphs1Dog <- FALSE;
  if (makeGraphs1Dog)
    draw_graphs_1dog()
  else
    writeLines(paste("SKIPPING GRAPHS FOR DOG", dog_name));

  
    
  # return the distance to the closest sheep for outside the function
  csv_results <- data.frame(
    res_n=c(dog_name),
    res_d1=c(dist_abs_c),
    res_d2=c(dist_abs_m1),
    res_d3=c(dist_abs_m2),
    res_d4=c(dist_rel_m1),
    res_d5=c(dist_rel_m2),
    res_d6=c(mean_dist_c_m*1000),
    res_d7=c(mean_dist_c_closest*1000),
    res_d8=c(median_dist_c_m*1000),
    res_d9=c(median_dist_c_closest*1000),
    res_c1=c(mean_front_m1*1000),
    res_c2=c(mean_front_m2*1000),
    res_c3=c(mean_align_m1*100),
    res_c4=c(mean_align_m2*100),
    res_c5=c(mean_coord_m1*100),
    res_c6=c(mean_coord_m2*100),
    res_c7 = c(mean_coord_align_pos_m1*100),
    res_c8 = c(mean_coord_align_neg_m1*100),
    res_c9 = c(mean_coord_align_pos_m2*100),
    res_c10 = c(mean_coord_align_neg_m2*100),
    res_mc1 = c(mean_coord_align_pos_right_m1*100),
    res_mc2 = c(mean_coord_align_pos_left_m1 *100),
    res_mc3 = c(mean_coord_align_neg_right_m1*100),
    res_mc4 = c(mean_coord_align_neg_left_m1 *100),
    res_mc5 = c(mean_coord_align_pos_right_m2*100),
    res_mc6 = c(mean_coord_align_pos_left_m2 *100),
    res_mc7 = c(mean_coord_align_neg_right_m2*100),
    res_mc8 = c(mean_coord_align_neg_left_m2 *100),
    res_fp1 = c(fp_str),
    res_fp2 = c(mean_dist_fp),
    res_t1  = c(format(start_date_time, output_datetime_format)),
    res_t2  = c(format(end_date_time, output_datetime_format)),
    res_t3  = c(duration)
  );
  
  
  
  # create a CSV file with the distance data, for the Italian friend of Ueli
  distance_data <- data.frame(
    timestamp =                     d_c[,"Timestamp"],
    date_time =                     if (is.null(d_c$Time_Date)) d_c$Date_Time else d_c$Time_Date,
    coord_dog_latitude =            d_c[,"Latitude"],
    coord_dog_longitude =           d_c[,"Longitude"],
    coord_sheep1_latitude =         d_m1[,"Latitude"],
    coord_sheep1_longitude =        d_m1[,"Longitude"],
    coord_sheep2_latitude =         d_m2[,"Latitude"],
    coord_sheep2_longitude =        d_m2[,"Longitude"],
    displacement_dog_m =            1000 * append(c(0), norms_c),
    displacement_sheep1_m =         1000 * append(c(0), norms_m1),
    displacement_sheep2_m =         1000 * append(c(0), norms_m2),
    distance_dog_sheep1_m =         1000 * dist_c_m1,
    distance_dog_sheep2_m =         1000 * dist_c_m2,
    distance_sheep1_sheep2_m =      1000 * dist_m1_m2,
    distance_dog_closest_sheep_m =  1000 * dist_c_closest,
    distance_dog_middle_sheep_m =   1000 * dist_c_m   
  );
  
  dist_filename <- paste(folder, "distances_",dog_name, ".csv", sep="");
  write.csv(distance_data, dist_filename, row.names=F);
  writeLines(paste("Distances exporté dans le fichier \"", dist_filename, "\""));
  
  
  writeLines(paste("\ntraitement du chien ", dog_name, " fini!", sep=""));
  
  results <- list(
    csv_results,
    dist_c_closest,
    dist_c_m1,
    dist_c_m2,
    dist_c_m,
    coord_m1_positive_alignment,
    coord_m1_negative_alignment,
    coord_m2_positive_alignment,
    coord_m2_negative_alignment
  );
  
  return (results);
}

handle_dogs <- function (base_folder, data_location)
{
  size_each_entry <- 7;
  number_dogs <- length(data_location) / size_each_entry;
  data_location <- array(data_location, dim =c(size_each_entry,number_dogs));
  dog_names <- data_location[1,];
  
  results_csv <- NULL;
  results_dist_closest <- NULL;
  results_dist_m1 <- NULL;
  results_dist_m2 <- NULL;
  results_dist_mean <- NULL;
  results_coord_pos_align_m1 <- NULL;
  results_coord_neg_align_m1 <- NULL;
  results_coord_pos_align_m2 <- NULL;
  results_coord_neg_align_m2 <- NULL;
  
  # do the job!
  for (i in 1:number_dogs)
  {
    tmp_results <- handle_dog(data_location[,i]);
    results_csv <- rbind(results_csv, as.data.frame(tmp_results[1]));
    results_dist_closest <- c(results_dist_closest, as.data.frame(tmp_results[2]));
    results_dist_m1 <- c(results_dist_m1, as.data.frame(tmp_results[3]));
    results_dist_m2 <- c(results_dist_m2, as.data.frame(tmp_results[4]));
    results_dist_mean <- c(results_dist_mean, as.data.frame(tmp_results[5]));
    results_coord_pos_align_m1 <- c(results_coord_pos_align_m1, as.data.frame(tmp_results[6]));
    results_coord_neg_align_m1 <- c(results_coord_neg_align_m1, as.data.frame(tmp_results[7]));
    results_coord_pos_align_m2 <- c(results_coord_pos_align_m2, as.data.frame(tmp_results[8]));
    results_coord_neg_align_m2 <- c(results_coord_neg_align_m2, as.data.frame(tmp_results[9]));
  }
  
  
  # write the results of all dogs in a CSV file which can be imported in spreadsheets
  results_csv_list <- list();
  results_csv_list[[get_translation("res_n")]] <- results_csv$res_n;
  results_csv_list[[get_translation("res_d1")]] <- results_csv$res_d1;
  results_csv_list[[get_translation("res_d2")]] <- results_csv$res_d2;
  results_csv_list[[get_translation("res_d3")]] <- results_csv$res_d3;
  results_csv_list[[get_translation("res_d4")]] <- results_csv$res_d4;
  results_csv_list[[get_translation("res_d5")]] <- results_csv$res_d5;
  results_csv_list[[get_translation("res_d6")]] <- results_csv$res_d6;
  results_csv_list[[get_translation("res_d7")]] <- results_csv$res_d7;
  results_csv_list[[get_translation("res_d8")]] <- results_csv$res_d8;
  results_csv_list[[get_translation("res_d9")]] <- results_csv$res_d9;
  results_csv_list[[get_translation("res_c1")]] <- results_csv$res_c1;
  results_csv_list[[get_translation("res_c2")]] <- results_csv$res_c2;
  results_csv_list[[get_translation("res_c3")]] <- results_csv$res_c3;
  results_csv_list[[get_translation("res_c4")]] <- results_csv$res_c4;
  results_csv_list[[get_translation("res_c5")]] <- results_csv$res_c5;
  results_csv_list[[get_translation("res_c6")]] <- results_csv$res_c6;
  results_csv_list[[get_translation("res_c7")]] <- results_csv$res_c7;
  results_csv_list[[get_translation("res_c8")]] <- results_csv$res_c8;
  results_csv_list[[get_translation("res_c9")]] <- results_csv$res_c9;
  results_csv_list[[get_translation("res_c10")]] <- results_csv$res_c10;
  results_csv_list[[get_translation("res_mc1")]] <- results_csv$res_mc1;
  results_csv_list[[get_translation("res_mc2")]] <- results_csv$res_mc2;
  results_csv_list[[get_translation("res_mc3")]] <- results_csv$res_mc3;
  results_csv_list[[get_translation("res_mc4")]] <- results_csv$res_mc4;
  results_csv_list[[get_translation("res_mc5")]] <- results_csv$res_mc5;
  results_csv_list[[get_translation("res_mc6")]] <- results_csv$res_mc6;
  results_csv_list[[get_translation("res_mc7")]] <- results_csv$res_mc7;
  results_csv_list[[get_translation("res_mc8")]] <- results_csv$res_mc8;
  results_csv_list[[get_translation("res_t1")]] <- results_csv$res_t1;
  results_csv_list[[get_translation("res_t2")]] <- results_csv$res_t2;
  results_csv_list[[get_translation("res_t3")]] <- results_csv$res_t3;
  results_csv_list[[get_translation("res_fp1")]] <- results_csv$res_fp1;
  results_csv_list[[get_translation("res_fp2")]] <- results_csv$res_fp2;
  
  csv_file <- paste(base_folder,get_translation("name_csv"),".csv",sep="");
  write.csv(results_csv_list, file=csv_file, row.names=FALSE);
  
  writeLines(paste("result data exported to",csv_file));
  
  
  #if (number_dogs > 1)
  {
    writeLines( paste("creating comparison graphs, exporting to:\n   ",base_folder));

    setPDFFolder ( base_folder );
   
    ncols <- 2;
    nrows <- as.integer(ceiling(number_dogs/ncols));
    mfrow <- c(nrows, ncols);

    width <- ncols * 10;
    height <- nrows * 6;
    
    title_y <- min(0.99, 1 -0.08 / 2^floor((number_dogs - 1) / 2));
    title_fig <- c(0.49,0.51,title_y, 1);
      
    xlab_multidog <- paste("\n", get_translation("time"));

    datetime_starts <- strptime(results_csv$res_t1, format=output_datetime_format);
    datetime_ends   <- strptime(results_csv$res_t2, format=output_datetime_format);
    datetime_labels_at <- list();
    datetime_labels    <- list();
    datetime_xaxis     <- list();
    for (i in seq(1, number_dogs))
    {
        datetime_xaxis[[i]] <- seq.POSIXt(datetime_starts[i], datetime_ends[i], by=1);
        datetime_labels_at[[i]] <- seq.POSIXt(datetime_starts[i], datetime_ends[i], by=1.5*3600);
        datetime_labels[[i]] <- format(datetime_labels_at[[i]], graph_datetime_format);
    }

    writeLines(paste("dog names:", dog_names));

    # export boxplot with the distances of all the dogs
    startPDF(name=get_trans_filename("boxplot_all"));
    boxplot(results_dist_closest, names=dog_names, main=get_translation("dists_all_dogs_closest"), ylab=get_translation("dist_km"));
    endPDF();
    #CairoPDF(paste(base_folder,get_trans_filename("boxplot_all"), ".pdf",sep=""));
    #boxplot(results_dist_closest, names=data_location[1,], main=get_translation("dists_all_dogs_closest"), ylab=get_translation("dist_km"));
    #dev.off();
    writeLines("all1");
    
    # export boxplot with the distances of all the dogs WITHOUT outliers
    startPDF(name=get_trans_filename("boxplot_all_no_outlier"));
    boxplot(results_dist_closest, names=dog_names, main=get_translation("dists_all_dogs_closest"), ylab=get_translation("dist_km"), outline=FALSE);
    endPDF();
    #CairoPDF(paste(base_folder,get_trans_filename("boxplot_all_no_outlier"), ".pdf",sep=""));
    #boxplot(results_dist_closest, names=data_location[1,], main=get_translation("dists_all_dogs_closest"), ylab=get_translation("dist_km"), outline=FALSE);
    #dev.off();
    writeLines("all2");
    
    # export graph with distance of all dogs to their two sheep
    key <- c("dists_all_dogs_both");
    startPDF ( name=get_trans_filename(key), w=width, h=height, mfrow=mfrow );
#    CairoPDF(paste(base_folder,get_trans_filename("dists_all_dogs_both"), ".pdf",sep=""), width=width, height=height);
#    par(mfrow=c(nrows, ncols));
    for (i in 1:number_dogs)
    {
      data_m1 <- as.data.frame(results_dist_m1[i]);
      data_m2 <- as.data.frame(results_dist_m2[i]);
      at_ <- datetime_labels_at[[i]];
      ls_ <- datetime_labels[[i]];
      x <- datetime_xaxis[[i]];
      #x1 <- 1 : (dim(data_m1)[1]);
      #x2 <- 1 : (dim(data_m2)[1]);
      #xlab = paste(base_xlab_multidog,dog_names[i],sep="");
      #plot(1:(dim(data_m1)[1]), data_m1[,1], type="l", ylab=get_translation("dist_km"), xlab=xlab, ylim=c(0,1), xaxt="n", col="blue");
      #lines(1:(dim(data_m2)[1]), data_m2[,1], type='l', col='green');
      extraFn <- function()
      {
        lines(x, data_m2[,1], type='l', col='green');
      }
      justPlot ( x=x, y=data_m1[,1], main_direct=dog_names[i], main_transl_key="", xlab=xlab_multidog, col="blue", extraFn=extraFn, custom_datetime_labels=ls_, custom_datetime_labels_at=at_);
    }
    add_title_y(get_translation(key),"", 0, 0, title_fig);
    #dev.off();
    endPDF();
writeLines("all3");
    
    # export graph with distances of all dogs to the closest sheep
    key <- c("dists_all_dogs_closest");
    startPDF ( name=get_trans_filename(key), w=width, h=height, mfrow=mfrow );
    #CairoPDF(paste(base_folder,get_trans_filename("dists_all_dogs_closest"), ".pdf",sep=""), width=width, height=height);
    #par(mfrow=c(nrows, ncols));
    for (i in 1:number_dogs)
    {
      at_ <- datetime_labels_at[[i]];
      ls_ <- datetime_labels[[i]];
      x <- datetime_xaxis[[i]];
      data <- as.data.frame(results_dist_closest[i]);
      xlab = paste ( base_xlab_multidog, dog_names[i], sep="" );
      justPlot ( x=x, y=data[,1], main_direct=dog_names[i], main_transl_key="", xlab=xlab_multidog, ylim=c(0,1), custom_datetime_labels=ls_, custom_datetime_labels_at=at_ );
      #plot(1:(dim(data)[1]), data[,1], type="l", ylab=get_translation("dist_km"), xlab=xlab, ylim=c(0,1), xaxt="n");
    }
    add_title_y(get_translation("dists_all_dogs_closest"),"", 0, 0, title_fig);
    endPDF();
    #dev.off();
writeLines("all4");
    
    # export graph with distances of all dogs to the mean position of the sheep
    key <- c("dists_all_dogs_mean");
    startPDF ( name=get_trans_filename(key), w=width, h=height, mfrow=mfrow );
    #CairoPDF(paste(base_folder,get_trans_filename("dists_all_dogs_mean"),".pdf",sep=""), width=width, height=height);
    #par(mfrow=c(nrows, ncols));
    for (i in 1:number_dogs)
    {
      at_ <- datetime_labels_at[[i]];
      ls_ <- datetime_labels[[i]];
      x <- datetime_xaxis[[i]];
      data <- as.data.frame(results_dist_mean[i]);
      xlab = paste(base_xlab_multidog,dog_names[i],sep="");
      justPlot ( x=x, y=data[,1], main_direct=dog_names[i], main_transl_key="", xlab=xlab_multidog, ylim=c(0,1), custom_datetime_labels=ls_, custom_datetime_labels_at=at_ );
      #plot(1:(dim(data)[1]), data[,1], type="l", ylab=get_translation("dist_km"), xlab=xlab, ylim=c(0,1), xaxt="n");
    }
    add_title_y(get_translation(key),"", 0, 0, title_fig);
    endPDF();
    #dev.off();
writeLines("all5");
    
    
    ## now the same with histograms
    
    
    # export graph with distances of all dogs to the closest sheep
    key <- c("hist_dists_all_dogs_closest");
    startPDF ( name=get_trans_filename(key), w=width, h=height, mfrow=mfrow );
    #CairoPDF(paste(base_folder,get_trans_filename("hist_dists_all_dogs_closest"),".pdf",sep=""), width=width, height=height);
    #par(mfrow=c(nrows, ncols));
    for (i in 1:number_dogs)
    {
      data <- as.data.frame(results_dist_closest[i]);
      #xlab=dog_names[i];
      justHist ( data[,1], main_transl_key="", main_direct=dog_names[i], ylim=c(0,12000) );
      #med <-median(data[,1]);
      #hist(data[,1], breaks=hist_default_breaks, ylim=c(0,12000), xlim=hist_default_xlim, xlab=dog_names[i], ylab=get_translation("freq"), main="", sub=get_translation("red_median",c(signif(med,digits=4))));
      #abline(v=med, col="red", lwd=2); # add a red line for the median
    }
    add_title_y(get_translation(key),"", 0, 0, title_fig);
    #dev.off();
    endPDF();
writeLines("all6");
    
    # export graph with distances of all dogs to the mean position of the sheep
    key <- c("hist_dists_all_dogs_mean");
    startPDF (name=get_trans_filename(key), w=width, h=height, mfrow=mfrow);
    #CairoPDF(paste(base_folder,get_trans_filename("hist_dists_all_dogs_mean"),".pdf",sep=""), width=width, height=height);
    #par(mfrow=c(nrows, ncols));
    for (i in 1:number_dogs)
    {
      data <- as.data.frame(results_dist_mean[i]);
      xlab=dog_names[i];
      justHist (data[,1], main_transl_key="", main_direct=dog_names[i], ylim=c(0,12000));
      #med <-median(data[,1]);
      #hist(data[,1], breaks=HIST_DEFAULT_BREAKS, ylim=c(0,12000), xlim=hist_default_xlim, xlab=dog_names[i], ylab=get_translation("freq"), main="", sub=get_translation("red_median",c(signif(med,digits=4))));
      #abline(v=med, col="red", lwd=2); # add a red line for the median
    }
    add_title_y(get_translation(key),"", 0, 0, title_fig);
   # dev.off();
    endPDF();
writeLines("all7");
    
    
    
    
    # export histogram of coordination, splitted in positive and negative alignment
    max_pos_freq <- 0;
    max_neg_freq <- 0;
    bs <- seq(-1,1,1/HISTOGRAM_CLASSES);
    for (i in 1:number_dogs)
    {
      if (len_no_nan (results_coord_pos_align_m1[i]) != 0)
      {
        data_pos_m1 <- as.data.frame(results_coord_pos_align_m1[i]);
        data_neg_m1 <- as.data.frame(results_coord_neg_align_m1[i]);
        hist_pos_m1 <- hist(data_pos_m1[,1], plot=FALSE, breaks=bs);
        hist_neg_m1 <- hist(data_neg_m1[,1], plot=FALSE, breaks=bs);
        sum <- sum(hist_pos_m1$counts) + sum(hist_neg_m1$counts);
        m_p_m1 <- max(hist_pos_m1$counts / sum);
        if (m_p_m1 > max_pos_freq) max_pos_freq <- m_p_m1;
        m_n_m1 <- max(hist_neg_m1$counts / sum);
        if (m_n_m1 > max_neg_freq) max_neg_freq <- m_n_m1;
      }
      
      if (len_no_nan (results_coord_pos_align_m2[i]) != 0)
      {
        data_pos_m2 <- as.data.frame(results_coord_pos_align_m2[i]);
        data_neg_m2 <- as.data.frame(results_coord_neg_align_m2[i]);
        hist_pos_m2 <- hist(data_pos_m2[,1], plot=FALSE, breaks=bs);
        hist_neg_m2 <- hist(data_neg_m2[,1], plot=FALSE, breaks=bs);
        sum <- sum(hist_pos_m2$counts) + sum(hist_neg_m2$counts);
        m_p_m2 <- max(hist_pos_m2$counts / sum);
        if (m_p_m2 > max_pos_freq) max_pos_freq <- m_p_m2;
        m_n_m2 <- max(hist_neg_m2$counts / sum);
        if (m_n_m2 > max_neg_freq) max_neg_freq <- m_n_m2;
      }
    }
    max_freq <- max(max_pos_freq, max_neg_freq);
    key <- c("graph_hist_all_dogs_coord_both_sheep");
    startPDF (name=get_trans_filename(key), w=width, h=height, mfrow=mfrow);
    #CairoPDF(paste(base_folder,get_trans_filename("graph_hist_all_dogs_coord_both_sheep"),".pdf",sep=""), width=width, height=height);
    #par(mfrow=c(nrows, ncols));
    #red <- rgb(1,0,0,0.5);
    #yellow <- rgb(1,1,0,0.5);
    #blue <- rgb(0,0,1,0.5);
    #green <- rgb(0,1,0,0.5);
    text_x_left <- -0.5;
    text_x_right <- 0.5;
    text_y_top <- 1.03 * max_freq;
    text_y_bottom <- 1.03 * -max_freq;
    for (i in 1:number_dogs)
    {
      draw1 <- F;
      draw2 <- F;
      if (len_no_nan (results_coord_pos_align_m1[i]) != 0)
      {
        data_pos_m1 <- as.data.frame(results_coord_pos_align_m1[i]);
        data_neg_m1 <- as.data.frame(results_coord_neg_align_m1[i]);
        hist_pos_m1 <- hist(data_pos_m1[,1], plot=FALSE, breaks=bs);
        hist_neg_m1 <- hist(data_neg_m1[,1], plot=FALSE, breaks=bs);
        mean_pos_m1 <- mean(data_pos_m1[,1], na.rm=TRUE);
        mean_neg_m1 <- mean(data_neg_m1[,1], na.rm=TRUE);
        sum <- sum(hist_pos_m1$counts) + sum(hist_neg_m1$counts);
        hist_pos_m1$counts <- hist_pos_m1$counts / sum;
        hist_neg_m1$counts <- -hist_neg_m1$counts / sum;
        lims <- c(-max_freq,max_freq);
        plot(hist_pos_m1, ylim=lims, col="red", main="", xlab=dog_names[i], ylab=get_translation("rel_freq"));
        lines(hist_neg_m1, col="blue");
        draw1 <- T;
      }
      if (len_no_nan (results_coord_pos_align_m2[i]) != 0)
      {
        data_pos_m2 <- as.data.frame(results_coord_pos_align_m2[i]);
        data_neg_m2 <- as.data.frame(results_coord_neg_align_m2[i]);
        hist_pos_m2 <- hist(data_pos_m2[,1], plot=FALSE, breaks=bs);
        hist_neg_m2 <- hist(data_neg_m2[,1], plot=FALSE, breaks=bs);
        mean_pos_m2 <- mean(data_pos_m2[,1], na.rm=TRUE);
        mean_neg_m2 <- mean(data_neg_m2[,1], na.rm=TRUE);
        sum <- sum(hist_pos_m2$counts) + sum(hist_neg_m2$counts);
        hist_pos_m2$counts <- hist_pos_m2$counts / sum;
        hist_neg_m2$counts <- -hist_neg_m2$counts / sum;
        if (draw1)
          lines(hist_pos_m2, col="yellow")
        else
          plot(hist_pos_m2, col="yellow");
        lines(hist_neg_m2, col="green");
        draw2 <- T;
      }
      if (draw1 || draw2)
      {
        hist_posneg_labels_top <- c(get_translation("graph_align_pos_left_label"), get_translation("graph_align_pos_right_label"));
        hist_posneg_labels_bot <- c(get_translation("graph_align_neg_left_label"), get_translation("graph_align_neg_right_label"));
        text(x=c(text_x_left, text_x_right), y=c(text_y_top), labels=hist_posneg_labels_top);
        text(x=c(text_x_left, text_x_right), y=c(text_y_bottom), labels=hist_posneg_labels_bot);
      } else {
        plot(0,0,col="white");
        text(x=c(0), y=c(0), labels=get_translation("no_available_data", c(dog_names[i])));
      }
      abline(v=0);
    }
    add_title_y(get_translation("graph_hist_all_dogs_coord_both_sheep"), "", 0, 0, title_fig);
    subtitle_y <- title_y - 0.12 / 2^floor((number_dogs - 1) / 2);
    par(fig=c(0.49,0.51,subtitle_y, 1));
    text(x=c(0),y=c(0), labels=c(get_translation("hist_colors_label")), cex=1.0);
    dev.off();
writeLines("all9");
    
  }
  
  
  writeLines("magic finished =)");
}


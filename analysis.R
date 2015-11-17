
output_datetime_format <- "%d/%m/%Y %H:%M:%S";

graph_datetime_format <- "%d/%m/%y\n%H:%M";

makeNewFileName <- function (filename)
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
  return (new_filename);
 }

# this function copies the file and removes the first UTF BOM bytes
# if they are present..
removeUTFBOM <- function(filename)
{
  new_filename <- makeNewFileName(filename);
 
  # now copy over to the new file
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

handleDupTimestamps <- function (filename)
{
  new_filename <- makeNewFileName(filename);
  f_in  <- shQuote(filename);
  lines <- system2 ("perl", args=c("handle-timestamps.pl", "file"), stdin=f_in, stdout=TRUE);
  conWrite <- file (description=new_filename, open="w");
  writeLines (lines, con=conWrite);
  close(conWrite);
  return (new_filename);
}

read_file <- function (file)
{
  f2 <- removeUTFBOM(file);
  f3 <- handleDupTimestamps(f2);
  tmp <- as.data.frame ( read.csv (file=f3, sep=DATA_SEP, row.names=NULL) );
  file.remove(f2);
  file.remove(f3);
  return (tmp);
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
compute_dx <- function(vals)
{
  # first create the empty result array
  res <- array(0, dim=c(nrow(vals)-1,3))
  
  # for each value: the result[i-1] = x[i] - x[i-1]
  for (i in 2:nrow(vals))
    res[i-1,] <- vals[i,] - vals[i-1,];
  
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

# given a data frame, returns the same with unique timestamps values
remove_dup_timestamp <- function(d)
{
  vi <- 1; # last valid index
  is <- c(vi); # valid indices
  for ( i in 2:length(d$Timestamp) )
  {
    if ( d$Timestamp[i] != d$Timestamp[vi] )
    {
      is <- c(is, i);
      vi <- i;
    }
  }
  d [ is, ] ;
}

# check, remove, count, test existence of NaN values
f_no_nan <- function(x) !is.nan(x);
filter_no_nan <- function(x) return (Filter(f_no_nan, x));
len_no_nan <- function (x) return (length(filter_no_nan(x)));
f_has_no_nan <- function (x) return (len_no_nan(x)>0);

# says if a value is bigger than 0
f_pos <- function(x) x > 0;
# says if a value is smaller than 0
f_neg <- function(x) x < 0;


# Using the data for 1 test data set, computes all the metrics,
# such as displacement, distance between animals, etc.
analyse_one_animal <- function (animal_data, export_graphs)
{
  # take the data
  num_animals   <- animal_data@numAnimals[[1]];
  num_sheep     <- animal_data@numSheep[[1]];
  files_animals <- animal_data@animalFiles[[1]][1:num_animals];
  files_sheep   <- animal_data@sheepFiles[[1]][1:num_sheep];
  names_animals <- animal_data@animalNames[[1]][1:num_animals];
  names_sheep   <- animal_data@sheepNames[[1]][1:num_sheep];
  useFP         <- animal_data@useFixedPoint;
  

writeLines("Reading data files...");

  # prepare the data for the test animals and the sheep
  animals_data <- Map ( function(x) read_file(x),   files_animals );
  sheep_data  <- Map ( function(x) read_file(x), files_sheep );

  # just check the timestamps
  min_tstamps <- Map ( function (x) min(x[,"Timestamp"]), c(animals_data, sheep_data)); 
  max_tstamps <- Map ( function (x) max(x[,"Timestamp"]), c(animals_data, sheep_data)); 
  test_min_tstamp  <- unique(min_tstamps);
  test_max_tstamp  <- unique(max_tstamps);
  # if the timestamps do not start or stop at the same moment, or do not have the same length,
  # we correct them
  if ( length (test_min_tstamp) > 1 || length (test_max_tstamp) > 1 )
  {
    writeLines ("Unifying timestamps....");
    min <- max(unlist(test_min_tstamp));
    max <- min(unlist(test_max_tstamp));
    f_test <- function (data) data[data$Timestamp>=min & data$Timestamp <= max, ]
    animals_data <- Map ( f_test, animals_data );
    sheep_data <- Map ( f_test, sheep_data );
    num_samples <- Map ( function (x) length(x[,"Timestamp"]), c(animals_data, sheep_data) );
    if ( length ( unique (num_samples) ) > 1 ) stop ("Timestamps not unique!");
  }
  num_samples <- num_samples[[1]]


  # Prepare the time data as time sequence
  datetime_input_format <-
    if (is.null(animals_data[[1]]$Time_Date))
      "%Y-%m-%d %H:%M:%S"
    else
      "%H:%M:%S-%d.%m.%Y";
  datetime_vector <-
    if (is.null(animals_data[[1]]$Time_Date))
      animals_data[[1]]$Date_Time
    else
      animals_data[[1]]$Time_Date;
  time_seq <- as.POSIXct(datetime_vector, format=datetime_input_format);



writeLines("Calculating positions....");

  # now prepare the values
  # xx_pos is the 3D position in space, from (long,lat), as a point
  # xx_dx is the displacement from the previous position in 3D space, as a vector
  animals_pos  <- Map ( function(x) compute_pos(x), animals_data );
  animals_dx   <- Map ( function(x) compute_dx(x),  animals_pos );
  sheep_pos    <- Map ( function(x) compute_pos(x), sheep_data );
  sheep_dx     <- Map ( function(x) compute_dx(x),  sheep_pos );
  # sum up all the sheep positions and divide by number of sheep to find middle point
  sheep_middle_pos <- Reduce ( function(acc,s_pos) acc + s_pos, sheep_pos ) / num_sheep;

  if (useFP)
  {
    # find the distances from the animals to the given fixed point
    fp            <- animal_data@fixedPoint[[1]];
    fp_dataframe  <- data.frame(Latitude=fp[1], Longitude=fp[2]);
    fp_3D         <- compute_pos (fp_dataframe);
    animals_to_fp_distances <-
      # foreach animal, compute the distance at every moment to the fixed point
      Map ( function (pos_a)
                 {
                   len <- dim(pos_a)[1];
                   fps <- t( array (rep_len(fp_3D, 3*len), dim=c(3,len)) );
                   norm3D(pos_a - fps)
                 }
                 , animals_pos )
  }


writeLines("Calculating distances between animals, sheep, etc...");

  # find relative positions of sheep wrt animals (vector from sheep to animal)
  animals_to_sheep_vectors <-
    # foreach animal .. 
    Map ( function (pos_a)
            # .. compute the vector to each sheep, a simple substraction
            Map ( function (pos_s) pos_a - pos_s, sheep_pos )
        , animals_pos
    );



  # find the distance between animals and sheep
  animals_to_sheep_distances <-
    # foreach animal .. 
    Map ( function (pos_a)
            # .. compute the vector to each sheep, a simple substraction
            Map ( function (pos_s) norm3D(pos_a - pos_s), sheep_pos )
        , animals_pos
    );

  # find the distance to the middle point of the sheep
  animals_to_middle_sheep_distances <-
    Map ( # for each animal ..
      function (pos_a) norm3D(pos_a - sheep_middle_pos) # dist to middle = vector norm of pos(animal) - sheep_middle_pos
      , animals_pos
    );


  # find the average distance to the sheep
  # AND the distance to the closest: We need to do almost the same calculations...
  # So we compute both at the same time, and sort them later
  tmp <-
    Map ( # for each animal...
          function (a2s_d)
          { # .. create a matrix nxm, n=number of measures, and m=number of sheep ...
            ncols <- length(a2s_d);
            nrows <- length(a2s_d[[1]]); #[[1]] must exist
            arr <- array ( unlist (a2s_d), dim=c(nrows, ncols));
            dist_to_middle  <- apply ( arr, 1, mean); # ... then we take the mean in each row (=mean of dist to all sheep) ...
            dist_to_closest <- apply ( arr, 1, min);  # ... and also the minimum in each row.
            list(dist_to_middle, dist_to_closest);
          }
        ,
        animals_to_sheep_distances
    );
  # now we sort the values into two lists: tmp now contains pairs of vectors with min and mean
  animals_to_sheep_mean_distances  <-
    Map ( function(l) l[[1]], tmp) ; # take the first vector, mean
  animals_to_closest_sheep_distances <-
    Map ( function(l) l[[2]], tmp) ; # take the 2nd vector, min

  # Find the alignment between animals and sheep
  f_align <- function (dx_a, dx_s) # alignment between a and s is given by
    dotprod3D(dx_a,dx_s) / norm3D(dx_a) / norm3D(dx_s) # the same formula to find angle b/w 2 vectors
  animals_align_with_sheep <-
    # foreach animal .. 
    Map ( function (dx_a)
            # .. compute the alignment with each sheep
            Map ( function(dx_s) f_align(dx_a, dx_s) , sheep_dx )
        , animals_dx
    );


writeLines("Calculating coordination values...");

  # Compute how much the animals are in front of the sheep
  f_infront <- function (dx_a, vector_a_to_s) # in front between a and s is given by
    dotprod3D(dx_a,vector_a_to_s) / norm3D(dx_a) / norm3D(vector_a_to_s) # again that formula with: the animal direction and the relative position of the sheep wrt the animal
  animals_infrontof_sheep <- list();
  for (i_a in 1:num_animals)
  {
    animals_infrontof_sheep[[i_a]] <- list();
    a_dx <- animals_dx[[i_a]];
    for (i_s in 1:num_sheep)
    {
      x <- animals_to_sheep_vectors[[i_a]][[i_s]];
      #a_to_s_v <- animals_to_sheep_vectors[[i_a]][[i_s]][0:-1,]; # remove the first vector, since compute_dx does the same
      nrows = nrow(x);
      a_to_s_v <- x[2:nrows,]; # remove the first vector, since compute_dx does the same
      animals_infrontof_sheep[[i_a]][[i_s]] <- f_infront (a_dx, a_to_s_v );
    }
  }

  # filter the values out, if dog is too far.
  for (i_a in 1:num_animals)
  {
    for (i_s in 1:num_sheep)
    {
      x_tmp <- animals_to_sheep_distances[[i_a]][[i_s]];
      indexes_too_far <- x_tmp[2:length(x_tmp)] > FILTER_DIST;
      animals_infrontof_sheep[[i_a]][[i_s]][indexes_too_far] <- NaN;
      animals_align_with_sheep[[i_a]][[i_s]][indexes_too_far] <- NaN;
    }
  }


  # Now assemble in front and alignment to obtain an overall coordination value:
  # The two values are simply multiplied. The simple definitions are sometimes the best :)
  animals_sheep_coord_posalign <- list();
  animals_sheep_coord_negalign <- list();
  for (i_a in 1:num_animals)
  {
    animals_sheep_coord_posalign[[i_a]] <- list();
    animals_sheep_coord_negalign[[i_a]] <- list();
    for (i_s in 1:num_sheep)
    {
      a_align_s   <- animals_align_with_sheep[[i_a]][[i_s]];
      a_infront_s <- animals_infrontof_sheep[[i_a]][[i_s]];
      tmp_coord <- a_align_s * a_infront_s;

      # now keep pos and neg alignment separate (in coordination histograms, it is top and bottom)
      pos_indexes <- a_align_s >= 0;
      neg_indexes <- a_align_s < 0;
      animals_sheep_coord_posalign[[i_a]][[i_s]] <- ifelse (pos_indexes, tmp_coord, NaN);
      animals_sheep_coord_negalign[[i_a]][[i_s]] <- ifelse (neg_indexes, tmp_coord, NaN);
      
    }
  }

writeLines("Calculating groups...");

  # Classify the distance from the animal to the middle of the sheep when they are "in group"
  # say that a "group" is when animals are closer than 5m to the middle point of them
  f_choose_group <- function (d)
  {
    cls <- ifelse( d <= DIST_GROUP_MAX_INGROUP,      # inside group max?
                  1,                                 # yes: "In group"
                  ifelse (d <= DIST_GROUP_MAX_CLOSE, # no: inside close max ?
                          2,                         #     yes : "Close to group"
                          3));                       #     no: "Far"
    Map (function(x) DIST_GROUP_CLASSES[x], cls)# apply to all elements
  }
  # step 1. Find when the sheep are "in group" and when not
  s2mid_dists <- Map ( function(pos) norm3D(pos - sheep_middle_pos), sheep_pos);
  are_sheep_ingroup <- Reduce (
                        init = rep (TRUE, num_samples), # start with true at every moment
                        function (acc, s2mid_d)
                            ifelse (s2mid_d <= DIST_GROUP_MAX_INGROUP # is the distance sheep - middle <= d_max_group ?
                                    , acc                  # yes -> keep the older value
                                    , F)                   # no  -> make it false
                            , s2mid_dists); # do that with all sheep
  # step 2. At the moments where they are in group, classify the animals
  animals_group_relation <-
    Map ( function (a2mid_d) # at each timestamp
             ifelse (are_sheep_ingroup, # are sheep in group at this moment? 
                        f_choose_group(a2mid_d), # yes -> use the function to classify
                        DIST_GROUP_CLASSES[4])         # no  -> "No group"
             , animals_to_middle_sheep_distances # do that for each animal
      );

  return (
    list (
      "a2s_dist"        = animals_to_sheep_distances,
      "a2_mean_dist"    = animals_to_sheep_mean_distances,
      "a2_closest_s"    = animals_to_closest_sheep_distances,
      "a2fp_dist"       = if (useFP) animals_to_fp_distances else c(),
      "a2_middle_dist"  = animals_to_middle_sheep_distances,
      "coord_posalign"  = animals_sheep_coord_posalign,
      "coord_negalign"  = animals_sheep_coord_negalign,
      "a_pos"           = animals_pos,
      "a_dx"            = animals_dx,
      "s_pos"           = sheep_pos,
      "s_dx"            = sheep_dx,
      "groups"          = animals_group_relation,
      "align"           = animals_align_with_sheep,
      "infront"         = animals_infrontof_sheep,
      "time"            = time_seq
    )
  );
}

#################################
# write some results out to the console
write_results <- function(useFP, fp_str, animal_names, sheep_names, vals, output_folder)
{
  # open a file to write the results
  filename <- paste(output_folder, "text-values.txt", sep="");
  file <- file (filename, open="w"); 
  # foreach animal
  for ( i_a in 1:length(animal_names))
  {
    # data that is valid for all sheep, but we write it for all sheep
    dist_a    <- sum (norm3D (vals$"a_dx"[[i_a]]), na.rm=T);
    mean_dist_a_closest   <- mean(vals$"a2_closest_s"[[i_a]]);
    median_dist_a_closest <- median(vals$"a2_closest_s"[[i_a]]);
    mean_dist_a_middle   <- mean(vals$"a2_middle_dist"[[i_a]]);
    median_dist_a_middle <- median(vals$"a2_middle_dist"[[i_a]]);
    mean_dist_a_mean   <- mean(vals$"a2_mean_dist"[[i_a]]);
    median_dist_a_mean <- median(vals$"a2_mean_dist"[[i_a]]);
    if (useFP) mean_dist_a_fp <- mean(vals$"a2fp_dist"[[1]]);
    start_date_time <- vals$"time"[1];
    end_date_time <- tail(vals$"time", n=1);
    duration <- pretty_time(end_date_time - start_date_time);

    # for each sheep
    for ( i_s in 1:length(sheep_names))
    {
      dist_s    <- sum (norm3D (vals$"s_dx"[[i_s]]), na.rm=T);
      dist_rel  <- dist_a / dist_s;

      # Find the values related to alignment
      coord_posalign <- vals$"coord_posalign"[[i_a]][[i_s]];
      coord_negalign <- vals$"coord_negalign"[[i_a]][[i_s]];

      front     <- vals$"infront"[[i_a]][[i_s]];
      align     <- vals$"align"[[i_a]][[i_s]];

      mean_front <- mean(front, na.rm=T);
      mean_align <- mean(align, na.rm=T);

      mean_coord_posalign <- mean(coord_posalign, na.rm=TRUE);
      mean_coord_negalign <- mean(coord_negalign, na.rm=TRUE);
      
      # coord_posalign > 0 when dog is in front
      # coord_negalign < 0 when dog is in front
      mean_coord_palign_infront <- mean(Filter(f_pos, coord_posalign), na.rm=T);
      mean_coord_palign_inback  <- mean(Filter(f_neg, coord_posalign), na.rm=T);
      mean_coord_nalign_inback  <- mean(Filter(f_pos, coord_negalign), na.rm=T);
      mean_coord_nalign_infront <- mean(Filter(f_neg, coord_negalign), na.rm=T);
     
      
      # Now start writing both to standard output and to a file
      wrtr <- function ( key , args = c() )
      {
        str <- get_translation (key, args);
        writeLines (str);
        writeLines (str, con=file);
      }
      intro <- function (con)
      {
        writeLines(con=con, "==========================");
        writeLines(con=con, "==========================");
        writeLines(con=con, "  Results for:");
        writeLines(con=con, paste("    Animal: ", animal_names[[i_a]]));
        writeLines(con=con, paste("    Sheep: ", sheep_names[[i_s]]));
        writeLines(con=con, "==========================");
        writeLines(con=con, "==========================");
      }
      intro(stdout());
      intro(file);
      wrtr ("dist_dog",                        c(dist_a));
      wrtr ("dist_sheep",                      c(dist_s));
      wrtr ("dist_sheep_rel",                  c(dist_rel));
      wrtr ("mean_dist_closest",               c(mean_dist_a_closest));
      wrtr ("median_dist_closest",             c(median_dist_a_closest));
      wrtr ("mean_dist_mean",                  c(mean_dist_a_middle));
      wrtr ("median_dist_mean",                c(median_dist_a_middle));
      wrtr ("mean_dist_middle",                c(mean_dist_a_mean));
      wrtr ("median_dist_middle",              c(median_dist_a_mean));
      wrtr ("dog_in_front1000",                c(mean_front * 1000));
      wrtr ("dog_aligned100",                  c(mean_align * 100));
      wrtr ("coord_align_pos",                 c(mean_coord_posalign));
      wrtr ("coord_align_neg",                 c(mean_coord_negalign));
    if (useFP) wrtr ("fixed_pt",               c(fp_str));
    if (useFP) wrtr ("mean_dist_fixed_pt",     c(mean_dist_a_fp));
      wrtr ("mean_coord_palign_infront",       c(mean_coord_palign_infront));
      wrtr ("mean_coord_palign_inback",	       c(mean_coord_palign_inback ));
      wrtr ("mean_coord_nalign_inback",	       c(mean_coord_nalign_inback ));
      wrtr ("mean_coord_nalign_infront",	   c(mean_coord_nalign_infront));
      wrtr ("test_date_start",                 c(start_date_time));
      wrtr ("test_date_end",                   c(end_date_time));
      wrtr ("test_duration",                   c(duration));
    }
  }
  close (file);
}


start_analysis <- function (
    animals_data,
    output_folder,
    export_1animal_graphs,
    export_allanimals_graphs
)
{
  source_file("plot.R"); # load it here, to update histogram classes and others

  stored_values <- list();

  for ( i in 1:animals_data@numEntries )
  {
    writeLines(paste("\n\n\nStarting analysis with data set", i));
    data <-  extractAnimalData(animals_data, i);
    vals <- analyse_one_animal (data);
    names_a <- data@animalNames[[1]];
    names_s <- data@sheepNames[[1]];
    useFP   <- data@useFixedPoint;
    fp_str  <- if (useFP) paste(data@fixedPoint[[1]][1],data@fixedPoint[[1]][2],sep=",") else "";
    write_results(useFP, fp_str, names_a, names_s, vals, data@outputFolder);

    if (export_1animal_graphs)
      draw_graphs_1animal (data@outputFolder, useFP, fp_str,
                              names_a, names_s, vals);

    stored_values[[i]] <- vals;
  }

  writeLines("\n\n\nMagic finished =)");

}

# Function to draw "one value" for each animal,
# for example everal graphs with only one line each;
# or the histogram of a given value for each animal.
# If the argument `x_time_data' is given, graphs are created with y_data.
# If the argument `x_hists_data' is given, histograms are created with it.
# That means we can do both
draw_graph_1val <- function (num_animals,
                         x_time_data=NULL, # specify this to plot
                         x_hists_data=NULL, # give a list here to make histograms
                         y_data, # list, 1 entry for each animal
                         main_directs, # vector, 1 entry for each animal
                         x_axis_labels, x_axis_at,
                         key_title_plot, args_title_plot=c(),
                         key_filename_plot=key_title_plot,
                         args_filename_plot=args_title_plot,
                         key_title_hist, args_title_hist=c(),
                         key_filename_hist=key_title_hist,
                         args_filename_hist=args_title_hist,
                         ...
                         )
{
  mfrow <- c( num_animals , 1 );# one column, n rows
  if ( ! is.null(x_time_data) )
  {
    filename <- get_translation(key_filename_plot, args_filename_plot);
    startPDF(name=filename, mfrow=mfrow);
    title <- get_translation(key_title_plot, args_title_plot);
    preparePDFTitle(title=title);
    for (i in 1:num_animals)
    {
      justPlot ( x=x_time_data, y=y_data[[i]],
                      main_direct=main_directs[i],
                      custom_datetime_labels=x_axis_labels,
                      custom_datetime_labels_at=x_axis_at,
                      type="l",
                      ... );
    }
    endPDF();
  }
  if ( ! is.null (x_hists_data) )
  {
    filename <- get_translation(key_filename_hist, args_filename_hist);
    startPDF(name=filename, mfrow=mfrow);
    title <- get_translation(key_title_hist, args_title_hist);
    preparePDFTitle(title=title);
    for (i in 1:num_animals)
    {
      justHist ( x=x_hists_data[[i]],
                      main_direct=main_directs[i],
                      ... );
    }
    endPDF();
  }
}

# function to draw multiple values for each animal
draw_graph_nvals <- function(
                         num_animals,
                         x_data,
                         y_data, # list of lists, 1 entry for each animal
                         main_directs, # vector, 1 entry for each animal
                         x_axis_labels, x_axis_at,
                         key_title, args_title=c(),
                         key_filename=key_title,
                         args_filename=args_title,
                         colors,
                         ...
                       )
{
  mfrow <- c( num_animals , 1 );# one column, n rows
  filename <- get_translation(key_filename, args_filename);
  startPDF(name=filename, mfrow=mfrow);
  title <- get_translation(key_title, args_title);
  preparePDFTitle(title=title);
  for (i in 1:num_animals)
  {
    justPlot ( x=x_data, y=y_data[[i]][[1]],
                    main_direct=main_directs[i],
                    custom_datetime_labels=x_axis_labels,
                    custom_datetime_labels_at=x_axis_at,
                    type="l",
                    col=colors[1],
                    ... );
    num_vals <- length (y_data[[i]]);
    if ( num_vals > 1 )
    {
      for (i2 in 2:num_vals)
      {
        lines( x=x_data, y=y_data[[i]][[i2]],
                type="l", col=colors[i2],
                ...);
      }
    }
  }
  endPDF();
}


draw_graphs_1animal <- function(folder, useFP, fp_str, animal_names, sheep_names, values)
{
  hours_bw_labels <- 3; 
  x_time_data <- values$"time";
  dates_POSIXlt <- as.POSIXlt (values$time);
  t0 <- dates_POSIXlt[1];
  tn <- tail(dates_POSIXlt, n=1);
  dt <- difftime (tn, t0);
  dt_hours <- ( (tn$year - t0$year) * 365 + (tn$yday - t0$yday) ) * 24 + (tn$hour - t0$hour);
  num_ticks <- as.double(dt_hours) / hours_bw_labels;
  DEFAULT_PDF_WIDTH <<- 0.8 *  num_ticks;

  axis_dates <- seq.POSIXt(t0, tn, length.out=num_ticks);
  axis_labels <- format(axis_dates, graph_datetime_format);

  num_animals <- length(animal_names);
  num_sheep   <- length(sheep_names);

  # Prepare some data we want to show separately for day and night
  date_day_indexes <- dates_POSIXlt$hour >= DAY_HOUR_START & dates_POSIXlt$hour <= DAY_HOUR_END;
  date_night_indexes <- ! date_day_indexes;
  f_dayOr0 <- function (x) ifelse(date_day_indexes, x, 0);
  f_day <- function (x) x[date_day_indexes];
  f_nightOr0 <- function (x) ifelse(date_night_indexes, x, 0);
  f_night <- function (x) x[date_night_indexes];
  dayor0_a2middle <- Map (f_dayOr0, values$a2_mean_dist);
  nightor0_a2middle <- Map ( f_nightOr0, values$a2_mean_dist);
  day_a2middle <- Map (f_day, values$a2_mean_dist);
  night_a2middle <- Map (f_night, values$a2_mean_dist);
  dayor0_a2closest <- Map (f_dayOr0, values$a2_closest_s);
  nightor0_a2closest <- Map (f_nightOr0, values$a2_closest_s);
  day_a2closest <- Map (f_day, values$a2_closest_s);
  night_a2closest <- Map (f_night, values$a2_closest_s);

  # prepare a text with all the animal name together for the
  # file names and titles.
  # Then, prepare texts for above the graphs, telling what is 
  # happening (Animal name for mean, animal vs sheep name for
  # individual graphs)
  # When there is only one animal, no special main: It is in the
  # title already
  allnames               <- animal_names[1]
  main_direct_1animal    <- c("");
  if (num_animals > 1)
  {
    main_direct_1animal <- animal_names;
    for ( i in 2:num_animals )
      allnames <- paste(allnames, animal_names[i], sep="+");
  }
  args_names <- c(allnames);
  tmp_a_vs_s <- expand.grid(animal_names, sheep_names);
  f_a_vs_s_txt <- function(x) paste(x[1], x[2], sep=" <-> ");
  main_direct_as_vs_sheep <-
    array (
        apply ( tmp_a_vs_s, 1, FUN = f_a_vs_s_txt)
        , dim=c(num_animals, num_sheep));


  setPDFFolder ( folder );

  if (useFP)
  {
    # distances from the dog to the given fixed point
    draw_graph_1val(num_animals,
                           x_time_data=x_time_data,
                           y_data=values$"a2fp_dist",
                           x_hists_data=values$"a2fp_dist",
                           main_directs=main_direct_1animal,
                           x_axis_labels=axis_labels,
                           x_axis_at=axis_dates,
                           key_title_plot="graph_dist_fp",
                           args_title_plot=c(allnames, fp_str),
                           key_title_hist="hist_dist_fp",
                           args_title_hist=c(allnames, fp_str)
                           );
  }
  # distances to the closest sheep
  draw_graph_1val(num_animals,
                         x_time_data=x_time_data,
                         y_data=values$"a2_closest_s",
                         x_hists_data=values$"a2_closest_s",
                         main_directs=main_direct_1animal,
                         x_axis_labels=axis_labels,
                         x_axis_at=axis_dates,
                         key_title_plot="graph_closest",
                         args_title_plot=c(allnames),
                         key_title_hist="hist_closest",
                         args_title_hist=c(allnames),
                         );
  # distances to the closest sheep (day)
  draw_graph_1val(num_animals,
                         x_time_data=x_time_data,
                         y_data=dayor0_a2closest,
                         x_hists_data=day_a2closest,
                         main_directs=main_direct_1animal,
                         x_axis_labels=axis_labels,
                         x_axis_at=axis_dates,
                         key_title_plot="graph_closest_day",
                         args_title_plot=c(allnames),
                         key_title_hist="hist_closest_day",
                         args_title_hist=c(allnames),
                         );
  # distances to the closest sheep (night)
  draw_graph_1val(num_animals,
                         x_time_data=x_time_data,
                         y_data=nightor0_a2closest,
                         x_hists_data=night_a2closest,
                         main_directs=main_direct_1animal,
                         x_axis_labels=axis_labels,
                         x_axis_at=axis_dates,
                         key_title_plot="graph_closest_night",
                         args_title_plot=c(allnames),
                         key_title_hist="hist_closest_night",
                         args_title_hist=c(allnames),
                         );
  # normalised distance to the closest sheep
  draw_graph_1val(num_animals,
                         x_time_data=x_time_data,
                         y_data=values$"a2_closest_s",
                         main_directs=main_direct_1animal,
                         x_axis_labels=axis_labels,
                         x_axis_at=axis_dates,
                         key_title_plot="graph_closest_norm",
                         args_title_plot=c(allnames),
                         ylim=c(0,1)
                         );
  # distance to the middle of the sheep
  draw_graph_1val(num_animals,
                         x_time_data=x_time_data,
                         y_data=values$"a2_middle_dist",
                         x_hists_data=values$"a2_middle_dist",
                         main_directs=main_direct_1animal,
                         x_axis_labels=axis_labels,
                         x_axis_at=axis_dates,
                         key_title_plot="graph_dist_middle",
                         args_title_plot=c(allnames),
                         key_title_hist="graph_hist_dist_middle",
                         args_title_hist=c(allnames)
                         );
  # distance to the middle of the sheep (day)
  draw_graph_1val(num_animals,
                         x_time_data=x_time_data,
                         y_data=dayor0_a2middle,
                         x_hists_data=day_a2middle,
                         main_directs=main_direct_1animal,
                         x_axis_labels=axis_labels,
                         x_axis_at=axis_dates,
                         key_title_plot="graph_dist_middle_day",
                         args_title_plot=c(allnames),
                         key_title_hist="graph_hist_dist_middle_day",
                         args_title_hist=c(allnames)
                         );
  # distance to the middle of the sheep (night)
  draw_graph_1val(num_animals,
                         x_time_data=x_time_data,
                         y_data=nightor0_a2middle,
                         x_hists_data=night_a2middle,
                         main_directs=main_direct_1animal,
                         x_axis_labels=axis_labels,
                         x_axis_at=axis_dates,
                         key_title_plot="graph_dist_middle_night",
                         args_title_plot=c(allnames),
                         key_title_hist="graph_hist_dist_middle_night",
                         args_title_hist=c(allnames)
                         );
  # normalised distance to the middle of the sheep
  draw_graph_1val(num_animals,
                         x_time_data=x_time_data,
                         y_data=values$"a2_middle_dist",
                         main_directs=main_direct_1animal,
                         x_axis_labels=axis_labels,
                         x_axis_at=axis_dates,
                         key_title_plot="graph_dist_middle_norm",
                         args_title_plot=c(allnames),
                         ylim=c(0,1)
                         );
  # Distances dog to all sheep
  draw_graph_nvals(num_animals,
                         x_time_data,
                         values$"a2s_dist",
                         main_direct_1animal,
                         axis_labels, axis_dates,
                         key_title="graph_dist_both_sheep",
                         args_title=c(allnames),
                         colors=c("blue","green","yellow","red")
                         );
  # Histogram of groups
  breaks <- c(0:4+0.2, 0:4+0.8); # where to separate data; trick to have thin columns
  vals <- Map (
    function(x)
    {
      suppressWarnings({
        h <- hist (as.integer(x), plot=F, breaks=breaks, freq=T);
        h$density <- h$counts <- h$counts / sum(h$counts);
        return (h);
      });
    }
    , values$"groups" );
  # display name of groups at bottom
  labels <- c("",as.character(DIST_GROUP_CLASSES),"");
  labels[2] <- paste(labels[2], "( <=", DIST_GROUP_MAX_INGROUP,")");
  labels[3] <- paste(labels[3], "( <=", DIST_GROUP_MAX_CLOSE,")");
  at <- 0:5;
  f_draw_axis <- function() axis(1, tick=F, at=at, labels=labels, padj=-1.5);
  draw_graph_1val(num_animals,
                         x_hists_data=vals,
                         main_directs=main_direct_1animal,
                         key_title_hist="hist_dist_groups",
                         args_title_hist=c(allnames),
                         showMedian=FALSE,
                         ylab=get_translation("rel_freq"),
                         xlab="", xaxt="n", extraFn=f_draw_axis
                         )

  # histogram of coordination: we combine orientation and in front of to obtain an overall "coordination index"
  # we will draw two histograms for each animal-sheep pair:
  # - one when the alignment is positive, on the top half of the graph
  # - the other for negative alignment which will be on the bottom half

  make_coordination_pdfs(values,
                                    num_animals, num_sheep,
                                    main_direct_as_vs_sheep,
                                    allnames);

  # combine coordination values.
  # For example, if we have 2 animals and 2 sheep,
  # we will have 2 elmts (for animals) of 4 lists:
  #  - 2 first are the positive alignment values
  #  - 2 last are the negative alignment values
  cpos <- values$"coord_posalign";
  cneg <- values$"coord_negalign";

  has_coord_pos <- Map ( function(coords_a) lapply(coords_a, f_has_no_nan), cpos);
  has_coord_neg <- Map ( function(coords_a) lapply(coords_a, f_has_no_nan), cneg);


  # create counts for the data sets
  bs <- seq(-1,1,length.out=HISTOGRAM_CLASSES);
  hists_no_plot_pos <- Map (
      function(coords_a) # for each animal .. 
          lapply(coords_a, hist, plot=F, breaks=bs)
      , cpos);
  hists_no_plot_neg <- Map (
      function(coords_a) # for each animal .. 
          lapply(coords_a, hist, plot=F, breaks=bs)
      , cneg);
  
  max_freq <- 0;
  for ( i_a in 1:length(cpos) ) # for each animal
  {
    for ( i_s in 1:length(cpos[[i_a]]) )
    {
      sum <- sum(hists_no_plot_pos[[i_a]][[i_s]]$counts,
                 hists_no_plot_neg[[i_a]][[i_s]]$counts);
      hists_no_plot_pos[[i_a]][[i_s]]$counts <-
          hists_no_plot_pos[[i_a]][[i_s]]$counts / sum;
      hists_no_plot_neg[[i_a]][[i_s]]$counts <-
          - hists_no_plot_neg[[i_a]][[i_s]]$counts / sum;
      max_freq <- max( max_freq,
                       hists_no_plot_pos[[i_a]][[i_s]]$counts,
                       abs(hists_no_plot_neg[[i_a]][[i_s]]$counts));
    }
  }
  # prepare additional text pos
  text_y_top <- 1.03 * max_freq;
  text_y_bottom <- 1.03 * -max_freq;
  text_x_left <- -0.5;
  text_x_right <- 0.5;
  # prepare other parameters
  xlab <- "";
  ylab <- get_translation ("rel_freq");
  key <- "graph_hist_coord_both_sheep";
  args <- c(allnames);
  ylim <- c(-max_freq, max_freq);
  filename <- get_translation (key, args);
  title <- get_translation (key, args);
  cols <- list("red","blue","yellow","green");

  justCoordHist <- function(dataPos, dataNeg, has_dataPos, has_dataNeg, main,
                            ylim, xlab, ylab, colPos, colNeg, 
                            suppl_text_x, suppl_text_y)
  {
    plot_f <- function(data,col)
    {
      plot (data, main=main, ylim=ylim, xlab=xlab, ylab=ylab, col=col);
    }
    suppl_text <- list (
                    get_translation("graph_align_pos_left_label"),
                    get_translation("graph_align_neg_left_label"),
                    get_translation("graph_align_pos_right_label"),
                    get_translation("graph_align_neg_right_label")
                  );
    # if there is positive coordination data, plot it
    if (has_dataPos)
      plot_f (data=dataPos, col=colPos);

    # if there is negative coordination data, plot it
    if (has_dataNeg)
    {
      if (has_dataPos)
        lines (dataNeg, col=colNeg)
      else
        plot_f (data=dataNeg, col=colNeg);
    }

    if (has_dataPos | has_dataNeg)
    {
      ls_top <- c(suppl_text[[1]], suppl_text[[3]]);
      ls_bot <- c(suppl_text[[2]], suppl_text[[4]]);
      text(x=c(suppl_text_x[1], suppl_text_x[2]), y=c(suppl_text_y[1]), labels=ls_top);
      text(x=c(suppl_text_x[1], suppl_text_x[2]), y=c(suppl_text_y[2]), labels=ls_bot);
      abline(v=0);
    }
    else
    {
      plot(c(),c(),xlim=c(-1,1),ylim=c(-1,1),axes=FALSE,xlab="",ylab="")
      text(0, 0, get_translation("no_available_data"));
    }
  }


  # ok let's draw
  n_a <- num_animals; n_s <- num_sheep;
  mfrow <- c(n_a, n_s);
  startPDF ( name=filename, mfrow=mfrow, w=1.4*DEFAULT_PDF_WIDTH, h=1.4*DEFAULT_PDF_HEIGHT ); # make 2 graphs, 1 for each sheep
  preparePDFTitle (title);
  for (i_a in 1:n_a)
  {
    for (i_s in 1:n_s)
    {
      main <- main_direct_as_vs_sheep[i_a,i_s];
      dpos <- hists_no_plot_pos[[i_a]][[i_s]];
      dneg <- hists_no_plot_neg[[i_a]][[i_s]];
      has_data_pos <- has_coord_pos[[i_a]][[i_s]];
      has_data_neg <- has_coord_neg[[i_a]][[i_s]];
      justCoordHist (dataPos=dpos, dataNeg=dneg,
                    has_dataPos=has_data_pos,
                    has_dataNeg=has_data_neg,
                    main=main, ylim=ylim, xlab=xlab,
                    ylab=ylab, colPos=cols[[1]],
                    colNeg=cols[[2]],
                    suppl_text_x=c(text_x_left,text_x_right),
                    suppl_text_y=c(text_y_top,text_y_bottom));
    }
  }
  endPDF();

  writeLines("");
  writeLines("");
}


make_coordination_pdfs <- function (values,
                                    num_animals, num_sheep,
                                    main_direct_as_vs_sheep,
                                    allnames)
{
  # histogram of sheep-animals alignment

  # prepare params
  datas <- list(values$align, values$infront);
  xlab <- "";
  ylab <- get_translation ("rel_freq");
  keys <- c("hist_align", "hist_infront");
  args <- c(allnames);
  n_a <- num_animals; n_s <- num_sheep;
  mfrow <- c(n_a, n_s);
  # do plot
  for (i in 1:2)
  {
    key <- keys[i];
    filename <- get_translation (key, args);
    title <- get_translation (key, args);
    startPDF ( name=filename, mfrow=mfrow, w=1.4*DEFAULT_HIST_PDF_WIDTH, h=1.4*DEFAULT_HIST_PDF_HEIGHT ); # make 2 graphs, 1 for each sheep
    preparePDFTitle (title);
    for (i_a in 1:n_a)
    {
      for (i_s in 1:n_s)
      {
        data <- filter_no_nan (datas[[i]][[i_a]][[i_s]]);
        num_vals <- get_translation("num_vals", length(data));
        main <- paste (main_direct_as_vs_sheep[i_a,i_s], num_vals, sep=" -- ");
        justHist(data,
                 main_transl_key="",
                 main_direct=main,
                 showMedian=FALSE);
      }
    }
    endPDF();
  }
}



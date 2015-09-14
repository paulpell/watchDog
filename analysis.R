
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

read_file <- function (file)
{
  f2 <- removeUTFBOM(file);
  tmp <- as.data.frame ( read.csv (file=f2, sep=DATA_SEP, row.names=NULL) );
  file.remove(f2);
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

# remove NaN values for the sums and means
f_no_nan <- function(x) !is.nan(x);
filter_no_nan <- function(x) return (Filter(f_no_nan, x));
len_no_nan <- function (x) return (length(filter_no_nan(x)));
f_has_no_nan <- function (x) return (len_no_nan(x)>0);

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

analyse_one_animal <- function (animal_data, export_graphs)
{
  # take the data
  num_animals   <- animal_data@numAnimals[[1]];
  num_sheep     <- animal_data@numSheep[[1]];
  files_animals <- animal_data@animalFiles[[1]][1:num_animals];
  files_sheep   <- animal_data@sheepFiles[[1]][1:num_sheep];
  names_animals <- animal_data@animalNames[[1]][1:num_animals];
  names_sheep   <- animal_data@sheepNames[[1]][1:num_sheep];
  

  # prepare the data for the test animals and the sheep
  animals_data <- Map ( function(x) read_file(x),   files_animals );
  sheep_data  <- Map ( function(x) read_file(x), files_sheep );

  # just check the timestamps
  min_tstamps <- Map ( function (x) min(x[,"Timestamp"]), c(animals_data, sheep_data)); 
  max_tstamps <- Map ( function (x) max(x[,"Timestamp"]), c(animals_data, sheep_data)); 
  test_min_tstamp  <- unique(min_tstamps);
  test_max_tstamp  <- unique(max_tstamps);
  if ( length (test_min_tstamp) > 1 )
    warning("All animals do not have the same start timestamp!");
  if ( length (test_max_tstamp) > 1 )
    warning("All animals do not have the same end timestamp!");

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



  # now prepare the values
  # xx_pos is the 3D position in space, from (long,lat), as a point
  # xx_dx is the displacement from the previous position in 3D space, as a vector
  animals_pos  <- Map ( function(x) compute_pos(x), animals_data );
  animals_dx   <- Map ( function(x) compute_dx(x),  animals_pos );
  sheep_pos    <- Map ( function(x) compute_pos(x), sheep_data );
  sheep_dx     <- Map ( function(x) compute_dx(x),  sheep_pos );

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

  # find the distance to the middle point of the sheep, that is the mean of all the distances
  # AND the distance to the closest: We need to do almost the same calculations...
  # So we compute both at the same time, and sort them later
  tmp <-
    Map ( # for each animal...
          function (a2s_d)
          { # .. create a matrix nxm, n=number of measures, and m=number of sheep ...
            ncols <- length(a2s_d);
            nrows <- length(a2s_d[[1]]); #[[1]] must exist
            arr <- array ( unlist (a2s_d), dim=c(nrows, ncols));
            dist_to_middle  <- apply ( arr, 1, mean); # ... then we take the mean in each row (=mean of all sheep at given timestamp) ...
            dist_to_closest <- apply ( arr, 1, min);  # ... and also the minimum in each row.
            list(dist_to_middle, dist_to_closest);
          }
        ,
        animals_to_sheep_distances
    );
  # now we sort the values into two lists: tmp now contains pairs of vectors with min and mean
  animals_to_middle_sheep_distance  <-
    Map ( function(l) l[[1]], tmp) ; # take the first vector, mean
  animals_to_closest_sheep_distance <-
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
    
  return (
    list (
      "a2s_dist"        = animals_to_sheep_distances,
      "a2_middle_s"     = animals_to_middle_sheep_distance,
      "a2_closest_s"    = animals_to_closest_sheep_distance,
      "a2fp_dist"       = animals_to_fp_distances,
      "coord_posalign"  = animals_sheep_coord_posalign,
      "coord_negalign"  = animals_sheep_coord_negalign,
      "a_pos"           = animals_pos,
      "a_dx"            = animals_dx,
      "s_pos"           = sheep_pos,
      "s_dx"            = sheep_dx,
      "a2s_vectors"     = animals_to_sheep_vectors,
      "align"           = animals_align_with_sheep,
      "infront"         = animals_infrontof_sheep,
      "time"            = time_seq
    )
  );
}

#################################
# write some results out to the console
write_results <- function(animal_names, sheep_names, vals)
{
  # foreach animal
  for ( i_a in 1:length(animal_names))
  {
    # data that is valid for all sheep, but we write it for all sheep
    dist_a    <- sum (norm3D (vals$"a_dx"[[i_a]]), na.rm=T);
    mean_dist_a_closest   <- mean(vals$"a2_closest_s"[[i_a]]);
    median_dist_a_closest <- median(vals$"a2_closest_s"[[i_a]]);
    mean_dist_a_middle   <- mean(vals$"a2_middle_s"[[i_a]]);
    median_dist_a_middle <- median(vals$"a2_middle_s"[[i_a]]);
    mean_dist_a_fp <- mean(vals$"a2fp_dist"[[1]]);
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
     
      
      # Now start writing
      #fp_str <- paste(fixed_point_N,fixed_point_E);
      wrtr <- function ( key , args = c() )
        writeLines ( get_translation (key, args) );

      writeLines("==========================");
      writeLines("==========================");
      writeLines("  Results for:");
      writeLines(paste("    Animal: ", animal_names[[i_a]]));
      writeLines(paste("    Sheep: ", sheep_names[[i_s]]));
      writeLines("==========================");
      writeLines("==========================");
      wrtr ("dist_dog",                        c(dist_a));
      wrtr ("dist_sheep",                      c(dist_s));
      wrtr ("dist_sheep_rel",                  c(dist_rel));
      wrtr ("mean_dist_closest",               c(mean_dist_a_closest));
      wrtr ("median_dist_closest",             c(median_dist_a_closest));
      wrtr ("mean_dist_middle",                c(mean_dist_a_middle));
      wrtr ("median_dist_middle",              c(median_dist_a_middle));
      wrtr ("dog_in_front1000",                c(mean_front * 1000));
      wrtr ("dog_aligned100",                  c(mean_align * 100));
      wrtr ("coord_align_pos",                 c(mean_coord_posalign));
      wrtr ("coord_align_neg",                 c(mean_coord_negalign));
      #wrtr ("num_barkings",                    c(sum_barking));
      #wrtr ("fixed_pt",                        c(fp_str));
      wrtr ("mean_dist_fixed_pt",              c(mean_dist_a_fp));
      wrtr ("mean_coord_palign_infront",       c(mean_coord_palign_infront));
      wrtr ("mean_coord_palign_inback",	       c(mean_coord_palign_inback ));
      wrtr ("mean_coord_nalign_inback",	       c(mean_coord_nalign_inback ));
      wrtr ("mean_coord_nalign_infront",	   c(mean_coord_nalign_infront));
      wrtr ("test_date_start",                 c(start_date_time));
      wrtr ("test_date_end",                   c(end_date_time));
      wrtr ("test_duration",                   c(duration));
    }
  }
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
    writeLines(paste("\n\n\nStarting analysis of animal ", i));
    data <-  extractAnimalData(animals_data, i);
    vals <- analyse_one_animal (data);
    names_a <- data@animalNames[[1]];
    names_s <- data@sheepNames[[1]];
    write_results(names_a, names_s, vals);

    if (export_1animal_graphs)
      draw_graphs_1animal (data@outputFolder[i]
                            , names_a, names_s, vals);

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
    filename <- get_trans_filename(key_filename_plot, args_filename_plot);
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
    filename <- get_trans_filename(key_filename_hist, args_filename_hist);
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
  filename <- get_trans_filename(key_filename, args_filename);
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


draw_graphs_1animal <- function(folder, animal_names, sheep_names, values)
{

  x_time_data <- values$"time";
  start_date_time <- values$"time"[1];
  end_date_time <- tail(values$"time", n=1);

  axis_dates <- seq.POSIXt(start_date_time, end_date_time, length.out=9);
  axis_labels <- format(axis_dates, graph_datetime_format);

  num_animals <- length(animal_names);
  num_sheep   <- length(sheep_names);

  # prepare a text with all the animal name together for the
  # file names and titles.
  # Then, prepare texts for above the graphs, telling what is 
  # happening (Animal name for mean, animal vs sheep name for
  # individual graphs)
  # When there is only one animal, no special main: It is in the
  # title already
  allnames               <- animal_names[1]
  main_direct_1animal    <- c();
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

  # distances from the dog to the given fixed point
  vals <- values$"a2fp_dist";
  draw_graph_1val(num_animals,
                         x_time_data=x_time_data,
                         y_data=vals,
                         x_hists_data=vals,
                         main_directs=main_direct_1animal,
                         x_axis_labels=axis_labels,
                         x_axis_at=axis_dates,
                         key_title_plot="graph_dist_fp",
                         args_title_plot=c(allnames),
                         key_title_hist="hist_dist_fp",
                         args_title_hist=c(allnames)
                         );
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
                         y_data=values$"a2_middle_s",
                         x_hists_data=values$"a2_middle_s",
                         main_directs=main_direct_1animal,
                         x_axis_labels=axis_labels,
                         x_axis_at=axis_dates,
                         key_title_plot="graph_dist_mean",
                         args_title_plot=c(allnames),
                         key_title_hist="graph_hist_dist_mean",
                         args_title_hist=c(allnames)
                         );
  # normalised distance to the middle of the sheep
  draw_graph_1val(num_animals,
                         x_time_data=x_time_data,
                         y_data=values$"a2_middle_s",
                         main_directs=main_direct_1animal,
                         x_axis_labels=axis_labels,
                         x_axis_at=axis_dates,
                         key_title_plot="graph_dist_mean_norm",
                         args_title_plot=c(allnames),
                         ylim=c(0,1)
                         );
     browser()

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



  # histogram of coordination: we combine orientation and in front of to obtain an overall "coordination index"
  # we will draw two histograms for each animal-sheep pair:
  # - one when the alignment is positive, on the top half of the graph
  # - the other for negative alignment which will be on the bottom half

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
  filename <- get_trans_filename (key, args);
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



#################################################
#################################################

# Below are the old functions


#################################################
#################################################
#################################################
#################################################
#################################################
#################################################
#################################################
#################################################
#################################################

#do the job!
handle_dog <- function(export_dog_graphs, data_location)
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
    if (abs(dist_c_m[i]) > FILTER_DIST)
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
  start_date_time <- x_time_data[1];
  end_date_time <- tail(x_time_data, n=1);
  duration <- pretty_time(end_date_time - start_date_time);
  
  if (export_dog_graphs)
    draw_graphs_1dog(folder, dog_name, fp_str, x_time_data, norms_c_fp, dist_c_closest, dist_c_m,
                               dist_c_m1, dist_c_m2, coord_m1_positive_alignment,
                               coord_m1_negative_alignment, coord_m2_positive_alignment,
                               coord_m2_negative_alignment)
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
handle_dogs <- function (
        base_folder, 
        export_1dog_graphs,
        export_comparison_graphs,
        data_location
)
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
    tmp_results <- handle_dog(export_1dog_graphs, data_location[,i]);
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
  
  
  if ( export_comparison_graphs & number_dogs > 1 )
  {
    writeLines( paste("creating comparison graphs, exporting to:\n   ",base_folder));

    # prepare the values used for all the graphs

    setPDFFolder ( base_folder );
   
    ncols <- 2;
    nrows <- as.integer(ceiling(number_dogs/ncols));
    mfrow <- c(nrows, ncols);

    width <- ncols * 10;
    height <- nrows * 6;
    
    title_y <- min(0.99, 1 -0.08 / 2^floor((number_dogs - 1) / 2));
    title_fig <- c(0.49,0.51,title_y, 1);
      
    xlab_multidog <- get_translation("time");

    # prepare labels
    datetime_starts <- strptime(results_csv$res_t1, format=output_datetime_format);
    datetime_ends   <- strptime(results_csv$res_t2, format=output_datetime_format);
    datetime_labels_at <- list();
    datetime_labels    <- list();
    datetime_xaxis     <- list();
    for (i in 1:number_dogs)
    {
        datetime_xaxis[[i]] <- seq.POSIXt(datetime_starts[i], datetime_ends[i], by=1);
        datetime_labels_at[[i]] <- seq.POSIXt(datetime_starts[i], datetime_ends[i], length.out=9);
        datetime_labels[[i]] <- format(datetime_labels_at[[i]], graph_datetime_format);
    }


    # export boxplot with the distances of all the dogs
    startPDF(name=get_trans_filename("boxplot_all"));
    boxplot(results_dist_closest, names=dog_names, main=get_translation("dists_all_dogs_closest"), ylab=get_translation("dist_km"));
    endPDF();
    
    # export boxplot with the distances of all the dogs WITHOUT outliers
    startPDF(name=get_trans_filename("boxplot_all_no_outlier"));
    boxplot(results_dist_closest, names=dog_names, main=get_translation("dists_all_dogs_closest"), ylab=get_translation("dist_km"), outline=FALSE);
    endPDF();
    
    # export graph with distance of all dogs to their two sheep
    quickMultiPlot (ydata=results_dist_m1, ydata2=results_dist_m2,
                        xdata=datetime_xaxis, nplots=number_dogs,
                        "dists_all_dogs_both", c(), datetime_labels_at,
                        datetime_labels, dog_names, col="blue", col2="green");
        
    # export graph with distances of all dogs to the closest sheep
    quickMultiPlot (ydata=results_dist_closest, xdata=datetime_xaxis, nplots=number_dogs,
                        transl_key="dists_all_dogs_closest", transl_args=c(),
                        xlab_at=datetime_labels_at, xlabels=datetime_labels,
                        mains=dog_names);
    
    # export graph with distances of all dogs to the mean position of the sheep
    quickMultiPlot (ydata=results_dist_mean, xdata=datetime_xaxis, nplots=number_dogs,
                        transl_key="dists_all_dogs_mean", transl_args=c(),
                        xlab_at=datetime_labels_at, xlabels=datetime_labels,
                        mains=dog_names);
    
    
    ## now the same with histograms
    
#TODO: quickPlot =)
    
    # export graph with distances of all dogs to the closest sheep
    key <- c("hist_dists_all_dogs_closest");
    startPDF ( name=get_trans_filename(key), w=width, h=height, mfrow=mfrow );
    preparePDFTitle(get_translation(key));
    for (i in 1:number_dogs)
    {
      data <- as.data.frame(results_dist_closest[i]);
      justHist ( data[,1], main_transl_key="", main_direct=dog_names[i], ylim=c(0,12000) );
    }
    endPDF();
    
    # export graph with distances of all dogs to the mean position of the sheep
    key <- c("hist_dists_all_dogs_mean");
    startPDF (name=get_trans_filename(key), w=width, h=height, mfrow=mfrow);
    preparePDFTitle(get_translation(key));
    for (i in 1:number_dogs)
    {
      data <- as.data.frame(results_dist_mean[i]);
      xlab=dog_names[i];
      justHist (data[,1], main_transl_key="", main_direct=dog_names[i], ylim=c(0,12000));
    }
    endPDF();
    
    
    
    
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
    key <- c("hist_all_dogs_coord_both_sheep");
    subtitle = get_translation("hist_colors_label");
    startPDF (name=get_trans_filename(key), w=width, h=height, mfrow=mfrow);
    preparePDFTitle(get_translation(key), subtitle=subtitle);
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
        abline(v=0);
      } else {
        plot(0,0,col="white",xlab="",ylab="", axes=FALSE);
        text_no_data <- get_translation("no_available_data", c(dog_names[i]));
        text(x=0, y=0, cex=1.5, labels=text_no_data);
      }
    }
    endPDF();
    
  }
  
  
  writeLines("\nmagic finished =)\n");
}




##################################################
##################################################
##################################################
##################################################
##################################################
##################################################
#
#  Backup old draw graph function

draw_graphs_1dog <- function(folder, dog_name, fp_str, x_time_data, norms_c_fp, dist_c_closest, dist_c_m,
                               dist_c_m1, dist_c_m2, coord_m1_positive_alignment,
                               coord_m1_negative_alignment, coord_m2_positive_alignment,
                               coord_m2_negative_alignment)
{

  start_date_time <- x_time_data[1];
  end_date_time <- tail(x_time_data, n=1);

  axis_dates <- seq.POSIXt(start_date_time, end_date_time, length.out=9);
  axis_labels <- format(axis_dates, graph_datetime_format);


  setPDFFolder ( folder );
  
  # distances from the dog to the given fixed point
  key <- "graph_dist_fp";
  args <- c(dog_name, fp_str);
  makePlot( x=x_time_data, y=norms_c_fp, name_transl_key=key, name_transl_args=args,
                  custom_datetime_labels=axis_labels, custom_datetime_labels_at=axis_dates);

  
  # histogram of the distances from the dog to the given fixed point
  key <- "hist_dist_fp";
  args <- c(dog_name, fp_str);
  makeHist( x=norms_c_fp, name_transl_key=key, name_transl_args=args, useDefaultBreaks=FALSE );

  # distances to the closest sheep
  key <- "graph_closest";
  args <- c(dog_name);
  makePlot( x=x_time_data, y=dist_c_closest, name_transl_key=key, name_transl_args=args,
                  custom_datetime_labels=axis_labels, custom_datetime_labels_at=axis_dates );
  
  # normalised distance dog-sheep mouton le plus proche
  key <- "graph_closest_norm";
  args <- c(dog_name);
  makePlot ( x=x_time_data, y=dist_c_closest, name_transl_key=key, name_transl_args=args, ylim=c(0,1),
                  custom_datetime_labels=axis_labels, custom_datetime_labels_at=axis_dates);
   
  # histogram of the distances dog-sheep mouton le plus proche
  key <- "graph_hist_dist_closest";
  args <- c(dog_name);
  makeHist ( x=dist_c_closest, name_transl_key=key, name_transl_args=args, useDefaultBreaks=FALSE );
  
  # distance dog-sheep ? la position moyenne du mouton
  key <- "graph_dist_mean";
  args <- c(dog_name);
  makePlot( x=x_time_data, y=dist_c_m, name_transl_key=key, name_transl_args=args,
                  custom_datetime_labels=axis_labels, custom_datetime_labels_at=axis_dates);
  
  # normalised distance dog-sheep ? la position moyenne du mouton
  key <- "graph_dist_mean_norm";
  args <- c(dog_name);
  makePlot( x=x_time_data, y=dist_c_m, name_transl_key=key, name_transl_args=args, ylim=c(0,1),
                  custom_datetime_labels=axis_labels, custom_datetime_labels_at=axis_dates);
    
  # histogram of the distances dog-sheep ? la position moyenne du mouton
  key <- "graph_hist_dist_mean";
  args <- c(dog_name);
  makeHist ( x=dist_c_m, name_transl_key=key, name_transl_args=args, useDefaultBreaks=FALSE );
   
  # Distances dog to both sheep: 2 lines, one green, one blue
  key <- "graph_dist_both_sheep";
  args <- c(dog_name);
  data_m1 <- as.data.frame(dist_c_m1);
  data_m2 <- as.data.frame(dist_c_m2);
  extraFn <- function ()
  {
    lines(x_time_data, data_m2[,1], type='l', col='green', xaxt='n');
  }
  makePlot ( x=x_time_data, y=data_m1[,1], name_transl_key=key, name_transl_args=args, col="blue", extraFn=extraFn,
                  custom_datetime_labels=axis_labels, custom_datetime_labels_at=axis_dates );


  # histogram of coordination: we combine orientation and in front of to obtain an overall "coordination index"
  # we will draw two histograms:
  # - one when the alignment is positive, on the top half of the graph
  # - the other for negative alignment which will be on the bottom half

  coords <- list(
                filter_no_nan(coord_m1_positive_alignment),
                filter_no_nan(coord_m1_negative_alignment),
                filter_no_nan(coord_m2_positive_alignment),
                filter_no_nan(coord_m2_negative_alignment)
           );
  # find which one has elements != NaN
  has_coord <- Map ( function(v) { length(v) > 0 }, coords);

  # find if there is data to draw
  should_draw_coord <- Reduce (function (tmp, has_c){tmp | has_c}, has_coord, FALSE); # makes TRUE if at least one has no NaN

  if ( ! should_draw_coord )
      writeLines ("Coordination histogram can NOT be created")
  else
  # if yes, let's start =)
  {
    # create counts for the 4 data sets (is also ok if only NaN)
    bs <- seq(-1,1,length.out=HISTOGRAM_CLASSES);
    hists_no_plot <- list(
                         hist(coords[[1]], plot=FALSE, breaks=bs),
                         hist(coords[[2]], plot=FALSE, breaks=bs),
                         hist(coords[[3]], plot=FALSE, breaks=bs),
                         hist(coords[[4]], plot=FALSE, breaks=bs)
                     );
    max_freq <- 0;
    for ( i in c(1,3) )
    {
      sum <- sum (hists_no_plot[[i]]$counts, hists_no_plot[[i+1]]$counts);
      hists_no_plot[[i]]$counts <- hists_no_plot[[i]]$counts / sum;
      hists_no_plot[[i+1]]$counts <- -hists_no_plot[[i+1]]$counts / sum;
      max_tmp <- max (hists_no_plot[[1]]$counts, abs(hists_no_plot[[i+1]]$counts));
      max_freq <- max(max_freq, max_tmp);
    }

    # prepare additional text
    text_y_top <- 1.03 * max_freq;
    text_y_bottom <- 1.03 * -max_freq;
    text_x_left <- -0.5;
    text_x_right <- 0.5;
    # prepare other parameters
    xlab <- "";
    ylab <- get_translation ("rel_freq");
    key <- "graph_hist_coord_both_sheep";
    args <- c(dog_name);
    ylim <- c(-max_freq, max_freq);
    filename <- get_trans_filename (key, args);
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
      # if coords[[1 or 3]] have data, plot it
      if (has_dataPos)
        plot_f (data=dataPos, col=colPos);

      # if coords[[2 or 4]] have data, plot it
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
    startPDF ( name=filename, mfrow=c(1, 2), w=1.4*DEFAULT_PDF_WIDTH, h=1.4*DEFAULT_PDF_HEIGHT ); # make 2 graphs, 1 for each sheep
    preparePDFTitle (title);
    for (startI in c(1,3))
    {
      sheep_i <- 2 - (startI %% 3); # 1->1, 3->2
      main <- get_translation("sheep_i", c(sheep_i));
      i1 <- startI; i2 <- startI + 1;
      justCoordHist (dataPos=hists_no_plot[[i1]], dataNeg=hists_no_plot[[i2]],
                      has_dataPos=has_coord[[i1]], has_dataNeg=has_coord[[i2]],
                      main=main, ylim=ylim, xlab=xlab, ylab=ylab, colPos=cols[[i1]],
                      colNeg=cols[[i2]], suppl_text_x=c(text_x_left,text_x_right),
                      suppl_text_y=c(text_y_top,text_y_bottom));
    }
    endPDF();
  }

  writeLines("");
  writeLines("");
}

#
#
#  WARNING:
#  as this program is separated in several files, a trick is used that requires that:
#  the file is SOURCED. 
#   From the R console this is done using the source() function.
#   From a graphical interface, there must be a button, or an entry in a menu to do that.
#


########################################
# BEGIN R SETUP ########################
# 
# first, clean the working environment, if an R session is reused
rm ( list=ls(all=TRUE) )
# we use this to find where this file is located in the file system,
# and load the file given as arg from the same path
  # this seems to be tricky: you NEED!! to SOURCE the file, not run it line by line
if ( ! exists("source_folder") )
  source_folder <<- sys.frame(1)$fileName;
if (is.null(source_folder))
  source_folder <<- sys.frame(1)$ofile;
source_file <- function (filename)
{
  filepath <- paste(dirname(source_folder), filename, sep = .Platform$file.sep);
  source(filepath);
}

# include computations file
source_file("analysis.R")
#include translation file
source_file("translate.R")
# the plot file is included only when starting the analysis, to adapt to
# the values from the GUI

# if this fails, you need to install the RGtk2 package, available at:
library("RGtk2");
#
#
# END R SETUP ########################
########################################



# define constants
DATA_SEP = ";"; # symbol between values in input files
RADIUS_EARTH <- 6371L; # km
HISTOGRAM_CLASSES <<- 200; # `how many columns in one histogram?'
LANGS <- c(FR=1,DE=2); # indexes for language
LANG <<- LANGS[["FR"]]; # used in translate.R
FILTER_DIST <<- 0.05; # km; for coordination value, use only when animal closer than that

# This folder is where we output the comparison (several data sets together) graphs
comparisonOutputFolder <-
  if ( .Platform$OS.type == "unix" ) "/tmp/" else {
      if ( .Platform$OS.type == "windows" )
        paste(Sys.getenv("TEMP"),.Platform$file.sep,sep="");
  }

datasetOutputFolder <- ".";


useFixedPoint <- FALSE;
fixedPoint_north <- 0;
fixedPoint_east <- 0;

#### this object is very important: we use it to collect the data
#### about the animals (animal name, filepath)
#### We define it before sourcing analysis.R
AnimalsDataSet <-
  setClass ("AnimalsDataSet",
            slots=c(
                numEntries="numeric", # number of entries
                numSheep="numeric", # constant over experiments
                numAnimals="numeric", # same
                outputFolder="character", # vector of single names
                sheepNames="list", # list of vectors
                sheepFiles="list", # list of vectors
                animalNames="list", # list of vectors
                animalFiles="list", # list of vectors
                useFixedPoint="logical", # vector for each data set
                fixedPoint="list" # for each data set: c(N,E)
            ));

extractAnimalData <- function (x, i)
{
  AnimalsDataSet(
    numEntries=length(i),
    numSheep=x@numSheep,
    numAnimals=x@numAnimals,
    outputFolder=x@outputFolder[i],
    sheepNames=x@sheepNames[i],
    sheepFiles=x@sheepFiles[i],
    animalNames=x@animalNames[i],
    animalFiles=x@animalFiles[i],
    useFixedPoint=x@useFixedPoint[i],
    fixedPoint=x@fixedPoint[i]);
}


MAX_ANIMALS <- 5;

# temp debug data
tmpdir <- paste("sample", .Platform$file.sep, sep="");
temp_dog_data <- list (
    list( # animal names
      c("Helix","Tirex")
    ),
    list( # sheep names
      c("m1","m2","m3")
    ),
    c( # output folders
      tmpdir
    ),
    list( # animal data files
      c(
        paste(tmpdir,"Helix.txt", sep=""),
        paste(tmpdir,"Tirex-modified.txt", sep="")
      )
    ),
    list( # sheep data files
      c(
        paste(tmpdir,"mouton1.txt", sep=""),
        paste(tmpdir,"mouton2.txt", sep=""),
        paste(tmpdir,"mouton3.txt", sep="")
      )
    ),
    c(
      TRUE
    ),
    list( # fixed points
      c(0,0)
    )
);

#itinialize the data with an empty data set
#animals_data_set <- AnimalsDataSet(numEntries=0);

# Initialize with the debug data above
animals_data_set <- AnimalsDataSet(
        numEntries    =1,
        numAnimals    =2,
        numSheep      =2,
        animalNames   =temp_dog_data[[1]],
        sheepNames    =temp_dog_data[[2]],
        outputFolder  =temp_dog_data[[3]],
        animalFiles   =temp_dog_data[[4]],
        sheepFiles    =temp_dog_data[[5]],
        useFixedPoint =temp_dog_data[[6]],
        fixedPoint    =temp_dog_data[[7]]
        );

# when the user chooses a first file from a folder, we then propose that folder
fastFolder <- ".";

######################################################################
# graphical interface objects, from which we read the values


# the main window
window <- gtkWindow();

###############
##
## here we define all the components where we take the data from
##
# text entry for the number of classes in the histograms
histClassesEntry <- gtkEntry();
histClassesEntry$setText(as.character(HISTOGRAM_CLASSES));
# text entry for the filter distance
filterDistEntry <- gtkEntry();
filterDistEntry$setText(as.character(FILTER_DIST));
# label for the folder where the comparison CSV file and graphs are output
#labelComparisonOutputFolder <- gtkLabel(base_folder);
labelComparisonOutputFolder <- gtkLabel(comparisonOutputFolder);
# two check boxes for the graph export options
checkBox_exp_1animal_graphs <- gtkCheckButton(label="Export graphs for each data set");
checkBox_exp_1animal_graphs$active <- TRUE;
checkBox_exp_allanimals_graphs <- gtkCheckButton(label="Export comparison graphs");
checkBox_exp_allanimals_graphs$active <- FALSE;
checkBox_exp_allanimals_graphs$sensitive <- FALSE;
# choose the language
comboLangs <- gtkComboBoxNewText();
comboLangs$appendText("FR");
comboLangs$appendText("DE");
comboLangs$active <- 0;
# this label displays what's happening (eg. the chosen dogs, or "Calculs en cours..")
animalCombo <- gtkComboBoxNewText();
sheepCombo  <- gtkComboBoxNewText();
# the 'browse' buttons will add the filenames to these two lists
animal_filenames <- Map ( function(i){""}, 1:MAX_ANIMALS);
sheep_filenames <- Map ( function(i){""}, 1:MAX_ANIMALS);
# the labels that will display the chosen filenames
animal_filenameLabels <- Map ( function(i){gtkLabel("??")}, 1:MAX_ANIMALS);
sheep_filenameLabels <- Map ( function(i){gtkLabel("??")}, 1:MAX_ANIMALS);
# the entries to choose the name for each animal and sheep
animal_nameEntries <- Map ( function(i){gtkEntry()}, 1:MAX_ANIMALS);
sheep_nameEntries <- Map ( function(i){gtkEntry()}, 1:MAX_ANIMALS);
# text displaying the `fastFolder', can be changed
label_datasetOutputFolder <- gtkLabel();
# 2 text entries for the fixed point: North, East
check_useFixedPoint <- gtkCheckButton(label="Use fixed point:");
entryFPN <- gtkEntry();
entryFPE <- gtkEntry();
# display the current data set
data_setLabel <- gtkLabel("");


addButtonHandler <- function (but, handler, data=NULL)
{
  gSignalConnect (but, "pressed", handler, data=data);
  gSignalConnect (but, "activate", handler, data=data);
}

# save the given folder, and arrange to display in label_dataset (short name)
# give 2nd arg FALSE for comparison
updateOutputFolder <- function (folder, is_datasetFolder)
{
  if ( is_datasetFolder )
    datasetOutputFolder <<- folder
  else
    comparisonOutputFolder <<- folder;

  l <- nchar(folder);
  n_max <- 30;
  if (l > n_max) folder <- paste("...", substr(folder, l-n_max+1, l), sep="");
  if ( is_datasetFolder )
    label_datasetOutputFolder$setText(folder)
  else
    labelComparisonOutputFolder$setText(folder);
}

#user.data is: c(bool_is_animal, num) (bool is false for sheep)
choose_data_file <- function (button, user.data)
{
  d <- gtkFileChooserDialog(title="Choose a GPS data file", parent=window, action="open",
                            "gtk-cancel", GtkResponseType["cancel"],
                            "gtk-open", GtkResponseType["accept"]);
  d$setCurrentFolder(fastFolder);
  filter_txt_csv <- gtkFileFilterNew();
  filter_txt_csv$addPattern("*.txt");
  filter_txt_csv$addPattern("*.csv");
  filter_txt_csv$setName("Text data file");
  d$addFilter(filter_txt_csv);
  filter_all <- gtkFileFilterNew();
  filter_all$addPattern("*");
  filter_all$setName("All file types");
  d$addFilter(filter_all);
  v <- d$run();
  if (v == GtkResponseType["accept"])
  {
    f <- d$getFilename();
    disp_fname <- substr(basename(f), 1, 15) # maximum 15 characters displayed
    fastFolder <<- make_folder_path (dirname(f));
    dataoutFolder <- label_datasetOutputFolder$getText();
    if ( "" == dataoutFolder ) updateOutputFolder(fastFolder, TRUE);
    is_an <- user.data[1];
    num <- user.data[2];
    if (is_an)
    {
      animal_filenames[[num]] <<- f;
      animal_filenameLabels[[num]]$setText(disp_fname); # display only filename, not path
    }
    else
    {
      sheep_filenames[[num]] <<- f;
      sheep_filenameLabels[[num]]$setText(disp_fname); # display only filename, not path
    }
  }
  d$destroy();
}

filter_dir_f <- function (f)
{
  return(file_test("-d",f$filename));
}

make_folder_path <- function (f)
{
  pattern <- paste("([^",.Platform$file.sep,"])$", sep=""); # "([^/])$": anything but a / before end of line
  repl <- paste("\\1",.Platform$file.sep, sep=""); # replace with last char matched
  sub (pattern, repl, f, perl=TRUE);
}

# is_datasetFolder: TRUE for dataset output folder, FALSE if we choose base folder
choose_folder <- function(button, is_datasetFolder)
{
  d <- gtkFileChooserDialog(title="Choose a directory", parent=window, action="select-folder",
                            "gtk-cancel", GtkResponseType["cancel"],
                            "gtk-open", GtkResponseType["accept"]);
  filter <-  gtkFileFilter();
  gtkFileFilterAddCustom(filter, GtkFileFilterFlags['filename'], filter_dir_f);
  gtkFileChooserAddFilter(d, filter);
  v <- d$run();
  if (v == GtkResponseType["accept"])
  {
    f <- make_folder_path (d$getFilename());
    updateOutputFolder (f, is_datasetFolder);
  }
  d$destroy();
}

# displays a message to the user
errDialog <- function(text)
{
  dialog <- gtkMessageDialog(text=text, parent=window, flags=0, type="error", buttons="none")
  myf <- function(button, data){dialog$destroy();}
  b <- gtkDialogAddButton(dialog, "Close", 1);
  icon <- gtkImageNewFromIconName("gtk-close", GtkIconSize["button"]);
  b$setImage(icon);
  addButtonHandler(b, myf)
}

# obtain the number of histogram classes in the user interface
get_histClasses <- function()
{
  hC <- histClassesEntry$getText()
  histClasses <- as.numeric(hC);
  if (is.na(histClasses))
  {
    errMsg <- paste("Invalid number of histogram classes: \""
                    ,hC,"\", using: ", HISTOGRAM_CLASSES,"\n", sep="");
    histClassesEntry$setText(as.character(HISTOGRAM_CLASSES));
    stop (errMsg);
  } else {
    return (histClasses);
  }
}

update_FPAvailable <- function (but)
{
  useFP <- ! but$active; # this call happens before state changed
  entryFPN$setSensitive(useFP);
  entryFPE$setSensitive(useFP);
}

# obtain the value of the filter distance in the user interface
get_filterDist <- function()
{    
  dF <- filterDistEntry$getText();
  distFilter <- as.numeric(dF);
  if (is.na(distFilter))
  {
    errMsg <- paste(errMsg, "Invalid filter distance: \""
                     ,dF,"\", using: ", FILTER_DIST,"\n", sep="");
    filterDistEntry$setText(as.character(FILTER_DIST));
    stop (errMsg);
  } else {
    return (distFilter)
  }
}
# find the output folder, and validate it
get_baseFolder <- function()
{
  bf_text <- labelComparisonOutputFolder$getText();
  bf <- paste(bf_text, .Platform$file.sep, sep="");
  if (bf == "")
    stop ("No output folder for the comparison data was chosen")
  else if (!file_test("-d", bf))
    stop (paste("The output folder for the comparison data(",bf,") does not exist!", sep=""));
  return (bf);
}

# obtain the names of the test animals if arg=TRUE, of sheep otherwise
# num is the max number to collect
get_names <- function(is_test_animal)
{
  names <- c();
  entries <- if (is_test_animal) animal_nameEntries else sheep_nameEntries;
  num     <- if (is_test_animal) animals_data_set@numAnimals else animals_data_set@numSheep;
  i <- 1;
  for ( e in entries)
  {
    name <- e$getText();
    if (name == "")
      stop ("Unspecified animal name");
    names <- append(names, name);
    if ( i >= num )
      return (names);
    i <- i + 1;
  }
  return (names);
}

reset_temp_data <- function()
{
  animal_filenames <- Map ( function(i){""}, 1:MAX_ANIMALS);
  sheep_filenames <- Map ( function(i){""}, 1:MAX_ANIMALS);
  for ( l in animal_filenameLabels ) l$setText("??");
  for ( l in sheep_filenameLabels ) l$setText("??");
  for ( e in animal_nameEntries ) e$setText("");
  for ( e in sheep_nameEntries ) e$setText("");
  if ( check_useFixedPoint$active )
  {
    entryFPN$setText("0");
    entryFPE$setText("0");
  }
  datasetOutputFolder <- ".";
  label_datasetOutputFolder$setText("");
}

get_fixed_point <- function()
{
  FPN <- as.double(entryFPN$getText());
  FPE <- as.double(entryFPE$getText());
  if (is.na(FPN) | is.na(FPE))
    stop("The fixed point is not a number!");
  c(FPN,FPE);
}

collect_current_data <- function(button)
{
  tryCatch(
  {
    a_num <- animals_data_set@numAnimals;
    s_num <- animals_data_set@numSheep;
    # find which files do not exist
    valid_animal_filenames <- Reduce(
      function(v,f){ v & file_test("-f", f) }, animal_filenames[1:a_num], TRUE);
    valid_sheep_filenames <- Reduce(
      function(v,f){ v & file_test("-f", f) }, sheep_filenames[1:s_num], TRUE);

    if ( ! valid_animal_filenames || ! valid_sheep_filenames )
      stop("Some data files do not exist");

    a_names <- get_names (TRUE);
    s_names <- get_names (FALSE);

    if ( ! file_test ("-d", datasetOutputFolder) )
      stop("The output folder for the graph output does not exist");

    useFP <- check_useFixedPoint$active;
    fp <- if (useFP) get_fixed_point() else c(0,0);

    # include the data in the total data set
    animals_data_set@numEntries <<- animals_data_set@numEntries + 1;
    n <- animals_data_set@numEntries;
    animals_data_set@outputFolder[[n]] <<- datasetOutputFolder;
    animals_data_set@sheepNames[[n]] <<- as.list(s_names);
    animals_data_set@animalNames[[n]] <<- as.list(a_names);
    animals_data_set@sheepFiles[[n]] <<- sheep_filenames;
    animals_data_set@animalFiles[[n]] <<- animal_filenames;
    animals_data_set@useFixedPoint[[n]] <<- useFP;
    animals_data_set@fixedPoint[[n]] <<- fp;

    # make the text to display for this data part
    entry <- "(";
    la <- length(a_names);
    if (la > 1)
      for (i in 1:(la-1))
        entry <- paste(entry, a_names[i], ", " , sep="");
    entry <- paste(entry, a_names[la], " <-> ", sep="");
    ls <- length(s_names);
    if (ls > 1)
      for (i in 1:(ls-1))
        entry <- paste(entry, s_names[i], ", " , sep="");
    entry <- paste(entry, s_names[ls], ")", sep="");
    entry <- paste(entry, "=>", basename(fastFolder));

    spl <- strsplit(data_setLabel$getText(), "\n")[[1]];
    spl <- grep ( "^$", spl, invert=T, value=T);
    text <- if ( length(spl) > 0 )
    {
      pre <- paste( spl, sep="\n" );
      paste (pre, entry, sep="\n");
    } else {
      entry;
    }
    if ( n < 4 ) for (i in n:3) text <- paste(text, "\n"); # add newlines to have min. 4 lines
    data_setLabel$setText(text);

    reset_temp_data();
  }
  , error = function (e)
  {
    msg <- paste("Error:", geterrmessage());
    errDialog (msg);
  });
}

# Start the analysis with the provided values
# user.data = c(num_test_animals, num_sheep)
startStuff <- function (button)
{
  tryCatch( # in case of error, give bad message
    {
      if (animals_data_set@numEntries == 0)
        stop("No data to analyse!");
      HISTOGRAM_CLASSES <<- get_histClasses();
      FILTER_DIST <<- get_filterDist();
      LANG <<- comboLangs$active + 1;
      comparisonOutputFolder <- get_baseFolder();
      export_1animal_graph <- checkBox_exp_1animal_graphs$active;
      export_allanimals_graph <- checkBox_exp_allanimals_graphs$active;

      start_analysis (
            animals_data_set,
            comparisonOutputFolder,
            export_1animal_graph,
            export_allanimals_graph
            );
    },
    error = function(e)
    {
      msg <- paste("Error:", geterrmessage());
      errDialog(msg);
      cs <- sys.calls();
      msg <- paste(msg, "Details:", cs, sep="\n\n" );
      writeLines(msg);
    }
  );
}

# this function adds stuff to the end of the given box using packStart
create_general_options <- function (box)
{  
  hboxOutputFolder <- gtkHBox(F, 10);
  box$packStart(hboxOutputFolder, F, F, 3);
  hboxOutputFolder$packStart(gtkLabel("Output folder:"), F, F, 3);
  hboxOutputFolder$packStart(labelComparisonOutputFolder, F, F, 3);
  baseFolderButton <- gtkButton("Choose...");
  hboxOutputFolder$packEnd(baseFolderButton, F, F, 10);
  addButtonHandler(baseFolderButton, choose_folder, FALSE);

  hboxExpGraphs <- gtkHBox ( T, 10 );
  box$packStart(hboxExpGraphs, F, F, 3);
  hboxExpGraphs$packStart(checkBox_exp_1animal_graphs, F, F, 3);
  hboxExpGraphs$packEnd(checkBox_exp_allanimals_graphs, F, F, 3);

  
  hboxLanguage <- gtkHBox(F, 10);
  box$packStart(hboxLanguage, F, F, 3);
  labelLang <- gtkLabel("Lang:");
  hboxLanguage$packStart(labelLang, T, F, 3);
  hboxLanguage$packStart(comboLangs, T, F, 3);
  
  hboxSettings <- gtkHBox(F, 10);
  box$packStart(hboxSettings, F, F, 3);
  labelHist <- gtkLabel("Number of histogram classes:");
  hboxSettings$packStart(labelHist, F, F, 3);
  hboxSettings$packEnd(histClassesEntry, F, F, 3);
  histClassesEntry$setWidthChars(7);
  
  hboxSettings2 <- gtkHBox(F, 10);
  box$packStart(hboxSettings2, F, F, 3);
  labelFilter <- gtkLabel("Filter distance:");
  hboxSettings2$packStart(labelFilter, F, F, 3);
  hboxSettings2$packEnd(filterDistEntry, F, F, 3);
  filterDistEntry$setWidthChars(7);
 }


# callback for the ok button in animal number choice, b is useless but needed
# user.data = list ( container to remove, data for create_data_choice_box)
show_data_choice <- function(b, user.data=NULL)
{
  window$title <- "Choose the data to analyse";
  if ( ! is.null (user.data))
    window$remove (user.data);


  # create the container with the number of animals
  animals_data_set@numSheep <<- sheepCombo$active + 1;
  animals_data_set@numAnimals <<- animalCombo$active + 1;

  cont <- create_data_choice_box();

  window$add (cont);
  window$resize(1,1); # make minimum size
}


# num is the number of the given animal/sheep
# give isAnimal = TRUE tfor test animal; FALSE for sheep
make_choose_file_button <- function (isAnimal, num)
{
  b <- gtkButtonNew();
  icon <- gtkImageNewFromIconName("gtk-file", GtkIconSize["button"]);
  b$setImage(icon);
  user.data <- c(isAnimal, num);
  addButtonHandler (b, choose_data_file, user.data);
  return (b);
}

# Create a frame for choosing the names and files for animals,
# and some settings: Output folder, histogram classes, ...
create_data_choice_box <- function()
{

  # get data for how to set up the window
  numberAnimals <- animals_data_set@numAnimals;
  numberSheep <- animals_data_set@numSheep;
  numRows <- max (numberAnimals, numberSheep);

  # create the total container
  contDataChoice <- gtkFrame();

  vboxAll <- gtkVBox(spacing=3);

  hboxDataInput <- gtkHBox(spacing=3);

  # create the components for the test animals
  vboxAnimals <- gtkVBox(spacing=3); 
  vboxAnimals$packStart (gtkLabel("Data for animals"), padding=5);
  for (i in 1:numberAnimals)
  {
    hbox <- gtkHBox (spacing=2);
    animal_nameEntries[[i]]$setText(paste("Test animal ", i));
    animal_nameEntries[[i]]$setWidthChars(12);
    b <- make_choose_file_button (TRUE, i);
    hbox$packStart(animal_nameEntries[[i]], expand=F, fill=F);
    hbox$packStart(animal_filenameLabels[[i]], expand=T, fill=F);
    hbox$packEnd(b, expand=F, fill=F);
    vboxAnimals$packStart (hbox);
  }
  # complete to have the same number of rows on both side
  if (numRows > numberAnimals)
    for (i in (numberAnimals+1):numRows)
      vboxAnimals$packStart(gtkLabel());


  # create the components for the sheep
  vboxSheep <- gtkVBox(spacing=3); 
  vboxSheep$packStart (gtkLabel("Data for sheeps"), padding=5);
  for (i in 1:numberSheep)
  {
    hbox <- gtkHBox (spacing=2);
    sheep_nameEntries[[i]]$setText(paste("Sheep ", i));
    sheep_nameEntries[[i]]$setWidthChars(12);
    b <- make_choose_file_button (FALSE, i);
    hbox$packStart(sheep_nameEntries[[i]], expand=F, fill=F);
    hbox$packStart(sheep_filenameLabels[[i]], expand=T, fill=F);
    hbox$packEnd(b, expand=F, fill=F);
    vboxSheep$packStart (hbox);
  }
  # complete to have the same number of rows on both side
  if (numRows > numberSheep)
    for (i in (numberSheep+1):numRows)
      vboxAnimals$packStart(gtkLabel());


  # add the 3 in horizontal
  hboxDataInput$packStart (vboxAnimals, expand=T, fill=T);
  hboxDataInput$packStart (gtkVSeparator(), expand=F, fill=F, padding=3);
  hboxDataInput$packStart (vboxSheep, expand=T, fill=T);
  vboxAll$packStart (hboxDataInput, expand=T, fill=F);
  # little separation line
  vboxAll$packStart (gtkHSeparator(), expand=F, fill=F);

  # Use fixed point?
  hboxFPInput <- gtkHBox(spacing=3);
  entryFPN$setWidthChars(9);entryFPE$setWidthChars(9);
  entryFPN$setText("0.0");entryFPE$setText("0.0");
  hboxFPInput$packStart(check_useFixedPoint, expand=T, fill=F);
  addButtonHandler (check_useFixedPoint, update_FPAvailable);
  hboxFPInput$packEnd(entryFPE, expand=F, fill=F);
  hboxFPInput$packEnd(entryFPN, expand=F, fill=F);
  entryFPN$setSensitive(FALSE);
  entryFPE$setSensitive(FALSE);
  hboxFPInput$packEnd(gtkLabel("(N,E):"), expand=F, fill=F);
  vboxAll$packStart (hboxFPInput, expand=T, fill=F);

  # button to add this set of files to the data to analyse
  hbox <- gtkHBox();
  hbox$packStart(gtkLabel("Out folder:"), expand=T, fill=F, padding=1);
  button_browseOutFolder <- gtkButtonNew();
  addButtonHandler (button_browseOutFolder, choose_folder, TRUE);
  icon <- gtkImageNewFromIconName("gtk-directory", GtkIconSize["button"]);
  button_browseOutFolder$setImage(icon);
  hbox$packEnd (button_browseOutFolder, expand=F, fill=F);
  hbox$packEnd(label_datasetOutputFolder, expand=F, fill=F, padding=1);
  vboxAll$packStart (hbox, expand=T, fill=F, padding=5);

  hbox <- gtkHBox();
  button_add <- gtkButtonNewWithLabel("Add this data set");
  addButtonHandler (button_add, collect_current_data);
  icon <- gtkImageNewFromIconName("gtk-go-forward-ltr", GtkIconSize["button"]);
  button_add$setImage(icon);
  hbox$packStart (button_add, expand=T, fill=F);
  vboxAll$packStart (hbox, expand=T, fill=F, padding=5);

  vboxAll$packStart (gtkHSeparator(), expand=T, fill=F);


  vboxAll$packStart (gtkLabel("Chosen data:"), expand=T, fill=T)
  vboxAll$packStart (data_setLabel, expand=T, fill=T);
  data_setLabel$setText("\n\n\n\n");


  vboxAll$packStart (gtkHSeparator(), expand=T, fill=F);

  # create the general options and start button at the bottom
  create_general_options (vboxAll);
 

  # add the "start" button
  hbox <- gtkHBox(F, 10);
  vboxAll$packStart(hbox, F, F, 3);
  
  buttonStart <- gtkButton("Analyser");
  icon <- gtkImageNewFromIconName("gtk-apply", GtkIconSize["button"]);
  buttonStart$setImage(icon);
  hbox$packEnd(buttonStart, T, F, 3);
  addButtonHandler(buttonStart, startStuff);


  contDataChoice$add (vboxAll);

  return (contDataChoice);
}

# Create a frame to choose the number of test animals and number of sheep
create_format_choice_box <- function()
{
  window$title <- "Choose the test format";
  contFormatChoice <- gtkFrame();

  layout <- gtkVBox( homogeneous=TRUE, spacing = 9 );

  offset_y <- 25; # how much space to the left 
  offset_x <- 15; # where, from the top, we start adding components
  delta_x_comp <- 73; # horiz. distance from label to combobox
  delta_y_group <- 29; # vert. distance 

  # Choose how many test animals to use
  animalNumLabel <- gtkLabel ("How many animals (lama/dog):");
  for (i in seq(1,4))
    animalCombo$appendText(as.character(i));
  animalCombo$setActive(1);
  hbox1 <- gtkHBox(spacing=10);
  hbox1$packStart(animalNumLabel, expand=T, fill=F);
  hbox1$packStart(animalCombo, expand=T, fill=F);
  layout$add(hbox1);

  # Choose how many sheep to use
  offset_y <- offset_y + delta_y_group;

  sheepNumLabel <- gtkLabel ("How many sheep:");
  for (i in seq(1,4))
    sheepCombo$appendText(as.character(i));
  sheepCombo$setActive(2);
  hbox1 <- gtkHBox(spacing=10);
  hbox1$packStart(sheepNumLabel, expand=T, fill=F);
  hbox1$packStart(sheepCombo, expand=T, fill=F);
  layout$add(hbox1);


  # the ok button at the bottom will show the data dialog, with show_data_choice()
  offset_y <- offset_y +delta_y_group;
  offset_x <- delta_x_comp;

  buttonOk <- gtkButton("OK");
  buttonOk$grabFocus();
  icon <- gtkImageNewFromIconName("gtk-ok", GtkIconSize["button"]);
  buttonOk$setImage(icon);
  hbox1 <- gtkHBox(spacing=10);
  hbox1$packStart(buttonOk, expand=T, fill=F);
  layout$packStart(hbox1, expand=F, fill=T);

  # the data we pass to show_data_choice is this container (to remove it)
  addButtonHandler (buttonOk, show_data_choice, data=contFormatChoice);

  contFormatChoice$add (layout)

  return (contFormatChoice);
}

create_GUI <- function ()
{
# just show format window, it will call further to data choice window
  contFmtChoice <- create_format_choice_box();
  window$add(contFmtChoice);
  window$resize(1,1); # make minimum size
  window$move (gdkScreenWidth()/2 - 99, gdkScreenHeight() / 2 - 99);
}

create_GUI();


#
#
#  WARNING:
#  as this program is separated in several files, a trick is used that requires that:
#  the file is SOURCED. 
#   From the R console this is done using the source() function.
#   From a graphical interface, there must be a button, or an entry in a menu to do that.
#


# settings for the language
LANGS <- c(FR=1,DE=2);
LANG <<- LANGS[["FR"]];

#change the base working folder here
base_folder <- "%USERPROFILE%\\Desktop";

DATA_SEP = ";"; # symbol between values in input files

RADIUS_EARTH <- 6371L; # km

HISTOGRAM_CLASSES <<- 200;

fixedPoint_north <- 0;
fixedPoint_east <- 0;

# the script does not take into account the moments where the dog is far from the sheep,
# for alignment, in_front and these things.
# This value (FILTER_DIST) is the max distance. 10km will take everything into account, probably
#FILTER_DIST <- 0.03; # 30 meters
FILTER_DIST <<- 0.05; # 0.050 kilometers, takes everything into account!

# this seems to be tricky: you NEED!! to SOURCE the file, not run it line by line
test <- sys.frame(1)$fileName;
if (is.null(test))
  test <- sys.frame(1)$ofile;

# include computations file
analysispath <- paste(dirname(test),"analysis.R", sep = .Platform$file.sep);
source(analysispath);

#include translation file
translatepath <- paste(dirname(test),"translate.R", sep = .Platform$file.sep);
source(translatepath);

#include plot file
plotpath <- paste(dirname(test),"plot.R", sep = .Platform$file.sep);
source(plotpath);


# if this fails, you need to install RGtk2, available at:
# http://cran.r-project.org/web/packages/RGtk2/index.html
library("RGtk2");


MAX_ANIMALS <- 5;

#### this object is very important: we use it to collect the data
#### about the animals (animal name, filepath)
animals_data_set <- array();


# when the user chooses a first file from a folder, we then propose that folder
fastFolder <- base_folder;

######################################################################
# graphical interface objects, from which we read the values


#entryDogName <- gtkEntry();
#labelDogFile <- gtkLabel("");
#dogFile <- "";
#labelSheep1File <- gtkLabel("");
#sheep1File <- "";
#labelSheep2File <- gtkLabel("");
#sheep2File <- "";





# the main window
window <- gtkWindow();

###############
##
## here we define all the components where we take the data from
##
# text entry for the number of classes in the histograms
histClassesEntry <- gtkEntry();
# text entry for the filter distance
filterDistEntry <- gtkEntry();
# label for the folder where the comparison CSV file and graphs are output
labelOutputFolder <- gtkLabel("??");
# two check boxes for the graph export options
checkBox_exp_1animal_graphs <- gtkCheckButton(label="Exporter graphes pour chaque chien", show=T);
checkBox_exp_1animal_graphs$active <- TRUE;
checkBox_exp_allanimals_graphs <- gtkCheckButton(label="Exporter graphes de comparaison", show=T);
checkBox_exp_allanimals_graphs$active <- TRUE;
# choose the language
comboLangs <- gtkComboBoxNewText();
comboLangs$appendText("FR");
comboLangs$appendText("DE");
comboLangs$active <- 0;
# this label displays what's happening (eg. the chosen dogs, or "Calculs en cours..")
#dogListLabel <- gtkLabel("Aucun chien choisi");
animalCombo <- gtkComboBoxNewText();
sheepCombo  <- gtkComboBoxNewText();
# the 'browse' buttons will add the filenames to these two lists
animal_filenames <- Map ( function(i){"??"}, 1:MAX_ANIMALS);
sheep_filenames <- Map ( function(i){"??"}, 1:MAX_ANIMALS);
# the labels that will display the chosen filenames
animal_filenameLabels <- Map ( function(i){gtkLabel(animal_filenames[[i]])}, 1:MAX_ANIMALS);
sheep_filenameLabels <- Map ( function(i){gtkLabel(sheep_filenames[[i]])}, 1:MAX_ANIMALS);
# the entries to choose the name for each animal and sheep
animal_nameEntries <- Map ( function(i){gtkEntry()}, 1:MAX_ANIMALS);
sheep_nameEntries <- Map ( function(i){gtkEntry()}, 1:MAX_ANIMALS);
# 2 text entries for the fixed point: North, East
entryFPN <- gtkEntry();
entryFPE <- gtkEntry();
# display the current data set
data_setLabel <- gtkLabel("Chosen data:");

#choose_file <- function(button, data)
#{
#  d <- gtkFileChooserDialog(title="Choose a directory", parent=window, action="open",
#                            "gtk-cancel", GtkResponseType["cancel"],
#                            "gtk-open", GtkResponseType["accept"]);
#  gtkFileChooserSetCurrentFolder(d, fastFolder);
#  v <- d$run();
#  if (v == GtkResponseType["accept"])
#  {
#    f <- d$getFilename();
#    fastFolder <<- dirname(f);
#    d$destroy();
#    if (data == "dog")
#    {
#      gtkLabelSetText(labelDogFile, basename(f));
#      dogFile <<- f;
#    }
#    if (data == "s1")
#    {
#      gtkLabelSetText(labelSheep1File, basename(f));
#      sheep1File <<- f;
#    }
#    if (data == "s2")
#    {
#      gtkLabelSetText(labelSheep2File, basename(f));
#      sheep2File <<- f;
#    }
#  }
#}

#user.data is: c(bool_is_animal, num) (bool is false for sheep)
choose_data_file <- function (button, user.data)
{
  d <- gtkFileChooserDialog(title="Choose a GPS data file", parent=window, action="open",
                            "gtk-cancel", GtkResponseType["cancel"],
                            "gtk-open", GtkResponseType["accept"]);
  gtkFileChooserSetCurrentFolder(d, fastFolder);
  v <- d$run();
  if (v == GtkResponseType["accept"])
  {
    f <- d$getFilename();
    disp_fname <- substr(basename(f), 1, 15) # maximum 15 characters displayed
    fastFolder <<- dirname(f);
    is_an <- user.data[1];
    num <- user.data[2];
    if (is_an)
    {
      animal_filenames[[num]] <- f;
      animal_filenameLabels[[num]]$setText(disp_fname); # display only filename, not path
    }
    else
    {
      sheep_filenames[[num]] <- f;
      sheep_filenameLabels[[num]]$setText(disp_fname); # display only filename, not path
    }
  }
  d$destroy();
}

filter_dir_f <- function (f)
{
  return(file_test("-d",f$filename));
}

choose_base_folder <- function(button, data)
{
  d <- gtkFileChooserDialog(title="Choose a directory", parent=window, action="select-folder",
                            "gtk-cancel", GtkResponseType["cancel"],
                            "gtk-open", GtkResponseType["accept"]);
  filter <-  gtkFileFilter();
  gtkFileFilterAddCustom(filter, GtkFileFilterFlags['filename'], filter_dir_f);
  gtkFileChooserAddFilter(d, filter);
  v <- d$run();
  if (v == GtkResponseType["accept"])
    labelOutputFolder$setText(d$getFilename());
  d$destroy();
}

# displays a message to the user
errDialog <- function(text)
{
  dialog <- gtkMessageDialog(text=text, parent=window, flags=0, type="error", buttons="none")
  myf <- function(button, data){dialog$destroy();}
  b <- gtkDialogAddButton(dialog, "fermer", 1);
  icon <- gtkImageNewFromIconName("gtk-close", GtkIconSize["button"]);
  b$setImage(icon);
  gSignalConnect(b, "pressed", myf)
}

#update_fixed_point <- function()
#{
#  fixedPoint_north <<- as.double(entryFPN$getText());
#  fixedPoint_east <<- as.double(entryFPE$getText());
#  
#  res <- !is.na(fixedPoint_east) & !is.na(fixedPoint_north);
#  
#  return (res);
#}

#add_dog <- function(button, user.data)
#{
#  dogName <- entryDogName$getText();
#  if (dogName == "" | dogFile == "" | sheep1File == "" | sheep2File == "" | !update_fixed_point())
#  {
#    errDialog("Pas assez d'informations fournies =)");
#  } else {
#    folder <- paste(dirname(dogFile), .Platform$file.sep, sep="");
#    GUI_dog_data <<- c(GUI_dog_data, c(dogName, folder, dogFile, sheep1File, sheep2File, fixedPoint_north, fixedPoint_east));
#    print(GUI_dog_data);
#    entryDogName$setText("");
#    labelDogFile$setText("");
#    labelSheep1File$setText("");
#    labelSheep2File$setText("");
#    dogFile <<- "";
#    sheep1File <<- "";
#    sheep2File <<- "";
#    
#    dogL <- "Chiens choisis: ";
#    for (d in GUI_dog_data[seq(1,length(GUI_dog_data),7)])
#      dogL <- paste(dogL, d, sep = "  ");
#    dogListLabel$setText(dogL);
#    fastFolder <<- base_folder;
#  }
#}
#remove_dogs <- function(button, user.data)
#{
#  GUI_dog_data <<- c();
#  entryDogName$setText("");
#  labelDogFile$setText("");
#  labelSheep1File$setText("");
#  labelSheep2File$setText("");
#  dogFile <<- "";
#  sheep1File <<- "";
#  sheep2File <<- "";
#  dogListLabel$setText("Aucun chien choisi");
#  fastFolder <<- base_folder;
#}
# obtain the number of histogram classes in the user interface
get_histClasses <- function()
{
    hC <- histClassesEntry$getText()
  histClasses <- as.numeric(hC);
  if (is.na(histClasses))
  {
    errMsg <- paste("Nombre classes histogrammes invalid: \""
                    ,hC,"\", valeur utilisee: ", HISTOGRAM_CLASSES,"\n", sep="");
    histClassesEntry$setText(as.character(HISTOGRAM_CLASSES));
    stop (errMsg);
  } else {
    return (histClasses);
  }
}
# obtain the value of the filter distance in the user interface
get_filterDist <- function()
{    
  dF <- filterDistEntry$getText();
  distFilter <- as.numeric(dF);
  if (is.na(distFilter))
  {
    errMsg <- paste(errMsg, "Distance de filtre invalide: \""
                     ,dF,"\", valeur utilisee: ", FILTER_DIST,"\n", sep="");
    filterDistEntry$setText(as.character(FILTER_DIST));
    stop (errMsg);
  } else {
    return (distFilter)
  }
}
# find the output folder, and validate it
get_baseFolder <- function()
{
  bf <- paste(labelOutputFolder$getText(), .Platform$file.sep, sep="");
  if (bf == "")
    stop ("Pas de dossier de sortie choisi!")
  else if (!file_test("-d", bf))
    stop ("Le dossier de sortie n'existe pas!");
  return (bf);
}

# obtain the names of the test animals if arg=TRUE, of sheep otherwise
get_names <- function(is_test_animal)
{
  names <- c();
  entries <- if (is_test_animal) animal_nameEntries else sheep_nameEntries;
  for ( e in entries)
  {
    name <- e$getText();
    if (name == "")
      stop ("Unspecified animal name");
    names <- append(names, name);
  }
  return (names);
}

collect_current_data <- function(button)
{
  a_names <- get_names (TRUE);
  s_names <- get_names (FALSE);
  entry <- "(";
  la <- length(a_names);
  for (i in 1:(la-1))
    entry <- paste(entry, a_names[i], ", " , sep="");
  entry <- paste(entry, a_names[la], " <-> ", sep="");
  ls <- length(s_names);
  for (i in 1:(ls-1))
    entry <- paste(entry, s_names[i], ", " , sep="");
  entry <- paste(entry, s_names[ls], ")", sep="");

  text <- paste (data_setLabel$getText(), entry, sep="\n");
  data_setLabel$setText(text);
}
# start the analysis, using the entered data
#startStuff <- function(button, user.data)
#{
#  
#  if (length(GUI_dog_data) == 0)
#    errDialog("Aucun chien choisi!")
#  else
#  {
#    dogsText <- dogListLabel$getText();
#    dogListLabel$setText("Calculs en cours...");
#    withCallingHandlers(
#      {
#        HISTOGRAM_CLASSES <<- get_histClasses();
#        FILTER_DIST <<- get_filterDist();
#        LANG <<- comboLangs$active + 1;
#        base_folder <- get_baseFolder();
#        export_1dog_graph <- checkBox_exp_1dog_graphs$active;
#        export_alldogs_graph <- checkBox_exp_comp_graphs$active;
#
#        handle_dogs(base_folder, export_1dog_graph, export_alldogs_graph, GUI_dog_data);
#      },
#      error = function(e)
#      {
#        msg <- paste("Une erreur s'est produite:", e);
#        errDialog(msg);
#        cs <- sys.calls();
#        msg <- paste(msg, "Details:", cs, sep="\n\n" );
#        writeLines(msg);
#      }
#    );
#    dogListLabel$setText(dogsText);
#  }
#}

# create the window, with all the widgets
#create_GUI_old <- function()
#{  
#  window$title <- "Test jeune chien";
#  frame <- gtkFrame();
#  window$add(frame);
#  vbox <- gtkVBox(F, 10);
#  frame$add(vbox);
#  
#  # name of the dog
#  hboxDogName <- gtkHBox(F, 10);
#  vbox$packStart(hboxDogName, F, F, 3);
#  labelName <- gtkLabel("Nom du chien:");
#  hboxDogName$packStart(labelName, F, F, 3);
#  entryDogName$setWidthChars(30);
#  hboxDogName$packEnd(entryDogName, F, F, 3);
#  
#  # horizontal box for dog file
#  hboxDog <- gtkHBox(F, 10);
#  vbox$packStart(hboxDog, F, F, 3);
#  label1 <- gtkLabel("Fichier chien:");
#  hboxDog$packStart(label1, F, F, 3);
#  hboxDog$packStart(labelDogFile);
#  buttonDogFile <- gtkButton("Choisir..");
#  gSignalConnect(buttonDogFile, "pressed", choose_file, data="dog");
#  label1$setMnemonicWidget(buttonDogFile);
#  hboxDog$packEnd(buttonDogFile, F, F, 3);
#  
#  # sheep 1
#  hboxS1 <- gtkHBox(F, 10);
#  vbox$packStart(hboxS1, F, F, 3);
#  label2 <- gtkLabel("Fichier mouton 1:");
#  hboxS1$packStart(label2, F, F, 3);
#  hboxS1$packStart(labelSheep1File);
#  buttonSheep1File <- gtkButton("Choisir..");
#  gSignalConnect(buttonSheep1File, "pressed", choose_file, data="s1");
#  label2$setMnemonicWidget(buttonSheep1File);
#  hboxS1$packEnd(buttonSheep1File, F, F, 3);
#  
#  # sheep 2
#  hboxS2 <- gtkHBox(F, 10);
#  vbox$packStart(hboxS2);
#  label3 <- gtkLabel("Fichier mouton 2:");
#  hboxS2$packStart(label3, F, F, 3);
#  hboxS2$packStart(labelSheep2File);
#  buttonSheep2File <- gtkButton("Choisir..");
#  gSignalConnect(buttonSheep2File, "pressed", choose_file, data="s2");
#  label3$setMnemonicWidget(buttonSheep2File);
#  hboxS2$packEnd(buttonSheep2File, F, F, 3);
#  
#  # fixed position point
#  hboxFP <- gtkHBox(F, 10);
#  vbox$packStart(hboxFP);
#  labelFP <- gtkLabel("Point fixe:"); 
#  entryFPN$setText("0.0");
#  entryFPE$setText("0.0");
#  hboxFP$packStart(labelFP, F, F, 3);
#  # north first, east second (inverted in code, as it starts from end)
#  hboxFP$packEnd(entryFPE, F, F, 0);
#  hboxFP$packEnd(gtkLabel("E"), F, F, 0);
#  hboxFP$packEnd(entryFPN, F, F, 0);
#  hboxFP$packEnd(gtkLabel("N"), F, F, 0);
#  
#  # button to add the dog
#  hboxButtons <- gtkHBox(F, 10);
#  vbox$packStart(hboxButtons, F, F, 3);
#  buttonAddDog <- gtkButton("Ajouter ce chien");
#  hboxButtons$packEnd(buttonAddDog, F, F, 3);
#  gSignalConnect(buttonAddDog, "pressed", add_dog);
#  buttonRemoveAllDogs <- gtkButton("Enlever les chiens");
#  hboxButtons$packEnd(buttonRemoveAllDogs, F, F, 3);
#  gSignalConnect(buttonRemoveAllDogs, "pressed", remove_dogs);
#  
#  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0);
#  
#  #list the loaded dogs
#  vbox$add(dogListLabel);
#  
#  
#  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0);
#
#  # add the other options
#  create_general_options (vbox);
#}

# user.data = c(num_test_animals, num_sheep)
startStuff <- function (button, user.data)
{
  #text <- paste("Coming soon ;)\n\ndata=");
  #d <- gtkMessageDialog(text=text, parent=window, flags=0, type="info", buttons="ok")
  #d$run();
  #d$destroy();
  numAnimals <- user.data[1];
  numSheep <- user.data[2];
  withCallingHandlers
  (
    {
      HISTOGRAM_CLASSES <<- get_histClasses();
      FILTER_DIST <<- get_filterDist();
      LANG <<- comboLangs$active + 1;
      base_folder <- get_baseFolder();
      export_1animal_graph <- checkBox_exp_1animal_graphs$active;
      export_allanimals_graph <- checkBox_exp_allanimals_graphs$active;

      start_analysis (numAnimals,numSheep,base_folder,export_1animal_graph,export_allanimals_graph,animals_data_set);
} );#, error = function(e){});
  #  },
  #  error = function(e)
  #  {
  #    msg <- paste("Une erreur s'est produite:", e);
  #    errDialog(msg);
  #    cs <- sys.calls();
  #    msg <- paste(msg, "Details:", cs, sep="\n\n" );
  #    writeLines(msg);
  #  }
  #);
}

# this function adds stuff to the end of the given box using packStart
create_general_options <- function (box)
{  
  hboxOutputFolder <- gtkHBox(F, 10);
  box$packStart(hboxOutputFolder, F, F, 3);
  hboxOutputFolder$packStart(gtkLabel("Output folder:"), F, F, 3);
  hboxOutputFolder$packStart(labelOutputFolder, F, F, 3);
  baseFolderButton <- gtkButton("Choose...");
  hboxOutputFolder$packEnd(baseFolderButton, F, F, 10);
  gSignalConnect(baseFolderButton, "pressed", choose_base_folder);

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
  labelHist <- gtkLabel("Number histogram classes:");
  hboxSettings$packStart(labelHist, F, F, 3);
  hboxSettings$packEnd(histClassesEntry, F, F, 3);
  histClassesEntry$setText(as.character(HISTOGRAM_CLASSES));
  histClassesEntry$setWidthChars(7);
  
  hboxSettings2 <- gtkHBox(F, 10);
  box$packStart(hboxSettings2, F, F, 3);
  labelFilter <- gtkLabel("Filter distance:");
  hboxSettings2$packStart(labelFilter, F, F, 3);
  hboxSettings2$packEnd(filterDistEntry, F, F, 3);
  filterDistEntry$setText(as.character(FILTER_DIST));
  filterDistEntry$setWidthChars(7);
 }


# callback for the ok button in animal number choice, b is useless but needed
# user.data = list ( container to remove, data for create_data_choice_box)
show_data_choice <- function(b, user.data)
{
  window$title <- "Choose the data to analyse";
  window$remove (user.data);


  # create the container with the number of animals
  user.data <- c( animalCombo$active + 1  ,   sheepCombo$active + 1)
  cont <- create_data_choice_box(user.data);

  window$add (cont);
  window$resize(1,1); # make minimum size
}


# num is the number of the given animal/sheep
# give isAnimal = TRUE tfor test animal; FALSE for sheep
make_choose_file_button <- function (isAnimal, num)
{
  b <- gtkButtonNewWithLabel("browse..");
  user.data <- c(isAnimal, num);
  filepath <- gSignalConnect (b, "pressed", choose_data_file, user.data);
  filepath <- gSignalConnect (b, "activate", choose_data_file, user.data);
  return (b);
}
create_data_choice_box <- function( user.data)
{
  # get data
  numberAnimals <- user.data[1];
  numberSheep <- user.data[2];
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

  # button to add this set of files to the data to analyse
  button_add <- gtkButtonNewWithLabel("Add this data set");
  hbox <- gtkHBox();
  hbox$packStart (button_add, expand=T, fill=F, padding=5);
  vboxAll$packStart (hbox, expand=T, fill=F, padding=5);
  gSignalConnect (button_add, "pressed", collect_current_data);

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
  user.data <- c(numberAnimals, numberSheep);
  gSignalConnect(buttonStart, "pressed", startStuff, user.data);


  contDataChoice$add (vboxAll);

  return (contDataChoice);
}

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
  gSignalConnect (buttonOk, "pressed", show_data_choice, data=contFormatChoice);
  gSignalConnect (buttonOk, "activate", show_data_choice, data=contFormatChoice);



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
# temp data to play with
GUI_dog_data <<- c("Helix_testplot",
                   "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Helix/",
                   "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Helix/JHP_Helix_22_23_5_2014.txt",
                   "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Helix/JHP_mouton1_Helix_22_23_5_2014.txt",
                   "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Helix/JHP_mouton2_Helix_22_23_5_2014.txt",
                   0,0,
                   
                   "Tirex_testplot" ,
                   "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Tirex/", 
                    "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Tirex/JHP_Tirex_10_11_4_2014.txt", 
                    "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Tirex/JHP_mouton2_Tirex_10_11_4_2014.txt",
                    "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Tirex/JHP_mouton1_Tirex_10_11_4_2014.txt",
                    0, 
                    0,
                    "Helix_testplot",
                   "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Helix/",
                   "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Helix/JHP_Helix_22_23_5_2014.txt",
                   "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Helix/JHP_mouton1_Helix_22_23_5_2014.txt",
                   "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Helix/JHP_mouton2_Helix_22_23_5_2014.txt",
                   0,0,
                   
                   "Tirex_testplot" ,
                   "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Tirex/", 
                    "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Tirex/JHP_Tirex_10_11_4_2014.txt", 
                    "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Tirex/JHP_mouton2_Tirex_10_11_4_2014.txt",
                    "/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS/Pfister_2014_Tirex_Agnella_Helix_Soldanella/Tirex/JHP_mouton1_Tirex_10_11_4_2014.txt",
                    0, 
                    0

                   );


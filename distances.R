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


######################################################################
# graphical interface objects, from which we read the values

# the main window
window <- gtkWindow();

#entryDogName <- gtkEntry();
#labelDogFile <- gtkLabel("");
#dogFile <- "";
#labelSheep1File <- gtkLabel("");
#sheep1File <- "";
#labelSheep2File <- gtkLabel("");
#sheep2File <- "";

# combo box for the possible output languages
comboLangs <- gtkComboBoxNewText();

# 2 text entries for the fixed point: North, East
entryFPN <- gtkEntry();
entryFPE <- gtkEntry();
fixedPoint_north <- 0;
fixedPoint_east <- 0;

# label for the folder where the comparison CSV file and graphs are output
labelBaseFolder <- gtkLabel(base_folder);

# text entry for the number of classes in the histograms
histClassesEntry <- gtkEntry();
# text entry for the filter distance
filterDistEntry <- gtkEntry();

# this label displays what's happening (eg. the chosen dogs, or "Calculs en cours..")
dogListLabel <- gtkLabel("Aucun chien choisi");

# when the user chooses a first file from a folder, we then propose that folder
fastFolder <- base_folder;

# collect all the dogs' data together
GUI_dog_data <- c();

choose_file <- function(button, data)
{
  d <- gtkFileChooserDialog(title="Choose a directory", parent=window, action="open",
                            "gtk-cancel", GtkResponseType["cancel"],
                            "gtk-open", GtkResponseType["accept"]);
  gtkFileChooserSetCurrentFolder(d, fastFolder);
  v <- d$run();
  if (v == GtkResponseType["accept"])
  {
    f <- d$getFilename();
    fastFolder <<- dirname(f);
    d$destroy();
    if (data == "dog")
    {
      gtkLabelSetText(labelDogFile, basename(f));
      dogFile <<- f;
    }
    if (data == "s1")
    {
      gtkLabelSetText(labelSheep1File, basename(f));
      sheep1File <<- f;
    }
    if (data == "s2")
    {
      gtkLabelSetText(labelSheep2File, basename(f));
      sheep2File <<- f;
    }
  }
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
    labelBaseFolder$setText(d$getFilename());
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
  bf <- paste(labelBaseFolder$getText(), .Platform$file.sep, sep="");
  if (bf == "")
    stop ("Pas de dossier de sortie choisi!")
  else if (!file_test("-d", bf))
    stop ("Le dossier de sortie n'existe pas!");
  return (bf);
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

create_general_options <- function (box)
{  
  hboxBF <- gtkHBox(F, 10);
  box$packStart(hboxBF, F, F, 3);
  labelBF <- gtkLabel("Dossier de sortie:");
  hboxBF$packStart(labelBF, F, F, 3);
  hboxBF$packStart(labelBaseFolder, F, F, 3);
  baseFolderButton <- gtkButton("Choisir...");
  hboxBF$packEnd(baseFolderButton, F, F, 3);
  gSignalConnect(baseFolderButton, "pressed", choose_base_folder);

  hboxExpGraphs <- gtkHBox ( T, 10 );
  box$packStart(hboxExpGraphs, F, F, 3);
  checkBox_exp_1dog_graphs <<- gtkCheckButton(label="Exporter graphes pour chaque chien", show=T);
  checkBox_exp_1dog_graphs$active <- TRUE;
  hboxExpGraphs$packStart(checkBox_exp_1dog_graphs, F, F, 3);
  checkBox_exp_comp_graphs <<- gtkCheckButton(label="Exporter graphes de comparaison", show=T);
  checkBox_exp_comp_graphs$active <- TRUE;
  hboxExpGraphs$packEnd(checkBox_exp_comp_graphs, F, F, 3);

  
  hboxLanguage <- gtkHBox(F, 10);
  box$packStart(hboxLanguage, F, F, 3);
  labelLang <- gtkLabel("Langue:");
  hboxLanguage$packStart(labelLang, T, F, 3);
  comboLangs$appendText("FR");
  comboLangs$appendText("DE");
  comboLangs$active <- 0;
  hboxLanguage$packStart(comboLangs, T, F, 3);
  
  hboxSettings <- gtkHBox(F, 10);
  box$packStart(hboxSettings, F, F, 3);
  labelHist <- gtkLabel("Nombre classes histogrammes:");
  hboxSettings$packStart(labelHist, F, F, 3);
  hboxSettings$packEnd(histClassesEntry, F, F, 3);
  histClassesEntry$setText(as.character(HISTOGRAM_CLASSES));
  
  hboxSettings2 <- gtkHBox(F, 10);
  box$packStart(hboxSettings2, F, F, 3);
  labelFilter <- gtkLabel("Distance filtre:");
  hboxSettings2$packStart(labelFilter, F, F, 3);
  hboxSettings2$packEnd(filterDistEntry, F, F, 3);
  filterDistEntry$setText(as.character(FILTER_DIST));
  
  hboxButtons2 <- gtkHBox(F, 10);
  box$packStart(hboxButtons2, F, F, 3);
  
  buttonStart <- gtkButton("Analyser");
  icon <- gtkImageNewFromIconName("gtk-apply", GtkIconSize["button"]);
  buttonStart$setImage(icon);
  hboxButtons2$packEnd(buttonStart, T, F, 3);
  gSignalConnect(buttonStart, "pressed", startStuff);

}

show_data_choice <- function(button, user.data)
{
  window$title <- "Choose the data to analyse";
  window$remove (user.data);
  window$add ( create_data_choice_box() );
  window$resize(1,1); # make minimum size
}
create_data_choice_box <- function()
{
  contDataChoice <- gtkFrame();
  l <- gtkLabel("Coming soon...");
  contDataChoice$add(l);

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

  sheepNumLabel <- gtkLabel ("How many sheep:");
  sheepCombo <- gtkComboBoxNewText();
  for (i in seq(1,4))
    sheepCombo$appendText(as.character(i));
  sheepCombo$setActive(1);
  hbox1 <- gtkHBox(spacing=10);
  hbox1$packStart(sheepNumLabel, expand=T, fill=F);
  hbox1$packStart(sheepCombo, expand=T, fill=F);
  layout$add(hbox1);

  offset_y <- offset_y + delta_y_group;

  animalNumLabel <- gtkLabel ("How many animals (lama/dog):");
  animalCombo <- gtkComboBoxNewText();
  for (i in seq(1,4))
    animalCombo$appendText(as.character(i));
  animalCombo$setActive(1);
  hbox1 <- gtkHBox(spacing=10);
  hbox1$packStart(animalNumLabel, expand=T, fill=F);
  hbox1$packStart(animalCombo, expand=T, fill=F);
  layout$add(hbox1);

  offset_y <- offset_y +delta_y_group;
  offset_x <- delta_x_comp;

  buttonOk <- gtkButton("OK");
  icon <- gtkImageNewFromIconName("gtk-ok", GtkIconSize["button"]);
  buttonOk$setImage(icon);
  gSignalConnect (buttonOk, "pressed", show_data_choice, data=contFormatChoice);
  hbox1 <- gtkHBox(spacing=10);
  hbox1$packStart(buttonOk, expand=T, fill=F);
  layout$packStart(hbox1, expand=F, fill=T);


  contFormatChoice$add (layout)

  return (contFormatChoice);
}

create_GUI <- function ()
{
# just show format window, it will call further to data choice window
  c <- create_format_choice_box();
  window$add(c);
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
#    dogL <- "Chiens choisis: ";
#    for (d in GUI_dog_data[seq(1,length(GUI_dog_data),7)])
#      dogL <- paste(dogL, d, sep = "  ");
#    dogListLabel$setText(dogL);
#  labelBaseFolder$setText("/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS");


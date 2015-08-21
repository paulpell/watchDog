



               #NO_GUI <- TRUE;



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


# the script does not take into account the moments where the dog is far from the sheep,
# for alignment, in_front and these things.
# This value (filter_dist) is the max distance. 10km will take everything into account, probably
#filter_dist <- 0.03; # 30 meters
filter_dist <- 0.05; # 0.050 kilometers, takes everything into account!

# this seems to be tricky: you NEED!! to SOURCE the file, not run it line by line
test <- sys.frame(1)$fileName;
if (is.null(test))
  test <- sys.frame(1)$ofile;

# include computations file
analysispath <- paste(dirname(test),"analysis4.R", sep = .Platform$file.sep);
source(analysispath);

#include translation file
translatepath <- paste(dirname(test),"translate.R", sep = .Platform$file.sep);
source(translatepath);

#include plot file
plotpath <- paste(dirname(test),"plot.R", sep = .Platform$file.sep);
source(plotpath);



#if ( exists("NO_GUI") & NO_GUI ) stop("GUI disabled");



# if this fails, you need to install RGtk2, available at:
# http://cran.r-project.org/web/packages/RGtk2/index.html
library("RGtk2");




# graphical interface objects, from which we read the values
window <- gtkWindow();
labelBaseFolder <- gtkLabel(base_folder);
entryDogName <- gtkEntry();
labelDogFile <- gtkLabel("");
dogFile <- "";
labelSheep1File <- gtkLabel("");
sheep1File <- "";
labelSheep2File <- gtkLabel("");
sheep2File <- "";
dogListLabel <- gtkLabel("Aucun chien choisi");
combo <- gtkComboBoxNewText();

entryFPN <- gtkEntry();
entryFPE <- gtkEntry();

fixedPoint_north <- 0;
fixedPoint_east <- 0;

histClassesEntry <- gtkEntry();
filterDistEntry <- gtkEntry();

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

filterFun <- function (f)
{
  return(file_test("-d",f$filename));
}

choose_base_folder <- function(button, data)
{
  d <- gtkFileChooserDialog(title="Choose a directory", parent=window, action="select-folder",
                            "gtk-cancel", GtkResponseType["cancel"],
                            "gtk-open", GtkResponseType["accept"]);
  filter <-  gtkFileFilter();
  gtkFileFilterAddCustom(filter, GtkFileFilterFlags['filename'], filterFun);
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

update_fixed_point <- function()
{
  fixedPoint_north <<- as.double(entryFPN$getText());
  fixedPoint_east <<- as.double(entryFPE$getText());
  
  res <- !is.na(fixedPoint_east) & !is.na(fixedPoint_north);
  
  return (res);
}

add_dog <- function(button, user.data)
{
  dogName <- entryDogName$getText();
  if (dogName == "" | dogFile == "" | sheep1File == "" | sheep2File == "" | !update_fixed_point())
  {
    errDialog("Pas assez d'informations fournies =)");
  } else {
    folder <- paste(dirname(dogFile), .Platform$file.sep, sep="");
    GUI_dog_data <<- c(GUI_dog_data, c(dogName, folder, dogFile, sheep1File, sheep2File, fixedPoint_north, fixedPoint_east));
    print(GUI_dog_data);
    entryDogName$setText("");
    labelDogFile$setText("");
    labelSheep1File$setText("");
    labelSheep2File$setText("");
    dogFile <<- "";
    sheep1File <<- "";
    sheep2File <<- "";
    
    dogL <- "Chiens choisis: ";
    for (d in GUI_dog_data[seq(1,length(GUI_dog_data),7)])
      dogL <- paste(dogL, d, sep = "  ");
    dogListLabel$setText(dogL);
    fastFolder <<- base_folder;
  }
}
remove_dogs <- function(button, user.data)
{
  GUI_dog_data <<- c();
  entryDogName$setText("");
  labelDogFile$setText("");
  labelSheep1File$setText("");
  labelSheep2File$setText("");
  dogFile <<- "";
  sheep1File <<- "";
  sheep2File <<- "";
  dogListLabel$setText("Aucun chien choisi");
  fastFolder <<- base_folder;
}
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
                    0
                   );
    dogL <- "Chiens choisis: ";
    for (d in GUI_dog_data[seq(1,length(GUI_dog_data),7)])
      dogL <- paste(dogL, d, sep = "  ");
    dogListLabel$setText(dogL);
  labelBaseFolder$setText("/media/data/toSave/paul/AGRIDEA/Base_de_donnée_GPS");

# start the analysis, using the entered data
startStuff <- function(button, user.data)
{
  errMsg <- "";
  hC <- histClassesEntry$getText()
  histClasses <- as.numeric(hC);
  if (is.na(histClasses))
  {
    errMsg <- paste("Nombre classes histogrammes invalid: \""
                    ,hC,"\", valeur utilisee: ", HISTOGRAM_CLASSES,"\n", sep="");
    histClassesEntry$setText(as.character(HISTOGRAM_CLASSES));
  } else {
    HISTOGRAM_CLASSES <<- histClasses;
  }
  
  dF <- filterDistEntry$getText();
  distFilter <- as.numeric(dF);
  if (is.na(distFilter))
  {
    errMsg <- paster(errMsg, "Distance de filtre invalide: \""
                     ,dF,"\", valeur utilisee: ", filter_dist,"\n", sep="");
    filterDistEntry$setText(as.character(filter_dist));
  } else {
    filter_dist <<- distFilter;
  }
  
  if (errMsg != "")
  {
    errDialog(errMsg);
    return();
  }
  
  bf <- paste(labelBaseFolder$getText(), .Platform$file.sep, sep="");
  if (length(GUI_dog_data) == 0)
    errDialog("Aucun chien choisi!")
  else if (bf == "")
    errDialog("Pas de dossier de sortie choisi!")
  else if (!file_test("-d", bf))
    errDialog("Le dossier de sortie n'existe pas!")
  else
  {
    dogsText <- dogListLabel$getText();
    dogListLabel$setText("Calculs en cours...");
    #tryCatch (
    withCallingHandlers(
      {
        LANG <<- combo$active + 1;
        handle_dogs(bf, GUI_dog_data);
      },
      error = function(e)
      {
        cs <- sys.calls();
        #cs2 <- cs[2:(length(cs)-2)];
        msg <- paste("Une erreur s'est produite:", e);
        msg <- paste(msg, "Details:", cs, sep="\n\n" );
        writeLines(msg);
        errDialog(msg);
      }
    );
    dogListLabel$setText(dogsText);
  }
}

# create the window, with all the widgets
create_GUI <- function()
{  
  window$title <- "Test jeune chien";
  frame <- gtkFrame();
  window$add(frame);
  vbox <- gtkVBox(F, 10);
  frame$add(vbox);
  
  # name of the dog
  hboxDogName <- gtkHBox(F, 10);
  vbox$packStart(hboxDogName, F, F, 3);
  labelName <- gtkLabel("Nom du chien:");
  hboxDogName$packStart(labelName, F, F, 3);
  entryDogName$setWidthChars(30);
  hboxDogName$packEnd(entryDogName, F, F, 3);
  
  # horizontal box for dog file
  hboxDog <- gtkHBox(F, 10);
  vbox$packStart(hboxDog, F, F, 3);
  label1 <- gtkLabel("Fichier chien:");
  hboxDog$packStart(label1, F, F, 3);
  hboxDog$packStart(labelDogFile);
  buttonDogFile <- gtkButton("Choisir..");
  gSignalConnect(buttonDogFile, "pressed", choose_file, data="dog");
  label1$setMnemonicWidget(buttonDogFile);
  hboxDog$packEnd(buttonDogFile, F, F, 3);
  
  # sheep 1
  hboxS1 <- gtkHBox(F, 10);
  vbox$packStart(hboxS1, F, F, 3);
  label2 <- gtkLabel("Fichier mouton 1:");
  hboxS1$packStart(label2, F, F, 3);
  hboxS1$packStart(labelSheep1File);
  buttonSheep1File <- gtkButton("Choisir..");
  gSignalConnect(buttonSheep1File, "pressed", choose_file, data="s1");
  label2$setMnemonicWidget(buttonSheep1File);
  hboxS1$packEnd(buttonSheep1File, F, F, 3);
  
  # sheep 2
  hboxS2 <- gtkHBox(F, 10);
  vbox$packStart(hboxS2);
  label3 <- gtkLabel("Fichier mouton 2:");
  hboxS2$packStart(label3, F, F, 3);
  hboxS2$packStart(labelSheep2File);
  buttonSheep2File <- gtkButton("Choisir..");
  gSignalConnect(buttonSheep2File, "pressed", choose_file, data="s2");
  label3$setMnemonicWidget(buttonSheep2File);
  hboxS2$packEnd(buttonSheep2File, F, F, 3);
  
  # fixed position point
  hboxFP <- gtkHBox(F, 10);
  vbox$packStart(hboxFP);
  labelFP <- gtkLabel("Point fixe:"); 
  entryFPN$setText("0.0");
  entryFPE$setText("0.0");
  hboxFP$packStart(labelFP, F, F, 3);
  # north first, east second (inverted in code, as it starts from end)
  hboxFP$packEnd(entryFPE, F, F, 0);
  hboxFP$packEnd(gtkLabel("E"), F, F, 0);
  hboxFP$packEnd(entryFPN, F, F, 0);
  hboxFP$packEnd(gtkLabel("N"), F, F, 0);
  
  # button to add the dog
  hboxButtons <- gtkHBox(F, 10);
  vbox$packStart(hboxButtons, F, F, 3);
  buttonAddDog <- gtkButton("Ajouter ce chien");
  hboxButtons$packEnd(buttonAddDog, F, F, 3);
  gSignalConnect(buttonAddDog, "pressed", add_dog);
  buttonRemoveAllDogs <- gtkButton("Enlever les chiens");
  hboxButtons$packEnd(buttonRemoveAllDogs, F, F, 3);
  gSignalConnect(buttonRemoveAllDogs, "pressed", remove_dogs);
  
  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0);
  
  #list the loaded dogs
  vbox$add(dogListLabel);
  
  
  vbox$packStart(gtkHSeparatorNew(), FALSE, FALSE, 0);
  
  hboxBF <- gtkHBox(F, 10);
  vbox$packStart(hboxBF, F, F, 3);
  labelBF <- gtkLabel("Dossier de sortie:");
  hboxBF$packStart(labelBF, F, F, 3);
  hboxBF$packStart(labelBaseFolder, F, F, 3);
  baseFolderButton <- gtkButton("Choisir...");
  hboxBF$packEnd(baseFolderButton, F, F, 3);
  gSignalConnect(baseFolderButton, "pressed", choose_base_folder);
  
  hboxLanguage <- gtkHBox(F, 10);
  vbox$packStart(hboxLanguage, F, F, 3);
  labelLang <- gtkLabel("Langue:");
  hboxLanguage$packStart(labelLang, T, F, 3);
  combo$appendText("FR");
  combo$appendText("DE");
  combo$active <- 0;
  hboxLanguage$packStart(combo, T, F, 3);
  
  hboxSettings <- gtkHBox(F, 10);
  vbox$packStart(hboxSettings, F, F, 3);
  labelHist <- gtkLabel("Nombre classes histogrammes:");
  hboxSettings$packStart(labelHist, F, F, 3);
  hboxSettings$packEnd(histClassesEntry, F, F, 3);
  histClassesEntry$setText(as.character(HISTOGRAM_CLASSES));
  
  hboxSettings2 <- gtkHBox(F, 10);
  vbox$packStart(hboxSettings2, F, F, 3);
  labelFilter <- gtkLabel("Distance filtre:");
  hboxSettings2$packStart(labelFilter, F, F, 3);
  hboxSettings2$packEnd(filterDistEntry, F, F, 3);
  filterDistEntry$setText(as.character(filter_dist));
  
  hboxButtons2 <- gtkHBox(F, 10);
  vbox$packStart(hboxButtons2, F, F, 3);
  
  buttonStart <- gtkButton("Analyser");
  icon <- gtkImageNewFromIconName("gtk-apply", GtkIconSize["button"]);
  buttonStart$setImage(icon);
  hboxButtons2$packEnd(buttonStart, T, F, 3);
  gSignalConnect(buttonStart, "pressed", startStuff);
}


create_GUI();


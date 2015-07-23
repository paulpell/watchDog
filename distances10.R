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

HISTOGRAM_CLASSES <- 100;

# the script does not take into account the moments where the dog is far from the sheep,
# for alignment, in_front and these things.
# This value (filter_dist) is the max distance. 10km will take everything into account, probably
#filter_dist <- 0.03; # 30 meters
filter_dist <- 0.05; # 0.050 kilometers, takes everything into account!

# this seems to be tricky: you NEED!! to SOURCE the file, not run it line by line
test <- sys.frame(1)$fileName;
if (is.null(test))
  test <- sys.frame(1)$ofile;
fullpath <- paste(dirname(test),"analysis4.R", sep = .Platform$file.sep);
source(fullpath);

# if this fails, you need to install RGtk2, available at:
# http://cran.r-project.org/web/packages/RGtk2/index.html
library("RGtk2");

# this library
library("Cairo");


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
    tryCatch (
      {
        LANG <<- combo$active + 1;
        handle_dogs(bf, GUI_dog_data);
      },
      error = function(e)
      {
        msg <- paste("Une erreur s'est produite:", e);
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

# translations french/german
# Arguments are provided using ^1, ^2, etc.
# For example: "Distances ^1 aux deux moutons", where ^1 will be replaced whatever is given

LANGUAGES <- new.env();
LANGUAGES$dist_dog <- c("Distance parcourue par le chien: ^1km",
                        "Strecke vom Hund gelaufen: ^1km ");
LANGUAGES$dist_sheep <- c("Distance parcourue par le mouton ^1: ^2km",
                          "Strecke vom Schaf ^1 gelaufen: ^2km");
LANGUAGES$dist_sheep_rel <- c("Distance parcourue relative du chien pour le mouton ^1: ^2",
                              "Relative Strecke vom Hund im Bezug zum Schaf ^1 gelaufen: ^2");
LANGUAGES$mean_dist_closest <- c("Distance moyenne entre le chien et le mouton le plus proche: ^1km",
                                 "Durchschnittliche Distanz zwischen den Hund und das naechste Schaf: ^1km");
LANGUAGES$median_dist_closest <- c("Distance mediane entre le chien et le mouton le plus proche: ^1km",
                                   "Median der distanzen zwischen den Hund und das naechste Schaf: ^1km");
LANGUAGES$mean_dist_middle <- c("Distance moyenne entre le chien et le milieu des moutons: ^1km",
                                "Durchschnittliche Distanz zwischen den Hund und die Mitte der Schafe: ^1km");
LANGUAGES$median_dist_middle <- c("Distance medianne entre le chien et le milieu des moutons: ^1km",
                                "Median der Distanzen zwischen den Hund und die Mitte der Schafe: ^1km");
LANGUAGES$dog_in_front1000 <- c("Chien est devant le mouton ^1, en moyenne (x1000): ^2",
                                  "Hund ist durschnittlich vor dem Schaf ^1 (x1000): ^2");
LANGUAGES$dog_aligned100 <- c("Chien est aligne avec le mouton ^1, en moyenne (x100): ^2",
                                "Hund lauft durschnittlich mit dem Schaf ^1 parallel (x100): ^2");
LANGUAGES$coord_align_pos <- c("Coordination mouton ^1 (quand ils sont alignes): ^2",
                               "Koordination Schaf ^1 (wenn sie parallel gehen): ^2");
LANGUAGES$coord_align_neg <- c("Coordination mouton ^1 (quand ils ne sont pas alignes): ^2",
                               "Koordination Schaf ^1 (wenn sie nicht parallel gehen): ^2");
LANGUAGES$num_barkings <- c("Nombre d'aboiements: ^1",
                               "Anzahl Bellen: ^1");

LANGUAGES$fixed_pt <- c("Point fixe: ^1", "Fixer Punkt: ^1");
LANGUAGES$mean_dist_fixed_pt <- c("Moyenne des distances au point fixe: ^1","Durchschnitt der Distanzen zum fixen Punkt: ^1");


LANGUAGES$dist_km <- c("Distance [km]", "Distanz [km]");
LANGUAGES$date <- c("Date", "Datum");
LANGUAGES$time <- c("Temps", "Zeit");
LANGUAGES$freq <- c("Frequence", "Frequenz");
LANGUAGES$rel_freq <- c("Frequence / #mesures", "Frequenz / #Werte");
LANGUAGES$red_median <- c("Mediane en rouge: ^1",
                          "Median in rot: ^1");
LANGUAGES$sheep_i <- c("Mouton ^1", "Schaf ^1");

LANGUAGES$graph_closest <- c("Distances ^1 au mouton le plus proche",
                                   "Distanzen ^1 zum naechsten Schaf");

LANGUAGES$graph_dist_fp <- c("Distances ^1 au point fixe ^2", "Distanzen ^1 zum fixen Punkt ^2");

LANGUAGES$hist_dist_fp <- c("Histogramme des distances ^1 au point fixe ^2",
                            "Histogram der Distanzen ^1 zum fixen Punkt ^2");

LANGUAGES$graph_closest_norm <- c("Distances ^1 au mouton le plus proche, normalisees sur 1 km",
                                   "Distanzen ^1 zum naechsten Schaf auf 1km normiert");

LANGUAGES$graph_hist_dist_closest <- c("Histogramme des distances ^1 au mouton le plus proche",
                                        "Distanzhistogramm ^1 zum naechsten Schaf");

LANGUAGES$graph_dist_mean <- c("Distances ^1 a la moyenne des moutons",
                               "Distanzen ^1 zur Mitte der Schaf");

LANGUAGES$graph_dist_mean_norm <- c("Distances ^1 a la moyenne des moutons, normalisees sur 1 km",
                                    "Distanzen ^1 zur Mitte der Schafe, auf 1km normiert");

LANGUAGES$graph_hist_dist_mean <- c("Histogramme distances ^1 a la moyenne des moutons",
                                    "Distanzhistogramm ^1 zum Mittelpunkt der Schafe");

LANGUAGES$graph_dist_both_sheep <- c("Distances ^1 aux deux moutons",
                                     "Distanzen ^1 zu beide Schafen");

LANGUAGES$graph_hist_coord_both_sheep <- c("Histogramme de coordination ^1 aux deux moutons",
                                     "Koordinationshistogramm ^1 zu beide Schafen");
LANGUAGES$graph_hist_coord_both_sheep_filename <- c("Histogramme_de_coordination_^1_aux_deux_moutons",
                                           "Koordinationshistogramm_^1_zu_beide_Schafen");

LANGUAGES$graph_align_pos_left_label <- c("alignement > 0, chien suit mouton",
                                          "Ausrichtung > 0, Hund folgt Schaf");
LANGUAGES$graph_align_pos_right_label <- c("alignement > 0, mouton suit chien",
                                           "Ausrichtung > 0, Schaf folgt Hund");
LANGUAGES$graph_align_neg_left_label <- c("alignement < 0, animaux s'eloignent",
                                          "Ausrichtung < 0, Tiere gehen auseinander");
LANGUAGES$graph_align_neg_right_label <- c("alignement < 0, animaux se rapprochent",
                                           "Ausrichtung < 0, Tiere gehen zueinander");


LANGUAGES$dists_all_dogs_closest <- c("Distances des chiens au mouton le plus proche",
                                      "Distanz der Hunde zum naechsten Schaf");

LANGUAGES$hist_dists_all_dogs_closest <- c("Histogramme des distances des chiens au mouton le plus proche",
                                           "Histogramm der Distanzen der Hunde zum naechsten Schaf");

LANGUAGES$dists_all_dogs_mean <- c("Distances des chiens a la moyenne des moutons",
                                      "Distanz der Hunde zur Mitte der Schafe");

LANGUAGES$hist_dists_all_dogs_mean <- c("Histogramme des distances des chiens a la moyenne des moutons",
                                        "Histogramm der Distanzen der Hunde zur Mitte der Schafe");

LANGUAGES$dists_all_dogs_both <- c("Distances des chiens aux deux moutons",
                                   "Distanz der Hunde zu beide Schafe");


LANGUAGES$graph_hist_all_dogs_coord_both_sheep <- c("Histogramme de coordination chiens aux deux moutons",
                                           "Koordinationshistogramm Hunde zu beide Schafen");

LANGUAGES$no_available_data <- c("Pas de donnees disponibles pour ^1", "Keine verfuegbare Daten für ^1");

LANGUAGES$hist_colors_label <- c("m1: rouge et bleu, m2: jaune et vert", "S1: rot und blau, S2: gelb und gruen");

LANGUAGES$boxplot_all <- c("Boxplot tous les chiens", "Boxplot alle Hunde");

LANGUAGES$boxplot_all_no_outlier <- c("Boxplot tous les chiens sans extremes", "Boxplot alle Hunder ohne extreme");

LANGUAGES$name_csv <- c("Resultats_tous_les_chiens", "Ergebnisse_alle_Hunde");
LANGUAGES$res_n <- c("Nom_du_chien","Name_Hund");
LANGUAGES$res_d1 <- c("Distance_parcourue_par_le_chien_Km","Distanz_vom_Hund_gelaufen_km");
LANGUAGES$res_d2 <- c("Distance_parcourue_par_le_mouton1_Km","Distanz_vom_Schaf1_gelaufen_km");
LANGUAGES$res_d3 <- c("Distance_parcourue_par_le_mouton2_Km","Distanz_vom_Schaf1_gelaufen_km");
LANGUAGES$res_d4 <- c("Distance_relative_chien_mouton1","Relative_Distance_Hund_Schaf1");
LANGUAGES$res_d5 <- c("Distance_relative_chien_mouton2","Relative_Distance_Hund_Schaf2");
LANGUAGES$res_d6 <- c("Distance_moyenne_chien_mouton_moyen_m","Durchschnitt_Distanz_Hund_Mitte_Schafe_m");
LANGUAGES$res_d7 <- c("Distance_moyenne_chien_mouton_le_plus_proche_m","Durchschnitt_Distanz_Hund_naechste_Schaf_m");
LANGUAGES$res_d8 <- c("Distance_mediane_chien_mouton_moyen_m","Median_Distanz_Hund_Mitte_Schafe_m");
LANGUAGES$res_d9 <- c("Distance_mediane_chien_mouton_le_plus_proche_m","Media_Distanz_Hund_naechste_Schaf_m");
LANGUAGES$res_c1 <- c("Chien_devant_mouton1_moyenne_x1000","Hund_vor_Schaf1_Durchschnitt_x1000");
LANGUAGES$res_c2 <- c("Chien_devant_mouton2_moyenne_x1000","Hund_vor_Schaf2_Durchschnitt_x1000");
LANGUAGES$res_c3 <- c("Chien_aligne_mouton1_x100","Hund_Schaf1_parallel_x100");
LANGUAGES$res_c4 <- c("Chien_aligne_mouton2_x100","Hund_Schaf2_parallel_x100");
LANGUAGES$res_c5 <- c("In_front_and_alignment_mouton1_x100","Hund_vor_Schaf1_und_parallel_x100");
LANGUAGES$res_c6 <- c("In_front_and_alignment_mouton2_x100","Hund_vor_Schaf2_und_parallel_x100");
LANGUAGES$res_c7 <- c("Coordination_alignment_positif_mouton1_x100","Koordination_positive_Ausrichtung_Schaf1_x100");
LANGUAGES$res_c8 <- c("Coordination_alignment_negatif_mouton1_x100","Koordination_negative_Ausrichtung_Schaf1_x100");
LANGUAGES$res_c9 <- c("Coordination_alignment_positif_mouton2_x100","Koordination_positive_Ausrichtung_Schaf2_x100");
LANGUAGES$res_c10 <- c("Coordination_alignment_negatif_mouton2_x100","Koordination_negative_Ausrichtung_Schaf2_x100");
LANGUAGES$res_t1 <- c("debut_test_date","Anfang_Test_Datum");
LANGUAGES$res_t2 <- c("fin_test_date","Schluss_Test_Datum");
LANGUAGES$res_t3 <- c("duree_test","Dauer_Test");
LANGUAGES$res_mc1 <- c("Moyenne_coord_align_pos_droite_m1_x100", "Durchschnitt_koord_align_pos_rechts_m1_x100");
LANGUAGES$res_mc2 <- c("Moyenne_coord_align_pos_gauche_m1_x100", "Durchschnitt_koord_align_pos_links_m1_x100");
LANGUAGES$res_mc3 <- c("Moyenne_coord_align_neg_droite_m1_x100", "Durchschnitt_koord_align_neg_rechts_m1_x100");
LANGUAGES$res_mc4 <- c("Moyenne_coord_align_neg_gauche_m1_x100", "Durchschnitt_koord_align_neg_links_m1_x100");
LANGUAGES$res_mc5 <- c("Moyenne_coord_align_pos_droite_m2_x100", "Durchschnitt_koord_align_pos_rechts_m2_x100");
LANGUAGES$res_mc6 <- c("Moyenne_coord_align_pos_gauche_m2_x100", "Durchschnitt_koord_align_pos_links_m2_x100");
LANGUAGES$res_mc7 <- c("Moyenne_coord_align_neg_droite_m2_x100", "Durchschnitt_koord_align_neg_rechts_m2_x100");
LANGUAGES$res_mc8 <- c("Moyenne_coord_align_neg_gauche_m2_x100", "Durchschnitt_koord_align_neg_links_m2_x100");
LANGUAGES$res_fp1 <- c("Point_fixe", "Fixer_Punkt");
LANGUAGES$res_fp2 <- c("Moyenne_distances_point_fixe", "Durchschnitt_Distanzen_fixen_Punkt");




# obtain the translation of a given text, using the LANG variable
get_translation <- function(text, args = c())
{
  trans <- LANGUAGES[[text]][LANG];
  if (is.null(trans))
    stop("Bad translation");
  
  if(is.null(args))
    return (trans);
  
  for (i in 1:length(args))
  {
    pattern <- paste("\\^", i, sep="");
    trans <- sub(pattern, args[i], trans);
  }
  return (trans);
}

get_trans_filename <- function(text, args=c())
{
  trans <- get_translation(text, args);
  fname <- gsub("\\s+", "_", trans);
  return(fname);
}

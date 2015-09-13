
# translations french/german
# Arguments are provided using ^1, ^2, etc.
# For example: "Distances ^1 aux deux moutons", where ^1 will be replaced whatever is given

LANGUAGES <- new.env();
LANGUAGES$dist_dog <- c("Distance parcourue par le chien: ^1km",
                        "Strecke vom Hund gelaufen: ^1km ");
LANGUAGES$dist_sheep_i <- c("Distance parcourue par le mouton ^1: ^2km",
                          "Strecke vom Schaf ^1 gelaufen: ^2km");
LANGUAGES$dist_sheep <- c("Distance parcourue par le mouton: ^1km",
                          "Strecke vom Schaf gelaufen: ^1km");
LANGUAGES$dist_sheep_rel_i <- c("Distance parcourue relative du chien pour le mouton ^1: ^2",
                              "Relative Strecke vom Hund im Bezug zum Schaf gelaufen ^1: ^2");
LANGUAGES$dist_sheep_rel <- c("Distance parcourue relative du chien pour le mouton: ^1",
                              "Relative Strecke vom Hund im Bezug zum Schaf gelaufen: ^1");
LANGUAGES$mean_dist_closest <- c("Distance moyenne entre le chien et le mouton le plus proche: ^1km",
                                 "Durchschnittliche Distanz zwischen den Hund und das naechste Schaf: ^1km");
LANGUAGES$median_dist_closest <- c("Distance mediane entre le chien et le mouton le plus proche: ^1km",
                                   "Median der distanzen zwischen den Hund und das naechste Schaf: ^1km");
LANGUAGES$mean_dist_middle <- c("Distance moyenne entre le chien et le milieu des moutons: ^1km",
                                "Durchschnittliche Distanz zwischen den Hund und die Mitte der Schafe: ^1km");
LANGUAGES$median_dist_middle <- c("Distance medianne entre le chien et le milieu des moutons: ^1km",
                                "Median der Distanzen zwischen den Hund und die Mitte der Schafe: ^1km");
LANGUAGES$dog_in_front1000 <- c("Chien est devant le mouton, en moyenne (x1000): ^1",
                                  "Hund ist durschnittlich vor dem Schaf (x1000): ^1");
LANGUAGES$dog_in_front1000_i <- c("Chien est devant le mouton ^1, en moyenne (x1000): ^2",
                                  "Hund ist durschnittlich vor dem Schaf ^1 (x1000): ^2");
LANGUAGES$dog_aligned100_i <- c("Chien est aligne avec le mouton ^1, en moyenne (x100): ^2",
                                "Hund lauft durschnittlich mit dem Schaf ^1 parallel (x100): ^2");
LANGUAGES$dog_aligned100 <- c("Chien est aligne avec le mouton, en moyenne (x100): ^1",
                                "Hund lauft durschnittlich mit dem Schaf parallel (x100): ^1");
LANGUAGES$coord_align_pos_i <- c("Coordination mouton ^1 (quand ils sont alignes): ^2",
                               "Koordination Schaf ^1 (wenn sie parallel gehen): ^2");
LANGUAGES$coord_align_pos <- c("Coordination mouton (quand ils sont alignes): ^1",
                               "Koordination Schaf (wenn sie parallel gehen): ^1");
LANGUAGES$coord_align_neg_i <- c("Coordination mouton ^1 (quand ils ne sont pas alignes): ^2",
                               "Koordination Schaf ^1 (wenn sie nicht parallel gehen): ^2");
LANGUAGES$coord_align_neg <- c("Coordination mouton (quand ils ne sont pas alignes): ^1",
                               "Koordination Schaf (wenn sie nicht parallel gehen): ^1");
LANGUAGES$num_barkings <- c("Nombre d'aboiements: ^1",
                               "Anzahl Bellen: ^1");

LANGUAGES$fixed_pt <- c("Point fixe: ^1", "Fixer Punkt: ^1");
LANGUAGES$mean_dist_fixed_pt <- c("Moyenne des distances au point fixe: ^1","Durchschnitt der Distanzen zum fixen Punkt: ^1");

LANGUAGES$mean_coord_palign_infront <- c("Moyenne coord align > 0, devant (x100): ^1", "Durchschnitt koord align > 0, vorne; (x100): ^1");
LANGUAGES$mean_coord_palign_inback  <- c("Moyenne coord align > 0, derriere (x100): ^1", "Durchschnitt koord align > 0, hinten; (x100): ^1");
LANGUAGES$mean_coord_nalign_inback  <- c("Moyenne coord align < 0, derriere (x100): ^1", "Durchschnitt koord align < 0, hinten; (x100): ^1");
LANGUAGES$mean_coord_nalign_infront <- c("Moyenne coord align < 0, devant (x100): ^1", "Durchschnitt koord align < 0, vorne; (x100): ^1");
LANGUAGES$test_date_start <- c("Date debut du test: ^1","Datum Testanfang: ^1");
LANGUAGES$test_date_end <- c("Date fin du test: ^1","Datum Testende: ^1");
LANGUAGES$test_duration <- c("Duree test: ^1","Dauer Test: ^1");

################
## Graph translations

# labels, axis
LANGUAGES$dist_km <- c("Distance [km]", "Distanz [km]");
LANGUAGES$date <- c("Date", "Datum");
LANGUAGES$time <- c("Temps", "Zeit");
LANGUAGES$freq <- c("Frequence", "Frequenz");
LANGUAGES$rel_freq <- c("Frequence / #mesures", "Frequenz / #Werte");
LANGUAGES$red_median <- c("Mediane en rouge: ^1",
                          "Median in rot: ^1");
LANGUAGES$sheep_i <- c("Mouton ^1", "Schaf ^1");

LANGUAGES$graph_align_pos_left_label <- c("alignement > 0, chien suit mouton",
                                          "Ausrichtung > 0, Hund folgt Schaf");
LANGUAGES$graph_align_pos_right_label <- c("alignement > 0, mouton suit chien",
                                           "Ausrichtung > 0, Schaf folgt Hund");
LANGUAGES$graph_align_neg_left_label <- c("alignement < 0, animaux s'eloignent",
                                          "Ausrichtung < 0, Tiere gehen auseinander");
LANGUAGES$graph_align_neg_right_label <- c("alignement < 0, animaux se rapprochent",
                                           "Ausrichtung < 0, Tiere gehen zueinander");

LANGUAGES$no_available_data <- c("Pas de donnees disponibles pour ^1", "Keine verfuegbare Daten für ^1");

LANGUAGES$hist_colors_label <- c("m1: rouge et bleu, m2: jaune et vert", "S1: rot und blau, S2: gelb und gruen");

# graph names, also used to write to console
LANGUAGES$graph_closest <- c("Distances ^1 au mouton le plus proche",
                                   "Distanzen ^1 zum naechsten Schaf");

LANGUAGES$graph_dist_fp <- c("Distances ^1 au point fixe ^2", "Distanzen ^1 zum fixen Punkt ^2");

LANGUAGES$hist_dist_fp <- c("Histogramme des distances ^1 au point fixe ^2",
                            "Histogram der Distanzen ^1 zum fixen Punkt ^2");

LANGUAGES$graph_closest_norm <- c("Distances ^1 au mouton le plus proche, normalisees sur 1 km",
                                   "Distanzen ^1 zum naechsten Schaf auf 1km normiert");

LANGUAGES$hist_closest <- c("Histogramme des distances ^1 au mouton le plus proche",
                                        "Distanzhistogramm ^1 zum naechsten Schaf");

LANGUAGES$graph_dist_mean <- c("Distances ^1 a la moyenne des moutons",
                               "Distanzen ^1 zur Mitte der Schaf");

LANGUAGES$graph_dist_mean_norm <- c("Distances ^1 a la moyenne des moutons, normalisees sur 1 km",
                                    "Distanzen ^1 zur Mitte der Schafe, auf 1km normiert");

LANGUAGES$graph_hist_dist_mean <- c("Histogramme distances ^1 a la moyenne des moutons",
                                    "Distanzhistogramm ^1 zum Mittelpunkt der Schafe");

LANGUAGES$graph_dist_both_sheep <- c("Distances ^1 aux moutons",
                                     "Distanzen ^1 zu den Schafen");

LANGUAGES$graph_hist_coord_both_sheep <- c("Histogramme de coordination ^1 aux deux moutons",
                                     "Koordinationshistogramm ^1 zu beide Schafen");


# comparison graphs
LANGUAGES$boxplot_all <- c("Boxplot tous les chiens", "Boxplot alle Hunde");

LANGUAGES$boxplot_all_no_outlier <- c("Boxplot tous les chiens sans extremes", "Boxplot alle Hunder ohne Extreme");

LANGUAGES$dists_all_dogs_closest <- c("Distances des chiens au mouton le plus proche",
                                      "Distanz der Hunde zum naechsten Schaf");

LANGUAGES$dists_all_dogs_mean <- c("Distances des chiens a la moyenne des moutons",
                                      "Distanz der Hunde zur Mitte der Schafe");

LANGUAGES$dists_all_dogs_both <- c("Distances des chiens aux deux moutons",
                                   "Distanz der Hunde zu beide Schafe");

LANGUAGES$hist_dists_all_dogs_mean <- c("Histogramme des distances des chiens a la moyenne des moutons",
                                        "Histogramm der Distanzen der Hunde zur Mitte der Schafe");

LANGUAGES$hist_dists_all_dogs_closest <- c("Histogramme des distances des chiens au mouton le plus proche",
                                           "Histogramm der Distanzen der Hunde zum naechsten Schaf");


LANGUAGES$hist_all_dogs_coord_both_sheep <- c("Histogramme de coordination chiens aux deux moutons",
                                           "Koordinationshistogramm Hunde zu beide Schafen");

# csv headers

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
    stop(paste("This translation does not exist: \"",text,"\"", sep=""));
  
  if(is.null(args))
    return (trans);
  
  for (i in 1:length(args))
  {
    pattern <- paste("\\^", i, sep=""); # the pattern will be ^1, ^2, etc.
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

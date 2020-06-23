####################################
### 1 - Importation des packages ###
####################################
#install.packages('pacman')
pacman::p_load('pacman')

pacman::p_load('tidyr')
pacman::p_load('dplyr')

#ggplot pour les représentations graphiques
pacman::p_load('ggplot2')
pacman::p_load('plotly')

# MASS pour la fonction stepAIC
pacman::p_load('MASS')

#
pacman::p_load('questionr')

# pour fonction sample.split
pacman::p_load('caTools')

pacman::p_load('data.table')

pacman::p_load('lubridate')

pacman::p_load('eeptools')


############################################
### 2 - Import des données & Préparation ###
############################################


df_sondage <- read.csv("./data/Formulaire sans titre.csv")

#Suppression 1er colonne
df_sondage <- df_sondage[,-1]

#Renommage colonne
df_sondage <- df_sondage %>% rename(Retour_Travail = Retourner.au.travail.ou.Ã..l.Ã.cole.est.il.une.source.de.stress..,
                                    Categorie_Socio = Dans.quelle.catÃ.gorie.socio.professionnelle.vous.situez.vous..,
                                    Etude = Quel.est.votre.niveau.dâ..Ã.tude..,
                                    Sexe = Quel.est.votre.sexe..,
                                    Age = Dans.quelle.tranche.d.age.vous.situez.vous.,
                                    Statut_Marital = Statut.marital,
                                    Nb_Perso_Foyer = Combien.de.personne.compose.votre.foyer..,
                                    Milieu_Habitat = OÃ¹.habitez.vous..,
                                    Type_Habitat = Vous.vivez....,
                                    Transport_Pro = Quel.est.votre.mode.de.dÃ.placement.professionnel..,
                                    Changement_Transport = Suite.au.confinement..pensez.vous.changer.votre.mode.de.dÃ.placement.,
                                    Teletravail = ApprÃ.ciez.vous.le.tÃ.lÃ.travail..,
                                    Productivite = Pensez.vous.Ãªtre.plus.productif.en.tÃ.lÃ.travail..,
                                    Commentaire_Teletravail = Pourquoi..,
                                    Question_Test = Veuillez.rÃ.pondre.Â..Un.peu.Â..Ã..cette.question.,
                                    Geste_Barriere = Selon.vous.combien.de.temps.les.mesures.de.gestes.barriÃ.res.devraient.Ãªtre.appliquÃ.es..,
                                    Ameliorer_Teletravail = Qu.est.ce.qui..selon.vous..pourrait.amÃ.liorer.le.tÃ.lÃ.travail..,
                                    Mesure_Teletravail = Selon.vous..aprÃ.s.cet.Ã.pisode.de.crise..quelle.s..mesure.s..devraient.Ãªtre.prise.s..lors.de.votre.retour.sur.votre.lieu.de.travail...
                                    )


#Filtres sur la question test
df_sondage <- df_sondage[(df_sondage$Question_Test == "Un peu"),]

#Suppression question
df_sondage <- df_sondage[,-13]


############################################
# Modification Categorie_Socio
df_sondage$Categorie_Socio <- as.character(df_sondage$Categorie_Socio)


#df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Agriculteur'] <- '1'
#df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Artisan'] <- '2'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'CommerÃ§ant'] <- 'Commerçant'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Chef dâ€™entreprise'] <- 'Chef d\'entreprise'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Profession libÃ©rale'] <- 'Profession libérale'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Cadre ou profession intellectuelle supÃ©rieure'] <- 'Cadre ou profession intellectuelle supérieure'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Profession intermÃ©diaire'] <- 'Profession intermédiaire'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'EmployÃ©'] <- 'Employé'
#df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Ouvrier'] <- '8'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'RetraitÃ©'] <- 'Retraité'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Demandeur dâ€™emploi'] <- 'Demandeur d\'emploi'
df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Etudiant, lycÃ©en'] <- 'Étudiant, lycéen'
#df_sondage$Categorie_Socio[df_sondage$Categorie_Socio == 'Autres'] <- '12'


#df_sondage$Categorie_Socio <- as.factor(df_sondage$Categorie_Socio)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
#df_sondage$Categorie_Socio <- relevel(df_sondage$Categorie_Socio, 'Femme')

#levels(df_sondage$Categorie_Socio)
#freq(df_sondage$Categorie_Socio)



############################################
# Modification Etude
df_sondage$Etude <- as.character(df_sondage$Etude)

df_sondage$Etude[df_sondage$Etude=='Sans diplÃ´me'] <- 'Sans diplôme'
df_sondage$Etude[df_sondage$Etude=='Brevet des collÃ¨ges'] <- 'Brevet'
df_sondage$Etude[df_sondage$Etude=='CAP/BEP (autres diplÃ´mes techniques)'] <- 'CAP/BEP'
df_sondage$Etude[df_sondage$Etude=='Bac (gÃ©nÃ©ral, pro et technologique)'] <- 'Bac'
df_sondage$Etude[df_sondage$Etude=='Bac+2 (BTS ou autre)'] <- 'Bac+2'
df_sondage$Etude[df_sondage$Etude=='Bac+3/4 (Licence, MaÃ®trise)'] <- 'Bac+3/4'
df_sondage$Etude[df_sondage$Etude=='Bac+5 (Master, Ã©coles d\'ingÃ©, Ã©coles d\'arts...)'] <- 'Bac+5'
df_sondage$Etude[df_sondage$Etude=='Bac+8 etc (Doctorat, post-doc, thÃ¨se)'] <- 'Bac+8'

#df_sondage$Etude <- as.factor(df_sondage$Etude)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
#df_sondage$Etude <- relevel(df_sondage$Etude, '0')

#levels(df_sondage$Etude)
#freq(df_sondage$Etude)


############################################
# Modification Sexe
df_sondage$Sexe <- as.character(df_sondage$Sexe)

df_sondage$Sexe[df_sondage$Sexe=='FÃ©minin'] <- 'Femme'
df_sondage$Sexe[df_sondage$Sexe=='Masculin'] <- 'Homme'
#df_sondage$Sexe <- as.factor(df_sondage$Sexe)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
#df_sondage$Sexe <- relevel(df_sondage$Sexe, '0')

#levels(df_sondage$Sexe)
#freq(df_sondage$Sexe)


############################################
# Modification Milieu_Habitat

df_sondage$Milieu_Habitat[df_sondage$Milieu_Habitat=='En milieu pÃ©riurbain'] <- 'En milieu périurbain'

#df_sondage$Milieu_Habitat <- as.factor(df_sondage$Milieu_Habitat)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
#df_sondage$Milieu_Habitat <- relevel(df_sondage$Milieu_Habitat, '0')

#levels(df_sondage$Milieu_Habitat)
#freq(df_sondage$Milieu_Habitat)


############################################
# Modification Statut_Marital
df_sondage$Statut_Marital[df_sondage$Statut_Marital=='CÃ©libataire'] <- 'Célibataire'

#df_sondage$Statut_Marital <- as.factor(df_sondage$Statut_Marital)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
#df_sondage$Statut_Marital <- relevel(df_sondage$Statut_Marital, '0')

#levels(df_sondage$Statut_Marital)
#freq(df_sondage$Statut_Marital)


############################################
# Modification Transport_Pro

df_sondage$Transport_Pro[df_sondage$Transport_Pro=='Ã€ pied'] <- 'À pied'
df_sondage$Transport_Pro[df_sondage$Transport_Pro=='Ã€ vÃ©lo'] <- 'À vélo'
df_sondage$Transport_Pro[df_sondage$Transport_Pro=='Teletravail toute l\'annÃ©e '] <- 'Télétravail toute l\'année'


#df_sondage$Transport_Pro <- as.factor(df_sondage$Transport_Pro)

#La fonction relevel me permet de choisir quel element sera le referenciel ici la
#df_sondage$Transport_Pro <- relevel(df_sondage$Transport_Pro, '0')

#levels(df_sondage$Transport_Pro)
#freq(df_sondage$Transport_Pro)


############################################
# Modification Geste_Barriere

df_sondage$Geste_Barriere[df_sondage$Geste_Barriere=='Jusqu\'Ã  l\'arrivÃ©e d\'un vaccin'] <- 'Jusqu\'à  l\'arrivée d\'un vaccin'






#######################
### 3 - Sauvegarde  ###
#######################

save(df_sondage, file = 'data/df_sondage.RData')
















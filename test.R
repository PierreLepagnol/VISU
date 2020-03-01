donnee=read_csv('./DataViz.csv')
targetvar='prix'
vect_name=colnames(donnee)[c(1,2,4)]
donnee2=Poopup_refactor(donnee,vect_name,targetvar)


Poopup_refactor=function(dataset_raw,vect_name,targetvar){
  # Selection des variables pertinantes
  dataset2=dataset_raw %>% select(vect_name)
  # Ajout du nom de la variable pour chaque ligne
  dataset2[] <- Map(paste, names(dataset2), dataset2, sep = ' : ')
  # création du popup
  dataset_raw$poopup <- apply( dataset2[ , vect_name] , 1 , FUN= paste,collapse = "<br/>" )
  # Selection des variables utiles à leaflet
  newDs=dataset_raw %>% select(lon,lat,poopup,targetvar)
  return(newDs)
}

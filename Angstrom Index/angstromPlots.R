seuil_incendie <- 10000 #m2 seuil pour les incendies

date_debut <- as.Date("01/06/2000","%d/%m/%Y")
date_fin <- as.Date("22/12/2020","%d/%m/%Y")

########

Incendies_filtered <- Incendie_81_2021[Incendie_81_2021$`Surface parcourue (m2)` >= seuil_incendie,]


graph_angstrom_index <- Angstrom_Index %>% filter(between(as.Date(DATE,origin="1970-01-01"), date_debut,date_fin))

graph_summer_angstrom_index <- Index_Summer %>% filter(between(as.Date(DATE,origin="1970-01-01"), date_debut,date_fin))


### Plotting all dates within chosen range ###

ggplot(data = graph_angstrom_index,
       mapping = aes(x = as.Date(DATE, origin="1970-01-01"), y = ANGSTROM_INDEX, color = ANGSTROM_INDEX, xmin = date_debut,xmax=date_fin)) +
  geom_point() +
  scale_color_gradient(guide="none", low="red", high="green") +
  labs(x = "Date", y = "Angstrom Index") +
  geom_point(data = graph_angstrom_index %>% filter(as.Date(DATE, origin="1970-01-01") %in% as.Date.character(Incendies_filtered$Alerte,format="%d/%m/%Y %H:%M", origin="1970-01-01")),
             pch=16, size=2, colour="black")

### Plotting all summer dates within chosen range ###

ggplot(data = graph_summer_angstrom_index,
       mapping = aes(x = as.Date(DATE, origin="1970-01-01"), y = ANGSTROM_INDEX, color = ANGSTROM_INDEX, xmin = date_debut,xmax=date_fin)) +
  geom_point() +
  scale_color_gradient(guide="none", low="red", high="green") +
  labs(x = "Date", y = "Angstrom Index") +
  geom_point(data = graph_summer_angstrom_index %>% filter(as.Date(DATE, origin="1970-01-01") %in% as.Date.character(Incendies_filtered$Alerte,format="%d/%m/%Y %H:%M", origin="1970-01-01")),
             pch=16, size=2, colour="black")

### plotting temperatures within chosen range ###

ggplot(data = graph_angstrom_index,
       mapping = aes(x = as.Date(DATE, origin="1970-01-01"), y = T, color = T, xmin = date_debut,xmax=date_fin)) +
  geom_point() +
  scale_color_gradient(guide="none", low="green", high="red") +
  labs(x = "Date", y = "Temperatures") +
  geom_point(data = graph_angstrom_index %>% filter(as.Date(DATE, origin="1970-01-01") %in% as.Date.character(Incendies_filtered$Alerte,format="%d/%m/%Y %H:%M", origin="1970-01-01")),
             pch=16, size=2, colour="black")

### plotting temperatures within chosen range ###

ggplot(data = graph_angstrom_index,
       mapping = aes(x = as.Date(DATE, origin="1970-01-01"), y = U, color = U, xmin = date_debut,xmax=date_fin)) +
  geom_point() +
  scale_color_gradient(guide="none", low="red", high="green") +
  labs(x = "Date", y = "Temperatures") +
  geom_point(data = graph_angstrom_index %>% filter(as.Date(DATE, origin="1970-01-01") %in% as.Date.character(Incendies_filtered$Alerte,format="%d/%m/%Y %H:%M", origin="1970-01-01")),
             pch=16, size=2, colour="black")
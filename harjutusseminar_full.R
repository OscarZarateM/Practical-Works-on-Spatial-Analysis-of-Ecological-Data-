
## teine variant paketiga dplyr
crops=filter(eestipunktid_sf,Esri_label=="Crops") #ei kaasa NA ridu 
plot(crops$geometry)
class(crops) #jääb samaks, mis algsel objektil
st_crs(crops)==st_crs(eestipunktid_sf) #jääb alles


################################### 8 PUHVRID ##################################
### Teeme 10 esimesele 'crops' punktile 5 km puhvrid ümber 
puhv=st_buffer(crops[1:10,],25000) #pakett saab ise aru, et tahame meetreid, mitte kraade
plot(puhv)
### Arvutame puhvrite pindala funktsiooniga st_area ja lisame selle veeru:
pindalad=st_area(puhv)
puhv$area=pindalad 

### Vaatame paketiga t-map kaarti, värvime  veeru "hfi" ehk human footprint indexi järgi,lisame parameetri alpha, et alad veidi läbi paistaksid
m4=tm_basemap("https://tiles.maaamet.ee/tm/tms/1.0.0/foto@GMC/{z}/{x}/{y}.png",tms=T)+
  tm_shape(puhv) +tm_polygons("hfi",alpha=0.8)+
  tm_shape(soomaavektor)+tm_polygons(col="lightblue")
leaflet=tmap_leaflet(m4)
show(leaflet)

################### 9 KATTUVUSED RASTRI JA VEKTORI VAHEL########################
### Leiame ala, kus puhvrite objekt kattub soomaaga
kattuvus=st_intersection(puhv,soomaavektor)
plot(kattuvus$geometry)

################################### 10 RASTERANDMED #############################
### Kasutades enne sisse laetud kõrgusandmeid,leiame keskmise kõrguse soomaa rahvuspargis. 

### Esmalt kontrollime, kas kihtide crs ühtib:
st_crs(soomaavektor)==st_crs(korgusraster)

### Lõikame kõrgusrasteri väiksemaks (ei ole tegelikult päringu tegemiseks vajalik)
soomaakorgus=crop(korgusraster,soomaavektor)
plot(soomaakorgus)
soomaakorgus=mask(korgusraster,soomaavektor)
plot(soomaakorgus) ## mida need funktsioonid erinevalt teevad?

### Küsime väärtused soomaa rahvuspargi piirides
soomaakorgused=unlist(terra::extract(korgusraster,soomaavektor))
hist(soomaakorgused,50)
mean(soomaakorgused)

### Vaatame kaarti ka 
m5=tm_basemap("https://tiles.maaamet.ee/tm/tms/1.0.0/foto@GMC/{z}/{x}/{y}.png",tms=T)+
  tm_shape(soomaavektor)+tm_borders(col="red")+
  tm_shape(soomaakorgus)+tm_raster()
leaflet=tmap_leaflet(m5)
show(leaflet)

### Muudame rasterkihi väärtusi -- nagu maatriksiga töötamine. Millised alad jäävad vee alla, kui merepind tõuseb 20 m?
korgusraster.uus=korgusraster > 20
plot(korgusraster.uus)

### Kauguse arvutus punkti(de)st, tulemuseks uus rasterkiht
kaugus_crop_punktidest=distance(korgusraster,crops)
plot(kaugus_crop_punktidest)
kaugus_crop_punktidest[is.na(korgusraster)]=NA
plot(kaugus_crop_punktidest)
plot(crops,add=TRUE,pch=16)

### Salvestame uue rasterkihi
writeRaster(kaugus_crop_punktidest,"kaugus_crop_punktidest.tif")

#################### KOHUSTUSLIKU OSA LÕPP ####################################




















































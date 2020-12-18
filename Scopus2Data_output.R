library(dplyr)
library(tidyr)
library(stringr)


scopus2data = function(dir)

    {
  
  #- ouvre le tableau : dir est le chemin vers ton jeu de donnÃ©es

  
  
   scopus=read.table(dir,sep=",",skip=7,header=T,encoding="UTF-8")
  
  
  
  
    #- crée un tableau par variable
  access = na.omit(scopus[,1:2]) ; names(access)[2]="N"
  years = na.omit(scopus[,3:4]) ; names(years)[2]="N"
  authors = na.omit(scopus[,5:6]) ; names(authors)[2]="N"
  subject_area = na.omit(scopus[,7:8]) ; names(subject_area)[2]="N"
  doc_type = na.omit(scopus[,9:10]) ; names(doc_type)[2]="N"
  journal = na.omit(scopus[,13:14]) ; names(journal)[2]="N"
  keywords = na.omit(scopus[,15:16]) ; names(keywords)[2]="N"
  affiliations = na.omit(scopus[,17:18]) ; names(affiliations)[2]="N"
  funding = na.omit(scopus[,19:20]) ; names(funding)[2]="N"
  country = na.omit(scopus[,20:21]) ; names(country)[2]="N"
  
  #- retourne une liste
  out = list(
    access=access,
    authors=authors,
    subject_area=subject_area,
    doc_type=doc_type,
    journal=journal,
    keywords=keywords,
    affiliations=affiliations,
    funding=funding,
    country=country
  )
  
    return(out)
}

dupli_authors = function(name1,name2)
  
{
    if (name1 %in% authors_i$author & name2 %in% authors_i$author)
  {
    authors_i[which(authors_i$author==name1),]$N=authors_i[which(authors_i$author==name1),]$N+authors_i[which(authors_i$author==name2),]$N
    authors_i=authors_i[-(which(authors_i$author==name2)),]
  }
  return(authors_i)  
}



`%notin%` <- Negate(`%in%`)


niveau="Pre_Sta"

if (niveau=="Dri_Pre")
{
wd="C:\\R\\output_requetes_DriPre"
setwd(wd)
files=list.files(path=wd)
file_names=paste(wd,files,sep = "\\")
}

if (niveau=="Pre_Sta")
{
wd="C:\\R\\output_requetes_PreSt"
setwd(wd)
files=list.files(path=wd)
file_names=paste(wd,files,sep = "\\")
}


### noms doubles 
name1=c(
  "Munson,  A.", 
  "Harvey,  B.", 
  "Walsh,  D.", 
  "Kneeshaw,  D.", 
  "Greene,  D.", 
  "Langor,  D.", 
  "Dewailly,  E.", 
  "Thiffault,  E.", 
  "Martin,  I.", 
  "Savard,  J.P.", 
  "Anyomi,  K.", 
  "Harper,  K.", 
  "Tsuji,  L.", 
  "Flannigan,  M.", 
  "Fenton, ", 
  "Alvarez,  A.", 
  "Bernier,  P.", 
  "Bradley,  R.", 
  "Parsons,  W.")
name2=c("Munson,  A.D.", 
        "Harvey,  B.D.", 
        "Walsh,  D.A.", 
        "Kneeshaw,  D.D.", 
        "Greene,  D.F.", 
        "Langor,  D.W.", 
        "Dewailly,  É.", 
        "Thiffault,  É.", 
        "Martin,  I.D.", 
        "Savard,  J.P.L.", 
        "Anyomi,  K.A.", 
        "Harper,  K.A.", 
        "Tsuji,  L.J.S.", 
        "Flannigan,  M.D.", 
        "Fenton,  N.J.", 
        "Alvarez,  P.E.", 
        "Bernier,  P.Y.", 
        "Bradley,  R.L.", 
        "Parsons,  W.F.J.")
  
query=character()
nb_pub_df=data.frame(query=character(),N=numeric(),stringsAsFactors = FALSE)
authors_df=data.frame(query=character(),authors=character(),N=numeric(),stringsAsFactors = FALSE)
journal_df=data.frame(query=character(),journal=character(),N=numeric(),stringsAsFactors = FALSE)
subject_area_df=data.frame(query=character(),subject_area=character(),N=numeric(),stringsAsFactors = FALSE)
keywords_df=data.frame(query=character(),keywords=character(),N=numeric(),stringsAsFactors = FALSE)
id_i=numeric()

for (i in 1:length(files))

{
dir=file_names[i]

query_i=as.character(read.table(dir,sep=",", fill=T)[2,1])


if
(query_i %notin% query)
{
query= c(query, query_i)
 
data=scopus2data(dir=file_names[i])  

##update dataframes 
nb_pub_i=data.frame(query=as.character(query_i),N=sum(data$access$N))
nb_pub_df=bind_rows(nb_pub_i,nb_pub_df)

authors_i=data.frame(query=as.character(rep(query_i,nrow(data$authors))),author=as.character(data$authors$AUTHOR.NAME),N=data$authors$N,stringsAsFactors = FALSE)

## Merge duplicate authors

for (j in 1:length(name1))
{
dupli_authors(name1[j],name2[j])
}


authors_df=bind_rows(authors_df,authors_i)



journal_i=data.frame(query=as.character(rep(query_i,nrow(data$journal))),journal=as.character(data$journal$SOURCE.TITLE),N=data$journal$N,stringsAsFactors = FALSE)
journal_df=bind_rows(journal_df,journal_i)

subject_area_i=data.frame(query=as.character(rep(query_i,nrow(data$subject_area))),subject_area=as.character(data$subject_area$SUBJECT.AREA),N=data$subject_area$N,stringsAsFactors = FALSE)
subject_area_df=bind_rows(subject_area_df,subject_area_i)


keywords_i=data.frame(query=as.character(rep(query_i,nrow(data$keywords))),keywords=as.character(data$keywords$KEYWORD),N=data$keywords$N,stringsAsFactors = FALSE)
keywords_df=bind_rows(keywords_df,keywords_i)
id_i=c(id_i,i)

  }
}

id_i
length(id_i)  

query_low=str_to_lower(query)

region=ifelse(str_detect(query, "Canada", negate = FALSE),"Canada","Quebec")
Pressure=character(length=length(query))

length(which(region=="Quebec"))

Pressure[which(str_detect(query_low, fixed("contamination or contaminant or pollut"), negate = FALSE))]="Contaminant release"

Pressure[which(str_detect(query_low, fixed("species and (distribution or extinct")))]="Species range change"

Pressure[which(str_detect(query_low, fixed("electricity and ( distribution or transmission)")))]="Electricity distribution"

Pressure[which(str_detect(query_low, fixed("forest and fire")))]="Fire"

Pressure[which(str_detect(query_low, fixed("mining and (metal or mineral or ore")))]="Ore extraction"

Pressure[which(str_detect(query_low, fixed("road and (construction or closure")))]="Road development"

Pressure[which(str_detect(query_low, fixed("weather or   season* or meteo*")))]="weather changes"

Pressure[which(str_detect(query_low, fixed("silvicultur* or intensive or {site preparation}")))]="sylvicultural treatments"

Pressure[which(str_detect(query_low, fixed("harvest* or logging or cut or clear-cut  or clearcut or {clear cut}")))]="Timber harvests"

Pressure[which(Pressure=="")]="erreur"

unique(Pressure)

if (niveau=="Pre_Sta")
{

#### State

State=character(length=length(query))

State[which(str_detect(query_low, fixed("{age structure}  or  old-growth"), negate = FALSE))]="Age structure"

State[which(str_detect(query_low, fixed("{tree composition} or {tree species composition} or {stand composition} or {tree assemblage} or {tree species assemblage}")))]="Composition"

State[which(str_detect(query_low, fixed("cultural and keystone)  or  spiritual or sacred or  special) and (site or place)")))]="Cultural places"

State[which(str_detect(query_low, fixed("geese or goose")))]="Geese area"

State[which(str_detect(query_low, fixed("soil  or  ground  or  {woody debris}  or  microtopography  or  micro-topography")))]="Ground state"

State[which(str_detect(query_low, "ice", negate = FALSE))]="Ice conditions"

State[which(str_detect(query_low, fixed("naturalness or  wilderness or primeval or preindustrial or pre-industrial  or {intact forest}")))]="Naturalness"

State[which(str_detect(query_low, fixed("predat*")))]="Predators"

State[which(str_detect(query_low, fixed("road  and (density or network)")))]="Road density"

State[which(str_detect(query_low, fixed("{fish habitat}  or  (spawning and (ground or area))")))]="Spawning areas"

State[which(str_detect(query_low, fixed("traffic or  crowd*  or {human presence} or cabin or {land users} or recreation*"), negate = FALSE))]="Traffic"

State[which(str_detect(query_low, fixed("truck or train or (noise  and disturbance)")))]="Transportation"

State[which(str_detect(query_low, fixed("water and  (quality or lake or river or stream)")))]="Water quality"

State[which(str_detect(query_low, fixed("{water temperature} or  {lake temperature} or {river temperature} or {stream temperature}")))]="Water temperature"

State[which(str_detect(query_low, fixed("(species and diversity) or biodiversity)")))]="Species diversity"

State[which(str_detect(query_low, fixed("parasite or stress or {heavy metal} or mercury or")))]="Wildlife health"

State[which(str_detect(query_low, fixed("mine or mining) and (tailing or  slam or  waste or residu*")))]="Mine tailing"

State[which(State=="")]="erreur"

unique(State)



### Pressure State

Pre_sta_qc=tbl_df(data.frame(query, region, Pressure, State))%>% 
  filter(State!="erreur" & Pressure!="erreur")%>%
  left_join(nb_pub_df)%>%
  select(region, Pressure, State, N)%>%
  filter(region=="Quebec")
write.table(Pre_sta_qc,"C:\\R\\pre_sta_qc.csv")

Pre_sta_can=tbl_df(data.frame(query, region, Pressure, State))%>% 
  filter(State!="erreur" & Pressure!="erreur")%>%
  left_join(nb_pub_df)%>%
  select(region, Pressure, State, N)%>%
  filter(region=="Canada")
write.table(Pre_sta_can,"C:\\R\\pre_sta_canc.csv")

Pre_sta_auteurs_qc=tbl_df(data.frame(query, region, Pressure, State))%>% 
  filter(State!="erreur" & Pressure!="erreur")%>%
  filter(region=="Quebec")%>%
  left_join(authors_df)%>%
  group_by(Pressure, State,author)%>%
  summarise(NB=sum(N))%>%
  group_by(Pressure,State)%>%
  summarise(n_author=n_distinct(author))

write.table(Pre_sta_auteurs_qc,"C:\\R\\pre_sta_qc_author.csv")

Pre_sta_auteurs_can=tbl_df(data.frame(query, region, Pressure, State))%>% 
  filter(State!="erreur" & Pressure!="erreur")%>%
  filter(region=="Canada")%>%
  left_join(authors_df)%>%
  group_by(Pressure, State)%>%
  summarise(n_author=n_distinct(author))


write.table(Pre_sta_auteurs_can,"C:\\R\\pre_sta_can_author.csv")

### Vérifier la distribution du nombre de ub par auteurs par requete pour garder juste les plus productifs
prod_qc=tbl_df(data.frame(query, region, Pressure, State))%>% 
  filter(State!="erreur" & Pressure!="erreur")%>%
  filter(region=="Quebec")%>%
  left_join(authors_df)%>%
  group_by(Pressure, State,author)%>%
  summarise(NB=sum(N))%>%
  filter(NB>2)%>%
  group_by(Pressure, State)%>%
  summarise(nb_auteurs=n_distinct(author))

write.table(prod_qc,"C:\\R\\pre_sta_qc_author_SUP2.csv")


prod_can=tbl_df(data.frame(query, region, Pressure, State))%>% 
  filter(State!="erreur" & Pressure!="erreur")%>%
  filter(region=="Canada")%>%
  left_join(authors_df)%>%
  group_by(Pressure, State,author)%>%
  summarise(NB=sum(N))%>%
  filter(NB>2)%>%
  group_by(Pressure, State)%>%
  summarise(nb_auteurs=n_distinct(author))

write.table(prod_can,"C:\\R\\pre_sta_can_author_SUP2.csv")


}


if (niveau=="Dri_Pre")
{

Driver=character(length=length(query))

Driver[which(str_detect(query_low, fixed("{climate change} or {climatic change} or {global warming}")))]="Climate change"

Driver[which(str_detect(query_low, fixed("forestry or {forest management} or {forest practices}")))]="Forestry"

Driver[which(str_detect(query_low, fixed("mine or mining"), negate = FALSE))]="Mining industry"

Driver[which(str_detect(query_low, fixed("electricity or hydropower or hydro-power or hydroelectricity or hydro-electricity")))]="Hydro-electricity"


Driver[which(Driver=="")]="erreur"


##### Driver Pressure

Dri_Pre_qc=tbl_df(data.frame(query, region, Driver, Pressure))%>% 
  filter(Driver!="erreur" & Pressure!="erreur")%>%
  left_join(nb_pub_df)%>%
  select(region, Driver, Pressure, N)%>%
  filter(region=="Quebec")
write.table(Dri_Pre_qc,"C:\\R\\dri_pre_qc.csv")
hist(Dri_Pre_qc$N)
unique(Dri_Pre_qc$Driver)



Dri_Pre_can=tbl_df(data.frame(query, region, Driver, Pressure))%>% 
  filter(Driver!="erreur" & Pressure!="erreur")%>%
  left_join(nb_pub_df)%>%
  select(region, Driver, Pressure, N)%>%
  filter(region=="Canada")
write.table(Dri_Pre_can,"C:\\R\\dri_pre_canc.csv")


unique(Dri_Pre_can$Driver)


Dri_Pre_auteurs_qc=tbl_df(data.frame(query, region, Driver, Pressure))%>% 
  filter(Driver!="erreur" & Pressure!="erreur")%>%
  filter(region=="Quebec")%>%
  left_join(authors_df)%>%
  group_by(Driver, Pressure)%>%
  summarise(test=n_distinct(author))

write.table(Dri_Pre_auteurs_qc,"C:\\R\\dri_pre_qc_author.csv")

Dri_Pre_auteurs_can=tbl_df(data.frame(query, region, Driver,Pressure))%>% 
  filter(Driver!="erreur" & Pressure!="erreur")%>%
  filter(region=="Canada")%>%
  left_join(authors_df)%>%
  group_by(Driver, Pressure)%>%
  summarise(test=n_distinct(author))

write.table(Dri_Pre_auteurs_can,"C:\\R\\dri_pre_can_author.csv")

### VÃ©rifier la distribution du nombre de ub par auteurs par requete pour garder juste les plus productifs (minimum de 4)
Dri_Pre_auteurs_Min3_qc=tbl_df(data.frame(query, region, Driver, Pressure))%>% 
  filter(Driver!="erreur" & Pressure!="erreur")%>%
  filter(region=="Quebec")%>%
  left_join(authors_df)%>%
  group_by(Driver, Pressure,author)%>%
  summarise(NB=sum(N))%>%
  filter(NB>2)%>%
  group_by(Driver, Pressure)%>%
  summarise(nb_auteurs=n_distinct(author))

View(Dri_Pre_auteurs_Min3_qc)

write.table(Dri_Pre_auteurs_Min3_qc,"C:\\R\\dri_pre_qc_author_SUP2.csv")



Dri_Pre_auteurs_Min3_can=tbl_df(data.frame(query, region, Driver, Pressure))%>% 
  filter(Driver!="erreur" & Pressure!="erreur")%>%
  filter(region=="Canada")%>%
  left_join(authors_df)%>%
  group_by(Driver,Pressure,author)%>%
  summarise(NB=sum(N))%>%
  filter(NB>2)%>%
  group_by(Driver,Pressure)%>%
  summarise(nb_auteurs=n_distinct(author))

write.table(Dri_Pre_auteurs_Min3_can,"C:\\R\\dri_pre_can_author_SUP2.csv")

}




##Validation 
#sortir les auteurs qui ont plus de deux publications  par requete ()


if (niveau=="Pre_Sta")
{

# Pressure State
auteursSUP2_pre_st_qc=tbl_df(data.frame(query, region, Pressure, State))%>% 
  filter(State!="erreur" & Pressure!="erreur")%>%
  filter(region=="Quebec")%>%
  left_join(authors_df)%>%
  group_by(Pressure, State,author)%>%
  summarise(NB=sum(N))%>%
  filter(NB>2)

length(unique(auteursSUP2_pre_st_qc$author))

write.table(auteursSUP2_pre_st_qc,"C:\\R\\validation_pre_st_auteursSUP2_qc_valisation.csv")
}

#Driver Pressure
if (niveau=="Dri_Pre")
{

auteursSUP2_dri_pre_qc=tbl_df(data.frame(query, region, Driver, Pressure))%>% 
  filter(Driver!="erreur" & Pressure!="erreur")%>%
  left_join(tbl_df(authors_df),by="query")%>%
  filter(region=="Quebec")%>%
  group_by(Driver, Pressure, author)%>%
  summarise(NB=sum(N))%>%
  filter(NB>2)
write.table(auteursSUP2_dri_pre_qc,"C:\\R\\validation_dri_pre_auteursSUP2_qc.csv")
}

length(unique(auteursSUP2_dri_pre_qc$author))
##Validation 
#Vérification des noms doubles (initiales qui diffèrent)(Québec)
auteurs_nom_dri_pre_qc=tbl_df(data.frame(query, region, Driver, Pressure))%>% 
  filter(Driver!="erreur" & Pressure!="erreur")%>%
  left_join(tbl_df(authors_df),by="query")%>%
  filter(region=="Quebec")%>%
  group_by(author)%>%
  summarise(NB=sum(N))

split=str_split_fixed(auteurs_nom_dri_pre_qc$author,pattern=",",n=2)

noms_doubles=tbl_df(data.frame(last=split[,1],init=split[,2]))%>%
  group_by(last)%>%
  summarise(NB=n())%>%
  filter(NB>1)
  
noms=tbl_df(data.frame(last=split[,1],init=split[,2]))%>%
  filter(last %in% noms_doubles$last)
View(noms)
write.csv(noms,"C:\\R\\noms.csv")

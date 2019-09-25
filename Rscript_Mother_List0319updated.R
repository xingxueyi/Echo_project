#Copy this file, echocon_xx.sas7bdat, echoconpv_xx.sas7bdat, bid.csv, and previous lists into the same folder you are working. 
#SOURCE this line first, DO NOT use run.
rm(list=ls())
mydir <-dirname(parent.frame(2)$ofile)

setwd(mydir)

stop( "You already changed your working directory. Please disregard this error message and continue to run the rest codes, :)")
#########################################

#install and load needed library packages

#install.packages('foreign')
#install.packages('Hmisc')
#install.packages('dplyr')
#install.packages('sas7bdat')
#install.packages('reshape')
#install.packages('haven')

library(foreign)
library(Hmisc)
library(dplyr)
library(sas7bdat)
library(reshape) 
library(haven)

motherlist<- 'P1_HealthCare_2_26_2019.csv'
consentnames<- 'echoconpv_37.sas7bdat'
consentlist<-'echocon_37.sas7bdat'
bidlist<-'bid.csv'
inputpalist<-'BOXPAMotherList031819.csv'
inputnclist<-'BOXNCMotherList031819.csv'
outputpamotherlist<-'BOXPAMotherList052119.csv'
outputncmotherlist<-'BOXNCMotherList052119.csv'
outputproviderlistpa<-'BOXPAProviderList052119.csv'
outputproviderlistnc<-'BOXNCProviderList052119.csv'
NCmotheruniquenames<-"NC_Mother_Unique_Names052119.csv"
PAmotheruniquenames<-"PA_Mother_Unique_Names052119.csv"

#this line does not need to change
inputproviderlist<-"BOXAllMotherProviderList020118.csv"

bid<-read.csv(bidlist)
nrow(bid)
pa7a <- read_sas(consentlist)
View(pa7a)
pa7aaddv<-pa7a[c(1:2,28,33,42,44,305)]
pa7aaddv2<-merge(pa7aaddv, bid, by='S_ID')
pa7aaddv2<-pa7aaddv2[-c(9)]
pa7aaddv2nc<-pa7aaddv2[pa7aaddv2$State=="NC",]
pa7aaddv2pa<-pa7aaddv2[pa7aaddv2$State=="PA",]
#write.csv(pa7aaddv2nc,file="NC_consent_info_all_kids.csv", row.names=F)
#write.csv(pa7aaddv2pa,file="PA_consent_info_all_kids.csv", row.names=F)

#get the subset of parents who gave consents
pb7<-subset(pa7a, momHipHCP==1 & PCSgnAHIPHCP==1)


#pb71<-subset(pa7a, PCSgnAHIPHCP==0|PCSgnAHIPMed==0)
#test<-anti_join(pb71,pb7,by='S_ID')
#keep id, state, sex dob, health record permit, and school names
pb7<-pb7[c(1,2,19,20,30,31,33,42:45,28,305)]
colnames(pb7)[1]='FLPID'


# [1] 1331
colnames(bid)[1]='FLPID'

#get bids
pb7<-merge(pb7,bid,by='FLPID')


#read data. HCPPreDr1,2 are the 1st and 2nd providers, and HCPPreER is the emergence department provider.
wide<-read.csv(motherlist)

#change factor variables to string variables
wide$HCPPreDr1<-as.character(wide$HCPPreDr1)
wide$HCPPreDr2<-as.character(wide$HCPPreDr2)
wide$HCPPreER<-as.character(wide$HCPPreER)
View(wide)

#since we have two provider names for each subject, we need to change the wide format to long format, 

long<-reshape(wide, varying =c('HCPPreDr1','HCPPreDr2'), timevar = "order", idvar = "S_ID", direction='long', sep = "")

#sort by S_ID and order
long<-long[order(long$S_ID,long$order),]

#clean the data, change all nonsense values to "UNKNOWN", although I have included many values, more values might need to be added.
long$HCPPreDr[is.na(long$HCPPreDr)]<-"UNKNOWN"
long$HCPPreDr[long$HCPPreDr %in% c(".D","na","same", "Same", "SAME","none","N/A","n/a","0","same as above","Na","GRANDMOTHER IS PC","Hinesville, GA","NO PRENATAL CARE","NOT SURE OF NAME PRIVATE PRACTICE IN WILSON","B Building near the Altoona Hospital","Do Not Remeber","doctor's office","Dont Know")]<-"UNKNOWN"
long$HCPPreDr[long$HCPPreDr %in% c("dont know" ,"Doesnt know","Dont know","in Huntingdon","unknown|Johnstown","K","None","k","N/a","y","UNKNOWN","no","doctor","Obgyn","a","Clinic","Doctor","None","Adoptive Parent")]<-"UNKNOWN"

long$HCPPreDr[long$HCPPreDr %in% c(".D","na","same", "Same", "SAME","none","N/A","n/a","0","same as above","Na")]<-"UNKNOWN"
long$HCPPreDr[long$HCPPreDr %in% c("GRANDMOTHER IS PC","Hinesville, GA","NO PRENATAL CARE","NOT SURE OF NAME PRIVATE PRACTICE IN WILSON","B Building near the Altoona Hospital","Do Not Remeber","doctor's office")]<-"UNKNOWN"

long$HCPPreDr[long$HCPPreDr %in% c("Dont Know","dont know","Doesnt know","Dont know","in Huntingdon","unknown|Johnstown","K","None","k","N/a","y","UNKNOWN","no","doctor","Obgyn","a","Clinic","Doctor","None","Adoptive Parent")]<-"UNKNOWN"

#deduplicate records with same SID and the provider name
#define column names to be used
depvar1<-long[,c('S_ID','HCPPreDr')]
View(depvar1)

#depulicate
longunq<-long[!duplicated(depvar1),]


longunq1<-longunq[,c(1:4,15,45)]
#longunq1$HCPPreDr<-gsub("/"," ",longunq1$HCPPreDr)
#match provider names to consented subjects
mconsentnames<-merge(pb7, longunq1, by.x="FLPID",by.y="S_ID", all.x=T)
mconsentnames$HCPPreDr[is.na(mconsentnames$HCPPreDr)]<-"UNKNOWN"
#rerun mark p1;
#Get the provider list.
#inputproviderlist<-"BOXAllMotherProviderList020118.csv"
mprolist<-read.csv(inputproviderlist,stringsAsFactors=FALSE)

mprolist<-mprolist[order(mprolist$state,mprolist$Clinic),]

mprolist$groupid<-seq(1:nrow(mprolist))

m<-nrow(mconsentnames)
p<-nrow(mprolist)
for (i in 1:m){
  for (j in 1:p){
    if (grepl(mconsentnames$HCPPreDr[i],mprolist$AllPossibleNames[j],fixed=TRUE)){ mconsentnames$groupname[i]<- mprolist$Clinic[j]
    mconsentnames$gid[i]<- mprolist$groupid[j]
    break}
    else { mconsentnames$groupname[i]<- NA
    mconsentnames$gid[i]<- NA
    }
  }}
mconsentnames1a<-mconsentnames[is.na(mconsentnames$groupname),]
#write.csv(mconsentnames1a,file='verifymproviders.csv')
#should be null, if not, fill in new names in BOXAllMotherProviderList020118.csv and rerun this part (rerun mark p1).


###rerun mark pa2
#subset of those groupname is not unknown
mconsentnames1<-subset(mconsentnames, mconsentnames$groupname!='Unknown')
#deduplicating against FLPID and groupname
mconsentnames1u<-mconsentnames1[!duplicated(mconsentnames1[,c('FLPID', 'groupname')]),]

#subset of those groupname is unknown
mconsentnames2<-subset(mconsentnames, mconsentnames$groupname=='Unknown')
#deduplicating against FLPID
mconsentnames2u<-mconsentnames2[!duplicated(mconsentnames2$FLPID),]

#see if the flpids in mconsentnames2u appeared in mconsentnames1, if all true then just use mconsentnames1u for the rest steps (not likely to happen).
mconsentnames2u$status<-mconsentnames2u$FLPID %in% mconsentnames1u$FLPID
#see if the flpids in mconsentnames2 appeared in mconsentnames1u. 
mconsentnames2u$seqe<-seq(1:nrow(mconsentnames2u))
#see which case is not in mconsentnames1
which(!mconsentnames2u$status)
#subsetting these cases, appending them to consentnames1 
mconsentnames2ua<-mconsentnames2u[!mconsentnames2u$status,]
mconsentnames2ub<-subset(mconsentnames2ua, select=-c(status, seqe))
mconsentnamesnew<-rbind(mconsentnames1u,mconsentnames2ub)




#library(dplyr)
dist1<-mconsentnamesnew %>% distinct(groupname, FLPID)
dist2<-dist1%>% arrange(groupname, FLPID)

#consentnames: all names from EchoCon\echoconpv_xx.sas7bdat, change files each time;
all_names<- read_sas(consentnames) 
namespa<-all_names
View(namespa)
names1pa<-namespa[c(1:20)] 
View(namespa)
names1pa$FLPID<-names1pa$S_ID
namepamerge<-merge(dist2,names1pa,by='FLPID')

namepamerge$TCBirCFirNam[namepamerge$TCBirCFirNam==".D"]=''
namepamerge$TCBirCMidNam[namepamerge$TCBirCMidNam==".D"]=''
namepamerge$TCBirCLstNam[namepamerge$TCBirCLstNam==".D"]=''
namepamerge$TCBirCFirNam[namepamerge$TCBirCFirNam=="N/A"]=''
namepamerge$TCBirCMidNam[namepamerge$TCBirCMidNam=="N/A"]=''
namepamerge$TCBirCLstNam[namepamerge$TCBirCLstNam=="N/A"]=''


namepamerge$newname<-paste(namepamerge$TCBirCFirNam,namepamerge$TCBirCMidNam,namepamerge$TCBirCLstNam,by=' ')
library(data.table)
setnames(namepamerge, old=c("SFulName",'PCFulName','PCMaidenNam','TCBirCFirNam','TCBirCMidNam','TCBirCLstNam','newname','PCDOB'), new=c("ChildFullName", "ParentFullName",'ParentMaidenName','ChildFirstName','ChildMidName','ChildLastName','ChildFullName','ParentDOB'))
head(namepamerge)
names(namepamerge)[23]<-'ChildNewFullName'

#pb7 is the list of subjects who gave consent (see SAS codes), which has bids. Alternatively, you can merge namepamerge1 with bid.csv.
namepamerge1<-merge(namepamerge,pb7,by='FLPID')
namepamerge2<-subset(namepamerge1,!is.na(namepamerge1$groupname))
nrow(namepamerge2)

#delete first column, FLPID, S_ID, CaseID, state, adjust orders as boxpachildrenlist0717.csv, fill missing names
namepamerge2a = subset(namepamerge2, select = c(FLPID, ChildFullName, groupname,ParentFullName, ParentMaidenName, birthCert_ID, ParentDOB, ChildFirstName, ChildMidName, ChildLastName, ChildNewFullName,SSex, SDOB, state, PCFirAdrStr, PCFirAdrCty, PCFirAdrSta,PCFirAdrZip) )

setnames(namepamerge2a, old=c("groupname","SSex","SDOB"), new=c("ClinicName", "ChildSex","ChildDOB"))

namepamerge2a<-namepamerge2a[order(namepamerge2a$ClinicName),]
namepamerge2a$seqen<-seq(1:nrow(namepamerge2a))
View(namepamerge2a)
#check missing values for child name
which(namepamerge2a$ChildFirstName=="")
# 27 122 131 186 216 237 270
#change all factors to strings
str(namepamerge2a)
fctr.cols <- sapply(namepamerge2a, is.factor)
fctr.cols
namepamerge2a[, fctr.cols] <- sapply(namepamerge2a[, fctr.cols], as.character)
str(namepamerge2a)
#filling missing names
seqlist<- which(namepamerge2a$ChildFirstName=="")

for (i in seqlist) {
  #separate fir/mid/last names and transform them into lower cases 
  missingnames<-strsplit(tolower(namepamerge2a$ChildFullName[i]), " ")[[1]]
  #drop any ',' in the names
  missingnames<-gsub(",", "", missingnames)
  ifelse(head(missingnames,1) %in% c("mr","mr.","dr","dr.","mrs","mrs.","sir","sir.","miss","miss.","ms","ms."), 
         missingnames<-tail(missingnames,-1),
         missingnames<-missingnames)
  #identify valid last names, create new lastnames when they has suffixes 
  ifelse(tail(missingnames,1) %in% c("jr","jr.","sr","sr.","ii","iii","iv","iv."), 
         missingnames<-c(head(missingnames,-2), paste(head(tail(missingnames,2),-1), toupper(tail(missingnames,1)),sep=', ')),
         missingnames<-missingnames)
  #get the first names
  namepamerge2a$ChildFirstName[i]<- paste(toupper(substring(missingnames[1],1,1)),substring(missingnames[1],2),sep="")
  #get the right midnames
  ifelse(length(missingnames)==2, 
         namepamerge2a$ChildMidName[i]<-'',
         namepamerge2a$ChildMidName[i]<- paste(toupper(substring(missingnames[2],1,1)),substring(missingnames[2],2),sep=""))
  ifelse(length(missingnames)==3, 
         namepamerge2a$ChildLastName[i]<- paste(toupper(substring(missingnames[3],1,1)),substring(missingnames[3],2),sep=""),
         namepamerge2a$ChildLastName[i]<-paste(toupper(substring(tail(missingnames,1),1,1)),substring(tail(missingnames,1),2),sep=""))
  #assemble new full names
  namepamerge2a$ChildNewFullName[i]<-paste(namepamerge2a$ChildFirstName[i], namepamerge2a$ChildMidName[i], namepamerge2a$ChildLastName[i],sep=" ")}



#check again, if not zero, repeat above procedures.
which(namepamerge2a$ChildFirstName=="")
namepamerge2a<- namepamerge2a[order(namepamerge2a$ClinicName, namepamerge2a$ParentFullName),]
#drop some columns
namepamerge2a<-subset(namepamerge2a, select=-c(ChildFullName, seqen))

#read last batch of mother lists, for comparison purpose.

ppalist<- read.csv(inputpalist)
pnclist<- read.csv(inputnclist)
#combine pa and nc mother lists
#inputpamotherlist<-rbind(palist,nclist)
previouspamotherlist<- rbind(ppalist,pnclist)
namepamerge2a$is.not.new<-namepamerge2a$birthCert_ID %in% previouspamotherlist$birthCert_ID
namepamerge2apa<- namepamerge2a[namepamerge2a$state=='PA',]
namepamerge2anc<- namepamerge2a[namepamerge2a$state=='NC',]

write.csv(namepamerge2apa, file= outputpamotherlist, row.names=F)
write.csv(namepamerge2anc, file= outputncmotherlist, row.names=F)
namepamerge2apand<-namepamerge2apa[!duplicated(namepamerge2apa$FLPID),]
write.csv(namepamerge2apand,file=PAmotheruniquenames, row.names=F)
namepamerge2ancnd<-namepamerge2anc[!duplicated(namepamerge2anc$FLPID),]
write.csv(namepamerge2ancnd,file=NCmotheruniquenames, row.names=F)
#delete the first column (unsorted ids)
existedaddnc<-merge(namepamerge2ancnd, pa7aaddv2nc, by="birthCert_ID")
existedaddpa<-merge(namepamerge2apand, pa7aaddv2pa, by="birthCert_ID")
existedaddpa<-existedaddpa[order(existedaddpa$ClinicName,existedaddpa$ParentFullName),]
existedaddnc<-existedaddnc[order(existedaddnc$ClinicName,existedaddnc$ParentFullName),]
existedaddpa<-subset(existedaddpa,select=-c(S_ID,state))
existedaddnc<-subset(existedaddnc,select=-c(S_ID,state))
write.csv(existedaddnc,file=NCmotheruniquenames,row.names=F)
write.csv(existedaddpa,file=PAmotheruniquenames,row.names=F)


#count the frequency of patients in each provider
library(plyr)
namepamerge2a1<-namepamerge2a[namepamerge2a$state=='PA',]
y1=count(namepamerge2a1, 'ClinicName')
colnames(y1)<-c('Clinic','freq')
View(y1)
#mprolistpa<-mprolist[mprolist$state=='PA',]

meg11<-merge(y1,mprolist,by='Clinic')
meg12<-subset(meg11,select=-c(groupid))


write.csv(meg12, file= outputproviderlistpa, row.names=F)

namepamerge2a2<-namepamerge2a[namepamerge2a$state=='NC',]
y2=count(namepamerge2a2, 'ClinicName')
colnames(y2)<-c('Clinic','freq')
View(y2)
#mprolistpa<-mprolist[mprolist$state=='PA',]

meg21<-merge(y2,mprolist,by='Clinic')
meg22<-subset(meg21,select=-c(groupid))

write.csv(meg22, file= outputproviderlistnc, row.names=F)
#delete first and other unnecessary columns.


#################################################################
#                        testing code                           #
#################################################################

#-----------------------------------

# drugreaction_1drug<-extractcases(d1=drugs1,d2="OTHER DRUGS")
# #draw 2 pie plots and provide data
# drug1pie<-tdte2pie(tdte=drugreaction_1drug$d1alle[,c(1,3)]%>%unique(),druggroupname=druggroupname1)
# 
# drug2pie<-tdte2pie(tdte=drugreaction_1drug$d2alle[,c(1,3)]%>%unique(),druggroupname=druggroupname2)
# # group according to param

#show most potential reactions-----------------
fdapieplot<-function(drugs1=NULL,druggroupname1=NULL,USRID=NULL){
  USRID_mpt<-paste(USRID,"_mpt",sep = "")
  drugreaction_1drug<-extractcases(d1=drugs1,d2="OTHER DRUGS")
  groupres<-groupbynon(drugreaction=drugreaction_1drug)
  drug1pie<-tdte2pie(tdte=drugreaction_1drug$d1alle[,c(1,3)]%>%unique(),druggroupname=druggroupname1,USRID=USRID)
  if(nrow(drug1pie$df)>15){tr=as.character(drug1pie$df$Reaction[1:15])}else{tr=as.character(drug1pie$df$Reaction)}
  most_potential_reactions<-combnongroupres(groupres=groupres,targetreac=tr)
  dfall<-most_potential_reactions[order(most_potential_reactions$est,decreasing = T),]
  cgres2plot(dfall=dfall,druggroupname1=druggroupname1,druggroupname2="Other Drugs",USRID=USRID_mpt)
  write.table(most_potential_reactions,paste("WWW/temp/",USRID_mpt,"_forest.xls",sep=""),col.names = T,row.names = F,sep="\t")
  write.table(drug1pie$df,paste("WWW/temp/",USRID,"_pie.xls",sep=""),col.names = T,row.names = F,sep="\t")
  return(drug1pie$df2)
  }
# drugs1="FERROUS CYSTEINE GLYCINATE"
# druggroupname1="test"
# x<-drugreaction_1drug$d1alle
#draw forest plot for drug vs drug reactions-----------------
ffplot_dvd<-function(drugs1=NULL,drugs2=NULL,USRID=NULL,druggroupname1=NULL,druggroupname2=NULL,targetreac=NULL,subgroup=NULL,age_unit=NULL,age=NULL){
  USRID_dvd<-paste(USRID,"_dvd",sep = "")
  drugreaction_1drug<-extractcases(d1=drugs1,d2=drugs2)
  if (subgroup=="age"){
  groupres<-groupbyage(drugreaction=drugreaction_1drug,age_unit=age_unit,age=age)
  dfall<-combagegroupres(groupres=groupres,targetreac=targetreac,age=age)
  }
  if (subgroup=="gender"){
  groupres<-groupbygender(drugreaction=drugreaction_1drug)
  dfall<-combgendergroupres(groupres=groupres,targetreac=targetreac)
  }
  if (subgroup=="no"){
  groupres<-groupbynon(drugreaction=drugreaction_1drug)
  dfall<-combnongroupres(groupres=groupres,targetreac=targetreac)
  }
  cgres2plot(dfall=dfall,druggroupname1=druggroupname1,druggroupname2=druggroupname2,USRID=USRID_dvd)
  write.table(dfall,paste("WWW/temp/",USRID_dvd,"_forest.xls",sep=""),col.names = T,row.names = F,sep="\t")
  write.table(drugreaction_1drug$d1alle,paste("WWW/temp/",USRID_dvd,"_allreac.xls",sep=""),col.names = T,row.names = F,sep="\t")
  drugreaction_1drug$d1alle
}
# ffplot_dvd(drugs1=c("ADEFOVIR","ADEFOVIR DIPIVOXIL"),drugs2=c("TENOFOVIR","TENOFOVIR ALAFENAMIDE"),USRID=123123,
#            druggroupname1 = "test1",druggroupname2 = "test2",targetreac = c("abortion","stillbirth"),subgroup = "Gender")

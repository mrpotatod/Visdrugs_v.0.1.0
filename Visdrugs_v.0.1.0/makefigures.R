###input drug name------------
library(dplyr)
library(stringr)
library(parallel)
library(epitools)
library(forestploter)
library(ggplot2)
library(grid)
library(ggrepel)
library(viridis)
library(EBImage)

load("WWW/data/6keydata_prof_v2.RData")
########################################################################
#                    data processing functions                         #
########################################################################

#extract all cases according to drugs func.1------------------------
finddrugreacs<-function(drugs=NULL){
  targetdrugdata<-indexdata_prof[indexdata_prof$prod_ai%in%drugs,]
  
  #target drug's all effect and non target drug's all effect
  tdalle<-allreac_prof[allreac_prof$primaryid%in%targetdrugdata$primaryid,]
  ntdalle<-allreac_prof[!allreac_prof$primaryid%in%targetdrugdata$primaryid,]
  return(list(tdalle=tdalle,ntdalle=ntdalle))
}

#extract all cases according to drugs func.2------------------------
extractcases<-function(d1=drugs1,d2="OTHER DRUGS"){
  
  if(is.null(d1)){stop("Please select drug for treatment group")}
  
  tdalle_ntdalle<-finddrugreacs(drugs=d1)
  d1alle<-tdalle_ntdalle$tdalle
  if(d2[1]=="OTHER DRUGS"){d2alle<-tdalle_ntdalle$ntdalle}else{
    d2alle<-finddrugreacs(drugs=d2)
    d2alle<-d2alle$tdalle
  }
  return(list(d1alle=d1alle,d2alle=d2alle))
}

# group according to age

groupbyage<-function(drugreaction=NULL,age_unit=NULL,age=NULL){
  calage<-function(age=NULL,age_unit=NULL){
    if(age_unit=="year"){age=age*360}
    if(age_unit=="day"){age=age}
    if(age_unit=="hour"){age=age/24}
    age
  }
  agecaled=calage(age=age,age_unit=age_unit)
  drugreaction$d1alle$age<-demos_prof$age_day[match(drugreaction$d1alle$primaryid,demos_prof$primaryid)]
  drugreaction$d2alle$age<-demos_prof$age_day[match(drugreaction$d2alle$primaryid,demos_prof$primaryid)]
  dr1d1c<-drugreaction$d1alle[which(drugreaction$d1alle$age>=agecaled),]
  dr1d2c<-drugreaction$d1alle[which(drugreaction$d1alle$age<agecaled),]
  dr2d1c<-drugreaction$d2alle[which(drugreaction$d2alle$age>=agecaled),]
  dr2d2c<-drugreaction$d2alle[which(drugreaction$d2alle$age<agecaled),]
  return(list(dr1d1c=dr1d1c,dr1d2c=dr1d2c,dr2d1c=dr2d1c,dr2d2c=dr2d2c))
}

groupbygender<-function(drugreaction=NULL){
  drugreaction$d1alle$sex<-demos_prof$sex[match(drugreaction$d1alle$primaryid,demos_prof$primaryid)]
  drugreaction$d2alle$sex<-demos_prof$sex[match(drugreaction$d2alle$primaryid,demos_prof$primaryid)]
  dr1d1c<-drugreaction$d1alle[which(drugreaction$d1alle$sex=="M"),]
  dr1d2c<-drugreaction$d1alle[which(drugreaction$d1alle$sex=="F"),]
  dr2d1c<-drugreaction$d2alle[which(drugreaction$d2alle$sex=="M"),]
  dr2d2c<-drugreaction$d2alle[which(drugreaction$d2alle$sex=="F"),]
  return(list(dr1d1c=dr1d1c,dr1d2c=dr1d2c,dr2d1c=dr2d1c,dr2d2c=dr2d2c))
}

groupbynon<-function(drugreaction=NULL){
  dr1d1c<-drugreaction$d1alle
  dr1d2c<-drugreaction$d1alle
  dr2d1c<-drugreaction$d2alle
  dr2d2c<-drugreaction$d2alle
  return(list(dr1d1c=dr1d1c,dr1d2c=dr1d2c,dr2d1c=dr2d1c,dr2d2c=dr2d2c))
}
#calculate odd ratio--------------------------
calodd<-function(targetreac=NULL,drug1reactions=NULL,drug2reactions=NULL){
  
  res<-lapply(targetreac,function(x){
    td1te<-drug1reactions[drug1reactions$pt%in%x,1]
    td1nte<-drug1reactions[!drug1reactions$pt%in%x,1]
    td2te<-drug2reactions[drug2reactions$pt%in%x,1]
    td2nte<-drug2reactions[!drug2reactions$pt%in%x,1]
    mt<-matrix(c(length(td1te)%>%as.numeric(),length(td1nte)%>%as.numeric(),length(td2te)%>%as.numeric(),length(td2nte)%>%as.numeric()),nrow=2,byrow = T)
    colnames(mt)<-c("target_event","not_target_event")
    rownames(mt)<-c("target_drug","not_target_drug")
    df<-epitab(mt,method = "oddsratio")
    df<-df[[1]]%>%as.data.frame()
    df[2,which(df[2,]=="NaN")]=0
    df[2,which(df[2,]==Inf|df[2,]==-Inf)]=0
    df[2,which(is.na(df[2,]))]=0
    low=df$lower[2]%>%round(digits = 2)
    hi=df$upper[2]%>%round(digits = 2)
    est=df$oddsratio[2]%>%round(digits = 2)
    se=(log(hi) - log(est))/1.96
    se=ifelse(se!="NaN",se,0)
    #cal or
    or<-paste(est," (",low," to ",hi,")",sep="")
    #output
    oddres<-data.frame(Reaction=x,Drug_of_interest=paste(length(td1te)," / ",length(td1nte),sep = ""),Other_Drug=paste(length(td2te)," / ",length(td2nte),sep = ""),low=low,high=hi,est=est,se=se,removethis=" ",OR=or)
  })
  res<-do.call(rbind,res)
  colnames(res)[8]<-"                                       "
  res
}

combagegroupres<-function(groupres=NULL,targetreac=NULL,age=NULL){
  reac1c<-calodd(targetreac=targetreac,drug1reactions=groupres$dr1d1c,drug2reactions=groupres$dr2d1c)
  reac2c<-calodd(targetreac=targetreac,drug1reactions=groupres$dr1d2c,drug2reactions=groupres$dr2d2c)
  # install.packages("epitools",dependencies = T)
  dfall<-data.frame()
  for (i in 1:nrow(reac1c)){
    dfn<-rbind(reac1c[i,],reac1c[i,],reac2c[i,])
    dfn[1,2:9]<-""
    dfn[2,1]<-paste("   ","age","≥",age)
    dfn[3,1]<-paste("   ","age","<",age)
    dfall<-rbind(dfall,dfn)
  }
  dfall
}

combgendergroupres<-function(groupres=NULL,targetreac=NULL){
  reac1c<-calodd(targetreac=targetreac,drug1reactions=groupres$dr1d1c,drug2reactions=groupres$dr2d1c)
  reac2c<-calodd(targetreac=targetreac,drug1reactions=groupres$dr1d2c,drug2reactions=groupres$dr2d2c)
  # install.packages("epitools",dependencies = T)
  dfall<-data.frame()
  for (i in 1:nrow(reac1c)){
    dfn<-rbind(reac1c[i,],reac1c[i,],reac2c[i,])
    dfn[1,2:9]<-""
    dfn[2,1]<-paste("   ","Male")
    dfn[3,1]<-paste("   ","Female")
    dfall<-rbind(dfall,dfn)
  }
  dfall
}

combnongroupres<-function(groupres=NULL,targetreac=NULL){
  reac1c<-calodd(targetreac=targetreac,drug1reactions=groupres$dr1d1c,drug2reactions=groupres$dr2d1c)
  reac1c
}
########################################################################
#                      figure making functions                         #
########################################################################

# forestplot----------------------
cgres2plot<-function(dfall=NULL,druggroupname1=NULL,druggroupname2=NULL,USRID=NULL){
  colnames(dfall)[c(2,3,9)]<-c(paste(druggroupname1,"TR / not TR",sep = "\n"),paste(druggroupname2,"TR / not TR",sep = "\n"),"ROR (95% CI)")
  dfall$`ROR (95% CI)`[which(dfall$`ROR (95% CI)`=="0 (0 to 0)")]<-"Not Available"
  tm <- forest_theme(base_size = 13,
                     title_just = "center",
                     ci_pch = 18,
                     ci_col = "darkgreen",
                     ci_lwd = 2,
                     ci_fill = "black",
                     refline_gp  = gpar(col = "red", cex = 0.6),
                     arrow_type = "closed",
                     footnote_gp = gpar(col = "blue", cex = 0.6)
  )
  est = dfall$est%>%as.numeric()%>%log()
  est[is.infinite(est)] = NA
  lower = dfall$low%>%as.numeric()%>%log()
  lower[is.infinite(lower)] = NA
  upper = dfall$hi%>%as.numeric()%>%log()
  upper[is.infinite(upper)] = NA
  cmlower<-floor(min(lower[!is.na(lower)]))
  cmupper<-ceiling(max(upper[!is.na(lower)]))
  p<-forest(dfall[,c(1:3,8,9)],
         est = est,
         lower = lower, 
         upper = upper,
         sizes = 0.8,
         ci_column = 4,
         ref_line = 0,
         title = paste("Reaction Comparision for Drugs of Interest\n"),
         xlim = c(ifelse(cmlower<0,cmlower,-1), ifelse(cmupper>0,cmupper,1) ),
         xlab = "log2 (95% CI)",
         ticks_at = c(ifelse(cmlower<0,cmlower,-1) ,0, ifelse(cmupper>0,cmupper,1) ),
         footnote = "",
         theme = tm)
  ggsave(filename=paste("WWW/temp/",USRID,"_forest.png",sep=""), plot = p, width = 2500, height = (nrow(dfall)*80+400), units = "px", dpi = 180)

  resize_image(input_path=paste("WWW/temp/",USRID,"_forest.png",sep=""),output_path=paste("WWW/temp/",USRID,"_forest2.png",sep=""), fixed_width =1500)
  
  # Print plot
  return("plot forest")
}
# pie plot-----------------------------


tdte2pie<-function(tdte=NULL,druggroupname=NULL,USRID=NULL){
  # 创建示例数据
  df <- tdte$pt%>%table()%>%as.data.frame()
  colnames(df) <- c("Reaction", "Frequency")
  df<-df[order(df$Frequency,decreasing = T),]
  tmp<-ifelse(nrow(df)>15,df2<-df[1:15,],df2<-df) 
  
  df2$Reaction <- factor(df2$Reaction, levels = df2$Reaction)
  # 计算百分比
  df2$Percentage <- df2$Frequency / sum(df2$Frequency) * 100
  
  # 添加标签
  df2$Label <- paste0(round(df2$Percentage, 1), "%")
  df2$Frequency<-as.character(df2$Frequency)
  # 绘制饼图
  p<-ggplot(df2, aes(x = "", y = Percentage, fill = Reaction)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    theme_void() +  # 移除背景和坐标轴
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5)) +
    labs(title = paste("Reaction Distribution of",druggroupname)) +
    scale_fill_viridis_d(begin = 0.15,end = 0.85,option = "H")  # 设置颜色主题
  
    ggsave(filename=paste("WWW/temp/",USRID,"_pie.png",sep=""), plot = p, width =  2600, height = 1600, units = "px", dpi = 300)
  
  resize_image(input_path=paste("WWW/temp/",USRID,"_pie.png",sep=""),output_path=paste("WWW/temp/",USRID,"_pie2.png",sep=""), fixed_width =1000)
  
  return(list(df=df,df2=df2,p=p))  

  }

#resize image-------------------------
#缩放热图的功能
resize_image <- function(input_path="WWW/temp/temp.png",output_path="WWW/temp/temp2.png", fixed_width =800) {
  # 读取图像
  img <- readImage(input_path)
  original_dim<-dim(img)
  original_width <- original_dim[1]
  original_height <- original_dim[2]
  # 计算等比例缩放后的高度
  scale_factor <- fixed_width / original_width
  new_height <- original_height * scale_factor
  # 缩放图像 (默认使用双线性插值)
  resized_img <- resize(img, w = fixed_width, h = new_height)
  writeImage(resized_img,output_path)
}

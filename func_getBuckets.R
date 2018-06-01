getBuckets <- function(segmentdata,segments_distribution,allsegments,segments_bool){
  
  lkp1 <- data.frame();
  unq <- unique(segmentdata[,c("measurename","measurevalue")])
  for(i in 1:length(seggrp)){
    unqq <- NULL
    unqq <- unq[unq$measurename == seggrp[i],]
  
    ## Skipping segments which has no rows & NRX 
    if(nrow(unqq)<1 || seggrp[i] == "NRX"){
      cat(paste(i,"\n Either no rows or belongs to NRX, hence skipping for :", seggrp[i]));
      next();
    }
    if(seggrp[i] == "Age"){
     cat(paste("\n Applied Standard Method :", seggrp[i]));
      #cat(paste(seggrp[i]));
      unqq$measurevalue <- as.numeric(unqq$measurevalue) 
      unqq<-unqq[order(unqq$measurevalue),]
      unqq$cum_perc <- 0
      unqq$cat <- ifelse(unqq$measurevalue <= 39 & unqq$measurevalue >=25,"25-39",
                         ifelse(unqq$measurevalue <= 49 & unqq$measurevalue >=40,"40-49",
                         ifelse(unqq$measurevalue <= 59 & unqq$measurevalue >=50,"50-59",
                         ifelse(unqq$measurevalue <= 69 & unqq$measurevalue >=60,"60-69",
                         ifelse(unqq$measurevalue >=70,"70+",
                         ifelse(is.na(unqq$measurevalue) | is.null(unqq$measurevalue)| unqq$measurevalue == "",
                                paste0(seggrp[i],"_NULL"),paste0(seggrp[i],"_NULL")))))))
      unqq$measurevalue <- as.character(unqq$measurevalue)
      
      }else if(seggrp[i] %in% segments_distribution){
        cat(paste("\n Applied Distribution Method :", seggrp[i]));
        unqq$measurevalue <- as.numeric(unqq$measurevalue) 
        unqq<-unqq[order(unqq$measurevalue),]
        seg_data <- segmentdata[segmentdata$measurename == seggrp[i],]
        unqq$cum_perc <- round((cumsum(table(as.numeric(seg_data$measurevalue)))/nrow(seg_data))*100,2)
        unqq$cat <- ifelse(unqq$cum_perc <= 33,"low",ifelse(unqq$cum_perc <= 67 & unqq$cum_perc > 33,"Medium","High"))
        unqq[unqq$measurevalue == "NA" | unqq$measurevalue == "" | is.na(unqq$measurevalue) | is.null(unqq$measurevalue),"measurevalue"] <- paste0(seggrp[i],"_NULL")
        unqq$measurevalue <- as.character(unqq$measurevalue) 
        
      }else if(seggrp[i] %in% c( "engagement","impression")){
        cat(paste("\n Applied Standard Method :", seggrp[i]));
        #cat(paste(seggrp[i]));
        unqq$measurevalue <- as.numeric(unqq$measurevalue) 
        unqq<-unqq[order(unqq$measurevalue),]
        unqq$cum_perc <- 0
        unqq$cat <- ifelse(unqq$measurevalue <= 5 & unqq$measurevalue >=1,"1 to 5",
                    ifelse(unqq$measurevalue <= 10 & unqq$measurevalue >=6,"6 to 10",
                    ifelse(unqq$measurevalue <= 15 & unqq$measurevalue >=11,"11 to 15",
                    # ifelse(unqq$measurevalue <= 70 & unqq$measurevalue >=61,"61 to 70",
                    # ifelse(unqq$measurevalue <= 80 & unqq$measurevalue >=71,"71 to 80",
                    # ifelse(unqq$measurevalue <= 90 & unqq$measurevalue >=81,"81 to 90",
                    # ifelse(unqq$measurevalue <= 100 & unqq$measurevalue >=91,"91 to 100",
                    ifelse(unqq$measurevalue >= 15,"15+",
                    ifelse(unqq$measurevalue == "NA" | is.na(unqq$measurevalue) | is.null(unqq$measurevalue) | unqq$measurevalue == "", "None", "None")))))
                    unqq$measurevalue <- as.character(unqq$measurevalue)
        
      }else if (seggrp[i] %in% segments_bool){
        cat(paste0("\n Applied Yes/No Method : ",seggrp[i]))
        unqq$measurevalue <- as.character(unqq$measurevalue)
        unqq$cum_perc <- 0
        unqq$cat <- ifelse(unqq$measurevalue == "1","Yes",ifelse(unqq$measurevalue == "0","No",paste0(seggrp[i],"_NULL")))
        }else{
          cat(paste0("\n No Method Applied : ",seggrp[i]))
          unqq<-unqq[order(unqq$measurevalue),]
          unqq[unqq$measurevalue == "NA" | unqq$measurevalue == "" | is.na(unqq$measurevalue)| is.null(unqq$measurevalue),"measurevalue"] <- paste0(seggrp[i],"_NULL")
          unqq$cum_perc <- 0
          unqq$cat <- unqq$measurevalue
      }
    if(nrow(lkp1) >0){
      lkp1 <- bind_rows(lkp1,unqq)
    }else{
      lkp1 <- unqq
    }
  }
  #cat(paste(i,"Finished :", seggrp[i]));
 return(lkp1)
}

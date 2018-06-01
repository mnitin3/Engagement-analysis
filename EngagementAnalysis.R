#########################################
### Engagement Analysis - Automation ####
#########################################
#### Developed by: Nitin Agarwal ######## 
#########################################

#### clean workspace #####
rm(list = ls(all = TRUE)) 
cat("\014")

library(dplyr)
library(reshape)
library(reshape2)
source("~/Eng_Anal_Automation/func_getBuckets.R")

start_time <- Sys.time()
cat("\014")
today <- format(Sys.Date(), format = "%m-%d-%Y")

##### Variables and user Inputs ######
## Please input folder path in the below function:
sourcepath <-  
brandname <-  
campaignname <-  
campaignStartDate <- 201706
campaignEndDate   <- 201803


cat(paste("starting Engagement Analysis for Brand:",brandname,"Campaign: ",campaignname,"from ",campaignStartDate,
          " to ",campaignEndDate))

ddTargetList      <- read.csv(paste0(sourcepath, "/Target_List.csv"), stringsAsFactors = F)
ddChannel         <- read.csv(paste0(sourcepath, "/Channels_Data.csv"), stringsAsFactors = F)
ddSegment         <- read.csv(paste0(sourcepath, "/Segments.csv"), stringsAsFactors = F)

names(ddChannel)  <- tolower(names(ddChannel))
names(ddSegment)  <- tolower(names(ddSegment))
names(ddTargetList) <- tolower(names(ddTargetList))

ddChannel$testcontrolflag <- 'T'

##### Data import and selection ######
ddChannelSel <- ddChannel %>% filter(ddChannel$testcontrolflag == 'T') %>% select( campaignid, campaignname,
                                                                                   clienthcpid,channelname,vendorname,
                                                                                   tacticid,tacticstartdate,unit_type,
                                                                                   testcontrolflag,actualstartdate,multiplier,
                                                                                   name)
if(nrow(ddChannelSel)>0){
  cat(paste("\n Channels file loaded with",nrow(ddChannelSel),"rows."))
}

ddSegmentSel <- ddSegment  %>% filter(ddSegment$testcontrolflag == 'T') %>% select(clienthcpid = cid,testcontrolflag,
                                                                                   measurename,yearmonth,measurevalue,
                                                                                   extractiondate)

if(nrow(ddSegmentSel)>0){
  cat(paste("\n Segments file loaded with",nrow(ddSegmentSel),"rows."))
}


ddTargetListSel <- ddTargetList %>% filter(ddTargetList$testcontrolflag == 'T') %>%  select( clienthcpid = clientid,
                                                                                             specialty,state,professionaldesignation,
                                                                                             testcontrolflag,brandactiongroup,
                                                                                             specialty.grouping = specialtygrouping,
                                                                                             #writergroup = writer_group_2,
                                                                                             seestatus = seestatus)

if(nrow(ddTargetListSel)>0){
  cat(paste("\n Target file loaded with",nrow(ddTargetListSel),"rows."))
}


ddSegmentSel$clienthcpid <- as.integer(ddSegmentSel$clienthcpid)
ddChannel <- ddChannel[!ddChannel$channelname == "None",]
unique_hcps <- unique(ddTargetListSel$clienthcpid)

### ***** Filtered NRX Segment as of now , as we are not performing engagement analysis for NRX,TRX segment *****
ddSegmentSel <- ddSegmentSel[ddSegmentSel$measurename != "NRX", ]
ddSegmentSel <- ddSegmentSel[ddSegmentSel$measurename != "TRX", ]
ddSegmentSel <- filter(ddSegmentSel, ddSegmentSel$clienthcpid %in% unique_hcps)


### Creating segments from Impressions/Engagements ##
channelgrp <- ddChannelSel %>% group_by(clienthcpid,channelname,vendorname)
suppressMessages(cross_all <- dcast(channelgrp, clienthcpid ~ unit_type, fun.aggregate = length))
suppressMessages(cross_channel_vendor <- dcast(channelgrp, clienthcpid + channelname + vendorname ~ unit_type, fun.aggregate = length))

names(cross_all) <- tolower(names(cross_all))
names(cross_channel_vendor) <- tolower(names(cross_channel_vendor))

cat("\n\n\nCollating segments Using Engagements and Impressions: \n")
segImpEng <- tolower(c("ENGAGEMENT","IMPRESSION"))

for (c in 1:length(segImpEng)) {
  cat(paste0("\n  -",segImpEng[c]))
  if ((segImpEng[c] %in% colnames(cross_all))) {
    ddSegmentSel <- bind_rows(ddSegmentSel,data.frame(clienthcpid = as.integer(cross_all$clienthcpid),
                                                      testcontrolflag = 'T',
                                                      measurename = as.character(segImpEng[c]),yearmonth = 1L,
                                                      measurevalue = as.character(cross_all[,segImpEng[c]]),
                                                      extractiondate = as.character(01162018)))
  } else{
    next();
  }
}


##### Create universal list - horizontal ########
segTargetlist <- tolower(c("Specialty","state","professionaldesignation","brandactiongroup","specialty.grouping",
                           "writergroup","seestatus"))

cat("\n\n\nCollating segments from Target List: \n")
for (t in 1:length(segTargetlist)) {
  cat(paste0("\n  -",segTargetlist[t]))
  if ((segTargetlist[t] %in% colnames(ddTargetListSel))) {
    ddSegmentSel <- bind_rows(ddSegmentSel,data.frame(clienthcpid = as.integer(ddTargetListSel$clienthcpid),
                                                      testcontrolflag = as.character(ddTargetListSel[, "testcontrolflag"]),
                                                      measurename = as.character(segTargetlist[t]),yearmonth = 1L,
                                                      measurevalue = as.character(ddTargetListSel[, segTargetlist[t]]),
                                                      extractiondate = as.character(01162018)))
  } else{
    next();
  }
}

segGrpList <- as.character(unique(ddSegmentSel$measurename))

#segGrpList
segPrePost <- c('Details', 'WSample', 'MSample', 'FSample')

## Creating Pre, Post Campaign Flags
for (i in 1:length(segPrePost)) {
   
  segmentName <- segPrePost[i]
  prelen  <- length(ddSegmentSel[ddSegmentSel$yearmonth < campaignStartDate & ddSegmentSel$measurename == segmentName, ]$measurename)
  postlen <- length(ddSegmentSel[ddSegmentSel$yearmonth >= campaignEndDate & ddSegmentSel$measurename == segmentName, ]$measurename)
  
  if (prelen > 0) {
    ddSegmentSel[ddSegmentSel$yearmonth < campaignStartDate & ddSegmentSel$measurename == segmentName, ]$measurename <- 
      as.character(paste0(segmentName, "-pre"))
  }
  if (postlen > 0) {
    ddSegmentSel[ddSegmentSel$yearmonth >= campaignEndDate & ddSegmentSel$measurename == segmentName, ]$measurename <- 
      as.character(paste0(segmentName, "-post"))
  }
}
seggrp <- unique(ddSegmentSel$measurename)

## Aggregating Sample data ##
segpp <- c('Details', 'WSample', 'MSample', 'FSample',
           'Details-pre', 'WSample-pre', 'MSample-pre', 'FSample-pre',
           'Details-post', 'WSample-post', 'MSample-post', 'FSample-post')

ff <- ddSegmentSel[ddSegmentSel$measurename %in% segpp,]
ff$yearmonth <- 1
ff$measurevalue <- as.numeric(ff$measurevalue)
ffgrp <-ff %>% group_by(clienthcpid, testcontrolflag,measurename, yearmonth) %>% summarise(measurevalue = sum(measurevalue))
ffgrp.df <- as.data.frame(ffgrp)
ffgrp.df$extractiondate <- "2018-05-07 08:36:56.203"
ffgrp.df$measurevalue <- as.character(ffgrp.df$measurevalue)

ddSegmentSel <- rbind(ddSegmentSel[!(ddSegmentSel$measurename %in% segpp),],ffgrp.df)


## Also Added TRX segment in distribution bucketing method we did not had clarity of bucket method for this segment.
segments_distribution <- c(segPrePost, as.character(paste0(segPrePost, "-pre")), as.character(paste0(segPrePost, "-post")))

#### yes/No segment Group
segments_bool <- c("Past DM Engager", "Past TS Engager", "Past Alerts Engager", "Past Email Engager")

######### bucketing ##############
cat("\n\n")
bucketlkp <- getBuckets(ddSegmentSel, segments_distribution, seggrp, segments_bool)
write.csv(bucketlkp, paste0(sourcepath, "/Bucket_lookup.csv"), row.names = FALSE)
cat(paste0( "\n\n Bucket lookup file has been placed at:", sourcepath, "/Bucket_lookup.csv \n"))

## Given time to users to update Bucket lookup file.
q1 <- readline(prompt = "Have you updated the lookup file and saved it? (Y/N): ")
if (q1 == 'Y' | q1 == 'y') {
  bucket_completed <- T
} else {
  bucket_completed <- F
}

#cat(paste("\n Applying buckets on segments data..."))

updated_buckets <- read.csv(paste0(sourcepath, "/Bucket_lookup.csv"))

## Replacing NULL categories to append '_NULL' with the category
if (length(ddSegmentSel[ddSegmentSel$measurevalue == "NA" | ddSegmentSel$measurevalue == "" | is.na(ddSegmentSel$measurevalue) | is.null(ddSegmentSel$measurevalue) , ]$measurevalue) > 0) {
  ddSegmentSel[ddSegmentSel$measurevalue == "NA" | ddSegmentSel$measurevalue == "" | is.na(ddSegmentSel$measurevalue)| is.null(ddSegmentSel$measurevalue) , ]$measurevalue  <- 
    paste0(ddSegmentSel[ddSegmentSel$measurevalue == "NA" | ddSegmentSel$measurevalue == "" | is.na(ddSegmentSel$measurevalue)| is.null(ddSegmentSel$measurevalue) , ]$measurename, "_NULL")
}

#applyting the buckets prepared
ddSegmentSel <- left_join(ddSegmentSel, updated_buckets, by = c("measurename", "measurevalue"))

# writing to the bucket lookup files.
cat(paste("\n Applied buckets on segments data... \n"))

write.csv(ddSegmentSel, paste0(sourcepath, "/Bucket_lookup_joined.csv"), row.names = FALSE)
segGrpList <- as.character(unique(ddSegmentSel$measurename))
names(ddSegmentSel) <- c( 'clienthcpid', 'testcontrolflag', 'measurename', 'yearmonth', 'measurevalue_origninal', 
                          'extractiondate', 'cum_perc', 'measurevalue')

############# Group of Segments #################
# Take input from the user either by csv or console
combinedSpecialties <- c('Age|Gender')
segGrpList <- c(segGrpList, combinedSpecialties)


######### Create universal list - Vertical ######
chaList <- as.character(unique(ddChannelSel$channelname))
venList <- as.character(unique(ddChannelSel$vendorname))
chavencombine <- paste(ddChannelSel$channelname, ddChannelSel$vendorname, sep = "_")
chavenList <- as.character(unique(chavencombine))

## Manually add additional channel-vendor combinations seperated by |(pipe)
additionalList <- as.character("Overall")
chaList <- c(additionalList, chaList)


#### Frame Initializations ######
cha_loop          <- data.frame()
cha_com           <- data.frame()
seg_chan_loop     <- data.frame()
seg_chan_complete <- data.frame()

############## Engagement Analysis ##############
cat(paste("\n\n Starting KPI Calculations for: \n"))

for (i in 1:length(segGrpList)) {
  sp <- strsplit(segGrpList[i], split = '|', fixed = T)
  if (lengths(sp) == 1) {
    cat(paste0("\n single specialty:", segGrpList[i]))
    segFil <- filter(ddSegmentSel, ddSegmentSel$measurename == sp[[1]][1])
  } else if (lengths(sp) == 2) {
    cat(paste0("\n Multiple specialty:", segGrpList[i]))
    segFil <- ddSegmentSel %>% filter(measurename == sp[[1]][1] | measurename == sp[[1]][2])
    segFilTranspose <- dcast(segFil, clienthcpid ~ measurename)
    measures <- unique(segFil$measurename)
    segFilTranspose$measurevalue <- paste(segFilTranspose[, measures[1]], '|', segFilTranspose[, measures[2]])
    segFilTranspose$measurename <- paste(measures, collapse = '|')
    segFil <- segFilTranspose
  }
  
  target_hcps <- as.data.frame(ddTargetListSel$clienthcpid)
  names(target_hcps) <- c("clienthcpid")
  segFil <- left_join(target_hcps, segFil, by = "clienthcpid")
  
  ## Calculating overall Total HCP and Not_Reached as per segment only.
  channelFil <- ddChannelSel %>% select(clienthcpid, unit_type)
  suppressMessages(chan_cross <- dcast(channelFil, clienthcpid ~ unit_type, fun.aggregate = length))
  dd_kpi <- left_join(segFil, chan_cross, by = c("clienthcpid")) %>% select("clienthcpid", "ENGAGEMENT", "IMPRESSION",
                                                                            "measurename", "measurevalue")
  dd <- dd_kpi %>% group_by(measurename, measurevalue)
  dd[is.na(dd$ENGAGEMENT), "ENGAGEMENT"] <- 0
  dd[is.na(dd$IMPRESSION), "IMPRESSION"] <- 0
  total_hcp <- dd %>% summarise(total_hcp = n())
  not_reached <- dd %>% filter(IMPRESSION < 1) %>% dplyr::summarise(not_reached = n())
  
  ## Processing for Channel level
  cha_com <- data.frame()
  for (j in 1:length(chaList)) {
    cha_loop <- data.frame()
    cat(paste0("\n  -" , chaList[j]))
    if (chaList[j] == "Overall") {
      channelFil <- ddChannelSel %>% select(clienthcpid, unit_type)
    } else{
      channelFil <- ddChannelSel %>% filter(channelname == chaList[j]) %>% select(clienthcpid, unit_type)
    }
    suppressMessages(chan_cross <- dcast(channelFil, clienthcpid ~ unit_type, fun.aggregate = length))
    if (chaList[j] != "Print") { 
      if (!("ENGAGEMENT" %in% names(chan_cross))) {
        chan_cross$ENGAGEMENT <- 0
      }
      dd_kpi <- left_join(segFil, chan_cross, by = c("clienthcpid")) %>% select("clienthcpid", "ENGAGEMENT", "IMPRESSION",
                                                                                "measurename", "measurevalue")
      dd <- dd_kpi %>% group_by(measurename, measurevalue)
      dd[is.na(dd$ENGAGEMENT), "ENGAGEMENT"] <- 0
      dd[is.na(dd$IMPRESSION), "IMPRESSION"] <- 0
      
      dd2 <- dd
      dd2$Channel <- chaList[j]
      unique_imp  <-  filter(dd, IMPRESSION > 0) %>% dplyr::summarise(unique_imp  = n())
      total_imp   <-  dplyr::summarise(dd, total_imp  = sum(IMPRESSION))
      unique_eng  <-  filter(dd, ENGAGEMENT > 0) %>% dplyr::summarise(unique_eng  = n())
      total_eng   <-  dplyr::summarise(dd, total_eng  = sum(ENGAGEMENT))
      plus3_eng   <-  filter(dd, ENGAGEMENT > 2) %>% dplyr::summarise(plus3_eng = n())
      ll <- left_join(total_hcp, not_reached, by = c("measurename", "measurevalue"))
      ll <- left_join(ll, unique_imp , by = c("measurename", "measurevalue"))
      ll <- left_join(ll, total_imp, by = c("measurename", "measurevalue"))
      ll <- left_join(ll, unique_eng , by = c("measurename", "measurevalue"))
      ll <- left_join(ll, total_eng , by = c("measurename", "measurevalue"))
      ll <- left_join(ll, plus3_eng , by = c("measurename", "measurevalue"))
      
      cc <- ll %>% dplyr::mutate( perc_reached = round(unique_imp   / total_hcp, digits = 3),
                                  unique_eng_rate = round(unique_eng   / unique_imp , digits = 3),
                                  plus3_eng_rate = round(plus3_eng  / unique_imp, digits = 3),
                                  avg_impressions = round(total_imp / unique_imp , digits = 3),
                                  gross_eng_rate = round(total_eng   / total_imp , digits = 3),
                                  unique_eng_rate = round(unique_eng   / unique_imp, digits = 3), 
                                  unique_eng_rate2 = round(unique_eng   / total_hcp, digits = 3))
      
      if (sum(is.na(cc$measurename))) {
        cc[is.na(cc$measurename), ]$measurename <- as.character(unique(cc$measurename)[1])
      }
      cc$Channel <- chaList[j]
      cha_loop <- cc
    }
    
    if(ncol(cha_com) > 0) {
      cha_com <- bind_rows(cha_com, cha_loop)
    } else{
      cha_com <- cha_loop
    }
  }
  seg_chan_loop <- cha_com
  
  if (ncol(seg_chan_complete) > 0) {
    seg_chan_complete <- bind_rows(seg_chan_complete, seg_chan_loop)
  } else{
    seg_chan_complete <- seg_chan_loop
  }
  
  ## Processing for Channel-vendor level
  cha_com <- data.frame()
  for(k in 1:length(chavenList)) {
    cha_loop <- data.frame()
    cat(paste0("\n  -" , chavenList[k]))
    channelFil <- ddChannelSel %>% filter(paste(channelname, vendorname, sep = "_") == chavenList[k]) %>% select(clienthcpid, 
                                                                                                                 unit_type)
    suppressMessages(chan_cross <- dcast(channelFil, clienthcpid ~ unit_type, fun.aggregate = length))
    
    if (!("ENGAGEMENT" %in% names(chan_cross))) {
      chan_cross$ENGAGEMENT <- 0
    }
    dd_kpi <- left_join(segFil, chan_cross,   by = c("clienthcpid")) %>% select("clienthcpid", "ENGAGEMENT", "IMPRESSION",
                                                                                "measurename", "measurevalue")
    
    dd <- dd_kpi %>% group_by(measurename, measurevalue)
    dd[is.na(dd$ENGAGEMENT), "ENGAGEMENT"] <- 0
    dd[is.na(dd$IMPRESSION), "IMPRESSION"] <- 0
    dd2 <- dd
    dd2$Channel <- chavenList[k]
    unique_imp <- filter(dd, IMPRESSION > 0) %>% dplyr::summarise(unique_imp  = n())
    total_imp <- dplyr::summarise(dd, total_imp  = sum(IMPRESSION))
    unique_eng <- filter(dd, ENGAGEMENT > 0) %>% dplyr::summarise(unique_eng  = n())
    total_eng <- dplyr::summarise(dd, total_eng  = sum(ENGAGEMENT))
    plus3_eng <- filter(dd, ENGAGEMENT > 2) %>% dplyr::summarise(plus3_eng = n())
    ll <- left_join(total_hcp, not_reached, by = c("measurename", "measurevalue"))
    ll <- left_join(ll, unique_imp , by = c("measurename", "measurevalue"))
    ll <- left_join(ll, total_imp, by = c("measurename", "measurevalue"))
    ll <- left_join(ll, unique_eng , by = c("measurename", "measurevalue"))
    ll <- left_join(ll, total_eng , by = c("measurename", "measurevalue"))
    ll <- left_join(ll, plus3_eng , by = c("measurename", "measurevalue"))
    
    cc <- ll %>% dplyr::mutate(perc_reached = round(unique_imp   / total_hcp, digits = 3),
                               unique_eng_rate = round(unique_eng   / unique_imp , digits = 3), 
                               plus3_eng_rate = round(plus3_eng  / unique_imp, digits = 3),
                               avg_impressions = round(total_imp / unique_imp , digits = 3),
                               gross_eng_rate = round(total_eng   / total_imp , digits = 3),
                               unique_eng_rate = round(unique_eng   / unique_imp, digits = 3),
                               unique_eng_rate2 = round(unique_eng   / total_hcp, digits = 3))
    
    if (sum(is.na(cc$measurename))) {
      cc[is.na(cc$measurename), ]$measurename <- as.character(unique(cc$measurename)[1])
    }
    cc$Channel <- chavenList[k]
    cha_loop <- cc
    ##
    
    if (ncol(cha_com) > 0) {
      cha_com <- bind_rows(cha_com, cha_loop)
    } else{
      cha_com <- cha_loop
    }
  }
  seg_chan_loop <- cha_com
  
  if (ncol(seg_chan_complete) > 0) {
    seg_chan_complete <- bind_rows(seg_chan_complete, seg_chan_loop)
  } else{
    seg_chan_complete <- seg_chan_loop
  }
}

ss <- seg_chan_complete %>% select("measurename","measurevalue","total_hcp","not_reached","unique_imp", "total_imp",
                                   "perc_reached","avg_impressions", "unique_eng","total_eng","plus3_eng", 
                                   "unique_eng_rate","plus3_eng_rate", "gross_eng_rate","unique_eng_rate2", "Channel")

names(ss) <- c("measurename","measurevalue","Total HCP","Not Reached","Unique impressions", "Total Impressions",
               "% Reached","Average Impressions", "Unique Engagements","Total Engagements","3+ Engagements", 
               "Unique Engagement Rate", "3+ Engagement Rate", "Gross Engagement Rate","Unique Engagement Rate 2", 
               "Channel")

ss_pivot = reshape2:::melt.data.frame(ss,id.vars=c(1,2,16),measured.vars=c(3:15))
ss_pivot_cast <- cast(ss_pivot, measurename+measurevalue ~ Channel+variable, sum)
write.csv(ss_pivot_cast,paste0(sourcepath, "/", brandname, "-", campaignname, "-", today, ".csv"), row.names = FALSE)
write.csv(ss_pivot,paste0(sourcepath, "/", brandname, "-", campaignname, "-", today, "_Melted.csv"), row.names = FALSE)
cat(paste0("\n\n total time taken:", Sys.time() - start_time))
cat("\n\n---------------- Engagement Analysis has been compelted -------------------------")

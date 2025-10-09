library(readr)
library(gmodels)
library(ggplot2)
library(cowplot)
library(dplyr)

setwd('/Users/matsuuratakeru/Library/Mobile Documents/com~apple~CloudDocs/T-Matsuura_LDLT')
path <- getwd()

"&" <- function(e1, e2) {
  if (is.character(c(e1, e2))) {
    paste(e1, e2, sep = "")
  } else {
    base::"&"(e1, e2) 
  }
}


df_lossnonloss<- read.csv(''&path&'/Data/Loss_nonloss_compare_R_revise.csv', na.strings = c(" ", "NA"))
df_lossnonloss
#Index Marking Setup
scientific_notation <- function(x) {
  x <- ifelse(x == 0, "0", format(x, scientific = TRUE)) 
  x <- gsub("^(.*)e", "'\\1'e", x)
  x <- gsub("e", "%*%10^", x)
  x <- gsub('\\+', '', x)
  parse(text = x)
}
# Plot function
violinPertwo <- function(data,cluster_type="graftloss_within_6months",value='value',label_text1,font_size1,y_range,title,ylabel,cols,filename){
  

  plt_dur <- ggplot(data=data_use,aes(x=as.factor(cluster_type), y=value))+
    geom_violin(aes(alpha=0.5, fill=as.factor(cluster_type)))+# violin plot
    geom_boxplot(width = .1, fill ="white")+#Box plot
    geom_text(x = 1.5, y = y_range[2]+distance1*diff(y_range), label = label_text1,size = font_size1) +
    geom_segment(x = 1, xend = 1, y = y_range[2]+0.02*diff(y_range), yend = y_range[2]+0.005*diff(y_range)) +
    geom_segment(x = 1, xend = 2, y = y_range[2]+0.02*diff(y_range), yend = y_range[2]+0.02*diff(y_range)) +
    geom_segment(x = 2, xend = 2, y = y_range[2]+0.02*diff(y_range), yend = y_range[2]+0.005*diff(y_range))+
    # 正常範囲の上下限に横線を追加
    geom_hline(yintercept = normal_min, linetype = "dashed", color = "blue") +
    geom_hline(yintercept = normal_max, linetype = "dashed", color = "blue") +
    stat_summary(fun = "mean", geom = "point", shape = 23, size = 1.5, alpha=1, fill = "black") + #Mean
    scale_fill_manual(values = cols) +# Color
    scale_y_continuous(limits = c(y_range[1], y_range[2]+y_range[2]/25),breaks = scales::breaks_pretty(5),labels = if (i <= 2 ) scientific_notation else scales::label_number()) +
    xlab(xlabel)+ylab(ylabel)+ggtitle(title)+scale_x_discrete(labels = c('True' = "Early loss",'False' = "Others"))+#,labels = scientific_notation
    theme(axis.text = element_text(colour = "black",size = 24,face="plain"),
          axis.title = element_text(colour = "black",size = 24,face="plain"),
          plot.title = element_text(colour = "black",size = 28,face="plain"),
          legend.text = element_text(size=16,face='plain'),
          legend.title = element_text(size=20,colour = 'white'),
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),legend.position='none') 
  
  
  ggsave(plt_dur,file=filename,width=5,height=5,dpi=300)
  return(plt_dur)
}

colna <- c("ascite_30POD_"   ,  "ascite_14POD_", "T_BIL_14POD_",                   
           "PT_per14POD_" , "donor_max_TB" )
colsave <- c("ascites(30POD)"   ,  "ascites(14POD)", "T-BIL(14POD)",                   
           "PT(14POD)" , "D max T-BIL" )
colreal <- c("ascites (30POD)","ascites (14POD)","T-BIL (14POD)",
             "PT% (14POD)","D max T-BIL")
#Unit list
ylabel_l <- c("mL",'mL','mg/dL','%','mg/dL')
# 例: 正常範囲を定義するベクトル
normal_range_min <- c("ascite_30POD_" = 0, "ascite_14POD_" = 0, "T_BIL_14POD_" = 0.4,"PT_per14POD_"=70, "donor_max_TB"= 0.4)  # item名に対応
normal_range_max <- c("ascite_30POD_" = 0, "ascite_14POD_" = 0, "T_BIL_14POD_" = 1.5,"PT_per14POD_"=130 , "donor_max_TB" = 1.5 )
for(i in 1:length(colna)){
  a <- df_lossnonloss[which(df_lossnonloss$item == colna[i]),]
  a <- a[which(a$graftloss_within_6months == 'True'),]
  a <- a$value
  a
  b <- df_lossnonloss[which(df_lossnonloss$item == colna[i]),]
  b <- b[which(b$graftloss_within_6months == 'False'),]
  b <- b$value 
  b
  result <- wilcox.test(a, b,correct=FALSE)
  # Result
  if (result$p.value < 0.05) {
    cat("significant difference (* p < 0.05)")
    
    # significant difference
    data_use <- df_lossnonloss[which(df_lossnonloss$item == colna[i]),] 
    data_use <- transform(data_use,cluster_type= factor(graftloss_within_6months, levels = c('True','False')))
    data_use
    
    title<-""&colreal[i]&""
    xlabel<-""
    ylabel<-ylabel_l[i]
    
    filename = ''&path&'/Output/Fig1/Fig1F_'&colsave[i]&'_revise.pdf'
    cols <- c('#ccff00','#808080')#color
    y_range<-range(data_use$value)
    cluster_type="graftloss_within_6months"
    significance_level1 <- result$p.value
    label_text1 <- ifelse(significance_level1 < 0.001, "***", ifelse(significance_level1 < 0.01, "**", ifelse(significance_level1 < 0.05, "*", "N.S.")))
    font_size1 <- ifelse(significance_level1 < 0.001, 10, ifelse(significance_level1 < 0.01, 10,  ifelse(significance_level1 < 0.05, 10, 12)))
    distance1 <-  ifelse(significance_level1 < 0.001, 0.02, ifelse(significance_level1 < 0.01, 0.02,  ifelse(significance_level1 < 0.05, 0.02, 0.08)))
    # 今回の項目に対応した正常範囲を取得
    item_name <- colna[i]
    normal_min <- normal_range_min[item_name]
    normal_max <- normal_range_max[item_name]
    plt<- violinPertwo(data_use,cluster_type="graftloss_within_6months",value='value',label_text1,font_size1,y_range,title,ylabel,cols,filename)
    
    # 
  } else {
    data_use <- df_lossnonloss[which(df_lossnonloss$item == colna[i]),]
    data_use <- transform(data_use, cluster_type= factor(graftloss_within_6months, levels = c('False','True')))
    
    title<-""&colreal[i]&""
    xlabel<-""
    ylabel<-ylabel_l[i]
    filename =''&path&'/Output/Fig1/Fig1F_'&colsave[i]&'.pdf'
    cols <- c('#ccff00','#808080')#  Color
    y_range<-range(data_use$value)
    cluster_type="graftloss_within_6months"
    significance_level1 <- result$p.value
    label_text1 <- ifelse(significance_level1 < 0.001, "***", ifelse(significance_level1 < 0.01, "**", ifelse(significance_level1 < 0.05, "*", "N.S.")))
    font_size1 <- ifelse(significance_level1 < 0.001, 12, ifelse(significance_level1 < 0.01, 12,  ifelse(significance_level1 < 0.05, 12, 8)))
    distance1 <-  ifelse(significance_level1 < 0.001, 0.02, ifelse(significance_level1 < 0.01, 0.02,  ifelse(significance_level1 < 0.05, 0.02, 0.09)))
    # 今回の項目に対応した正常範囲を取得
    item_name <- colna[i]
    normal_min <- normal_range_min[item_name]
    normal_max <- normal_range_max[item_name]
    plt<- violinPertwo(data_use,cluster_type="graftloss_within_6months",value='value',label_text1,font_size1,y_range,title,ylabel,cols,filename)
    
    cat("No significant difference")
  }
}

#Categorical
df_lossnonloss_ca<- read.csv(''&path&'/Data/Loss_nonloss_compare_categorical_R_revise.csv', na.strings = c(" ", "NA"))

explanational <- colnames(df_lossnonloss_ca[1:2])
title_l <- c("precondition Home","Splenectomy")
#Plot function
test_plot<-function(n,p_values,data,objective,explanation,filename2,title){
  k =1
  cross <- data.frame(
    obj = data[, c(objective)],
    expl = data[,c(explanation)]
  )
  cross$expl <- as.factor(cross$expl)
  pltData <-  cross %>% 
    group_by(obj,expl) %>% 
    summarise(count = n()) %>%
    mutate(prop = round(count/sum(count),digits = 3))
  
  plt_dur<- ggplot(data = pltData ,aes(x=factor(obj, levels = c('True','False')),y = prop, fill =expl ,label = expl)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_fill_manual(values =c("#808080","#f4a460"))+scale_x_discrete(labels = c('True' = "Early loss",'False' = "Others"))+
   
    xlab('')+ylab("")+ggtitle(title)+
    
    theme(axis.text = element_text(colour = "black",size = 24,face="plain"),
          axis.title = element_text(colour = "black",size = 24,face="plain"),
          plot.title = element_text(colour = "black",size = 28,face="plain"),
          legend.text = element_text(size=16,face='plain'),
          legend.title = element_text(size=20,colour = 'white'),
          axis.ticks = element_line(colour = "black"),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank()
    )
  
  ttt<-1
  for (b in seq(1,n-1) ){
    for (c in seq(b+1,n)){ 
      print(k)
      yuui <- p_values[b,c]*choose(n, 2)
      
      if(yuui < 0.05){
        ttt<- ttt+1
      }else{
        ttt<- ttt
      }}}
  print('ttt')
  print(ttt)
  if (ttt < choose(n, 2)/3){
    #Balancing the Plot
    print('Significant difference is less than one-third')
    for (b in seq(1,n-1) ){
      for (c in seq(b+1,n)){ 
        
        significance_level <- p_values[b,c]*choose(n, 2)
        #label text and size
        label_text <- ifelse(significance_level < 0.001, "***", ifelse(significance_level < 0.01, "**", ifelse(significance_level < 0.05, "*", 'N.S.')))
        font_size <- ifelse(significance_level < 0.001, 12, ifelse(significance_level < 0.01, 12,  ifelse(significance_level < 0.05, 12, 6)))
        if (significance_level > 0.05){
          print('N.S.') 
          plt_dur <- plt_dur +geom_text(x = 0.5*(b+c), y =1+0.1*k+0.05, label = label_text,size = font_size) +
            geom_segment(x = b, xend = b, y = 1+0.1*k, yend =1+0.1*k+0.02) +
            geom_segment(x = b, xend = c, y =1+0.1*k+0.02, yend =1+0.1*k+0.02) +
            geom_segment(x = c, xend = c, y =1+0.1*k, yend =1+0.1*k+0.02)
        }else{ plt_dur <- plt_dur +geom_text(x = 0.5*(b+c), y =1+0.1*k+0.03, label = label_text,size = font_size) +
          geom_segment(x = b, xend = b, y = 1+0.1*k, yend =1+0.1*k+0.03) +
          geom_segment(x = b, xend = c, y =1+0.1*k+0.03, yend =1+0.1*k+0.03) +
          geom_segment(x = c, xend = c, y =1+0.1*k, yend =1+0.1*k+0.03)
        k = k +1
        }
      }}
    y__limit <- 1+0.1*k+0.03
  }else{
    #Balancing the Plot
    print('Significant difference is more than one-third')
    for (b in seq(1,n-1) ){
      for (c in seq(b+1,n)){ 
        
        significance_level <- p_values[b,c]*choose(n, 2)
        
        label_text <- ifelse(significance_level < 0.001, "***", ifelse(significance_level < 0.01, "**", ifelse(significance_level < 0.05, "*", 'N.S.')))
        font_size <- ifelse(significance_level < 0.001, 10, ifelse(significance_level < 0.01, 10,  ifelse(significance_level < 0.05, 10, 6)))
        if (significance_level > 0.05){
          print('N.S.')
          plt_dur <- plt_dur +geom_text(x = 0.5*(b+c), y =1+0.07*k+0.05, label = label_text,size = font_size) +
            geom_segment(x = b, xend = b, y = 1+0.05*k, yend =1+0.05*k+0.02) +
            geom_segment(x = b, xend = c, y =1+0.05*k+0.02, yend =1+0.05*k+0.02) +
            geom_segment(x = c, xend = c, y =1+0.05*k, yend =1+0.05*k+0.02)
        }else{ plt_dur <- plt_dur +geom_text(x = 0.5*(b+c), y =1+0.05*k+0.03, label = label_text,size = font_size) +
          geom_segment(x = b, xend = b, y = 1+0.05*k, yend =1+0.05*k+0.03) +
          geom_segment(x = b, xend = c, y =1+0.05*k+0.03, yend =1+0.05*k+0.03) +
          geom_segment(x = c, xend = c, y =1+0.05*k, yend =1+0.05*k+0.03)
        k = k +1
        }}
      y__limit <- 1+0.08*k+0.03
    }
  }
  plt_dur <- plt_dur+ ylim(0,y__limit)
  if(a == length(explanational)){
    legend <- get_legend(plt_dur)
    legend_plot <- ggdraw(legend)
    plt_dur <- plt_dur+ theme(legend.position = 'none')
    ggsave(''&path&'/Output/Fig1/Fig1F_legend.pdf', legend_plot, width=6,height=4,dpi=300)
  }else{
    plt_dur <- plt_dur+ theme(legend.position = 'none')
  }
  ggsave(plt_dur,file=filename2,width=5,height=5,dpi=300)
  return(plt_dur)
}

# Fiser test function
perform_test_each<-function(n,method,data,objective,explanation){
  cross <- data.frame(
    obj = data[, c(objective)],
    expl = data[,c(explanation)]
  )
  cross$expl <- as.factor(cross$expl)
  crossTable <- CrossTable(cross$obj, cross$expl, prop.chisq = FALSE)
  matrix_data <- as.table(crossTable$t)
  
  p_values <- matrix(NA, ncol = n, nrow = n)
  for (i in seq(1,n-1)) {
    for (j in (i + 1):n) {
      if (method == 1) {
        fisher_result <- fisher.test(matrix_data[c(i, j), ],simulate.p.value=TRUE)
        p_values[i, j] <- p_values[j, i] <- fisher_result$p.value*choose(n,2)
      }
    }
  }
  return(p_values)
}

for (a in 1:length(explanational)){
  #Number of groups to be compared
  n <- 2
  method <- 1
  objective <- colnames(df_lossnonloss_ca)[3]
  explanation <- explanational[[a]]
  p_values <- perform_test_each(n,method,df_lossnonloss_ca,objective,explanation)
  print(p_values)
  explanation2 <- explanational[[a]]
  title <- title_l[[a]]
  filename2<- ''&path&'/Output/Fig1/Fig1F_'&title_l[a]&'_revise.pdf'
  
  plts <- test_plot(n,p_values,df_lossnonloss_ca,objective,explanation,filename2,title)  
}


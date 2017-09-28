# Supporting functions

#' @title mdsplot
#' @description Creates MDS plot
#' @param dfr A dataframe with genes (variables) as column and samples as rows
#' @param mdata Metadata dataframe with rows as samples. Row names as sample names.
#' @param textlab Which variable to be used as text. The name must be in the column names of mdata.
#' @param textcol Which variable to be used to colour text. The name must be in the column names of mdata.
#' @param pointcol Which variable to be used to colour points. The name must be in the column names of mdata.
#' @returndata If TRUE, MDS data is returned, else ggplot2 plot object is returned.
#' 
mdsplot <- function(dfr=NULL,mdata=NULL,textlab="sample",textcol="condition",pointcol="condition",returndata=F)
{
  library(ggplot2)
  mat1 <- as.data.frame(cmdscale(dist(dfr,method="euclidean"),eig=F, k=2))
  mat1$sample <- row.names(mat1)
  
  rnames <- rownames(mdata)
  mdata <- as.data.frame(sapply(mdata,as.factor))
  mdata$sample <- rnames
  mat1 <- merge(mat1,mdata,by="sample")
  
  if(returndata) 
  {
    return(mat1)
  }else{
    p <- ggplot(data=mat1,aes(V1,V2))+
      geom_text_repel(aes_string(label=textlab,colour=textcol),size=4,force=2)+
      geom_point(aes_string(fill=pointcol),size=3,alpha=0.4,shape=21)+
      #scale_colour_manual(values=col_cond_dark)+
      labs(x="MDS 1",y="MDS 2")+
      theme_bw(base_size=17)+
      theme(plot.title = element_text(lineheight=1.2,hjust=0,colour="grey40",face="bold"),
            plot.caption=element_text(colour="grey40",hjust=0),
            legend.position="top",
            legend.justification="right",
            legend.direction="horizontal",
            legend.key.size=unit(0.3,"cm"),
            legend.title=element_text(colour="grey40",size=8),
            legend.text=element_text(colour="grey40"),
            strip.background=element_rect(fill="#F2F2F2",size=0,colour="white"),
            strip.text=element_text(colour="grey30",size=7),
            axis.text.x=element_text(angle=0,hjust=1,vjust=0.5,colour="grey50",size=7),
            axis.text.y=element_text(colour="grey50",size=7),
            axis.title=element_text(colour="grey30",size=8),
            axis.ticks = element_blank(),
            axis.line=element_blank(),
            panel.grid.minor=element_blank(),
            panel.grid.major=element_line(size=0.2),
            panel.border=element_blank())
    return(p)
  }
}


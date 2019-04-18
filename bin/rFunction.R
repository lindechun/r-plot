## CREATED BY Lin Dechun (lindechun@genomics.cn)

# personage tools for data processing
## change type of element for dataframe
as.fac <- function(dat){
    for (i in colnames(dat)){
        dat[,i] <- factor(dat[,i])
    }
    return(dat)
}

as.chr <- function(dat){
    for (i in colnames(dat)){
        dat[,i] <- as.character(dat[,i])
    }
    return(dat)
}

as.num <- function(dat){
    for (i in colnames(dat)){
        dat[,i] <- as.numeric(dat[,i])
    }
    return(dat)
}

#install package
## install pacakge from Bioconductor
bio <- function(packagelist,force=F) {
  for (package in packagelist) {
    if (!package %in% .packages(all.available = T) || force == T) {
      print(paste("install package: ",package,sep=""))
      source("http://bioconductor.org/biocLite.R")
      biocLite(package)
    } else {
      print(paste(package," has been installed!",sep=""))
    }
  }
}

## install package from CRAN
installp <- function(packagelist,force=F) {
  for (package in packagelist) {
    if (!package %in% .packages(all.available = T) || force == T) {
    # if (!require(package)){
      print(paste("install package: ",package,sep=""))
      install.packages(package,repo="https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
    } else {
      print(paste(package," has been installed!",sep=""))
    }
  }
}

## install package from github
installg <- function(..., force=F) {
    if (!require(devtools)){
      install.packages(package,repo="https://mirrors.tuna.tsinghua.edu.cn/CRAN/")
      library(devtools)
    }
  # installp('devtools',force=force)
  install_github(...)
}
# themes for ggplot2 self-definited
## axis.title的size大小适用于labs(),而对默认生成的坐标轴标题偏小5左右
theme_classic2 <- function(...) {
    ## 上下左右均有坐标线，主题风格简约
  theme_bw(...)+theme(panel.background=element_rect(color='black'),
    panel.border=element_rect(fill='transparent',color='black'),
    panel.grid=element_blank(),
    plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),'cm'),
    title=element_text(color='black',family="Times"),
    plot.title=element_text(size=15,margin=unit(c(0.5,0.5,0.5,0.5),'cm'),hjust=0.5),
    axis.title =element_text(size=14,vjust = 0.1),
    axis.text=element_text(family="Times",size=12,color='black',margin=unit(0.8,"lines")),
    #axis.text.y=element_text(family="Times",size=8,color='black',margin=margin(8,0,3,0,"pt")),
    #axis.text.x=element_text(family="Times",size=12,color='black',margin=margin(8,0,3,0,"pt")),
    legend.background = element_blank(),
    legend.text=element_text(size=10,color='black',family = 'Times', hjust = 0),
    legend.title=element_text(size = 11,color='black',family='Times', hjust = 0),
    strip.background=element_rect(fill="white")
    )
}

theme_classic3 <- function(...) {
    ## 左边和下边才有坐标线
  theme_bw(...)+ theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),'cm'),
        title=element_text(color='black',face='plain',family="Times"),
        plot.title=element_text(size = 15,margin=unit(c(0.5,0.5,0.5,0.5),'cm'),hjust=0.5),
        axis.title =element_text(size=14,vjust = 0.1),
        axis.text=element_text(family="Times",size=12,color='black',margin = unit(0.8,"lines")),
        legend.background = element_blank(),
        axis.line.x=element_line(size=0.5,colour="black", linetype='solid'),
        axis.line.y=element_line(size=0.5,colour="black", linetype='solid'),
        legend.text=element_text(size=10,color='black',family='Times',hjust=0),
        legend.title=element_text(size = 11,color = 'black',family = 'Times', hjust = 0),
        strip.background=element_rect(fill="white")
        )
}

theme_cin <- function(..., bg='transparent') {
    ## 轴续向内伸
  require(grid)
  theme_bw(...) + theme(rect=element_rect(fill=bg),
        plot.margin=unit(rep(0.5,4), 'lines'),
        panel.background=element_rect(fill='transparent',
        color='black'), panel.border=element_rect(fill='transparent', color='transparent'),
        panel.grid=element_blank(),
        title=element_text(color='black',face='plain',family="Times"),
        plot.title=element_text(size = 15,color='black',margin=unit(c(0.5,0.5,0.5,0.5),'cm'),hjust=0.5),
        axis.title = element_text(size=14,color='black',vjust=0.1), axis.ticks.length = unit(-0.3,"lines"),
        axis.text.x=element_text(family="Times",size=12,color='black',margin=margin(8,0,3,0,"pt")),
        axis.text.y=element_text(family="Times",size=12,color='black',margin=margin(0,8,0,3,"pt")),
        axis.ticks = element_line(color='black'),
        legend.background = element_blank(),
        legend.text=element_text(size=10,color='black',family='Times', hjust=0),
        legend.title=element_text(size=11,color='black',family='Times',hjust=0),
        legend.key=element_rect(fill='transparent', color='transparent'),
        strip.background=element_rect(fill="white")
        )
}

## bar 特定主题
theme_bar <- function(..., bg='transparent'){
  ##
  theme_bw(...) + theme(plot.margin=unit(rep(0.5,4), 'lines'),
        title=element_text(color='black',face='plain',family="Times"),
        plot.title=element_text(size = 15,color='black',margin=unit(c(0.5,0.5,0.5,0.5),'cm'),hjust=0.5),
        axis.title = element_text(size=14,color='black', vjust=0.1),
        axis.text.x=element_text(family="Times",size=12,color='black',margin=margin(8,0,3,0,"pt")),
        axis.text.y=element_text(family="Times",size=12,color='black',margin=margin(0,8,0,3,"pt")),
        strip.background  = element_blank(),
        panel.grid.major = element_line(colour = "grey80"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        legend.position="bottom",
        legend.background = element_blank(),
        legend.text=element_text(size=10,color='black',family='Times', hjust=0),
        legend.title=element_text(size = 11,color = 'black',family = 'Times', hjust = 0),
        legend.key=element_rect(fill='transparent', color='transparent')
        )
}

theme_point1 <- function(...) {
    ## 主题是theme_classic2,添加y轴主垂直线
  theme_bw(...)+theme(panel.background=element_rect(color='black'),
    panel.border=element_rect(fill='transparent',color='black'),
    # panel.grid=element_blank(),
    plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),'cm'),
    title=element_text(color='black',family="Times"),
    plot.title=element_text(size=15,margin=unit(c(0.5,0.5,0.5,0.5),'cm'),hjust=0.5),
    axis.title =element_text(size=14,vjust = 0.1),
    axis.text=element_text(family="Times",size=12,color='black',margin=unit(0.8,"lines")),
    legend.background = element_blank(),
    legend.text=element_text(size=10,color='black',family = 'Times', hjust = 0),
    legend.title=element_text(size = 11,color='black',family='Times', hjust = 0),
    strip.background=element_rect(fill="white"),
    panel.grid.major.x=element_blank(),
    panel.grid.minor.x=element_blank(),
    panel.grid.minor.y=element_blank(),
    panel.grid.major.y=element_line(linetype="dashed"),

    # panel.grid.major=element_line(linetype="solid",size=0.5)
    )
}

theme_point2 <- function(...) {
    ## 主题是theme_classic2,添加x轴主垂直线
  theme_bw(...)+theme(panel.background=element_rect(color='black'),
    panel.border=element_rect(fill='transparent',color='black'),
    # panel.grid=element_blank(),
    plot.margin=unit(c(0.5, 0.5, 0.5, 0.5),'cm'),
    title=element_text(color='black',family="Times"),
    plot.title=element_text(size=15,margin=unit(c(0.5,0.5,0.5,0.5),'cm'),hjust=0.5),
    axis.title =element_text(size=14,vjust = 0.1),
    axis.text=element_text(family="Times",size=12,color='black',margin=unit(0.8,"lines")),
    legend.background = element_blank(),
    legend.text=element_text(size=10,color='black',family = 'Times', hjust = 0),
    legend.title=element_text(size = 11,color='black',family='Times', hjust = 0),
    strip.background=element_rect(fill="white"),
    panel.grid.major.x=element_line(linetype="dashed"),
    panel.grid.minor.x=element_blank(),
    panel.grid.major.y=element_blank(),
    panel.grid.minor.y=element_blank()
    # panel.grid.major=element_line(linetype="solid",size=0.5)
    )
}

# alias command
rmd <- function(){
  rm(list=ls())
}

# plot tools
## self-correcting location of x-aixs label ( applies to ggplot2)

Xlable_angle_correct <- function(selfp, xtics_angle) {

  if (xtics_angle != 0){
    if (xtics_angle == 90) {
      selfp <- selfp + theme(axis.text.x=
        element_text(angle=xtics_angle, hjust=1, vjust=0.5))
    } else if (xtics_angle == 45) {
      selfp <- selfp + theme(axis.text.x=
        element_text(angle=xtics_angle, hjust=1, vjust=1))
    } else {
      selfp <- selfp + theme(axis.text.x=
        element_text(angle=xtics_angle, hjust=0.5, vjust=0.5))
    }
  }
  return(selfp)

}

LegendPos <- function (p, legend_pos) {
  #Set the position of legend
  top='top'
  botttom='bottom'
  left='left'
  right='right'
  none='none'
  legend_pos_par <- legend_pos
  p <- p + theme(legend.position=legend_pos_par)
  return(p)
}

Plot_size_x <- function(w_auto_temp, xvariable, variable, rotate_plot, legend_pos, other_w_v=T, plott="box", char=F) {
      #     if (w_auto_temp < 9) {
      #   w_auto=0.7*w_auto_temp
      #   if (w_auto>6){w_auto=6}
      #   if (w_auto<4 & w_auto >1 ){w_auto=4}
      # } else if (w_auto_temp < 20) {
      #   w_auto=4+(w_auto_temp-8)/5
      # } else if (w_auto_temp < 100) {
      #   w_auto=7+(w_auto_temp-20)/5
      # } else{
      #   w_auto=22
      # }
  if (plott == "box"){
      w_auto = sqrt(w_auto_temp)*0.9+w_auto_temp*0.7
      # w_auto <- w_auto_temp*(atan(w_auto_temp-20)+2.570796)
  }else if (plott == "bar"){
      w_auto = sqrt(w_auto_temp)*0.9+w_auto_temp*0.1

  }else if (plott == "line" || plott == "point"){
      w_auto=sqrt(w_auto_temp)+1.5*sqrt(sqrt(w_auto_temp))+1
  }else if (plott == "areaplot"){
      w_auto=0.65*sqrt(w_auto_temp)
  }
  #     if (w_auto_temp < 9) {
  #       w_auto=1.1*w_auto_temp
  #       if (w_auto>6){w_auto=6}
  #     } else if (w_auto_temp < 20) {
  #       w_auto=4+(w_auto_temp-8)/5
  #     } else if (w_auto_temp < 100) {
  #       w_auto=7+(w_auto_temp-20)/5
  #     } else{
  #       w_auto=22
  #     }
  # }

  if (variable != xvariable & plott == "box") {
    if(other_w_v == "dodge"){
      w_auto=w_auto+0.3*sqrt(w_auto_temp)
    }
  }

  h_auto=4
  if (w_auto_temp >20){h_auto=6}

  if(plott == "point" & ! char){
    w_auto=4
    h_auto=5
  }else if (plott == "areaplot" & ! char){
    w_auto=6
    h_auto=5
  }else if(plott =="density2d"){
    w_auto=6
    h_auto=5
  }
  if (variable != xvariable & variable != "c_t_c_t0304"){
    if (rotate_plot == 'FALSE') {
      if (legend_pos == "right" || legend_pos == "left") {
        w_auto=w_auto+1.5
      }
      if (legend_pos == "top" || legend_pos == "bottom") {
        h_auto=h_auto+0.4
      }
    } else {
      if (legend_pos == "top" || legend_pos == "bottom") {
        w_auto=w_auto+1
      }
    }
  }

  return(c(w_auto, h_auto))
}

Plot_size_final <- function (xva_level_n, rotate_plot, legend_pos, variable, xvariable, facet, facet_direction, facet_level_n, dodge_status="dodge",plottype="box",char=F) {

   w_h_auto <- Plot_size_x(xva_level_n, xvariable, variable, rotate_plot, legend_pos,other_w_v=dodge_status,plott=plottype,char=char)

   w_auto <- w_h_auto[1]
   h_auto <- w_h_auto[2]

  if (facet != "NoMeAnInGTh_I_n_G_s") {
    if (facet_direction == "h"){
      if (rotate_plot == "FALSE"){
        if(xvariable == facet){
          w_auto=w_auto+0.5
        }else if(variable == facet){
          w_auto=w_auto+facet_level_n*0.5
        }else{
          w_auto=ifelse(w_auto*facet_level_n*0.5 < 50, 0.5*w_auto*facet_level_n,49)
        }
      }else{
        if(xvariable == facet){
          h_auto=w_auto+0.5
        }else if(variable == facet){
          h_auto=w_auto+facet_level_n*0.5
        }else{
          h_auto=ifelse(w_auto*facet_level_n*0.5 < 50, 0.5*w_auto*facet_level_n,49)
        }
      }
    }else{
      if (rotate_plot == "FALSE"){
        h_auto=ifelse(h_auto*facet_level_n*0.5 < 50, 0.5*h_auto*facet_level_n,49)
        if(xvariable == facet){
          # w_auto=xva_level_n
          w_auto=3
        }
      }else{        
        w_auto=ifelse(h_auto*facet_level_n*0.5 < 50, 0.5*h_auto*facet_level_n,49)
        if(xvariable == facet){
          # w_auto=xva_level_n
          h_auto=3
        }
      }
    }
  }

  return(c(w_auto, h_auto))
}

PLot_margin <-function (p, title, xlab, ylab) {
  # Control margin of plot by exist status of title, xlab, ylab
  if (title == "") {
    p <- p+ theme(plot.title=element_blank())
  }
  if (xlab == "") {
    p <- p+ theme(axis.title.x=element_blank())
  }
  if (ylab == "") {
    p <- p+ theme(axis.title.y=element_blank())
  }
  return(p)
}

Savefig <- function(filename,plott, width, height, ppi, ftype="pdf") {
  if (ftype == "pdf") {
    ggsave(plot=plott, filename=filename, dpi=ppi, width=width,
    height=height, units=c("in"),colormodel="srgb")
  } else {
    ggsave(plot=plott, filename=filename, dpi=ppi, width=width,
    height=height, units=c("in"))
  }
}

Plot_savefig <- function(finename, plott, uwid, vhig, w_auto, h_auto, ppi, rotate_plot, ftype="pdf") {

  if (uwid != FALSE) {
    if (rotate_plot == FALSE){
      Savefig(finename, p, uwid, vhig, ppi, ftype)
    }else{
      Savefig(finename, p, vhig, uwid, ppi, ftype)
    }
  } else {
    if (rotate_plot == FALSE) {
      Savefig(finename, p, w_auto, h_auto, ppi, ftype)
    } else {
      Savefig(finename, p, h_auto, w_auto, ppi, ftype)
    }
  }
}

PrintWD <- function(uwid, vhig, w_auto, h_auto, rotate_plot){
    if(uwid != FALSE){
        if(rotate_plot == FALSE){
            print(paste("width of picture:",uwid,sep=" "))
            print(paste("height of picture:",vhig,sep=" "))
        }else{
            print(paste("width of picture:",vhig,sep=" "))
            print(paste("height of picture:",uwid,sep=" "))
        }
    }else{
        if(rotate_plot == FALSE){
            print(paste("width of picture:",round(w_auto,3),sep=" "))
            print(paste("height of picture:",round(h_auto,3),sep=" "))
        }else{
            print(paste("width of picture:",round(h_auto,3),sep=" "))
            print(paste("height of picture:",round(w_auto,3),sep=" "))
        }
    }
}

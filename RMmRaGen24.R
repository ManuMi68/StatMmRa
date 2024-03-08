
#+-+-+-+-+-+-+-+-+-+-Manuel Miguel Ramos Álvarez+-+-+-+-+-+-+-+-+-+-  
#+-+-+-+-+-+-+-+-+-+-Noviembre de 2023          +-+-+-+-+-+-+-+-+-+-  
#+-+-+-+-+-+-+-+-+-+-General basic functions    +-+-+-+-+-+-+-+-+-+-  


# ▼▼▼================= Information Processing =================▼▼▼
# They are debugged
{

  greek = data.frame( # lowercase Greek letters
    alpha='\u03b1', beta='\u03b2', gamma='\u03b3', delta='\u03b4', epsilon='\u03b5', zeta='\u03b6',
    eta='\u03b7', theta='\u03b8', iota='\u03b9', kappa='\u03ba', lambda='\u03bb', mu='\u03bc',
    nu='\u03bd', xi='\u03be', omicron='\u03bf', pi='\u03c0', rho='\u03c1', sigma='\u03c3', tau='\u03c4',
    upsilon='\u03c5', phi='\u03c6', chi='\u03c7', psi='\u03c8', omega='\u03c9',
    # uppercase Greek letters
    Alpha='\u0391', Beta='\u0392', Gamma='\u0393', Delta='\u0394', Epsilon='\u0395', Zeta='\u0396',
    Eta='\u0397', Theta='\u0398', Iota='\u0399', Kappa='\u039a', Lambda='\u039b', Mu='\u039c',
    Nu='\u039d', Xi='\u039e', Omicron='\u039f', Pi='\u03a0', Rho='\u03a1', Sigma='\u03a3', Tau='\u03a4',
    Upsilon='\u03a5', Phi='\u03a6', Chi='\u03a7', Psi='\u03a8', Omega='\u03a9',
    # mathematical symbols
    infinity ='\u221e', leftrightarrow ='\u21d4', forall='\u2200', exist ='\u2203', notexist ='\u2204',
    emptyset ='\u2205', elementof='\u2208', notelementof='\u2209', proportional='\u221d',
    asymptoticallyEqual='\u2243', notasymptoticallyEqual='\u2244', approxEqual='\u2245', almostEqual='\u2248',
    leq='\u2264', geq='\u2265', muchless='\u226a', muchgreater='\u226b', leftarrow='\u21d0', rightarrow='\u21d2',
    equal='\uff1d', notEqual='\u2260', integral='\u222b', doubleintegral='\u222c', tripleintegral='\u222d',
    logicalAnd='\u2227', logicalOr='\u2228', intersection='\u2229', union='\u222a')
    
    
  chkPkg <-function(l.of.p){
    new.packages <- l.of.p[!(l.of.p %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    lapply(l.of.p, function(x) {
      if(! x %in% tolower((.packages()))) library(x, character.only = TRUE)
    })
    # lapply(l.of.p, library, character.only = TRUE)
    invisible()
    #library(l.of.p, character.only = TRUE)
    #library(get(list.of.packages))
  }
  chkPkg("data.table")
  
  
  # Better chkPkg, but I keep it for compatibility.
  initPkg <- function(need) {
    ip <- .packages(all.available = T)
    if (any((need %in% ip) == F)) {
      install.packages(need[!(need %in% ip)],repos="http://cran.us.r-project.org")
    }
    ok <- sapply(1:length(need), function(p) require(need[[p]], character.only = T))
  }
  
  Mk.dir<-function(NmDirp="Result") {
    NmDir<-paste0(getwd(),"/",NmDirp,"/")
    if (!dir.exists(NmDir)) {try(dir.create(NmDir))}
  }
  
  '%ni%' <- Negate('%in%')
  
  NNa<- function(x,na.rm=FALSE) {ifelse(na.rm,length(na.omit(x)),length(x))}
  
  na.del<-function(x){xm<-as.matrix(x);xm[complete.cases(xm),]} # As elimna or elimnaMM
  
  DesLst <-function(x) cat("Number:" , length(x), "\n", names(x))
  
  try_catch <- function(exprs) {!inherits(try(eval(exprs)), "try-error")}
  
  IfFilterSess <- function(x,Fs=F){
    if(Fs){
      filter(x,!(session==1&Seq<9))
    }else{
      x
    }
  }
  
  IfFilterLog <- function(x,Fs=F){
    if(Fs){
      x<-x[,logBF:=log10(BF),by=list(Group,Day,Subj)]
      x
    }else{
      x<-x[,logBF:=log(BF),by=list(Group,Day,Subj)]
      x
    }
  }
  
  IfFilterLog2 <- function(x,Fs=F){
    if(Fs){
      x<-x[,logBF:=log10(BF),by=list(Day,Subj)]
      x
    }else{
      x<-x[,logBF:=log(BF),by=list(Day,Subj)]
      x
    }
  }
  
  frmMM<-function(x,d=2) formatC(round(x,d),d,format="f")
  
  chpden <-function(x) {x[is.na(x)] <-""; x[x=="ns"]<-""; x}
  
  RoundMM <-function(Elx, decim=4, Alfa=.05, Signif="less") { # "less", "great")
    # Signif c(=NULL, "less", "great")
    ElxFrm=NA
    if (!is.na(Elx)) {
      ElxNum =as.numeric(Elx)
      ElxFrm <- formatC(round(ElxNum,decim),decim,format="f")
      #if (!is.null(Signif))
      if (!is.null(Signif) & ((Signif=="less" & ElxNum <=Alfa) | (Signif=="great" & ElxNum >Alfa))) ElxFrm <- paste0(ElxFrm,"*")
    }
    ElxFrm
  }
  
  rd<-function(x,Dec=2) {trunc(x*(10^Dec))/(10^Dec)}
  
  numf <- function(x, digits = 2) { 
    ncode <- paste0("%.", digits, "f")
    sub("^(-?)0.", "\\1.", sprintf(ncode, x))
  }
  
  percent <- function(num, digits = 2, ...) {
    # To compute percentage
    percentage <-formatC(num * 100, format = "f", digits = digits, ...)
    # appending "%" symbol at the end of
    # calculate percentage value
    paste0(percentage, "%")}
  
  fdec = function(X1)gsub("0\\.","\\.", X1)
  
  # from Zheng-Ling Yang, Yan-Wen Song, Zhi-Feng Duan, Teng Wang & Jun Zhang (2016) 
  #     New Sigmoid-like function better than Fisher z transformation, 
  #     Communications in Statistics - Theory and Methods, 45:8, 2332-2341,
  #     DOI: 10.1080/03610926.2013.771750
  Sigmoid_FisherZ <-function(x, k=1.701) {(1/k)*log((1+x)/(1-x))}
  
  InvZFish <- function (y) {
    r <- (exp(2 * y) - 1)/(exp(2 * y) + 1)
    return(r)
  }
} 
# ▲▲▲================= Information Processing =================▲▲▲


  
# ▼▼▼================ Information Presentation ================▼▼▼
# They are debugged
{
  kableTabl<-function(TableFor, LebelFor="",NoteFor="", heightp = "800px") {
    #chkPkg("kableExtra")
    ResFitAllF<- TableFor %>%
      kbl(caption=paste0(LebelFor),
          digits=4,escape = F, align = "r")  %>%
      kable_paper("hover", full_width = F,font_size = 10) %>%
      kable_styling(fixed_thead = T) %>%
      footnote(general = NoteFor) %>%
      scroll_box(width = "100%", height = heightp)
    options(show.error.messages = TRUE)
    ResFitAllF
  }
  
  kableTabl2<-function(TableFor, LebelFor="",NoteFor="", heightp = "800px") {
    #chkPkg("kableExtra")
    ResFitAllF<- TableFor %>%
      kbl(caption=paste0(LebelFor),
          digits=4,escape = F, align = "c")  %>%
      kable_paper("hover", full_width = F,font_size = 10) %>%
      column_spec ((1:ncol(TableFor)+1),border_left = T, border_right = T) %>%
      row_spec((0:nrow(TableFor)), extra_css = "border-bottom: 1px solid") %>%
      #row_spec((0:nrow(TableFor)), extra_css = "border-ups: 1px solid") %>%
      kable_styling(fixed_thead = T) %>%
      footnote(general = NoteFor) %>%
      scroll_box(width = "100%", height = heightp)
    options(show.error.messages = TRUE)
    ResFitAllF
  }  
  
  # TableFor=; ModRef="SSasymp Original"; LebelFor=""; NoteFor=""; heightp = "800px"
  kableTablCol<- function(TableFor, ModRef="SSasymp Original", LebelFor="",NoteFor="", heightp = "800px") {
    #chkPkg("kableExtra")
    
    color.1 <- TableFor[,which(GenTypeM=="E.Mod")]
    color.2 <- TableFor[,which(GenTypeM=="S.Mod")]
    color.3 <- TableFor[,which(GenTypeM=="M.Mod")]
    color.4 <- TableFor[,which(Model==ModRef)]
    color.1 <- color.1[color.1%ni%color.4]
    
    ResFitAllF<- TableFor %>%
      kbl(caption=paste0(LebelFor),
          digits=4,escape = F, align = "r")  %>%
      kable_paper("hover", full_width = F,font_size = 14) %>%
      kable_styling(fixed_thead = T) %>%
      row_spec(color.1, bold = T, color = "white", background = "red") %>%
      row_spec(color.2, bold = T, color = "white", background = "green") %>%
      row_spec(color.3, bold = T, color = "white", background = "blue") %>%
      row_spec(color.4, bold = T, color = "white", background = "orange") %>%
      footnote(general = NoteFor) %>%
      scroll_box(width = "100%", height = heightp)
    options(show.error.messages = TRUE)
    ResFitAllF
  }
}
# ▲▲▲================ Information Presentation ================▲▲▲



# ▼▼▼======================== Tool Kit ========================▼▼▼
# They are debugged
{
  # Taductor
  #install.packages("devtools")
  #devtools::install_github("zumbov2/deeplr")
  
  chkPkg("deeplr")
  my_keyDeep="16ce0043-b6d4-60a9-9177-6d12910a20b2:fx"
}
# ▲▲▲======================== Tool Kit ========================▲▲▲
  
  

# ▼▼▼============= Graphs & Exploratory & Visual ==============▼▼▼
# They are debugged
{
  chkPkg("cowplot")
  
  decilecolors=c("#7F7FCE", "#7F7FF7", "#88A9F9", "#93D2FB", "#A0FCFE",
                 "#BDFDD7", "#DDFEB3", "#FFFF91", "#F8D68B", "#F3AE86" )
  
  theme_ipsum.vMM<- function (base_family = "Helvetica", base_size = 11.5, plot_title_family = base_family, 
                              plot_title_size = 18, plot_title_face = "bold", plot_title_margin = 10, 
                              subtitle_family = base_family, subtitle_size = 12, subtitle_face = "plain", 
                              subtitle_margin = 15, strip_text_family = base_family, strip_text_size = 12, 
                              strip_text_face = "plain", caption_family = base_family, 
                              caption_size = 9, caption_face = "italic", caption_margin = 10, 
                              axis_text_size = base_size, axis_title_family = subtitle_family, 
                              axis_title_size = 9, axis_title_face = "plain", axis_title_just = "rt", 
                              plot_margin = margin(30, 30, 30, 30), grid_col = "#cccccc", 
                              grid = TRUE, axis_col = "#cccccc", axis = FALSE, ticks = FALSE) 
  {
    ret <- ggplot2::theme_minimal(base_family = base_family, 
                                  base_size = base_size)
    ret <- ret + theme(legend.background = element_blank())
    ret <- ret + theme(legend.key = element_blank())
    if (inherits(grid, "character") | grid == TRUE) {
      ret <- ret + theme(panel.grid = element_line(color = grid_col, 
                                                   size = 0.2))
      ret <- ret + theme(panel.grid.major = element_line(color = grid_col, 
                                                         size = 0.2))
      ret <- ret + theme(panel.grid.minor = element_line(color = grid_col, 
                                                         size = 0.15))
      if (inherits(grid, "character")) {
        if (regexpr("X", grid)[1] < 0) 
          ret <- ret + theme(panel.grid.major.x = element_blank())
        if (regexpr("Y", grid)[1] < 0) 
          ret <- ret + theme(panel.grid.major.y = element_blank())
        if (regexpr("x", grid)[1] < 0) 
          ret <- ret + theme(panel.grid.minor.x = element_blank())
        if (regexpr("y", grid)[1] < 0) 
          ret <- ret + theme(panel.grid.minor.y = element_blank())
      }
    }
    else {
      ret <- ret + theme(panel.grid = element_blank())
    }
    if (inherits(axis, "character") | axis == TRUE) {
      ret <- ret + theme(axis.line = element_line(color = "#2b2b2b", 
                                                  size = 0.15))
      if (inherits(axis, "character")) {
        axis <- tolower(axis)
        if (regexpr("x", axis)[1] < 0) {
          ret <- ret + theme(axis.line.x = element_blank())
        }
        else {
          ret <- ret + theme(axis.line.x = element_line(color = axis_col, 
                                                        size = 0.15))
        }
        if (regexpr("y", axis)[1] < 0) {
          ret <- ret + theme(axis.line.y = element_blank())
        }
        else {
          ret <- ret + theme(axis.line.y = element_line(color = axis_col, 
                                                        size = 0.15))
        }
      }
      else {
        ret <- ret + theme(axis.line.x = element_line(color = axis_col, 
                                                      size = 0.15))
        ret <- ret + theme(axis.line.y = element_line(color = axis_col, 
                                                      size = 0.15))
      }
    }
    else {
      ret <- ret + theme(axis.line = element_blank())
    }
    if (!ticks) {
      ret <- ret + theme(axis.ticks = element_blank())
      ret <- ret + theme(axis.ticks.x = element_blank())
      ret <- ret + theme(axis.ticks.y = element_blank())
    }
    else {
      ret <- ret + theme(axis.ticks = element_line(size = 0.15))
      ret <- ret + theme(axis.ticks.x = element_line(size = 0.15))
      ret <- ret + theme(axis.ticks.y = element_line(size = 0.15))
      ret <- ret + theme(axis.ticks.length = grid::unit(5, 
                                                        "pt"))
    }
    xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, 
                 l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
    yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, 
                 l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
    ret <- ret + theme(axis.text.x = element_text(size = axis_text_size, 
                                                  margin = margin(t = 0)))
    ret <- ret + theme(axis.text.y = element_text(size = axis_text_size, 
                                                  margin = margin(r = 0)))
    ret <- ret + theme(axis.title = element_text(size = axis_title_size, 
                                                 family = axis_title_family))
    ret <- ret + theme(axis.title.x = element_text(hjust = xj, 
                                                   size = axis_title_size, family = axis_title_family, face = axis_title_face))
    ret <- ret + theme(axis.title.y = element_text(hjust = yj, 
                                                   size = axis_title_size, family = axis_title_family, face = axis_title_face))
    ret <- ret + theme(axis.title.y.right = element_text(hjust = yj, 
                                                         size = axis_title_size, angle = 90, family = axis_title_family, 
                                                         face = axis_title_face))
    ret <- ret + theme(strip.text = element_text(hjust = 0, size = strip_text_size, 
                                                 face = strip_text_face, family = strip_text_family))
    ret <- ret + theme(panel.spacing = grid::unit(2, "lines"))
    ret <- ret + theme(plot.title = element_text(hjust = 0, size = plot_title_size, 
                                                 margin = margin(b = plot_title_margin), family = plot_title_family, 
                                                 face = plot_title_face))
    ret <- ret + theme(plot.subtitle = element_text(hjust = 0, 
                                                    size = subtitle_size, margin = margin(b = subtitle_margin), 
                                                    family = subtitle_family, face = subtitle_face))
    ret <- ret + theme(plot.caption = element_text(hjust = 1, 
                                                   size = caption_size, margin = margin(t = caption_margin), 
                                                   family = caption_family, face = caption_face))
    ret <- ret + theme(plot.margin = plot_margin)
    ret
  }
  
  theme_bw_MM <- function (base_size = 11, base_family = "", base_line_size = base_size/22, 
                           base_rect_size = base_size/22) {
    theme_grey(base_size = base_size, base_family = base_family, 
               base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace% 
      theme(panel.background = element_rect(fill = "white", 
                                            colour = NA), panel.border = element_rect(fill = NA, 
                                                                                      colour = "grey20"), panel.grid = element_line(colour = "grey92"), 
            panel.grid.minor = element_line(linewidth = rel(0.5)), 
            strip.background = element_rect(fill = "white", 
                                            colour = "grey20"), legend.key = element_rect(fill = "white", 
                                                                                          colour = NA), complete = TRUE)
  }
  
  # adds transparancy to a color
  addTrans <- function(color,trans) {
    # This function adds transparancy to a color.
    # Define transparancy with an integer between 0 and 255
    # 0 being fully transparant and 255 being fully visable
    # Works with either color and trans a vector of equal length,
    # or one of the two of length 1.
    
    if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
    if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
    if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
    
    num2hex <- function(x)
    {
      hex <- unlist(strsplit("0123456789ABCDEF",split=""))
      return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
    }
    rgb <- rbind(col2rgb(color),trans)
    res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
    return(res)
  }
  
  Grph.2023<-function(DatP,Dvp,VarX="Day",VarFill="Class",LblsP=c("Preference for Displaced Object", "B","Object Preference During Test",""),
                      ylmP=NULL,hLin=F,lvIp=4,GrpSel=c(1:4),wMain=F,
                      TyGrp="Box", Wthdot=F, Relleno=T,  ResumAd=T,Spcng=1,ColPer,ColViol="gray75") {
    # TyGrp= 'Bar', 'Box', 'Violin', 'SplitViolin'
    chkPkg("ggplot2")
    ColPerI<-rep(ColPer[GrpSel],lvIp)
    TopePoint=ifelse(is.null(ylmP),.4,(1/30))
    #TopePoint=ifelse(is.null(ylmP),.4,(1/(30*(ylmP[2]-ylmP[1]))))
    
    gPrv<-ggplot2::ggplot(data=DatP, aes(x=get(VarX), y=get(Dvp),fill=factor(get(VarFill)))) +
      theme_classic2() +
      theme(legend.title = element_blank(),legend.position = "top", panel.spacing = unit(Spcng, "lines"),
            text = element_text(size=28))
    
    RangeMM<-ggplot_build(gPrv)$layout$panel_scales_y[[1]]$range$range
    RangeMf= ((ylmP[2]-ylmP[1])/(RangeMM[2]-RangeMM[1]))*.4
    TopePoint=ifelse(is.null(ylmP),.4,RangeMf)
    
    if (Relleno | TyGrp == "SplitViolin") {
      g1<-ggplot2::ggplot(data=DatP, aes(x=get(VarX), y=get(Dvp),fill=factor(get(VarFill)))) +
        theme_classic2() +
        theme(legend.title = element_blank(),legend.position = "top", panel.spacing = unit(Spcng, "lines"),
              text = element_text(size=28)) +
        # theme(legend.position="top", text = element_text(size=28),
        #       axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
        labs(tag=LblsP[2],y=LblsP[3],x=LblsP[4]) +
        scale_fill_manual(values=ColPerI)
      
      if (TyGrp=="SplitViolin") {
        g1<- g1 +
          introdataviz::geom_split_violin(alpha = .4, trim = FALSE) +
          geom_boxplot(width = .2, alpha = .8, fatten = NULL, show.legend = F,outlier.shape = NA)
      }
      if (TyGrp=="Bar") {
        g1 <- g1 + stat_summary(geom = "bar", fun = mean, position = "dodge") +
          stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=.2,
                       show.legend = F)
        #geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
        # position=position_dodge(.9))
      }
      if (TyGrp=="Box") g1 <- g1 + geom_boxplot(colour="black", outlier.shape = NA,lwd=1)
      if (TyGrp=="Violin") g1<-g1 + stat_ydensity(scale="width", trim=F,
                                                  position=position_dodge(0.75),draw_quantiles = c(.5),
                                                  width=.7, alpha=.5,lwd=1)
      
      if (Wthdot) g1 <- g1 +
          geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(.75),dotsize=TopePoint,
                       fill = "grey45",aes(group = interaction(get(VarX), get(VarFill))), alpha = 1)
      
      if (ResumAd)  {
        if (TyGrp=="SplitViolin") {
          g1 <- g1 + 
            stat_summary(fun=mean, geom="point", shape=18, color=ColPerI,
                         size=7,show.legend = F,position=position_dodge(width=.175)) +
            stat_summary(fun=mean, geom="point", shape=18, color=ColViol,
                         size=4,show.legend = F,position=position_dodge(width=.175))
        } else {
          g1 <- g1 + 
            stat_summary(fun=mean, geom="point", shape=18, color="white",
                         size=10,position=position_dodge(width=.75), show.legend = F) +
            stat_summary(fun=mean, geom="point", shape=18, color="black",
                         size=8,position=position_dodge(width=.75), show.legend = F)
        }
        
      }
      
      if(hLin) g1 <- g1 + geom_hline(yintercept=0, linetype="dashed", color = "gray30")
      if(wMain) g1 <- g1 + 
          stat_summary(fun=mean, geom="point", shape=13, size=8, color="black", fill="gray30") +
          stat_summary(fun=mean, geom="point", shape=13, size=7, color="black", fill="gray75")
    }
    
    
    # Unfilled
    if (!Relleno){
      g1<-ggplot2::ggplot(data=DatP, aes(x=get(VarX), y=get(Dvp),fill=factor(get(VarFill)),color=factor(get(VarFill)))) +
        theme_classic2() +
        theme(legend.title = element_blank(),legend.position = "top",panel.spacing = unit(Spcng, "lines"),
              text = element_text(size=28)) +
        # theme(legend.position="top", text = element_text(size=28),
        #       axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
        labs(tag=LblsP[2],y=LblsP[3],x=LblsP[4]) +
        scale_color_manual(values=ColPerI) + scale_fill_manual(values=rep("white",length(ColPer)))
      
      if (TyGrp=="Bar") {
        g1 <- g1 + stat_summary(geom = "bar", fun = mean, position = "dodge") +
          stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=.2,
                       show.legend = F)
        #geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
        # position=position_dodge(.9))
      }
      if (TyGrp=="Box") g1 <- g1 + geom_boxplot(outlier.shape = NA,lwd=1)
      if (TyGrp=="Violin") g1 <- g1 + stat_ydensity(scale="width", trim=F,
                                                    position=position_dodge(0.75),draw_quantiles = c(.5),
                                                    width=.7, alpha=.5,lwd=1)
      
      if (Wthdot) g1 <- g1 +
          geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(.75),dotsize=TopePoint,
                       fill = "grey45",aes(group = interaction(get(VarX), get(VarFill))), alpha = 1)
      
      if (ResumAd)  {	  
        g1 <- g1 + 
          stat_summary(fun=mean, geom="point", shape=18, color=ColPerI,
                       size=8,position=position_dodge(width=.75), show.legend = F) +
          stat_summary(fun=mean, geom="point", shape=18, color="black",
                       size=5,position=position_dodge(width=.75), show.legend = F) 
      }
      if(hLin) g1 <- g1 + geom_hline(yintercept=0, linetype="dashed", color = "gray30")
      if(wMain) g1 <- g1 + 
          stat_summary(fun=mean, geom="point", shape=13, size=8, color="black", fill="gray30") +
          stat_summary(fun=mean, geom="point", shape=13, size=7, color="black", fill="gray30")
    }
    if(!is.null(ylmP)) g1 <- g1 + coord_cartesian(ylim = ylmP)
    g1
  }
  
  Grph.2023b<-function(DatP,Dvp,VarX="Day",VarFill="Class",LblsP=c("Preference for Displaced Object", "B","Object Preference During Test",""),
                      ylmP=NULL,hLin=F,lvIp=4,GrpSel=c(1:4),wMain=F,
                      TyGrp="Box", Wthdot=F, Relleno=T,  ResumAd=T,Spcng=1,ColPer,ColViol="gray75") {
    # TyGrp= 'Bar', 'Box', 'Violin', 'SplitViolin'
    chkPkg("ggplot2")
    ColPerI<-rep(ColPer[GrpSel],lvIp)
    TopePoint=ifelse(is.null(ylmP),.4,(1/30))
    #TopePoint=ifelse(is.null(ylmP),.4,(1/(30*(ylmP[2]-ylmP[1]))))
    
    gPrv<-ggplot2::ggplot(data=DatP, aes(x=get(VarX), y=get(Dvp),fill=factor(get(VarFill)))) +
      theme_classic2() +
      theme(legend.title = element_blank(),legend.position = "top", panel.spacing = unit(Spcng, "lines"),
            text = element_text(size=28))
    
    RangeMM<-ggplot_build(gPrv)$layout$panel_scales_y[[1]]$range$range
    RangeMf= ((ylmP[2]-ylmP[1])/(RangeMM[2]-RangeMM[1]))*.4
    TopePoint=ifelse(is.null(ylmP),.4,RangeMf)
    
    if (Relleno | TyGrp == "SplitViolin") {
      g1<-ggplot2::ggplot(data=DatP, aes(x=get(VarX), y=get(Dvp),fill=factor(get(VarFill)))) +
        theme_classic2() +
        theme(legend.title = element_blank(),legend.position = "top", panel.spacing = unit(Spcng, "lines"),
              text = element_text(size=28)) +
        # theme(legend.position="top", text = element_text(size=28),
        #       axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
        labs(tag=LblsP[2],y=LblsP[3],x=LblsP[4]) +
        scale_fill_manual(values=ColPerI)
      
      if (TyGrp=="SplitViolin") {
        g1<- g1 +
          introdataviz::geom_split_violin(alpha = .4, trim = FALSE) +
          geom_boxplot(width = .2, alpha = .8, fatten = NULL, show.legend = F,outlier.shape = NA)
      }
      if (TyGrp=="Bar") {
        g1 <- g1 + stat_summary(geom = "bar", fun = mean, position = "dodge") +
          stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=.2,
                       show.legend = F)
        #geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
        # position=position_dodge(.9))
      }
      if (TyGrp=="Box") g1 <- g1 + geom_boxplot(colour="black", outlier.shape = NA,lwd=1, staplewidth = .5)
      if (TyGrp=="Violin") g1<-g1 + stat_ydensity(scale="width", trim=F,
                                                  position=position_dodge(0.75),draw_quantiles = c(.5),
                                                  width=.7, alpha=.5,lwd=1)
      
      if (Wthdot) g1 <- g1 +
          geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(.75),dotsize=TopePoint,
                       fill = "grey45",aes(group = interaction(get(VarX), get(VarFill))), alpha = 1)
      
      if (ResumAd)  {
        if (TyGrp=="SplitViolin") {
          g1 <- g1 + 
            stat_summary(fun=mean, geom="point", shape=18, color=ColPerI,
                         size=7,show.legend = F,position=position_dodge(width=.175)) +
            stat_summary(fun=mean, geom="point", shape=18, color=ColViol,
                         size=4,show.legend = F,position=position_dodge(width=.175))
        } else {
          g1 <- g1 + 
            stat_summary(fun=mean, geom="point", shape=18, color="white",
                         size=10,position=position_dodge(width=.75), show.legend = F) +
            stat_summary(fun=mean, geom="point", shape=18, color="black",
                         size=8,position=position_dodge(width=.75), show.legend = F)
        }
        
      }
      
      if(hLin) g1 <- g1 + geom_hline(yintercept=0, linetype="dashed", color = "gray30")
      if(wMain) g1 <- g1 + 
          stat_summary(fun=mean, geom="point", shape=13, size=8, color="black", fill="gray30") +
          stat_summary(fun=mean, geom="point", shape=13, size=7, color="black", fill="gray75")
    }
    
    
    # Unfilled
    if (!Relleno){
      g1<-ggplot2::ggplot(data=DatP, aes(x=get(VarX), y=get(Dvp),fill=factor(get(VarFill)),color=factor(get(VarFill)))) +
        theme_classic2() +
        theme(legend.title = element_blank(),legend.position = "top",panel.spacing = unit(Spcng, "lines"),
              text = element_text(size=28)) +
        # theme(legend.position="top", text = element_text(size=28),
        #       axis.text.x = element_text(colour = "black"), axis.text.y = element_text(colour = "black")) +
        labs(tag=LblsP[2],y=LblsP[3],x=LblsP[4]) +
        scale_color_manual(values=ColPerI) + scale_fill_manual(values=rep("white",length(ColPer)))
      
      if (TyGrp=="Bar") {
        g1 <- g1 + stat_summary(geom = "bar", fun = mean, position = "dodge") +
          stat_summary(geom = "errorbar", fun.data = mean_se, position = position_dodge(.9),width=.2,
                       show.legend = F)
        #geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
        # position=position_dodge(.9))
      }
      if (TyGrp=="Box") g1 <- g1 + geom_boxplot(outlier.shape = NA,lwd=1, staplewidth = .5)
      if (TyGrp=="Violin") g1 <- g1 + stat_ydensity(scale="width", trim=F,
                                                    position=position_dodge(0.75),draw_quantiles = c(.5),
                                                    width=.7, alpha=.5,lwd=1)
      
      if (Wthdot) g1 <- g1 +
          geom_dotplot(binaxis='y', stackdir='center', position=position_dodge(.75),dotsize=TopePoint,
                       fill = "grey45",aes(group = interaction(get(VarX), get(VarFill))), alpha = 1)
      
      if (ResumAd)  {	  
        g1 <- g1 + 
          stat_summary(fun=mean, geom="point", shape=18, color=ColPerI,
                       size=8,position=position_dodge(width=.75), show.legend = F) +
          stat_summary(fun=mean, geom="point", shape=18, color="black",
                       size=5,position=position_dodge(width=.75), show.legend = F) 
      }
      if(hLin) g1 <- g1 + geom_hline(yintercept=0, linetype="dashed", color = "gray30")
      if(wMain) g1 <- g1 + 
          stat_summary(fun=mean, geom="point", shape=13, size=8, color="black", fill="gray30") +
          stat_summary(fun=mean, geom="point", shape=13, size=7, color="black", fill="gray30")
    }
    if(!is.null(ylmP)) g1 <- g1 + coord_cartesian(ylim = ylmP)
    g1
  }
  
  GrphModels <-function (DTg,modP, ElTit="Example", NmModP=NULL, AjAx=FALSE,
                         sample.curve = 1000, ylab = "Dependent", 
                         xlab = "Independent", theme = theme_classic(), legend.position = "top", 
                         r2 = "all", ic = FALSE, fill.ic = "gray70", alpha.ic = 0.5, 
                         error = "SE", point = "all", width.bar = NA, scale = "none", 
                         textsize = 12, pointsize = 3, linesize = 2, linetype = 1, 
                         pointshape = 21, fillshape = "gray40", colorline = "blue", 
                         round = NA, xname.formula = "x", yname.formula = "y", comment = NA, 
                         fontfamily = "sans") {
    
    DTg2<-as.data.table(copy(DTg))
    if (ElTit=="NLS.MMLin1.MM (from renz)") {DTg2=1/DTg; xlab=paste0("1/ ",xlab); ylab=paste0("1/ ",ylab)}
    if (ElTit=="NLS.MMLin2.MM (from renz)") {DTg2[,Y:=X/Y]; ylab=paste0(xlab," / ",ylab)}
    if (ElTit=="NLS.MMLin3.MM (from renz)") {DTg2[,Y:=Y/X]; ylab=paste0(ylab," / ",xlab)}
    
    IV=DTg2$X
    DV=DTg2$Y
    
    MinX =min(IV)
    if (AjAx) MinX=0
    IV.p = seq(MinX, max(IV), length.out = sample.curve)
    DF.p = data.frame(X = IV.p, Y = predict(modP, newdata = data.frame(X = IV.p))) 
    
    grph = ggplot(DTg2, aes(x = X, y = Y))
    
    grph = grph + theme + geom_line(data = DF.p, aes(x = X, y = Y, color = "black"), linewidth = linesize, lty = linetype) +
      ggtitle(ElTit) +
      scale_color_manual(name = "", values = colorline, label = as.expression(NmModP)) + 
      theme(axis.text = element_text(size = textsize, color = "black",
                                     family = fontfamily), axis.title = element_text(size = textsize,
                                                                                     color = "black", family = fontfamily), legend.position = legend.position, 
            legend.text = element_text(size = textsize, family = fontfamily), 
            legend.direction = "vertical", legend.text.align = 0, 
            legend.justification = 0) + ylab(ylab) + xlab(xlab)
    grph
  }
  
  SwtTypMod <- function(idx, type, dtp) {
    switch(type,
           nls1 = PrintMod.gg(DTg = dtp, modP = eval(AllModelsJn[AllModelsJn.Nm==AllModF[idx,Id]][[1]]),
                              ElTit = AllModF[idx,Id],NmModP = "", AjAx=TRUE),
           nls2 = Devuelvnls2(AllModF[idx,Id],dtp),
           drc = DevuelvDRC(AllModF[idx,Id],dtp))
  }
  
  Gridplot <- function(myplots, n){
    splitted_plots <- split(myplots, ceiling(seq_along(myplots)/n))
    lapply(splitted_plots, function(x) plot_grid(plotlist = x, labels = "AUTO"))
    # Grid <- cowplot::plot_grid(plotlist = myplots)
    # return(Grid)
  }
  
  
}
# ▲▲▲============= Graphs & Exploratory & Visual ==============▲▲▲
  

# ▼▼▼=========== Statistics & Mathematics: General ============▼▼▼
# They are debugged
{
  ExtrSig<-function(DTPas,decim=4) {
    #chkPkg("data.table")
    DTpp<-copy(DTPas)
    Mx<-as.matrix(round(unlist(DTpp),decim))
    ResP<-data.table(Eff=rownames(Mx),p=Mx);
    ResP[,p.Sig:=""]
    ResP[p.V1<.05,p.Sig:="*    "]
    return(as.data.table(ResP))
  }
  
  ExtrSig2<-function(DTPas,decim=4,Alfa=.05) {
    #chkPkg("data.table")
    DTpp<-copy(DTPas)
    Mx<-formatC(as.matrix(round(unlist(DTpp),decim)),decim,format="f")
    Mx2<-formatC(as.matrix(round(unlist(DTpp),4)),4,format="f")
    ResP<-data.table(Eff=rownames(Mx),p=Mx,p2=Mx2);
    ResP[,p.Sig:=""]
    ResP[p.V1<=Alfa,p.Sig:=paste0(p.V1,"*")]
    ResP[p.V1>Alfa,p.Sig:=p.V1]
    ResP[p.Sig=="0.00*",p.Sig:=paste0(p2.V1,"*")]
    return(as.data.table(ResP))
  }
  
  ExtSig.23 <- function(x,decim=4,Alfa=.05) {
    #chkPkg("data.table")
    p1<-copy(x)
    ResP<- setNames(data.table(as.matrix(unlist(p1))), c("p")) %>%
      data.table() %>% dplyr::mutate(pSig:=frmMM(p,4))  %>%
      data.table() %>% dplyr::mutate(pSig:=fdec(pSig)) %>%
      .[p<.0001,pSig:="<.0001"] %>%
      .[p>.99, pSig:=">.999 "] %>%
      dplyr::mutate(pSig:=paste(pSig,InterP(p),sep=" ")) %>%
      data.table()
    return(as.vector(ResP$pSig))
  }
  
  PosHoc.MM <- function(emPas){
    # chkPkg("rlang")
    ResPosNone<-pairs(emPas,adjust = "none")
    ResPosHolm<-pairs(emPas,adjust = "holm")
    asDT <-as.data.table(ResPosNone)
    LaspEm<-asDT[,p.value]
    nconPH=length(levels(ResPosNone)$contrast)
    lInter=nrow(asDT)/nconPH
    splir<-split(LaspEm,rep(c(1:lInter),each=nconPH))
    pRom=  unsplit(lapply(splir,adjustRom),rep(c(1:lInter),each=nconPH))
    asDT <-ResPosHolm %>% as.data.table() %>%
      dplyr::rename(., pHolm=p.value) %>%
      .[,pRom:=pRom] %>% 
      .[,pHolm:=ExtSig.23(pHolm)] %>%
      .[,pRom:=ExtSig.23(pRom)] %>%
      .[,`:=`(estimate=frmMM(estimate),SE=frmMM(SE,4),t.ratio=frmMM(t.ratio))] %>%
      data.table()
    asDT
  }
  
  # if (lambda >  0){TRANS = x ^ lambda} 
  # if (lambda == 0){TRANS = log(x)} 
  # if (lambda <  0){TRANS = -1 * x ^ lambda} 
  InterLambda<-function(lmb) {
    case_when(
      lmb < -.1             ~ 'lambda <  0: TRANS = -1 * x ^ lambda',
      lmb >= -.1 & lmb <=.1 ~ 'lambda == 0: TRANS = log(x)',
      lmb >  .1             ~ 'lambda >  0: TRANS = x ^ lambda',
      .default = NA
    )
  }
  
  TransfLambda<-function(x,lmb) {
    case_when(
      lmb < -.1             ~ (-1 * x ^ lmb),
      lmb >= -.1 & lmb <=.1 ~ (log(x)),
      lmb >  .1             ~ (x ^ lmb),
      .default = NA
    )
  }
  
  # A lambda **> |2|** Is a sign you should not be using the method  
  # - 2: 1/Y^2  
  # - 1: 1/Y        [inverse]  
  # -.5: 1/sqrt(Y)  [Inverse Root]  
  #   0: log(Y)     [Logarithmic]  
  # +.5: sqrt(Y)    [Root]  
  # + 1: Y          [No transformation necessary]
  # + 2: Y^2        [Quadratic]]
  InterLambCox2<-function(lmb) {
    case_when(
      lmb >= -2 & lmb <= mean(c(-2,-1))             ~ '- 2: 1/Y^2 vs - 1: 1/Y        [inverse]',
      lmb > mean(c(-2,-1)) & lmb <= mean(c(-1,-.5)) ~ '- 1: 1/Y        [inverse] vs -.5: 1/sqrt(Y)  [Inverse Root]',
      lmb > mean(c(-1,-.5)) & lmb <= mean(c(-.5,0)) ~ '-.5: 1/sqrt(Y)  [Inverse Root] vs 0: log(Y)     [Logarithmic]',
      lmb > mean(c(-.5,0)) & lmb <= mean(c(0,.5))   ~ '  0: log(Y)     [Logarithmic] vs +.5: sqrt(Y)    [Root]',
      lmb > mean(c(0,.5)) & lmb <= mean(c(.5,1))    ~ '+.5: sqrt(Y)    [Root] vs + 1: Y          [No transformation necessary]',
      lmb > mean(c(.5,1)) & lmb <= mean(c(1,2))     ~ '+ 1: Y          [No transformation necessary] vs + 2: Y^2        [Quadratic]]',
      lmb > mean(c(1,2)) & lmb <= 2                 ~ '+ 2: Y^2        [Quadratic]]',
      .default = 'you should not be using the method'
    )
  }
  
  InterLambCox<-function(lmb) {
    case_when(
      lmb >= -2 & lmb <= mean(c(-2,-1))             ~ '- 2: 1/Y^2',
      lmb > mean(c(-2,-1)) & lmb <= mean(c(-1,-.5)) ~ '- 1: 1/Y        [inverse]',
      lmb > mean(c(-1,-.5)) & lmb <= mean(c(-.5,0)) ~ '-.5: 1/sqrt(Y)  [Inverse Root]',
      lmb > mean(c(-.5,0)) & lmb <= mean(c(0,.5))   ~ '  0: log(Y)     [Logarithmic]',
      lmb > mean(c(0,.5)) & lmb <= mean(c(.5,1))    ~ '+.5: sqrt(Y)    [Root]',
      lmb > mean(c(.5,1)) & lmb <= mean(c(1,2))     ~ '+ 1: Y          [No transformation necessary]',
      lmb > mean(c(1,2)) & lmb <= 2                 ~ '+ 2: Y^2        [Quadratic]]',
      .default = 'you should not be using the method'
    )
  }
  
  BondTransf<-function(xt){
    chkPkg("nortest")
    ptest <- nortest::pearson.test(xt)
    unname(ptest$statistic / ptest$df)
  }
  
  # Automated Interpretation of Indices of Effect Size
  # https://cran.r-project.org/web/packages/effectsize/vignettes/interpret.html
  # https://github.com/easystats/effectsize/blob/main/R/interpret.R
  
  InterP<-function(x) {
    dplyr::case_when(
      x <= .001   ~ "***",
      x <= .01    ~ "** ",
      x <= .05    ~ "*  ",
      x <  .1     ~ ".  ",
      .default =    "   ")
  }
  
  InterpEta<-function(e) {
    #interpret_omega_squared(x, rules = "field2013")
    # ES < 0.01 - Very small
    # 0.01 <= ES < 0.06 - Small
    # 0.06 <= ES < 0.14 - Medium
    # ES >= 0.14 - Large
    e=abs(e)
    ValCorte= c(.01,.06,.14)
    r_e <- dplyr::case_when(
      e <   ValCorte[1] ~ "Very small",
      e <   ValCorte[2] ~ "Small",
      e <   ValCorte[3] ~ "Medium",
      e >=  ValCorte[3] ~ "Large",
      .default = "Undetermined")
    
    # if (e<.01) r_e="Very Small"
    # if (data.table::between(e, .01, .06)) r_e="Small"
    # if (data.table::between(e, .06, .14)) r_e="Medium"
    # if (e>.14) r_e="Large"
    r_e
  }
  
  InterpdCohen<-function(e) {
    # interpret_cohens_d(x, rules = "cohen1988")
    # d < 0.2 - Very small
    # 0.2 <= d < 0.5 - Small
    # 0.5 <= d < 0.8 - Medium
    # d >= 0.8 - Large
    e=abs(e)
    ValCorte= c(.2,.5,.8)
    r_e <- dplyr::case_when(
      e <   ValCorte[1] ~ "Very small",
      e <   ValCorte[2] ~ "Small",
      e <   ValCorte[3] ~ "Medium",
      e >=  ValCorte[3] ~ "Large",
      .default = "Undetermined")
    
    # if (e<.2) r_e="Very Small"
    # if (data.table::between(e, .2, .5)) r_e="Small"
    # if (data.table::between(e, .5, .8)) r_e="Medium"
    # if (e>.8) r_e="Large"
    r_e
  }
  
  InterpExplana<-function(e) {
    e=abs(e)
    ValCorte= c(.15,.35,.5)
    r_e <- dplyr::case_when(
      e <   ValCorte[1] ~ "Very small",
      e <   ValCorte[2] ~ "Small",
      e <   ValCorte[3] ~ "Medium",
      e >=  ValCorte[3] ~ "Large",
      .default = "Undetermined")
    
    # if (e<.15) r_e="Very Small"
    # if (data.table::between(e, .15, .35)) r_e="Small"
    # if (data.table::between(e, .35, .5)) r_e="Medium"
    # if (e>.5) r_e="Large"
    r_e
  }
  
  InterpQS<-function(e) {
    e=abs(e)
    ValCorte= c(.55,.65,.7)
    r_e <- dplyr::case_when(
      e <   ValCorte[1] ~ "Very small",
      e <   ValCorte[2] ~ "Small",
      e <   ValCorte[3] ~ "Medium",
      e >=  ValCorte[3] ~ "Large",
      .default = "Undetermined")
    
    # if (e<.55) r_e="Very Small"
    # if (data.table::between(e, .55, .65)) r_e="Small"
    # if (data.table::between(e, .65, .70)) r_e="Medium"
    # if (e>.7) r_e="Large"
    r_e
  }
  
  InterpDeltaWilcox<-function(e) {
    e=abs(e)
    ValCorte= c(.1,.3,.5)
    r_e <- dplyr::case_when(
      e <   ValCorte[1] ~ "Very small",
      e <   ValCorte[2] ~ "Small",
      e <   ValCorte[3] ~ "Medium",
      e >=  ValCorte[3] ~ "Large",
      .default = "Undetermined")
    
    # if (e<.10) r_e="Very Small"
    # if (data.table::between(e, .10, .30)) r_e="Small"
    # if (data.table::between(e, .30, .50)) r_e="Medium"
    # if (e>.50) r_e="Large"
    r_e
  }
  
  #e=0.3939394; valpas=c(.46,.38,.31)
  InterpEfGen<-function(e,valpas) {
    e=abs(e)
    valpas=abs(valpas)
    if (valpas[1]>valpas[2])  {
      if (e>valpas[1]) r_e="Very Small"
      if (data.table::between(e, valpas[2], valpas[1])) r_e="Small"
      if (data.table::between(e, valpas[3], valpas[2])) r_e="Medium"
      if (e<valpas[3]) r_e="Large"
    }
    if (valpas[1]<valpas[2]) {
      if (e<valpas[1]) r_e="Very Small"
      if (data.table::between(e, valpas[1], valpas[2])) r_e="Small"
      if (data.table::between(e, valpas[2], valpas[3])) r_e="Medium"
      if (e>valpas[3]) r_e="Large"
    }
    r_e
  }
  
  # library(effectsize)
  # interpret_bf
  # interpret
  # interpret_cohens_g
  # frmMM(ValCorte,3)
  #log(ValCorte)
  #log10(ValCorte)
  # InterpBF(3.1)
  InterpBF<-function(e) {
    # Jeffreys (1961) ("jeffreys1961"; default)
    # o	BF = 1 - No evidence
    # o	1 < BF <= 3 - Anecdotal
    # o	3 < BF <= 10 - Moderate
    # o	10 < BF <= 30 - Strong
    # o	30 < BF <= 100 - Very strong
    # o	BF > 100 - Extreme.
    # Raftery (1995) ("raftery1995")
    # o	BF = 1 - No evidence
    # o	1 < BF <= 3 - Weak
    # o	3 < BF <= 20 - Positive
    # o	20 < BF <= 150 - Strong
    # o	BF > 150 - Very strong
    
    #BasLogIn=c(3, 10, 10^2, 10^9, 10^10)
    BasLogIn=c(3, 10, 20, 30, 100, 150)
    ValCorte= c(rev(1/BasLogIn),1, BasLogIn)
    
    # 1,  3,    10,   20,   30,   100,   150
    #     1/3,  1/10, 1/20, 1/30, 1/100, 1/150
    #if (Log10)  ValCorte=log10(ValCorte)
    #if (!Log10)  ValCorte=log(ValCorte)
    res <- dplyr::case_when(
      e < 0 ~ "Impossible",
      e <= ValCorte[1] ~ "Contrary Exteme/Very strong",
      e <= ValCorte[2] ~ "Contray Extreme/Strong",
      e <= ValCorte[3] ~ "Contray Very strong/Strong",
      e <= ValCorte[4] ~ "Contray Strong",
      e <= ValCorte[5] ~ "Contray Strong/Positive",
      e <= ValCorte[6] ~ "Contray Moderate/Positive",
      e < ValCorte[6+1] ~ "Contray Anecdotal/Weak",
      
      e == ValCorte[6+1] ~ "No evidence",
      
      e <= ValCorte[6+2] ~ "Anecdotal/Weak",
      e <= ValCorte[6+3] ~ "Moderate/Positive",
      e <= ValCorte[6+4] ~ "Strong/Positive",
      e <= ValCorte[6+5] ~ "Strong",
      e <= ValCorte[6+6] ~ "Very strong/Strong",
      e <= ValCorte[6+7] ~ "Extreme/Strong",
      e > ValCorte[6+7] ~ "Extreme/Very strong",
      .default = "No evidence"
    )
    res
      # if (e<=ValCorte[1]) r_e="Contrary Exteme/Very strong"
      # if (data.table::between(e, ValCorte[1], ValCorte[2])) r_e="Contray Extreme/Strong"
      # if (data.table::between(e, ValCorte[2], ValCorte[3])) r_e="Contray Very strong/Strong"
      # if (data.table::between(e, ValCorte[3], ValCorte[4])) r_e="Contray Strong"
      # if (data.table::between(e, ValCorte[4], ValCorte[5])) r_e="Contray Strong/Positive"
      # if (data.table::between(e, ValCorte[5], ValCorte[6])) r_e="Contray Moderate/Positive"
      # if (data.table::between(e, ValCorte[6], ValCorte[6+1])) r_e="Contray Anecdotal/Weak"
      # 
      # if (e<=ValCorte[6+1]) r_e="No evidence"
      # 
      # if (data.table::between(e, ValCorte[6+1], ValCorte[6+2])) r_e="Anecdotal/Weak"
      # if (data.table::between(e, ValCorte[6+2], ValCorte[6+3])) r_e="Moderate/Positive"
      # if (data.table::between(e, ValCorte[6+3], ValCorte[6+4])) r_e="Strong/Positive"
      # if (data.table::between(e, ValCorte[6+4], ValCorte[6+5])) r_e="Strong"
      # if (data.table::between(e, ValCorte[6+5], ValCorte[6+6])) r_e="Very Strong/Strong"
      # if (data.table::between(e, ValCorte[6+6], ValCorte[6+7])) r_e="Extrem/Strong"
      # if (e>ValCorte[6+7]) r_e="Extreme/Very strong"
      # 
      # r_e
  }
  
  
  SummToTable<-function(SummPas) {
    ResT<-list()
    ANOVAPrv<-data.table(coef(SummPas), keep.rownames = 'term')
    LongT=ncol(ANOVAPrv)-1
    for (i in (2:LongT))ANOVAPrv[[i]]<-as.numeric(frmMM(ANOVAPrv[[i]],2))
    Lasp=ANOVAPrv[[LongT+1]]
    mystars <- ifelse(Lasp <.001, "***", ifelse(Lasp < .01, "**", 
                                                ifelse(Lasp < .05, "* ", ifelse(Lasp < .1,".","  "))))
    ANOVAPrv[[LongT+1]]<-paste(frmMM(Lasp,4), mystars, sep="")
    ResT$APA<-ANOVAPrv
    ResT$Kbl<-ANOVAPrv %>% 
      kbl(caption=paste0("ANOVA Table"),
          digits=4,escape = F, align = "c")  %>%
      kable_paper("hover", full_width = F,font_size = 10) %>%
      kable_styling(fixed_thead = T) %>%
      footnote(general = "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1") %>%
      scroll_box(width = "100%", height = "800px")
    ResT
  }
  
  ETM <- function(x, na.rm=FALSE) {sqrt(var(x,na.rm = na.rm)/NNa(x,na.rm))}
  
  SEM = ETM
  
  dnormvarMM<-function(x){x^2 * dnorm(x)}
  
  #' Combine matrices with different dimensions, add NA for additional rows/columns
  #' Used in outputting the DTR tree.
  #' @param m1 Matrix 1.
  #' @param m2 Matrix 2.
  combine.mat<-function(m1,m2,by="column"){
    nrow1<-nrow(m1);ncol1<-ncol(m1)
    nrow2<-nrow(m2);ncol2<-ncol(m2)
    if(by=="column"){
      combine<-matrix(NA,max(nrow1,nrow2),ncol1+ncol2)
      combine[1:nrow1,1:ncol1]<-m1
      combine[1:nrow2,(ncol1+1):(ncol1+ncol2)]<-m2
    }
    if(by=="row"){
      combine<-matrix(NA,nrow1+nrow2,max(ncol1,ncol2))
      combine[1:nrow1,1:ncol1]<-m1
      combine[(nrow1+1):(nrow1+nrow2),1:ncol2]<-m2
    }
    return(combine)
  }
  
  # GenMultCon(3)
  GenMultCon<-function(nLev) {
    cont=0;
    nContr=(nLev*(nLev-1))/2
    Contr<-matrix(data=0,ncol=nContr,nrow=nLev)
    for (i in (1:(nLev-1))) {
      for (j in ((i+1):(nLev))) {
        cont=cont+1
        Contr[i,cont]=1;Contr[j,cont]=-1
      }
    }
    Contr
  }
  
  # mkIdxJ(c("A","B","C"))
  # mkIdxJ(c(1:3))
  mkIdxJ<-function(nv) {
    nLev=length(nv)
    k <- 1; il <- vector()
    for (i in (1:(nLev-1))) {
      for (j in ((i+1):(nLev))) {
        il[k]<-paste(nv[i],nv[j],sep=".")
        #jl[k]<-nv[j]
        k<- k+1
      }
    }
    return(il)
  }
  
  stats_MM <- function(x) {
    nadt <-0
    summ<-summary(x)
    if (length(summ)>6) nadt= summ[[7]]
    n = length(x)-nadt
    min <- frmMM(min(x, na.rm = TRUE),2)
    max <- frmMM(max(x, na.rm = TRUE),2)
    mean <- frmMM(mean(x, na.rm = TRUE),2)
    med <- frmMM(median(x, na.rm = TRUE))
    trimm = frmMM(mean(x, na.rm = TRUE,trim=.2),2)
    Q1 <- frmMM(summ[[2]],2)
    Q3 <- frmMM(summ[[5]],2)
    Iqr <- frmMM(summ[[5]]-summ[[2]],2)
    sd <- frmMM(sd(x, na.rm =TRUE),2)
    se <- frmMM(sd(x, na.rm =TRUE)/sqrt(n),2)
    mad <- frmMM(mad(x, na.rm = TRUE),2)
    summary <- data.table(n=n, 'NAs' = nadt, Min = min, '1st Qu'=Q1, 
                          Median=med, Mean = mean, 'Trimmed(20%)' = trimm,
                          '3rd Qu'=Q3, Max = max, 
                          SD = sd, SEM = se, IQR = Iqr, MAD = mad)
    summary
  }
  
}

# ▲▲▲================== Statistics: General ===================▲▲▲  
  



# ▼▼▼================== Statistics: Robust ====================▼▼▼
# They are debugged
{
  # To prepare data for Wilcox functions
  MtxToLst<-function(m,DepV="N",Dec=2) {
    # Extract information from a redundant matrix
    mm<-copy(m)
    mm<-round(mm,Dec)
    mm[lower.tri(mm,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
    mm<-na.omit(as.data.table(as.table(mm)))  #Turn into a 3-column table
    setkey(mm,N)
    names(mm)[3]<-DepV
    mm
  }
  
  MtrxWilk2f <-function(Dtp,lv1,lv2,DepV) {
    Res<-NA
    Interm2x2<-  lapply(1:2, function (x) lapply(1:2, function(y) 
      Dtp[.(lv1[x],lv2[y],with=F),..DepV][[1]]))
    Res<-(unlist(Interm2x2,recursive = F))
    Res
  } # Prepare two-way designs for Wilcox Data format
  
  #Dtp=SelAxis;lv1=levels(DT$Group);DepV="Perc" 
  MtrxWilk1f <-function(Dtp,lv1,DepV) {
    lapply(1:2, function (x) 
      Dtp[.(lv1[x],with=F),..DepV][[1]])
  } # Prepare one-way designs for Wilcox Data format
  
  #formula=Memoria ~ Grupos
  #data=EstudOrig.b
  ChNmMod<-function(formula, data) {
    # To change the name of the variables to Grp and DepV,
    #  and I make sure that it is data.frame and Grp is as factor  
    if (missing(data)) {
      mf <- model.frame(formula)
    }else {
      mf <- model.frame(formula, data)
    }
    dataDF<-copy(data)
    LosNm<-all.vars(formula)
    setnames(dataDF,eval(LosNm[1]),"DepV")
    setnames(dataDF,eval(LosNm[2]),"Grp")
    dataDF<-as.data.frame(dataDF)
    dataDF$Grp<-factor(dataDF$Grp)
    dataDF
  }
  
  # split data from formula 
  DatFrmMod<-function(formula, data) {
    # split data from formula 
    if (missing(data)) {
      mf <- model.frame(formula)
    }else {
      mf <- model.frame(formula, data)
    }
    split(model.extract(mf, "response"), mf[, 2])
  }
  
  # Pre-stores values that I have already calibrated
   # Examples of use:
   # Elest="DTS"
   # RefSizes[,get(Elest)]
   # RefSizes[,Wass]
  RefSizes=data.table(AKP=c(.1,.3,.5),QS=c(.55,.65,.70),SIGN=c(.46,.38,.31),KS=c(.08,.2,.32),Kuiper=c(.08,.2,.32),
                      Wass=c(.2,.5,.8), Lp=c(.2,.5,.8), CVM=c(75.14,440.54,1098.32), AD= c(3964568,22889998,56219919), 
                      DTS= c(63.57,	151.03,	239.47))
  
  Trg<-function(x,tr=.2,na.rm=FALSE) {
    # Winsorize the data y permite omitir o no los NA en la lógica de sustitución de los valores
    # From Wilcox: Trimm for the data in the vector x, tr is the amount of Winsorization
    # Trg es como winval de  Wilcox, pero discrepa al quitar los NA automáticamente
    # Corregir pues le faltaba la linea n<-length(x)
    if (!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
      warning("argument is not numeric or logical: returning NA")
      return(NA_real_)
    }
    if (!is.numeric(tr) || length(tr) != 1L) stop("'trim' must be numeric of length one")
    n<-length(x)
    if (tr > 0 && n) {
      if (is.complex(x)) stop("trimmed sample is not defined for complex data")
      if (any(is.na(x)) && !na.rm) warning("Missing values ere automatically removed but
                                         the sample size has not been adjusted accordingly")
      if (tr >= 0.5) warning("trim >= 0.5 is odd...trying it anyway")
      if(na.rm)x<-x[!is.na(x)]
      y<-sort(x)
      ibot<-floor(tr*length(x))+1
      itop<-length(x)-ibot+1
      xbot<-y[ibot];xtop<-y[itop]
      x[x<=xbot]<-xbot;x[x>=xtop]<-xtop
    }
    x
  }
  
  winvarMM<-function(x,tr=.2,na.rm=FALSE,rees=FALSE) {
    wv=var(na.omit(Trg(x,tr,na.rm)))
    if (rees) {
      #require(MASS)
      cterm=ifelse(tr==0,1,area(dnormvarMM,qnorm(tr),qnorm(1-tr))+2*(qnorm(tr)^2)*tr)
      wv=wv/cterm
    }
    wv
  } #As Wilcox' "winvar" (Winsor Var), and "winvarN" (reescale Winsor Var)
  
  winMM<-function(x,tr=.2,na.rm=FALSE){mean(na.omit(Trg(x,tr,na.rm)))} #As Wilcox' "win", Winsor Mean
  
  trimseMM<-function(x,tr=.2,na.rm=FALSE){sqrt(winvarMM(x,tr,na.rm))/((1-2*tr)*sqrt(NNa(x,na.rm)))} #As Wilcox' "trimse" Trimmed SEM
  
  # Old, in 2-variable format
  # wincorMM<-function(x,y=NULL,tr=.2){
  {#   #   Winsorized correlation between x and y (tr is the amount of Winsorization)
  #   #   Also returns the Winsorized covariance
  #   #   Pairwise deletion of missing values is performed.
  #   if (is.null(y[1])){y = x[, 2];x = x[, 1]}
  #   sig<-NA
  #   if(length(x)!=length(y))stop("Lengths of vectors are not equal")
  #   m1=na.del(cbind(x,y))
  #   g<-floor(tr*nrow(m1))
  #   vec<-cbind(Trg(m1[,1],tr),Trg(m1[,2],tr))
  #   wcor<-cor(vec)[1,2]
  #   wcov<-var(vec)[1,2]
  #   if(sum(x==y,na.rm = T)!=nrow(m1)){
  #     test<-wcor*sqrt((nrow(m1)-2)/(1.-wcor^2))
  #     sig<-as.numeric(as.character(frmMM(2*(1-pt(abs(test),nrow(m1)-2*g-2)),10)))
  #   }
  #   list(cor=wcor,cov=wcov,siglevel=sig,n=nrow(m1))
  # } #As Wilcox' "wincor", and "wincor.sub"
  }
  
  #x=DTa
  #x=DTc;TyOut = "Lst"; tr=.2
  wincorMM<-function(x,tr=.2,TyOut="Mx") {
    #   Winsorized correlation between cols of a Matrix or data.frame (x)
    #   tr: the amount of Winsorization
    #   TyOut: Type Output Matrix (Mx), List (Lst) Table (Tbl)
    #   Also returns the Winsorized covariance, and p.val
    #   Pairwise deletion of missing values is performed.
    sig<-NA; Res<-NA
    m1<-as.data.table(na.omit(x))
    g<-floor(tr*nrow(m1))
    m1<-m1[,lapply(.SD, Trg,tr)]
    wcor <- m1[,cor(.SD)]
    wcov <- m1[,var(.SD)]
    test<-wcor*sqrt((nrow(m1)-2)/(1.-wcor^2))
    sig<-ifelse(is.infinite(test),NA,
                as.numeric(as.character(frmMM(2*(1-pt(abs(test),nrow(m1)-2*g-2)),10))))
    sig.adj<-p.adjust(sig,"holm")
    sg2<-matrix(sig.adj,byrow = T,nrow=nrow(sig))
    colnames(sg2)=row.names(sg2)=row.names(sig)
    if (length(m1)<3) {Res<-list(n=nrow(m1), cor=wcor[1,2], cov=wcov[1,2],
                                 p.val=sig[1,2],p.Sig=ExtrSig(sig[1,2])$p.Sig)}
    if (length(m1)>2 && TyOut=="Mx") {Res<-list(n=nrow(m1), cor=wcor,cov=wcov,p.val=sig,p.adj=sg2)}
    if (length(m1)>2 && TyOut=="Lst") {
      Res<-list(n=nrow(m1), 
                cor=MtxToLst(wcor,"Cor"), cov=MtxToLst(wcov,"Cov"),
                p.val=cbind(MtxToLst(sig,"p.val"), p.Sig=ExtrSig(MtxToLst(sig,"p.val")$p.val)$p.Sig),
                p.Adj=cbind(MtxToLst(sg2,"p.adj"), p.Sig.Adj=ExtrSig(MtxToLst(sg2,"p.adj")$p.adj)$p.Sig)
      )
    }
    if (length(m1)>2 && TyOut=="Tbl") {
      corLst=MtxToLst(wcor,"Cor")
      covLst=MtxToLst(wcov,"Cov")
      p.valLst=MtxToLst(sig,"p.val")
      p.valLst=cbind(p.valLst,p.Sig=ExtrSig(p.valLst$p.val)$p.Sig)
      p.adjLst=MtxToLst(sg2,"p.adj")
      p.adjLst=cbind(p.adjLst,p.Sig.Adj=ExtrSig(p.adjLst$p.adj)$p.Sig)
      
      Mrg<-merge(merge(merge(corLst,covLst,c("V1","V2")),p.valLst,c("V1","V2")),p.adjLst,c("V1","V2"))
      setkey(Mrg,Cor)
      Res<-list(n=nrow(m1), All=Mrg)
    }
    if (TyOut=="APA") {
      corLst=MtxToLst(wcor,"Cor")
      p.valLst=MtxToLst(sig,"p.val")
      #p.valLst=cbind(p.valLst,p.Sig=ExtrSig(p.valLst$p.val)$p.Sig)
      p.adjLst=MtxToLst(sg2,"p.adj")
      #p.adjLst=cbind(p.adjLst,p.Sig.Adj=ExtrSig(p.adjLst$p.adj)$p.Sig)
      Mrg<-merge(merge(corLst,p.valLst,c("V1","V2")),p.adjLst,c("V1","V2"))
      setkey(Mrg,Cor)
      Res<- paste0(Mrg$V1, " vs. ",Mrg$V2,
                   ": R(",nrow(m1)-2*g-2,", N=",nrow(m1),")=", numf(Mrg$Cor),
                   ", p=", numf(Mrg$p.val,3),
                   ", p.Adj=", numf(Mrg$p.adj,3)
      )
    }
    Res
  } #As Wilcox' "wincor", and "wincor.sub"
  
  #ii<-m2;tr=.2
  PowBas<-function(ii,tr=.2) {
    e.pow=NA
    Res<-ii[,.(N = sum(!is.na(DepV)),MeanTR=mean(DepV,tr=tr,na.rm=T)),by=.(Grp)]
    top=Res[,var(MeanTR)]
    bot=ii[,winvarMM(DepV,tr=tr,na.rm = T,rees = T)]
    e.pow=fifelse(bot==0,1,top/bot)
    if(e.pow>=1) e.pow=wincorMM(cbind(rep(c(1:nrow(Res)),c(Res$N)),c(na.omit(ii$DepV))),tr=tr)$cor^2
    e.pow
  } # With data.table
  
  # Less effective but interesting alternative option for the use of dplyr
  # Res2<-mclapply (1:nboot, function(ib) {
  #         ii %>%
  #         .[,sample(na.omit(DepV),min(Res$N)),by=.(Grp)] %>%
  #         dplyr::rename(.,DepV=V1) %>%
  #         PowBas(.)
  #       },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE)
  
  # De momento solo sirve para PB=FALSE
  # después tendré q incorporar el caso de PB=TRUE
  PowBasS<-function(x,tr=.2) {
    # Versión básica más eficiente pero requiere previamente split pues trabaja con listas
    e.pow=NA
    grp <- 1:length(x); J <- length(grp)
    N<-sapply(grp, function(j)sum(!is.na(x[[j]])))
    MeanTR<-sapply(grp, function(j){mean(x[[j]], tr=tr,na.rm=T)})
    top=var(MeanTR)
    bot=winvarMM(unlist(x),tr=tr,na.rm = T,rees = T)
    e.pow=ifelse(bot==0,1,top/bot)
    if(e.pow>=1) e.pow=wincorMM(cbind(rep(grp,N),c(na.omit(unlist(x)))),tr=tr)$cor^2
    e.pow
  } # With apply
  
  RobEffS<-function(xsp,tr=.2,loc.fun=median,PB=FALSE,nboot=100, SEED=TRUE) {
    # Versión básica más eficiente pero requiere previamente split pues trabaja con listas
    e.pow2=NA
    #print(xsp)
    grp <- 1:length(xsp); J <- length(grp)
    N<-sapply(grp, function(j)sum(!is.na(xsp[[j]])))
    chkn=var(N)
    e.pow2=ifelse(chkn==0,
                  PowBasS(xsp,tr=tr),
                  { Res2<-mclapply (1:nboot, function(ib) {
                    PowBasS(lapply(grp, function(j){sample(xsp[[j]], min(N))}),tr=tr)
                  },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE, mc.set.seed = !SEED)
                  loc.fun(unlist(Res2),na.rm=TRUE)
                  })
    sqrt(e.pow2)
  } # With formula & apply
  
  # formula<-DepV1~Grp1; data =DTa
  RobEff<-function(formula,data,tr=.2,loc.fun=median,PB=FALSE,nboot=100, SEED=TRUE) {
    if (SEED)set.seed(2)
    if (missing(data)) {
      mf <- model.frame(formula)
    }else {
      mf <- model.frame(formula, data) #na.action = NULL xq me interesa elminar los na
    }
    m2<-as.data.table(mf)
    setnames(m2,names(mf)[1],"DepV")
    setnames(m2,names(mf)[2],"Grp")
    e.pow2=NA
    Res<-m2[,.(.N),by=.(Grp)]
    chkn=Res[,var(N)]
    e.pow2=fifelse(chkn==0,
                   PowBas(m2,tr=tr),
                   { Res2<-mclapply (1:nboot, function(ib) {
                     PowBas(m2[,.(DepV=sample(DepV,min(Res$N))),by=.(Grp)],tr=tr)
                   },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE, mc.set.seed = !SEED)
                   loc.fun(unlist(Res2),na.rm=TRUE)
                   })
    sqrt(e.pow2)
  } # With formula & data.table
  
  # DTFisp<-DTFis
  # IdSj="Subject";wv1="Class";wv2="Day";LaVd="Rate"
  RobSis<- function(DTFisp,IdSj="Subject",wv1="Class",wv2="Day",LaVd="Rate") {
    RsAOV<-list()
    RsAOVOmn<-NA; RobAOvAPA.1<-NA; RsAOVOmn2<-NA
    DTFisDif<-NA; DTp2<-NA
    DTp2 <- DTFisp %>% 
      dplyr::rename(IdSuj=all_of(IdSj)) %>% 
      dplyr::rename(x1=all_of(wv1)) %>% 
      dplyr::rename(x2=all_of(wv2)) %>%
      dplyr::rename(y=all_of(LaVd)) %>%
      mutate(x1=factor(x1)) %>%
      mutate(x2=factor(x2)) %>%
      mutate(IdSuj=factor(IdSuj)) %>%
      mutate(Inter:=factor(interaction(x1,x2))) %>%
      data.table() %>%
      setkey(.,Inter)
    
    NumL=length(levels(DTp2$Inter)); vector=c(1:NumL)
    lv1=length(levels(DTp2$x1));lv2=length(levels(DTp2$x2))
    
    # Data structure for Wilcox functions
    ArP=lapply(1:NumL, function (x) DTp2[.(levels(DTp2$Inter)[x]),"y",with=F][[1]])
    
    # A) Robust AOV with wwtrim
    RsAOVOmn<-bwtrim(lv2,lv1,ArP)
    try(RsAOVOmn2<-bwtrimbt_vMM(lv2,lv1,ArP))
    
    RobAOvAPA.1 <- with(RsAOVOmn, paste0(wv2,": Fw(1) = ",round(Qa,3), ", p = ",round(Qa.p.value,3),
                                         ", ",wv1,": Fw(1) = ",round(Qb,3), ", p = ",round(Qb.p.value,3),
                                         ", ",wv2,"*",wv1, ": Fw(1) = ",round(Qab,3),
                                         ", p = ",round(Qab.p.value,3)))
    
    RobAOvTXT<-round(unlist(RsAOVOmn),4)
    RobAOvAPA.2<-paste0("[",wv1,": ", "V(",lv1-1,") = ", 
                        frmMM(RobAOvTXT[3],2), ", p = ", frmMM(RobAOvTXT[4],4), "; ",
                        wv2, ": ", "V(",lv2-1,") = ",
                        frmMM(RobAOvTXT[1],2), ", p = ", frmMM(RobAOvTXT[2],4), "; ",
                        "Interaction ",wv1, " x ", wv2,": ", "V(",(lv1-1)*(lv2-1),") = ", 
                        frmMM(RobAOvTXT[5],2), ", p = ", frmMM(RobAOvTXT[6],4),"]" )
    
    # Resultados con un estilo
    RobAOvAPA.1
    # Resultados con otro estilo
    RobAOvAPA.2
    
    RsAOV$Omn1<-RobAOvAPA.1
    RsAOV$Omn2<-RobAOvAPA.2
    RsAOV$Omn3<-RsAOVOmn2
    
    # Effect Size old
    TamA=mean(ESmainMCP(lv2,lv1,ArP)$Factor.A[,3])
    TamB=mean(ESmainMCP(lv2,lv1,ArP)$Factor.B[,3])
    TamInt=mean(esImcp(lv2,lv1,ArP)$Effect.Sizes)
    AllTam = c(TamA,TamB,TamInt)
    RsAOV$EffSize=sapply(AllTam, frmMM)
    
    TamInterp= unlist(lapply(AllTam, InterpExplana))
    
    # Effect Size New
    DTFisDif <- DTp2 %>% 
      group_by(IdSuj) %>% 
      mutate(Difference = y[x1 == levels(x1)[1]] - y[x1 == levels(x1)[2]]) %>%
      filter(.,x1 == levels(x1)[1]) %>%
      dplyr::select(.,-x1) %>%
      data.table()
    
    x1.Spl <- split(DTp2$y, DTp2$x1)
    x1.Ef=yuendv2(x1.Spl[[1]],x1.Spl[[2]],pr=F)$Effect.Size
    x1.Ef.b=rmES.pro(x1.Spl)$effect.size
    # dep.ES.summary.CI(x[[1]],x[[2]])
    
    x2.Spl<-split(DTp2$y, DTp2$x2)
    x2.Ef= t1wayv2(x2.Spl)$Effect.Size
    
    setkey(DTFisDif,x2)
    ArPDif=lapply(1:lv2, function (x) DTFisDif[.(levels(DTFisDif$x2)[x]),"Difference",with=F][[1]])
    xInter.Ef= t1wayv2(ArPDif)$Effect.Size
    
    RsAOV$EffSize2=round(c(x2.Ef,x1.Ef,xInter.Ef),2)
    
    RobAOvAPA.3<-paste0("[",wv1,": ", "V(",lv1-1,") = ", 
                        frmMM(RobAOvTXT[3],2), ", p = ", frmMM(RsAOVOmn2[[2]],4), ", ",
                        greek$xi," = ", frmMM(x1.Ef,2), " (",TamInterp[2], " effect); ",
                        wv2, ": ", "V(",lv2-1,") = ",
                        frmMM(RobAOvTXT[1],2), ", p = ", frmMM(RsAOVOmn2[[1]],4), ", ",
                        greek$xi," = ", frmMM(x2.Ef,2), " (",TamInterp[1], " effect); ",
                        "Interaction ",wv1, " x ", wv2,": ", "V(",(lv1-1)*(lv2-1),") = ", 
                        frmMM(RobAOvTXT[5],2), ", p = ", frmMM(RsAOVOmn2[[3]],4), ", ",
                        greek$xi," = ", frmMM(TamInt,2), " (",TamInterp[3], " effect)",
                        "]" )
    
    RsAOV$Omn4<-RobAOvAPA.3
    
    # B) Simple effect (with yuenv2) and Rom vs BH Pos Hoc for Between,
    # and with yuendv2 and Rom vs BH Pos Hoc for Within
    # B.1) Direction Class (within) on each Day (Between)
    cnt=0; yd<-list();LapAd<-NULL;LapAd2<-NULL
    for (i2 in 1:lv2) {
      LaP=NULL
      for (i in (1:(lv1-1))) {
        for (j in ((i+1):(lv1))) {
          cnt=cnt+1
          sample1<-DTp2[x2==levels(DTp2$x2)[i2]&x1==levels(DTp2$x1)[i],y] 
          sample2<-DTp2[x2==levels(DTp2$x2)[i2]&x1==levels(DTp2$x1)[j],y] 
          out=c(unlist(yuendv2(sample1,sample2)))
          yd[[cnt]]<-as.data.table(rbind(out))
          LaP=c(LaP,yd[[cnt]]$p.value)
          yd[[cnt]]<-data.table(cbind(IV1=levels(DTp2$x2)[i2],IV2.a=levels(DTp2$x1)[i],
                                      IV2.b=levels(DTp2$x1)[j],yd[[cnt]]))
        }
      }
      #LapAd<-c(LapAd,p.adjust(LaP, "holm"))
      LapAd2<-c(LapAd2,p.adjust(LaP, "BH")) # Optimo
      if (length(LaP)==1) LapAd<-c(LapAd,(LaP))
      if (length(LaP)>1) LapAd<-c(LapAd,adjustRom(LaP))
    }
    
    ext1<-ExtrSig(LapAd); names(ext1)<-c("p.Rom","Sig.Rom")
    ext2<-ExtrSig(LapAd2); names(ext2)<-c("p.BH","Sig.BH")
    ResPosRob<-data.table(do.call("rbind", yd),ext1,ext2)
    ResPosRob$p.value=frmMM( ResPosRob$p.value,4)
    for (i in (c(4,5,7:11,14)))ResPosRob[[i]]<-as.numeric(frmMM(ResPosRob[[i]],2))
    ResPosRob<-data.table(ResPosRob, Tam= unlist(lapply(ResPosRob$Effect.Size, InterpExplana)))
    setorder(ResPosRob, -Sig.BH)
    
    #with (ResPosRob[1,], paste0(IV1,": ",IV2.a," - ",IV2.b," = ",dif,": ","tw(",df,") = ",
    #                               teststat, "; p = ", p.V1, "; \U1D6CF =",
    #                               Effect.Size, " (",Tam," effect)"))
    
    ResAPA<-lapply(1:nrow(ResPosRob), function(i) with (ResPosRob[i,], paste0(IV1,": ",IV2.a," - ",IV2.b," = ",dif,": ",
                                                                              "tw(",df,") = ", teststat,
                                                                              "; pROM = ", p.Rom,
                                                                              "; pBH = ", p.BH, "; ",
                                                                              greek$xi," = ", Effect.Size, " (",Tam," effect)")))
    ResAPA2<-data.table(do.call("rbind",ResAPA))
    
    RsAOV$SimplEfa.1<- ResPosRob
    RsAOV$SimplEfa.2<- ResAPA2
    
    ResPosRob
    ResAPA2
    
    # Guía para texto paper
    # F(1, 17) = 4.43, p = .050, ηG2 = .13 (medium effect)
    # t(33.00) = −0.56, p = .582, dpair = −0.28 (small effect)
    
    
    # B.2) Direction Day (Between) on each Class (within)
    cnt=0; yd<-list();LapAd<-NULL;LapAd2<-NULL
    for (i2 in 1:lv1) {
      LaP=NULL
      for (i in (1:(lv2-1))) {
        for (j in ((i+1):(lv2))) {
          cnt=cnt+1
          sample1<-DTp2[x2==levels(DTp2$x2)[i]&x1==levels(DTp2$x1)[i2],y] 
          sample2<-DTp2[x2==levels(DTp2$x2)[j]&x1==levels(DTp2$x1)[i2],y]
          out=c(unlist(yuenv2(sample1,sample2)))
          yd[[cnt]]<-as.data.table(rbind(out))
          LaP=c(LaP,yd[[cnt]]$p.value)
          yd[[cnt]]<-data.table(cbind(IV1=levels(DTp2$x1)[i2],
                                      IV2.a=levels(DTp2$x2)[i],IV2.b=levels(DTp2$x2)[j],yd[[cnt]]))
          
        }
      }
      #LapAd<-c(LapAd,p.adjust(LaP, "holm"))
      LapAd2<-c(LapAd2,p.adjust(LaP, "BH")) # Optimo
      if (length(LaP)==1) LapAd<-c(LapAd,(LaP))
      if (length(LaP)>1) LapAd<-c(LapAd,adjustRom(LaP))
    }
    
    ext1<-ExtrSig(LapAd); names(ext1)<-c("p.Rom","Sig.Rom")
    ext2<-ExtrSig(LapAd2); names(ext2)<-c("p.BH","Sig.BH")
    ResPosRob2<-data.table(do.call("rbind", yd),ext1,ext2)
    ResPosRob2$p.value=frmMM( ResPosRob2$p.value,4)
    for (i in (c(4,5,9:15)))ResPosRob2[[i]]<-as.numeric(frmMM(ResPosRob2[[i]],2))
    ResPosRob2<-data.table(ResPosRob2, Tam= unlist(lapply(ResPosRob2$Effect.Size, InterpExplana)))  
    
    setorder(ResPosRob2, -Sig.BH)
    #with (ResPosRob[1,], paste0(IV1,": ",IV2.a," - ",IV2.b," = ",dif,": ","tw(",df,") = ",
    #                               teststat, "; p = ", p.V1, "; \U1D6CF =",
    #                               Effect.Size, " (",Tam," effect)"))
    
    ResAPAb<-lapply(1:nrow(ResPosRob2), function(i) with (ResPosRob2[i,], paste0(IV1,": ",IV2.a," - ",IV2.b," = ",dif,": ",
                                                                                "tw(",df,") = ", teststat,
                                                                                "; pROM = ", p.Rom,
                                                                                "; pBH = ", p.BH,
                                                                                "; ", greek$xi," = ", Effect.Size, " (",Tam," effect)")))
    ResAPA2b<-data.table(do.call("rbind",ResAPAb))
    
    RsAOV$SimplEfb.1<- ResPosRob2
    RsAOV$SimplEfb.2<- ResAPA2b
    
    ResPosRob2
    ResAPA2b
    
    RsAOV$Dif<-DTFisDif[,c(1,2,5)]
    
    RsAOV
    
  }
  
  RobSis24 <- function(DTFisp,IdSj="Subject",wv1="Context",wv2="Day",LaVd="Rate") {
    RsAOV<-list()
    RsAOVOmn<-NA; RsAOVOmn2<-NULL;
    #RobAOvAPA.1<-NA; 
    DTFisDif<-NA; DTp2<-NA
    DTp2 <- DTFisp %>% 
      dplyr::rename(IdSuj=all_of(IdSj)) %>% 
      dplyr::rename(x1=all_of(wv1)) %>% 
      dplyr::rename(x2=all_of(wv2)) %>%
      dplyr::rename(y=all_of(LaVd)) %>%
      mutate(x1=factor(x1)) %>%
      mutate(x2=factor(x2)) %>%
      mutate(IdSuj=factor(IdSuj)) %>%
      mutate(Inter:=factor(interaction(x1,x2))) %>%
      data.table() %>%
      setkey(.,Inter)
    
    NumL=length(levels(DTp2$Inter)); vector=c(1:NumL)
    lv1=length(levels(DTp2$x1));lv2=length(levels(DTp2$x2))
    
    # Data structure for Wilcox functions
    ArP=lapply(1:NumL, function (x) DTp2[.(levels(DTp2$Inter)[x]),"y",with=F][[1]])
    
    # A) Robust AOV with wwtrim vs bwtrimbt_vMM
    try(RsAOVOmn<-bwtrim(lv2,lv1,ArP))
    try(RsAOVOmn2<-bwtrimbt_vMM(lv2,lv1,ArP))
    RobAOvTXT<-round(unlist(RsAOVOmn),4)
    if(is.null(RsAOVOmn2)) RsAOVOmn2<-RsAOVOmn
    
    # Effect Size 
    DTFisDif <- DTp2 %>% 
      group_by(IdSuj,x2) %>% 
      mutate(Difference = y[x1 == levels(x1)[1]] - y[x1 == levels(x1)[2]]) %>%
      filter(.,x1 == levels(x1)[1]) %>%
      dplyr::select(.,-x1) %>%
      data.table()
    
    x1.Spl <- split(DTp2$y, DTp2$x1)
    x1.Ef=yuendv2(x1.Spl[[1]],x1.Spl[[2]],pr=F)$Effect.Size
    x1.Ef.b=rmES.pro(x1.Spl)$effect.size
    # dep.ES.summary.CI(x[[1]],x[[2]])
    x2.Spl<-split(DTp2$y, DTp2$x2)
    x2.Ef= t1wayv2(x2.Spl)$Effect.Size
    IntEff=mean(esImcp(lv2,lv1,ArP)$Effect.Sizes)
    
    # Prepare all statistics in an organized APA style: Omnibus AOV and Effect Sizes
    RobAOvAPA.1<-paste0("[",wv1,": ", "FW(",lv1-1,") = ", 
                        frmMM(RobAOvTXT[3],2), ", p = ", frmMM(RsAOVOmn2[[2]],4), ", ",
                        greek$xi," = ", frmMM(x1.Ef,2), " (",InterpExplana(x1.Ef), " effect); ",
                        wv2, ": ", "FW(",lv2-1,") = ",
                        frmMM(RobAOvTXT[1],2), ", p = ", frmMM(RsAOVOmn2[[1]],4), ", ",
                        greek$xi," = ", frmMM(x2.Ef,2), " (",InterpExplana(x2.Ef), " effect); ",
                        "Interaction ",wv1, " x ", wv2,": ", "FW(",(lv1-1)*(lv2-1),") = ", 
                        frmMM(RobAOvTXT[5],2), ", p = ", frmMM(RsAOVOmn2[[3]],4), ", ",
                        greek$xi," = ", frmMM(IntEff,2), " (",InterpExplana(IntEff), " effect)",
                        "]" )
    
    RsAOV$Omn<-RobAOvAPA.1
    
    # B) Simple effect (with yuenv2) and Rom vs BH Pos Hoc for Between,
    # and with yuendv2 and Rom vs BH Pos Hoc for Within
    # B.1) Direction Class (within) on each Day (Between)
    cnt=0; yd<-list();LapAd<-NULL;LapAd2<-NULL
    for (i2 in 1:lv2) {
      LaP=NULL
      for (i in (1:(lv1-1))) {
        for (j in ((i+1):(lv1))) {
          cnt=cnt+1
          sample1<-DTp2[x2==levels(DTp2$x2)[i2]&x1==levels(DTp2$x1)[i],y] 
          sample2<-DTp2[x2==levels(DTp2$x2)[i2]&x1==levels(DTp2$x1)[j],y] 
          out=c(unlist(yuendv2(sample1,sample2)))
          yd[[cnt]]<-as.data.table(rbind(out))
          LaP=c(LaP,yd[[cnt]]$p.value)
          yd[[cnt]]<-data.table(cbind(IV1=levels(DTp2$x2)[i2],IV2.a=levels(DTp2$x1)[i],
                                      IV2.b=levels(DTp2$x1)[j],yd[[cnt]]))
        }
      }
      LapAd2<-c(LapAd2,p.adjust(LaP, "BH")) # Optimo
      if (length(LaP)==1) LapAd<-c(LapAd,(LaP))
      if (length(LaP)>1) LapAd<-c(LapAd,adjustRom(LaP))
    }
    
    ext1<-ExtrSig(LapAd); names(ext1)<-c("p.Rom","Sig.Rom")
    ext2<-ExtrSig(LapAd2); names(ext2)<-c("p.BH","Sig.BH")
    ResPosRob<-data.table(do.call("rbind", yd),ext1,ext2)
    ResPosRob$p.value=frmMM( ResPosRob$p.value,4)
    for (i in (c(4,5,7:11,14)))ResPosRob[[i]]<-as.numeric(frmMM(ResPosRob[[i]],2))
    ResPosRob<-data.table(ResPosRob, Tam= unlist(lapply(ResPosRob$Effect.Size, InterpExplana)))
    setorder(ResPosRob, -Sig.BH)
    
    ResAPA<-lapply(1:nrow(ResPosRob), function(i) with (ResPosRob[i,], paste0(IV1,": ",IV2.a," - ",IV2.b," = ",dif,": ","tw(",df,") = ",
                                                                              teststat, "; pROM = ", p.Rom, "; ", greek$xi," = ",
                                                                              Effect.Size, " (",Tam," effect)")))
    ResAPA2<-data.table(do.call("rbind",ResAPA))
    
    RsAOV$SimplEfa.1<- ResPosRob
    RsAOV$SimplEfa.2<- ResAPA2
    
    # B.2) Direction Day (Between) on each Class (within)
    cnt=0; yd<-list();LapAd<-NULL;LapAd2<-NULL
    for (i2 in 1:lv1) {
      LaP=NULL
      for (i in (1:(lv2-1))) {
        for (j in ((i+1):(lv2))) {
          cnt=cnt+1
          sample1<-DTp2[x2==levels(DTp2$x2)[i]&x1==levels(DTp2$x1)[i2],y] 
          sample2<-DTp2[x2==levels(DTp2$x2)[j]&x1==levels(DTp2$x1)[i2],y]
          out=c(unlist(yuenv2(sample1,sample2)))
          yd[[cnt]]<-as.data.table(rbind(out))
          LaP=c(LaP,yd[[cnt]]$p.value)
          yd[[cnt]]<-data.table(cbind(IV1=levels(DTp2$x1)[i2],
                                      IV2.a=levels(DTp2$x2)[i],IV2.b=levels(DTp2$x2)[j],yd[[cnt]]))
          
        }
      }
      #LapAd<-c(LapAd,p.adjust(LaP, "holm"))
      LapAd2<-c(LapAd2,p.adjust(LaP, "BH")) # Optimo
      if (length(LaP)==1) LapAd<-c(LapAd,(LaP))
      if (length(LaP)>1) LapAd<-c(LapAd,adjustRom(LaP))
    }
    
    ext1<-ExtrSig(LapAd); names(ext1)<-c("p.Rom","Sig.Rom")
    ext2<-ExtrSig(LapAd2); names(ext2)<-c("p.BH","Sig.BH")
    ResPosRob2<-data.table(do.call("rbind", yd),ext1,ext2)
    ResPosRob2$p.value=frmMM( ResPosRob2$p.value,4)
    for (i in (c(4,5,9:15)))ResPosRob2[[i]]<-as.numeric(frmMM(ResPosRob2[[i]],2))
    ResPosRob2<-data.table(ResPosRob2, Tam= unlist(lapply(ResPosRob2$Effect.Size, InterpExplana)))  
    
    setorder(ResPosRob2, -Sig.BH)
    #with (ResPosRob[1,], paste0(IV1,": ",IV2.a," - ",IV2.b," = ",dif,": ","tw(",df,") = ",
    #                               teststat, "; p = ", p.V1, "; \U1D6CF =",
    #                               Effect.Size, " (",Tam," effect)"))
    
    ResAPAb<-lapply(1:nrow(ResPosRob2), function(i) with (ResPosRob2[i,], paste0(IV1,": ",IV2.a," - ",IV2.b," = ",dif,": ","tw(",df,") = ",
                                                                                 teststat, "; pROM = ", p.Rom,"; pBH = ", p.BH,  "; ", greek$xi," = ",
                                                                                 Effect.Size, " (",Tam," effect)")))
    ResAPA2b<-data.table(do.call("rbind",ResAPAb))
    
    RsAOV$SimplEfb.1<- ResPosRob2
    RsAOV$SimplEfb.2<- ResAPA2b
    
    RsAOV$Dif<-DTFisDif[,-"y"]
    
    RsAOV
    
  }
  
  # h is the number of observations in the jth group after trimming.
  hMM<-function(x,tr=.2,na.rm=FALSE){if(na.rm)x<-na.del(x);length(x) - 2 * floor(tr * length(x))}
  
  wV<-function(x,tr=.2,na.rm=FALSE){
    if(na.rm)x<-na.del(x)
    hl <- hMM(x,tr,na.rm)
    hl * (hl - 1)/((length(x) - 1) * winvarMM(x,tr,na.rm,rees=FALSE))
  }
  
  # Computa efecto básico de yuen para todo, 2 o mas grupos
  yuenMM <- function(x,tr=0.2,alphap=0.05) {
    #
    #  Perform Yuen's test for trimmed means on the data in x and y.
    #  The default amount of trimming is 20%
    #  Missing values (values stored as NA) are automatically removed.
    #
    #  A confidence interval for the trimmed mean of x minus the
    #  the trimmed mean of y is computed and returned in yuen$ci.
    #  The significance level is returned in yuen$p.value
    #
    #   Unlike the function yuen, a robust quantile shift measure
    #   of effect size is returned.
    #
    
    Res<-NA
    grp <- 1:length(x)
    J <- length(grp)
    h<-sapply(grp, function(j){hMM(x[[j]], tr)})
    w<-sapply(grp, function(j){wV(x[[j]], tr,TRUE)})
    xbar <- sapply(grp, function(j){mean(x[[j]], tr)})
    u=sum(w)
    xtil <- sum(w * xbar)/u
    glf<-sum((1 - w/u)^2/(h - 1))/(J^2 - 1)
    A <- sum(w * (xbar - xtil)^2)/(J - 1)
    B <- 2 * (J - 2) * glf
    TEST <- A/(B + 1)
    nu1 <- J - 1
    nu2 <- 1/(3 * glf)
    sig <- 1 - pf(TEST, nu1, nu2)
    N<-sapply(x,length)
    Res<-list(Test = TEST, Means=xbar,df1 = nu1, df2 = nu2, n = N, p.value = sig)
    if (J==2) {
      wV2<-sapply(grp, function(j){winvarMM(x[[j]], tr,TRUE,FALSE)})
      q<-(N-1)*wV2/(h*(h-1))
      crit<-qt(1-alphap/2,nu2)
      dif<-xbar[[1]]-xbar[[2]]
      low<-dif-crit*sqrt(sum(q))
      up<- dif+crit*sqrt(sum(q))
      Res<-list(Test = TEST, Means=xbar,df1 = nu1, df2 = nu2, n = N, p.value = sig,
                Dif_1_2=dif, se_1_2=sqrt(sum(q)), ci=c(low,up),teststat=sqrt(TEST),crit=crit,df=nu2)
    }
    Res
  }
  
  # Computa vriante de yuen a partir de QS (Shift Function) from Trimmed Means
  yuen.QS.MM <- function(xp,tr=0.2,alphap=0.05) {
    if(tr==.5)stop('Use medpb to compare medians.')
    if(tr>.5)stop('cannot have tr>.5')
    
    chkPkg("MASS")
    Res<-NA
    grp <- 1:length(xp)
    J <- length(grp)
    N<-sapply(xp,length)
    h<-sapply(grp, function(j){hMM(xp[[j]], tr)})
    q<-sapply(grp, function(j){(N[[j]]-1)*winvar(xp[[j]],tr)/(h[[j]]*(h[[j]]-1))})
    df<-((sum(q))^2)/sum(q^2/(h-1))
    crit<-qt(1-alphap/2,df)
    xbar <- sapply(grp, function(j){mean(xp[[j]], tr)})
    mbar=sum(xbar)/2
    dif=xbar[1]-xbar[2]
    low<-dif-crit*sqrt(sum(q))
    up<- dif+crit*sqrt(sum(q))
    test<-abs(dif/sqrt(sum(q)))
    yuen<-2*(1-pt(test,df))
    e.pow=shiftesMM(xp,tmean)
    
    if(yuen<=alphap) Respp<-paste0("p = ",numf(yuen,4),"*")
    if(yuen>alphap)  Respp<-paste0("p = ",numf(yuen,4))
    ciEfSize<-with(shiftes.ci.MM(xp,locfun = tmean), paste0(CILow,", ",CIHigh))
    Res<-list(Test = test, Means=xbar,difference=dif,se=sqrt(sum(q)), CI=c(low,up),
              crit=crit,df = df, n = N, 
              Q.Effect=e.pow,
              CI_QS=ciEfSize,
              p.value = yuen)
    
    APA=with (Res, paste0("Group differences examined using Robust t-test found ... ",
                          paste0(names(xp), " (TrimM =",frmMM(xbar,2), ")",  collapse=" vs. "),
                          "; difference of ", frmMM(dif,2),
                          "; ",(1-alphap)*100,"%CI [", paste(frmMM(CI,2),collapse=", "),"]",
                          "; TrimSEM = ",frmMM(se,2),
                          "; Tw(",frmMM(df,2), ") = ", frmMM(test,2),
                          "; QS.Eff Size = ", frmMM(e.pow,2)," (",InterpQS(e.pow), " effect)",
                          "; ",(1-alphap)*100,"%CI [", ciEfSize,"]",
                          "; ",Respp
    ))
    Res<-list(Res,APA=APA)
    Res
  }
  
  #REL.M=c(0.2, 0.5, 0.8); n = 10000;reps=10
  # xp=DatFrmMod(Corr~Class,DTa)
  bmp.MM<-function(xp) {
    # Brunner and Munzel (2000) heteroscedastic analog of WMW test.
    
    ResF<-list()
    nLev=length(xp)
    grp <- 1:nLev; 
    N<-sapply(grp, function(j)sum(!is.na(xp[[j]]))); Ntt=sum(N) # n1 y n2
    xp<-lapply(grp, function(j)xp[[j]][!is.na(xp[[j]])])
    flag<-parallel::splitIndices(Ntt, ncl = ceiling(Ntt/N[1])) #flag1 y 2
    R<-rank(c(xp[[1]],xp[[2]]))
    Rr<-sapply(grp, function(j)mean(R[flag[[j]]])) # R1 y R2
    Rg<-sapply(grp, function(j)rank(xp[[j]])) # Rg1 y Rg2
    Ss<-sapply(grp, function(j) sum((R[flag[[j]]]-Rg[,j]-Rr[[j]]+(N[j]+1)/2)^2)/(N[j]-1)) # S1sq y S2sq
    sig<-Ss/N^2 # sig1 y 2
    #phat<-(R2-(n2+1)/2)/n1
    phat<-(Rr[[2]]-(N[[2]]+1)/2)/N[[1]]
    dhat<-1-2*phat
    ResF$phat<-phat
    ResF$dhat<-dhat
    ResF
  }
  
  # For bmp.MM
  ES.sum.REL.MAG.MM<-function(REL.M=c(0.2, 0.5, 0.8),n = 10000,reps=100,SEED=TRUE){
    #  Determine small medium and large equivalent measures of effect size based on the values in REL.M
    if(length(REL.M)!=3)stop('Should have three value in REL.M')
    if(n>10000)n=10000
    #x=rnorm(n)
    #y=rnorm(n)
    output<-mclapply (1:reps, function(ii) {
      x=rnorm(n); y=rnorm(n)
      sapply(1:3, function(j)bmp.MM(list(x,y-REL.M[j]))$dhat)
    },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE, mc.set.seed = !SEED)
    output<-colSums(matrix(unlist(output), ncol=3, byrow=TRUE))/reps
    output
  }
  
  # For locfun & REL.M
  ES.sum.REL.MAG.MM2<-function(REL.M=c(0.2, 0.5, 0.8),locfun=dts_stat,n = 10000,reps=100,SEED=TRUE){
    #  Determine small medium and large equivalent measures of effect size based on the values in REL.M
    if(length(REL.M)!=3)stop('Should have three value in REL.M')
    if(n>10000)n=10000
    #x=rnorm(n)
    #y=rnorm(n)
    output<-mclapply (1:reps, function(ii) {
      x=rnorm(n); y=rnorm(n)
      sapply(1:3, function(j)locfun(x,y-REL.M[j]))
    },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE, mc.set.seed = !SEED)
    output<-colSums(matrix(unlist(output), ncol=3, byrow=TRUE))/reps
    output
  }
  
  # For locfun & qt-of-REL.M instead of just REL.M
  ES.sum.REL.MAG.MM2b<-function(REL.M=c(.58,.69,.79),locfun=dts_stat,n = 10000,reps=100,SEED=TRUE){
    #  Determine small medium and large equivalent measures of effect size based on the values in REL.M
    if(length(REL.M)!=3)stop('Should have three value in REL.M')
    if(n>10000)n=10000
    #x=rnorm(n)
    #y=rnorm(n)
    output<-mclapply (1:reps, function(ii) {
      x=rnorm(n); y=rnorm(n)
      qt<-c(quantile(y,REL.M))
      sapply(1:3, function(j)locfun(x,y-qt[[j]]))
    },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE, mc.set.seed = !SEED)
    output<-colSums(matrix(unlist(output), ncol=3, byrow=TRUE))/reps
    output
  }
  
  # la he cambiado en octubre de 2023 pues daba errores
  # ecdfdist(list(x,y),method=locfun,p = 1,as.dist = T)[[1]]
  ES.sum.REL.MAG.MM3<-function(REL.M=c(0.2, 0.5, 0.8),locfun="KS",n = 10000,reps=100,SEED=TRUE) {
    #  Determine small medium and large equivalent measures of effect size based on the values in REL.M
    if(length(REL.M)!=3)stop('Should have three value in REL.M')
    if(n>10000)n=10000
    #x=rnorm(n)
    #y=rnorm(n) j=1
    output<-mclapply (1:reps, function(ii) {
      x=rnorm(n);
      y=rnorm(n); 
      x=stats::ecdf(x)  
      sapply(1:3, function(j) {
        y=stats::ecdf(y-REL.M[j]);
        ecdfdist(list(x,y),method=locfun,p = 1,as.dist = T)[1]
      })
    },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE, mc.set.seed = !SEED)
    output<-colSums(matrix(unlist(output), ncol=3, byrow=TRUE))/reps
    output
  }
  
  #QSfun=median; tr=.2;
  #SEED=TRUE;alpha=.05;nboot=2000;method='hoch';
  #NULL.V=c(0,0,.5,.5,.5,0);REL.M=NULL;n.est=1000000
  ES.summary.CI.MM<-function(x,y,tr=.2,QSfun=median,SEED=TRUE,alpha=.05,nboot=2000,method='hoch',
                             NULL.V=c(0,0,.5,.5,.5,0),REL.MAG=NULL,REL.M=NULL,n.est=1000000) {
    #
    # Estimate a collection of effect sizes:
    #  AKP: Homoscedastic robust analog of Cohen's d
    #  EP:  Explanatory power
    #  QS:  Quantile shift based on the median of the distribution of X-Y, 
    #  QStr: Quantile shift based on the trimmed mean of the distribution of X-Y 
    #  WMW:  P(X<Y)
    #  KMS:  Robust heteroscedastic analog  of Cohen's d
    #  Cliff
    #  Brunner-Munzel
    a=c('AKP','EP','QS','QStr','WMW','KMS')
    output=matrix(NA,ncol=10,nrow=8)
    dimnames(output)=list(c('AKP','EP','QS (median)','QStr','WMW','KMS','Cliff','Brunner-Munzel'),
                          c('Est','NULL','S','M','L','ci.low','ci.up','p.value','adj.p.value','Quali'))
    for(j in 1:6){
      output[j,1]=ESfun(x,y,QSfun=QSfun,method=a[j],tr=tr,pr=FALSE)
    }
    if(!is.null(REL.MAG))REL.M=REL.MAG
    if(is.null(REL.M)){
      SM=c(.2,.14,.55,.55,.55,.1)
      MED=c(.5,.34,.64,.64,.64,.25)
      LAR=c(.8,.52,.71,.71,.71,.4)
    }
    if(!is.null(REL.M)){
      v=ES.sum.REL.MAG(REL.M,n=n.est)
      SM=v[,1]
      MED=v[,2]
      LAR=v[,3]
      SM[1]=REL.M[1]
      SM[6]=REL.M[1]/2
      MED[1]=REL.M[2]
      MED[6]=REL.M[2]/2
      LAR[1]=REL.M[3]
      LAR[6]=REL.M[3]/2
    }
    output[1:6,2:5]=cbind(NULL.V,SM,MED,LAR)
    if(output[1,1]<0)output[1,3:5]=-1*output[1,3:5]
    for(j in 3:4){
      if(output[j,1]<.5){
        dif=output[j,3:5]-.5
        output[j,3:5]=.5-dif
      }}
    if(output[5,1]>.5 & output[5,5]< .5){
      output[5,3:5]=1-output[5,3:5]
    }
    if(output[5,1]<.5 & output[5,5]> .5){
      output[5,3:5]=1-output[5,3:5]
    }
    
    a=akp.effect.ci(x,y,tr=tr,alpha=alpha,nboot=nboot,SEED=SEED)
    output[1,6:7]=a$ci
    output[1,8]=a$p.value
    a=EPci(x,y,tr=tr,alpha=alpha,SEED=SEED,nboot=nboot)
    output[2,6:7]=a$ci
    output[2,8]=yuen(x,y,tr=tr)$p.value
    a=shiftPBci(x,y,locfun=QSfun,alpha=alpha,nboot=nboot,SEED=SEED)
    output[3,6:7]=a$ci
    output[3,8]=a$p.value
    a=shiftPBci(x,y,locfun=tmean,alpha=alpha,nboot=nboot,SEED=SEED)
    output[4,6:7]=a$ci
    output[4,8]=a$p.value
    a=cidv2(x,y,alpha=alpha)
    output[5,6:7]=a$p.ci
    output[5,8]=a$p.value
    a=KMS.ci(x,y,alpha=alpha,nboot=nboot,SEED=SEED)
    output[6,6:7]=a$ci
    output[6,8]=a$p.value
    if(output[6,1]<0)output[6,3:5]=-1*output[6,3:5]
    output[,9]=p.adjust(output[,8],method=method)
    
    Cliff<-cidv2(x,y)
    NewInt<-round(ES.sum.REL.MAG.MM(REL.M=c(0.2, 0.5, 0.8)),2)
    output[7,c(1:8)]=with (Cliff, c(d.hat,0,NewInt,d.ci,round(p.value,10)))
    Br.Man<-bmp(x,y)
    output[8,c(1:8)]=with (Br.Man, c(dhat,0,NewInt,ci.p,round(p.value,10)))
    
    #InterEf<-sapply(1:8, function(j)InterpEfGen(c(output[j,1]),output[j,3:5]))
    
    outputDT=as.data.table(output,keep.rownames = T)
    outputDT$Quali<-sapply(1:8, function(j)InterpEfGen(c(output[j,1]),output[j,3:5]))
    outputDT
  }
  
  # sapply(Samp,"[",5,)
  # La función me permite Nj diferentes pero de momento la he dejado como Wilcox con =Nj
  PowBasSCI<-function(x,tr=.2,loc.fun=median,PB=FALSE,nboot=1000, SEED=TRUE,alphap=.05,pcpas=NA) {
    ci=NA
    if(SEED)set.seed(2)
    grp <- 1:length(x); J <- length(grp)
    N<-sapply(grp, function(j)sum(!is.na(x[[j]])))
    #Samp<-lapply(grp, function(jj)matrix(sample(x[[jj]], size=N[jj]*nboot,replace=TRUE),nrow=nboot))
    #Samp<-lapply(grp, function(jj)matrix(sample(x[[jj]], size=max(N)*nboot,replace=TRUE),nrow=nboot))
    Samp<-lapply(grp, function(jj)matrix(sample(x[[jj]], size=min(N)*nboot,replace=TRUE),nrow=nboot))
    bvec<-mclapply (1:nboot, function(ib) {
      # sqrt(RobEffS(lapply(Samp,"[",ib,),tr=tr,loc.fun=loc.fun,PB=PB,nboot=nboot))
      sqrt(PowBasS(lapply(Samp,"[",ib,),tr=tr))
    },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE, mc.set.seed = !SEED)
    bvec<-sort(abs(unlist(bvec)))
    crit<-alphap/2
    icl<-round(crit*nboot)+1
    icu<-nboot-icl
    ci<-c(bvec[icl],bvec[icu])
    pchk<-ifelse(is.na(pcpas),yuenMM(x,tr=tr)$p.value,pcpas)
    if(pchk>alphap)ci[1]=0
    if(ci[1]<0)ci[1]=0
    ci
  } # With apply
  
  #x=DatFrmMod(Memoria~Grupos,EstudOrig.b)
  # The bias of a statistic is the difference between its expected value (mean) across many samples and the true population value
  PowBasSCIPlus<-function(x,tr=.2,loc.fun=median,PB=FALSE,nboot=1000, SEED=TRUE,alphap=.05,pcpas=NA) {
    ci=NA
    if(SEED)set.seed(2)
    grp <- 1:length(x); J <- length(grp)
    N<-sapply(grp, function(j)sum(!is.na(x[[j]])))
    #Samp<-lapply(grp, function(jj)matrix(sample(x[[jj]], size=N[jj]*nboot,replace=TRUE),nrow=nboot))
    #Samp<-lapply(grp, function(jj)matrix(sample(x[[jj]], size=max(N)*nboot,replace=TRUE),nrow=nboot))
    Samp<-lapply(grp, function(jj)matrix(sample(x[[jj]], size=min(N)*nboot,replace=TRUE),nrow=nboot))
    bvec<-mclapply (1:nboot, function(ib) {
      # sqrt(RobEffS(lapply(Samp,"[",ib,),tr=tr,loc.fun=loc.fun,PB=PB,nboot=nboot))
      sqrt(PowBasS(lapply(Samp,"[",ib,),tr=tr))
    },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE, mc.set.seed = !SEED)
    t=unlist(bvec)
    bvec<-sort(abs(unlist(bvec)))
    crit<-alphap/2
    icl<-round(crit*nboot)+1
    icu<-nboot-icl
    ci<-c(bvec[icl],bvec[icu])
    pchk<-ifelse(is.na(pcpas),yuenMM(x,tr=tr)$p.value,pcpas)
    if(pchk>alphap)ci[1]=0
    if(ci[1]<0)ci[1]=0
    list(ci=ci, t=t, t0=mean(t), Bias=mean(t)-sqrt(PowBasS(x)),sd=sd(t))
  } # With apply
  
  # Alternativa para más de 2 niveles, comparados 2 a 2 como PosH
  #xp=DatFrmMod(DepV1~Grp1,DTf)
  #xp=DatFrmMod(DepV1~Grp1,DTBig4)
  shiftesAOVMM<-function(xp,locfun=median,iter=100,SEED=TRUE,...) {
    #
    #  Probabilistic measure of effect size: shift of the median.
    #
    ef.size=NA
    ResF<-list();ResNm<-matrix();cont=0
    ResMtx<-NA
    nLev=length(xp)
    grp <- 1:nLev; 
    N<-sapply(grp, function(j)sum(!is.na(xp[[j]])))
    nt=prod(N)
    if(sum(N<10)>0)stop('Q Shift cannot be calculated since a minimum of 10 data is needed per condition')
    ef.size<-
      ifelse(nt<10^6, {
        for (i in (1:(nLev-1))) {
          for (j in ((i+1):(nLev))) {
            cont=cont+1
            L=outer(xp[[i]],xp[[j]],FUN='-') #producto matricial}
            est=locfun(L)
            ResMtxPrv<-L-est<=est
            if (cont==1) ResMtx<-L-est<=est
            if (cont>1) ResMtx<-combine.mat(ResMtx,ResMtxPrv)
          }
        }
        mean(ResMtx,na.rm = T)
      },
      {
        if(SEED)set.seed(2)
        L=NULL;  vef=NA
        nmin=min(c(N,100))     # El 100 no debería de ser iter??
        vef<-mclapply (1:iter, function(ii) {
          cont=0
          Samp<-lapply(grp, function(jj)sample(N[[jj]], size=nmin))
          for (i in (1:(nLev-1))) {
            for (j in ((i+1):(nLev))) {
              cont=cont+1 
              L=outer(xp[[i]][Samp[[i]]],xp[[j]][Samp[[j]]],FUN='-')
              est=locfun(L)
              ResMtxPrv<-L-est<=est
              if (cont==1) ResMtx<-L-est<=est
              if (cont>1) ResMtx<-combine.mat(ResMtx,ResMtxPrv)
            }
          }
          mean(ResMtx,na.rm = T)
        },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE, mc.set.seed = !SEED)
        
        mean(unlist(vef))
      }
      )
    ef.size
  }
  
  # formula=DepV1 ~ Grp1; data=DTc;tr = 0.2;
  #  nboot = 100; SEED = TRUE; pr = TRUE; loc.fun = median;alphap=0.05
  
  #xp=DatFrmMod(Corr~Class,DTa);locfun=median;alpha=.05;nboot=500;SEED=TRUE
  shiftes.ci.MM<-function(xp, locfun=median,alpha=.05,nboot=500,SEED=TRUE,...) {
    #
    # Computes the robust effect size for two-sample case using
    # Algina, Keselman, ºPenfield Pcyh Methods, 2005, 317-328
    #
    
    shitEsRes<-NA; Resf<-list()
    nLev=length(xp)
    grp <- 1:nLev; 
    N<-sapply(grp, function(j)sum(!is.na(xp[[j]])))
    
    xp<-lapply(grp, function(j)xp[[j]][!is.na(xp[[j]])])
    L=round((alpha/2) * nboot); U=nboot-L; L<-L+1
    if(SEED)set.seed(2)
    be.f=NA
    
    be.f<-mclapply (1:nboot, function(ii) {
      X=sample(xp[[1]],N[[1]],replace=TRUE)
      Y=sample(xp[[2]],N[[2]],replace=TRUE)
      shiftesMM (list(X,Y),locfun=locfun)
    },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE, mc.set.seed = !SEED)
    
    be.f.o<-sort(unlist(be.f))
    ci1<-round(be.f.o[L],4) # Estaba mal pues sumaba 1 dos veces: be.f.o[L+1]
    ci2<-round(be.f.o[U],4)
    # pv=mean(be.f.o<null.val)+mean(be.f.o==null.val)
    # pv=2*min(c(pv,1-pv))
    shitEsRes<-round(shiftesMM(list(xp[[1]],xp[[2]]),locfun=locfun),4)
    Resf<-list('SQ'=shitEsRes, 'CILow'=ci1, 'CIHigh'=ci2,
               'APA:' = paste0('SQ = ', shitEsRes, ' [', ci1, ", ", ci2, ']'))
    Resf
  }
  
  #x=EstudOrig.b2; R=100
  CIFromBoot<-function(x,R=100) {
    Outp<-list()
    EstBootT1w <- function(d, i) { 
      # Function for the {boot} library
      # Has to include an i parameter for index
      # Previously you have to change the name of the variables
      # to Grp and DepV, and I make sure that it is data.frame and Grp is as factor
      Dd<-split(d[i,]$DepV, d$Grp)
      RobEffS(Dd)
    }
    cat("Con R=10000; puede tardar hasta 30 mins\n")
    start_time <- Sys.time()
    Res<- boot(x, EstBootT1w, strata=x$Grp, R=R, parallel = "multicore")
    Res2<-boot.ci(Res)
    end_time <- Sys.time()
    print(end_time - start_time)
    Outp$Boot<-Res
    Outp$Resum<-Res %>% broom::tidy(.) 
    Outp$Graph<-Res %>% plot(.)
    Outp$CI<-Res2
    Outp$CIRecom<- Res %>%
      broom::tidy(.) %>%
      summarise(ifelse(abs(bias) > std.error, "Best BCa that percentile","percentile can serve"))
    Outp$CIPerc<-Res2$percent
    Outp$Time<-end_time - start_time
    Outp
  }
  
  CIFromBootSimpl<-function(x,R=100) {
    Outp<-list()
    
    RobEffS1Boot<-function(xsp,tr=.2,loc.fun=median,PB=FALSE,nboot=100) {
      # Versión básica más eficiente pero requiere previamente split pues trabaja con listas
      e.pow2=NA
      #print(xsp)
      grp <- 1:length(xsp); J <- length(grp)
      N<-sapply(grp, function(j)sum(!is.na(xsp[[j]])))
      chkn=var(N)
      e.pow2=ifelse(chkn==0,
                    PowBasS(xsp,tr=tr),
                    PowBasS(lapply(grp, function(j){sample(xsp[[j]], min(N))}),tr=tr)
      )
      sqrt(e.pow2)
    } # With formula & apply 
    
    EstBootT1w.1Boot <- function(d, i) { # La función tiene que incluir un parámetro i para índice
      Dd<-split(d[i,]$DepV, d$Grp)
      RobEffS1Boot(Dd)
      #RobEffS(xsp = Dd,nboot = 1)
    }
    
    cat("Con R=10000; puede tardar hasta 1 mins\n")
    start_time <- Sys.time()
    Res<- boot(x, EstBootT1w.1Boot, strata=x$Grp, R=R, parallel = "multicore")
    Res2<-boot.ci(Res)
    end_time <- Sys.time()
    print(end_time - start_time)
    Outp$Boot<-Res
    Outp$Resum<-Res %>% broom::tidy(.) 
    Outp$Graph<-Res %>% plot(.)
    Outp$CI<-Res2
    Outp$CIRecom<- Res %>%
      broom::tidy(.) %>%
      summarise(ifelse(abs(bias) > std.error, "Best BCa that percentile","percentile can serve"))
    Outp$CIPerc<-Res2$percent
    Outp$Time<-end_time - start_time
    Outp
  }
  
  yuen.effect.ciMin <- function(x,y,SEED=TRUE,nboot=400,tr=.2,alpha=.05) {
    #
    # Compute a 1-alpha  confidence interval
    # for a robust, heteroscedastic  measure of effect size
    #  The absolute value of the measure of effect size is used.
    #
    if(SEED)set.seed(2) # set seed of random number generator so that
    #             results can be duplicated.
    x=elimna(x)
    y=elimna(y)
    bvec=0
    Mn=min(length(x),length(y))
    datax<-matrix(sample(x,size=Mn*nboot,replace=TRUE),nrow=nboot)
    datay<-matrix(sample(y,size=Mn*nboot,replace=TRUE),nrow=nboot)
    for(i in 1:nboot){
      bvec[i]=yuenv2(datax[i,],datay[i,],tr=tr,SEED=FALSE)$Effect.Size
    }
    bvec<-sort(abs(bvec))
    crit<-alpha/2
    icl<-round(crit*nboot)+1
    icu<-nboot-icl
    ci<-NA
    ci[1]<-bvec[icl]
    pchk=yuen(x,y,tr=tr)$p.value
    if(pchk>alpha)ci[1]=0
    ci[2]<-bvec[icu]
    if(ci[1]<0)ci[1]=0
    es=abs(yuenv2(x,y,tr=tr)$Effect.Size)
    plot(bvec,ylim=c(0.6,1))
    points(x = icl,y=ci[1], col="red",cex=2,pch=19)
    points(x = icu,y=ci[2], col="red",cex=2,pch=19)
    list(CI=ci,Effect.Size=es)
  }
  
  yuen.effect.ciMax <-function(x,y,SEED=TRUE,nboot=400,tr=.2,alpha=.05) {
    #
    # Compute a 1-alpha  confidence interval
    # for a robust, heteroscedastic  measure of effect size
    #  The absolute value of the measure of effect size is used.
    #
    if(SEED)set.seed(2) # set seed of random number generator so that
    #             results can be duplicated.
    x=elimna(x)
    y=elimna(y)
    bvec=0
    Mn=max(length(x),length(y))
    datax<-matrix(sample(x,size=Mn*nboot,replace=TRUE),nrow=nboot)
    datay<-matrix(sample(y,size=Mn*nboot,replace=TRUE),nrow=nboot)
    for(i in 1:nboot){
      bvec[i]=yuenv2(datax[i,],datay[i,],tr=tr,SEED=FALSE)$Effect.Size
    }
    bvec<-sort(abs(bvec))
    crit<-alpha/2
    icl<-round(crit*nboot)+1
    icu<-nboot-icl
    ci<-NA
    ci[1]<-bvec[icl]
    pchk=yuen(x,y,tr=tr)$p.value
    if(pchk>alpha)ci[1]=0
    ci[2]<-bvec[icu]
    if(ci[1]<0)ci[1]=0
    es=abs(yuenv2(x,y,tr=tr)$Effect.Size)
    plot(bvec,ylim=c(0.6,1))
    points(x = icl,y=ci[1], col="red",cex=2,pch=19)
    points(x = icu,y=ci[2], col="red",cex=2,pch=19)
    list(CI=ci,Effect.Size=es)
  }
  
  # The original function yuen.effect.ci reproduced, to study it
  # x=Data;y=Data2; nboot=400;tr=.2;alphap=.05
  {# yuen.effect.ci<-function(x,y,SEED=TRUE,nboot=400,tr=.2,alpha=.05){
  #       #
  #       # Compute a 1-alpha  confidence interval
  #       # for a robust, heteroscedastic  measure of effect size
  #       #  The absolute value of the measure of effect size is used.
  #       #
  #       if(SEED)set.seed(2) # set seed of random number generator so that
  #       #             results can be duplicated.
  #       x=elimna(x)
  #       y=elimna(y)
  #       bvec=0
  #       datax<-matrix(sample(x,size=length(x)*nboot,replace=TRUE),nrow=nboot)
  #       datay<-matrix(sample(y,size=length(y)*nboot,replace=TRUE),nrow=nboot)
  #       for(i in 1:nboot){
  #         bvec[i]=yuenv2(datax[i,],datay[i,],tr=tr,SEED=FALSE)$Effect.Size
  #       }
  #       bvec<-sort(abs(bvec))
  #       crit<-alphap/2
  #       icl<-round(crit*nboot)+1
  #       icu<-nboot-icl
  #       ci<-NA
  #       ci[1]<-bvec[icl]
  #       pchk=yuen(x,y,tr=tr)$p.value
  #       if(pchk>alphap)ci[1]=0
  #       ci[2]<-bvec[icu]
  #       if(ci[1]<0)ci[1]=0
  #       es=abs(yuenv2(x,y,tr=tr)$Effect.Size)
  #       list(CI=ci,Effect.Size=es)
  #     }
  }
  
  #formula=DepV1 ~ Grp1; data=DTa;tr = 0.2;
  # t1wayMM<-function (formula, data, tr = 0.2, alphap=0.05,
  {#                    nboot = 100, SEED = TRUE, pr = TRUE, loc.fun = median, PB=FALSE) 
  # {
  #   #require(MASS)
  #   if (SEED)set.seed(2)
  #   if (missing(data)) {
  #     mf <- model.frame(formula)
  #   }else {
  #     mf <- model.frame(formula, data)
  #   }
  #   cl <- match.call()
  #   x <- split(model.extract(mf, "response"), mf[, 2])
  #   if (tr == 0.5) 
  #     warning("Comparing medians should not be done with this function!")
  #   yRes<-yuenMM(x,tr)
  #   
  #   #Estimacion Potencia añadida a partir de t1wayv2 & t1way.effect
  #   Ef.Siz<-RobEffS(x,tr=tr,loc.fun=loc.fun,PB=PB,nboot=nboot)
  #   CI.EfSiz<-PowBasSCI(x,tr=tr, loc.fun=loc.fun, PB=PB,nboot=nboot, SEED=SEED, alphap=alphap,pcpas=yRes$p.value)
  #   c(list(call = cl),yRes, list(Var.Explained = Ef.Siz^2, Effect.Size = Ef.Siz, CI.Ef.Size=CI.EfSiz))
  #   #class(result) <- c("t1way")
  #   #result
  # }
  }
  
  t1way_vMM<-function (formula, data, tr = 0.2, alphap=0.05,
                     nboot = 100, SEED = TRUE, pr = TRUE, loc.fun = median, PB=FALSE) 
  {
    #require(MASS)
    if (SEED)set.seed(2)
    if (missing(data)) {
      mf <- model.frame(formula)
    }else {
      mf <- model.frame(formula, data)
    }
    cl <- match.call()
    x <- split(model.extract(mf, "response"), mf[, 2])
    if (tr == 0.5) 
      warning("Comparing medians should not be done with this function!")
    yRes<-yuenMM(x,tr)
    grp <- 1:length(x)
    trmean <- sapply(grp, function(j){mean(x[[j]], tr)})
    winvtr<-sapply(grp, function(j){trimseMM(x[[j]], tr)})
    
    # Estimated power added from t1wayv2 & t1way.effect
    Ef.Siz<-RobEffS(x,tr=tr,loc.fun=loc.fun,PB=PB,nboot=nboot)
    CI.EfSiz<-PowBasSCI(x,tr=tr, loc.fun=loc.fun, PB=PB,nboot=nboot, SEED=SEED, alphap=alphap,pcpas=yRes$p.value)
    CIPlus<-PowBasSCIPlus(x,tr=tr, loc.fun=loc.fun, PB=PB,nboot=nboot, SEED=SEED, alphap=alphap,pcpas=yRes$p.value)
    StatDif<-ifelse(yRes$p.value<=alphap,"there was a statistically significant difference","there was no statistically significant difference")
    APA=with (yRes, paste0("Group differences examined using Robust ANOVA found ...", 
                           paste(names(x),"(TrimM =",frmMM(trmean,2),", TrimSEM =",frmMM(winvtr,2),")",collapse=", ")," ",
                           StatDif,
                           ". FW(",paste(frmMM(df1,2),collapse=", "),", ",
                           paste(frmMM(df2,2),collapse=", "),
                           ") = ",
                           frmMM(Test,2),
                           ", p = ",numf(p.value,4),", ",
                           greek$xi," = ", frmMM(Ef.Siz,2)," (",InterpDeltaWilcox(Ef.Siz), " effect) ",
                           (1-alphap)*100,"% CI [", paste(frmMM(CI.EfSiz,2),collapse=", "),"]"
    ))
    c(list(call = cl),yRes, list(Var.Explained = Ef.Siz^2, Effect.Size = Ef.Siz, CI.Ef.Size=CI.EfSiz, CIPlus=CIPlus, APARes=APA))
    #class(result) <- c("t1way")
    #result
  }
  
  t2waybt_vMM<-function(J,K,x,tr=.2,grp=c(1:p),p=J*K,nboot=599,SEED=TRUE) {
    #
    #   Two-way ANOVA based on trimmed means and a bootstrap-t method
    #   The data are assumed to be stored as described in the function t2way
    #   The default number of bootstrap samples is nboot=599
    # My version improves loops with mclapply
    require(parallel)
    if(is.data.frame(x))x=as.matrix(x)
    if(is.matrix(x))x<-listm(x)
    if(!is.list(x))stop("Data must be stored in a matrix or in list mode.")
    if(SEED)set.seed(2) # set seed of random number generator so that
    #             results can be duplicated.
    # compute test statistics:
    tests=t2way(J=J,K=K,x,tr=tr,grp=grp)
    TA=NULL
    TB=NULL
    TAB=NULL
    data=list()
    xcen=list()
    for(j in 1:length(x))xcen[[j]]<-x[[j]]-mean(x[[j]],tr)
    print("Taking bootstrap samples. Please wait.")
    Res<-mclapply (1:nboot, function(ib) {
      data<-lapply(1:length(x), function(j) sample(xcen[[j]],size=length(x[[j]]),replace=TRUE))
      bt=t2way(J,K,data,tr=tr,grp=grp)
      with(bt, c(Qa,Qb,Qab))
    },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE)
    Res2<-data.table(t(as.data.table(Res)))
    pA<-sum(tests$Qa<=Res2$V1)/nboot
    pB<-sum(tests$Qb<=Res2$V2)/nboot
    pAB<-sum(tests$Qab<=Res2$V3)/nboot
    list(A.p.value=pA,B.p.value=pB,AB.p.value=pAB)
  }
  
  trimciv2_vMM <- function(x,tr=.2,alpha=.05,null.value=0,pr=TRUE, alternative="two.sided") {
    #
    #  Compute a 1-alpha confidence interval for the trimmed mean
    #  Same as trimci, only a standardized measure of effect size is reported:
    # the difference between the trimmed mean and hypothesized value divided by
    # the Winsorized standard deviation, rescaled to estimate the standard deviation
    # when sampling from a normal distribution. 
    #
    #  The default amount of trimming is tr=.2
    #  Manuel Miguel added the alternative variants
    library(MASS)
    x<-elimna(x)
    se<-sqrt(winvar(x,tr))/((1-2*tr)*sqrt(length(x)))
    trimci<-vector(mode="numeric",length=2)
    df<-length(x)-2*floor(tr*length(x))-1
    trimci[1]<-mean(x,tr) - qt(1-alpha/2,df)*se
    trimci[2]<-mean(x,tr) + qt(1-alpha/2,df)*se
    test<-(mean(x,tr)-null.value)/se
    if (alternative == "less") {
      # pval <- pt(tstat, df)
      # sig  <- pt(abs(test),df)
      sig  <- pt(test,df)
    } else if (alternative == "greater") {
      # pval <- pt(tstat, df, lower.tail = FALSE)
      # sig  <- pt(abs(test),df,lower.tail = FALSE)
      sig  <- pt(test,df,lower.tail = FALSE)
    } else {
      # pval <- 2 * pt(-abs(tstat), df)
      sig<-2*(1-pt(abs(test),df))
    }
    # sig<-2*(1-pt(abs(test),df))
    if(tr==0)term=1
    if(tr>0)term=sqrt(area(dnormvar,qnorm(tr),qnorm(1-tr))+2*(qnorm(tr)^2)*tr)
    epow=(mean(x,tr)-null.value)*term/sqrt(winvar(x,tr=tr,na.rm=TRUE))
    list(ci=trimci,estimate=mean(x,tr),test.stat=test,se=se,p.value=sig,n=length(x),Effect.Size=epow)
  }
  
  # J=3;K=2;x=elimna(ArP);tr=.2;JK=J*K;grp=c(1:JK);nboot=599;SEED=TRUE
  bwtrimbt_vMM <- function(J,K,x,tr=.2,JK=J*K,grp=c(1:JK),nboot=599, SEED=TRUE) {
    #
    # A bootstrap-t for performing a split-plot design
    # with trimmed means.
    # By default, 20% trimming is used with B=599 bootstrap samples.
    #
    #
    #  The R variable x is assumed to contain the raw
    #  data stored in list mode or in a matrix or a data frame
    #  If in list mode, x[[1]] contains the data
    #  for the first level of both factors: level 1,1.
    #  x[[2]] is assumed to contain the data for level 1 of the
    #  first factor and level 2 of the second: level 1,2
    #  x[[K]] is the data for level 1,K
    #  x[[K+1]] is the data for level 2,1, x[[2K]] is level 2,K, etc.
    #
    #  If the data are in a matrix, column 1 is assumed to
    #  correspond to x[[1]], column 2 to x[[2]], etc.
    #
    #  When in list mode x is assumed to have length JK, the total number of
    #  groups being tested, but a subset of the data can be analyzed
    #  using grp
    #
    x=elimna(x) # La he añadido yo
    if(SEED)set.seed(2)
    if(is.data.frame(x) || is.matrix(x)) {
      y <- list()
      ik=0
      il=c(1:K)-K
      for(j in 1:J){
        il=il+K
        zz=x[,il]
        zz=elimna(zz)
        for(k in 1:K){
          ik=ik+1
          y[[ik]]=zz[,k]
        }}
      x <- y
    }
    JK<-J*K
    data<-list()
    xcen<-list()
    for(j in 1:length(x)){
      data[[j]]<-x[[grp[j]]] # Now have the groups in proper order.
      xcen[[j]]<-data[[j]]-mean(data[[j]],tr) # Centered data
    }
    x<-data
    #
    set.seed(2) # set seed of random number generator so that
    #             results can be duplicated.
    # Next determine the n_j values
    nvec<-NA
    jp<-1-K
    for(j in 1:J){
      jp<-jp+K
      nvec[j]<-length(x[[j]])
    }
    blist<-list()
    print("Taking bootstrap samples. Please wait.")
    testmat<-matrix(NA,ncol=3,nrow=nboot)
    for(iboot in 1:nboot){
      iv<-0
      for(j in 1:J){
        temp<-sample(nvec[j],replace = T)
        for(k in 1:K){
          iv<-iv+1
          tempx<-xcen[[iv]]
          blist[[iv]]<-tempx[temp]
        }}
      btest<-tsplit(J,K,blist,tr)
      testmat[iboot,1]<-btest$Qa
      testmat[iboot,2]<-btest$Qb
      testmat[iboot,3]<-btest$Qab
    }
    test=tsplit(J,K,x,tr=tr)
    pbA=mean(test$Qa[1]<testmat[,1])
    pbB=mean(test$Qb[1]<testmat[,2])
    pbAB=mean(test$Qab[1]<testmat[,3])
    list(p.value.A=pbA,p.value.B=pbB,p.value.AB=pbAB)
  }
  
  RobSisOne <- function(DTFisp,IdSj="Subject",wv1="Context",wv2="Day",LaVd="Rate") {
    RsAOV<-list()
    DTp2<-NA
    AOVOmn <-NA
    DTp2 <- DTFisp %>% 
      dplyr::rename(IdSuj=all_of(IdSj)) %>% 
      dplyr::rename(x1=all_of(wv1)) %>% 
      dplyr::rename(x2=all_of(wv2)) %>%
      dplyr::rename(y=all_of(LaVd)) %>%
      mutate(x1=factor(x1)) %>%
      mutate(x2=factor(x2)) %>%
      mutate(IdSuj=factor(IdSuj)) %>%
      mutate(Inter:=factor(interaction(x1,x2))) %>%
      data.table() %>%
      setkey(.,Inter)
    
    NumL=length(levels(DTp2$Inter)); vector=c(1:NumL)
    lv1=length(levels(DTp2$x1));lv2=length(levels(DTp2$x2))
    
    
    # Direction Day (Between) on each Class (within)
    cnt=0; yd<-list();LapAd<-NULL;LapAd2<-NULL
    for (i2 in 1:lv1) {
      LaP=NULL
      AOVOmn[[i2]]<- paste0("For ", levels(DTp2$x1)[i2], ": ", t1way_vMM(formula = y~x2,data = DTp2[x1==levels(DTp2$x1)[i2]])$APARes)
      for (i in (1:(lv2-1))) {
        for (j in ((i+1):(lv2))) {
          cnt=cnt+1
          sample1<-DTp2[x2==levels(DTp2$x2)[i]&x1==levels(DTp2$x1)[i2],y] 
          sample2<-DTp2[x2==levels(DTp2$x2)[j]&x1==levels(DTp2$x1)[i2],y]
          out=c(unlist(yuenv2(sample1,sample2)))
          yd[[cnt]]<-as.data.table(rbind(out))
          LaP=c(LaP,yd[[cnt]]$p.value)
          yd[[cnt]]<-data.table(cbind(IV1=levels(DTp2$x1)[i2],
                                      IV2.a=levels(DTp2$x2)[i],IV2.b=levels(DTp2$x2)[j],yd[[cnt]]))
          
        }
      }
      #LapAd<-c(LapAd,p.adjust(LaP, "holm"))
      LapAd2<-c(LapAd2,p.adjust(LaP, "BH")) # Optimo
      if (length(LaP)==1) LapAd<-c(LapAd,(LaP))
      if (length(LaP)>1) LapAd<-c(LapAd,adjustRom(LaP))
    }
    
    ext1<-ExtrSig(LapAd); names(ext1)<-c("p.Rom","Sig.Rom")
    ext2<-ExtrSig(LapAd2); names(ext2)<-c("p.BH","Sig.BH")
    ResPosRob2<-data.table(do.call("rbind", yd),ext1,ext2)
    ResPosRob2$p.value=frmMM( ResPosRob2$p.value,4)
    for (i in (c(4,5,9:15)))ResPosRob2[[i]]<-as.numeric(frmMM(ResPosRob2[[i]],2))
    ResPosRob2<-data.table(ResPosRob2, Tam= unlist(lapply(ResPosRob2$Effect.Size, InterpExplana)))
    setorder(ResPosRob2, -Sig.BH)
    
    ResAPAb<-lapply(1:nrow(ResPosRob2), function(i) with (ResPosRob2[i,], paste0(IV1,": ",IV2.a," - ",IV2.b," = ",dif,": ","tw(",df,") = ",
                                                                                 teststat, "; pROM = ", p.Rom,"; pBH = ", p.BH,  "; ", greek$xi," = ",
                                                                                 Effect.Size, " (",Tam," effect)")))
    ResAPA2b<-data.table(do.call("rbind",ResAPAb))
    RsAOV$Omn <- AOVOmn
    RsAOV$SimplEfb.1<- ResPosRob2
    RsAOV$SimplEfb.2<- ResAPA2b   
    
    RsAOV
    
  }
  
  # ▼▼▼ For checks, which are likely to be eliminated ▼▼▼
  {
    # I am not going to use it, it is only for testing purposes.
    ES.sum.REL.MAG.MM4<-function(REL.M=c(0.2, 0.5, 0.8),locfun=dts_stat,n = 10000,reps=100,SEED=TRUE){
      # Determine small medium and large equivalent measures of effect size based on the values in REL.M
      if(length(REL.M)!=3)stop('Should have three value in REL.M')
      if(n>10000)n=10000
      sm1=seq(-1,1, by=.0000001)
      output<-mclapply (1:reps, function(ii) {
        x=dnorm(sm1); y=dnorm(sm1)
        sapply(1:3, function(j)locfun(x,y-REL.M[j]))
      },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE, mc.set.seed = !SEED)
      output<-colSums(matrix(unlist(output), ncol=3, byrow=TRUE))/reps
      output
    }
    
    # It has been subsumed in the one that serves for more than 2 levels (shiftesAOVMM),
     # but I keep it to preserve compatibility and in the future it will be extinguished.
    shiftesMM<-function(xp,locfun=median,iter=100,SEED=TRUE,...){
      #
      #  Probabilistic measure of effect size: shift of the median.
      #
      ef.size=NA
      grp <- 1:length(xp);
      if(length(grp)>2)stop('There can only be two conditions, use Power Effect Size')
      N<-sapply(grp, function(j)sum(!is.na(xp[[j]])))
      nt=prod(N)
      if(sum(N<10)>0)stop('Q Shift cannot be calculated since a minimum of 10 data is needed per condition')
      ef.size<-
        ifelse(nt<10^6, {
          L=outer(xp[[1]],xp[[2]],FUN='-') #producto matricial}
          est=locfun(L)
          mean(L-est<=est)
        },
        {
          if(SEED)set.seed(2)
          L=NULL;  vef=NA
          nmin=min(c(N,100))     # El 100 no debería de ser iter??
          vef<-mclapply (1:iter, function(ii) {
            Samp<-lapply(grp, function(jj)sample(N[[jj]], size=nmin))
            L=outer(xp[[1]][Samp[[1]]],xp[[2]][Samp[[2]]],FUN='-')
            est=locfun(L)
            mean(L-est<=est)
          },mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE, mc.set.seed = !SEED)
          mean(unlist(vef))
        }
        )
      ef.size
    }
    
  }
  
  # ▲▲▲ For checks, which are likely to be eliminated ▲▲▲
}
# ▲▲▲================== Statistics: Robust ====================▲▲▲


  

  
  

  
# ▼▼▼======================== Adj Func ========================▼▼▼
# For function adjustment (come from LibrFunc.R)
{
  # ▼▼▼------------------- Revised & Checked --------------------▼▼▼
  {
    # Previous aosmic
    {
      
      # Functions from aosmic
      
      # Negative exponential - Monomulecolar growth
      monoGrowthMean <- function(predictor, a, b, c) {
        x <- predictor
        a - (a - b) * exp (- c * x)
      }
      
      monoGrowthInit <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        plateau <- max(y) * 1.05
        
        ## Linear regression on pseudo y values
        pseudoY <- log( 1 - (y / plateau ) )
        coefs <- coef( lm(pseudoY ~ x) )
        temp <- exp(coefs[1])
        b <- plateau * (1 - temp)
        c <- - coefs[2]
        a <- plateau
        value <- c(a, b, c)
        names(value) <- mCall[c("a", "b", "c")]
        value
      }
      
      NLSmonoGrowth <- selfStart(monoGrowthMean, monoGrowthInit, parameters=c("a", "b", "c"))
      
      #Logistic growth - 1 ###############################
      logiGrowth1Mean <- function(predictor, a, b, c) {
        x <- predictor
        a / (1 + exp(- b * x + c))
      }
      
      logiGrowth1Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        
        a <- max(y) * 1.05
        
        ## Linear regression on pseudo y values
        pseudoY <- log( (a / y ) - 1 )
        coefs <- coef( lm(pseudoY ~ x) )
        b <- - coefs[2]
        c <- coefs[1]
        value <- c(a, b, c)
        names(value) <- mCall[c("a", "b", "c")]
        value
      }
      
      NLSlogiGrowth.1 <- selfStart(logiGrowth1Mean, logiGrowth1Init, parameters=c("a", "b", "c"))
      
      
      #Logistic Growth 2
      logiGrowth2Mean <- function(predictor, a, b, c) {
        x <- predictor
        a / (1 + b * exp(- c * x))
      }
      
      logiGrowth2Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        
        a <- max(y) * 1.05
        
        ## Linear regression on pseudo y values
        pseudoY <- log( (a / (y + 0.0001) ) - 1 )
        coefs <- coef( lm(pseudoY ~ x) )
        c <- - coefs[2]
        k <- coefs[1]
        b <- exp(k)
        value <- c(a, b, c)
        names(value) <- mCall[c("a", "b", "c")]
        value
      }
      
      NLSlogiGrowth.2 <- selfStart(logiGrowth2Mean, logiGrowth2Init, parameters=c("a", "b", "c"))
      
      #Logistic Growth 3
      logiGrowth3Mean <- function(predictor, init, m, plateau) {
        x <- predictor
        init * plateau / (init + (plateau - init) * exp( - m * x))
      }
      
      logiGrowth3Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        
        plateau <- max(y) * 1.05
        
        ## Linear regression on pseudo y values
        pseudoY <- log( (plateau / (y + 0.0001) ) - 1 )
        coefs <- coef( lm(pseudoY ~ x) )
        b <- exp(coefs[1])
        init <- plateau / (1 + b)
        m <- - coefs[2]
        value <- c(init, m, plateau)
        names(value) <- mCall[c("init", "m", "plateau")]
        value
      }
      
      NLSlogiGrowth.3 <- selfStart(logiGrowth3Mean, logiGrowth3Init, parameters=c("init", "m", "plateau"))
      
      #Logistic Growth 4
      logiGrowth4Mean <- function(predictor, t50, m, plateau) {
        x <- predictor
        plateau / (1 + exp(- m * (x - t50)))}
      
      logiGrowth4Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        
        plateau <- max(y) * 1.05
        
        ## Linear regression on pseudo y values
        pseudoY <- log( (plateau / (y + 0.0001) ) - 1 )
        coefs <- coef( lm(pseudoY ~ x) )
        m <- - coefs[2]
        t50 <- coefs[1] / m
        value <- c(t50, m, plateau)
        names(value) <- mCall[c("t50", "m", "plateau")]
        value
      }
      
      NLSlogiGrowth.4 <- selfStart(logiGrowth4Mean, logiGrowth4Init, parameters=c("t50", "m", "plateau"))
      
      #Logistic function 5
      logistic5Mean <- function(predictor, a, b, c, d) {
        x <- predictor
        c + (d-c) / (1 + b * exp(a * x))
      }
      
      logistic5Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        
        d <- max(y) * 1.05
        c <- min(y) -0.0001
        ## Linear regression on pseudo y values
        pseudoY <- log((d - y) / (y - c ))
        coefs <- coef( lm(pseudoY ~ x) )
        a <- coefs[2]
        k <- coefs[1]
        b <- exp(k)
        value <- c(a, b, c, d)
        names(value) <- mCall[c("a", "b", "c", "d")]
        value
      }
      
      NLSlogistic.1 <- selfStart(logistic5Mean, logistic5Init, parameters=c("a", "b", "c", "d"))
      
      #Logistic function 6
      logistic6Mean <- function(predictor, a, b) {
        x <- predictor
        1 / (1 + b * exp(- a * x))
      }
      
      logistic6Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        
        ## Linear regression on pseudo y values
        pseudoY <- log((1 - y) / y)
        coefs <- coef( lm(pseudoY ~ x) )
        a <- - coefs[2]
        k <- coefs[1]
        b <- exp(k)
        value <- c(a, b)
        names(value) <- mCall[c("a", "b")]
        value
      }
      
      NLSlogistic.2 <- selfStart(logistic6Mean, logistic6Init, parameters=c("a", "b"))
      
      #LOG_LOGISTIC FUNCTIONS##########################################################
      
      
      #GOMPERTZ MODELS################################################################
      
      #Gompertz growth - 1
      gompGrowth1Mean <- function(predictor, a, m, c) {
        x <- predictor
        a * exp( - (m/c) * exp (-c * x))
      }
      
      gompGrowth1Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        
        plateau <- max(y) * 1.05
        
        ## Linear regression on pseudo y values
        pseudoY <- log( - log( y / plateau ) )
        coefs <- coef( lm(pseudoY ~ x) )
        k <- coefs[1]; c <- - coefs[2]
        b <- exp(k) 
        m <- b * c
        a <- plateau
        value <- c(a, m, c)
        names(value) <- mCall[c("a", "m", "c")]
        value
      }
      
      NLSgompGrowth.1 <- selfStart(gompGrowth1Mean, gompGrowth1Init, parameters=c("a", "m", "c"))
      
      #Gompertz growth 2
      gompGrowth2Mean <- function(predictor, a, b, c) {
        x <- predictor
        a * exp( - exp (b - c*x))
      }
      
      gompGrowth2Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        
        a <- max(y) * 1.05
        
        ## Linear regression on pseudo y values
        pseudoY <- log( - log( y / a ) )
        coefs <- coef( lm(pseudoY ~ x) )
        
        k <- coefs[1]
        c <- - coefs[2]
        b <- k
        value <- c(a, b, c)
        names(value) <- mCall[c("a", "b", "c")]
        value
      }
      
      NLSgompGrowth.2 <- selfStart(gompGrowth2Mean, gompGrowth2Init, parameters=c("a", "b", "c"))
      
      #Gompertz growth - 3
      gompGrowth3Mean <- function(predictor, a, b, c) {
        x <- predictor
        a * exp( -b * exp (-c*x))
      }
      
      gompGrowth3Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        
        a <- max(y) * 1.05
        
        ## Linear regression on pseudo y values
        pseudoY <- log( - log( y / a ) )
        coefs <- coef( lm(pseudoY ~ x) )
        
        k <- coefs[1]
        c <- - coefs[2]
        b <- exp(k)
        value <- c(a, b, c)
        names(value) <- mCall[c("a", "b", "c")]
        value
      }
      
      NLSgompGrowth.3 <- selfStart(gompGrowth3Mean, gompGrowth3Init, parameters=c("a", "b", "c"))
      
      
      #Extreme value
      extremeValueMean <- function(predictor, a, b, c) {
        x <- predictor
        a * (1 - exp( - exp (b - c*x)))
      }
      
      extremeValueInit <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        
        a <- max(y) * 1.05
        
        ## Linear regression on pseudo y values
        pseudoY <- log( - log( (a - y ) / a ) )
        coefs <- coef( lm(pseudoY ~ x) )
        
        k <- coefs[1]
        c <- - coefs[2]
        b <- k
        value <- c(a, b, c)
        names(value) <- mCall[c("a", "b", "c")]
        value
      }
      
      NLSextremeValue <- selfStart(extremeValueMean, extremeValueInit, parameters=c("a", "b", "c"))
      
      #WEIBULL TYPE MODELS
      #Weibull-1
      
      #Modified Mitscherlich equation for A vs PFD relationships
      AvsPFDMean <- function(predictor, Rd, Amax, Qapp) {
        x <- predictor
        Rd+Amax*(1-exp((-Qapp/Amax)*x))
      }
      
      AvsPFDInit <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        interc <- min(y)
        plateau <- max(y) * 1.05 - interc        
        ## Linear regression on pseudo y values
        pseudoY <- log( 1 - (((y - interc) / plateau ) ))
        coefs <- coef( lm(pseudoY ~ x - 1) )
        Amax <- plateau
        Rd <- interc
        b <-  coefs[1]
        Qapp <- -b*plateau
        value <- c(Rd,Amax,Qapp)
        names(value) <- mCall[c("Rd", "Amax", "Qapp")]
        value
      }
      
      NLSAvsPFD <- selfStart(AvsPFDMean, AvsPFDInit, parameters=c("Rd", "Amax", "Qapp"))
      
      #Inverse polynomial
      polyInv.3mean <- function(predictor, a, b, c) {
        x <- predictor
        1/(a + b*x + c*x^2)
      }
      
      polyInv.3Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        ## Linear regression on pseudo y values
        #pseudoY <- 1/y
        coefs <- coef(glm(y ~ x + I(x^2), family=gaussian(link="inverse")))
        a <- coefs[1]; b <- coefs[2]; c <- coefs[3]
        
        value <- c(a,b,c)
        names(value) <- mCall[c("a", "b", "c")]
        value
      }
      
      NLSpolyInv.3 <- selfStart(polyInv.3mean, polyInv.3Init, parameters=c("a", "b", "c"))
      
      #Inverse polynomial 2
      polyInv.4mean <- function(predictor, a, b, c) {
        x <- predictor
        x/(a + b*x + c*x^2)
      }
      
      polyInv.4Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        ## Linear regression on pseudo y values
        pseudoY <- y/x
        coefs <- coef(glm(pseudoY ~ x + I(x^2), family=gaussian(link="inverse")))
        a <- coefs[1]; b <- coefs[2]; c <- coefs[3]
        print(a);print(b);print(c)
        value <- c(a,b,c)
        names(value) <- mCall[c("a", "b", "c")]
        value
      }
      
      NLSpolyInv.4 <- selfStart(polyInv.4mean, polyInv.4Init, parameters=c("a", "b", "c"))
      
      
      
      # Bragg's equation
      logBragg.3.fun <- function(X, b, d, e){
        d * exp(- b * (log(X + 0.000001) - e)^2)
      }
      
      # Da fare
      DRC.logBragg.3 <- function(){
        fct <- function(x, parm) {
          logBragg.3.fun(x, parm[,1], parm[,2], parm[,3])
        }
        ssfct <- function(data){
          # Get the data     
          x <- log(data[, 1] + 0.000001)
          y <- data[, 2]
          
          d <- max(y)
          e <- x[which.max(y)]
          
          ## Linear regression on pseudo-y and pseudo-x
          pseudoY <- log( (y + 0.0001) / d )
          pseudoX <- (x - e)^2
          coefs <- coef( lm(pseudoY ~ pseudoX - 1) )
          b <- - coefs[1]
          start <- c(b, d, e)
          return( start )
        }
        names <- c("b", "d", "e")
        text <- "log-Bragg equation with three parameters"
        
        ## Returning the function with self starter and names
        returnList <- list(fct = fct, ssfct = ssfct, names = names, text = text)
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      
      # Negative exponential cumulative distribution #############
      negExpDist.fun <- function(predictor, c) {
        x <- predictor
        1 - exp (- c * x)
      }
      
      negExpDist.Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        ## Linear regression on pseudo y values
        pseudoY <- log( 1 - y )
        coefs <- coef( lm(pseudoY ~ x - 1) )
        c <- - coefs[1]
        value <- c(c)
        names(value) <- mCall[c("c")]
        value
      }
      
      NLS.negExpDist <- selfStart(negExpDist.fun, negExpDist.Init, 
                                  parameters=c("c"))
      
      DRC.negExpDist <-
        function(fixed = NA, names = c("c"))
        {
          ## Checking arguments
          numParm <- 1
          if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
          if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}
          
          ## Fixing parameters (using argument 'fixed')
          notFixed <- is.na(fixed)
          parmVec <- rep(0, numParm)
          parmVec[!notFixed] <- fixed[!notFixed]
          
          ## Defining the non-linear function
          fct <- function(x, parm)
          {
            parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
            parmMat[, notFixed] <- parm
            
            c <- parmMat[, 1]
            1 - exp (- c * x)
          }
          
          ## Defining self starter function
          ssfct <- function(dataf)
          {
            x <- dataf[, 1]
            y <- dataf[, 2]
            
            ## Linear regression on pseudo y values
            pseudoY <- log( 1 - y )
            coefs <- coef( lm(pseudoY ~ x - 1) )
            c <- - coefs[1]
            
            return(c(c)[notFixed])
          }
          
          ## Defining names
          pnames <- names[notFixed]
          
          ## Defining derivatives
          deriv1 <- function(x, parms){
            parmMat <- matrix(parmVec, nrow(parms), 
                              numParm, byrow = TRUE)
            parmMat[, notFixed] <- parms
            
            # Approximation by using finite differences
            a <- as.numeric(parmMat[,1])
            
            d1.1 <- negExpDist.fun(x, a)
            d1.2 <- negExpDist.fun(x, (a + 10e-7))
            d1 <- (d1.2 - d1.1)/10e-7
            d1
            # cbind(d1)[notFixed]
          }
          
          ## Defining the first derivative (in x=dose)
          derivx <- function(x, parm)
          {
            parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
            parmMat[, notFixed] <- parm
            
            a <- as.numeric(parmMat[,1])
            
            d1.1 <- negExpDist.fun(x, a)
            d1.2 <- negExpDist.fun((x + 10e-7), a)
            d1 <- (d1.2 - d1.1)/10e-7
            d1
          }
          
          ## Defining the ED function
          
          ## Defining the inverse function
          
          ## Defining descriptive text
          text <- "Negative exponential cumulative distribution function"
          
          ## Returning the function with self starter and names
          returnList <- list(fct = fct, ssfct = ssfct, names = pnames, 
                             text = text, noParm = sum(is.na(fixed)),
                             deriv1 = deriv1, derivx = derivx)
          
          class(returnList) <- "drcMean"
          invisible(returnList)
        }
      
      #Rational function ################################################
      # Ratio of two polynomials ############################
      Rational.fun <- function(predictor, a, b, c) {
        x <- predictor
        (b + c*x) / (1 + a*x)
      }
      
      Rational.Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        pseudoY <-  y 
        pseudoX <- x
        pseudoXY <- x*y
        coefs <- coef( lm(pseudoY ~ pseudoX + pseudoXY) )
        b <- coefs[1]        
        c <- coefs[2]
        a <- - coefs[3]
        value <- c(a, b, c)
        names(value) <- mCall[c("a", "b", "c")]
        value
      }
      
      NLS.Rational <- selfStart(Rational.fun, Rational.Init, parameters=c("a", "b", "c"))
      
      "DRC.Rational" <-
        function(fixed = c(NA, NA, NA), names = c("a", "b", "c"))
        {
          ## Checking arguments
          numParm <- 3
          if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
          if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}
          
          ## Fixing parameters (using argument 'fixed')
          notFixed <- is.na(fixed)
          parmVec <- rep(0, numParm)
          parmVec[!notFixed] <- fixed[!notFixed]
          
          ## Defining the non-linear function
          fct <- function(x, parm)
          {
            parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
            parmMat[, notFixed] <- parm
            
            a <- parmMat[, 1]; b <- parmMat[, 2]; c <- parmMat[, 3]
            (b + c*x)/(1 + a*x)
          }
          
          ## Defining self starter function
          ssfct <- function(dataf)
          {
            x <- dataf[, 1]
            y <- dataf[, 2]
            xy <- dataf[,1]*dataf[,2]
            
            ## Linear regression on pseudo y values
            
            coefs <- coef(lm(y ~ x + xy) )
            b <- coefs[1]
            c <- coefs[2]
            a <- - coefs[3]
            
            return(c(a, b, c)[notFixed])
          }
          
          ## Defining names
          pnames <- names[notFixed]
          
          ## Defining derivatives
          
          ## Defining the ED function
          
          ## Defining the inverse function
          
          ## Defining descriptive text
          text <- "Inverse polynomial 1"
          
          ## Returning the function with self starter and names
          returnList <- list(fct = fct, ssfct = ssfct, names = pnames, text = text, noParm = sum(is.na(fixed)))
          
          class(returnList) <- "drcMean"
          invisible(returnList)
        }
      
      # Hill function
      hillCurveMean <- function(predictor, a, b, c) {
        (a * predictor^c)/(b + predictor^c)
      }
      
      hillCurveInit <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["predictor"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        a <- max(y) * 1.05
        pseudoY <-  log(( a - y )/ y)
        pseudoX <- log(x)
        lmFit <- lm(pseudoY ~ pseudoX )
        coefs <- coef(lmFit)
        b <- exp(coefs[1])
        c <- - coefs[2]
        value <- c(a, b, c)
        names(value) <- mCall[c("a", "b", "c")]
        value
      }
      
      NLShillCurve <- selfStart(hillCurveMean, hillCurveInit, parameters=c("a", "b", "c"))
      
      "hill" <-
        function(fixed = c(NA, NA, NA), names = c("a", "b", "c"))
        {
          ## Checking arguments
          numParm <- 3
          if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
          if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}
          
          ## Fixing parameters (using argument 'fixed')
          notFixed <- is.na(fixed)
          parmVec <- rep(0, numParm)
          parmVec[!notFixed] <- fixed[!notFixed]
          
          ## Defining the non-linear function
          fct <- function(x, parm)
          {
            parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
            parmMat[, notFixed] <- parm
            
            a <- parmMat[, 1]; b <- parmMat[, 2]; c <- parmMat[, 3]
            (a * x^c)/(b + x^c)
          }
          
          ## Defining self starter function
          ssfct <- function(dataf)
          {
            x <- dataf[, 1]
            y <- dataf[, 2]
            
            a <- max(y) * 1.05
            
            ## Linear regression on pseudo y values
            pseudoY <-  log(( a - y )/ y)
            pseudoX <- log(x)
            coefs <- coef( lm(pseudoY ~ pseudoX ) )
            b <- exp(coefs[1])
            c <- - coefs[2]
            
            return(c(a, b, c)[notFixed])
          }
          
          ## Defining names
          pnames <- names[notFixed]
          
          ## Defining derivatives
          
          ## Defining the ED function
          
          ## Defining the inverse function
          
          ## Defining descriptive text
          text <- "Hill function (Morgan-Mercer-Flodin)"
          
          ## Returning the function with self starter and names
          returnList <- list(fct = fct, ssfct = ssfct, names = pnames, text = text, noParm = sum(is.na(fixed)))
          
          class(returnList) <- "drcMean"
          invisible(returnList)
        }
      
      "hillMax" <-
        function(fixed = c(NA, NA, NA), names = c("a", "b", "c"))
        {
          ## Checking arguments
          numParm <- 3
          if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
          if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}
          
          ## Fixing parameters (using argument 'fixed')
          notFixed <- is.na(fixed)
          parmVec <- rep(0, numParm)
          parmVec[!notFixed] <- fixed[!notFixed]
          
          ## Defining the non-linear function
          fct <- function(x, parm)
          {
            parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
            parmMat[, notFixed] <- parm
            
            a <- parmMat[, 1]; b <- parmMat[, 2]; c <- parmMat[, 3]
            (8 * 0.8 * (1 + b * exp ( -c * log(a))))/(1 + b * exp ( -c * log(x)))
          }
          
          ## Defining self starter function
          
          ## Defining names
          pnames <- names[notFixed]
          
          ## Defining descriptive text
          text <- "Hill function (Morgan-Mercer-Flodin) with expected value parameters replacement"
          
          ## Returning the function with self starter and names
          returnList <- list(fct = fct, names = pnames, text = text, noParm = sum(is.na(fixed)))
          
          class(returnList) <- "drcMean"
          invisible(returnList)
        }
      
      
      powerCurveNO.fun <- function(predictor, a, b, c) {
        a * ( predictor ^ b ) + c
      }
      
      
      "DRC.powerCurveNO" <- function(fixed = c(NA, NA, NA), names = c("a", "b", "c"))
      {
        ## Checking arguments
        numParm <- 3
        if (!is.character(names) | !(length(names) == numParm)) {stop("Not correct 'names' argument")}
        if (!(length(fixed) == numParm)) {stop("Not correct 'fixed' argument")}
        
        ## Fixing parameters (using argument 'fixed')
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        
        ## Defining the non-linear function
        fct <- function(x, parm)
        {
          
          
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          
          a <- parmMat[, 1]; b <- parmMat[, 2]; c <- parmMat[, 3]
          a * x ^(b) + c
        }
        
        ## Defining self starter function
        # ssfct <- function(dataf)
        # {
        #   x <- dataf[, 1]
        #   y <- dataf[, 2]
        #   
        #   #regression on pseudo y values
        #   pseudoY <- log( y + 0.00001)
        #   pseudoX <- log(x)
        #   coefs <- coef( lm(pseudoY ~ pseudoX) )
        #   a <- exp(coefs[1])
        #   
        #   b <- coefs[2]
        #   
        #   return(c(a, b)[notFixed])
        # }
        
        ## Defining names
        pnames <- names[notFixed]
        
        ## Defining derivatives
        
        ## Defining the ED function
        
        ## Defining the inverse function
        
        ## Defining descriptive text
        text <- "Power curve not passing for origin"
        
        ## Returning the function with self starter and names
        returnList <- list(fct = fct, names = pnames, text = text, noParm = sum(is.na(fixed)))
        
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
    }
    # Required libraries
    list.of.packages <- c("data.table","dplyr", "ggplot2", "knitr", "kableExtra", 
             "inflection", "deSolve", 
             "nlme","car", "drc", "aomisc", "RootsExtremaInflections", "minpack.lm",
             "ggplotify","nlraa","vegan", "ggtrendline", "HydroMe", "NRAIA", "NISTnls", "rTPC", "nls.multstart")
    #source("https://github.com/ManuMi68/StatMmRa/raw/main/Mas_aosmic.R")
    chkPkg(list.of.packages)
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    
    # I will expand it little by little
    NmMod=c("Pow1","Pow2","Pow3","Exp1","Exp2","Exp3","Exp4","Exp5",
            "Hyp1","Hyp2","Hyp3","Hyp4","Recip1","RegAs1","RegAs2","RegAs3",
            "Ratio1","Logis1","Logis2","Logis3","Logis4","Logis5","Logis6","Logis7","Logis8",
            "Gomp1","Extr1","Hill1","Lomo1","Chap1",
            "Weibull1","Weibull2","Weibull3","Weibull4","Weibull5",
            "CBeta1","Plateau1","Poly1")
    
    # Define Models
    {
    
      # Complement to dcr
      BC.3 <- function (fixed = c(NA, NA, NA), names = c("b", "e", "f"), ...) 
      {
        if (!is.character(names) | !(length(names) == 3)) {
          stop("Not correct 'names' argument")
        }
        return(drc::braincousens(names = c(names[1], "c", "d",names[2:3]), 
                            fixed = c(fixed[1], 0, 0, fixed[2:3]), fctName = as.character(match.call()[[1]]), 
                            fctText = "Brain-Cousens (hormesis) with lower limit and Upper horizontal asymptote fixed at 0", 
                            ...))
      }
      
    # A) dcr and nls
      # 1) # Asymptotic regression, Exponential decay
      NmRegModel<-c(
        "AR.2", "AR.3",
        "EXD.2", "EXD.3",
        "DRC.asymReg",
        "DRC.linear",
        "DRC.poly2",
        "DRC.SSasymp",
        "DRC.expoDecay",
        "DRC.expoGrowth",
        "DRC.powerCurve",
        "DRC.logCurve",
        "DRC.negExp",
        "DRC.negExpDist",
        "DRC.Rational",
        "hill",
        "hillMax",
        "DRC.powerCurveNO"
      ) # Sin Half 
      RegModel<-list(
        AR.2(), AR.3(),
        EXD.2(), EXD.3(),
        DRC.asymReg(),
        DRC.linear(),
        DRC.poly2(),
        DRC.SSasymp(),
        DRC.expoDecay(),
        DRC.expoGrowth(),
        DRC.powerCurve(),
        DRC.logCurve(),
        DRC.negExp(),
        DRC.negExpDist(),
        DRC.Rational(),
        hill(),
        hillMax(),
        DRC.powerCurveNO()
      ) # Sin Half 
      #R.RegModel=ProcMod(Efs,RegModel,NmRegModel,"E.Mod","AsymRegres")
      
      NmRegModel.nls<-c(
        "NLS.linear", "NLS.linearOrigin", "NLS.poly2",
        "NLSpolyInv.3", "NLSpolyInv.4",
        "NLS.expoGrowth", "NLS.expoDecay", "NLSmonoGrowth",
        # "NLS.asymReg",
        "NLS.powerCurve",
        "NLS.logCurve",
        # "NLS.logCurveNI", 	# Ya la he programado yo mismo a partir de aomisc
        "NLS.negExp", 		# La tengo que programar yo
        "NLS.negExpDist",
        "NLSextremeValue"
      )
      RegModel.nls<-list(
        expression(nlsLM(Y ~ NLS.linear(X, a, b), data = DTPas)),
        expression(nlsLM(Y ~ NLS.linearOrigin(X, b), data = DTPas)),
        expression(nlsLM(Y ~ NLS.poly2(X, a, b, c), data = DTPas)),
        expression(nlsLM(Y ~ NLSpolyInv.3(X, a, b, c), data = DTPas)),
        expression(nlsLM(Y ~ NLSpolyInv.4(X, a, b, c), data = DTPas)),
        expression(nlsLM(Y ~ NLS.expoGrowth(X, init, k), data = DTPas)),
        expression(nlsLM(Y ~ NLS.expoDecay(X, C0, k), data = DTPas)),
        expression(nlsLM(Y ~ NLSmonoGrowth(X, a, b, c), data = DTPas)),
        # expression(nlsLM(Y ~ NLS.asymReg(X, init, m, plateau), data = DTPas)), # Ya
        expression(nlsLM(Y ~ NLS.powerCurve(X, a, b), data = DTPas)),
        expression(nlsLM(Y ~ NLS.logCurve(X, a, b), data = DTPas)),
        # expression(nlsLM(Y ~ NLS.logCurveNI(X, b), data = DTPas)),         # Ya
        expression(nlsLM(Y ~ NLS.negExp(X, a, c), data = DTPas)),
        expression(nlsLM(Y ~ NLS.negExpDist(X, c), data = DTPas)),
        expression(nlsLM(Y ~ NLSextremeValue(X, a, b, c), data = DTPas))
      )
      
      # 2) Michaelis-Menten or Rectangular hyperbola
      NmMModel<-c(
        "MM.2", "MM.3"
      )
      MMModel <-list(
        MM.2(), MM.3()
      )
      # R.MMModel=ProcMod(Efs,MMModel,NmMModel,"E.Mod","MM")
      
      # This model is also included in the base NLMM2.MM (from Eq), but
       # I add it here so that the category is not empty.
      NmMModel.nls<-c(
        "NLS.MM.MM"
      )
      MMModel.nls <-list(
        expression(nlsLM(Y ~ NLS.MM.MM(X, Km, Vm), data = DTPas))
      )
      
      # To avoid conflicts with other packages
      L.2 <- function (upper = 1, fixed = c(NA, NA), names = c("b", "e")) 
      {
        numParm <- 2
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct length of 'fixed' argument")
        }
        return(drc::logistic(fixed = c(fixed[1], 0, upper, fixed[2], 1), 
                        names = c(names[1], "c", "d", names[2], "f"), fctName = as.character(match.call()[[1]]), 
                        fctText = "Logistic (ED50 as parameter)"))
      }
      
      # 3) logistic growth  
      NmLogModel<-c(
        "l2",  "l3",     "l3u",    "l4", "l5",
        "L.2", "L.3", "L.4",    "L.5",
        "LN.2", "LN.3",  "LN.3u",  "LN.4",
        "LL.2", "LL.3",  "LL.3u",  "LL.4",  "LL.5",
        "LL2.2","LL2.3", "LL2.3u", "LL2.4", "LL2.5",
        "FPL.4(-1,1)",
        "gaussian", "lgaussian",
        "logistic", "llogistic", "llogistic2",
        "lnormal")
      LogModel<-list(
        l2(), l3(), l3u(), l4(), l5(),
        L.2(), L.3(), L.4(), L.5(),
        LN.2(), LN.3(), LN.3u(), LN.4(),
        LL.2(), LL.3(), LL.3u(), LL.4(), LL.5(),
        LL2.2(), LL2.3(), LL2.3u(), LL2.4(), LL2.5(),
        FPL.4(-1,1),
        gaussian(), lgaussian(), # No van del todo bien
        drc::logistic(), llogistic(), llogistic2(),
        lnormal()
      )
      
      NmLogModel.nls<-c(
        "NLS.L4",  "NLS.L3",  "NLS.L2",
        "NLS.LL4", "NLS.LL3", "NLS.LL2",
        "NLSlogiGrowth.1", "NLSlogiGrowth.2", "NLSlogiGrowth.3", "NLSlogiGrowth.4",
        "NLSlogistic.1", "NLSlogistic.2",
        "NLShillCurve"
      )
      LogModel.nls<-list(
        expression(nlsLM(Y ~ NLS.L4(X, b,c,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.L3(X, b,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.L2(X, b,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.LL4(X, b,c,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.LL3(X, b,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.LL2(X, b,e), data = DTPas)),
        expression(nlsLM(Y ~ NLSlogiGrowth.1(X, a, b, c), data = DTPas)),
        expression(nlsLM(Y ~ NLSlogiGrowth.2(X, a, b, c), data = DTPas)),
        expression(nlsLM(Y ~ NLSlogiGrowth.3(X, init, m, plateau), data = DTPas)),
        expression(nlsLM(Y ~ NLSlogiGrowth.4(X, t50, m, plateau), data = DTPas)),
        expression(nlsLM(Y ~ NLSlogistic.1(X, a, b, c, d), data = DTPas)),
        expression(nlsLM(Y ~ NLSlogistic.2(X, a, b), data = DTPas)),
        expression(nlsLM(Y ~ NLShillCurve(X, a, b, c), data = DTPas))
      )
      # R.LogModel=ProcMod(Efs,LogModel,NmLogModel,"S.Mod","Log")
      
      # 4) Gompertz dose-response or growth curve
      NmGModel<-c(
        "G.2", "G.3", "G.3u", "G.4", 
        "E.2", "E.3", "E.4",
        "gompertz", "gompertzd"
      )
      GModel<-list(
        G.2(), G.3(), G.3u(), G.4(), 
        E.2(), E.3(), E.4(),
        gompertz(), gompertzd()
      )
      
      NmGModel.nls<-c(
        "NLS.G4",  "NLS.G3",  "NLS.G2",
        "NLS.E4",  "NLS.E3",  "NLS.E2",
        "NLSgompGrowth.1", "NLSgompGrowth.2", "NLSgompGrowth.3"
      )
      GModel.nls<-list(
        expression(nlsLM(Y ~ NLS.G4(X, b,c,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.G3(X, b,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.G2(X, b,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.E4(X, b,c,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.E3(X, b,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.E2(X, b,e), data = DTPas)),
        expression(nlsLM(Y ~ NLSgompGrowth.1(X, a, m, c), data = DTPas)),
        expression(nlsLM(Y ~ NLSgompGrowth.2(X, a, b, c), data = DTPas)),
        expression(nlsLM(Y ~ NLSgompGrowth.3(X, b,e), data = DTPas))
      )
      # R.GModel=ProcMod(Efs,GModel,NmGModel,"S.Mod","Gompertz")
      
      # 5) Weibull
      NmWModel<-c(
        "W1.2",  "W1.3",  "W1.4",
        "W2.2",  "W2.3",  "W2.4", 
        "W2x.3", "W1.3u", "W2.3u",
        "weibull1", "weibull2", "weibull2x",
        "w4", "W2x.4", "w3", "w2"
      )
      WModel<-list(
        W1.2(),  W1.3(),  W1.4(), 
        W2.2(),  W2.3(),  W2.4(), 
        W2x.3(), W1.3u(), W2.3u(),
        weibull1(), weibull2(), weibull2x(),
        w4(), W2x.4(), w3(), w2()
      )
      
      NmWModel.nls<-c(
        "NLS.W1.4","NLS.W1.3","NLS.W1.2",
        "NLS.MM.W2.4","NLS.W2.3","NLS.W2.2"
      )
      WModel.nls<-list(
        expression(nlsLM(Y ~ NLS.W1.4(X, b,c,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.W1.3(X, b,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.W1.2(X, b,e), data = DTPas)),
        # expression(nlsLM(Y ~ NLS.W2.4(X, b,c,d,e), data = DTPas)), # tiene un error, la he corregido
        expression(nlsLM(Y ~ NLS.MM.W2.4(X, b, c, d, e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.W2.3(X, b,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.W2.2(X, b,e), data = DTPas))
      )
      # R.WModel=ProcMod(Efs,WModel,NmWModel,"S.Mod","Weibull")
      
      # 6) Brain-Cousens hormesis models
      NmBCModel<-c(
        "BC.4", "BC.5",
        "bcl3", "bcl4",
        "braincousens",
        "BC.3"
      )
      BCModel<-list(
        BC.4(), BC.5(),
        bcl3(), bcl4(),
        braincousens(),
        BC.3()
      )
      
      NmBCModel.nls<-c(
      )
      BCModel.nls<-list(
      )
      # R.BCModel=ProcMod(Efs,BCModel,NmBCModel,"M.Mod","Brain-Cousens")
      
      # 7) Cedergreen-Ritz-Streibig, Hormesis-type
      # ucedergreen() is the general way to vary alpha into other values
       # I have taken advantage of the nls part to include
       # those of the nls_multstart package.
      NmCRSModel<-c(
        "CRS.4a", "CRS.4b", "CRS.4c",
        "CRS.5a", "CRS.5b", "CRS.5c",
        "UCRS.4a", "UCRS.4b", "UCRS.4c",
        "UCRS.5a", "UCRS.5b", "UCRS.5c",
        "CRS.6",
        "ml4a", "ml4b", "ml4c",
        "uml4a", "uml4b", "uml4c",
        "ml3b", "ml3a", "ml3c",
        "uml3a", "uml3b", "uml3c"
      )
      CRSModel<-list(
        CRS.4a(),  CRS.4b(),  CRS.4c(), 
        CRS.5a(),  CRS.5b(),  CRS.5c(),
        UCRS.4a(), UCRS.4b(), UCRS.4c(),
        UCRS.5a(), UCRS.5b(), UCRS.5c(),
        CRS.6(),
        ml4a(), ml4b(), ml4c(),
        uml4a(), uml4b(), uml4c(),
        ml3b(), ml3a(), ml3c(),
        uml3a(), uml3b(), uml3c()
      )
      
      NmCRSModel.nls<-c(
        "beta_2012", "boatman_2017", "briere2_1999", "delong_2017", "deutsch_2008", "flinn_1991",
        "gaussian_1987", "hinshelwood_1947", "joehnk_2008", "johnsonlewin_1946", "kamykowski_1985", "lactin2_1995",
        "lrf_1991", "modifiedgaussian_2006", "oneill_1972", "pawar_2018", "quadratic_2008", "ratkowsky_1983",
        "rezende_2019","sharpeschoolfull_1981", "sharpeschoolhigh_1981", "sharpeschoollow_1981",  "spain_1982",
        "thomas_2012", "thomas_2017", "weibull_1995"
      )
      CRSModel.nls<-list(
        expression(nls_multstart(Y~beta_2012(X,a, b, c, d, e),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'beta_2012')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'beta_2012')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'beta_2012'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'beta_2012'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~boatman_2017(X, rmax, tmin, tmax, a, b),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'boatman_2017')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'boatman_2017')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'boatman_2017'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'boatman_2017'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~briere2_1999(X, tmin, tmax, a, b),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'briere2_1999')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'briere2_1999')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'briere2_1999'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'briere2_1999'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~delong_2017(X, c, eb, ef, tm, ehc),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'delong_2017')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'delong_2017')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'delong_2017'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'delong_2017'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~deutsch_2008(X, rmax, topt, ctmax, a),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'deutsch_2008')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'deutsch_2008')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'deutsch_2008'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'deutsch_2008'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~flinn_1991(X, a, b, c),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'flinn_1991')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'flinn_1991')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'flinn_1991'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'flinn_1991'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~gaussian_1987(X,rmax, topt, a),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'gaussian_1987')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'gaussian_1987')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'gaussian_1987'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'gaussian_1987'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~hinshelwood_1947(X,a, e, b, eh),data = DTPas,iter = 500,
                           start_lower = get_start_vals(DTPas$X,DTPas$Y,'hinshelwood_1947')-10,
                           start_upper = get_start_vals(DTPas$X,DTPas$Y,'hinshelwood_1947')+10,
                           lower = get_lower_lims(DTPas$X,DTPas$Y,'hinshelwood_1947'),
                           upper = get_upper_lims(DTPas$X,DTPas$Y,'hinshelwood_1947'),
                           supp_errors = "Y")),
        expression(nls_multstart(Y~joehnk_2008(X,rmax, topt, a, b, c),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'joehnk_2008')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'joehnk_2008')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'joehnk_2008'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'joehnk_2008'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~johnsonlewin_1946(X,r0, e, eh, topt),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'johnsonlewin_1946')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'johnsonlewin_1946')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'johnsonlewin_1946'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'johnsonlewin_1946'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~kamykowski_1985(X,tmin, tmax, a, b, c),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'kamykowski_1985')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'kamykowski_1985')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'kamykowski_1985'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'kamykowski_1985'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~lactin2_1995(X,a, b, tmax, delta_t),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'lactin2_1995')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'lactin2_1995')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'lactin2_1995'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'lactin2_1995'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~lrf_1991(X,rmax, topt, tmin, tmax),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'lrf_1991')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'lrf_1991')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'lrf_1991'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'lrf_1991'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~modifiedgaussian_2006(X,rmax, topt, a, b),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'modifiedgaussian_2006')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'modifiedgaussian_2006')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'modifiedgaussian_2006'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'modifiedgaussian_2006'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~oneill_1972(X,rmax, ctmax, topt, q10),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'oneill_1972')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'oneill_1972')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'oneill_1972'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'oneill_1972'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~pawar_2018(X, r_tref, e, eh, topt, tref=5),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'pawar_2018')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'pawar_2018')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'pawar_2018'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'pawar_2018'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~quadratic_2008(X,a, b, c),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'quadratic_2008')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'quadratic_2008')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'quadratic_2008'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'quadratic_2008'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~ratkowsky_1983(X,tmin, tmax, a, b),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'ratkowsky_1983')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'ratkowsky_1983')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'ratkowsky_1983'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'ratkowsky_1983'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~rezende_2019(X,q10, a, b, c),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'rezende_2019')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'rezende_2019')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'rezende_2019'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'rezende_2019'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~sharpeschoolfull_1981(X,r_tref, e, el, tl, eh, th, tref=5),data = DTPas,
                      iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'sharpeschoolfull_1981')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'sharpeschoolfull_1981')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'sharpeschoolfull_1981'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'sharpeschoolfull_1981'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~sharpeschoolhigh_1981(X,r_tref, e, eh, th, tref=5),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'sharpeschoolhigh_1981')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'sharpeschoolhigh_1981')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'sharpeschoolhigh_1981'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'sharpeschoolhigh_1981'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~sharpeschoollow_1981(X,r_tref, e, el, tl, tref=5),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'sharpeschoollow_1981')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'sharpeschoollow_1981')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'sharpeschoollow_1981'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'sharpeschoollow_1981'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~spain_1982(X, a, b, c, r0),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'spain_1982')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'spain_1982')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'spain_1982'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'spain_1982'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~thomas_2012(X,a, b, c, topt),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'thomas_2012')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'thomas_2012')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'thomas_2012'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'thomas_2012'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~thomas_2017(X,a, b, c, d, e),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'thomas_2017')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'thomas_2017')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'thomas_2017'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'thomas_2017'),
                      supp_errors = "Y")),
        expression(nls_multstart(Y~weibull_1995(X,a, topt, b, c),data = DTPas,iter = 500,
                      start_lower = get_start_vals(DTPas$X,DTPas$Y,'weibull_1995')-10,
                      start_upper = get_start_vals(DTPas$X,DTPas$Y,'weibull_1995')+10,
                      lower = get_lower_lims(DTPas$X,DTPas$Y,'weibull_1995'),
                      upper = get_upper_lims(DTPas$X,DTPas$Y,'weibull_1995'),
                      supp_errors = "Y"))
      )
      # R.CRSModel=ProcMod(Efs,CRSModel,NmCRSModel,"M.Mod","CRS")
      
      
      # 8) Bragg, Lorentz & Beta Maxim/Minim
      # You have to adapt it, it changes a little
      # They are included among those of aomis
      NmBragModel<-c(
        "DRC.bragg.3",
        "DRC.bragg.4",
        "DRC.logBragg.3",
        "DRC.lorentz.3",
        "DRC.lorentz.4",
        "DRC.beta",
        "DRC.cousens85",
        "DRC.YL"
      )
      BragModel<-list(
        DRC.bragg.3(),
        DRC.bragg.4(),
        DRC.logBragg.3(),
        DRC.lorentz.3(),
        DRC.lorentz.4(),
        DRC.beta(),
        DRC.cousens85(),
        DRC.YL()
      )
      
      NmBragModel.nls<-c(
        "NLS.bragg.3", "NLS.bragg.4",
        "NLS.lorentz.3", "NLS.lorentz.4",
        "NLS.beta",
        "NLS.YL"
      )
      BragModel.nls<-list(
        expression(nlsLM(Y ~ NLS.bragg.3(X, b,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.bragg.4(X, b,c,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.lorentz.3(X, b,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.lorentz.4(X, b,c,d,e), data = DTPas)),
        expression(nlsLM(Y ~ NLS.beta(X, b,d,Xb, Xo,Xc), data = DTPas)),
        expression(nlsLM(Y ~ NLS.YL(X, i,A), data = DTPas))
      )
      # R.BragModel=ProcMod(Efs,BragModel,NmBragModel,"M.Mod","Bragg")
      
      # 9) no effect concentration (NEC) & Rare
      NmSpModel<-c(
        "NEC.2","NEC.3","NEC.4",
        "baro5", "gammadr", "multi2",
        "twophase",
        "FPL.4(1,1)", "FPL.4(-1,-1)", "FPL.4(-1,1)", "FPL.4(-2,3)", "FPL.4(-.5,.5)"
      )
      SpModel<-list(
        NEC.2(),NEC.3(),NEC.4(),
        baro5(), gammadr(), multi2(),
        twophase(),
        FPL.4(1,1), FPL.4(-1,-1), FPL.4(-1,1), FPL.4(-2,3), FPL.4(-.5,.5)
      )
      
      # This model is also included in the base NLMM2.MM (from Eq), but
        # I add it here so that the category is not empty.
      NmSpModel.nls <-c(
        "Green_Ampt"
      )
      SpModel.nls<-list(
        expression(nlsLM(Y ~ SSgampt(X,ks,A), data = DTPas))                      # Green-Ampt
      )
      # R.SpModel=ProcMod(Efs,SpModel,NmSpModel,"M.Mod","RareMod")
      
      # 10) Other
      aomiscModel<-list(
        DRC.asymReg(),
        DRC.beta(),
        DRC.bragg.3(),
        DRC.bragg.4(),
        DRC.logBragg.3(),
        DRC.lorentz.3(),
        DRC.lorentz.4(),
        DRC.cousens85(),
        DRC.expoDecay(),
        DRC.expoGrowth(),
        DRC.linear(),
        DRC.logCurve(),
        DRC.negExp(),
        DRC.poly2(),
        DRC.powerCurve(),
        DRC.SSasymp(),
        DRC.YL()
      )
      
      # Other later ones to incorporate
      NmnlraaModel<- list(
        "SSbell",
        "SSbeta5",
        "SSbg4rp",
        "SSbgf",
        "SSbgf4",
        "SSbgrp",
        "SSblin",
        "SScard3",
        "SSdlf",
        "SSexpf",
        "SSexpfp",
        "SSexplin",
        "SShill1",
        "SShill2",
        "SShill3",
        "SSlinp",
        "SSlogis5",
        "SSmoh",
        "SSnrh",
        "SSpexpf",
        "SSplin",
        "SSpquad",
        "SSpquad3",
        "SSprofd",
        "SSquadp",
        "SSquadp3",
        "SSquadp3xs",
        "SSratio",
        "SSricker",
        "SSscard3",
        "SSsharp",
        "SStemp3",
        "SStrlin"
      )
      
      veganModels<- list (
        "SSarrhenius", "SSgleason", "SSgitay", "SSlomolino"
        )
      
      ggtrendlineModels<-list("SSexp2P", "SSexp3P", "SSpower2P", "SSpower3P")
      
      HydroMeModels <-list("SSgampt","SSgard","SShorton", "SSkosugi", "SSomuto", "SSphilip", "SSvgm","SSvgm4","SSfredlund")
      
      AllModels.drc <-c(
        RegModel,
        GModel,
        BCModel,
        WModel,
        SpModel,
        CRSModel,
        MMModel,
        LogModel,
        aomiscModel
      )
      # drc
      E.Models<-c(RegModel,MMModel)
      S.Models<-c(LogModel, GModel, WModel)
      M.Models<-c(BCModel, CRSModel, BragModel,SpModel)
      
      E.Models.Nm<-c(NmRegModel,NmMModel)
      S.Models.Nm<-c(NmLogModel, NmGModel, NmWModel)
      M.Models.Nm<-c(NmBCModel, NmCRSModel, NmBragModel, NmSpModel)
      
      AllModels <- c(E.Models,S.Models,M.Models)
      AllModels.Nm <- c(E.Models.Nm,S.Models.Nm,M.Models.Nm)
      
      #nls
      E.Models.nls<-c(RegModel.nls,MMModel.nls)
      S.Models.nls<-c(LogModel.nls, GModel.nls, WModel.nls)
      M.Models.nls<-c(BCModel.nls, CRSModel.nls, BragModel.nls,SpModel.nls)
      
      E.Models.Nm.nls<-c(NmRegModel.nls,NmMModel.nls)
      S.Models.Nm.nls<-c(NmLogModel.nls, NmGModel.nls, NmWModel.nls)
      M.Models.Nm.nls<-c(NmBCModel.nls, NmCRSModel.nls, NmBragModel.nls, NmSpModel.nls)
      
      AllModels.nls <- c(E.Models.nls, S.Models.nls, M.Models.nls)
      AllModels.Nm.nls <- c(E.Models.Nm.nls, S.Models.Nm.nls, M.Models.Nm.nls)
      
      AllModelsJn <- c(AllModels, AllModels.nls)
      AllModelsJn.Nm <- c(AllModels.Nm, AllModels.Nm.nls)
      
    # B) Second list from direct equations,
       # which I will elaborate little by little because it is very laborious.
      # First, the basic definitions
      MM.Recip1.Mod <- function(X, Alfa, Beta) {Alfa + Beta/X}
      MM.Recip1.Init <- function(mCall, LHS, data,...) {
        xy <- sortedXyData(mCall[["X"]], LHS, data)
        x <- xy[, "x"]
        y <- xy[, "y"]
        pseudoY <- log(y + 1e-06)
        coefs <- coef(lm(pseudoY ~ x))
        k <- coefs[1]
        b <- coefs[2]
        a <- exp(k)
        value <- c(k, b)
        names(value) <- mCall[c("Alfa", "Beta")]
        value
      }
      SSMMRecip1 <- selfStart(MM.Recip1.Mod, MM.Recip1.Init, c("Alfa", "Beta"))
      
      MM.Hyper4.Mod <- function(X, Alfa, Beta) {Alfa/(1+Beta*X)}
      SSMMHyper4 <- selfStart(MM.Hyper4.Mod, MM.Recip1.Init, c("Alfa", "Beta"))
      
      #Logarithmic regression without intercept
      logCurveNI.fun <- function(X, b) {
        x <- X
        b * log(x)
      }
      logCurveNI.Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["X"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        pseudoY <-  y 
        pseudoX <- log(x)
        coefs <- coef( lm(pseudoY ~ pseudoX - 1) )
        b <- coefs[1]
        value <- c(b)
        names(value) <- mCall[c("b")]
        value
      }
      NLS.logCurveNI <- selfStart(logCurveNI.fun, logCurveNI.Init, parameters=c("b"))
      
      MM.W2.4.Mod <- function(X, b, c, d, e) {c + (d - c) * (1 - exp(-exp(b * (log(X + 1e-07) - log(e)))))}
      MM.W2.4.Init <- function(mCall, LHS, data,...) {
        xy <- sortedXyData(mCall[["X"]], LHS, data)
        x <- xy[, "x"]
        y <- xy[, "y"]
        d <- max(y) * 1.05
        c <- min(y) * 0.9
        pseudoY <- log(-log((d - y)/(d - c)))
        coefs <- coef(lm(pseudoY ~ log(x + 1e-07)))
        b <- coefs[2]
        e <- exp(-coefs[1]/b)
        value <- c(b, c, d, e)
        names(value) <- mCall[c("b", "c", "d", "e")]
        value
      }
      NLS.MM.W2.4 <- selfStart(MM.W2.4.Mod, MM.W2.4.Init, c("b", "c", "d", "e"))
      
      Rational.fun <- function(X, a, b, c) {(b + c*X) / (1 + a*X)}
      Rational.Init <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["X"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        pseudoY <-  y 
        pseudoX <- x
        pseudoXY <- x*y
        coefs <- coef( lm(pseudoY ~ pseudoX + pseudoXY) )
        b <- coefs[1]        
        c <- coefs[2]
        a <- - coefs[3]
        value <- c(a, b, c)
        names(value) <- mCall[c("a", "b", "c")]
        value
      }
      NLS.Rational <- selfStart(Rational.fun, Rational.Init, parameters=c("a", "b", "c"))
      
      AvsPFDMean <- function(X, Rd, Amax, Qapp) {Rd+Amax*(1-exp((-Qapp/Amax)*X))}
      AvsPFDInit <- function(mCall, LHS, data, ...) {
        xy <- sortedXyData(mCall[["X"]], LHS, data)
        x <-  xy[, "x"]; y <- xy[, "y"]
        interc <- min(y)
        plateau <- max(y) * 1.05 - interc        
        ## Linear regression on pseudo y values
        pseudoY <- log( 1 - (((y - interc) / plateau ) ))
        coefs <- coef( lm(pseudoY ~ x - 1) )
        Amax <- plateau
        Rd <- interc
        b <-  coefs[1]
        Qapp <- -b*plateau
        value <- c(Rd,Amax,Qapp)
        names(value) <- mCall[c("Rd", "Amax", "Qapp")]
        value
      }
      NLSAvsPFD <- selfStart(AvsPFDMean, AvsPFDInit, parameters=c("Rd", "Amax", "Qapp"))
      
      # data=Efs
      MM.MM.fun <-function(X, Km, Vm) { (Vm * X)/(Km + X) }
      MM.MM.Init <-function(mCall, LHS, data, ...) {
        ecb.MM <- function (data, unit_S = "mM", unit_v = "au") 
        {
          colnames(data)[1] <- "S"
          colnames(data)[2:length(colnames(data))] <- LETTERS[1:length(colnames(data)) - 
                                                                1]
          data <- data[complete.cases(data), ]
          if (ncol(data) > 2) {
            tdata <- apply(data[, -1], MARGIN = 2, function(x) x/data[, 
                                                                      1])
            mean_v <- apply(data[, -1], MARGIN = 1, mean)
            mean_v_S <- apply(tdata, MARGIN = 1, mean)
          }
          else {
            tdata <- data[, 2]/data[, 1]
            mean_v <- data[, 2]
            mean_v_S <- tdata
          }
          tdata <- as.data.frame(tdata)
          names(tdata)[1] <- "v_S"
          tdata <- cbind(data, tdata)
          names(tdata)[2] <- "v"
          tdata<-as.data.frame(tdata)
          nc <- ncol(data) + 1
          line.intersect <- function(v, v_S) {
            Kms <- Vms <- c()
            for (i in 1:(length(v) - 1)) {
              for (j in (i + 1):length(v)) {
                Km <- (v[j] - v[i])/(v_S[i] - v_S[j])
                Kms <- c(Kms, Km)
                Vm <- (v_S[i] * (v[j] - v[i])/(v_S[i] - v_S[j])) + 
                  v[j]
                Vms <- c(Vms, Vm)
              }
            }
            Km <- median(Kms, na.rm = TRUE)
            Vm <- median(Vms, na.rm = TRUE)
            return(c(Km, Vm))
          }
          Kms <- Vms <- c()
          c <- 0
          for (i in 2:(nc - 1)) {
            p <- line.intersect(tdata[, i], tdata[, nc + c])
            Kms <- c(p[1], Kms)
            Vms <- c(p[2], Vms)
            c <- c + 1
          }
          Km <- paste("Km: ", round(mean(Kms), 3), " ± ", round(sd(Kms), 
                                                                3), " ", unit_S)
          Vm <- paste("Vm: ", round(mean(Vms), 3), " ± ", round(sd(Vms), 
                                                                3), " ", unit_v)
          fitted_parameters <- c(round(mean(Kms, na.rm = TRUE), 3), 
                                 round(mean(Vms, na.rm = TRUE), 3))
          names(fitted_parameters) <- c("Km", "Vm")
          
          output <- list(fitted_parameters, Km, Vm)
          names(output) <- c("fitted_parameters", "Km", "Vm")
          return(output)
        }
        xy <- sortedXyData(mCall[["X"]], LHS, data)
        t <- ecb.MM(xy)
        K <- t$fitted_parameters[1]
        V <- t$fitted_parameters[2]
        value <- c(K, V)
        names(value) <- mCall[c("Km", "Vm")]
        value
      }
      NLS.MM.MM <- selfStart(MM.MM.fun, MM.MM.Init, parameters=c("Km", "Vm"))
      
      lb.MM<- function (data, unit_S = "mM", unit_v = "au", weighting = FALSE, 
                        plot = TRUE) 
      {
        data <- as.data.frame(data[complete.cases(data), ])
        colnames(data)[1] <- "S"
        tdata <- apply(data, MARGIN = 2, function(x) 1/x)
        tdata <- as.data.frame(tdata, 4)
        names(tdata)[1] <- "inv_S"
        Kms <- Vms <- R2 <- SE <- AIC <- c()
        for (j in 2:ncol(tdata)) {
          model <- lm(tdata[, j] ~ tdata$inv_S)
          Vm <- round(1/model$coefficients[1], 2)
          Km <- round(Vm * model$coefficients[2], 2)
          Vms <- c(Vms, Vm)
          Kms <- c(Kms, Km)
          R2 <- c(R2, round(summary(model)$r.squared,2))
          SE <- c(SE, round(summary(model)$sigma,4))
          AIC <-c(AIC, round(AIC(model),2))
        }
        if (ncol(tdata) > 2) {
          tdata$inv_v <- round(apply(tdata[, -1], MARGIN = 1, mean), 4)
          tdata$sd <- round(apply(tdata[, -c(1, ncol(tdata))], MARGIN = 1, sd), 4)
        }
        else {
          names(tdata)[2] <- "inv_v"
        }
        if (weighting) {
          model <- lm(tdata$inv_v ~ tdata$inv_S, weights = 1/(tdata$inv_v)^4)
        }
        else {
          model <- lm(tdata$inv_v ~ tdata$inv_S)
        }
        Vm <- round(1/model$coefficients[1], 2)
        Km <- round(Vm * model$coefficients[2], 2)
        if (plot == TRUE) {
          parameters <- paste("Km: ", Km, "     Vm: ", Vm, sep = "")
          plot(tdata$inv_S, tdata$inv_v, ty = "p", 
               ylim = c(0, max(tdata[, -1]) + 0.1 * max(tdata[, -1])), 
               xlab = paste("1/[S] (1/", unit_S, ")", sep = ""),
               ylab = paste("1/v (1/", unit_v,")", sep = ""), main = parameters)
          abline(model)
          if (ncol(tdata) > 2) {
            arrows(tdata$inv_S, tdata$inv_v - tdata$sd, tdata$inv_S, 
                   tdata$inv_v + tdata$sd, length = 0.05, angle = 90, 
                   code = 3)
          }
        }
        fitted_parameters <- c(Km, Vm)
        names(fitted_parameters) <- c("Km", "Vm")
        Resf.MM<-paste0(c("Km", "Vm"),"= ", c(Km, Vm),collapse="; ")
        Res.MM=NA
        Res.MM= as.data.table(list(
          AIC=AIC,
          LogLik=logLik(model)[[1]],
          SE=SE,
          coefs =Resf.MM))
        
        output <- list(Km= unname(Kms), Vm=unname(Vms), R2 = R2, SE= SE, AIC= AIC,
                       fitted_parameters=fitted_parameters, inverse_data= tdata, ResAPA =Res.MM)
        names(output) <- c("Km", "Vm", "R2", "SE", "AIC","fitted_parameters", "inverse_data", "ResAPA")
        return(output)
      }
      
      hw.MM <- function (data, unit_S = "mM", unit_v = "au", plot = TRUE) 
      {
        data <- as.data.frame(data[complete.cases(data), ])
        colnames(data)[1] <- "S"
        tdata <- apply(data, MARGIN = 2, function(x) 1/x)
        tdata <- as.data.frame(round(tdata, 4))
        tdata[, 1] <- 1/tdata[, 1]
        if (ncol(tdata) > 2) {
          tdata[, 2:ncol(tdata)] <- apply(tdata[, -1], MARGIN = 2, 
                                          function(x) x * tdata[, 1])
        }
        else {
          tdata[, 2] <- tdata[, 1] * tdata[, 2]
        }
        Kms <- Vms <- R2 <- SE <- AIC <- c()
        for (j in 2:ncol(tdata)) {
          model <- lm(tdata[, j] ~ tdata$S)
          Vm <- round(1/model$coefficients[2], 2)
          Km <- round(Vm * model$coefficients[1], 2)
          Vms <- c(Vms, Vm)
          Kms <- c(Kms, Km)
          R2 <- c(R2, summary(model)$r.squared)
          SE <- c(SE, round(summary(model)$sigma,4))
          AIC <-c(AIC, round(AIC(model),2))
        }
        if (ncol(tdata) > 2) {
          tdata$S_v <- round(apply(tdata[, -1], MARGIN = 1, mean),4)
          tdata$sd <- round(apply(tdata[, -c(1, ncol(tdata))],MARGIN = 1, sd), 4)
        }
        else {
          names(tdata)[2] <- "S_v"
        }
        model <- lm(tdata$S_v ~ tdata$S)
        Vm <- round(1/model$coefficients[2], 2)
        Km <- round(Vm * model$coefficients[1], 2)
        parameters <- paste("Km: ", Km, "     Vm: ", Vm, sep = "")
        if (plot) {
          plot(tdata$S, tdata$S_v, ty = "p", 
               ylim = c(0, max(tdata[, -1]) + 0.1 * max(tdata[, -1])),
               xlab = paste("[S] (", unit_S, ")", sep = ""),
               ylab = paste("[S]/v (", unit_S, "/", unit_v, ")", sep = ""), main = parameters)
          abline(model)
          if (ncol(tdata) > 2) {
            arrows(tdata$S, tdata$S_v - tdata$sd, tdata$S, tdata$S_v + 
                     tdata$sd, length = 0.05, angle = 90, code = 3)
          }
        }
        fitted_parameters <- c(Km, Vm)
        names(fitted_parameters) <- c("Km", "Vm")
        Resf.MM<-paste0(c("Km", "Vm"),"= ", c(Km, Vm),collapse="; ")
        Res.MM=NA
        Res.MM= as.data.table(list(
          AIC=AIC,
          LogLik=logLik(model)[[1]],
          SE=SE,
          coefs =Resf.MM))
        output <- list(Km= unname(Kms), Vm=unname(Vms), R2 = R2, SE= SE, AIC= AIC,
                       fitted_parameters=fitted_parameters, transformed_data= tdata,
                       ResAPA= Res.MM)
        names(output) <- c("Km", "Vm", "R2", "SE", "AIC", "fitted_parameters", "transformed_data","ResAPA")
        return(output)
      }
      
      eh.MM <- function (data, unit_S = "mM", unit_v = "au", plot = TRUE) 
      {
        data <- as.data.frame(data[complete.cases(data), ])
        colnames(data)[1] <- "S"
        replicates <- ncol(data) - 1
        if (replicates > 1) {
          tdata <- apply(data[, -1], MARGIN = 2, function(x) x/data[, 1])
          tdata <- as.data.frame(tdata)
          tdata$v_S <- apply(tdata, MARGIN = 1, mean)
          tdata$x_sd <- apply(tdata, MARGIN = 1, sd)
        }
        else {
          tdata <- data.frame(v_S = data[, 2]/data[, 1])
        }
        x_max <- max(tdata)
        if (replicates > 1) {
          tdata[, (ncol(tdata) + 1):(ncol(tdata) + ncol(data) - 
                                       1)] <- data[, -1]
          tdata$v <- apply(data[, -1], MARGIN = 1, mean)
          tdata$y_sd <- apply(data[, -1], MARGIN = 1, sd)
        }
        else {
          tdata$v <- data[, 2]
        }
        y_max <- max(data)
        Kms <- Vms <- R2 <- SE <- AIC <- c()
        if (replicates > 1) {
          for (j in 1:replicates) {
            model <- lm(tdata[, (replicates + 2 + j)] ~ tdata[, 
                                                              j])
            Km <- round(-model$coefficients[2], 2)
            Vm <- round(model$coefficients[1], 2)
            Vms <- c(Vms, Vm)
            Kms <- c(Kms, Km)
            R2 <- c(R2, round(summary(model)$r.squared,2))
            SE <- c(SE, round(summary(model)$sigma,4))
            AIC <-c(AIC, round(AIC(model),2))
          }
        }
        model <- lm(tdata$v ~ tdata$v_S)
        Km <- round(-model$coefficients[2], 2)
        Vm <- round(model$coefficients[1], 2)
        if (replicates == 1) {
          Vms <- Vm
          Kms <- Km
          R2 <- round(summary(model)$r.squared,2)
          SE <- round(summary(model)$sigma,4)
          AIC <-round(AIC(model),2)
        }
        parameters <- paste("Km: ", Km, "     Vm: ", Vm, sep = "")
        if (plot) {
          plot(tdata$v_S, tdata$v, ty = "p", xlim = c(0, x_max + 0.1 * x_max),
               ylim = c(0, y_max + 0.1 * y_max), 
               xlab = paste("v/[S] (", unit_v, "/", unit_S, ")", sep = ""), 
               ylab = paste("v (",  unit_v, ")", sep = ""), main = parameters)
          abline(model)
          if (ncol(tdata) > 2) {
            arrows(tdata$v_S, tdata$v - tdata$y_sd, tdata$v_S, 
                   tdata$v + tdata$y_sd, length = 0.05, angle = 90, 
                   code = 3)
            arrows(tdata$v_S - tdata$x_sd, tdata$v, tdata$v_S + 
                     tdata$x_sd, tdata$v, length = 0.05, angle = 90, 
                   code = 3)
          }
        }
        fitted_parameters <- c(Km, Vm)
        names(fitted_parameters) <- c("Km", "Vm")
        Resf.MM<-paste0(c("Km", "Vm"),"= ", c(Km, Vm),collapse="; ")
        Res.MM=NA
        Res.MM= as.data.table(list(
          AIC=AIC,
          LogLik=logLik(model)[[1]],
          SE=SE,
          coefs =Resf.MM))
        output <- list(Km= unname(Kms), Vm=unname(Vms), R2 = R2, SE= SE, AIC= AIC,
                       fitted_parameters=fitted_parameters, transformed_data = tdata, ResAPA = Res.MM)
        names(output) <- c("Kms", "Vms", "R2s", "fitted_parameters", "transformed_data", "ResAPA")
        return(output)
      }
      
      ecb.MM <- function (data, unit_S = "mM", unit_v = "au", plot = TRUE) 
      {
        data <- as.data.frame(data[complete.cases(data), ])
        colnames(data)[1] <- "S"
        colnames(data)[2:length(colnames(data))] <- LETTERS[1:length(colnames(data)) - 
                                                              1]
        data <- data[complete.cases(data), ]
        if (ncol(data) > 2) {
          tdata <- apply(data[, -1], MARGIN = 2, function(x) x/data[, 1])
          mean_v <- apply(data[, -1], MARGIN = 1, mean)
          mean_v_S <- apply(tdata, MARGIN = 1, mean)
        }
        else {
          tdata <- data[, 2]/data[, 1]
          mean_v <- data[, 2]
          mean_v_S <- tdata
        }
        tdata <- as.data.frame(tdata)
        names(tdata)[1] <- "v_S"
        tdata <- cbind(data, tdata)
        names(tdata)[2] <- "v"
        nc <- ncol(data) + 1
        line.intersect <- function(v, v_S) {
          Kms <- Vms <- c()
          for (i in 1:(length(v) - 1)) {
            for (j in (i + 1):length(v)) {
              Km <- (v[j] - v[i])/(v_S[i] - v_S[j])
              Kms <- c(Kms, Km)
              Vm <- (v_S[i] * (v[j] - v[i])/(v_S[i] - v_S[j])) + 
                v[j]
              Vms <- c(Vms, Vm)
            }
          }
          Km <- median(Kms, na.rm = TRUE)
          Vm <- median(Vms, na.rm = TRUE)
          return(c(Km, Vm))
        }
        Kms <- Vms <- c()
        c <- 0
        for (i in 2:(nc - 1)) {
          p <- line.intersect(tdata[, i], tdata[, nc + c])
          Kms <- c(p[1], Kms)
          Vms <- c(p[2], Vms)
          c <- c + 1
        }
        Km <- paste("Km: ", round(mean(Kms), 3), " ± ", round(sd(Kms), 3), " ", unit_S)
        Vm <- paste("Vm: ", round(mean(Vms), 3), " ± ", round(sd(Vms), 3), " ", unit_v)
        fitted_parameters <- c(round(mean(Kms, na.rm = TRUE), 3), 
                               round(mean(Vms, na.rm = TRUE), 3))
        names(fitted_parameters) <- c("Km", "Vm")
        if (plot) {
          plot(0, 0, ty = "n", xlim = c(-tdata$S[nrow(tdata)], tdata$S[nrow(tdata)]),
               ylim = c(0, 3 * max(tdata$v, na.rm = TRUE)),
               xlab = paste("Km (", unit_S, ")", sep = ""),
               ylab = paste("Vm (", unit_v, ")", sep = ""))
          for (i in 1:nrow(tdata)) {
            points(c(0, -tdata$S[i]), c(mean_v[i], 0), pch = 20)
            abline(mean_v[i], mean_v_S[i])
          }
          abline(v = 0, lty = 2)
          abline(h = 0, lty = 2)
        }
        output <- list(fitted_parameters, Km, Vm)
        names(output) <- c("fitted_parameters", "Km", "Vm")
        return(output)
      }
        
      
      DRC.SSasymp.MM <- function (fixed = c(NA, NA, NA), names = c("Asym", "R0", "lrc")) 
      {
        numParm <- 3
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          Asym <- parmMat[, 1]
          R0 <- parmMat[, 2]
          lrc <- parmMat[, 3]
          Asym + (R0 - Asym) * exp(-exp(lrc) * x)
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          dataS <- sortedXyData(x, y)
          Asym <- NLSstRtAsymptote(dataS)
          dataR <- dataS[1:2, ]
          R0 <- coef(lm(y ~ x, data = dataR))[1]
          pseudoY <- log((y - Asym)/(R0 - Asym))
          coefs <- coef(lm(pseudoY ~ x - 1))
          b <- -coefs[1]
          lrc <- log(b)
          return(c(Asym, R0, lrc)[notFixed])
        }
        pnames <- names[notFixed]
        deriv1 <- function(x, parms) {
          parmMat <- matrix(parmVec, nrow(parms), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parms
          meanfun <- function(x, a, b, c) {
            a + (b - a) * exp(-exp(c) * x)
          }
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          c <- as.numeric(parmMat[, 3])
          d1.1 <- meanfun(x, a, b, c)
          d1.2 <- meanfun(x, (a + 1e-06), b, c)
          d1 <- (d1.2 - d1.1)/1e-06
          d2.1 <- meanfun(x, a, b, c)
          d2.2 <- meanfun(x, a, (b + 1e-06), c)
          d2 <- (d2.2 - d2.1)/1e-06
          d3.1 <- meanfun(x, a, b, c)
          d3.2 <- meanfun(x, a, b, (c + 1e-06))
          d3 <- (d3.2 - d3.1)/1e-06
          cbind(d1, d2, d3)[notFixed]
        }
        derivx <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          meanfun <- function(x, a, b, c) {
            a + (b - a) * exp(-exp(c) * x)
          }
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          c <- as.numeric(parmMat[, 3])
          d1.1 <- meanfun(x, a, b, c)
          d1.2 <- meanfun((x + 1e-06), a, b, c)
          d1 <- (d1.2 - d1.1)/1e-06
          d1
        }
        text <- "Asymptotic regression model"
        returnList <- list(fct = fct, ssfct = ssfct, names = pnames, name="DRC.SSasymp.MM",
                           text = text, noParm = sum(is.na(fixed)), deriv1 = deriv1, 
                           derivx = derivx)
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.asymReg.MM <- function (fixed = c(NA, NA, NA), names = c("init", "m", "plateau")) 
      {
        numParm <- 3
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          init <- parmMat[, 1]
          m <- parmMat[, 2]
          plateau <- parmMat[, 3]
          plateau - (plateau - init) * exp(-m * x)
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          dataS <- sortedXyData(x, y)
          plateau <- NLSstRtAsymptote(dataS)
          dataR <- dataS[1:2, ]
          init <- coef(lm(y ~ x, data = dataR))[1]
          pseudoY <- log((y - plateau)/(init - plateau))
          coefs <- coef(lm(pseudoY ~ x - 1))
          m <- -coefs[1]
          return(c(init, m, plateau)[notFixed])
        }
        pnames <- names[notFixed]
        deriv1 <- function(x, parms) {
          parmMat <- matrix(parmVec, nrow(parms), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parms
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          c <- as.numeric(parmMat[, 3])
          d1.1 <- asymReg.fun(x, a, b, c)
          d1.2 <- asymReg.fun(x, (a + 1e-06), b, c)
          d1 <- (d1.2 - d1.1)/1e-06
          d2.1 <- asymReg.fun(x, a, b, c)
          d2.2 <- asymReg.fun(x, a, (b + 1e-06), c)
          d2 <- (d2.2 - d2.1)/1e-06
          d3.1 <- asymReg.fun(x, a, b, c)
          d3.2 <- asymReg.fun(x, a, b, (c + 1e-06))
          d3 <- (d3.2 - d3.1)/1e-06
          cbind(d1, d2, d3)[notFixed]
        }
        deriv2 <- NULL
        derivx <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          c <- as.numeric(parmMat[, 3])
          d1.1 <- asymReg.fun(x, a, b, c)
          d1.2 <- asymReg.fun((x + 1e-06), a, b, c)
          d1 <- (d1.2 - d1.1)/1e-06
          d1
        }
        edfct <- NULL
        text <- "Asymptotic Regression Model"
        returnList <- list(fct = fct, ssfct = ssfct, names = pnames, 
                           deriv1 = deriv1, deriv2= deriv2, derivx = derivx, edfct = edfct,
                           name = "DRC.asymReg.MM", text = text, 
                           noParm = sum(is.na(fixed)), fixed = fixed)
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.linear.MM <- function (fixed = c(NA, NA), names = c("a", "b")) 
      {
        numParm <- 2
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          a <- parmMat[, 1]
          b <- parmMat[, 2]
          linear.fun(x, a, b)
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          pseudoY <- y
          pseudoX <- x
          coefs <- coef(lm(pseudoY ~ pseudoX))
          a <- coefs[1]
          b <- coefs[2]
          return(c(a, b)[notFixed])
        }
        pnames <- names[notFixed]
        deriv1 <- function(x, parm) {
          d1 <- rep(1, length(x))
          d2 <- x
          cbind(d1, d2)
        }
        text <- "Straight line"
        returnList <- list(fct = fct, ssfct = ssfct, deriv1 = deriv1, name="DRC.linear.MM",
                           names = pnames, text = text, noParm = sum(is.na(fixed)))
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.poly2.MM <- function (fixed = c(NA, NA, NA), names = c("a", "b", "c")) 
      {
        numParm <- 3
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          a <- parmMat[, 1]
          b <- parmMat[, 2]
          c <- parmMat[, 3]
          a + b * x + c * (x^2)
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          pseudoY <- y
          pseudoX <- x
          coefs <- coef(lm(pseudoY ~ pseudoX + I(pseudoX^2)))
          a <- coefs[1]
          b <- coefs[2]
          c <- coefs[3]
          return(c(a, b, c)[notFixed])
        }
        pnames <- names[notFixed]
        deriv1 <- function(x, parm) {
          d1 <- rep(1, length(x))
          d2 <- x
          d3 <- x^2
          cbind(d1, d2, d3)
        }
        text <- "Second Order Polynomial"
        returnList <- list(fct = fct, ssfct = ssfct, names = pnames, name="DRC.poly2.MM",
                           text = text, noParm = sum(is.na(fixed)), deriv1 = deriv1)
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.expoDecay.MM <- function (fixed = c(NA, NA), names = c("init", "k")) 
      {
        numParm <- 2
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          init <- parmMat[, 1]
          k <- parmMat[, 2]
          init * exp(-k * x)
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          pseudoY <- log(y + 1e-06)
          coefs <- coef(lm(pseudoY ~ x))
          init <- exp(coefs[1])
          m <- -coefs[2]
          return(c(init, m)[notFixed])
        }
        pnames <- names[notFixed]
        deriv1 <- function(x, parms) {
          parmMat <- matrix(parmVec, nrow(parms), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parms
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          d1.1 <- expoDecay.fun(x, a, b)
          d1.2 <- expoDecay.fun(x, (a + 1e-06), b)
          d1 <- (d1.2 - d1.1)/1e-06
          d2.1 <- expoDecay.fun(x, a, b)
          d2.2 <- expoDecay.fun(x, a, (b + 1e-06))
          d2 <- (d2.2 - d2.1)/1e-06
          cbind(d1, d2)[notFixed]
        }
        derivx <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          d1.1 <- expoGrowth.fun(x, a, b)
          d1.2 <- expoGrowth.fun((x + 1e-06), a, b)
          d1 <- (d1.2 - d1.1)/1e-06
          d1
        }
        DT <- function(parms, respl, reference = "control", type = "relative") {
          dt.fun <- function(a, b, g) {
            log(a/g)/b
          }
          a <- as.numeric(parms[1])
          b <- as.numeric(parms[2])
          if (type == "relative") {
            g <- respl/100
            EDp <- dt.fun(a, b, g)
            d1.1 <- dt.fun(a, b, g)
            d1.2 <- dt.fun(a + 1e-05, b, g)
            d1 <- (d1.2 - d1.1)/1e-05
            d2.1 <- dt.fun(a, b, g)
            d2.2 <- dt.fun(a, b + 1e-05, g)
            d2 <- (d2.2 - d2.1)/1e-05
            EDder <- c(d1, d2)
          }
          else {
            g <- respl
            EDp <- dt.fun(a, b, g)
            d1.1 <- dt.fun(a, b, g)
            d1.2 <- dt.fun(a + 1e-05, b, g)
            d1 <- (d1.2 - d1.1)/1e-05
            d2.1 <- dt.fun(a, b, g)
            d2.2 <- dt.fun(a, b + 1e-05, g)
            d2 <- (d2.2 - d2.1)/1e-05
            EDder <- c(d1, d2)
          }
          return(list(EDp, EDder))
        }
        text <- "Exponential Decay Model"
        returnList <- list(fct = fct, ssfct = ssfct, deriv1 = deriv1, name="DRC.expoDecay.MM",
                           derivx = derivx, names = pnames, text = text, noParm = sum(is.na(fixed)), 
                           edfct = DT)
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.expoGrowth.MM <- function (fixed = c(NA, NA), names = c("init", "k")) 
      {
        numParm <- 2
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          init <- parmMat[, 1]
          k <- parmMat[, 2]
          init * exp(k * x)
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          pseudoY <- log(y + 1e-06)
          coefs <- coef(lm(pseudoY ~ x))
          init <- exp(coefs[1])
          m <- coefs[2]
          return(c(init, m)[notFixed])
        }
        pnames <- names[notFixed]
        deriv1 <- function(x, parms) {
          parmMat <- matrix(parmVec, nrow(parms), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parms
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          d1.1 <- expoGrowth.fun(x, a, b)
          d1.2 <- expoGrowth.fun(x, (a + 1e-06), b)
          d1 <- (d1.2 - d1.1)/1e-06
          d2.1 <- expoGrowth.fun(x, a, b)
          d2.2 <- expoGrowth.fun(x, a, (b + 1e-06))
          d2 <- (d2.2 - d2.1)/1e-06
          cbind(d1, d2)[notFixed]
        }
        derivx <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          d1.1 <- expoGrowth.fun(x, a, b)
          d1.2 <- expoGrowth.fun((x + 1e-06), a, b)
          d1 <- (d1.2 - d1.1)/1e-06
          d1
        }
        text <- "Exponential Growth Model"
        returnList <- list(fct = fct, ssfct = ssfct, deriv1 = deriv1, name= "DRC.expoGrowth.MM",
                           derivx = derivx, names = pnames, text = text, noParm = sum(is.na(fixed)))
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.powerCurve.MM <- function (fixed = c(NA, NA), names = c("a", "b")) 
      {
        numParm <- 2
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          a <- parmMat[, 1]
          b <- parmMat[, 2]
          a * x^(b)
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          pseudoY <- log(y + 1e-05)
          pseudoX <- log(x)
          coefs <- coef(lm(pseudoY ~ pseudoX))
          a <- exp(coefs[1])
          b <- coefs[2]
          return(c(a, b)[notFixed])
        }
        pnames <- names[notFixed]
        deriv1 <- function(x, parms) {
          parmMat <- matrix(parmVec, nrow(parms), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parms
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          d1.1 <- expoDecay.fun(x, a, b)
          d1.2 <- expoDecay.fun(x, (a + 1e-06), b)
          d1 <- (d1.2 - d1.1)/1e-06
          d2.1 <- expoDecay.fun(x, a, b)
          d2.2 <- expoDecay.fun(x, a, (b + 1e-06))
          d2 <- (d2.2 - d2.1)/1e-06
          cbind(d1, d2)[notFixed]
        }
        derivx <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          d1.1 <- expoGrowth.fun(x, a, b)
          d1.2 <- expoGrowth.fun((x + 1e-06), a, b)
          d1 <- (d1.2 - d1.1)/1e-06
          d1
        }
        text <- "Power curve (Freundlich equation)"
        returnList <- list(fct = fct, ssfct = ssfct, names = pnames, 
                           text = text, noParm = sum(is.na(fixed)), deriv1 = deriv1, name="DRC.powerCurve.MM",
                           derivx = derivx)
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.logCurve.MM <- function (fixed = c(NA, NA), names = c("a", "b")) 
      {
        numParm <- 2
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          a <- parmMat[, 1]
          b <- parmMat[, 2]
          a + b * log(x)
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          pseudoY <- y
          pseudoX <- log(x)
          coefs <- coef(lm(pseudoY ~ pseudoX))
          a <- coefs[1]
          b <- coefs[2]
          return(c(a, b)[notFixed])
        }
        pnames <- names[notFixed]
        deriv1 <- function(x, parms) {
          parmMat <- matrix(parmVec, nrow(parms), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parms
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          d1.1 <- logCurve.fun(x, a, b)
          d1.2 <- logCurve.fun(x, (a + 1e-06), b)
          d1 <- (d1.2 - d1.1)/1e-06
          d2.1 <- logCurve.fun(x, a, b)
          d2.2 <- logCurve.fun(x, a, (b + 1e-06))
          d2 <- (d2.2 - d2.1)/1e-06
          cbind(d1, d2)[notFixed]
        }
        derivx <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          d1.1 <- logCurve.fun(x, a, b)
          d1.2 <- logCurve.fun((x + 1e-06), a, b)
          d1 <- (d1.2 - d1.1)/1e-06
          d1
        }
        text <- "Linear regression on log-transformed x"
        returnList <- list(fct = fct, ssfct = ssfct, names = pnames, name="DRC.logCurve.MM",
                           deriv1 = deriv1, derivx = derivx, text = text, noParm = sum(is.na(fixed)))
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.negExp.MM <- function (fixed = c(NA, NA), names = c("a", "c")) 
      {
        numParm <- 2
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          a <- parmMat[, 1]
          c <- parmMat[, 2]
          a * (1 - exp(-c * x))
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          plateau <- max(y) * 1.05
          pseudoY <- log(1 - (y/plateau))
          coefs <- coef(lm(pseudoY ~ x - 1))
          a <- plateau
          c <- -coefs[1]
          return(c(a, c)[notFixed])
        }
        pnames <- names[notFixed]
        deriv1 <- function(x, parms) {
          parmMat <- matrix(parmVec, nrow(parms), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parms
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          d1.1 <- negExp.fun(x, a, b)
          d1.2 <- negExp.fun(x, (a + 1e-06), b)
          d1 <- (d1.2 - d1.1)/1e-06
          d2.1 <- negExp.fun(x, a, b)
          d2.2 <- negExp.fun(x, a, (b + 1e-06))
          d2 <- (d2.2 - d2.1)/1e-06
          cbind(d1, d2)[notFixed]
        }
        derivx <- function(x, parm) {
          print("qui")
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          a <- as.numeric(parmMat[, 1])
          b <- as.numeric(parmMat[, 2])
          d1.1 <- negExp.fun(x, a, b)
          d1.2 <- negExp.fun((x + 1e-06), a, b)
          d1 <- (d1.2 - d1.1)/1e-06
          d1
        }
        text <- "Negative exponential function"
        returnList <- list(fct = fct, ssfct = ssfct, names = pnames, name="DRC.negExp.MM",
                           text = text, noParm = sum(is.na(fixed)), deriv1 = deriv1, 
                           derivx = derivx)
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      E.2.MM <- function (fixed = c(NA, NA), names = c("b", "e")) 
      {
        numParm <- 2
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          E2.fun(x, parm[, 1], parm[, 2])
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          pseudoY <- log(-log(1.01 - y))
          coefs <- coef(lm(pseudoY ~ x))
          k <- coefs[1]
          b <- coefs[2]
          e <- -k/b
          value <- c(b, e)
          return(value[notFixed])
        }
        pnames <- names[notFixed]
        text <- "Modified Gompertz equation (2 parameters)"
        returnList <- list(fct = fct, ssfct = ssfct, names = pnames, name="E.2.MM",
                           text = text, noParm = sum(is.na(fixed)))
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      E.3.MM <- function (fixed = c(NA, NA, NA), names = c("b", "d", "e")) 
      {
        numParm <- 3
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          E3.fun(x, parm[, 1], parm[, 2], parm[, 3])
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          d <- max(y) * 1.05
          pseudoY <- log(-log((d - y)/d))
          coefs <- coef(lm(pseudoY ~ x))
          k <- coefs[1]
          b <- coefs[2]
          e <- -k/b
          value <- c(b, d, e)
          return(value[notFixed])
        }
        pnames <- names[notFixed]
        text <- "Modified Gompertz equation (3 parameters)"
        returnList <- list(fct = fct, ssfct = ssfct, names = pnames, name="E.3.MM",
                           text = text, noParm = sum(is.na(fixed)))
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      E.4.MM <- function (fixed = c(NA, NA, NA, NA), names = c("b", "c", "d", "e")) 
      {
        numParm <- 4
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          E4.fun(x, parm[, 1], parm[, 2], parm[, 3], parm[, 4])
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          d <- max(y) * 1.05
          c <- min(y) * 0.95
          pseudoY <- log(-log((d - y)/(d - c)))
          coefs <- coef(lm(pseudoY ~ x))
          k <- coefs[1]
          b <- coefs[2]
          e <- -k/b
          value <- c(b, c, d, e)
          return(value[notFixed])
        }
        pnames <- names[notFixed]
        text <- "Modified Gompertz equation (4 parameters)"
        returnList <- list(fct = fct, ssfct = ssfct, names = pnames, name="E.4.MM",
                           text = text, noParm = sum(is.na(fixed)))
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.bragg.3.MM <- function () 
      {
        fct <- function(x, parm) {
          bragg.3.fun(x, parm[, 1], parm[, 2], parm[, 3])
        }
        ssfct <- function(data) {
          x <- data[, 1]
          y <- data[, 2]
          d <- max(y)
          e <- x[which.max(y)]
          pseudoY <- log((y + 1e-04)/d)
          pseudoX <- (x - e)^2
          coefs <- coef(lm(pseudoY ~ pseudoX - 1))
          b <- -coefs[1]
          start <- c(b, d, e)
          return(start)
        }
        names <- c("b", "d", "e")
        text <- "Bragg equation with three parameters"
        returnList <- list(fct = fct, ssfct = ssfct, names = names, name="DRC.bragg.3.MM",
                           text = text)
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.bragg.4.MM <- function () 
      {
        fct <- function(x, parm) {
          bragg.4.fun(x, parm[, 1], parm[, 2], parm[, 3], parm[, 
                                                               4])
        }
        ssfct <- function(data) {
          x <- data[, 1]
          y <- data[, 2]
          d <- max(y)
          c <- min(y) * 0.95
          e <- x[which.max(y)]
          pseudoY <- log(((y + 1e-04) - c)/d)
          pseudoX <- (x - e)^2
          coefs <- coef(lm(pseudoY ~ pseudoX - 1))
          b <- -coefs[1]
          start <- c(b, c, d, e)
          return(start)
        }
        names <- c("b", "c", "d", "e")
        text <- "Bragg equation with four parameters"
        returnList <- list(fct = fct, ssfct = ssfct, names = names, name="DRC.bragg.4.MM",
                           text = text)
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.lorentz.3.MM <- function () 
      {
        fct <- function(x, parm) {
          lorentz.3.fun(x, parm[, 1], parm[, 2], parm[, 3])
        }
        ssfct <- function(data) {
          x <- data[, 1]
          y <- data[, 2]
          d <- max(y)
          e <- x[which.max(y)]
          pseudoY <- (d - y)/y
          pseudoX <- (x - e)^2
          coefs <- coef(lm(pseudoY ~ pseudoX - 1))
          b <- coefs[1]
          start <- c(b, d, e)
          return(start)
        }
        names <- c("b", "d", "e")
        text <- "Lorentz equation with three parameters"
        returnList <- list(fct = fct, ssfct = ssfct, names = names, name="DRC.lorentz.3.MM",
                           text = text)
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.lorentz.4.MM <- function () 
      {
        fct <- function(x, parm) {
          lorentz.4.fun(x, parm[, 1], parm[, 2], parm[, 3], parm[, 
                                                                 4])
        }
        ssfct <- function(data) {
          x <- data[, 1]
          y <- data[, 2]
          d <- max(y)
          c <- min(y) * 0.95
          e <- x[which.max(y)]
          pseudoY <- (d - y)/(y - c)
          pseudoX <- (x - e)^2
          coefs <- coef(lm(pseudoY ~ pseudoX - 1))
          b <- coefs[1]
          start <- c(b, c, d, e)
          return(start)
        }
        names <- c("b", "c", "d", "e")
        text <- "Lorentz equation with four parameters"
        returnList <- list(fct = fct, ssfct = ssfct, names = names, 
                           name="DRC.lorentz.4.MM", text = text)
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.beta.MM <- function () 
      {
        fct <- function(x, parm) {
          beta.fun(x, parm[, 1], parm[, 2], parm[, 3], parm[, 4], 
                   parm[, 5])
        }
        ssfct <- function(data) {
          x <- data[, 1]
          y <- data[, 2]
          d <- max(y)
          Xo <- x[which.max(y)]
          firstidx <- min(which(y != 0))
          Xb <- ifelse(firstidx == 1, x[1], (x[firstidx] + x[(firstidx - 
                                                                1)])/2)
          secidx <- max(which(y != 0))
          Xc <- ifelse(secidx == length(y), x[length(x)], (x[secidx] + 
                                                             x[(secidx + 1)])/2)
          c(1, d, Xb, Xo, Xc)
        }
        names <- c("b", "d", "Xb", "Xo", "Xc")
        text <- "Beta function"
        returnList <- list(fct = fct, ssfct = ssfct, 
                           names = names, name="DRC.beta.MM", text = text)
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.cousens85.MM <- function (fixed = c(NA, NA, NA), names = c("YWF", "i", "a")) 
      {
        numParm <- 3
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          YWF <- parmMat[, 1]
          i <- parmMat[, 2]
          a <- parmMat[, 3]
          YWF * (1 - (i * x)/(100 * (1 + i * x/a)))
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          YWF <- max(y) + 1e-05
          YL <- (1 - y/YWF) * 100
          pseudoY <- 1/(YL + 1e-06)
          pseudoX <- 1/(x + 1e-05)
          coefs <- coef(lm(pseudoY ~ pseudoX))
          a <- 1/coefs[1]
          i <- 1/coefs[2]
          return(c(YWF, i, a)[notFixed])
        }
        pnames <- names[notFixed]
        text <- "Yield-Weed Density function (Cousens, 1985)"
        returnList <- list(fct = fct, ssfct = ssfct, names = pnames, name="DRC.cousens85.MM", 
                           text = text, noParm = sum(is.na(fixed)))
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      DRC.YL.MM <- function (fixed = c(NA, NA), names = c("i", "A")) 
      {
        numParm <- 2
        if (!is.character(names) | !(length(names) == numParm)) {
          stop("Not correct 'names' argument")
        }
        if (!(length(fixed) == numParm)) {
          stop("Not correct 'fixed' argument")
        }
        notFixed <- is.na(fixed)
        parmVec <- rep(0, numParm)
        parmVec[!notFixed] <- fixed[!notFixed]
        fct <- function(x, parm) {
          parmMat <- matrix(parmVec, nrow(parm), numParm, byrow = TRUE)
          parmMat[, notFixed] <- parm
          i <- parmMat[, 1]
          A <- parmMat[, 2]
          YL.fun(x, i, A)
        }
        ssfct <- function(dataf) {
          x <- dataf[, 1]
          y <- dataf[, 2]
          pseudoY <- 1/y[x > 0]
          pseudoX <- 1/x[x > 0]
          coefs <- coef(lm(pseudoY ~ pseudoX))
          A <- 1/coefs[1]
          i <- 1/coefs[2]
          return(c(i, A)[notFixed])
        }
        pnames <- names[notFixed]
        text <- "Yield-Loss function (Cousens, 1985)"
        returnList <- list(fct = fct, ssfct = ssfct, names = pnames, name="DRC.YL.MM",
                           text = text, noParm = sum(is.na(fixed)))
        class(returnList) <- "drcMean"
        invisible(returnList)
      }
      
      
      UCRS.4a.MM <- function (names = c("b", "d", "e", "f"), ...) 
      {
        if (!is.character(names) | !(length(names) == 4)) {
          stop("Not correct 'names' argument")
        }
        returnA =ucedergreen(names = c(names[1], "c", names[2:4]),
                             fixed = c(NA, 0, NA, NA, NA), alpha = 1, ...)
        returnA$name="UCRS.4a.MM"
        returnA
      }
      
      UCRS.4b.MM <- function (names = c("b", "d", "e", "f"), ...) 
      {
        if (!is.character(names) | !(length(names) == 4)) {
          stop("Not correct 'names' argument")
        }
        returnA =ucedergreen(names = c(names[1], "c", names[2:4]), 
                             fixed = c(NA, 0, NA, NA, NA), alpha = 0.5, ...)
        returnA$name="UCRS.4b.MM"
        returnA
      }
      
      UCRS.4c.MM <- function (names = c("b", "d", "e", "f"), ...) 
      {
        if (!is.character(names) | !(length(names) == 4)) {
          stop("Not correct 'names' argument")
        }
        returnA=ucedergreen(names = c(names[1], "c", names[2:4]), 
                            fixed = c(NA, 0, NA, NA, NA), alpha = 0.25, ...)
        returnA$name="UCRS.4c.MM"
        returnA
      }
      
      UCRS.5a.MM <- function (names = c("b", "c", "d", "e", "f"), ...) 
      {
        if (!is.character(names) | !(length(names) == 5)) {
          stop("Not correct 'names' argument")
        }
        returnA =ucedergreen(names = names, fixed = c(NA, NA, NA, NA, 
                                                      NA), alpha = 1, ...)
        returnA$name="UCRS.5a.MM"
        returnA
      }
      
      UCRS.5b.MM <- function (names = c("b", "c", "d", "e", "f"), ...) 
      {
        if (!is.character(names) | !(length(names) == 5)) {
          stop("Not correct 'names' argument")
        }
        returnA=ucedergreen(names = names, fixed = c(NA, NA, NA, NA, 
                                                     NA), alpha = 0.5, ...)
        returnA$name="UCRS.5b.MM"
        returnA
      }
      
      UCRS.5c.MM <- function (names = c("b", "c", "d", "e", "f"), ...) 
      {
        if (!is.character(names) | !(length(names) == 5)) {
          stop("Not correct 'names' argument")
        }
        returnA=ucedergreen(names = names, fixed = c(NA, NA, NA, NA, 
                                                     NA), alpha = 0.25, ...)
        returnA$name="UCRS.5c.MM"
        returnA
      }
      # Secondly, graphic generators
      
      
      Grp.bell <-function() {
        set.seed(1234)
        x <- 1:20
        y <- bell(x, 8, -0.0314, 0.000317, 13) + rnorm(length(x), 0, 0.5)
        datg1 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSbell(X, ymax, a, b, xc), data = datg1)
        GrphModels(DTg = datg1, modP = fit, ElTit = "Bell-shaped curve",NmModP = "", AjAx=T)
      }
      
      Grp.Smoh <-function() {
        set.seed(1234)
        x <- seq(3, 30)
        y <- moh(x, 35, 3, 0.83) + rnorm(length(x), 0, 0.5)
        datg2 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSmoh(X, asym, xmin, k), data = datg2)
        GrphModels(DTg = datg2, modP = fit, ElTit = "Modified Hyperbola",NmModP = "", AjAx=T)
      }
      
      Grp.blin <-function() {
        x <- 1:5
        y <- c(1.1, 1.9, 3.1, 2, 0.9)
        datg3 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSblin(X, a, b, xs, c), data = datg3)
        GrphModels(DTg = datg3, modP = fit, ElTit = "Bilinear",NmModP = "", AjAx=T)
      }
      
      Grp.Hill1 <- function() {
        set.seed(1234)
        x <- 1:20
        y <- hill1(x, 10) + rnorm(20, sd = 0.03)
        datg4 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SShill1(X, Ka), data = datg4)
        GrphModels(DTg = datg4, modP = fit, ElTit = "Hill function 1-param",NmModP = "", AjAx=T)
      }
      
      Grp.Hill2 <- function() {
        set.seed(1234)
        x <- 1:100
        y <- hill2(x, 10, 1) + rnorm(100, sd = 0.03)
        datg5 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SShill2(X, Ka, n), data = datg5)
        GrphModels(DTg = datg5, modP = fit, ElTit = "Hill function 2-param",NmModP = "", AjAx=T)
      }
      
      Grp.Hill3 <- function() {
        set.seed(1234)
        x <- 1:80
        y <- hill3(x, 10, 1.5, 5) + rnorm(20, sd = 0.03)
        datg6 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SShill3(X, Ka, n, a), data = datg6)
        GrphModels(DTg = datg6, modP = fit, ElTit = "Hill function 3-param",NmModP = "", AjAx=T)
      }
      
      Grp.Trlin <-function() {
        set.seed(1234)
        x <- 1:30
        y <- trlin(x, 0.5, 2, 10, 0.1, 20, 1.75) + rnorm(30, 0, 0.5)
        datg7 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SStrlin(X, a, b, xs1, c, xs2, d), data = datg7)
        GrphModels(DTg = datg7, modP = fit, ElTit = "Trilinear",NmModP = "", AjAx=T)
      }
      
      Grp.quadp3 <- function() {
        set.seed(123)
        x <- 1:30
        y <- quadp3(x, 5, 1.7, -0.04) + rnorm(30, 0, 0.6)
        datg8 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSquadp3(X, a, b, c), data = datg8)
        GrphModels(DTg = datg8, modP = fit, ElTit = "Quadratic-Plateau",NmModP = "", AjAx=T)
      }
      
      Grp.quadp3xs <- function() {
        set.seed(123)
        x <- 1:30
        y <- quadp3xs(x, 5, 1.7, 20) + rnorm(30, 0, 0.6)
        datg9 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSquadp3xs(X, a, b, xs), data = datg9)
        GrphModels(DTg = datg9, modP = fit, ElTit = "Quadratic-Plateau with break-point",NmModP = "", AjAx=T)
      }
      
      Grp.Ratio <- function() {
        set.seed(1234)
        x <- 1:100
        y <- ratio(x, 1, 0.5, 1, 1.5) + rnorm(length(x), 0, 0.025)
        datg10 <- data.frame(X = x, Y = y)
        fit <- nlsLM(Y ~ SSratio(X, a, b, c, d), data = datg10)
        GrphModels(DTg = datg10, modP = fit, ElTit = "Rational",NmModP = "", AjAx=T)
      }
      
      Grp.Beta5 <-function()  {
        x <- c(0,  5, 15, 25, 30, 35, 38, 39, 40, 42)
        y <- c(0.01, 0.15, 0.45, 1.00, 1.25, 1.55, 1.50, 1.30, 0.80, 0.00)
        datg11 <- data.frame(X = x, Y = y)
        fit <- nlsLM(Y ~ SSbeta5(X, mu, tb, a, tc, b), data =datg11)
        GrphModels(DTg = datg11, modP = fit, ElTit = "Beta 5-params (crop development)",NmModP = "", AjAx=T)
      }
      
      Grp.bg4rp <-function() {
        set.seed(1234)
        x <- 1:100
        y <- bg4rp(x, 20, log(70), log(30), log(20)) + rnorm(100, 0, 1)
        datg12 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSbg4rp(X, w.max, lt.e, ldtm, ldtb), data = datg12)
        GrphModels(DTg = datg12, modP = fit, ElTit = "Beta 4-params Reparam",NmModP = "", AjAx=T)
      }
      
      Grp.bgf <-function() {
        x <- seq(0, 17, by = 0.25)
        y <- bgf(x, 5, 15, 7)
        datg13 <- data.frame(x = x, y = y)
        ggplot(data = datg13, aes(x = x, y = y)) + 
          geom_point() + 
          geom_line( aes(x = x, y = y),linewidth =2) +
          ggtitle("Beta 3-params") +
          theme_classic() 
      }
      
      Grp.bgf4 <-function() {
        x <- seq(0, 17, by = 0.25)
        y <- bgf4(x, 20, 15, 10, 2)
        datg14 <- data.frame(x = x, y = y)
        ggplot(data = datg14, aes(x = x, y = y)) + 
          geom_point() + 
          geom_line( aes(x = x, y = y),linewidth =2) +
          ggtitle("Beta 4-params") +
          theme_classic()
      }
      
      Grp.bgrp <-function() {
        x <- 1:30
        y <- bgrp(x, 20, log(25), log(5)) + rnorm(30, 0, 1)
        datg15 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSbgrp(X, w.max, lt.e, ldt), data = datg15)
        GrphModels(DTg = datg15, modP = fit, ElTit = "Beta 3-params Reparam",NmModP = "", AjAx=T)
      }
      
      Grp.vegan <- function() {
        ## Get species area data: sipoo.map gives the areas of islands
        data(sipoo, sipoo.map)
        S <- specnumber(sipoo)
        # x <- c( 4, 4, 5,  2, 6, 6, 4, 10, 6, 8, 13, 10, 12, 14, 16, 17, 30, 34 )
        plot(S ~ area, sipoo.map,  xlab = "Island Area (ha)",
             ylab = "Number of Species", ylim = c(1, max(S)))
        ## The Arrhenius model
        marr <- nls(S ~ SSarrhenius(area, k, z), data=sipoo.map)
        #marr
        ## confidence limits from profile likelihood
        #confint(marr)
        ## draw a line
        xtmp <- with(sipoo.map, seq(min(area), max(area), len=51))
        lines(xtmp, predict(marr, newdata=data.frame(area = xtmp)), lwd=2)
        ## The normal way is to use linear regression on log-log data,
        ## but this will be different from the previous:
        mloglog <- lm(log(S) ~ log(area), data=sipoo.map)
        #mloglog
        lines(xtmp, exp(predict(mloglog, newdata=data.frame(area=xtmp))),
              lty=2)
        ## Gleason: log-linear
        mgle <- nls(S ~ SSgleason(area, k, slope), sipoo.map)
        lines(xtmp, predict(mgle, newdata=data.frame(area=xtmp)),
              lwd=2, col=2)
        ## Gitay: quadratic of log-linear
        mgit <- nls(S ~ SSgitay(area, k, slope), sipoo.map)
        lines(xtmp, predict(mgit, newdata=data.frame(area=xtmp)),
              lwd=2, col = 3)
        ## Lomolino: using original names of the parameters (Lomolino 2000):
        mlom <- nls(S ~ SSlomolino(area, Smax, A50, Hill), sipoo.map)
        #mlom
        lines(xtmp, predict(mlom, newdata=data.frame(area=xtmp)),
              lwd=2, col = 4)
        ## One canned model of standard R:
        mmic <- nls(S ~ SSmicmen(area, slope, Asym), sipoo.map)
        lines(xtmp, predict(mmic, newdata = data.frame(area=xtmp)),
              lwd =2, col = 5)
        legend("bottomright", c("Arrhenius", "log-log linear", "Gleason", "Gitay", 
                                "Lomolino", "Michaelis-Menten"), col=c(1,1,2,3,4,5), lwd=c(2,1,2,2,2,2), 
               lty=c(1,2,1,1,1,1))
        #if (exists("sipoo")) rm("sipoo");
        #if (exists("sipoo.map")) rm("sipoo.map")
      }
      
      Grp.logis5 <- function() {
        x <- seq(0, 2000)
        y <- logis5(x, 30, 10, 800, 5, 2)
        datg17 <- data.frame(x = x, y = y)
        ggplot(data = datg17, aes(x = x, y = y)) + 
          geom_point() + 
          geom_line(aes(x = x, y = y),linewidth =2) +
          ggtitle("Logistic 5-params") +
          theme_classic()
      }
      
      Grp.linp <-function() {
        set.seed(123)
        x <- 1:30
        y <- linp(x, 0, 1, 20) + rnorm(30, 0, 0.5)
        datg18 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSlinp(X, a, b, xs), data = datg18)
        GrphModels(DTg = datg18, modP = fit, ElTit = "Linear-plateau",NmModP = "", AjAx=T)
      }
      
      Grp.explin <-function() {
        set.seed(12345)
        x <- seq(1,100, by = 5)
        y <- explin(x, 20, 0.14, 30) + rnorm(length(x), 0, 5)
        y <- abs(y)
        datg19 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSexplin(X, cm, rm, tb), data = datg19)
        GrphModels(DTg = datg19, modP = fit, ElTit = "Exponential-Linear",NmModP = "", AjAx=T)
      }
      
      Grp.expf <- function() {
        set.seed(1234)
        x <- 1:15
        y <- expf(x, 10, -0.3) + rnorm(15, 0, 0.2)
        datg20 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSexpf(X, a, c), data = datg20)
        GrphModels(DTg = datg20, modP = fit, ElTit = "Simple Exponential",NmModP = "", AjAx=T)
      }
      
      Grp.expfp <- function() {
        set.seed(12345)
        x <- 1:30
        y <- expfp(x, 10, 0.1, 15) + rnorm(30, 0, 1.5)
        datg21 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSexpfp(X, a, c, xs), data = datg21)
        GrphModels(DTg = datg21, modP = fit, ElTit = "Exponential-plateau",NmModP = "", AjAx=T)
      }
      
      Grp.dif <- function() {
        x <- seq(0, 17, by = 0.25)
        y <- dlf(x, 2, 10, 8, 1)
        datg22 <- data.frame(x = x, y = y)
        ggplot(data = datg22, aes(x, y)) + 
          geom_point() + 
          geom_line(aes(x = x, y = y),linewidth =2) +
          ggtitle("Declining Logistic") +
          theme_classic()
      }
      
      Grp.card3 <-function() {
        set.seed(1234)
        x <- 1:50
        y <- card3(x, 13, 25, 36) + rnorm(length(x), sd = 0.05)
        datg23 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SScard3(X, tb, to, tm), data = datg23)
        GrphModels(DTg = datg23, modP = fit, ElTit = "Smooth Cardinal",NmModP = "", AjAx=T)
      }
      
      Grp.Weibull<-function() {
        Chick.6 <- subset(ChickWeight, (Chick == 6) & (Time > 0))
        fm1 <- nls(weight ~ SSweibull(Time, Asym, Drop, lrc, pwr), data = Chick.6)
        ## Data and Fit:
        plot(weight ~ Time, Chick.6, xlim = c(0, 21), main = "SSweibull() fit to Chick.6")
        ux <- par("usr")[1:2]; x <- seq(ux[1], ux[2], length.out=250)
        lines(x, do.call(SSweibull, c(list(x=x), coef(fm1))), col = "red", lwd=2)
        As <- coef(fm1)[["Asym"]]; abline(v = 0, h = c(As, As - coef(fm1)[["Drop"]]), lty = 3)
        
      }
      
      Grp.Gompetz <- function() {
        DNase.1 <- subset(DNase, Run == 1)
        fm1 <- nls(density ~ SSgompertz(log(conc), Asym, b2, b3),
                   data = DNase.1)
        plot(density ~ log(conc), DNase.1, # xlim = c(0, 21),
             main = "SSgompertz() fit to DNase.1")
        ux <- par("usr")[1:2]; x <- seq(ux[1], ux[2], length.out=250)
        lines(x, do.call(SSgompertz, c(list(x=x), coef(fm1))), col = "red", lwd=2)
        As <- coef(fm1)[["Asym"]]; abline(v = 0, h = 0, lty = 3)
        axis(2, at= exp(-coef(fm1)[["b2"]]), quote(e^{-b[2]}), las=1, pos=0)
      }
      
      Grp.MicMan <- function() {
        ## Visualize the SSmicmen()  Michaelis-Menton model parametrization
        xx <- seq(0, 5, length.out = 101)
        yy <- 5 * xx/(1+xx)
        stopifnot(all.equal(yy, SSmicmen(xx, Vm = 5, K = 1)))
        require(graphics)
        op <- par(mar = c(0, 0, 3.5, 0))
        plot(xx, yy, type = "l", lwd = 2, ylim = c(-1/4,6), xlim = c(-1, 5),
             ann = FALSE, axes = FALSE, main = "Parameters in the SSmicmen model")
        mtext(quote(list(phi[1] == "Vm", phi[2] == "K")))
        usr <- par("usr")
        arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
        arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
        text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
        text(     -0.1, usr[4], "y", adj = c(1, 1))
        abline(h = 5, lty = 3)
        arrows(-0.8, c(2.1, 2.9),
               -0.8, c(0,   5  ),  length = 0.1, angle = 25)
        text(  -0.8,     2.5, quote(phi[1]))
        segments(1, 0, 1, 2.7, lty = 2, lwd = 0.75)
        text(1, 2.7, quote(phi[2]))
        par(op)
      }
      
      Grp.MMRecipr <- function() {
        x <- seq(0.1, 1, by=.01)
        y <- MM.Recip1.Mod(x, 0,.1)
        datg27 <- data.frame(x = x, y = y)
        ggplot(data = datg27, aes(x, y)) + 
          geom_point() + 
          geom_line( aes(x = x, y = y),linewidth =2) +
          ggtitle("Reciprocal Simple") +
          theme_classic()
      }
      
      Grp.MMHyper4 <- function() {
        x <- seq(0.1, 1, by=.01)
        y <- MM.Hyper4.Mod(x, 1,10)
        datg28 <- data.frame(x = x, y = y)
        ggplot(data = datg28, aes(x, y)) + 
          geom_point() + 
          geom_line( aes(x = x, y = y),linewidth =2) +
          ggtitle("Simple Hyperbola") +
          theme_classic()
      }
      
      Grp.nrh <-function() {
        set.seed(1234)
        x <- seq(0, 2000, 100)
        y <- nrh(x, 35, 0.04, 0.83, 2) + rnorm(length(x), 0, 0.5)
        datg29 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSnrh(X, asym, phi, theta, rd), data = datg29)
        GrphModels(DTg = datg29, modP = fit, ElTit = "Non-rectangular hyperbola",NmModP = "", AjAx=T)
      }
      
      Grp.pexpf <- function() {
        set.seed(1234)
        x <- 1:30
        y <- pexpf(x, 20, 15, -0.2) + rnorm(30, 0, 1)
        datg30 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSpexpf(X, a, xs, c), data = datg30)
        GrphModels(DTg = datg30, modP = fit, ElTit = "Plateau-exponential",NmModP = "", AjAx=T)
      }
      
      Grp.plin <-function() {
        set.seed(123)
        x <- 1:30
        y <- plin(x, 10, 20, 1) + rnorm(30, 0, 0.5)
        datg31 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSplin(X, a, xs, b), data = datg31)
        GrphModels(DTg = datg31, modP = fit, ElTit = "Plateau-linear",NmModP = "", AjAx=T)
      }
      
      Grp.pquad <-function() {
        set.seed(12345)
        x <- 1:40
        y <- pquad(x, 5, 20, 1.7, -0.04) + rnorm(40, 0, 0.6)
        datg32 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSpquad(X, a, xs, b, c), data = datg32)
        GrphModels(DTg = datg32, modP = fit, ElTit = "Plateau-quadratic",NmModP = "", AjAx=T)
      }
      
      Grp.pquad3 <-function() {
        set.seed(123)
        x <- 1:30
        y <- pquad3(x, 20.5, 0.36, -0.012) + rnorm(30, 0, 0.3)
        datg33 <- data.frame(X = x, Y = y)
        fit <- nlsLM(Y ~ SSpquad3(X, a, b, c), data = datg33)
        GrphModels(DTg = datg33, modP = fit, ElTit = "Plateau-quadratic 3-params",NmModP = "", AjAx=T)
      }
      
      Grp.profd <-function() {
        set.seed(1234)
        x <- 1:10
        y <- profd(x, 0.3, 0.05, 0.5, 4) + rnorm(10, 0, 0.01)
        datg34 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSprofd(X, a, b, c, d), data = datg34)
        GrphModels(DTg = datg34, modP = fit, ElTit = "Profile Decay",NmModP = "", AjAx=T)
      }
      
      Grp.quadp <-function() {
        set.seed(123)
        x <- 1:30
        y <- quadp(x, 5, 1.7, -0.04, 20) + rnorm(30, 0, 0.6)
        datg35 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSquadp(X, a, b, c, xs), data = datg35, algorithm = "port")
        GrphModels(DTg = datg35, modP = fit, ElTit = "Profile Decay",NmModP = "", AjAx=T)
      }
      
      Grp.ricker <-function() {
        set.seed(123)
        x <- 1:30
        y <- 30 * x * exp(-0.3 * x) + rnorm(30, 0, 0.25)
        datg36 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSricker(X, a, b), data = datg36)
        GrphModels(DTg = datg36, modP = fit, ElTit = "Ricker",NmModP = "", AjAx=T)
      }
      
      Grp.scard3 <-function() {
        set.seed(1234)
        x <- 1:50
        y <- scard3(x, 13, 25, 36) + rnorm(length(x), sd = 0.05)
        datg37 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SSscard3(X, tb, to, tm), data = datg37)
        GrphModels(DTg = datg37, modP = fit, ElTit = "Smooth Cardinal",NmModP = "", AjAx=T)
      }
      
      Grp.sharp <-function() {
        temp <- 0:45
        rate <- sharp(temp, 1, 0.03, 1.44, 28, 19, 44) + rnorm(length(temp), 0, 0.05)
        datg38 <- data.frame(X = temp, Y = rate)
        ## Fit model
        fit <- nlsLM(Y ~ SSsharp(X, r_tref, e, el, tl, eh, th, tref = 20), data = datg38)
        GrphModels(DTg = datg38, modP = fit, ElTit = "Temperature Response" ,NmModP = "", AjAx=T)
      }
      
      Grp.temp3 <-function() {
        set.seed(1234)
        x <- 1:50
        y <- temp3(x, 25, 13, 36) + rnorm(length(x), sd = 0.05)
        datg39 <- data.frame(X = x, Y = y)
        fit <- nls(Y ~ SStemp3(X, t.m, t.l, t.h), data = datg39)
        GrphModels(DTg = datg39, modP = fit, ElTit = "Collatz Temperature Response" ,NmModP = "", AjAx=T)
      }
      
      
      Grp.Exp2P <-function()  {
        x<-1:5
        y<-c(2,4,8,20,25)
        datg40 <- data.frame(X = x, Y = y)
        fit <- nls(Y~SSexp2P(X,a,b), data= datg40)
        GrphModels(DTg = datg40, modP = fit, ElTit = "Exp Reg" ,NmModP = "", AjAx=T)
      }
      
      Grp.Exp3P <-function()  {
        x<-1:5
        y<-c(2,4,8,16,28)
        datg41 <- data.frame(X = x, Y = y)
        fit <- nls(Y~SSexp3P(X,a,b,c), data=datg41)
        GrphModels(DTg = datg41, modP = fit, ElTit = "Exp Reg2" ,NmModP = "", AjAx=T)
      }
      
      Grp.Power2P <-function()  {
        x<-1:5
        y<-c(2,4,8,20,25)
        datg42 <- data.frame(X = x, Y = y)
        fit <- nls(Y~SSpower2P(X,a,b), data= datg42)
        GrphModels(DTg = datg42, modP = fit, ElTit = "Power Reg1" ,NmModP = "", AjAx=T)
      }
      
      Grp.Power3P <-function()  {
        x<-1:5
        y<-c(2,4,8,20,25)
        datg43<-data.frame(X = x, Y = y)
        fit <- nls(Y~SSpower3P(X,a,b,c), data=datg43)
        GrphModels(DTg = datg43, modP = fit, ElTit = "Power Reg2" ,NmModP = "", AjAx=T)
      }
      
      CreatAsimReg2<-function() {
        
        chkPkg("graphics")
        
        xx <- seq(0, 5, len = 101)
        yy <- 5 - 4 * exp(-xx/(2*log(2)))
        par(mar = c(0, 0, 4.1, 0))
        plot(xx, yy, type = "l", axes = FALSE, ylim = c(0,6), xlim = c(-1, 5),
             xlab = "", ylab = "", lwd = 2,
             main = "Parameters in the SSasymp model")
        usr <- par("usr")
        arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
        arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
        text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
        text(-0.1, usr[4], "y", adj = c(1, 1))
        abline(h = 5, lty = 2, lwd = 0)
        arrows(-0.8, 2.1, -0.8, 0, length = 0.1, angle = 25)
        arrows(-0.8, 2.9, -0.8, 5, length = 0.1, angle = 25)
        text(-0.8, 2.5, expression(phi[1]), adj = c(0.5, 0.5))
        segments(-0.4, 1, 0, 1, lty = 2, lwd = 0.75)
        arrows(-0.3, 0.25, -0.3, 0, length = 0.07, angle = 25)
        arrows(-0.3, 0.75, -0.3, 1, length = 0.07, angle = 25)
        text(-0.3, 0.5, expression(phi[2]), adj = c(0.5, 0.5))
        segments(1, 3.025, 1, 4, lty = 2, lwd = 0.75)
        arrows(0.2, 3.5, 0, 3.5, length = 0.08, angle = 25)
        arrows(0.8, 3.5, 1, 3.5, length = 0.08, angle = 25)
        text(0.5, 3.5, expression(t[0.5]), adj = c(0.5, 0.5))
      }
      
      CreatAsimReg1<-function() {
        ## Visualize the SSasymp()  model  parametrization :
        
        xx <- seq(-.3, 5, len = 101)
        ##  Asym + (R0-Asym) * exp(-exp(lrc)* x) :
        yy <- 5 - 4 * exp(-xx / exp(3/4))
        stopifnot( all.equal(yy, SSasymp(xx, Asym = 5, R0 = 1, lrc = -3/4)) )
        chkPkg("graphics")
        op <- par(mar = c(0, .2, 4.1, 0))
        plot(xx, yy, type = "l", axes = FALSE, ylim = c(0,5.2), xlim = c(-.3, 5),
             xlab = "", ylab = "", lwd = 2,
             main ="Parameters in the SSasymp model")
        mtext(quote({f[phi](x) == phi[1] + (phi[2]-phi[1])*~e^{-e^{phi[3]}*~x}}~
                      list(phi[1] == "Asym", phi[2] == "R0", phi[3] == "lrc")
        ))
        usr <- par("usr")
        arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
        arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
        text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
        text(     -0.1, usr[4], "y", adj = c(1, 1))
        abline(h = 5, lty = 3)
        arrows(c(0.35, 0.65), 1,
               c(0  ,  1   ), 1, length = 0.08, angle = 25); text(0.5, 1, quote(1))
        y0 <- 1 + 4*exp(-3/4) ; t.5 <- log(2) / exp(-3/4) ; AR2 <- 3 # (Asym + R0)/2
        segments(c(1, 1), c( 1, y0),
                 c(1, 0), c(y0,  1),  lty = 2, lwd = 0.75)
        text(1.1, 1/2+y0/2, quote((phi[1]-phi[2])*e^phi[3]), adj = c(0,.5))
        axis(2, at = c(1,AR2,5), labels= expression(phi[2], frac(phi[1]+phi[2],2), phi[1]),
             pos=0, las=1)
        arrows(c(.6,t.5-.6), AR2,
               c(0, t.5   ), AR2, length = 0.08, angle = 25)
        text(   t.5/2,   AR2, quote(t[0.5]))
        text(   t.5 +.4, AR2,
                quote({f(t[0.5]) == frac(phi[1]+phi[2],2)}~{} %=>% {}~~
                        {t[0.5] == frac(log(2), e^{phi[3]})}), adj = c(0, 0.5))
        par(op)
      }
      
      CreatAsimReg3<-function() {
        ## Visualize the SSasympOff()  model  parametrization :
        
        xx <- seq(0.25, 8,  by=1/16)
        yy <- 5 * (1 -  exp(-(xx - 3/4)*0.4))
        stopifnot( all.equal(yy, SSasympOff(xx, Asym = 5, lrc = log(0.4), c0 = 3/4)) )
        chkPkg("graphics")
        op <- par(mar = c(0, 0, 4.0, 0))
        plot(xx, yy, type = "l", axes = FALSE, ylim = c(-.5,6), xlim = c(-1, 8),
             xlab = "", ylab = "", lwd = 2,
             main = "Parameters in the SSasympOff model")
        mtext(quote(list(phi[1] == "Asym", phi[2] == "lrc", phi[3] == "c0")))
        usr <- par("usr")
        arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
        arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
        text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
        text(     -0.1, usr[4], "y", adj = c(1, 1))
        abline(h = 5, lty = 3)
        arrows(-0.8, c(2.1, 2.9),
               -0.8, c(0  , 5  ), length = 0.1, angle = 25)
        text  (-0.8, 2.5, quote(phi[1]))
        segments(3/4, -.2, 3/4, 1.6, lty = 2)
        text    (3/4,    c(-.3, 1.7), quote(phi[3]))
        arrows(c(1.1, 1.4), -.15,
               c(3/4, 7/4), -.15, length = 0.07, angle = 25)
        text    (3/4 + 1/2, -.15, quote(1))
        segments(c(3/4, 7/4, 7/4), c(0, 0, 2),   # 5 * exp(log(0.4)) = 2
                 c(7/4, 7/4, 3/4), c(0, 2, 0),  lty = 2, lwd = 2)
        text(      7/4 +.1, 2./2, quote(phi[1]*e^phi[2]), adj = c(0, .5))
        par(op)
      }
      
      CreatAsimReg4<-function() {
        ## Visualize the SSasympOrig()  model  parametrization :
        xx <- seq(0, 5,  by=.05)
        # xx <- seq(0, 5, length.out = 101)
        yy <- 5 * (1- exp(-xx * log(2)))
        stopifnot( all.equal(yy, SSasympOrig(xx, Asym = 5, lrc = log(log(2)))) )
        
        chkPkg("graphics")
        op <- par(mar = c(0, 0, 3.5, 0))
        plot(xx, yy, type = "l", axes = FALSE, ylim = c(0,5), xlim = c(-1/4, 5),
             xlab = "", ylab = "", lwd = 2,
             main = "Parameters in the SSasympOrig model")
        mtext(quote(list(phi[1] == "Asym", phi[2] == "lrc")))
        usr <- par("usr")
        arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
        arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
        text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
        text(   -0.1,   usr[4], "y", adj = c(1, 1))
        abline(h = 5, lty = 3)
        axis(2, at = 5*c(1/2,1), labels= expression(frac(phi[1],2), phi[1]), pos=0, las=1)
        arrows(c(.3,.7), 5/2,
               c(0, 1 ), 5/2, length = 0.08, angle = 25)
        text(   0.5,     5/2, quote(t[0.5]))
        text(   1 +.4,   5/2,
                quote({f(t[0.5]) == frac(phi[1],2)}~{} %=>% {}~~{t[0.5] == frac(log(2), e^{phi[2]})}),
                adj = c(0, 0.5))
        par(op)
      }
      
      CreatLogistic<-function() {
        ## Visualizing the  SSfpl()  parametrization
        xx <- seq(-0.5, 5, length.out = 101)
        yy <- 1 + 4 / (1 + exp((2-xx))) # == SSfpl(xx, *) :
        stopifnot( all.equal(yy, SSfpl(xx, A = 1, B = 5, xmid = 2, scal = 1)) )
        chkPkg("graphics")
        op <- par(mar = c(0, 0, 3.5, 0))
        plot(xx, yy, type = "l", axes = FALSE, ylim = c(0,6), xlim = c(-1, 5),
             xlab = "", ylab = "", lwd = 2,
             main = "Parameters in the SSfpl model")
        mtext(quote(list(phi[1] == "A", phi[2] == "B", phi[3] == "xmid", phi[4] == "scal")))
        usr <- par("usr")
        arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
        arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
        text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
        text(     -0.1, usr[4], "y", adj = c(1, 1))
        abline(h = c(1, 5), lty = 3)
        arrows(-0.8, c(2.1, 2.9),
               -0.8, c(0,   5  ), length = 0.1, angle = 25)
        text  (-0.8, 2.5, quote(phi[1]))
        arrows(-0.3, c(1/4, 3/4),
               -0.3, c(0,   1  ), length = 0.07, angle = 25)
        text  (-0.3, 0.5, quote(phi[2]))
        text(2, -.1, quote(phi[3]))
        segments(c(2,3,3), c(0,3,4), # SSfpl(x = xmid = 2) = 3
                 c(2,3,2), c(3,4,3),    lty = 2, lwd = 0.75)
        arrows(c(2.3, 2.7), 3,
               c(2.0, 3  ), 3, length = 0.08, angle = 25)
        text(      2.5,     3, quote(phi[4])); text(3.1, 3.5, "1")
        par(op)
      }
      
      CreatLogistic2 <-function() {
        ## Visualize the SSlogis()  model  parametrization :
        xx <- seq(-0.75, 5, by=1/32)
        yy <- 5 / (1 + exp((2-xx)/0.6)) # == SSlogis(xx, *):
        stopifnot( all.equal(yy, SSlogis(xx, Asym = 5, xmid = 2, scal = 0.6)) )
        require(graphics)
        op <- par(mar = c(0.5, 0, 3.5, 0))
        plot(xx, yy, type = "l", axes = FALSE, ylim = c(0,6), xlim = c(-1, 5),
             xlab = "", ylab = "", lwd = 2,
             main = "Parameters in the SSlogis model")
        mtext(quote(list(phi[1] == "Asym", phi[2] == "xmid", phi[3] == "scal")))
        usr <- par("usr")
        arrows(usr[1], 0, usr[2], 0, length = 0.1, angle = 25)
        arrows(0, usr[3], 0, usr[4], length = 0.1, angle = 25)
        text(usr[2] - 0.2, 0.1, "x", adj = c(1, 0))
        text(     -0.1, usr[4], "y", adj = c(1, 1))
        abline(h = 5, lty = 3)
        arrows(-0.8, c(2.1, 2.9),
               -0.8, c(0,   5  ), length = 0.1, angle = 25)
        text  (-0.8, 2.5, quote(phi[1]))
        segments(c(2,2.6,2.6), c(0,  2.5,3.5),   # NB.  SSlogis(x = xmid = 2) = 2.5
                 c(2,2.6,2  ), c(2.5,3.5,2.5), lty = 2, lwd = 0.75)
        text(2, -.1, quote(phi[2]))
        arrows(c(2.2, 2.4), 2.5,
               c(2.0, 2.6), 2.5, length = 0.08, angle = 25)
        text(      2.3,     2.5, quote(phi[3])); text(2.7, 3, "1")
        par(op)
      }
      
      
      
      # Database with functions
      {
        NLM.MM = list(
          # El Modelo de referencia
          'RegAs1' = list(
            GenTypeM="E.Mod",
            ClasType="AsymRegres",
            Model="SSasymp Original",
            Form1  =  formula(Y~Asym+(R0-Asym)*exp(-exp(lrc)*X)),
            Form1b = quote({f[theta](x) == theta[1] + (theta[2]-theta[1])*~e^{-e^{theta[3]}*~x}}~
                             list(theta[1] == "Asymp", theta[2] == "Origin", theta[3] == "Log of Rate")),
            Form1c = expression(nlsGen(Form1, formula(Y ~ SSasymp(X, Asym, R0, lrc)), DTPas, list(Asym = YMin, R0 = YMax, lrc = Rate))),
            Grp = as.ggplot(function() CreatAsimReg1()),
            Inv=FALSE
          ),
          'RegAs2' = list(
            GenTypeM="E.Mod",
            ClasType="AsymRegres",
            Model="SSasymp offset",
            Form1  =  formula(Y~Asym*(1 - exp(-exp(lrc)*(X - c0)))),
            Form1b = quote({f[theta](x) == theta[1]*~ (1-e^{-e^{theta[3]}*~(x-theta[2])})}~
                             list(theta[1] == "Asymp", theta[2] == "Orig (Y=0)", theta[3] == "Log of Rate")),
            Form1c = expression(nlsGen(Form1, formula(Y ~ SSasympOff(X, Asym, c0, lrc)), DTPas, list(Asym = Asym, c0 = Orig, lrc = lrc))),
            Grp = as.ggplot(function() CreatAsimReg3()),
            Inv=FALSE
          ),
          'RegAs3' = list(
            GenTypeM="E.Mod",
            ClasType="AsymRegres",
            Model="SSasymp through Origin",
            Form1  =  formula(Y~Asym*(1 - exp(-exp(lrc)*input))),
            Form1b = quote({f[theta](x) == theta[1]*~ (1-e^{-e^{theta[2]*~x}})}~
                             list(theta[1] == "Asymp", theta[2] == "Log of Rate")),
            Form1c = expression(nlsGen(Form1, formula(Y ~ SSasympOrig(X, Asym, lrc)), DTPas, list(Asym = Asym, lrc = lrc))),
            Grp = as.ggplot(function() CreatAsimReg4()),
            Inv=FALSE
          ),
          'RegAs4' = list(
            GenTypeM="E.Mod",
            ClasType="AsymRegres",
            Model="NLS.asymReg",
            Form1  =  formula(Y~plateau - (plateau - init) * exp(-m * X)),
            Form1b = quote({f[theta](x) == theta[1] + (theta[2]-theta[1])*~e^{-e^{theta[3]}*~x}}~
                             list(theta[1] == "Asymp", theta[2] == "Origin", theta[3] == "Log of Rate")),
            Form1c = expression(nlsGen(Form1, formula(Y ~ NLS.asymReg(X, init, m, plateau)), DTPas, 
                                       list(plateau = Asym, init = Orig, m = lrc))),
            Grp = as.ggplot(function() CreatAsimReg2()),
            Inv=FALSE
          ),
          'Poly1' = list(
            GenTypeM="E.Mod",
            ClasType="Polynomial",
            Model="Poly-5",
            Form1  =  formula(Y~poly(X,5)),
            Form1b = {f[theta](x) == theta[1]~+~theta[2]*x~+~theta[3]*x^2~+~...+theta[6]*x^5}~
              list(theta[1] == "Orig", theta[2] == "Lin", theta[3] == "Quad",...,theta[6] == "Quintic"),
            Form1c = expression(lm(Form1, data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Compart1' = list(
            GenTypeM="E.Mod",
            ClasType="Compartment Order-1",
            Model="SSfol (nls)",
            Form1  =  formula(Y~Dose * exp(lKe+lKa-lCl) * (exp(-exp(lKe)*X) - 
                                                             exp(-exp(lKa)*X)) / (exp(lKa) - exp(lKe))),
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSfol(Dose, X, lKe, lKa, lCl), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Mich_Men' = list(
            GenTypeM="E.Mod",
            ClasType="Michaelis-Menten Hyperbola",
            Model="SSmicmen (nls)",
            Form1  =  expression(Y ~ Vm*X/(K+X)),
            Form1b = quote({f[theta](x) == theta[1]*frac(X,theta[2]+X)}),
            Form1c =expression(nlsLM(Y ~ SSmicmen(X, Vm, K), data = DTPas)),
            Grp = as.ggplot(function() Grp.MicMan()),
            Inv=FALSE
          ),
          'Gompertz' = list(
            GenTypeM="E.Mod",
            ClasType="Gompertz",
            Model="SSgompertz (nls)",
            Form1  =  expression(Y~Asym*exp(-b2*b3^X)),
            Form1b = quote({f[theta](x) == theta[1]*e^{-theta[2]*theta[3]^~X}}),
            Form1c =expression(nlsLM(Y ~ SSgompertz(X, Asym, b2, b3), data = DTPas)),
            Grp = as.ggplot(function() Grp.Gompetz()),
            Inv=FALSE
          ),
          'Weibull1' = list(
            GenTypeM="S.Mod",
            ClasType="Weibull",
            Model="SSweibull (nls)",
            Form1  =  expression(Y~Asym-Drop*exp(-exp(lrc)*x^pwr)),
            Form1b = quote({f[theta](x) == theta[1]-theta[2]*e^{-e^theta[2]*X^theta[3]}}),
            Form1c =expression(nlsLM(Y ~ SSweibull(X, Asym, Drop, lrc, pwr), data = DTPas)),
            Grp = as.ggplot(function() Grp.Weibull()),
            Inv=FALSE
          ),
          'Logis1' = list(
            GenTypeM="S.Mod",
            ClasType="Logistic",
            Model="SSlogis (nls)",
            Form1  =  expression(Y~Asym/(1+exp((xmid-input)/scal))),
            Form1b = quote({f[theta](x) == frac(theta[1], 1+e^frac(theta[2]-X,theta[3]))}),
            Form1c =expression(nlsLM(Y ~ SSlogis(X, Asym, xmid, scal), data = DTPas)),
            Grp = as.ggplot(function() CreatLogistic2()),
            Inv=FALSE
          ),
          'Bell1' = list(
            GenTypeM="S.Mod",
            ClasType="Bell-shaped curve",
            Model="SSbell (nlraa)",
            Form1  =  NA,
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSbell(X, ymax, a, b, xc), data = DTPas)),
            Grp = Grp.bell(),
            Inv=FALSE
          ),
          'Beta5' = list(
            GenTypeM="S.Mod",
            ClasType="Beta 5-params (crop development)",
            Model="SSbeta5 (nlraa)",
            Form1  = formula(Y ~ exp(mu)*(temp-tb)^a*(tc-temp)^b),
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSbeta5(X, mu, tb, a, tc, b), data = DTPas)),
            Grp = Grp.Beta5(),
            Inv=FALSE
          ),
          'Beta4r' = list(
            GenTypeM="S.Mod",
            ClasType="Beta 4-params Reparam",
            Model="SSbg4rp (nlraa)",
            Form1  = formula(Y ~ w.max*(1+(exp(lt.e)-time)/exp(ldtm))*((time-(exp(lt.e)-exp(ldtb)))/exp(ldtb))(exp(ldtb)/exp(ldtm))),
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSbg4rp(X, w.max, lt.e, ldtm, ldtb), data = DTPas)),
            Grp = Grp.bg4rp(),
            Inv=FALSE
          ),
          'Beta3' = list(
            GenTypeM="S.Mod",
            ClasType="Beta 3-params",
            Model="SSbgf (nlraa)",
            Form1  = formula(Y ~ w.max*(1+(t.e-time)/(t.e-t.m))*(time/t.e)(t.e/(t.e-t.m))),
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSbgf(X,w.max, t.e, t.mb), data = DTPas)),
            Grp = Grp.bgf(),
            Inv=FALSE
          ),
          'Beta4b' = list(
            GenTypeM="S.Mod",
            ClasType="Beta 4-params",
            Model="SSbgf4 (nlraa)",
            Form1  = "Yin et al. (2003) “A Flexible Sigmoid Function of Determinate Growth",
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSbgf4(X, w.max, t.e, t.m, t.b), data = DTPas)),
            Grp = Grp.bgf4(),
            Inv=FALSE
          ),
          'Beta3r' = list(
            GenTypeM="S.Mod",
            ClasType="Beta 3-params Reparam",
            Model="SSbgrp (nlraa)",
            Form1  = formula(Y ~ w.max*(1+(exp(lt.e)-time)/exp(ldt))*(time/exp(lt.e))(exp(lt.e)/exp(ldt))),
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSbgrp(X, w.max, lt.e, ldt), data = DTPas)),
            Grp = Grp.bgrp(),
            Inv=FALSE
          ),
          'Cardin1' = list(
            GenTypeM="S.Mod",
            ClasType="Cardinal",
            Model="SScard3 (nlraa)",
            Form1  =  "Archontoulis and Miguez (2015) - (doi:10.2134/agronj2012.0506)",
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SScard3(X, tb, to, tm), data = DTPas)),
            Grp = Grp.card3(),
            Inv=FALSE
          ),
          'Log.nl1' = list(
            GenTypeM="S.Mod",
            ClasType="Declining Logistic",
            Model="SSdlf (nlraa)",
            Form1  =  expression(Y ~ (asym-a2)/(1+exp((xmid-time)/scal))+a2),
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSdlf(X, asym, a2, xmid, scal), data = DTPas)),
            Grp = Grp.dif(),
            Inv=FALSE
          ),
          'Expon.nl1' = list(
            GenTypeM="E.Mod",
            ClasType="Simple Exponential",
            Model="SSexpf (nlraa)",
            Form1  =  expression(Y ~ a*exp(c*x)),
            Form1b = quote(f[theta](x) == theta[1]*exp(1)^{theta[2] * X}~
                             list(theta[1] == b[0], theta[2] == b[1])),
            Form1c =expression(nlsLM(Y ~ SSexpf(X, a, c), data = DTPas)),
            Grp = Grp.expf(),
            Inv=FALSE
          ),
          'Expon.nl2' = list(
            GenTypeM="E.Mod",
            ClasType="Exponential-plateau",
            Model="SSexpfp (nlraa)",
            Form1  =  expression(Y ~ x.inf*a*exp(c*x)+x.sup*(a*exp(c*xs))),
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSexpfp(X, a, c, xs), data = DTPas)),
            Grp = Grp.expfp(),
            Inv=FALSE
          ),
          'Expon.nl3' = list(
            GenTypeM="E.Mod",
            ClasType="Exponential-linear",
            Model="SSexplin (nlraa)",
            Form1  =  expression(Y ~ (cm/rm)*log(1+exp(rm*(t-tb)))),
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSexplin(X, cm, rm, tb), data = DTPas)),
            Grp = Grp.explin(),
            Inv=FALSE
          ),
          'Bilin' = list(
            GenTypeM="E.Mod",
            ClasType="Bilinear",
            Model="SSblin (nlraa)",
            Form1  =  NA,
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSblin(X, a, b, xs, c), data = DTPas)),
            Grp = Grp.blin(),
            Inv=FALSE
          ),
          'Hill1' = list(
            GenTypeM="E.Mod",
            ClasType="Hill function 1-param",
            Model="SShill1 (nlraa)",
            Form1  = formula(Y ~ 1/(1+(Ka/X))),
            #Form1b = quote({f[theta](x) == 1/(1+(theta[1]/X))}),
            Form1b = quote({f[theta](x) == frac(1,1+(theta[1]/X))}),
            #quote({f[theta](x) == \frac{1}{1+(theta[1]/X)}}),
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SShill1(X, Ka), data = DTPas)),
            Grp = Grp.Hill1(),
            Inv=FALSE
          ),
          'Hill2' = list(
            GenTypeM="E.Mod",
            ClasType="Hill function 2-param",
            Model="SShill2 (nlraa)",
            Form1  = formula(Y ~ 1/(1+(Ka/x)^n)),
            Form1b = quote({f[theta](x) == 1/(1+(theta[1]/X)^theta[2])}),
            #quote({f[theta](x) == \frac{1}{1+(theta[1]/X)}}),
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SShill2(X, Ka, n), data = DTPas)),
            Grp = Grp.Hill2(),
            Inv=FALSE
          ),
          'Hill3' = list(
            GenTypeM="E.Mod",
            ClasType="Hill function 3-param",
            Model="SShill3 (nlraa)",
            Form1  = formula(Y ~ a/(1+(Ka/x)^n)),
            Form1b = quote({f[theta](x) == theta[3]/(1+(theta[1]/X)^theta[2])}),
            #quote({f[theta](x) == \frac{1}{1+(theta[1]/X)}}),
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SShill3(X, Ka, n, a), data = DTPas)),
            Grp = Grp.Hill3(),
            Inv=FALSE
          ),
          'LinPlat' = list(
            GenTypeM="E.Mod",
            ClasType="Linear-plateau",
            Model="SSlinp (nlraa)",
            Form1  =  "XInf: Lin; XSup: Flat at a+b*xs",
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSlinp(X, Ka, n, a), data = DTPas)),
            Grp = Grp.linp(),
            Inv=FALSE
          ),
          'Log.nl2' = list(
            GenTypeM="S.Mod",
            ClasType="Logistic 5-params",
            Model="SSlogis5 (nlraa)",
            Form1  =  expression(Y ~ asym2+(asym1-asym2)/(1+exp(iscal*(log(x)-log(xmid))))^theta),
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSlogis5(X, asym1, asym2, xmid, iscal, theta), data = DTPas)),
            Grp = Grp.logis5(),
            Inv=FALSE
          ),
          'Hyperb1' = list(
            GenTypeM="E.Mod",
            ClasType="Modified Hyperbola",
            Model="SSmoh (nlraa)",
            Form1  =  NA,
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSmoh(X, asym, xmin, k), data = DTPas)),
            Grp = Grp.Smoh(),
            Inv=FALSE
          ),
          'Hyperb2' = list(
            GenTypeM="E.Mod",
            ClasType="Non-rectangular hyperbola",
            Model="SSnrh (nlraa)",
            Form1  =  "Archontoulis and Miguez (2015) - (doi:10.2134/agronj2012.0506)",
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSnrh(X, asym, phi, theta, rd), data = DTPas)),
            Grp = Grp.nrh(),
            Inv=FALSE
          ),
          'Expon.nl4' = list(
            GenTypeM="E.Mod",
            ClasType="plateau-exponential",
            Model="SSpexpf (nlraa)",
            Form1  =  "XInf: a; XSup: a∗exp(c∗(x−xs))",
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSpexpf(X, a, xs, c), data = DTPas)),
            Grp = Grp.pexpf(),
            Inv=FALSE
          ),
          'LinPlat2' = list(
            GenTypeM="E.Mod",
            ClasType="plateau-linear",
            Model="SSplin (nlraa)",
            Form1  =  "XInf: a; XSup: a+b∗(x−xs)",
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSplin(X, a, xs, b), data = DTPas)),
            Grp = Grp.plin(),
            Inv=FALSE
          ),
          'PlatQuad' = list(
            GenTypeM="E.Mod",
            ClasType="plateau-quadratic",
            Model="SSpquad (nlraa)",
            Form1  =  "Archontoulis and Miguez (2015) - (doi:10.2134/agronj2012.0506)",
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSpquad(X, a, xs, b, c), data = DTPas)),
            Grp = Grp.pquad(),
            Inv=FALSE
          ),
          'PlatQuad2' = list(
            GenTypeM="E.Mod",
            ClasType="plateau-quadratic 3-params",
            Model="SSpquad3 (nlraa)",
            Form1  =  "Archontoulis and Miguez (2015) - (doi:10.2134/agronj2012.0506)",
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSpquad3(X, a, b, c), data = DTPas)),
            Grp = Grp.pquad3(),
            Inv=FALSE
          ),
          'Canopy' = list(
            GenTypeM="E.Mod",
            ClasType="Profile Decay",
            Model="SSprofd (nlraa)",
            Form1  =  "Archontoulis and Miguez (2015) - (doi:10.2134/agronj2012.0506)",
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSprofd(X, a, b, c, d), data = DTPas)),
            Grp = Grp.profd(),
            Inv=FALSE
          ),
          'PlatQuad3' = list(
            GenTypeM="E.Mod",
            ClasType="quadratic-plateau",
            Model="SSquadp (nlraa)",
            Form1  =  "Archontoulis and Miguez (2015) - (doi:10.2134/agronj2012.0506)",
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSquadp(X, a, b, c, xs), data = DTPas)),
            Grp = Grp.quadp(),
            Inv=FALSE
          ),
          'Quad1' = list(
            GenTypeM="E.Mod",
            ClasType="Quadratic-Plateau",
            Model="SSquadp3 (nlraa)",
            Form1  =  NA, #formula(Y ~ (xs1∗(a+b∗x+c∗x^2) + xs2∗(a+(−b^2)/(4∗c)))),
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSquadp3(X, a, b, c), data = DTPas)),
            Grp = Grp.quadp3(),
            Inv=FALSE
          ),
          'Quad2' = list(
            GenTypeM="E.Mod",
            ClasType="Quadratic-Plateau with break-point",
            Model="SSquadp3xs (nlraa)",
            Form1  =  NA, 
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSquadp3xs(X, a, b, xs), data = DTPas)),
            Grp = Grp.quadp3xs(),
            Inv=FALSE
          ),
          'Ratio' = list(
            GenTypeM="M.Mod",
            ClasType="Rational",
            Model="SSratio (nlraa)",
            Form1  =  formula(Y ~ a*x^c/(1+b*x^d)), 
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSratio(X, a, b, c, d), data = DTPas)),
            Grp = Grp.Ratio(),
            Inv=FALSE
          ),
          'Ricker' = list(
            GenTypeM="E.Mod",
            ClasType="Ricker",
            Model="SSricker (nlraa)",
            Form1  =  expression(Y ~ a*time*exp(-b*time)),
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSricker(X, a, b), data = DTPas)),
            Grp = Grp.ricker(),
            Inv=FALSE
          ),
          'Cardin2' = list(
            GenTypeM="E.Mod",
            ClasType="Smooth Cardinal",
            Model="SSscard3 (nlraa)",
            Form1  =  "Archontoulis and Miguez (2015) - (doi:10.2134/agronj2012.0506) - Equation 5.1 in Table 1",
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSscard3(X, tb, to, tm), data = DTPas)),
            Grp = Grp.scard3(),
            Inv=FALSE
          ),
          'Sharp' = list(
            GenTypeM="E.Mod",
            ClasType="Temperature Response",
            Model="SSsharp (nlraa)",
            Form1  =  "Schoolfield, R. M., Sharpe, P. J. & Magnuson (1981) https://doi.org/10.1016/0022-5193(81)90246-0",
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSsharp(X, r_tref, e, el, tl, eh, th, tref = 0), data = DTPas)),
            Grp = Grp.sharp(),
            Inv=FALSE
          ),
          'Collatz' = list(
            GenTypeM="E.Mod",
            ClasType="Collatz Temperature Response",
            Model="SStemp3 (nlraa)",
            Form1  =  "Schoolfield, R. M., Sharpe, P. J. & Magnuson (1981) https://doi.org/10.1016/0022-5193(81)90246-0",
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SStemp3(X,t.m, t.l, t.h), data = DTPas)),
            Grp = Grp.temp3(),
            Inv=FALSE
          ),
          'Trilin' = list(
            GenTypeM="E.Mod",
            ClasType="Trilinear",
            Model="SStrlin (nlraa)",
            Form1  =  NA,
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SStrlin(X,a, b, xs1, c, xs2, d), data = DTPas)),
            Grp = Grp.Trlin(),
            Inv=FALSE
          ),
          'Vegan1' = list(
            GenTypeM="E.Mod",
            ClasType="Arrhenius",
            Model="SSarrhenius (vegan)",
            Form1  =  formula(Y ~ k*area^z),
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSarrhenius(X, k, z), data = DTPas)),
            Grp = as.ggplot(function() Grp.vegan()),
            Inv=FALSE
          ),
          'Vegan2' = list(
            GenTypeM="E.Mod",
            ClasType="Gleason",
            Model="SSgleason (vegan)",
            Form1  =  formula(Y ~ k + slope*log(area)),
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSgleason(X, k, slope), data = DTPas)),
            Grp = as.ggplot(function() Grp.vegan()),
            Inv=FALSE
          ),
          'Vegan3' = list(
            GenTypeM="E.Mod",
            ClasType="Gitay",
            Model="SSgitay (vegan)",
            Form1  =  formula(Y ~ (k + slope*log(area))^2),
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSgitay(X, k, slope), data = DTPas)),
            Grp = as.ggplot(function() Grp.vegan()),
            Inv=FALSE
          ),
          'Vegan4' = list(
            GenTypeM="E.Mod",
            ClasType="Lomolino",
            Model="SSlomolino (vegan)",
            Form1  =  formula(Y ~ Asym/(1 + slope^log(xmid/area))),
            Form1b = NA,
            #Form1c =  expression(nlsGen(Form1, formula(Y ~ SSfol(Dose, X, lKe, lKa, lCl)), DTPas, 
            #                           list(lke = 1, lka = 1, lCl = 1))),
            Form1c =expression(nlsLM(Y ~ SSlomolino(X, Asym, xmid, slope), data = DTPas)),
            Grp = as.ggplot(function() Grp.vegan()),
            Inv=FALSE
          ),
          'ExpReg1' = list(
            GenTypeM="E.Mod",
            ClasType="Exp Reg",
            Model="SSexp2P (ggtrendline)",
            Form1  =  expression(Y ~ a*exp(b*x)),
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSexp2P(X, a, b), data = DTPas)),
            Grp = Grp.Exp2P(),
            Inv=FALSE
          ),
          'ExpReg2' = list(
            GenTypeM="E.Mod",
            ClasType="Exp Reg2",
            Model="SSexp3P (ggtrendline)",
            Form1  =  expression(Y ~ a*exp(b*x)+c),
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSexp3P(X, a, b, c), data = DTPas)),
            Grp = Grp.Exp3P(),
            Inv=FALSE
          ),
          'PowReg1' = list(
            GenTypeM="E.Mod",
            ClasType="Popwer Reg1",
            Model="SSpower2P (ggtrendline)",
            Form1  =  expression(Y ~ a*x^b),
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSpower2P(X, a, b), data = DTPas)),
            Grp = Grp.Power2P(),
            Inv=FALSE
          ),
          'PowReg2' = list(
            GenTypeM="E.Mod",
            ClasType="Popwer Reg2",
            Model="SSpower3P (ggtrendline)",
            Form1  =  expression(Y ~ a*x^b+c),
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSpower3P(X, a, b, c), data = DTPas)),
            Grp = Grp.Power3P(),
            Inv=FALSE
          ),
          'GreenAmpt' = list(
            GenTypeM="E.Mod",
            ClasType="Green-Ampt water infiltration",
            Model="SSgampt (HydroMe)",
            Form1  =  NA,
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSgampt(X,ks,A), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Gardner' = list(
            GenTypeM="E.Mod",
            ClasType="Gardner water retention",
            Model="SSgard (HydroMe)",
            Form1  =  NA,
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSgard(X,thr,ths,alp,nscal), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Horton' = list(
            GenTypeM="E.Mod",
            ClasType="Horton water infiltration",
            Model="SShorton (HydroMe)",
            Form1  =  NA,
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SShorton(X, fc, f0, lrk), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Kosugi' = list(
            GenTypeM="E.Mod",
            ClasType="Kosugi water retention",
            Model="SSkosugi (HydroMe)",
            Form1  =  NA,
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSkosugi(X, thr, ths, alp, nscal), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Omuto' = list(
            GenTypeM="E.Mod",
            ClasType="Omuto water retention",
            Model="SSomuto (HydroMe)",
            Form1  =  NA,
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSomuto(X, Ths1, alp1, Ths2, alp2), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Philip' = list(
            GenTypeM="E.Mod",
            ClasType="Philip water infiltration",
            Model="SSphilip (HydroMe)",
            Form1  =  NA,
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSphilip(X, fc, S), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'vanGenu1' = list(
            GenTypeM="E.Mod",
            ClasType="van Genuchten 5-par water retention",
            Model="SSvgm (HydroMe)",
            Form1  =  NA,
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSvgm(X, thr, ths, alp, nscal, mscal), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'vanGenu2' = list(
            GenTypeM="E.Mod",
            ClasType="van Genuchten 4-par water retention",
            Model="SSvgm4 (HydroMe)",
            Form1  =  NA,
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSvgm4(X, Thr, Ths, alp, nscal), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Fredlund' = list(
            GenTypeM="E.Mod",
            ClasType="Fredlund-Xing water retention",
            Model="SSfredlund (HydroMe)",
            Form1  =  NA,
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSfredlund(X,thr,ths,alp,nscal,mscal), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Chiwrut' = list(
            GenTypeM="E.Mod",
            ClasType="Chiwrut model for ultrasonic response",
            Model="SSChwirut (NRAIA)",
            Form1  =  expression(exp(-exp(lrc)*input)/(b0+b1*input)),
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSChwirut(X, lrc, b0, b1), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Richards' = list(
            GenTypeM="E.Mod",
            ClasType="Richards Growth Model",
            Model="SSRichards (NRAIA)",
            Form1  =  expression(Asym*(1+exp((xmid-input)/scal))^(-exp(-lpow))),
            Form1b = NA,
            Form1c =expression(nlsLM(Y ~ SSRichards(X, Asym, xmid, scal, lpow), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'MM.Recip1' = list(
            GenTypeM="E.Mod",
            ClasType="Reciprocal Simple",
            Model="SSMMRecip1 (from eq)",
            Form1  =  expression(Y ~ Alfa + Beta/X),
            Form1b = quote({f[theta](x) == theta[1] + frac(theta[2],X)}),
            Form1c =expression(nlsLM(Y ~ SSMMRecip1(X, Alfa, Beta), data = DTPas)),
            Grp = Grp.MMRecipr (),
            Inv=FALSE
          ),
          'MM.Hyper4' = list(
            GenTypeM="E.Mod",
            ClasType="Simple Hyperbola",
            Model="SSMMHyper4 (from eq)",
            Form1  =  expression(Y ~ Alfa/(1+Beta*X)),
            Form1b = quote({f[theta](x) == frac(theta[1],1+theta[2]*X)}),
            Form1c =expression(nlsLM(Y ~ SSMMHyper4(X, Alfa, Beta), data = DTPas)),
            Grp = Grp.MMHyper4 (),
            Inv=FALSE
          ),
          'LogaritNI' = list(
            GenTypeM="E.Mod",
            ClasType="Logaritmic without intercept",
            Model="NLS.logCurveNI (from eq)",
            Form1  =  expression(Y ~ Alfa/(1+Beta*X)),
            Form1b = quote({f[theta](x) == frac(theta[1],1+theta[2]*X)}),
            Form1c =expression(nlsLM(Y ~ NLS.logCurveNI(X, b), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Rational.aos' = list(
            GenTypeM="E.Mod",
            ClasType="Ratio of two polynomials",
            Model="NLS.Rational (from aosmic)",
            Form1  =  expression(Y ~ (b + c*X) / (1 + a*X)),
            Form1b = quote({f[theta](x) == frac(theta[1]+theta[2]*X,1+theta[3]*X)}),
            Form1c =expression(nlsLM(Y ~ NLS.Rational(X, a, b, c), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Mitscherlich' = list(
            GenTypeM="E.Mod",
            ClasType="Modified Mitscherlich",
            Model="NLSAvsPFD (from aosmic)",
            Form1  =  expression(Y ~ Rd+Amax*(1-exp((-Qapp/Amax)*X))),
            Form1b = quote({f[theta](x) == theta[1]+theta[2]*(1-e^{(-theta[3]/theta[2])*X})}), # Revisar
            Form1c =expression(nlsLM(Y ~ NLSAvsPFD(X, Rd, Amax, Qapp), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Michaelis_Menten' = list(
            GenTypeM="E.Mod",
            ClasType="Michaelis-Menten",
            Model="NLS.MM.MM (from renz)",
            Form1  =  expression(Y ~ (Vm * X)/(Km + X)),
            Form1b = quote({f[theta](x) == theta[1]*frac(X,theta[2]+X)}~{"; "}~list(theta[1]==V[max],theta[2]==K[m])), # Revisar
            Form1c =expression(nlsLM(Y ~ NLS.MM.MM(X, Km, Vm), data = DTPas)),
            Grp = NA,
            Inv=FALSE
          ),
          'Michaelis_Menten_Lin1' = list(
            GenTypeM="E.Mod",
            ClasType="Michaelis-Menten Lin by Lineweaver-Burk",
            Model="NLS.MMLin1.MM (from renz)",
            Form1  =  expression(1/Y ~ ((Km / Vmax)*(1/X))+(1/Vmax)),
            Form1b = NA,
            Form1c =expression(lm(Y ~ X, data = 1/DTPas)),
            Grp = NA,
            Inv=TRUE
          ),
          'Michaelis_Menten_Lin2' = list(
            GenTypeM="E.Mod",
            ClasType="Michaelis-Menten Lin by Hanes-Woolf",
            Model="NLS.MMLin2.MM (from renz)",
            Form1  =  expression(X/Y ~ (X / Vmax)+(Km/Vm)),
            Form1b = NA,
            Form1c =expression(lm(Y ~ X, data = copy(DTPas) %>% mutate(Y=X/Y))),
            Grp = NA,
            Inv=TRUE
          ),
          'Michaelis_Menten_Lin3' = list(
            GenTypeM="E.Mod",
            ClasType="Michaelis-Menten Lin by Eadie-Hofstee",
            Model="NLS.MMLin3.MM (from renz)",
            Form1  =  expression(Y ~ (-Km * (Y / X))+Vm),
            Form1b = NA,
            Form1c =expression(lm(Y ~ X, copy(DTPas) %>% mutate(Y=Y/X))),
            Grp = NA,
            Inv=TRUE
          )
        )
    }
    
      
    }
    
    InitRegAsym <-function(DTPas) {
      x <- DTPas[[1]]
      y <- DTPas[[2]]
      dataS <- sortedXyData(x, y)
      Asym <- NLSstRtAsymptote(dataS)
      dataR <- dataS[1:2, ]
      Orig <- unname(coef(lm(y ~ x, data = dataR))[1])
      pseudoY <- log((y - Asym)/(Orig - Asym))
      coefs <- coef(lm(pseudoY ~ x - 1))
      lrc <- unname(-coefs[1])
      c(Asym=Asym,Orig=Orig,lrc=lrc)
    }
    
    ExtrMiMod <-function(nmMod='RegAs1') {
      with( NLM.MM[[nmMod]],
            {
              ModelMM.1=eval(Form1c)
              try({
                PrintMod.23(ModelMM.1,nmMod,DTPas,AjAx = T,NmModP = Form1b)
                NNN.1 <-  AdjParms.G(ModelMM.1)
                ResM.1= data.table(GenTypeM=GenTypeM,ClasType=ClasType,
                                   Model=Model,NNN.1)
                ResM.1
              })
            })
    }
    
    ExtrMiMod2 <- function(nmMod='RegAs1') {
      with( NLM2.MM[[nmMod]],
            {
              ModelMM.1=eval(Form1c)
              try({
                PrintMod.23(ModelMM.1,nmMod,DTPas,AjAx = T,NmModP = Form1b)
                NNN.1 <-  AdjParms.G(ModelMM.1)
                ResM.1= data.table(GenTypeM=GenTypeM,ClasType=ClasType,
                                   Model=Model,NNN.1)
                ResM.1
              })
            })
    }
    
    ExtrMiModF <- function(nmMod='RegAs1',BasPa="NLM.MM",dtp) {
      with( get(BasPa)[[nmMod]],
            {
              ModelMM.1=eval(Form1c)
              try({
                # PrintMod.23(ModelMM.1,nmMod,dtp,AjAx = T,NmModP = Form1b)
                NNN.1 <-  AdjParms.G(ModelMM.1)
                ResM.1= data.table(GenTypeM=GenTypeM,ClasType=ClasType,
                                   Model=Model, Id= nmMod, Algor ="nls2", NNN.1)
                ResM.1
              })
            })
    }
    
    # nmMod = iiM;BasPa = "NLM.MM";dtp = Efs; AjAx=FALSE
    # PrintMod.gg2(DTg = dtp, modP = ModelMM.1, ElTit = Model,NmModP = Form1b)
    ExtrMiModF.gg <- function(nmMod='RegAs1',BasPa="NLM.MM",dtp,AjAx=FALSE) {
      with( get(BasPa)[[nmMod]],
            {
              ModelMM.1=eval(Form1c)
              try({
                grpM <-PrintMod.gg(DTg = dtp, modP = ModelMM.1, ElTit = Model,NmModP = Form1b, AjAx=AjAx)
                # PrintMod.23(ModelMM.1,nmMod,dtp,AjAx = T,NmModP = Form1b)
                NNN.1 <-  AdjParms.G(ModelMM.1)
                ResM.1= list(Params=data.table(GenTypeM=GenTypeM,ClasType=ClasType,
                                   Model=Model,NNN.1),Grp=grpM)
                ResM.1
              })
            })
    }
    
    Devuelvnls2 <-function(iMp,dtp) {
      gg<-ExtrMiModF.gg(nmMod =  iMp ,BasPa = "NLM.MM",dtp = dtp,AjAx = TRUE)$Grp
      gg
    }
    
    DevuelvDRC <-function(iMp,dtp) {
      jj=AllModelsJn[AllModelsJn.Nm==iMp][[1]]
      kk<-drm(Y ~ X,data = dtp,fct=jj)
      gg <- PrintMod.gg(DTg = dtp, modP = kk, ElTit = iMp,NmModP = "", AjAx=TRUE)
      gg
    }
    
    DevuelvDRC.PorSi <-function(iMp,dtp) {
      jj=AllModels[AllModels.Nm==iMp][[1]]
      kk<-drm(Y ~ X,data = dtp,fct=jj)
      gg <- PrintMod.gg(DTg = dtp, modP = kk, ElTit = iMp,NmModP = "", AjAx=TRUE)
      gg
    }
    
    RsqMM<-function(ModelP) {
      ResRsq<-1 - var(residuals(ModelP))/var(nlme::getData(ModelP))
      p <- length(coefficients(ModelP))
      n <- length(nlme::getData(ModelP))
      R2.adj <- 1 - (((n - 1)/(n - p)) * (1 - ResRsq))
      R2.adj
    }
    
    SEMod<-function(ModelP)sigma(ModelP)
    
    rootxiGraph <- function (x, y, i1, i2, nt, alpha = 5, doparallel = FALSE) {
      lev = (100 - alpha)/100
      x1 <- x[i1:i2]
      y1 <- y[i1:i2]
      i = NULL
      fs = function(i, x1, y1) {
        pr1 <- x1[i]
        xm <- cbind()
        df <- NULL
        for (j in (1:nt)) {
          xm <- cbind(xm, cbind((x1 - pr1)^j))
        }
        df <- as.data.frame(xm, row.names = NULL, optional = FALSE)
        xnam <- paste0("V", 1:(nt + 0))
        fmla <- as.formula(paste("y1 ~ ", paste(xnam, collapse = "+")))
        c1 <- lm(fmla, data = df)
        yp1 <- predict(c1)
        ci1 <- confint(c1, level = lev)
        am <- matrix(ci1, ncol = 2, dimnames = list(c(paste0("a", 
                                                             0:nt)), c(colnames(ci1))))
        v0n = as.double(abs(c1$coeff[1]))
        points = cbind(v0n, pr1)
        names(points) = c("a0", "x0")
        out = list(c(v0n, pr1), yp1, am)
        names(out) = c("points", "predicted", "confint")
        return(out)
      }
      if (doparallel) {
        ncores = detectCores()
        cat(paste0("Available workers are ", ncores), "\n")
        t1 = Sys.time()
        cl <- makeCluster(ncores)
        registerDoParallel(cl)
        m3 = foreach(i = 1:length(i1:i2)) %dopar% {
          fs(i, x1, y1)
        }
        stopCluster(cl)
        t2 = Sys.time()
        print(as.POSIXlt(t2, "GMT") - as.POSIXlt(t1, "GMT"), 
              quote = F)
      }
      else {
        m3 = lapply(1:length(i1:i2), fs, x1 = x1, y1 = y1)
      }
      mans = as.data.frame(do.call(rbind, sapply(m3, function(x) {
        x[1]
      })))
      colnames(mans) = c("a0", "x0")
      rownames(mans) = 1:dim(mans)[1]
      n0 <- which.min(mans$a0)
      ipc <- mans[n0, "x0"]
      ys = as.data.frame(do.call(cbind, sapply(m3, function(x) {
        x[2]
      })))
      yf1 <- ys[, n0]
      return(yf1)
    }
    
    # theme_bw_MM --> Previous section
    
    AsymRegMM <-function(x,Asym,R0,c) {Asym - (Asym - R0) * exp (- c * x)}
    
    # 1ª Derivate of AsymReg
    DrvAsymReg<-function(x,Asym,R0,c) {(Asym - R0) * (exp(-c * x) * c)}
    
    # change names of parameters "Theta"
    ChangeSymb <- function(EqQuo, SymToCh) {
      # It also serves
       # str2expression(gsub("theta[1]", "a", Eq.1b, fixed = TRUE))
      Exp2<-parse(text = gsub("f[theta]", "f", as.expression(EqQuo), fixed = TRUE))
      Exp2<-parse(text = gsub("theta[1]", SymToCh[1], Exp2, fixed = TRUE))
      Exp2<-parse(text = gsub("theta[2]", SymToCh[2], Exp2, fixed = TRUE))
      Exp2<-parse(text = gsub("e^{
            theta[3]
        }", SymToCh[3], Exp2, fixed = TRUE))
    }
    
    # change original names of parameters in SSasymp or drc
    ChangeSymbAR <- function(EqQuo, SymRes, SymToCh=c("Asym","R0","lrc"),LaX="x",LaY="Y") {
      # It also serves
       # str2expression(gsub("theta[1]", "a", Eq.1b, fixed = TRUE))
      Exp2<-parse(text = gsub("input", LaX, EqQuo, fixed = TRUE))
      Exp2<-parse(text = gsub("response", LaY, Exp2, fixed = TRUE))
      for (i in 1:length(SymToCh)) {Exp2<-parse(text = gsub(SymToCh[i], SymRes[i], Exp2, fixed = TRUE))}
      Exp2
    }
    
    AdjParms <-function(ModelP,DataP) {
      coefs <- coef(ModelP)
      stdErr <- summary(ModelP)$coef[,"Std. Error"]
      rsq <- 1 - var(residuals(ModelP))/var(DataP)
      p <- length(coefficients(ModelP))
      n <- length(DataP)
      rsq_adj <- 1 - (((n - 1)/(n - p)) * (1 - rsq))
      rsd <- resid(ModelP)
      SErr<- sigma(ModelP)
      # sqrt(deviance(fm) / (nobs(fm) - length(coef(fm))))
      list(coefs = coefs, 
           stdErr = stdErr,
           Resid = rsd,
           R2 = rsq, 
           R2.Adj = rsq_adj, 
           SE= SErr,
           LogLik=logLik(ModelP)[[1]],
           AIC=AIC(ModelP)
      )
    }
    
    
    #MPas=ElMod;Ty=1
    #MPas=RsM1$Model
    #MPas=ModelMM.1
    AdjParms.G.por_si<-function(MPas,Ty=1){
      AllLin=c('nls','lm', 'nlsLM')
      TyMP=as.character(MPas$call[[1]])
      Smm<-summary(MPas)
      Coef<-data.table(Smm$coefficients)
      Estim<-frmMM(c(Coef[,'Estimate'][[1]]),2)
      if (TyMP %in% AllLin) Lasp=Coef$`Pr(>|t|`
      if (TyMP=='drm') Lasp=Coef$`p-value`
      Lasp[is.nan(Lasp)]=1
      Estim[Lasp<=.05]<-paste0(Estim[Lasp<=.05],"*")
      SEp=ifelse(TyMP=="drm", sqrt(summary(MPas)$resVar),summary(MPas)$sigma)
      
      Resf=NA
      if (Ty==1&(TyMP %in% AllLin)) Resf <-paste0(rownames(Smm$coefficients),"= ", Estim,collapse="; ")
      if (Ty==2&(TyMP %in% AllLin)) {Resf<-data.table(rbind(Estim));names(Resf)<-rownames(Smm$coefficients)}
      
      if (Ty==1&TyMP=='drm') Resf<-paste0(MPas$parNames[[2]],"= ", Estim,collapse="; ")
      if (Ty==2&TyMP=='drm') {Resf<-data.table(rbind(Estim));names(Resf)<-MPas$parNames[[2]]}
      
      Res=NA
      Res= as.data.table(list(
        AIC=AIC(MPas),
        LogLik=logLik(MPas)[[1]],
        #SE= sigma(MPas),
        SE=SEp,
        coefs =Resf))
      Res
    }
    
    AdjParms.G <- function(MPas,Ty=1){
      AllLin=c('nls','lm', 'nlsLM')
      TyMP=as.character(MPas$call[[1]])
      for (i in 1:length(TyMP)) if(TyMP[i]=='nlsLM') TyMP='nlsLM'
      for (i in 1:length(TyMP)) if(TyMP[i]=='nls') TyMP='nls'
      for (i in 1:length(TyMP)) if(TyMP[i]=='lm') TyMP='lm'
      Smm<-summary(MPas)
      Coef<-data.table(Smm$coefficients)
      Estim<-frmMM(c(Coef[,'Estimate'][[1]]),2)
      if (TyMP %in% AllLin) Lasp=Coef$`Pr(>|t|`
      if (TyMP=='drm') Lasp=Coef$`p-value`
      Lasp[is.nan(Lasp)]=1
      Estim[Lasp<=.05]<-paste0(Estim[Lasp<=.05],"*")
      SEp=ifelse(TyMP=="drm", sqrt(summary(MPas)$resVar),summary(MPas)$sigma)
      
      Resf=NA
      if (Ty==1&(TyMP %in% AllLin)) Resf <-paste0(rownames(Smm$coefficients),"= ", Estim,collapse="; ")
      if (Ty==2&(TyMP %in% AllLin)) {Resf<-data.table(rbind(Estim));names(Resf)<-rownames(Smm$coefficients)}
      
      if (Ty==1&TyMP=='drm') Resf<-paste0(MPas$parNames[[2]],"= ", Estim,collapse="; ")
      if (Ty==2&TyMP=='drm') {Resf<-data.table(rbind(Estim));names(Resf)<-MPas$parNames[[2]]}
      
      Res=NA
      Res= as.data.table(list(
        AIC=AIC(MPas),
        LogLik=logLik(MPas)[[1]],
        #SE= sigma(MPas),
        SE=SEp,
        coefs =Resf))
      Res
    }
    
    # Expressly for the drda package
    AdjParms.drda <- function(MPas){
      coefs =coefficients(MPas)
      Estim <-  paste(names(coefs),"=",as.vector(frmMM(coefs,2)),collapse = "; ")
      SEp=sigma(MPas)
      Res=NA
      Res= as.data.table(list(
        AIC=AIC(MPas),
        LogLik=logLik(MPas)[[1]],
        SE=SEp,
        coefs =Estim))
      Res
    }
    
    # DatP=Efs;ModPasP=MMModel;NmModP=NmMModel;TyGen="S.Mod";sTy="Gompertz"
    ProcMod<-function(DatP, ModPasP,NmModP,TyGen="S.Mod", sTy="Gompertz") {
      ExtrParDRCInt<-function(MPas)  {
        Smm<-summary(MPas)
        Coef<-data.table(Smm$coefficients)
        Estim<-frmMM(c(Coef[,'Estimate'][[1]]),2)
        Lasp=c(Coef[,'p-value'][[1]])
        Lasp[is.nan(Lasp)]=1
        Estim[Lasp<=.05]<-paste0(Estim[Lasp<=.05],"*")
        Resf<-paste0(MPas$parNames[[2]],"= ", Estim,collapse="; ")
        Resf
      }
      
      AdjParms.DRC.Int<-function(MPas){
        list(
          AIC=AIC(MPas),
          LogLik=logLik(MPas)[[1]],
          #SE= sigma(MPas),
          SE=sqrt(summary(MPas)$resVar),
          coefs =ExtrParDRCInt(MPas)
        )}
      
      NNN<-mclapply(1:length(ModPasP), function(j){
        ModP=NULL
        try(ModP<-drm(Y ~ X,data = DatP,fct=ModPasP[[j]]))
        if(!is.null(ModP)) AdjParms.DRC.Int(ModP)
      })
      LosNul<-unlist(Map(is.null, NNN));
      NQueda=sum(!LosNul)
      if (NQueda==0) ResMo<-data.table(GenTypeM = TyGen, ClasType= sTy, Id =NA, Algor=NA, Model=NA, AIC =NA, LogLik=NA, SE=NA, coefs=NA)
      if (NQueda>0) {
        NNN<-Filter(Negate(is.null), NNN)
        ResMo= data.table(GenTypeM=rep(TyGen,NQueda),
                          ClasType=rep(sTy,NQueda),
                          Model=NmModP[!LosNul],
                          Id=NmModP[!LosNul],
                          Algor ="drc",
                          do.call("rbind", NNN))
        ResMo$AIC<-unlist(ResMo$AIC)
        ResMo$LogLik<-unlist(ResMo$LogLik)
        ResMo$SE<-unlist(ResMo$SE)
        ResMo$coefs<-unlist(ResMo$coefs)
      }
      ResMo
    }
    
    # Efs,BCModel.nls,NmBCModel.nls,"M.Mod","Brain-Cousens"
    #DatP=Efs;ModPasP=BCModel.nls;NmModP=NmBCModel.nls; TyGen="M.Mod";sTy="Brain-Cousens"
    ProcMod3 <- function(DatP, ModPasP,NmModP,TyGen="S.Mod", sTy="Gompertz") {
      NNN<-mclapply(1:length(ModPasP), function(j){
        ModP=NULL
        try(ModP<-eval(ModPasP[[j]]))
        if(!is.null(ModP)) AdjParms.G(ModP)
      })
      LosNul<-unlist(Map(is.null, NNN));
      NQueda=sum(!LosNul)
      if (NQueda==0) ResMo<-data.table(GenTypeM = TyGen, ClasType= sTy, Id =NA, Algor=NA, Model=NA, AIC =NA, LogLik=NA, SE=NA, coefs=NA)
      if (NQueda>0) {
        NNN<-Filter(Negate(is.null), NNN)
        ResMo= data.table(GenTypeM=rep(TyGen,NQueda),
                          ClasType=rep(sTy,NQueda),
                          Model=NmModP[!LosNul],
                          Id=NmModP[!LosNul],
                          Algor ="nls1",
                          do.call("rbind", NNN))
        ResMo$AIC<-unlist(ResMo$AIC)
        ResMo$LogLik<-unlist(ResMo$LogLik)
        ResMo$SE<-unlist(ResMo$SE)
        ResMo$coefs<-unlist(ResMo$coefs)
      }
      ResMo
    }
    
   
    
    # ElModP=ModelMM[[NmMod]];NmMod=NmMod;LasMedP=LasMed;AjAx = T;NmModP = Expon1.q
    # TyLbl=TRUE;xLb="X";yLb="Y";xmaxp=10;yminp=0;cexP=1
    
    # To visualize specific models
    PrintMod.23<-function(ElModP,ElTit,LasMedP,TyLbl=TRUE,xLb="X",yLb="Y",xmaxp=10,yminp=0,AjAx=FALSE,cexP=1,NmModP=NULL) {
      XMin=head(LasMedP$X,1)
      XMax=tail(LasMedP$X,1)
      YMax=LasMedP[LasMedP$X==XMin,]$Y;
      YMin=LasMedP[LasMedP$X==XMax,]$Y;
      Rate=-(YMax-YMin)/(XMax-XMin);
      xPred <- seq(0, XMax, length = 1000);
      SE=round(SEMod(ElModP),4)
      #RSq=round(1 - var(residuals(ElModP))/var(LasMedP$Y),4)
      PredMod=predict(ElModP, data.frame(X = xPred))
      ylim2=max(max(PredMod),max(LasMedP$Y))
      if (is.infinite(ylim2)) ylim2=max(LasMedP$Y)
      if (AjAx) plot(xPred, PredMod,lty=1,lwd=3,xlab=xLb,ylab=yLb,type="l",xlim=c(0,xmaxp),ylim=c(yminp, ylim2),frame.plot=FALSE,cex.lab=1.25)
      if (!AjAx) plot(xPred, PredMod,lty=1,lwd=3,xlab=xLb,ylab=yLb,type="l",xlim=c(0,xmaxp),ylim=c(yminp, 1),frame.plot=FALSE,cex.lab=1.25)
      with(LasMedP, lines(X,Y,type="b",pch=16,cex=cexP));
      EtTxt=bquote(.(ElTit)*~(RSE==.(SE)))
      if (TyLbl) title(main=EtTxt) else title(main=ElTit)
      if (!is.null(NmModP) ) mtext(NmModP,line=-1)
      print(ElModP)
      summary(ElModP)
    }
    
    # New enhanced version with ggplot, which would allow me to store the plots or display them together
    PrintMod.gg <-function (DTg,modP, ElTit="Example", NmModP=NULL, AjAx=FALSE,
                            sample.curve = 1000, ylab = "Dependent", 
                            xlab = "Independent", theme = theme_classic(), legend.position = "top", 
                            r2 = "all", ic = FALSE, fill.ic = "gray70", alpha.ic = 0.5, 
                            error = "SE", point = "all", width.bar = NA, scale = "none", 
                            textsize = 12, pointsize = 3, linesize = 2, linetype = 1, 
                            pointshape = 21, fillshape = "gray40", colorline = "blue", 
                            round = NA, xname.formula = "x", yname.formula = "y", comment = NA, 
                            fontfamily = "sans") {
      
      DTg2<-as.data.table(copy(DTg))
      if (ElTit=="NLS.MMLin1.MM (from renz)") {DTg2=1/DTg; xlab=paste0("1/ ",xlab); ylab=paste0("1/ ",ylab)}
      if (ElTit=="NLS.MMLin2.MM (from renz)") {DTg2[,Y:=X/Y]; ylab=paste0(xlab," / ",ylab)}
      if (ElTit=="NLS.MMLin3.MM (from renz)") {DTg2[,Y:=Y/X]; ylab=paste0(ylab," / ",xlab)}
      
      IV=DTg2$X
      DV=DTg2$Y
      SE=round(SEMod(modP),4)
      EtTxt=bquote(.(ElTit)*~(RSE==.(SE)))
      MinX =min(IV)
      if (AjAx) MinX=0
      IV.p = seq(MinX, max(IV), length.out = sample.curve)
      DF.p = data.frame(X = IV.p, Y = predict(modP, newdata = data.frame(X = IV.p))) 
      
      grph = ggplot(DTg2, aes(x = X, y = Y))
      grph = grph + geom_point(size=pointsize, shape=pointshape, fill=fillshape, colour="white")
      
      #geom_point(aes(color = "black"), size = pointsize, 
      #      shape = pointshape, fill = fillshape)
      
      
      grph = grph + theme + geom_line(data = DF.p, aes(x = X, y = Y, color = "black"), linewidth = linesize, lty = linetype) +
        geom_line(data = DTg2, aes(x = X, y = Y, color = "black"), linewidth = .15, lty = linetype) +
        ggtitle(EtTxt)
      grph = grph +
        scale_color_manual(name = "", values = colorline, label = as.expression(NmModP)) + 
        theme(axis.text = element_text(size = textsize, color = "black",
                                      family = fontfamily), axis.title = element_text(size = textsize,
                                      color = "black", family = fontfamily), legend.position = legend.position, 
              legend.text = element_text(size = textsize, family = fontfamily), 
              legend.direction = "vertical", legend.text.align = 0, 
              legend.justification = 0) + ylab(ylab) + xlab(xlab)
      grph = grph + geom_point(size=pointsize, shape=pointshape, fill=fillshape, colour="white", stroke=pointsize+.25)
      
      grph
    }
    
    
    
    # Final function to scrutinize the models
      # LasMedP=Efs; TyLbl=JnEq; xLb="Decile";yLb="Pearson's Correlation"; xmaxp=10; yminp=-1;PIp=NULL;LineRate=TRUE;pointXMed="p";RootG=TRUE;MaxG=FALSE;InflG=FALSE
      # PIp Possible Inflection Point that I pass directly to the chart, so that it can be included.
    
    ExtrParDRC<-function(MPas)  {
      Smm<-summary(MPas)
      Coef<-data.table(Smm$coefficients)
      Estim<-frmMM(c(Coef[,'Estimate'][[1]]),2)
      Lasp=c(Coef[,'p-value'][[1]])
      Lasp[is.nan(Lasp)]=1
      Estim[Lasp<=.05]<-paste0(Estim[Lasp<=.05],"*")
      Resf<-paste0(MPas$parNames[[2]],"= ", Estim,collapse="; ")
      Resf
    }
    
    AdjParms.DRC<-function(MPas){
      list(
        AIC=AIC(MPas),
        LogLik=logLik(MPas)[[1]],
        #SE= sigma(MPas),
        SE=sqrt(summary(MPas)$resVar),
        coefs =ExtrParDRC(MPas)
      )}
    
    ExtrParLM<-function(MPas)  {
      Smm<-summary(MPas)
      Coef<-data.table(Smm$coefficients)
      Estim<-frmMM(c(Coef[,'Estimate'][[1]]),2)
      Lasp=c(Coef[,'Pr(>|t|)'][[1]])
      Lasp[is.nan(Lasp)]=1
      Estim[Lasp<=.05]<-paste0(Estim[Lasp<=.05],"*")
      Resf<-paste0(rownames(Smm$coefficients),"= ", Estim,collapse="; ")
      Resf
    }
    
    AdjParms.LM<-function(MPas){
      list(
        AIC=AIC(MPas),
        LogLik=logLik(MPas)[[1]],
        #SE= sigma(MPas),
        SE=summary(MPas)$sigma,
        coefs =ExtrParLM(MPas)
      )}
    
    AdjMod.23.f<-function(LasMedP,TyLbl=NULL,xLb="X",yLb="Y",xmaxp=10,yminp=0,PIp=NULL,
                          LineRate=TRUE,pointXMed="p",pointXMedLb="OnAx",RootG=FALSE,MaxG=FALSE,InflG=FALSE) {
      chkPkg(c("shape","RootsExtremaInflections"))
      decilecolors=c("#7F7FCE", "#7F7FF7", "#88A9F9", "#93D2FB", "#A0FCFE",
                     "#BDFDD7", "#DDFEB3", "#FFFF91", "#F8D68B", "#F3AE86" )
      colorsdef=rep(decilecolors, each=6)
      #ResF<-list()
      NmMod="RegAs1"
      
      XMin=head(LasMedP$X,1)
      XMax=tail(LasMedP$X,1)
      YMax=LasMedP[LasMedP$X==XMin,]$Y;
      YMin=LasMedP[LasMedP$X==XMax,]$Y;
      RatePrv=-(YMax-YMin)/(XMax-XMin);
      # xPred <- seq(XMin, XMax, length = 1000);
      
      fit<-NA
      try(fit<-nlsAsym2(YMin,YMax,RatePrv,LasMedP))
      
      xPred <- seq(0, XMax, length = 1000);
      PredMod=predict(fit, data.frame(X = xPred))
      plot(xPred, PredMod,lty=1,lwd=3,xlab=xLb,ylab=yLb,type="l",xlim=c(0,xmaxp),ylim=c(yminp, 1),frame.plot=FALSE,cex.lab=1.25)
      if (!is.null(TyLbl)) {mtext(do.call(expression, TyLbl),side=3,line=(length(TyLbl)-1):0)}
      
      tetha1=a=Asymptote= Asym=AsymStim=coefficients(fit)[[1]]
      theta2=b=Origin=    R0=R0Stim=coefficients(fit)[[2]]
      theta3=LogRate=     lrc=lrcStim=coefficients(fit)[[3]]
      xMed=log(2)/(exp(lrcStim))
      PredxMed=predict(fit, data.frame(X = xMed))
      Rate=c=             exp(lrcStim)
      
      # Find root, plot results, print Taylor coefficients and rho estimation:
      bR <-rootxi(LasMedP$X,LasMedP$Y,1,length(LasMedP$X),5,5,plots=F); # bR$froot[2]
      # Find extreme, plot results, print Taylor coefficients and rho estimation:
      cR <-extremexi(LasMedP$X,LasMedP$Y,1,length(LasMedP$X),5,5,plots=F); # c$fextr[2]
      # Find inflection point, plot results, print Taylor coefficients and rho estimation:
      dR <-inflexi(LasMedP$X,LasMedP$Y,1,length(LasMedP$X),5,5,plots=F);   # d$finfl[2]
      
      if (RootG | MaxG | InflG) {
        PredModRoot<-rootxiGraph(LasMedP$X,LasMedP$Y,1,length(LasMedP$X),5,5,TRUE)
        lines(LasMedP$X, PredModRoot, lty=1,lwd=2,cex.lab=1.25,col="red")
        legend(2,yminp+.2, col = c("red"), lty = 1, 
               lwd = 1, legend = c(paste0("Taylor fit (Polynomial Order 5)")), bty = "n", cex = 0.7)
        legend(2,yminp+.25, col = c("black"), lty = 1, 
               lwd = 3, legend = bquote(.("Asymptotic Regression")*~(RSE==.(round(SEMod(fit),4)))), bty = "n", cex = 0.7)
      } else{
        with(LasMedP, lines(X,Y,type="b",pch=16));
      }
      
      # Las Medias al final
      with(LasMedP, points(X,Y,pch=16, cex = 1.45));
      with(LasMedP, points(X,Y,pch=16, col=decilecolors,cex = 1.25));
      
      #log(2)
      if (LineRate) {
        newx <- 1
        pred0 <- data.frame(x=newx, y=AsymRegMM(newx,AsymStim,R0Stim,c))
        pred1 <- data.frame(x=newx, y=DrvAsymReg(newx,AsymStim,R0Stim,c))
        yint <- pred0$y - (pred1$y*newx)
        xint <- -yint/pred1$y
        lines(xPred, yint + pred1$y*xPred, lty=2,lwd=1) # tangent (1st deriv. of spline at newx)
        #points(xint, 0, col=3, pch=19) # x intercept
        LTang=lm(yint + pred1$y*xPred ~ xPred)$coefficients
        ratio=5/1
        Angle = atan(LTang[2] * ratio) * (180 / pi) # Yo estimé 70
        text(2, .75,"Max Rate at x = 1",srt=Angle,adj=c(0.75,0),cex=.90) 
      }
      
      if (pointXMed=="p") points(xMed, PredxMed[[1]],pch=16,cex=1.5,col=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))
      if (pointXMed=="X") points(xMed, PredxMed[[1]],pch=4, cex=1.5,col=rgb(red = 0, green = 1, blue = 1))
      if (pointXMed %in% c("p", "X") & pointXMedLb =="OnAx") {
        lines(c(0,xMed),c(PredxMed[[1]],PredxMed[[1]]),lty=2,lwd=1)
        lines(c(xMed,xMed),c(PredxMed[[1]],-1),lty=2,lwd=1)
        # text(xMed+3, PredxMed,expression(X[0.5]%~~%.(xMed)))
        # text(xMed+1.5, PredxMed,bquote(X[0.5]==.(ceiling(xMed))),cex=1.5)
        text(xMed+.5, PredxMed[[1]],
             quote({x[0.5] %~~%1}), 
             adj = c(0, 0.5),
             cex=1)
        abline(h=AsymStim,lty=2,lwd=1)
        abline(h=R0Stim,lty=2,lwd=1)
        axis(side=2, at = PredxMed[[1]], labels= "",las=1, cex=.75)
        axis(side=1, at = xMed, labels= "",las=1, cex=.75)
        par(xpd=TRUE)
        text(x = -1.3, y= .1,labels=expression(frac(theta[1]+theta[2],2)),adj=c(.5,0),cex=.75)
        text(x = xMed, y= -1.4,labels=expression(frac(log(2), e^{theta[3]})==frac(log(2), Rate)),cex=.75)
        Arrows(-1,.15,-.4,PredxMed[[1]], arr.length = .25, arr.type="triangle", arr.adj = 1)
        Arrows(1,-1.35,1,-1.1, arr.length = .25, arr.type="triangle", arr.adj = 1 )
        par(xpd=FALSE)
        axis(side=2, at = R0Stim, labels= expression(theta[2]), pos=-.5, las=1)
        axis(side=2, at = AsymStim, labels= expression(theta[1]), pos=-.5, las=1)
        # axis(2, at = PredxMed[[1]], labels= expression(frac(theta[1]+theta[2],2)), 
        #                pos=-.25, las=1,cex=2)
      }
      
      if (pointXMed %in% c("p", "X") & pointXMedLb =="OnGrp") {
        lines(c(0,xMed),c(PredxMed[[1]],PredxMed[[1]]),lty=2,lwd=1)
        lines(c(xMed,xMed),c(PredxMed[[1]],-1),lty=2,lwd=1)
        text(xMed+.5, PredxMed[[1]],
             quote({f(x[0.5]) == frac(theta[1]+theta[2],2)}~{} %=>% {}~~
                     {x[0.5] == frac(log(2), e^{theta[3]})}~ {} %=>% {}~~ {x[0.5] %~~%1}), 
             adj = c(0, 0.5),
             cex=1)
        text(x = 0.5, y= PredxMed[[1]],labels=expression(frac(theta[1]+theta[2],2)),cex=.75)
      }
      
      if(!is.null(PIp)) {
        abline(v=PIp,lty=3,lwd=1)
        text(PIp, 1,"IP",cex=1)
      }
      
      if (RootG) {
        abline(v=bR$froot[2],lty=3,lwd=1)
        text(bR$froot[2],1, paste0("Root = ",bR$froot[2]),cex=1)
      }
      if (MaxG) {
        abline(v=cR$fextr[2],lty=3,lwd=1)
        text(cR$fextr[2],1, paste0("Extreme = ",cR$fextr[2]),cex=1)
      }
      if (InflG){
        abline(v=dR$finfl[2],lty=3,lwd=1)
        text(dR$finfl[2],1, paste0("Inflection = ",dR$finfl[2]),cex=1)
      }
      
      NNN.1 <-  AdjParms.LM(fit)
      for(ii in (1:3)) NNN.1[[ii]] <- round(NNN.1[[ii]],2)
      ResM.1= data.table(GenTypeM="E.Mod",ClasType="AsymRegres",
                         Model="SSasymp Orig",as.data.table(NNN.1))
      ResM.2= paste0(paste0(names(unlist(NNN.1[1:3])),"= ",  unlist(NNN.1[1:3]),collapse="; "),"; ",
                     NNN.1[[4]])
      
      ResF <-list(
        Type=NmMod,
        XPred=xPred,
        Model=fit,
        Rsq=round(1 - var(residuals(fit))/var(LasMedP$Y),4),
        SE=SEMod(fit),
        Summ = summary(fit),
        Par= c(coefficients(fit),Rate=Rate,XHalf=xMed,y_Xhalf=(AsymStim+R0Stim)/2),
        forAPA1=ResM.1,
        forAPA2=ResM.2,
        BondAj=AdjParms(fit,LasMedP$Y),
        root = list(an=bR$an, froot =bR$froot),
        extr = list(an =cR$an, extr =cR$fextr),
        inflexi = list(an =dR$an, finfl =dR$finfl)
      )
      ResF
    }
    
    MkKblMod <-function(ModPas,PathPas="Results",EtiqMas="",EqMOd="") {
      Ranv<-SummToTable(ModPas$Summ)
      NewRow=data.table("term"=names(ModPas$Par[4:5]),"Estimate"=as.numeric(frmMM(ModPas$Par[4:5],2)))
      NewRow2=data.table("term"="RSE","Estimate"=as.numeric(frmMM(ModPas$Summ$sigma,4)))
      NewRow3=data.table("term"="R2","Estimate"=as.numeric(frmMM(ModPas$Rsq,4)))
      TablFunde<-rbindlist(list(Ranv$APA, NewRow,NewRow2,NewRow3), fill = TRUE)
      
      options(knitr.kable.NA = '')
      ResTKbl<-TablFunde %>% 
        kbl(caption=paste0("ANOVA Table of ",ModPas$Type),
            digits=4,escape = F, align = "c")  %>%
        kable_paper("hover", full_width = F,font_size = 10) %>%
        kable_styling(fixed_thead = T) %>%
        # footnote(general = "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1") %>%
        footnote(general = c(EqMOd,
                             "Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1")) %>%
        
        scroll_box(width = "100%", height = "800px")
      save_kable(ResTKbl ,paste0(PathPas,"/",EtiqMas,"ANOVA Table of ",ModPas$Type,".html"))
      ResTKbl
    }
  }
  # ▲▲▲------------------- Revised & Checked --------------------▲▲▲

  
  # They need to be debugged
  # Some of them I have rescued from long ago
  # I will test the old code and add improvements to it
 {
   
   nlsGen <- function(FormulP, Formul.NLS, DataP, VInicP) {
     fit <- NULL
     try(fit <- nls(Formul.NLS, data=DataP, trace = TRUE,control = list(maxiter = 500)))
     if(!is.null(fit)){
       res <- fit; print ("Salió a la primera, estimando los valores iniciales")
     } else{
       try(fit <- nls(Formul.NLS, start = VInicP, data=DataP, trace = TRUE,control = list(maxiter = 500)))
       if(!is.null(fit)){
         res <- fit; print ("Salió a la segunda, fijando los valores iniciales")
       } else{
         try(fit <- nls(FormulP, start = VInicP, data=DataP, trace = TRUE,control = list(maxiter = 500)))
         if(!is.null(fit)){
           res <- fit; print ("Salió a la tercera, concretando la fórmula y además estimando los valores iniciales")
         } else{
           try(fit <- nlsLM(Formul.NLS, start = VInicP, data=DataP, trace = TRUE,control = list(maxiter = 500)))
           if(!is.null(fit)){
             res <- fit; print ("Salió a la 4ª, Con estimaciones Librería nlsLM")
           } else{	
             res =NA; print ("No Salió")
           }
         }
       }  
     }
     res
   }
   
   nlsGen2 <- function(FormulP, DataP, VInicP) {
     fit <- NULL
     try(fit <- nls(FormulP, start = VInicP, data=DataP, trace = TRUE,control = list(maxiter = 500)))
     if(!is.null(fit)){
       res <- fit; print ("Salió a la primera")
     } else{
       try(fit <- nlsLM(FormulP, start = VInicP, data=DataP, trace = TRUE,control = list(maxiter = 500)))
       if(!is.null(fit)){
         res <- fit; print ("Salió a la 2ª, Con estimaciones Librería nlsLM")
       } else{	
         res =NA; print ("No Salió")
       }
     }  
     res
   }
   
   # Comprobar que puedo prescindir de todas las que vienen a continuación
   nlsAsym <- function(YMin,YMax,Rate,LasMed) {
     ### Regresión Asintótica
     VInic=list(Asym = YMin, R0 = YMax, lrc = log(-Rate,base = exp(1)))  
     fit <- NULL
     try(fit <- nls(Y ~ SSasymp(X, Asym, R0, lrc), data=LasMed, trace = TRUE,control = list(maxiter = 500)))
     if(!is.null(fit)){
       res <- fit
       print ("Salió a la primera, estimando los valores iniciales")
     } else{
       try(fit<-nls(Y ~ SSasymp(X, Asym, R0, lrc),start = VInic, data=LasMed, trace = TRUE,control = list(maxiter = 500)))
       if(!is.null(fit)){
         res <- fit
         print ("Salió a la segunda, fijando los valores iniciales")
       } else{
         Asymreg1<-formula(Y~Asym+(R0-Asym)*exp(-exp(lrc)*X))
         try(fit<-nls(Asymreg1, data = LasMed, start = VInic, trace = TRUE,control = list(maxiter = 500)))
         if(!is.null(fit)){
           res <- fit
           print ("Salió a la tercera, concretando la fórmula y además estimando los valores iniciales")
         } else{
           res =NA
           print ("No Salió")
         }
       }  
     }
     res
   }
   
   nlsAsym2 <- function(YMin,YMax,Rate,LasMed) {
     ### Regresión Asintótica
     VInic=list(Asym = YMin, R0 = YMax, lrc = log(Rate,base = exp(1)))  
     fit <- NULL
     try(fit <- nls(Y ~ SSasymp(X, Asym, R0, lrc), data=LasMed, trace = TRUE,control = list(maxiter = 500)))
     if(!is.null(fit)){
       res <- fit
       print ("Salió a la primera, estimando los valores iniciales")
     } else{
       try(fit<-nls(Y ~ SSasymp(X, Asym, R0, lrc),start = VInic, data=LasMed, trace = TRUE,control = list(maxiter = 500)))
       if(!is.null(fit)){
         res <- fit
         print ("Salió a la segunda, fijando los valores iniciales")
       } else{
         Asymreg1<-formula(Y~Asym+(R0-Asym)*exp(-exp(lrc)*X))
         try(fit<-nls(Asymreg1, data = LasMed, start = VInic, trace = TRUE,control = list(maxiter = 500)))
         if(!is.null(fit)){
           res <- fit
           print ("Salió a la tercera, concretando la fórmula y además estimando los valores iniciales")
         } else{
           res =NA
           print ("No Salió")
         }
       }  
     }
     res
   }
   
   nlsExp1 <- function(YMax,Rate,LasMed) {
     ### Regresión Asintótica
     VInic=list(b0 = YMax, b1 = Rate)
     fit <- NULL
     ExpMod1<-formula(Y~ I(exp(1)^(b0 + b1 * X)))
     try(fit<-nls(ExpMod1, data = LasMed, start = VInic, trace = TRUE,control = list(maxiter = 500)))
     if(!is.null(fit)){
       res <- fit
       print ("Salió a la primera, estimando los valores iniciales")
     } else{      
       res =NA
       print ("No Salió")
     }
     res
   }
   
   nlsExp2 <- function(YMax,Rate,LasMed) {
     ### Regresión Asintótica
     VInic=list(b0 = YMax, b1 = Rate)
     fit <- NULL
     ExpMod1<-formula(Y ~ b0*exp(b1*X))
     try(fit<-nls(ExpMod1, data = LasMed, start = VInic, trace = TRUE,control = list(maxiter = 500)))
     if(!is.null(fit)){
       res <- fit
       print ("Salió a la primera, estimando los valores iniciales")
     } else{      
       res =NA
       print ("No Salió")
     }
     res
   }
   
   nlsExp3 <- function(YMax,Rate,LasMed) {
     ### Regresión Asintótica
     VInic=list(b1 = Rate)
     fit <- NULL
     ExpMod1<-formula(Y ~ exp(b1*X))
     try(fit<-nls(ExpMod1, data = LasMed, start = VInic, trace = TRUE,control = list(maxiter = 500)))
     if(!is.null(fit)){
       res <- fit
       print ("Salió a la primera, estimando los valores iniciales")
     } else{      
       res =NA
       print ("No Salió")
     }
     res
   }
   
   nlsExp4 <- function(YMin,YMax,Rate,LasMed) {
     ### Regresión Asintótica
     VInic=list(b0 = YMin, b1 = Rate, b2 = YMax)
     fit <- NULL
     ExpMod1<-formula(Y ~ b0*(1-exp(b1*(X-b2))))
     try(fit<-nls(ExpMod1, data = LasMed, start = VInic, trace = TRUE,control = list(maxiter = 500)))
     if(!is.null(fit)){
       res <- fit
       print ("Salió a la primera, estimando los valores iniciales")
     } else{      
       res =NA
       print ("No Salió")
     }
     res
   }
   
   nlsWeib <- function(AsymP, DropP, lrcP, pwrP,LasMed) {
     ### Weibull
     VInic=list(Asym = AsymP, Drop = DropP, lrc = lrcP, pwr=pwrP)
     fit <- NULL
     try(fit <- nls(Y ~ SSweibull(X, Asym, Drop, lrc, pwr), data=LasMed, trace = TRUE,control = list(maxiter = 500)))
     if(!is.null(fit)){
       res <- fit
       print ("Salió a la primera, estimando los valores iniciales")
     } else{
       try(fit<-nls(Y ~ SSweibull(X, Asym, Drop, lrc, pwr),start = VInic, data=LasMed, trace = TRUE,control = list(maxiter = 500)))
       if(!is.null(fit)){
         res <- fit
         print ("Salió a la segunda, fijando los valores iniciales")
       } else{
         AsymWeib1<-formula(Y~Asym-Drop*exp(-exp(lrc)*X^pwr))
         #Asymreg1<-formula(Y~Asym+(R0-Asym)*exp(-exp(lrc)*X))
         try(fit<-nls(AsymWeib1, data = LasMed, start = VInic, trace = TRUE,control = list(maxiter = 500)))
         if(!is.null(fit)){
           res <- fit
           print ("Salió a la tercera, concretando la fórmula y además estimando los valores iniciales")
         } else{
           res =NA
           print ("No Salió")
         }
       }  
     }
     res
   }
   
   
   
   
 }
  
  # ▼▼▼------------------- Main Function  --------------------▼▼▼
  
  # DTPas = DTPas;ModSel =  "SSasymp Original"
  MkAllMod <-function(DTPas, ModSel="SSasymp Original") {
    
    # Very important: load the data at the beginning
    DTPas<-data.table(DTPas)
    
    # drc
    R.RegModel=ProcMod(DTPas,RegModel,NmRegModel,"E.Mod","AsymRegres")
    R.MMModel=ProcMod(DTPas,MMModel,NmMModel,"E.Mod","MM")
    R.LogModel=ProcMod(DTPas,LogModel,NmLogModel,"S.Mod","Log")
    R.GModel=ProcMod(DTPas,GModel,NmGModel,"S.Mod","Gompertz")
    R.WModel=ProcMod(DTPas,WModel,NmWModel,"S.Mod","Weibull")
    R.BCModel=ProcMod(DTPas,BCModel,NmBCModel,"M.Mod","Brain-Cousens")
    R.CRSModel=ProcMod(DTPas,CRSModel,NmCRSModel,"M.Mod","CRS")
    R.BragModel=ProcMod(DTPas,BragModel,NmBragModel,"M.Mod","Bragg")
    R.SpModel=ProcMod(DTPas,SpModel,NmSpModel,"M.Mod","RareMod")
    AllMod = rbind(R.RegModel, R.MMModel,
                   R.LogModel, R.GModel, R.WModel,
                   R.BCModel, R.CRSModel, R.BragModel, R.SpModel)
    AllMod<-AllMod[!is.na(Model)]
    NulModelsGlob= AllModels.Nm[AllModels.Nm %ni%  AllMod$Model]
    
    AllModSor<-copy(AllMod)
    setorder(AllModSor, cols = "AIC")
  
    # nls
    R.RegModel.nls=ProcMod3(DTPas,RegModel.nls,NmRegModel.nls,"E.Mod","AsymRegres")
    R.MMModel.nls=ProcMod3(DTPas,MMModel.nls,NmMModel.nls,"E.Mod","MM")
    R.LogModel.nls=ProcMod3(DTPas,LogModel.nls,NmLogModel.nls,"S.Mod","Log")
    R.GModel.nls=ProcMod3(DTPas,GModel.nls,NmGModel.nls,"S.Mod","Gompertz")
    R.WModel.nls=ProcMod3(DTPas,WModel.nls,NmWModel.nls,"S.Mod","Weibull")
    R.BCModel.nls=ProcMod3(DTPas,BCModel.nls,NmBCModel.nls,"M.Mod","Brain-Cousens")
    R.CRSModel.nls=ProcMod3(DTPas,CRSModel.nls,NmCRSModel.nls,"M.Mod","CRS")
    R.BragModel.nls=ProcMod3(DTPas,BragModel.nls,NmBragModel.nls,"M.Mod","Bragg")
    R.SpModel.nls=ProcMod3(DTPas,SpModel.nls,NmSpModel.nls,"M.Mod","RareMod")
    AllMod.nls = rbind(R.RegModel.nls, R.MMModel.nls,
                       R.LogModel.nls, R.GModel.nls, R.WModel.nls,
                       R.BCModel.nls, R.CRSModel.nls, R.BragModel.nls, R.SpModel.nls)
    AllMod.nls<-AllMod.nls[!is.na(Model)]
    NulModelsGlob.nls= AllModels.Nm.nls[AllModels.Nm.nls %ni%  AllMod.nls$Model]
    
    # Combine both drc & nls
    AllModJn<-rbind(AllMod,AllMod.nls)
    NulModelsGlobJn <- c(NulModelsGlob, NulModelsGlob.nls)
    
    # To obtain them automatically
    DTPas<-data.table(Dose=1,DTPas)
    
    MMa<-mclapply (labels(NLM.MM), function(ii) {
      achk<-NULL;
      try(achk<-ExtrMiModF(ii,'NLM.MM',DTPas));
      achk}, mc.cores = parallel::detectCores(), mc.allow.recursive = TRUE)
    
    NulModels= labels(NLM.MM)[which(unlist(Map(is.null, MMa)))]
    MMa<- data.table(do.call("rbind", MMa)) 
    
    AllMod2<-rbind(AllModJn,MMa) # 247
    AllModSor2<-copy(AllMod2)
    setorder(AllModSor2, cols = "AIC")
    
    AllModSor2<-AllModSor2 %>% 
      mutate_at(c("GenTypeM", "ClasType", "Model"), factor) 
    
    AllModF<-data.table(cbind(Num=c(1:nrow(AllModSor2)),AllModSor2))
    TxtNote<-c("Orange: Reference Model; Red: E-Models; Green: S-Models; Blue: M-Models\n",
               "Unestimated models: ",
               NulModelsGlobJn,NulModels)
    
    PosModSel= percent( AllModF[Model==ModSel,Num]/nrow(AllModF))
    TxtNote2<-c(TxtNote, paste0("Position of the selected model: ",PosModSel))
    kblAll<-kableTablCol(AllModF,"SSasymp Original",NoteFor = TxtNote2 )
    kblAll
    
    # AllGrp<- lapply(1:nrow(AllModF), function(x){ SwtTypMod(idx = x, type = AllModF[x,Algor], dtp = DTPas)})
    
    ResMk <-list(
      AllModF = AllModF,
      kblAll = kblAll #,
      #AllGrp =AllGrp
    )
    ResMk
  }
  # ▲▲▲-------------------  Main Function --------------------▲▲▲
}
# ▲▲▲======================== Adj Func ========================▲▲▲
    

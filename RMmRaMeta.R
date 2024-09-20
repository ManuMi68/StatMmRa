

# Funciones de apoyo al meta-análisis
# (C) Manuel Miguel Ramos Álvarez
# Septiembre 2024

# Las he ido incorporando a la librería general RMmRaGen.R, especialmente en 2024
frmMM<-function(x,d=2) formatC(round(x,d),d,format="f")

percent <- function(num, digits = 2, ...) {
  # To compute percentage
  percentage <-formatC(num * 100, format = "f", digits = digits, ...)
  # appending "%" symbol at the end of
  # calculate percentage value
  paste0(percentage, "%")}

# Estaba en RMmRaGen.R kableTabl
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

# Para Meta-Analysis
ProcFreq<-function(DT,Extens,Crit=.90,NmFic="ResTable4_Alta") {
  Res<-list()
  LasVar<- names(DT)[Extens]
  Ntt=length(DT$ID)
  DTf<-data.table(names=LasVar,FreqPond=DT[,colSums(N*DT[,..Extens],na.rm = T)],
                  Freq=DT[,colSums(DT[,..Extens],na.rm = T)],
                  Perc=round(DT[,colSums(DT[,..Extens],na.rm = T)]/Ntt,4)*100)
  DTf[,Sign:=apply(as.matrix(DTf$Freq),1,
                   function(x) round(prop.test(x , n =Ntt, p = 0.01,alternative = "greater")$p.value,4))]
  setorder(DTf, -FreqPond)
  Res$Sup<-DTf[FreqPond>quantile(FreqPond,Crit)&Perc>quantile(Perc,Crit)&Sign<0.01]
  Res$Vars<-Res$Sup$names
  write.csv2(x =  DTf, file=paste0(NmFic,".csv"),fileEncoding = "UTF-8")
  write.xlsx(DTf, file = paste0(NmFic,".xlsx"), sheetName = "ResTable", append = FALSE)
  Res
}

TandP <-function(LaT, LaP) {
  LaT2=LaT
  if (!is.na(LaP)) {
    if (LaP<=0.01) LaT2=paste0(LaT2,"**")
    if (LaP<=0.05&LaP>0.01) LaT2=paste0(LaT2,"*")
  }
  LaT2
}

GrpDot<-function(FracSelp,qt1,qt2) {
  Valo<-FracSelp$Perc
  NVal<-length(Valo)
  Colores<-c(rep("grey40",NVal))
  Colores[FracSelp[,Perc>qt1&FreqPond>qt2]]<-"red"
  Colores2<-copy(Colores)
  Colores2[FracSelp[,FreqPond>qt2]]<-"blue"
  
  ggVarios<-ggplot(FracSelp, aes(x = Perc, y=forcats::fct_reorder(names,names,.desc = T))) +
    geom_point(size=4, color=Colores) +
    theme_ipsum(
      plot_title_face = "bold",
      plot_title_margin = 1) +
    theme(
      text=element_text(family="Arial Narrow"),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text = element_text( size=48 ),
      axis.text.y = element_text(size=12)) +
    xlim(0,100) +
    geom_text_repel(aes(label = round(Perc,1)),
                    size = 3,position = position_dodge(0.7),color=Colores)+
    annotate("text",x=100,y=c(1:NVal),label=rev(FracSelp$FreqPond),
             color=rev(Colores2),size = 3) +
    ylab("Biomarcador") + xlab("Porcentaje de estudios que reportan el Bimoarcador") + ggtitle("")+
    geom_vline(xintercept=qt1, linetype="dashed") +
    annotate("text",x=qt1+3,y=NVal-.5,label=expression(P[95]))
  ggVarios
}

ExtractEstad<- function(ResMetaP) {
  ResMasP<-data.frame(matrix(NA,nrow=1,ncol=11))
  names(ResMasP)<-c("k","Dif","se","z","p(z)","95%CI","Q","p(Q)","t2","I2","H2")
  ResPrv<- with(ResMetaP, c(k,round(beta,2), round(se,2), TandP(round(zval,2), pval), frmMM(pval,4), 
                            paste(round(ci.lb,2), round(ci.ub,2),sep="; "),
                            TandP(round(QE,2), QEp), frmMM(QEp,4), round(tau2,2), round(I2,2), round(H2,2)))
  ResMasP[1,] = rbind(ResPrv)
  BiasC<-list()
  BiasC<-summary(lm(c(ResMetaP$yi)/sqrt(c(ResMetaP$vi)) ~ 1/sqrt(c(ResMetaP$vi))))
  
  ResMasP$BiasMM = TandP(round(BiasC$coefficients[1,3],2), BiasC$coefficients[1,4])
  ResMasP$pBiasMM = frmMM(BiasC$coefficients[1,4],4)
  ResMasP$Half = fsn(ResMetaP$yi, ResMetaP$vi)$fsnum
  ResMasP$pHalf = frmMM(fsn(ResMetaP$yi, ResMetaP$vi)$pval,4)
  ResMasP$NTT=sum(ResMetaP$ni)
  ResMasP
}

#ResMetaP=m.gen
ExtractEstad2<- function(ResMetaP) {
  ResMasP<-data.frame(matrix(NA,nrow=1,ncol=11))
  names(ResMasP)<-c("k","Dif","se","z","p(z)","95%CI","Q","p(Q)","t2","I2","H2")
  ResPrv<- with(ResMetaP, c(k,round(TE.random,2), round(seTE.random,2), TandP(round(statistic.random,2), pval.random), frmMM(pval.random,4), 
                            paste(round(lower.random,2), round(upper.random,2),sep="; "),
                            TandP(round(Q,2), pval.Q), frmMM(pval.Q,4), round(tau2,2), percent(I2), round(H^2,2)))
  ResMasP[1,] = rbind(ResPrv)
  #ResMasP$I2<-percent(ResMasP$I2)
  #ResMasP$z2=TandP(ResMasP$z, ResMetaP$pval.random)
  #ResMasP$Q2=TandP(ResMasP$Q, ResMetaP$pval.Q)
  #ResMasP[1,5]<-frmMM(ResMetaP$pval.random,4); ResMasP[1,9]<-frmMM(ResMetaP$pval.Q,4)
  #ResMasP$"95%CI" = paste(ResMasP$CI.i, ResMasP$CI.s,sep="; ")
  
  BiasC<-list()
  BiasC<-summary(lm(c(ResMetaP$TE)/c(ResMetaP$seTE) ~ 1/(c(ResMetaP$seTE))))
  
  ResMasP$BiasMM = TandP(round(BiasC$coefficients[1,3],2), BiasC$coefficients[1,4])
  ResMasP$pBiasMM = frmMM(BiasC$coefficients[1,4],4)
  ResMasP$Half = fsn(ResMetaP$TE, ResMetaP$seTE^2)$fsnum
  ResMasP$pHalf = frmMM(fsn(ResMetaP$TE, ResMetaP$seTE^2)$pval,4)
  if (is.null(ResMetaP$exclude)) ResMasP$NTT=sum(ResMetaP$data$n.e)+sum(ResMetaP$data$n.c)
  if (!is.null(ResMetaP$exclude)) ResMasP$NTT=sum(ResMetaP$data$n.e[!ResMetaP$exclude])+sum(ResMetaP$data$n.c[!ResMetaP$exclude])
  ResMasP    
}

ExtractEstad3n<- function(ResMetaP) {
  ResMasP <- with(ResMetaP, data.table(Var=subgroup.name,Levels=subgroup.levels,k=k.w, TE=round(TE.random.w,2), 
                                       se=round(seTE.random.w,2), z=mapply(TandP, round(zval.random.w,2),pval.random.w),
                                       "p(z)"=round(pval.random.w,4), "95%-CI"=paste0("[", frmMM(lower.random.w,2),",", frmMM(upper.random.w,2),"]"),
                                       Q=mapply(TandP, round(Q.w,2),pval.Q.w),
                                       "p(Q)"=round(pval.Q.w,4),t2=round(tau2.w,2), I2=round(I2.w,2), H2=round(H.w^2,2)))
  ResMasP
}

ExtractEstad4<- function(ResMetaP) {
  ResMasP <- with(ResMetaP, paste0("QM","(",paste(QMdf,collapse =","),") = ", round(QM,2),", p = ",frmMM(QMp,4)))
  ResMasP    
}

ExtractEstad4.b<- function(ResMetaP) {
  ResMasP<-data.frame(matrix(NA,nrow=1,ncol=3))
  names(ResMasP)<-c("QM","dg","p(QM)")
  ResMasP <- with(ResMetaP, cbind('Moder.QM'=round(QM,2), 'Moder.df'=paste(QMdf,collapse =","), 'Moder.p(QM)'=frmMM(QMp,4)))
  ResMasP    
}

#"Moder.QM","Moder.df","Moder.p(QM)","Grp.QM","Grp.df","Grp.p(QM)")
ExtractEstad5<- function(ResMetaP) {
  ResMasP <- with(ResMetaP, paste0("QM","(",paste(df.Q.b.random,collapse =","),") = ", round(Q.b.random,2),", p = ",frmMM(pval.Q.b.random,4)))
  ResMasP    
}

ExtractEstad5.b<- function(ResMetaP) {
  ResMasP<-data.frame(matrix(NA,nrow=1,ncol=3))
  names(ResMasP)<-c("QM","dg","p(QM)")
  ResMasP <- with(ResMetaP, cbind('Grp.QM'=round(Q.b.random,2), 'Grp.df'=paste(df.Q.b.random,collapse =","), 'Grp.p(QM)'=frmMM(pval.Q.b.random,4)))
  ResMasP    
}

AdaptFor<-function(ModelPas,VarPas,widthP = 15, heightP = 35) {
  pdf(file = paste0(VarPas,"_forestplot_metacon.pdf"), width = widthP, height = heightP,family = "ArialMT")
  forest(ModelPas,
         sortvar = TE, random=T, text.random="Total (Random Model)",
         leftlabs = c("Estudio","N","Experimental\nMedia","SD","N","Control\nMedia","SD"),
         leftcols = c("Author","n.e","mean.e","sd.e","n.c","mean.c","sd.c"),
         just = "center", just.addcols = "right", just.studlab = "right",
         fs.study = 10, 
         fs.study.label = 10, ff.study.label = "bold",
         #ff.fixed = "plain", ff.hetstat = "plain",
         col.diamond = "green", col.diamond.lines = "black",
         smlab="Diferencia Medias",
         rightcols = c("effect.ci"),spacing=1,
         plotwidth="10cm",
         allstudies = F)
  dev.off()
}

#V1p = update.meta(m.gen, subgroup = get(LasModu[xx]), subgroup.name=(LasModu[xx]), tau.common = FALSE)
#V1bp = metareg(m.gen, LasModu[xx])
#FilOutp = FilOut,hp = Vhp[xx],wp = Vwp[xx],ap = VAp[xx],addT = Vet[xx],LasModu[xx]
ModerMeta<-function(V1p,V1bp,FilOutp=FilOut,hp=15,wp=35,ap=T,addT="",LaModer=V1p$subgroup.name) {
  #if (is.null(LaModerP)) LaModer=V1p$subgroup.name
  #if (!is.null(LaModerP)) LaModer=LaModerP
  ResSgrpInt<-list()
  ResSgrpInt[[1]]<-ExtractEstad3n(V1p)
  ResSgrpInt[[2]]<-paste0("Group Differences Test: ",ExtractEstad5(V1p), "; Moderator Effect Test: ", ExtractEstad4(V1bp))
  ResSgrpInt[[3]]<-data.table(Var=LaModer,ExtractEstad4.b(V1bp), ExtractEstad5.b(V1p))
  sink(file = FilOutp,append = ap); cat(paste0(LaModer,addT,"\n")); print(V1p);cat("\n\nMetaReg\n"); print(V1bp); cat("====================\n"); sink()
  AdaptFor(V1p,paste0(LaModer,addT),hp,wp)
  ResSgrpInt
}



#' Find Statistical Outliers in a Meta-Analysis
#'
#' Searches for statistical outliers in meta-analysis results generated by \code{\link[meta]{meta}} functions or the
#' \code{\link[metafor]{rma.uni}} in the \code{metafor} package.
#'
#' @usage find.outliers(x, ...)
#'
#' @param x Either (1) an object of class \code{meta}, generated by the \code{metabin}, \code{metagen},
#' \code{metacont}, \code{metacor}, \code{metainc}, \code{metarate} or \code{metaprop} function; or (2)
#' and object of class \code{rma.uni} created with the \code{\link[metafor]{rma.uni}} function in \code{metafor}.
#' @param ... Additional parameters for the \code{\link[metafor]{rma.uni}} or \code{\link[meta]{update.meta}} function.
#'
#' @details
#' This function searches for outlying studies in a meta-analysis results object. Studies are defined as outliers when
#' their 95\% confidence interval lies ouside the 95\% confidence interval of the pooled effect.
#'
#' When outliers are found, the function automatically recalculates the meta-analysis results, using the same settings as
#' in the object provided in \code{x}, but excluding the detected outliers.
#'
#' A forest plot of the meta-analysis with outliers removed can be generated directly by plugging the output of the function into
#' the \code{forest} function.
#'
#' @references Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
#' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803. \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/detecting-outliers-influential-cases.html}{Chapter 6.2}
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @return
#' Returns the identified outliers and the meta-analysis results when the outliers are removed.
#'
#' If the provided meta-analysis object is of class \code{meta}, the following objects are returned if the
#' results of the function are saved to another object:
#' \itemize{
#' \item \code{out.study.fixed}: A numeric vector containing the names of the outlying studies when
#' assuming a fixed-effect model.
#' \item \code{out.study.random}: A numeric vector containing the names of the outlying studies when
#' assuming a random-effects model. The \eqn{\tau^{2}} estimator \code{method.tau} is inherited from \code{x}.
#' \item \code{m.fixed}: An object of class \code{meta} containing the results of the meta-analysis with outliers
#' removed (assuming a fixed-effect model).
#' \item \code{m.random}: An object of class \code{meta} containing the results of the meta-analysis with outliers
#' removed (assuming a random-effects model, and using the same \code{method.tau} as in the original analysis).
#'}
#'
#' If the provided meta-analysis object is of class \code{rma.uni}, the following objects are returned if the
#' results of the function are saved to another object:
#' \itemize{
#' \item \code{out.study}: A numeric vector containing the names of the outlying studies.
#' \item \code{m}: An object of class \code{rma.uni} containing the results of the meta-analysis with outliers
#' removed (using the same settings as in the meta-analysis object provided).
#'}
#' @importFrom metafor rma.uni
#' @importFrom meta update.meta
#'
#' @export find.outliers
#'
#' @aliases spot.outliers.random spot.outliers.fixed spot.outliers
#'
#' @seealso \code{\link[metafor]{influence.rma.uni}}, \code{\link[meta]{metainf}}, \code{\link[meta]{baujat}}
#'
#' @examples
#' suppressPackageStartupMessages(library(meta))
#' suppressPackageStartupMessages(library(metafor))
#' suppressPackageStartupMessages(library(dmetar))
#'
#' # Pool with meta
#' m1 <- metagen(TE, seTE, data = ThirdWave,
#'               studlab = ThirdWave$Author, common = FALSE)
#'
#' # Pool with metafor
#' m2 <- rma(yi = TE, sei = seTE, data = ThirdWave,
#'           slab = ThirdWave$Author, method = "PM")
#'
#' # Find outliers
#' fo1 <- find.outliers(m1)
#' fo2 <- find.outliers(m2)
#'
#' # Show summary
#' summary(fo1)
#' summary(fo2)
#'
#' \dontrun{
#' # Make forest plot
#' # Pass additional arguments from meta & metafor's forest function
#' forest(fo1, prediction = TRUE)
#' forest(fo2, cex = .8, col = "lightblue")
#' }


find.outliers = spot.outliers.random = spot.outliers.fixed = function(x, ...){
  
  
  if (class(x)[1] %in% c("rma.uni", "rma")){
    
    token = "metafor"
    
    # Generate lower/upper for all effects
    lower = as.numeric(x$yi - 1.96*sqrt(x$vi))
    upper = as.numeric(x$yi + 1.96*sqrt(x$vi))
    
    # Select outliers
    mask = upper < x$ci.lb | lower > x$ci.ub
    dat = data.frame("yi" = x$yi[!mask],
                     "vi" = x$vi[!mask],
                     "studlab" = as.character(x$slab[!mask]))
    out.study = x$slab[mask]
    
    # Update metafor model
    method.tau = x$method
    m = metafor::rma.uni(dat$yi, vi = dat$vi, method = method.tau, slab = dat$studlab, ...)
    
    if (length(out.study) < 1){
      
      
      tau.token = "metafor.null"
      cat(paste0("No outliers detected (", method.tau,")."))
      out.study = NULL
      
    } else {
      
      tau.token = "metafor"
      
    }
    
  }
  
  if (class(x)[1] %in% c("metagen", "metapropr",
                         "metacor", "metainc", "metacont",
                         "metaprop", "metabin", "metabin")){
    
    token = "meta"
    
    # Control for objects with NAs in study data
    if (anyNA(x$TE) | anyNA(x$seTE)){
      warning("Studies with NAs not considered in outlier analysis.")
      
    }
    
    
    if (class(x)[1] == "metaprop"){
      
      lower = x$TE - 1.96*x$seTE
      upper = x$TE + 1.96*x$seTE
      
      # Generate mask with outliers (fixed/random)
      mask.fixed =
        (!is.na(upper) & upper < x$lower.fixed) |
        (!is.na(lower) & lower > x$upper.fixed)
      mask.random =
        (!is.na(upper) & upper < x$lower.random) |
        (!is.na(lower) & lower > x$upper.random)
      
    } else {
      
      # Generate mask with outliers (fixed/random)
      mask.fixed =
        (!is.na(x$upper) & x$upper < x$lower.fixed) |
        (!is.na(x$lower) & x$lower > x$upper.fixed)
      mask.random =
        (!is.na(x$upper) & x$upper < x$lower.random) |
        (!is.na(x$lower) & x$lower > x$upper.random)
      
    }
    
    # Update meta-analysis with outliers removed
    m.fixed = update.meta(x, exclude = mask.fixed, ...)
    m.random = update.meta(x, exclude = mask.random, ...)
    
    # Select names of outlying studies
    out.study.fixed = x$studlab[mask.fixed]
    out.study.random = x$studlab[mask.random]
    
    if (x$common == TRUE & x$random == FALSE){
      
      if (length(out.study.fixed) < 1){
        
        tau.token = "null.ftrf"
        out.study.fixed = NULL
        
      } else {
        
        tau.token = "ftrf"
        
      }
      
    }
    
    if (x$common == FALSE & x$random == TRUE){
      
      if (length(out.study.random) < 1){
        
        tau.token = "null.ffrt"
        out.study.random = NULL
        
      } else {
        
        tau.token = "ffrt"
        
      }
      
    }
    
    if (x$common == TRUE & x$random == TRUE){
      
      if (length(out.study.fixed) < 1 & length(out.study.random) < 1){
        
        out.study.fixed = NULL
        out.study.random = NULL
        tau.token = "null.ftrt"
        
      } else {
        
        if (length(out.study.fixed) < 1){out.study.fixed = NULL}
        if (length(out.study.random) < 1){out.study.random = NULL}
        
        tau.token = "ftrt"
        
      }
      
    }
    
  }
  
  if (!class(x)[1] %in% c("rma.uni", "rma", "metacont",
                          "metagen", "metapropr",
                          "metacor", "metainc",
                          "metaprop", "metabin", "metabin")){
    
    message("Input must be of class 'meta' or 'rma.uni'")
    
  }
  
  if (token == "metafor"){
    
    returnlist = list("out.study" = out.study,
                      "m" = m)
    
    if (tau.token == "metafor"){class(returnlist) = c("find.outliers", "mf", method.tau)}
    if (tau.token == "metafor.null"){class(returnlist) = c("find.outliers", "mf.null", method.tau)}
    
    # Return
    invisible(returnlist)
    
    returnlist
    
  } else {
    
    
    returnlist = list("out.study.fixed" = out.study.fixed,
                      "out.study.random" = out.study.random,
                      "m.fixed" = m.fixed,
                      "m.random" = m.random)
    
    # Set classes
    if (tau.token == "ftrf"){class(returnlist) = c("find.outliers", "ftrf")}
    if (tau.token == "ffrt"){class(returnlist) = c("find.outliers", "ffrt")}
    if (tau.token == "ftrt"){class(returnlist) = c("find.outliers", "ftrt")}
    if (tau.token == "null.ftrf"){class(returnlist) = c("find.outliers", "null.ftrf")}
    if (tau.token == "null.ffrt"){class(returnlist) = c("find.outliers", "null.ffrt")}
    if (tau.token == "null.ftrt"){class(returnlist) = c("find.outliers", "null.ftrt")}
    
    # Return
    invisible(returnlist)
    
    returnlist
    
  }
  
}














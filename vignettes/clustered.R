## ---- echo = FALSE, message = FALSE-------------------------------------------
library(simstudy)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(survival)
library(gee)
library(data.table)

plotcolors <- c("#B84226", "#1B8445", "#1C5974")

cbbPalette <- c("#B84226","#B88F26", "#A5B435", "#1B8446",
                "#B87326","#B8A526", "#6CA723", "#1C5974") 

ggtheme <- function(panelback = "white") {
  
  ggplot2::theme(
    panel.background = element_rect(fill = panelback),
    panel.grid = element_blank(),
    axis.ticks =  element_line(colour = "black"),
    panel.spacing =unit(0.25, "lines"),  # requires package grid
    panel.border = element_rect(fill = NA, colour="gray90"), 
    plot.title = element_text(size = 8,vjust=.5,hjust=0),
    axis.text = element_text(size=8),
    axis.title = element_text(size = 8)
  )  
  
}


## ---- tidy = TRUE-------------------------------------------------------------
gen.school <- defData(varname="s0", dist = "normal", 
                      formula = 0, variance = 3, id = "idSchool"
)
gen.school <- defData(gen.school, varname = "nClasses", 
                      dist = "noZeroPoisson", formula = 3
)

set.seed(282721)

dtSchool <- genData(8, gen.school)
dtSchool <- trtAssign(dtSchool, n = 2)

dtSchool


## ---- tidy = TRUE-------------------------------------------------------------
gen.class <- defDataAdd(varname = "c0", dist = "normal", formula = 0, 
                     variance = 2)
gen.class <- defDataAdd(gen.class, varname = "nStudents", dist = "noZeroPoisson", formula = 20
)

dtClass <- genCluster(dtSchool, "idSchool", numIndsVar = "nClasses",level1ID = "idClass")
dtClass <- addColumns(gen.class, dtClass)

head(dtClass, 10)

## ---- tidy = TRUE, tidy.opts= list(width.cutoff = 60)-------------------------
gen.student <- defDataAdd(varname="Male", dist="binary", formula=0.5)
gen.student <- defDataAdd(gen.student, varname="age", dist = "uniform", formula="9.5; 10.5")
gen.student <- defDataAdd(gen.student, varname="test", dist = "normal",
                       formula = "50 - 5*Male + s0 + c0 + 8 * trtGrp",                           variance = 2)
dtStudent <- genCluster(dtClass,cLevelVar="idClass", numIndsVar = "nStudents",                        level1ID = "idChild")

dtStudent <- addColumns(gen.student, dtStudent)

## ---- tidy = TRUE, echo = FALSE, fig.width = 7, fig.height = 3----------------
ggplot(data=dtStudent,aes(x=factor(idClass),y=test,group=idClass)) +
  geom_boxplot(aes(color=factor(trtGrp), fill = factor(idSchool)))+
  xlab("Classes")+
  ylab("Test scores") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_fill_manual(values = cbbPalette, guide = FALSE) +
  scale_color_manual(values = c("grey80", "#000000"),
                    labels = c("Ctrl", "Rx"),
                    guide = guide_legend(title = NULL,
                                         override.aes = list(shape = 1,
                                                             keywidth=8
                                                             )
                                         )
                    ) +
  theme(legend.key=element_rect(fill=NA)) +
  ggtheme()


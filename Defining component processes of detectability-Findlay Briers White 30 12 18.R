#Defining component processes of detection probability in camera-trap studies 
#to understand occurrence of false-negatives

#Findlay, M., Briers,R.,White, P.

#code should be run in the same folders as data files

require(lme4)
require(MuMIn)

########################################
#Fig 4 TRIGGER PROBABILITY MODELS P(trigger|pass)
########################################
#(a) badger with Bushnell CT on video setting 
#CLEAR OBJECTS if necessary#
rm(list=ls())

#Import data TRIGGER_FOX_BADGER.csv
Badger.trig<-read.csv("TRIGGER_FOX_BADGER.csv")

#select BADGER observations
Badger.trig<-Badger.trig[Badger.trig$SPECIES=="BADGER",]

#Create new variable called GAIT.1 from GAIT 
#GAIT has three levels, walk, trot and run. 
#combine run and trot into one level "runtrot", new variable will be renamed GAIT.1
GAIT<-Badger.trig$GAIT
levels(GAIT)
levels(GAIT)<-c("RUNTROT","RUNTROT","W")
Badger.trig<-data.frame(Badger.trig,GAIT)

#select all lateral passes
Badger.trig<-Badger.trig[Badger.trig$ORIENT=="L",]

#get rid of records with na values
Badger.trig<-na.omit(Badger.trig)

#create models
null.model<-glmer(TRIGGER~(1|CT.POS)+1,data=Badger.trig,family=binomial)
model1<-glmer(TRIGGER~(1|CT.POS)+DIST,data=Badger.trig,family=binomial)
model2<-glmer(TRIGGER~(1|CT.POS)+GAIT.1,data=Badger.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model3<-glmer(TRIGGER~(1|CT.POS)+LOIT,data=Badger.trig,family=binomial)
model4<-glmer(TRIGGER~(1|CT.POS)+AIR,data=Badger.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model5<-glmer(TRIGGER~(1|CT.POS)+GAIT.1+DIST,data=Badger.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model6<-glmer(TRIGGER~(1|CT.POS)+GAIT.1*DIST,data=Badger.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model7<-glmer(TRIGGER~(1|CT.POS)+AIR+DIST,data=Badger.trig,family=binomial)
model8<-glmer(TRIGGER~(1|CT.POS)+GAIT.1+AIR,data=Badger.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#create model selection table
BADGER.TRIGGER.MODELS<-model.sel(null.model,model1,model2,model3,model4,model5,model6,model7,model8,rank=AIC)

#view table
BADGER.TRIGGER.MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#fig 4b FOX with Bushnell CT on video setting

#CLEAR OBJECTS if necessary#
rm(list=ls())

#Import TRIGGER_FOX_BADGER.csv
Fox.trig<-read.csv("TRIGGER_FOX_BADGER.csv")

#select FOX observations
Fox.trig<-Fox.trig[Fox.trig$SPECIES=="FOX",]

#Create new variable called GAIT.1 from GAIT 
#GAIT has three levels, walk, trot and run. 
#combine run and trot into one level "runtrot", new variable will be renamed GAIT.1
GAIT<-Fox.trig$GAIT
levels(GAIT)
levels(GAIT)<-c("RUNTROT","RUNTROT","W")
Fox.trig<-data.frame(Fox.trig,GAIT)

#select all lateral passes
Fox.trig<-Fox.trig[Fox.trig$ORIENT=="L",]

#get rid of records with na values
Fox.trig<-na.omit(Fox.trig)

#create models
null.model<-glmer(TRIGGER~(1|CT.POS)+1,data=Fox.trig,family=binomial)
model1<-glmer(TRIGGER~(1|CT.POS)+DIST,data=Fox.trig,family=binomial)
model2<-glmer(TRIGGER~(1|CT.POS)+GAIT.1,data=Fox.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model3<-glmer(TRIGGER~(1|CT.POS)+LOIT,data=Fox.trig,family=binomial)
model4<-glmer(TRIGGER~(1|CT.POS)+AIR,data=Fox.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model5<-glmer(TRIGGER~(1|CT.POS)+GAIT.1+DIST,data=Fox.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model6<-glmer(TRIGGER~(1|CT.POS)+GAIT.1*DIST,data=Fox.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model7<-glmer(TRIGGER~(1|CT.POS)+AIR+DIST,data=Fox.trig,family=binomial)
model8<-glmer(TRIGGER~(1|CT.POS)+GAIT.1+AIR,data=Fox.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#create model selection table
FOX.TRIGGER.MODELS<-model.sel(null.model,model1,model2,model3,model4,model5,model6,model7,model8,rank=AIC)

#view table
FOX.TRIGGER.MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#fig 4c Dry otter from holt,Bushnell CT on video setting

#CLEAR OBJECTS if necessary#
rm(list=ls())

#Import TRIGGER_OTTER.csv
Dry.otter.trig<-read.csv("TRIGGER_OTTER.csv")

#Create new variable called GAIT.1 from GAIT 
#GAIT has three levels, walk, trot and run. 
#combine run and trot into one level "runtrot", new variable will be renamed GAIT.1

GAIT<-Dry.otter.trig$GAIT
levels(GAIT)
levels(GAIT)<-c("RUNTROT","RUNTROT","W")
Dry.otter.trig<-data.frame(Dry.otter.trig,GAIT)

##Select BUSHNELL VIDEO 
Dry.otter.trig<-Dry.otter.trig[Dry.otter.trig$CAMERA.ID=="BV",]

#select all dry passes
Dry.otter.trig<-Dry.otter.trig[Dry.otter.trig$FROM.HOLT=="1",]

#select lateral passes
Dry.otter.trig<-Dry.otter.trig[Dry.otter.trig$ORIENT=="L",]

#get rid of records with na values
Dry.otter.trig<-na.omit(Dry.otter.trig)


null.model<-glmer(TRIGGER~(1|CT.POS)+1,data=Dry.otter.trig,family=binomial)
model1<-glmer(TRIGGER~(1|CT.POS)+log(DIST),data=Dry.otter.trig,family=binomial)
model2<-glmer(TRIGGER~(1|CT.POS)+GAIT.1,data=Dry.otter.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model3<-glmer(TRIGGER~(1|CT.POS)+LOIT,data=Dry.otter.trig,family=binomial)
model4<-glmer(TRIGGER~(1|CT.POS)+AIR,data=Dry.otter.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model5<-glmer(TRIGGER~(1|CT.POS)+GAIT.1+log(DIST),data=Dry.otter.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model6<-glmer(TRIGGER~(1|CT.POS)+GAIT.1*log(DIST),data=Dry.otter.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model7<-glmer(TRIGGER~(1|CT.POS)+AIR+log(DIST),data=Dry.otter.trig,family=binomial)
model8<-glmer(TRIGGER~(1|CT.POS)+GAIT.1+AIR,data=Dry.otter.trig,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#create model selection table
Dry.otter.trig.MODELS<-model.sel(null.model,model1,model2,model3,model4,model5,model6,model7,model8,rank=AIC)

#view model selection table
Dry.otter.trig.MODELS

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#fig 4d Dry otter from holt,Acorn CT on video setting

#CLEAR OBJECTS if necessary#
rm(list=ls())

#Import TRIGGER_OTTER.csv
#Dry.otter.trig.Acorn<-read.csv("TRIGGER_OTTER.csv")
Dry.otter.trig.Acorn<-read.csv(file.choose())
#Create new variable called GAIT.1 from GAIT 
#GAIT has three levels, walk, trot and run. 
#combine run and trot into one level "runtrot", new variable will be renamed GAIT.1
GAIT<-Dry.otter.trig.Acorn$GAIT
levels(GAIT)
levels(GAIT)<-c("RUNTROT","RUNTROT","W")
Dry.otter.trig.Acorn<-data.frame(Dry.otter.trig.Acorn,GAIT)

##Select ACORN VIDEO 
Dry.otter.trig.Acorn<-Dry.otter.trig.Acorn[Dry.otter.trig.Acorn$CAMERA.ID=="A",]

#select all dry passes
Dry.otter.trig.Acorn<-Dry.otter.trig.Acorn[Dry.otter.trig.Acorn$FROM.HOLT=="1",]

#select lateral passes
Dry.otter.trig.Acorn<-Dry.otter.trig.Acorn[Dry.otter.trig.Acorn$ORIENT=="L",]

#get rid of records with na values
Dry.otter.trig.Acorn<-na.omit(Dry.otter.trig.Acorn)

#Run models
null.model<-glmer(TRIGGER~(1|CT.POS)+1,data=Dry.otter.trig.Acorn,family=binomial)
model1<-glmer(TRIGGER~(1|CT.POS)+DIST,data=Dry.otter.trig.Acorn,family=binomial)
model2<-glmer(TRIGGER~(1|CT.POS)+GAIT.1,data=Dry.otter.trig.Acorn,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model3<-glmer(TRIGGER~(1|CT.POS)+LOIT,data=Dry.otter.trig.Acorn,family=binomial)
model4<-glmer(TRIGGER~(1|CT.POS)+AIR,data=Dry.otter.trig.Acorn,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model5<-glmer(TRIGGER~(1|CT.POS)+GAIT.1+DIST,data=Dry.otter.trig.Acorn,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model6<-glmer(TRIGGER~(1|CT.POS)+GAIT.1*DIST,data=Dry.otter.trig.Acorn,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model7<-glmer(TRIGGER~(1|CT.POS)+AIR+DIST,data=Dry.otter.trig.Acorn,family=binomial)
model8<-glmer(TRIGGER~(1|CT.POS)+GAIT.1+AIR,data=Dry.otter.trig.Acorn,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#create model selection table
Dry.otter.trig.Acorn.MODELS<-model.sel(null.model,model1,model2,model3,model4,model5,model6,model7,model8,rank=AIC)

#view model selection table
Dry.otter.trig.Acorn.MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
######################################################
#TRIGGER MODELS FIGURE 5.
#Trigger Probability, P (T|P), including the variable WET.DRY
 
#(a) Bushnell video setting

#CLEAR OBJECTS if necessary#
rm(list=ls())
#Import TRIGGER.OTTER.WET.DRY.csv
otter.WD.BUSHNELL<-read.csv("TRIGGER_OTTER_WET.DRY.csv")

##-Select BUSHNELL VIDEO 
otter.WD.BUSHNELL<-otter.WD.BUSHNELL[otter.WD.BUSHNELL$CAMERA.ID=="BV",]

#Create new variable called GAIT.1 from GAIT 
#GAIT has three levels, walk, trot and run. 
#combine run and trot into one level "runtrot", new variable will be renamed GAIT.1
GAIT<-otter.WD.BUSHNELL$GAIT
levels(GAIT)
levels(GAIT)<-c("RUNTROT","RUNTROT","W")
otter.WD.BUSHNELL<-data.frame(otter.WD.BUSHNELL,GAIT)

#select all lateral passes
otter.WD.BUSHNELL<-otter.WD.BUSHNELL[otter.WD.BUSHNELL$ORIENT=="L",]

#get rid of records with na values
otter.WD.BUSHNELL<-na.omit(otter.WD.BUSHNELL)

#Run models
null.model<-glmer(TRIGGER~(1|CT.POS)+1,data=otter.WD.BUSHNELL,family=binomial)
model1<-glmer(TRIGGER~(1|CT.POS)+DIST,data=otter.WD.BUSHNELL,family=binomial)
model2<-glmer(TRIGGER~(1|CT.POS)+GAIT.1,data=otter.WD.BUSHNELL,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model3<-glmer(TRIGGER~(1|CT.POS)+LOIT,data=otter.WD.BUSHNELL,family=binomial)
model4<-glmer(TRIGGER~(1|CT.POS)+AIR,data=otter.WD.BUSHNELL,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model5<-glmer(TRIGGER~(1|CT.POS)+wet.dry,data=otter.WD.BUSHNELL,family=binomial)
model6<-glmer(TRIGGER~(1|CT.POS)+wet.dry+DIST,data=otter.WD.BUSHNELL,family=binomial)
model7<-glmer(TRIGGER~(1|CT.POS)+wet.dry*DIST,data=otter.WD.BUSHNELL,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model8<-glmer(TRIGGER~(1|CT.POS)+wet.dry+GAIT.1,data=otter.WD.BUSHNELL,family=binomial)
model9<-glmer(TRIGGER~(1|CT.POS)+wet.dry*GAIT.1,data=otter.WD.BUSHNELL,family=binomial)
model10<-glmer(TRIGGER~(1|CT.POS)+wet.dry+LOIT,data=otter.WD.BUSHNELL,family=binomial)
model11<-glmer(TRIGGER~(1|CT.POS)+wet.dry+AIR,data=otter.WD.BUSHNELL,family=binomial)
model12<-glmer(TRIGGER~(1|CT.POS)+wet.dry*AIR,data=otter.WD.BUSHNELL,family=binomial)
model13<-glmer(TRIGGER~(1|CT.POS)+GAIT.1+DIST,data=otter.WD.BUSHNELL,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model14<-glmer(TRIGGER~(1|CT.POS)+AIR+DIST,data=otter.WD.BUSHNELL,family=binomial)
model15<-glmer(TRIGGER~(1|CT.POS)+GAIT.1+AIR,data=otter.WD.BUSHNELL,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#create model selection table
otter.WD.BUSHNELL.MODELS<-model.sel(null.model,model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12,model13,model14,model15,rank=AIC)

#view table
otter.WD.BUSHNELL.MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# (b) Acorn video setting

#CLEAR OBJECTS if necessary#
rm(list=ls())
#Import TRIGGER_OTTER_WET.DRY.csv
otter.WD.ACORN<-read.csv("TRIGGER_OTTER_WET.DRY.csv")

##-Select ACORN VIDEO 
otter.WD.ACORN<-otter.WD.ACORN[otter.WD.ACORN$CAMERA.ID=="A",]

#Create new variable called GAIT.1 from GAIT 
#GAIT has three levels, walk, trot and run. 
#combine run and trot into one level "runtrot", new variable will be renamed GAIT.1
GAIT<-otter.WD.ACORN$GAIT
levels(GAIT)
levels(GAIT)<-c("RUNTROT","RUNTROT","W")
otter.WD.ACORN<-data.frame(otter.WD.ACORN,GAIT)

#select all lateral passes
otter.WD.ACORN<-otter.WD.ACORN[otter.WD.ACORN$ORIENT=="L",]

#get rid of records with na values
otter.WD.ACORN<-na.omit(otter.WD.ACORN)

#Run models
null.model<-glmer(TRIGGER~(1|CT.POS)+1,data=otter.WD.ACORN,family=binomial)
model1<-glmer(TRIGGER~(1|CT.POS)+DIST,data=otter.WD.ACORN,family=binomial)
model2<-glmer(TRIGGER~(1|CT.POS)+GAIT.1,data=otter.WD.ACORN,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model3<-glmer(TRIGGER~(1|CT.POS)+LOIT,data=otter.WD.ACORN,family=binomial)
model4<-glmer(TRIGGER~(1|CT.POS)+AIR,data=otter.WD.ACORN,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model5<-glmer(TRIGGER~(1|CT.POS)+wet.dry,data=otter.WD.ACORN,family=binomial)
model6<-glmer(TRIGGER~(1|CT.POS)+wet.dry+DIST,data=otter.WD.ACORN,family=binomial)
model7<-glmer(TRIGGER~(1|CT.POS)+wet.dry*DIST,data=otter.WD.ACORN,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model8<-glmer(TRIGGER~(1|CT.POS)+wet.dry+GAIT.1,data=otter.WD.ACORN,family=binomial)
model9<-glmer(TRIGGER~(1|CT.POS)+wet.dry*GAIT.1,data=otter.WD.ACORN,family=binomial)
model10<-glmer(TRIGGER~(1|CT.POS)+wet.dry+LOIT,data=otter.WD.ACORN,family=binomial)
model11<-glmer(TRIGGER~(1|CT.POS)+wet.dry+AIR,data=otter.WD.ACORN,family=binomial)
model12<-glmer(TRIGGER~(1|CT.POS)+wet.dry*AIR,data=otter.WD.ACORN,family=binomial)
model13<-glmer(TRIGGER~(1|CT.POS)+GAIT.1+DIST,data=otter.WD.ACORN,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model14<-glmer(TRIGGER~(1|CT.POS)+AIR+DIST,data=otter.WD.ACORN,family=binomial)
model15<-glmer(TRIGGER~(1|CT.POS)+GAIT.1+AIR,data=otter.WD.ACORN,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))


#create model selection table
otter.WD.ACORN.MODELS<-model.sel(null.model,model1,model2,model3,model4,model5,model6,model7,model8,model9,model10,model11,model12,model13,model14,model15,rank=AIC)

#view table
otter.WD.ACORN.MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################################################################
  #REGISTRATION MODELS FIGURE 6
#########################################################################  
#  a) Badger Video

#CLEAR OBJECTS if necessary#
rm(list=ls())

#Import REGISTRATION_FOX_BADGER (.csv)
BADGER.REG<-read.csv("REGISTRATION_FOX_BADGER.csv")

#select BADGER observations
BADGER.REG<-BADGER.REG[BADGER.REG$SPECIES=="BADGER",]

#Create new variable called GAIT.1 from GAIT 
#GAIT has three levels, walk, trot and run. 
#combine run and trot into one level "runtrot", new variable will be renamed GAIT.1
GAIT<-BADGER.REG$GAIT
levels(GAIT)
levels(GAIT)<-c("RUNTROT","RUNTROT","W")
BADGER.REG<-data.frame(BADGER.REG,GAIT)

#Select all lateral passes
BADGER.REG<-BADGER.REG[BADGER.REG$ORIENT=="L",]

#Select observations where TRIGGER=1
BADGER.REG<-BADGER.REG[BADGER.REG$TRIGGER=="1",]

#Get rid of records with na values
BADGER.REG<-na.omit(BADGER.REG)

#Run models
null.model<-glmer(CAPTURE~(1|CT.POS)+1,data=BADGER.REG,family=binomial)
model1<-glmer(CAPTURE~(1|CT.POS)+DIST,data=BADGER.REG,family=binomial)
model2<-glmer(CAPTURE~(1|CT.POS)+GAIT.1,data=BADGER.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model3<-glmer(CAPTURE~(1|CT.POS)+LOIT,data=BADGER.REG,family=binomial)
model4<-glmer(CAPTURE~(1|CT.POS)+AIR,data=BADGER.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model5<-glmer(CAPTURE~(1|CT.POS)+GAIT.1+DIST,data=BADGER.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model6<-glmer(CAPTURE~(1|CT.POS)+GAIT.1*DIST,data=BADGER.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model7<-glmer(CAPTURE~(1|CT.POS)+AIR+DIST,data=BADGER.REG,family=binomial)
model8<-glmer(CAPTURE~(1|CT.POS)+GAIT.1+AIR,data=BADGER.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model9<-glmer(CAPTURE~(1|CT.POS)+LOIT+DIST,data=BADGER.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model10<-glmer(CAPTURE~(1|CT.POS)+LOIT+GAIT.1+DIST,data=BADGER.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#Create and view model selection table
BADGER.REG.MODELS<-model.sel(null.model,model1,model2,model3,model4,model5,model6, model7,model8,model9,model10,rank=AIC)
BADGER.REG.MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#(b) fox, Bushnell video capture

#CLEAR OBJECTS if necessary#
rm(list=ls())

#Import REGISTRATION_FOX_BADGER (.csv)
FOX.REG<-read.csv("REGISTRATION_FOX_BADGER.csv")

#select FOX observations
FOX.REG<-FOX.REG[FOX.REG$SPECIES=="FOX",]

#Create new variable called GAIT.1 from GAIT 
#GAIT has three levels, walk, trot and run. 
#combine run and trot into one level "runtrot", new variable will be renamed GAIT.1
GAIT<-FOX.REG$GAIT
levels(GAIT)
levels(GAIT)<-c("RUNTROT","RUNTROT","W")
FOX.REG<-data.frame(FOX.REG,GAIT)

#Select all lateral passes
FOX.REG<-FOX.REG[FOX.REG$ORIENT=="L",]

#Select observations where TRIGGER=1
FOX.REG<-FOX.REG[FOX.REG$TRIGGER=="1",]

#Get rid of records with na values
FOX.REG<-na.omit(FOX.REG)

#Run models
null.model<-glmer(CAPTURE~(1|CT.POS)+1,data=FOX.REG,family=binomial)
model1<-glmer(CAPTURE~(1|CT.POS)+DIST,data=FOX.REG,family=binomial)
model2<-glmer(CAPTURE~(1|CT.POS)+GAIT.1,data=FOX.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model3<-glmer(CAPTURE~(1|CT.POS)+LOIT,data=FOX.REG,family=binomial)
model4<-glmer(CAPTURE~(1|CT.POS)+AIR,data=FOX.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model5<-glmer(CAPTURE~(1|CT.POS)+GAIT.1+DIST,data=FOX.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model6<-glmer(CAPTURE~(1|CT.POS)+GAIT.1*DIST,data=FOX.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model7<-glmer(CAPTURE~(1|CT.POS)+AIR+DIST,data=FOX.REG,family=binomial)
model8<-glmer(CAPTURE~(1|CT.POS)+GAIT.1+AIR,data=FOX.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model9<-glmer(CAPTURE~(1|CT.POS)+LOIT+DIST,data=FOX.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model10<-glmer(CAPTURE~(1|CT.POS)+LOIT+GAIT.1+DIST,data=FOX.REG,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#Create and view model selection table
FOX.REG.MODELS<-model.sel(null.model,model1,model2,model3,model4,model5,model6, model7,model8,model9,model10,rank=AIC)
FOX.REG.MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#c) ALL OTTER, Bushnell video

#CLEAR OBJECTS if necessary#
rm(list=ls())

#Import  (.csv)
OTTER.REG.BUSHNELL<-read.csv("REGISTRATION_OTTER.csv")

#SELECT BUSHNELL VIDEO OBSERVATIONS
OTTER.REG.BUSHNELL<-OTTER.REG.BUSHNELL[OTTER.REG.BUSHNELL$CAMERA.ID=="BV",]

#Create new variable called GAIT.1 from GAIT 
#GAIT has three levels, walk, trot and run. 
#combine run and trot into one level "runtrot", new variable will be renamed GAIT.1
GAIT<-OTTER.REG.BUSHNELL$GAIT
levels(GAIT)
levels(GAIT)<-c("RUNTROT","RUNTROT","W")
OTTER.REG.BUSHNELL<-data.frame(OTTER.REG.BUSHNELL,GAIT)

#Select all lateral passes
OTTER.REG.BUSHNELL<-OTTER.REG.BUSHNELL[OTTER.REG.BUSHNELL$ORIENT=="L",]

#Select observations where TRIGGER=1
OTTER.REG.BUSHNELL<-OTTER.REG.BUSHNELL[OTTER.REG.BUSHNELL$TRIGGER=="1",]

#Get rid of records with na values
OTTER.REG.BUSHNELL<-na.omit(OTTER.REG.BUSHNELL)

#Run models
null.model<-glmer(CAPTURE~(1|CT.POS)+1,data=OTTER.REG.BUSHNELL,family=binomial)
model1<-glmer(CAPTURE~(1|CT.POS)+DIST,data=OTTER.REG.BUSHNELL,family=binomial)
model2<-glmer(CAPTURE~(1|CT.POS)+GAIT.1,data=OTTER.REG.BUSHNELL,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model3<-glmer(CAPTURE~(1|CT.POS)+LOIT,data=OTTER.REG.BUSHNELL,family=binomial)
model4<-glmer(CAPTURE~(1|CT.POS)+AIR,data=OTTER.REG.BUSHNELL,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model5<-glmer(CAPTURE~(1|CT.POS)+GAIT.1+DIST,data=OTTER.REG.BUSHNELL,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model6<-glmer(CAPTURE~(1|CT.POS)+GAIT.1*DIST,data=OTTER.REG.BUSHNELL,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model7<-glmer(CAPTURE~(1|CT.POS)+AIR+DIST,data=OTTER.REG.BUSHNELL,family=binomial)
model8<-glmer(CAPTURE~(1|CT.POS)+GAIT.1+AIR,data=OTTER.REG.BUSHNELL,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model9<-glmer(CAPTURE~(1|CT.POS)+LOIT+DIST,data=OTTER.REG.BUSHNELL,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model10<-glmer(CAPTURE~(1|CT.POS)+LOIT+GAIT.1+DIST,data=OTTER.REG.BUSHNELL,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#Create and view model selection table
OTTER.REG.BUSHNELL.MODELS<-model.sel(null.model,model1,model2,model3,model4,model5,model6, model7,model8,model9,model10,rank=AIC)
OTTER.REG.BUSHNELL.MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#d) ALL OTTER, Acorn video

#CLEAR OBJECTS if necessary#
rm(list=ls())

#Import  (.csv)
OTTER.REG.ACORN<-read.csv("REGISTRATION_OTTER.csv")

#SELECT ACIORN VIDEO OBSERVATIONS
OTTER.REG.ACORN<-OTTER.REG.ACORN[OTTER.REG.ACORN$CAMERA.ID=="A",]

#Create new variable called GAIT.1 from GAIT 
#GAIT has three levels, walk, trot and run. 
#combine run and trot into one level "runtrot", new variable will be renamed GAIT.1
GAIT<-OTTER.REG.ACORN$GAIT
levels(GAIT)
levels(GAIT)<-c("RUNTROT","RUNTROT","W")
OTTER.REG.ACORN<-data.frame(OTTER.REG.ACORN,GAIT)

#Select all lateral passes
OTTER.REG.ACORN<-OTTER.REG.ACORN[OTTER.REG.ACORN$ORIENT=="L",]

#Select observations where TRIGGER=1
OTTER.REG.ACORN<-OTTER.REG.ACORN[OTTER.REG.ACORN$TRIGGER=="1",]

#Get rid of records with na values
OTTER.REG.ACORN<-na.omit(OTTER.REG.ACORN)

#Run models
null.model<-glmer(CAPTURE~(1|CT.POS)+1,data=OTTER.REG.ACORN,family=binomial)
model1<-glmer(CAPTURE~(1|CT.POS)+DIST,data=OTTER.REG.ACORN,family=binomial)
model2<-glmer(CAPTURE~(1|CT.POS)+GAIT.1,data=OTTER.REG.ACORN,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model3<-glmer(CAPTURE~(1|CT.POS)+LOIT,data=OTTER.REG.ACORN,family=binomial)
model4<-glmer(CAPTURE~(1|CT.POS)+AIR,data=OTTER.REG.ACORN,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model5<-glmer(CAPTURE~(1|CT.POS)+GAIT.1+DIST,data=OTTER.REG.ACORN,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model6<-glmer(CAPTURE~(1|CT.POS)+GAIT.1*DIST,data=OTTER.REG.ACORN,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model7<-glmer(CAPTURE~(1|CT.POS)+AIR+DIST,data=OTTER.REG.ACORN,family=binomial)
model8<-glmer(CAPTURE~(1|CT.POS)+GAIT.1+AIR,data=OTTER.REG.ACORN,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model9<-glmer(CAPTURE~(1|CT.POS)+LOIT+DIST,data=OTTER.REG.ACORN,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
model10<-glmer(CAPTURE~(1|CT.POS)+LOIT+GAIT.1+DIST,data=OTTER.REG.ACORN,family=binomial,control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))

#Create and view model selection table
OTTER.REG.ACORN.MODELS<-model.sel(null.model,model1,model2,model3,model4,model5,model6, model7,model8,model9,model10,rank=AIC)
OTTER.REG.ACORN.MODELS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#end
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

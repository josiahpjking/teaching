library(tidyverse)
load("../Desktop/uoepsy_courses/teaching/docs/misc/pptnames.rdata")
infl = pptnames[c(190,197,142,4,277,382,424)]
pptnames = setdiff(pptnames,infl)
pptnames1 = sample(pptnames, 100)
pptnames2 = setdiff(pptnames,pptnames1)
# want to know if audio illusions lead to greater sense of unease for X people. 


# q1a - 
  # do illusions result in unease?
  # chisq: (pitch/rhythm/standard music/silence) & (unease: y/n)
  # result: pitch&rhythm most unease, then silence, then music
# q1b - which illusion to study?
    # t.test: physiological measure ~ pitch vs rhythm
    # result: higher for rhythm  
# q1c - assess measurement
  # correlation: physiological measure - HRV? & self-report unease/tension scale
  # result. high correlation. self report measure is good to use



# illusions and unease
create_q1a <- function(group="alligators"){
  set.seed(as.numeric(sum(sapply(strsplit(group,"")[[1]], \(x) which(letters==x)))))
  while(TRUE){
    totN = 98
    df = tibble(
      participant = sample(pptnames1, 98),
      audio = sample(c("control","silence","pitch illusion","rhythm illusion"),totN,replace=TRUE),
      unease = rbinom(totN,1,
                      plogis(
                        (audio == "silence")*.2 + 
                        (audio == "pitch illusion")*.23 + 
                        (audio == "rhythm illusion")*.4
                      )
                )
    ) |> mutate(unease = ifelse(unease==1,"Yes","No"))
    
    # checks
    # 1. chisq. is signif
    tmp.test = chisq.test(table(df[,-1]))
    if(tmp.test$p.value<.05){
      return(df)
    }
  }
}


# fnirs rhythm vs pitch
create_q1b <- function(group="alligators"){
  set.seed(as.numeric(sum(sapply(strsplit(group,"")[[1]], \(x) which(letters==x)))))
  while(TRUE){
    df = tibble(
      illusion = rep(c("rhythm","pitch"),e=15),
      dHRV = round(c(
        rnorm(15,-9.43,4.3),
        rnorm(15,-4.2,6.3)
      ),1)
    ) 
    
    # checks
    # 1. unequal vars
    # 2. one sided t is signif (before messiness)
    tmp.var = var.test(dHRV ~ illusion, df)
    tmp.test = t.test(dHRV ~ illusion,df,alternative="greater")
    if(tmp.test$p.value<.05 & tmp.var$p.value<.05){
      
      df[sample(1:15,4),"illusion"] <- rep(c("rhthm","rhthym"),c(3,1))
      
      return(df)
    }
  }
}


# want to check SR measure
create_q1c <- function(group="alligators"){
  set.seed(as.numeric(sum(sapply(strsplit(group,"")[[1]], \(x) which(letters==x)))))
  while(TRUE){
    df = tibble(
      pid = paste0("ppt_",1:45),
      u_l = pmax(0,pmin(1,rnorm(45,.5,.2))),
      SRunease = map_dbl(u_l, ~sum(rbinom(8,10,.))),
      dHRV= round(rnorm(45, -1.8 - 2.7*scale(u_l), 2.3),1)
    ) |> select(-u_l) 

    outlier = sample(1:30,1)
    df$SRunease[outlier]<- sample(70:80,1)
    df$dHRV[outlier] <- round(max(df$dHRV)+2*sd(df$dHRV))
    df$dHRV[sample(setdiff(1:45,outlier),16)] <- NA
    
    # checks
    # 1. non-signif with outlier (n=1)
    # 2. signif without outlier (n=1)
    sig_with <- with(df, cor.test(SRunease,dHRV)$p.value < .05)
    sig_without <- with(df[-outlier, ], cor.test(SRunease,dHRV)$p.value < .05)
    if(!sig_with & sig_without){
      return(df)
    } 
  }
}

# stop()
# lm(SRunease ~ condition * (volume + speakers/headphones))
# glm(agreed_to_longer_study ~ rhythm_condition + volume + speakers/headphones



create_2n3 <- function(group="alligators"){
  set.seed(as.numeric(sum(sapply(strsplit(group,"")[[1]], \(x) which(letters==x)))))
  while(TRUE){
    totN = 210
    df = tibble(
      ppt = sample(pptnames2, totN),
      age = round(runif(totN,23,63)),
      opsys = c("iOS","android")[rbinom(totN, 1, .6)+1],
      hphones = rbinom(totN, 1, plogis(.5*scale(age))),
      volume = pmax(5,pmin(95,round(rchisq(totN, df=45+10*hphones+
                                             3*scale(age))))),
      rhythm = rep(letters[1:3],totN/3),
    )
    
    
    
    # dimnames( model.matrix( ~ opsys + hphones + volume + rhythm + hphones:rhythm + volume:rhythm, data=df) )[[2]]
    lp = 
      model.matrix( ~ scale(age) + opsys + hphones + scale(volume) + rhythm + 
                      hphones:rhythm + scale(volume):rhythm, data=df) %*%
      c(0,-.1,0,-0.5,.1,1,1.5,.25,-.25,.5,.8)
    
    df$SRU = pmin(80,pmax(0,round(27+rnorm(totN, lp, 1.5)*7.7)))
    

    # add in influencers
    dfi = tibble(
      ppt = sample(infl,7),
      age = round(runif(7,17,70)),
      opsys = c("android","iOS")[c(1,2,2,1,1,2,1)],
      hphones = rbinom(7,1,.4),
      rhythm = c("a","c","c","b","c","a","s"),
      volume = c(88,11,82,55,46,62,55),
      SRU = round(pmax(0,pmin(80,
                        c(max(df$SRU)+1.5*sd(df$SRU),
                          mean(df$SRU)+1.5*sd(df$SRU),
                          min(df$SRU)-1.5*sd(df$SRU),
                          84, 87, -2, 55))))
    )
    # ggplot(df,aes(x=volume,y=SRU,col=rhythm))+geom_point()+geom_smooth(method=lm)
    df = slice_sample(bind_rows(df,dfi[1:6,]),prop=1)
    
    plp = 
      model.matrix( ~ 1 + scale(SRU) + opsys + 
                      hphones + scale(volume) + rhythm, data=df) %*%
      c(.3,-3,0,.2,.3,0,0)
    
    df$interested = rbinom(nrow(df), 1, plogis(plp))
    df$interested[sample(1:totN,.2*totN)] <- NA
    
    # add in stuff to clean
    
    # do checks
    modcheck <- function(mod,terms,dirs,sigs){
      coeft = summary(mod)$coefficients[terms,]
      c1 = all( sign(coeft[,1]) == dirs )
      c2 = all((coeft[,4]<.05) == sigs)
      if(c1 & c2){return(TRUE)}else{return(FALSE)}
    }
    
    m1 = lm(SRU ~ age+opsys+hphones + volume * rhythm, df)
    m1a = lm(SRU ~ (age+opsys+hphones + volume) * rhythm, df)
    
    i_torm = which(cooks.distance(m1) > quantile(cooks.distance(m1), .99))
    m2 = lm(SRU ~ age+opsys+hphones + volume * rhythm, df[-i_torm, ])
    m2a = lm(SRU ~ (age+opsys+hphones + volume) * rhythm, df[-i_torm, ])
    
    
    m3 = glm(interested~1+SRU+hphones+rhythm,df,family=binomial)
    m3a = glm(interested~1+hphones+rhythm,df,family=binomial)
    
    
    checks = c(
      (modcheck(m1,c(8,9),c(1,1),c(T,F)) | modcheck(m1,c(8,9),c(1,1),c(F,T))),
      (modcheck(m1a,c(14,15),c(1,1),c(T,F)) | modcheck(m1a,c(14,15),c(1,1),c(F,T))),
      all(sort(abs(scale(cooks.distance(m1))[,1]),decreasing=TRUE)[1:3] > 3),
      all(sort(abs(scale(cooks.distance(m1a))[,1]),decreasing=TRUE)[1:3] > 3),
      modcheck(m2,c(8,9),c(1,1),c(T,T)),
      modcheck(m2a,c(14,15),c(1,1),c(T,T)),
      
      all((summary(m3)$coefficients[c(2,4,5),4]<.05) == c(T,F,F)),
      all((summary(m3a)$coefficients[c(3,4),4]<.05) == c(T,T))
      
    )
    checks
    
      
    if(all(checks)){
      
      df = slice_sample(bind_rows(df,dfi[7,]),prop=1)
      
      toch = sample(1:nrow(df),4)
      df$opsys[toch] <- ifelse(df$opsys[toch]=="iOS","iOs","Android")
      toch2 = sample(1:nrow(df),4)
      df$age[toch2] <- paste0(df$age[toch2], " years")
      df <- df |> mutate(
        rhythm = case_when(
          rhythm == "a" ~ 2,
          rhythm == "b" ~ 1,
          rhythm == "c" ~ 3,
          TRUE ~ 4
        )
      )
      

      
      return(list(
        df |> select(-interested),
        df |> select(ppt, interested) |> na.omit()
      ))
    }
  }
}


p1a <- create_q1a()
# chisq.test(table(p1a[,2:3]))
p1b <- create_q1b()
# p1b <- p1b |> mutate(illusion = ifelse(illusion!="pitch","rhythm","pitch"))
# t.test(dHRV ~ illusion, p1b)
p1c <- create_q1c()
# p1c <- drop_na(p1c, dHRV)
# p1c <- p1c[-which.max(resid(lm(SRunease~dHRV,p1c))), ]
# with(p1c, cor.test(SRunease, dHRV))
p23 <- create_2n3()
p2 <- p23[[1]] #|> mutate(rhythm=factor(rhythm),age=parse_number(age))
# lm(SRU ~ hphones + volume * rhythm, p2) |> summary()
# lm(SRU ~ (hphones + volume) * rhythm, p2) |> summary()
# # removing influencers
# lm(SRU ~ hphones + volume * rhythm, p2 |> filter(!ppt %in% infl)) |> summary()
# lm(SRU ~ (hphones + volume) * rhythm, p2 |> filter(!ppt %in% infl)) |> summary()
p3 <- p23[[2]]
# glm(interested ~ hphones + volume + rhythm, 
#     full_join(p2,p3), family=binomial) |> summary()


groups = c("unknown","analytical_adders","binomial_buzzards","calculating_capercaillies","estimating_eagles","frequency_fulmars","histogram_hares","jitterplot_jays","kurtosis_kites","likelihood_lapwings","outlier_otters","parameter_ptarmigans","quantile_quails","regression_ravens","standard-error_salmon","two-tailed_terns","variance_voles","winsorizing_wildcats")

for(g in groups[1]){
  pilotA <- create_q1a(group = g)
  pilotB <- create_q1b(group = g)
  pilotC <- create_q1c(group = g)
  p23 <- create_2n3(group = g)
  risset <- p23[[1]] 
  followup <- p23[[2]]
  save(pilotA,pilotB,pilotC,risset,followup,file = paste0(g,".RData"))
}





# get my data function.. 
get_my_data <- function(group_name){
  tryCatch(
    suppressWarnings(load(url(paste0("https://uoepsy.github.io/usmr/2627/misc/",tolower(group_name),".RData")))),
    
    error=function(e) cat(paste0(c("No Data Found. Are you sure you used the correct group name?\nGroup names should have an underscore between the words. Choose from these:  ", paste0(
      c("analytical_adders","binomial_buzzards","calculating_capercaillies","estimating_eagles","frequency_fulmars","histogram_hares","jitterplot_jays","kurtosis_kites","likelihood_lapwings","outlier_otters","parameter_ptarmigans","quantile_quails","regression_ravens","standard-error_salmon","two-tailed_terns","variance_voles","winsorizing_wildcats"),
      collapse="\n")), "\n\n\nIf you cannot make this work with your exam number, please use get_my_data(\"unknown\")"))
  )
  
  pilotA <<- pilotA
  pilotB <<- pilotB
  pilotC <<- pilotC
  risset <<- risset
  followup <<- followup
}







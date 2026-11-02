library(tidyverse)
load(url("https://josiahpjking.github.io/teaching/misc/pptnames.rdata"))
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
      u_l = runif(45,.2,.9),
      SRunease = map_dbl(u_l, ~sum(rbinom(9,5,.))),
      dHRV= round(rnorm(45, -4.4 - 2.7*scale(u_l), 2.3),1)
    ) |> select(-u_l) 

    outlier = sample(1:30,1)
    df$SRunease[outlier]<- sample(35:40,1)
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
      opsys = c("iOS","android")[rbinom(totN, 1, .6)+1],
      hphones = rbinom(totN, 1, .35),
      volume = pmax(5,pmin(95,round(rchisq(totN, df=45+10*hphones)))),
      rhythm = rep(letters[1:3],totN/3),
    )
    
    
    head(df)
  
    # dimnames( model.matrix( ~ opsys + hphones + volume + rhythm + hphones:rhythm + volume:rhythm, data=df) )[[2]]
    lp = 
      model.matrix( ~ opsys + hphones + scale(volume) + rhythm + 
                      hphones:rhythm + scale(volume):rhythm, data=df) %*%
      c(0,0,-.5,.3,1,1.5,.5,-.5,.5,.8)
    
    df$SRU = pmin(45,pmax(0,round(20+rnorm(totN, lp, 1)*3.7)))
    
    # sjPlot::tab_model(
    #   lm(SRU ~ (hphones + volume ) * rhythm, df),
    #   lm(SRU ~ hphones + volume * rhythm, df)
    # )
    # 
    # lm(SRU ~ (hphones + volume ) * rhythm, df) |>
    #   effects::effect(term="volume*rhythm") |>
    #   as.data.frame() |>
    #   ggplot(aes(x=volume,y=fit,ymin=lower,ymax=upper,
    #              col=rhythm))+
    #   geom_line()+geom_ribbon(alpha=.1)
    # 
    # ggplot(df,aes(x=volume,y=SRU,col=rhythm))+
    #   geom_point()+
    #   geom_smooth(method=lm)
    
    
    # add in influencers
    dfi = tibble(
      ppt = infl,
      opsys = c("android","iOS")[c(1,2,2,2,1,1,2)],
      hphones = rbinom(7,1,.4),
      rhythm = c("a","c","c","a","b","b","c"),
      volume = c(87,5,34,67,64,88,70),
      SRU = c(max(df$SRU)+1.5*sd(df$SRU),
              min(df$SRU)-2*sd(df$SRU),
             mean(df$SRU),
             mean(df$SRU)+2.3,mean(df$SRU)+2.8,
             mean(df$SRU)-3.4,mean(df$SRU)+4.3)
    )
    df = slice_sample(bind_rows(df,dfi),prop=1)
    
    # ggplot(df,aes(x=volume,y=SRU,col=rhythm))+
    #   geom_point()+
    #   geom_smooth(method=lm)
    
    
    plp = 
      model.matrix( ~ 1 + scale(SRU) + opsys + 
                      hphones + scale(volume) + rhythm, data=df) %*%
      c(.3,-3,0,.2,.3,0,0)
    
    df$interested = rbinom(nrow(df), 1, plogis(plp))
    df$interested[sample(1:totN,.2*totN)] <- NA
    glm(interested~1+hphones+rhythm,df,family=binomial) |> summary()
    
    # mod = 
    #   lm(EF ~ age+op_sys+env_concern*nudged, 
    #      df |> mutate(nudged=factor(nudged)))
    # mod_mi = 
    #   lm(EF ~ age+op_sys+env_concern*nudged, 
    #      df[df$isinf==1,] |> mutate(nudged=factor(nudged)))
    # mod3 = glm(uninstalled ~ age + op_sys + 
    #              env_concern + EF + nudged, 
    #            data = df[df$nudged >= 1, ] |> mutate(nudged=factor(nudged)), 
    #            family=binomial)
    
    # checks <- c(
    #   all(summary(mod_mi)$coefficients[7,4]>.05),
    #   all(summary(mod_mi)$coefficients[8,4]<.05),
    #   all(sign(summary(mod_mi)$coefficients[8,1])==-1),
    #   summary(mod3)$coefficients[5,4]<.05,
    #   sign(summary(mod3)$coefficients[5,1])==1
    # )
  
    
    
    # if(all(checks)){
    #   df$age = as.character(df$age)
    #   ww=sample(1:nrow(df),1)
    #   df[ww,"age"] <- paste0(df[ww,"age"]," years")
    #   ww=sample(1:nrow(df),1)
    #   df[ww,"age"] <- paste0(df[ww,"age"],"yrs")
    #   df$installed <- as.character(df$installed)
    #   ww=sample(1:nrow(df),1)
    #   df$installed[ww] <- paste0(df$installed[ww],".0")
    #   ww=sample(1:nrow(df),1)
    #   df$installed[ww] <- paste0(df$installed[ww],".0")
    #   
    #   return(list(
    #     
    #     df |> select(-ec_l,-installed,-uninstalled,-isinf),
    #     
    #     slice_sample(df |> select(ppt,installed), prop=1)
    #     
    #   ))
    # }
    if(1){
      return(list(
        
        df |> select(-interested),
        df |> select(ppt, interested) |> na.omit()
      ))
    }
  }
}


set.seed(987)
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
p2 <- p23[[1]]
# lm(SRU ~ hphones + volume * rhythm, p2) |> summary()
# lm(SRU ~ (hphones + volume) * rhythm, p2) |> summary()
# # removing influencers
# lm(SRU ~ hphones + volume * rhythm, p2 |> filter(!ppt %in% infl)) |> summary()
# lm(SRU ~ (hphones + volume) * rhythm, p2 |> filter(!ppt %in% infl)) |> summary()

p3 <- p23[[2]]
# glm(interested ~ hphones + volume + rhythm, 
#     full_join(p2,p3), family=binomial) |> summary()


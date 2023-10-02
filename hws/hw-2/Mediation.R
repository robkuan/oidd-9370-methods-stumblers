

 #Explain  'this.dir()' and 'groundhog'
  
  library('groundhog')
  groundhog.library('this.path','2023-07-01')
  path1 = file.path(this.dir() , 'buder et al.csv')
  dat=read.csv(path1)
  
#Code posted with paper
  #Recoode the button variables
    dat$button_0_1 <- NA
    dat[dat$button == "like",]$button_0_1 <- 0
    dat[dat$button == "dislike",]$button_0_1 <- 1
    
    table(dat$button,dat$button_0_1)
  
  #Key analysis in the paper 
    results <- psych::mediate(y = "value", x = "conf", m = "button_0_1", data = dat)
  
  #Do it by hand
    results

  
  
  
  
  
  
  table(df$forum)
  table(df$mani_check)
  table(df$echo)
  
  lm()
  
  table(df$conf)
  
  
  table(table(df$id))
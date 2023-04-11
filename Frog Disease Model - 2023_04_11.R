##### Packages #####
# Function to install/load packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)}
# List of packages
packages <- c("ggplot2", "cowplot", "dplyr", "MetBrewer","egg","patchwork")
ipak(packages)

##### Establishing state variable matrices #####

### Our states variables are: Time, Size, and Infection
tmax <- 52 # Time (52 weeks = 1 year)
Smax <- 208 # Size (how much they can grow in the time span)
Imax <- 11 # Infection level (1 are non infected, infection level increase 2-11)
Bmax <- 4 # Four seasonal scenarios

### Matrices
f <- array(0,dim=c(Smax,Imax,tmax))
d <- array(0,dim=c(Smax,Imax,(tmax-1)),dimnames = list(Size=1:Smax,Infection=1:Imax,times=1:(tmax-1)))
v <- array(0,dim=c(Smax,Imax,(tmax-1),3))

### Different seasonal scenario matrices
# d1- ; d2-; d3-; d4-. In which W=wet and D=dry
d1 <- array(0,dim=c(Smax,Imax,(tmax-1)),dimnames = list(Size=1:Smax,Infection=1:Imax,times=1:(tmax-1)))
d2 <- array(0,dim=c(Smax,Imax,(tmax-1)),dimnames = list(Size=1:Smax,Infection=1:Imax,times=1:(tmax-1)))
d3 <- array(0,dim=c(Smax,Imax,(tmax-1)),dimnames = list(Size=1:Smax,Infection=1:Imax,times=1:(tmax-1)))
d4 <- array(0,dim=c(Smax,Imax,(tmax-1)),dimnames = list(Size=1:Smax,Infection=1:Imax,times=1:(tmax-1)))

##### Figures 1 is framework related, not done in R #####
##### Figure S1 was made using data from another project; thus not included #####
##### Size-dependent Terminal fitness function #####

### Function
phi <- function(S){
  phi = 1/(1 + exp(-0.05*(S-100)))
  return(phi)}

### Plot Terminal fitness as function of size
datafrmphi = as.data.frame(Size <- 1:Smax)
datafrmphi$Fitness = phi(Size)
ggplot(data = datafrmphi, aes(x = Size, y=Fitness)) +
  geom_line(size = 2, color = "grey30")+
  theme_bw()+ 
  theme(panel.grid = element_blank(), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=16))+
  labs (x = "Size (states)", y = " Terminal fitness")
ggsave("Terminal fitness.tiff", units="in", width=4, height=4, dpi=300, 
       compression = 'lzw')

############### Basic model ###############
##### Parameters for the base model (Fig 2) #####

Ss <- 1:Smax # indicating size range
plot((phi(Ss))~Ss)

### Probability of dying due to size
# The probability of survival is based in a negative logistic curve
pS <- function(S,Ts,A,b){ # S=size, Ts=medium, A=probability of dying at smallest size, b=rate of decrease
  (A/(1 + exp(b*(S-Ts))))}
Ts = 50 # i.e., Size 50 is the medium prob of dying
A = 0.8/Ts # frogs at Size = 1 have an 80% chance of dying at each time step
b = 0.05 # the rate of decrease
plot(pS(Ss,Ts,A,b)~Ss)

# Creating dataframe
datafrmSs = as.data.frame(Size <- 1:Smax)
datafrmSs$Dying = (pS(Size,Ts,A,b))
# Plot mortality as function of size (Fig 3)
ggplot(data = datafrmSs, aes(x = Size, y=Dying)) +
  geom_line(size = 2, color = "grey30")+
  theme_bw()+ 
  theme(panel.grid = element_blank(), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  scale_color_manual(values=met.brewer("NewKingdom", 3), name = "Strategy:")+
  labs (x = "Size (states)", y = " Probability of dying")
ggsave("Fig3_M(S).tiff", units="in", width=4, height=4, dpi=300, 
       compression = 'lzw')

### Probability of dying from infection based on the code, but biologically is I-1
pD <- c(0.00, # Non-infected I=1 
        0.00009,0.00009, # Infected I=2,3
        0.0009,0.0009, # Infected I=4,5
        0.009,0.009, # Infected I=6,7
        0.09,0.09, # Infected I=8,9
        0.9,0.9) # Infected I=10,11

# Probability of finding 0, 2, or 4 energy units
# Season determines the energy gains per week, those energies are distributed among the behaviors
pE <- array(0,dim = c(2,3))
pE[1,] = c(0.33,0.33,0.33) # Dry Season: p(E==0,E==2,E==4)
pE[2,] = c(0.33,0.33,0.33) # Wet Season: p(E==0,E==2,E==4)

### Probability of getting infected: dry = [1], wet = [2]
(0.31+0.52)/2 # probabilities of dying in the seasonal model to calculate the average
pI <- c(0.415 , 0.415) 


##### Strategies functions #####
## How state variables (Size = S and Infection = I) change based on 
# 1) the strategies (i): 1=grow, 2=suppress infection, or 3=both
# 2) the available energy (E)

strat <- function(i,S,I,E){
  nI = I
  if (E == 0){ # no energy to allocate
    if (i==1){ 
      S = S 
      I = I + 1 - 0}
    if (i==2){ 
      S = S 
      I = I + 1 - 0}
    if (i==3){ 
      S = S  
      I = I + 1 - 0}} # end of function for E = 0
  if (E == 2){ # 2 energy units to allocate
    if (i==1){ # invest 2 units in growth and 0 in suppress infection
      S = S + 2
      I = I + 1 - 0}
    if (i==2){ # invest 1 units in growth and 1 in suppress infection
      S = S + 1
      I = I + 1 - 1}
    if (i==3){ # invest 0 units in growth and 2 in suppress infection
      S = S + 0 
      I = I + 1 - 2}} # end of function for E = 2
  if (E == 4){ # 4 energy units to allocate
    if (i==1){ # invest 4 units in growth and 0 in suppress infection
      S = S + 4
      I = I + 1 - 0}
    if (i==2){ # invest 2 units in growth and 2 in suppress infection
      S = S + 2
      I = I + 1 - 2}
    if (i==3){ # invest 0 units in growth and 4 in suppress infection
      S = S + 0
      I = I + 1 - 4}} # end of function for E = 4
  S = min(S,Smax)
  S = max(S,1)
  I = min(I,Imax)
  I = max(I,1)
  if (nI == 1){ # if it is not infected will remain like that
    I = 1 }
  return(list("S"=S,"I"=I))} # end of the strategy function

##### Seasonal time function #####
# vector space 1 = cool, vector space 2 = warm

seas <- function(dS){ # t = time, dS = seasonal distribution
  if (dS==1){ # cool season first half of year
    t.D = rep(1,times=tmax/2)
    t.W = rep(2, times=tmax/2)
    seas = c(t.D,t.W)} # end if (dS == 1)
  if (dS==2){ # warm season first half of year
    t.D = rep(1,times=tmax/2)
    t.W = rep(2, times=tmax/2)
    seas = c(t.W,t.D)} # end if (dS == 2)
  if (dS==3){ # cool season first and last quarter
    t.D = rep(1, times=tmax/4)
    t.W = rep(2, times=tmax/2)
    seas = c(t.D,t.W,t.D)} # end if (dS == 3)
  if (dS==4){ ## cool season middle half of year
    t.W = rep(2, times=tmax/4)
    t.D = rep(1, times=tmax/2)
    seas = c(t.W,t.D,t.W)} # end if (dS == 4)
  return(seas)} # end of seasonal time function

##### Backward iteration #####

## Backward iteration - base loop
for (B in 1:4){ # referring to the four seasonality scenarios
  for (S in 1:Smax){  # start of terminal Fitness Loop
    for (I in 1:11){
      f[S,I,tmax] = (1-(pD[I]*tmax))*phi(S)}} # end of the terminal fitness loop
  for (t in (tmax-1):1){ # start backward time loop
    for (S in Smax:1){ # start size loop
      for (I in Imax:1){ # start level of infection loop
        for (i in 1:3){ # start of the strategies loop
          C = seas(B)[t] # season at time = t
          dS0 = strat(i,S,I,0)$S #change in size energy = 0
          dS2 = strat(i,S,I,2)$S #change in size energy = 2
          dS4 = strat(i,S,I,4)$S #change in size energy = 4
          dI0 = strat(i,S,I,0)$I #change in infection level energy = 0
          dI2 = strat(i,S,I,2)$I #change in infection level energy = 2
          dI4 = strat(i,S,I,4)$I #change in infection level energy = 4
          # If there is no infection; just v1
          if (I==1){
            v[S,1,t,1] = (1-pS(S,Ts,A,b)) *
              (pE[C,1]*(pI[C]*((1-pD[2])*f[dS0,dI0,(t+1)]) +
                          (1-pI[C])*((1-pD[1])*f[dS0,dI0,(t+1)])) +
                 pE[C,2]*(pI[C]*((1-pD[2])*f[dS2,dI2,(t+1)]) +
                            (1-pI[C])*((1-pD[1])*f[dS2,dI2,(t+1)])) +
                 pE[C,3]*(pI[C]*((1-pD[2])*f[dS4,dI4,(t+1)]) +
                            (1-pI[C])*((1-pD[1])*f[dS4,dI4,(t+1)])))
            v[S,1,t,2] = 0
            v[S,1,t,3] = 0} # end of no infection
          # If there is infection; v1, v2, v3
          else{ 
            v[S,I,t,i] = (1-pS(S,Ts,A,b))*
              (pE[C,1]*(1-pD[dI0])*f[dS0,dI0,(t+1)] +
                 pE[C,2]*(1-pD[dI2])*f[dS2,dI2,(t+1)] +
                 pE[C,3]*(1-pD[dI4])*f[dS4,dI4,(t+1)])
          } # end if existing infection
        } # end strategy loop (i)
        f[S,I,t] = max(v[S,I,t,1:3]) # set max fitness
        for (i in 3:1){ # start decision matrix loop
          if (f[S,I,t] == max(v[S,I,t,i])){
            d[S,I,t] = i}
        } # end decision matrix loop
      } # end infection level loop
    } # end size loop
  } # end backward time loop
  # Different Decision Matrices for the Different Seasonal Patterns #
  if (B == 1){d1 = d} # Scenario C-C-W-W
  if (B == 2){d2 = d} # Scenario W-W-C-C
  if (B == 3){d3 = d} # Scenario C-W-W-C
  if (B == 4){d4 = d} # Scenario W-C-C-W
  # Convert current decision matrix to dataframe
  df <- as.data.frame(as.table(d))
  # convert elements of dataframe from factor to numeric 
  df$Size = as.numeric(df$Size)
  df$times = as.numeric(df$times)
  df$Infection = as.factor(df$Infection) # converted to factor for the sake of making heat map
} # End of the backward iteration loop

##### Decision matrices for each seasonal scenario - base model #####

### Extracting and manipulating data to plot matrices
df1 <- as.data.frame(as.table(d1))
df1$Scenarios= (rep("No seasons", length(d1)))
df1$Model= (rep("Basic", length(d1)))
df2 <- as.data.frame(as.table(d2))
df2$Scenarios= (rep("NA", length(d2)))
df2$Model= (rep("Basic", length(d2)))
df3 <- as.data.frame(as.table(d3))
df3$Scenarios= (rep("NA", length(d3)))
df3$Model= (rep("Basic", length(d3)))
df4 <- as.data.frame(as.table(d4))
df4$Scenarios= (rep("NA", length(d4)))
df4$Model= (rep("Basic", length(d4)))

### Binding data from four seasonal scenarios
df_base=rbind(df1,df2,df3,df4)

df_base$Infection_level = as.numeric(df_base$Infection) - 1
df_base$Strategy <- as.factor(ifelse(df_base$Freq == 1, "Grow",
                                       ifelse(df_base$Freq == 2, "Grow/Control infection", 
                                              ifelse(df_base$Freq == 3, "Control infection", NA))))
df_base$Strategy <- factor(df_base$Strategy,levels = c("Grow","Grow/Control infection","Control infection"))

# Calculating size in mm
df_base$Size = as.integer(df_base$Size)

df_base = subset(df_base, Scenarios == "No seasons")
# Exporting decision matrices dataframe so we don't have to run the model every time
write.csv(df_base,file="decisions_base.csv")

### Plot decision matrices ( just for m=visualization)
ggplot(data = df_base, aes(x = times, y = Size, color = Strategy))+
  geom_point(shape=15)+
  facet_grid(Infection_level ~ Scenarios)+
  theme_bw()+ 
  theme(legend.position = "bottom",panel.grid = element_blank(), 
        legend.title=element_text(size=14), 
        legend.text=element_text(size=14),
        axis.text=element_blank(),
        axis.title=element_text(size=16),
        strip.text = element_text(size=14),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  scale_color_manual(values=met.brewer("NewKingdom", 3), name = "Strategy:")+
  labs (x = "Time (wks)", y = "Size (states)")+
  geom_vline(xintercept = c(13,26,39), linetype="solid", color = "black", size=0.1)+
  guides(color = guide_legend(override.aes = list(size = 5)))

ggsave("Stategy_Basic.tiff", units="in", width=8.5, height=11, dpi=300, 
       compression = 'lzw')

##### Forward Simulation #####
### Setting the seed so both models use the same Monte Carlo random values
set.seed(854354)

### Parameters for the simulation
# Arbitrary Frog Number; 100 frogs per seasonal scenario
Fr <- 100
Frog_max <- Fr * 4
## Frog Agent Based Matrix holding 4 levels of information for each frog at each time
# 1 is size, 2 is infection, 3 is seasonal pattern, 4 is actual season
fABM <- array(0, dim=c(Frog_max,4,tmax)) 

### Setting up simulations
# Initial Conditions
tmp = 1:Fr
fABM[tmp,3,1:tmax] = 1 # No seasons
tmp = (tmp[Fr]+1):(tmp[Fr]+Fr)
fABM[tmp,3,1:tmax] = 2 # NA
tmp = (tmp[Fr]+1):(tmp[Fr]+Fr)
fABM[tmp,3,1:tmax] = 3 # NA
tmp = (tmp[Fr]+1):(tmp[Fr]+Fr)
fABM[tmp,3,1:tmax] = 4 # NA

# Loop for initial conditions
for (frog in 1:Frog_max){  # for all 100 x 4 frogs
  fABM[frog,1,1:tmax] = 1 # size at birth/egg emergence
  fABM[frog,2,1:tmax] = 1 # no infection at time of birth
  for (B in 1:4){
    if (fABM[frog,3,1] == B){ # storing season for t = 1
      fABM[frog,4,1] = seas(B)[1] 
    }}} # end of initial conditions loop

# Generating vectors to record Number of infected and dead individuals
num.infected <- rep(0,tmax)
num.dead <- rep(0,tmax)
Energy <- c(0,2,4)

### Forward simulation iteration loop
for (t in 1:(tmax-1)){ 
  for (frog in 1:Frog_max){
    ## Probability of Dying Due to Size
    S = fABM[frog,1,t] # current size
    mc.pS <- runif(1,0,1)
    if (mc.pS <= pS(S,Ts,A,b)){
      fABM[frog,1,t:tmax] = 0 
      fABM[frog,2,t:tmax] = 0
      num.dead[t] = num.dead[t] + 1}
    ## Conditional Asking if Frog(x) is still alive; if true proceed
    if (fABM[frog,1,t] != 0){ 
      ds = fABM[frog,3,1]
      C = seas(ds)[t] # season at time = t, conditional on seasonal pattern (ds)
      fABM[frog,4,t] = C
      mcE <- sample(1:3,1,prob = pE[C,])
      E = Energy[mcE]
      ## States at current time
      S = fABM[frog,1,t] # current size
      I = fABM[frog,2,t] # current infection level
      ## Strategy based on current states and seasonal pattern 
      if (ds == 1){i = d1[S,I,t]} # D-D-W-W
      if (ds == 2){i = d2[S,I,t]} # W-W-D-D
      if (ds == 3){i = d3[S,I,t]} # D-W-W-D
      if (ds == 4){i = d4[S,I,t]} # W-D-D-W
      ## State changes based on optimal decisions 
      fABM[frog,1,(t+1)] = strat(i,S,I,E)$S
      fABM[frog,2,(t+1)] = strat(i,S,I,E)$I
      ## Conditional Asking if frog are not infected 
      if (fABM[frog,2,t] == 1){
        mcI <- runif(1, min=0, max=1) # Monte Carlo to determines if frog gets infected
        if (mcI <= pI[C]){
          fABM[frog,2,(t+1)] = 2 #frog gets infected
          num.infected[t] = num.infected[t] + 1 } # print(c("Infected:","Size:",S,"time:",t))
        else{fABM[frog,2,(t+1)] = 1 #frog does not get infected
        }} # end if frogs are not infected
      ## Monte Carlo simulation to determine if Frog(x) died from infection
      mcD <- runif(1,0,1)
      if (mcD <= pD[I]){
        num.dead[t] = num.dead[t] + 1
        print(c("Frog", frog,"Died"))
        fABM[frog,1,t:tmax] = 0 # if frog dies, size = 0 and will be excluded from the rest of the simulation
        fABM[frog,2,t:tmax] = 0
      }}
    else{ # end condition that frog are still alive
      fABM[frog,1,t:tmax] = 0 
      fABM[frog,2,t:tmax] = 0
    } # end frog is dead conditional 
  } # end frog loop
} # end of forward simulation

##### Dataframe from forward simulation - base model #####
# Setup Data frame
frog.ID <- 1:Frog_max 
Size <- fABM[,1,1] # scale to whatever measurement precision available
Infection <- ceiling((fABM[,2,1] - 1)/2) # scaled to levels 0-10;
Pattern <- fABM[,3,1] # seasonal pattern for Frog(x)
Times <- rep(1,Frog_max) 

# Dataframe for t = 1
df <- data.frame(frog.ID,Size,Infection,Pattern,Times)

# Loop through times adding information to dataframe (df)
for (t in 2:tmax){
  frog.ID <- 1:Frog_max
  Size <- fABM[,1,t]
  Infection <- ceiling((fABM[,2,t] - 1)/2)
  Pattern <- fABM[,3,t]
  Times <- rep(t,Frog_max)
  tmp <- data.frame(frog.ID,Size,Infection,Pattern,Times) # temporary dataframe for time = t
  df <- rbind(df,tmp) # combine temporary dataframe with main/permanent dataframe
} # end of temporary time loop
summary(df)

### Preparing dataframe for ggplot
forward_base=df
head(forward_base)

# Adding the model to eventually merge dataset from both models
forward_base$Model = rep("Basic",20800)

# Indication if the period is dry or wet based on the seasonatily pattern and time step
forward_base = within (forward_base, Season <- ifelse (Pattern == 1 & Times <= 26, "No seasons",
                                               ifelse(Pattern == 1 & Times >= 27, "No seasons",
                                               ifelse (Pattern == 2 & Times <= 26, "No seasons",
                                               ifelse(Pattern == 2 & Times >= 27, "No seasons",
                                               ifelse (Pattern == 3 & Times <= 13, "No seasons",
                                               ifelse(Pattern == 3 & Times >= 14 & Times <= 39, "No seasons",
                                               ifelse(Pattern == 3 & Times >= 40, "No seasons",
                                               ifelse (Pattern == 4 & Times <= 13,"No seasons",
                                               ifelse(Pattern == 4 & Times >= 14 & Times <= 39, "No seasons",
                                               ifelse (Pattern == 4 & Times >= 40, "No seasons",NA)))))))))))


# Indication order if the seasons on each pattern
forward_base = within (forward_base, Periods <- ifelse (Pattern == 1, "No seasons",
                                                ifelse (Pattern == 2, "NA",
                                                ifelse(Pattern == 3, "NA",
                                                ifelse (Pattern == 4,"NA",NA)))))
forward_base$Period = factor(forward_base$Periods)

forward_base$Size = as.integer(forward_base$Size)

forward_base = subset(forward_base,Pattern == 1)

# Exporting dataframe so we don't need to run the model every time
write.csv(forward_base,file="forward_base.csv")

### Calculating when individuals die
dead_base= as.data.frame(forward_base %>%
                             group_by(Pattern,frog.ID) %>%
                             mutate(MaxSize=max(Size),MaxTime=max(Times[Size == max(Size)])) %>%
                             filter(MaxTime != 52))
dead_base
write.csv(dead_base,file="dead_base.csv")

# changing size 0 in dead aindividuals = for NA so they don't appear in the graphs
forward_base = within (forward_base, Size <- ifelse (Size == 0, NA,Size))

### Plot for visualization
ggplot(data=forward_base,aes(x=Times, y=Size, color=factor(round(Infection)), size=(Infection), group=frog.ID))+
  geom_line(size=0.75)+
  geom_point(data=dead_base,aes(x=MaxTime, y=MaxSize),size=1, shape=4,color="black")+
  theme_bw()+
  theme(panel.grid = element_blank(), #elimina las lineas grandes dentro del plot
        axis.text=element_text(size=14),axis.title=element_text(size=14), #cambia el tamano del texto de los ejes y de los labels de los ejes
        legend.position = "right",legend.title = element_text(size=10),
        strip.text = element_text(size=10))+
  facet_grid(~Model)+
  scale_color_manual(name="Level of \n infection",values=c("grey80", "#e6d1cd", "#cca29b", "#b37469", "#994537", "#801705"))+
  scale_size_continuous(range = c(0.1,0.5),guide = "none")+
  ylim(c(0,120))+
  geom_vline(xintercept = c(13,26,39), linetype="solid", 
             color = "black", size=0.1)

ggsave("Forward_Basic.tiff", units="in", width=8.5, height=11, dpi=300, 
       compression = 'lzw')

############### Seasonality model ###############
##### Parameters for the seasonality model #####

### Probability of dying due to size (same as in the basic)
# The probability of survival is based in a negative logistic curve
# It is the same as in the base model
pS <- function(S,Ts,A,b){ # S=size, Ts=medium, A=probability of dying at smallest size, b=rate of decrease
  (A/(1 + exp(b*(S-Ts))))}
Ts = 50 # i.e., Size 50 is the medium prob of dying
A = 0.8/Ts # frogs at Size = 1 have an 80% chance of dying at each time step
b = 0.05 # the rate of decrease
plot(pS(Ss,Ts,A,b)~Ss)

### Probability of dying from infection based on the code, but biologically is I-1 (same as in the basic)
# It is the same as in the base model
pD <- c(0.00, # Non-infected I=1 
        0.00009,0.00009, # Infected I=2,3
        0.0009,0.0009, # Infected I=4,4
        0.009,0.009, # Infected I=6,7
        0.09,0.09, # Infected I=8,9
        0.9,0.9) # Infected I=10,11

# Probability of finding 0, 2, or 4 energy units
# Season determines the energy gains per week, those energies are distributed among the behaviors
pE <- array(0,dim = c(2,3))
pE[1,] = c(0.45,0.45,0.1) # Dry Season: p(E==0,E==2,E==4)
pE[2,] = c(0.1,0.45,0.45) # Wet Season: p(E==0,E==2,E==4)

### Probability of getting infected: dry = [1], wet = [2]
# Season determine the probability og getting infected
pI <- c(0.31 , 0.52) 

##### Terminal Fitness Loop #####
# Terminal fitness function is defined in here, but included in the backward iteration loop
# terminal fitness is in function of both, size and level of infection
G <- function(I){
  exp(-0.75*(10-I-1))
}   

for (S in 1:Smax){
  for (I in 1:11){
    f[S,I,tmax] = (1-G(I))*phi(S) #(1-G(I)) is the probability of surviving with the current infection level
  }
}

plot(f[Ss,1,tmax]~Ss, type = "l", ylim = c(-3.5, 1),col="grey30")
lines(f[Ss,2,tmax]~Ss,col="grey")
lines(f[Ss,3,tmax]~Ss,col="orange")
lines(f[Ss,4,tmax]~Ss,col="orange")
lines(f[Ss,5,tmax]~Ss,col="darkorange")
lines(f[Ss,6,tmax]~Ss,col="darkorange")
lines(f[Ss,7,tmax]~Ss,col="red")
lines(f[Ss,8,tmax]~Ss,col="red")
lines(f[Ss,9,tmax]~Ss,col="darkred")
lines(f[Ss,10,tmax]~Ss,col="darkred")
lines(f[Ss,11,tmax]~Ss,col="darkred")
# Exported as png

##### Strategies functions #####
## How state variables (Size = S and Infection = I) change based on 
# 1) the strategies (i): 1=grow, 2=suppress infection, or 3=both
# 2) the available energy (E)

strat <- function(i,S,I,E){
  nI = I
  if (E == 0){ # no energy to allocate
    if (i==1){ 
      S = S 
      I = I + 1 - 0}
    if (i==2){ 
      S = S 
      I = I + 1 - 0}
    if (i==3){ 
      S = S  
      I = I + 1 - 0}} # end of function for E = 0
  if (E == 2){ # 2 energy units to allocate
    if (i==1){ # invest 2 units in growth and 0 in suppress infection
      S = S + 2
      I = I + 1 - 0}
    if (i==2){ # invest 1 units in growth and 1 in suppress infection
      S = S + 1
      I = I + 1 - 1}
    if (i==3){ # invest 0 units in growth and 2 in suppress infection
      S = S + 0 
      I = I + 1 - 2}} # end of function for E = 2
  if (E == 4){ # 4 energy units to allocate
    if (i==1){ # invest 4 units in growth and 0 in suppress infection
      S = S + 4
      I = I + 1 - 0}
    if (i==2){ # invest 2 units in growth and 2 in suppress infection
      S = S + 2
      I = I + 1 - 2}
    if (i==3){ # invest 0 units in growth and 4 in suppress infection
      S = S + 0
      I = I + 1 - 4}} # end of function for E = 4
  S = min(S,Smax)
  S = max(S,1)
  I = min(I,Imax)
  I = max(I,1)
  if (nI == 1){ # if it is not infected will remain like that
    I = 1 }
  return(list("S"=S,"I"=I))} # end of the strategy function

##### Seasonal time function #####
# vector space 1 = dry, vector space 2 = wet

seas <- function(dS){ # t = time, dS = seasonal distribution
  if (dS==1){ # cool season first half of year
    t.D = rep(1,times=tmax/2)
    t.W = rep(2, times=tmax/2)
    seas = c(t.D,t.W)} # end if (dS == 1)
  if (dS==2){ # warm season first half of year
    t.D = rep(1,times=tmax/2)
    t.W = rep(2, times=tmax/2)
    seas = c(t.W,t.D)} # end if (dS == 2)
  if (dS==3){ # cool season first and last quarter
    t.D = rep(1, times=tmax/4)
    t.W = rep(2, times=tmax/2)
    seas = c(t.D,t.W,t.D)} # end if (dS == 3)
  if (dS==4){ ## cool season middle half of year
    t.W = rep(2, times=tmax/4)
    t.D = rep(1, times=tmax/2)
    seas = c(t.W,t.D,t.W)} # end if (dS == 4)
  return(seas)} # end of seasonal time function

##### Backward iteration #####

## Backward iteration - base loop
for (B in 1:4){ # referring to the four seasonality scenarios
  for (S in 1:Smax){  # start of terminal Fitness Loop
    for (I in 1:11){
      f[S,I,tmax] = (1-(pD[I]*tmax))*phi(S)}} # end of the terminal fitness loop
  for (t in (tmax-1):1){ # start backward time loop
    for (S in Smax:1){ # start size loop
      for (I in Imax:1){ # start level of infection loop
        for (i in 1:3){ # start of the strategies loop
          C = seas(B)[t] # season at time = t
          dS0 = strat(i,S,I,0)$S #change in size energy = 0
          dS2 = strat(i,S,I,2)$S #change in size energy = 2
          dS4 = strat(i,S,I,4)$S #change in size energy = 4
          dI0 = strat(i,S,I,0)$I #change in infection level energy = 0
          dI2 = strat(i,S,I,2)$I #change in infection level energy = 2
          dI4 = strat(i,S,I,4)$I #change in infection level energy = 4
          # If there is no infection; just v1
          if (I==1){
            v[S,1,t,1] = (1-pS(S,Ts,A,b)) *
              (pE[C,1]*(pI[C]*((1-pD[2])*f[dS0,dI0,(t+1)]) +
                         (1-pI[C])*((1-pD[1])*f[dS0,dI0,(t+1)])) +
              pE[C,2]*(pI[C]*((1-pD[2])*f[dS2,dI2,(t+1)]) +
                         (1-pI[C])*((1-pD[1])*f[dS2,dI2,(t+1)])) +
              pE[C,3]*(pI[C]*((1-pD[2])*f[dS4,dI4,(t+1)]) +
                         (1-pI[C])*((1-pD[1])*f[dS4,dI4,(t+1)])))
            v[S,1,t,2] = 0
            v[S,1,t,3] = 0} # end of no infection
          # If there is infection; v1, v2, v3
          else{ 
            v[S,I,t,i] = (1-pS(S,Ts,A,b))*
              (pE[C,1]*(1-pD[dI0])*f[dS0,dI0,(t+1)] +
              pE[C,2]*(1-pD[dI2])*f[dS2,dI2,(t+1)] +
              pE[C,3]*(1-pD[dI4])*f[dS4,dI4,(t+1)])
          } # end if existing infection
        } # end strategy loop (i)
        f[S,I,t] = max(v[S,I,t,1:3]) # set max fitness
        for (i in 3:1){ # start decision matrix loop
          if (f[S,I,t] == max(v[S,I,t,i])){
            d[S,I,t] = i}
        } # end decision matrix loop
      } # end infection level loop
    } # end size loop
  } # end backward time loop
  # Different Decision Matrices for the Different Seasonal Patterns #
  if (B == 1){d1 = d} # Scenario C-C-W-W
  if (B == 2){d2 = d} # Scenario W-W-C-C
  if (B == 3){d3 = d} # Scenario C-W-W-C
  if (B == 4){d4 = d} # Scenario W-C-C-W
  # Convert current decision matrix to dataframe
  df <- as.data.frame(as.table(d))
  # convert elements of dataframe from factor to numeric 
  df$Size = as.numeric(df$Size)
  df$times = as.numeric(df$times)
  df$Infection = as.factor(df$Infection) # converted to factor for the sake of making heat map
} # End of the backward iteration loop

##### Decision matrices for each seasonal scenario - seasonality model #####

### Extracting and manipulating data to plot matrices
df1 <- as.data.frame(as.table(d1))
df1$Scenarios= (rep("C-C-W-W", length(d1)))
df1$Model= (rep("Seasons", length(d1)))
df2 <- as.data.frame(as.table(d2))
df2$Scenarios= (rep("W-W-C-C", length(d2)))
df2$Model= (rep("Seasons", length(d2)))
df3 <- as.data.frame(as.table(d3))
df3$Scenarios= (rep("C-W-W-C", length(d3)))
df3$Model= (rep("Seasons", length(d3)))
df4 <- as.data.frame(as.table(d4))
df4$Scenarios= (rep("W-C-C-W", length(d4)))
df4$Model= (rep("Seasons", length(d4)))

### Binding data from four seasonal scenarios
df_season=rbind(df1,df2,df3,df4)
df_season$Infection_level = as.numeric(df_season$Infection) - 1
df_season$Strategy <- as.factor(ifelse(df_season$Freq == 1, "Grow",
                                ifelse(df_season$Freq == 2, "Grow/Control infection", 
                                       ifelse(df_season$Freq == 3, "Control infection", NA))))
df_season$Strategy <- factor(df_season$Strategy,levels = c("Grow","Grow/Control infection","Control infection"))

df_season$Size = as.integer(df_season$Size)

# Exporting decision matrices dataframe so we don't have to run the model every time
write.csv(df_season,file="decisions_season.csv")

### Plot decision matrices (for visualization only)
ggplot(data = df_season, aes(x = times, y = Size, color = Strategy))+
  geom_point(shape=15)+
  facet_grid(Infection_level ~ Scenarios)+
  theme_bw()+ 
  theme(legend.position = "bottom",panel.grid = element_blank(), 
        legend.title=element_text(size=14), 
        legend.text=element_text(size=14),
        axis.text=element_blank(),
        axis.title=element_text(size=16),
        strip.text = element_text(size=14),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  scale_color_manual(values=met.brewer("NewKingdom", 3), name = "Strategy:")+
  labs (x = "Time (wks)", y = "Size (states)")+
  geom_vline(xintercept = c(13,26,39), linetype="solid", color = "black", size=0.1)+
  guides(color = guide_legend(override.aes = list(size = 5)))
ggsave("Stategy_Seasons.tiff", units="in", width=8.5, height=11, dpi=300, 
       compression = 'lzw')

##### Forward Simulation #####

### Setting the seed so both models use the same Monte Carlo random values
set.seed(854354)

### Parameters for the simulation
# Arbitrary Frog Number; 100 frogs per seasonal scenario
Fr <- 100
Frog_max <- Fr * 4
## Frog Agent Based Matrix holding 4 levels of information for each frog at each time
# 1 is size, 2 is infection, 3 is seasonal pattern, 4 is actual season
fABM <- array(0, dim=c(Frog_max,4,tmax)) 

### Setting up simulations
# Initial Conditions
tmp = 1:Fr
fABM[tmp,3,1:tmax] = 1 # C-C-W-W
tmp = (tmp[Fr]+1):(tmp[Fr]+Fr)
fABM[tmp,3,1:tmax] = 2 # W-W-C-C
tmp = (tmp[Fr]+1):(tmp[Fr]+Fr)
fABM[tmp,3,1:tmax] = 3 # C-W-W-C
tmp = (tmp[Fr]+1):(tmp[Fr]+Fr)
fABM[tmp,3,1:tmax] = 4 # W-C-C-W

# Loop for initial conditions
for (frog in 1:Frog_max){  # for all 100 x 4 frogs
  fABM[frog,1,1:tmax] = 1 # size at birth/egg emergence
  fABM[frog,2,1:tmax] = 1 # no infection at time of birth
  for (B in 1:4){
    if (fABM[frog,3,1] == B){ # storing season for t = 1
      fABM[frog,4,1] = seas(B)[1] 
      }}} # end of initial conditions loop

# Generating vectors to record Number of infected and dead individuals
num.infected <- rep(0,tmax)
num.dead <- rep(0,tmax)
Energy <- c(0,2,4)

### Forward simulation iteration loop
for (t in 1:(tmax-1)){ 
  for (frog in 1:Frog_max){
    ## Probability of Dying Due to Size
    S = fABM[frog,1,t] # current size
    mc.pS <- runif(1,0,1)
    if (mc.pS <= pS(S,Ts,A,b)){
      fABM[frog,1,t:tmax] = 0 
      fABM[frog,2,t:tmax] = 0
      num.dead[t] = num.dead[t] + 1}
    ## Conditional Asking if Frog(x) is still alive; if true proceed
    if (fABM[frog,1,t] != 0){ 
      ds = fABM[frog,3,1]
      C = seas(ds)[t] # season at time = t, conditional on seasonal pattern (ds)
      fABM[frog,4,t] = C
      mcE <- sample(1:3,1,prob = pE[C,])
      E = Energy[mcE]
    ## States at current time
      S = fABM[frog,1,t] # current size
      I = fABM[frog,2,t] # current infection level
    ## Strategy based on current states and seasonal pattern 
      if (ds == 1){i = d1[S,I,t]} # C-C-W-W
      if (ds == 2){i = d2[S,I,t]} # W-W-C-C
      if (ds == 3){i = d3[S,I,t]} # C-W-W-C
      if (ds == 4){i = d4[S,I,t]} # W-C-C-W
    ## State changes based on optimal decisions 
      fABM[frog,1,(t+1)] = strat(i,S,I,E)$S
      fABM[frog,2,(t+1)] = strat(i,S,I,E)$I
    ## Conditional Asking if frog are not infected 
      if (fABM[frog,2,t] == 1){
        mcI <- runif(1, min=0, max=1) # Monte Carlo to determines if frog gets infected
        if (mcI <= pI[C]){
          fABM[frog,2,(t+1)] = 2 #frog gets infected
          num.infected[t] = num.infected[t] + 1 } # print(c("Infected:","Size:",S,"time:",t))
        else{fABM[frog,2,(t+1)] = 1 #frog does not get infected
        }} # end if frogs are not infected
    ## Monte Carlo simulation to determine if Frog(x) died from infection
      mcD <- runif(1,0,1)
      if (mcD <= pD[I]){
        num.dead[t] = num.dead[t] + 1
        print(c("Frog", frog,"Died"))
        fABM[frog,1,t:tmax] = 0 # if frog dies, size = 0 and will be excluded from the rest of the simulation
        fABM[frog,2,t:tmax] = 0
      }}
    else{ # end condition that frog are still alive
      fABM[frog,1,t:tmax] = 0 
      fABM[frog,2,t:tmax] = 0
    } # end frog is dead conditional 
  } # end frog loop
} # end of forward simulation

##### Dataframe from forward simulation - seasonality model #####
# Setup Data frame
frog.ID <- 1:Frog_max 
Size <- fABM[,1,1] # scale to whatever measurement precision available
Infection <- ceiling((fABM[,2,1] - 1)/2) # scaled to levels 0-5;
Pattern <- fABM[,3,1] # seasonal pattern for Frog(x)
Times <- rep(1,Frog_max) 

# Dataframe for t = 1
df <- data.frame(frog.ID,Size,Infection,Pattern,Times)

# Loop through times adding information to dataframe (df)
for (t in 2:tmax){
  frog.ID <- 1:Frog_max
  Size <- fABM[,1,t]
  Infection <- ceiling((fABM[,2,t] - 1)/2)
  Pattern <- fABM[,3,t]
  Times <- rep(t,Frog_max)
  tmp <- data.frame(frog.ID,Size,Infection,Pattern,Times) # temporary dataframe for time = t
  df <- rbind(df,tmp) # combine temporary dataframe with main/permanent dataframe
} # end of temporary time loop
summary(df)

### Preparing dataframe for ggplot
forward_season=df
head(forward_season)

# Adding the model to eventually merge dataset from both models
forward_season$Model = rep("Seasons",20800)

# Indication if the period is dry or wet based on the seasonatily pattern and time step
forward_season = within (forward_season, Season <- ifelse (Pattern == 1 & Times <= 26, "Cool",
                           ifelse(Pattern == 1 & Times >= 27, "Warm",
                           ifelse (Pattern == 2 & Times <= 26, "Warm",
                           ifelse(Pattern == 2 & Times >= 27, "Cool",
                           ifelse (Pattern == 3 & Times <= 13, "Cool",
                           ifelse(Pattern == 3 & Times >= 14 & Times <= 39, "Warm",
                           ifelse(Pattern == 3 & Times >= 40, "Cool",
                           ifelse (Pattern == 4 & Times <= 13,"Warm",
                           ifelse(Pattern == 4 & Times >= 14 & Times <= 39, "Cool",
                           ifelse (Pattern == 4 & Times >= 40, "Warm",NA)))))))))))

# Indication order if the seasons on each pattern
forward_season = within (forward_season, Periods <- ifelse (Pattern == 1, "C-C-W-W",
                                                            ifelse (Pattern == 2, "W-W-C-C",
                                                                    ifelse(Pattern == 3, "C-W-W-C",
                                                                           ifelse (Pattern == 4,"W-C-C-W",NA)))))
forward_season$Period = factor(forward_season$Periods, levels = c("C-C-W-W","C-W-W-C","W-W-C-C","W-C-C-W"))

forward_season$Size = as.integer(forward_season$Size)

# Exporting dataframe so we don't need to run the model every time
write.csv(forward_season,file="forward_season.csv")

### Calculating when individuals die
dead_season= as.data.frame(forward_season %>%
  group_by(Pattern,frog.ID) %>%
  mutate(MaxSize=max(Size),MaxTime=max(Times[Size == max(Size)])) %>%
  filter(MaxTime != 52))
dead_season
write.csv(dead_season,file="dead_season.csv")
# changing size 0 in dead aindividuals = for NA so they don't appear in the graphs
forward_season = within (forward_season, Size <- ifelse (Size == 0, NA,Size))

### Plot
ggplot(data=forward_season,aes(x=Times, y=Size, color=factor(round(Infection)), size=(Infection), group=frog.ID))+
  geom_line(size=0.75)+
  geom_point(data=dead_season,aes(x=MaxTime, y=MaxSize),size=1, shape=4,color="black")+
  theme_bw()+
  theme(panel.grid = element_blank(), #elimina las lineas grandes dentro del plot
        axis.text=element_text(size=14),axis.title=element_text(size=14), #cambia el tamano del texto de los ejes y de los labels de los ejes
        legend.position = "right",legend.title = element_text(size=10),
        strip.text = element_text(size=10))+
  facet_grid(Period~Model)+
  scale_color_manual(name="Level of \n infection",values=c("grey80", "#e6d1cd", "#cca29b", "#b37469", "#994537", "#801705"))+
  scale_size_continuous(range = c(0.1,0.5),guide = "none")+
  ylim(c(0,120))+
  geom_vline(xintercept = c(13,26,39), linetype="solid", 
               color = "black", size=0.1)

ggsave("Forward_Seasons.tiff", units="in", width=8.5, height=11, dpi=300, 
       compression = 'lzw')

############### Comparing models ###############
##### Calling and merging dataframes #####
# Decision matrix
decisions_base = read.csv("decisions_base.csv")
#decisions_base = subset(decisions_base, Scenarios == "No seasons")
decisions_base$Scenario = decisions_base$Model
decisions_season = read.csv("decisions_season.csv")
decisions_season$Scenario = decisions_season$Scenarios
decisions_matrix = rbind(decisions_base, decisions_season)
table(decisions_matrix$Scenario)
decisions_matrix$Strategy = factor (decisions_matrix$Strategy, levels = c("Grow","Grow/Control infection","Control infection"))
decisions_matrix$Scenario = factor(decisions_matrix$Scenario, levels = c("Basic","C-C-W-W","C-W-W-C","W-W-C-C","W-C-C-W"))
decisions_matrix=decisions_matrix %>% mutate(Sizes = ifelse(Size == 0, NA,
                                                          ifelse(Size <= 50, ((Size-1)*0.449 + 6),
                                                                 ifelse(Size > 50, (6 + 49*0.449 + (Size-50)*0.22), 0))))

dead_base = read.csv("dead_base.csv")
#dead_base = dead_base[2:14]
dead_season = read.csv("dead_season.csv")

# Forward iteration results
forward_base = read.csv("forward_base.csv")
forward_season = read.csv("forward_season.csv")
forward_results = rbind (forward_base, forward_season)
table(forward_results$Periods)
forward_results$Periods = factor(forward_results$Periods, levels = c("No seasons","C-C-W-W","C-W-W-C","W-W-C-C","W-C-C-W"))
forward_results$Period = factor(forward_results$Period, levels = c("No seasons","C-C-W-W","C-W-W-C","W-W-C-C","W-C-C-W"))
forward_results=forward_results %>% mutate(Sizes = ifelse(Size == 0, NA,
                                              ifelse(Size <= 50, ((Size-1)*0.449 + 6),
                                                     ifelse(Size > 50, (6 + 49*0.449 + (Size-50)*0.22), 0))))

forward_dead = rbind(dead_base, dead_season)
table(forward_dead$Periods)
forward_dead$Periods = factor(forward_dead$Periods, levels = c("No seasons","C-C-W-W","C-W-W-C","W-W-C-C","W-C-C-W"))
forward_dead=forward_dead %>% mutate(Sizes = ifelse(MaxSize == 0, NA,
                                                    ifelse(MaxSize <= 50, ((MaxSize-1)*0.449 + 6),
                                                           ifelse(MaxSize > 50, (6 + 49*0.449 + (MaxSize-50)*0.22), 0))))

##### Figure Decision matrices  (Fig 3) #####
head(decisions_matrix)
summary(decisions_matrix)
str(decisions_matrix)

### Decision matrix (Fig 4)
p4 = ggplot(data = decisions_matrix, aes(x = times, y = Size, color = Strategy))+
  geom_point(shape=15)+
  facet_grid(Infection_level ~ Scenario,
             labeller = labeller(
               Scenario= ~ paste0("(",letters[as.numeric(factor(.x))],") ",{as.factor(factor(.x))}),
                Infection_level = ~ paste0({as.factor(factor(.x))})))+
  theme_bw()+ 
  theme(legend.position = "bottom",panel.grid = element_blank(), 
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        axis.text=element_blank(),
        axis.title=element_text(size=16),
        strip.text.x = element_text(size=14, hjust = 0.1),
        strip.text.y = element_text(size=14, angle = 0),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  scale_color_manual(values=met.brewer("NewKingdom", 3), name = "Strategy:")+
  labs (x = "Time (wks)", y = "Size (states)")+
  geom_vline(xintercept = c(13,26,39), linetype="solid", color = "grey40", size=0.25)+
  guides(color = guide_legend(override.aes = list(size = 5)))
p4

ggsave("Fig3_DecisionMatrices.tiff", units="in", width=8, height=7, dpi=300, compression = 'lzw')
# Tradeoff bar was added externally

##### Subset at week 52 for fitness #####

# Filtering week 52 and alive individuals
week52= as.data.frame(forward_results %>%
                        filter(Times == 52) %>%
                        mutate(Survived = ifelse(Size == 0 , "No",
                                          ifelse(Size >= 1 , "Yes", NA))))
week52

##### Forward simulations (Fig 4a and S2) #####

# changing size 0 for NA so they don't appear in the graphs
forward_results = within (forward_results, Size <- ifelse (Size == 0, NA,Size))

# Changing labels so the seasons fit inside rectangles in the graph
forward_results$Periods <- factor(forward_results$Periods, levels = c("No seasons","C-C-W-W","C-W-W-C","W-W-C-C","W-C-C-W"), 
                              labels = c("",
                                         "C               C              W               W", 
                                         "C               W              W               C", 
                                         "W               W              C               C",
                                         "W               C              C               W"))
forward_dead$Periods <- factor(forward_dead$Periods, levels = c("No seasons","C-C-W-W","C-W-W-C","W-W-C-C","W-C-C-W"), 
                           labels = c("",
                                      "C                   C                  W                    W", 
                                      "C                   W                  W                    C", 
                                      "W                   W                  C                    C",
                                      "W                   C                  C                    W"))

forward_results$Period <- factor(forward_results$Period, levels = c("No seasons","C-C-W-W","C-W-W-C","W-W-C-C","W-C-C-W"))
forward_dead$Period <- factor(forward_dead$Period, levels = c("No seasons","C-C-W-W","C-W-W-C","W-W-C-C","W-C-C-W"))
# Forward simulations (Fig 6)
forward_sim = ggplot(data=forward_results,aes(x=Times, y=Size))+
  geom_vline(xintercept = c(13,26,39), linetype="solid", color = "black", size=0.1)+
  geom_hline(yintercept = 50, linetype="dashed", color = "black", size=0.3)+
  geom_line(data=forward_results,aes(group=frog.ID, color=factor(round(Infection))),size=0.5)+
  geom_text(aes(label = Periods), x = 5, y = 80, hjust = 0, vjust = 0, size = 3.5) +
  geom_smooth(color="black")+
  geom_point(data=forward_dead,aes(x=MaxTime, y=MaxSize),size=1, shape=4,color="black")+
  theme_bw()+
  facet_grid(Period~.)+
  theme(panel.grid = element_blank(), #elimina las lineas grandes dentro del plot
        axis.text=element_text(size=13),
        axis.title=element_text(size=13), #cambia el tamano del texto de los ejes y de los labels de los ejes
        legend.position = "bottom",
        legend.title = element_text(size=13),
        legend.text =  element_text(size=13),
        strip.text.x =  element_text(size=13),
        strip.text.y = element_blank(),
        strip.background = element_blank())+
  #scale_color_manual(name="Level of \n infection",values=c("grey80", "#e6d1cd", "#cca29b", "#b37469", "#994537", "#801705"))+
  scale_color_manual(name="Level of \n infection",values=c("grey90","#fbc2a9", "#ee956a","#da6c42", "#973d21", "#6b200c"))+
  ylim(c(0,90))+ labs(x="Time (wks)", y="Size (states)")+
  guides(color = guide_legend(nrow = 3, byrow = T))
forward_sim

forward_sim=tag_facet(forward_sim, open = "(", close = ")",tag_pool = letters)
forward_sim

# Forward simulations (Fig 4a)

forward_sim2 = ggplot(data=forward_results,aes(x=Times, y=Sizes))+
  geom_vline(xintercept = c(13,26,39), linetype="solid", color = "black", size=0.1)+
  geom_hline(yintercept = 28, linetype="dashed", color = "black", size=0.3)+
  geom_line(data=forward_results,aes(group=frog.ID, color=factor(round(Infection))),size=0.5)+
  geom_text(aes(label = Periods), x = 5, y = 35, hjust = 0, vjust = 0, size = 3.5) +
  geom_smooth(color="black")+
  geom_point(data=forward_dead,aes(x=MaxTime, y=Sizes),size=1, shape=4,color="black")+
  theme_bw()+
  facet_grid(Period~.)+
  theme(panel.grid = element_blank(), #elimina las lineas grandes dentro del plot
        axis.text=element_text(size=13),
        axis.title=element_text(size=13), #cambia el tamano del texto de los ejes y de los labels de los ejes
        legend.position = "bottom",
        legend.title = element_text(size=13),
        legend.text =  element_text(size=13),
        strip.text.x =  element_text(size=13),
        strip.text.y = element_blank(),
        strip.background = element_blank())+
  #scale_color_manual(name="Level of \n infection",values=c("grey80", "#e6d1cd", "#cca29b", "#b37469", "#994537", "#801705"))+
  scale_color_manual(name="Level of \n infection",values=c("grey90","#fbc2a9", "#ee956a","#da6c42", "#973d21", "#6b200c"))+
  ylim(c(0,40))+ labs(x="Time (wks)", y="Size (SVL in mm)")+
  guides(color = guide_legend(nrow = 3, byrow = T))
forward_sim2

forward_sim2 = tag_facet(forward_sim2, open = "(", close = ")",tag_pool = letters)
forward_sim2

ggsave("FigS2_Forward_simulation_SVL.tiff", units="in", width=3.5, height=10, dpi=300, compression = 'lzw')

##### Growth rate ~ infection level (Fig 4b) #####

head(forward_results)
table(forward_results$Season)

forward_results = within (forward_results, Time_period <- ifelse (Times >= 1 & Times <=13, "Period1",
                                            ifelse (Times >= 14 & Times <=26, "Period2",
                                            ifelse (Times >= 27 & Times <=39, "Period3", 
                                            ifelse (Times >= 40 & Times <=52, "Period4",NA)))))

rates=as.data.frame(forward_results %>%
                      group_by(Model,Time_period,Season,frog.ID) %>%
                      mutate (meanInfection = mean(Infection),
                              GrowthRate= (max(Size)-min(Size))/13))
rates
rates$Time_period = factor(rates$Time_period)

rates$Season = factor(rates$Season, levels = c("No seasons","Cool","Warm"))
str(rates)

# Growth rate ~ mean infection level (Fig 7)

g_rates=ggplot(data=rates, aes(x = meanInfection, y = GrowthRate))+
  geom_point(size=1.5,aes(shape = Season, fill = Season),alpha = 0.1)+
  scale_shape_manual(values = c(21,21,21))+
  theme_bw()+
  facet_grid(Period ~ .)+
  labs(x="Mean level of infection", y= "Growth rate (13 wks)")+
  scale_fill_manual(values=c("grey90","#00A9FF","#F8766D"))+
  theme(panel.grid = element_blank(), #elimina las lineas grandes dentro del plot
        axis.text=element_text(size=13),
        axis.title=element_text(size=13), #cambia el tamano del texto de los ejes y de los labels de los ejes
        legend.position = "bottom",
        legend.box="vertical",
        legend.title = element_text(size=13),
        legend.text =  element_text(size=13),
        strip.text.x =  element_text(size=13),
        strip.text.y = element_text(size=13, angle = 0),
        strip.background = element_blank())+ 
  guides(fill = guide_legend(nrow = 3, byrow = T),
         shape = guide_legend(override.aes = list(alpha = 1),nrow = 3, byrow = T))
g_rates

g_rates=tag_facet(g_rates, open = "(", close = ")",tag_pool = letters[6:10])
g_rates

##### Fig 4 - forward simulations & Growth rate ~ infection level #####
new = forward_sim +  g_rates
new

ggsave("Fig4_Forward_GrowthRates-test.tiff", units="in", width=11, height=10, dpi=300, compression = 'lzw')


######## Fitness figure composition for the paper #####
##### Fitness: time to maturity (Fig 5) #####

str(week52)
# Calculating time to maturity for each individual
time_maturity=as.data.frame(forward_results %>%
                              filter(Size == 50) %>%
                              group_by(Model,Period,frog.ID) %>%
                              mutate(MinTime=min(Times))%>%
                              summarise(Min = min(MinTime)))
maturity_n = as.data.frame(time_maturity %>%
                             group_by(Model,Period,.drop = FALSE) %>%
                             summarise(Maturity_n = n(),
                                       mean_time = mean(Min)))
maturity_n

maturity_n = subset(maturity_n, Maturity_n > 0)
# Plot
maturity=ggplot(data=time_maturity, aes(x=Min, y=reorder(Period, desc(Period)),group=Period))+
  geom_boxplot(alpha=0.8, fill ="grey50", color="grey50")+
  #geom_jitter(height = 0.2, color = "grey40")+
  geom_point(data=maturity_n, aes(x=mean_time), size = 3)+
  labs(x="Size at week 52", y="Frequency")+
  scale_y_discrete (position = 'right')+
  #facet_grid(.~Model)+
  theme_bw()+ labs(x = "Time to maturity (wks)", y = "")+
  theme(panel.grid = element_blank(), #elimina las lineas grandes dentro del plot
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=0),
        axis.title=element_text(size=12), #cambia el tamano del texto de los ejes y de los labels de los ejes
        legend.position = "right",
        legend.title = element_text(size=10),
        strip.text = element_text(size=12) ,
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"))+
  xlim(10,52)+
  geom_text(data = maturity_n, aes(x = 13, y=Period, label=paste("n",Maturity_n)))
maturity

maturity= tag_facet(maturity, open = "(", close = ")",tag_pool = "a")

##### Fitness: size at week 52  #####

# Filtering week 52 and dead individuals
size_w52 = forward_results %>%
  filter(Times == 52, Size != 0)
mean_size_52 = size_w52 %>%
  group_by(Model,Period,.drop = FALSE) %>%
  summarise(mean_size = mean(Size))

# Plot
size52=ggplot(data=size_w52, aes(x=Size, y=reorder(Period, desc(Period))))+
  geom_boxplot(alpha=0.8, fill ="grey50", color="grey50")+
  #geom_jitter(height = 0.2, color = "grey40")+
  geom_point(data=mean_size_52, aes(x=mean_size), size = 3)+
  labs(x="Size at week 52", y="Frequency")+
  scale_y_discrete (position = 'right')+
  #facet_grid(.~Model)+
  theme_bw()+ labs(x = "Size at week 52", y = "")+
  theme(panel.grid = element_blank(), #elimina las lineas grandes dentro del plot
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=0),
        axis.title=element_text(size=12), #cambia el tamano del texto de los ejes y de los labels de los ejes
        legend.position = "right",
        legend.title = element_text(size=10),
        strip.text = element_text(size=12),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"))
size52
size52 = tag_facet(size52, open = "(", close = ")",tag_pool = "b")


##### Fitness: survival at week 52 #####

# Filtering week 52 and alive individuals
surv52= as.data.frame(forward_results %>%
                      filter(Times == 52) %>%
                      group_by(Model,Period) %>%
                      count(Survive=Size > 1))%>%
  filter(Survive == TRUE)
surv52

# Plot
survival52=ggplot(data=surv52, aes(x=n,y=reorder(Period, desc(Period))))+
  geom_bar(stat="identity", fill="grey60")+
  #facet_grid(~Model)+
  scale_y_discrete (position = 'right')+
  labs(x="Survival at week 52 (%)",y="")+
  xlim(c(-6,60))+
  theme_bw()+
  theme( #elimina las lineas grandes dentro del plot
        axis.text=element_text(size=12),
        axis.title=element_text(size=12), #cambia el tamano del texto de los ejes y de los labels de los ejes
        legend.position = "right",
        legend.title = element_text(size=10),
        strip.text = element_text(size=12) ,
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"))+
  geom_text(label=surv52$n, x=20, nudge_y=0, size=4)
survival52
survival52 = tag_facet(survival52, open = "", close = ")",tag_pool = "C")

##### Fitness: Figure composition #####
fitness = maturity + size52 + survival52
fitness
ggsave("Fig5_Fitness.tiff", units="in", width=7, height=12, dpi=4, compression = 'lzw')
# ggsave did not work. I just saved it as pdf (8x4)



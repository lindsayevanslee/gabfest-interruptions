# This is the original function I wrote that helped me capture the interruption
# data. I ran gabfest.interruptions() at the same time as I started playing the
# episode. The function starts a stopwatch internally and waits for the user to
# give an input each time an interruption occurs. Short instructions pop up when
# the function starts.



#what's an interruption:
#one person talks over another with the purpose of making their own point
#may or may not cause the other person to stop talking

#what's not an interruption:
#exclamations or conversational modes of emphasis, eg "right," "yeah," "that's interesting"


######stopwatch function for recording interruptions during Slate Political Gabfest episodes
gabfest.interruptions=function() {
  
  #initialize variables
  int="start"                                     #stores each new code
  int.code=int                                    #stores all codes for episode
  stopwatch=proc.time()["elapsed"]                #stores all timings for interuptions (in seconds)
  
  #print instructions
  cat("Input Political Gabfest interruptions\n
    Data input: xyz, 'x interrupts y during z'\n
    x = d, j, e, or G for David, John, Emily or guest \n
    y = d, j, e, or G \n
    z = a or n, for argument or normal conversation\n
    Add '???' if point should be revisited.\n
    Type 'pause' when pausing podcast and 'unpause' to start again.\n
    Type 'ad' when an ad starts and 'unad' when the ad ends.\n
    Type 'stop' at end of episode.")
  
  
  while (int!="stop") {
    int=readline("Interruption: ")                  #ask for input of interuption data point
    int.code=c(int.code,int)                        #store interuption code
    
    stopwatch=c(stopwatch,proc.time()["elapsed"])   #store time point
    
  }
  
  ###clean up stopwatch
  names(stopwatch)=NULL                             #erase stopwatch names from proc.time()
  stopwatch=stopwatch-stopwatch[1]                  #subtract out start time
  
  ##subtract pause lengths
  pauses=which(int.code=="pause")                   #store index for pauses
  
  #only deal with pauses if there were actually pauses in int.code
  if (length(pauses)>=1) {
    
    unpauses=which(int.code=="unpause")               #store index for unpauses
    
    pause.len=stopwatch[unpauses]-stopwatch[pauses]   #store pause lengths
    pause.num=length(pause.len)                       #store number of pauses
    stopwatch.num=length(stopwatch)
    
    for (i in 1:pause.num) {
      p.index=pause.num-(i-1)                         #store index for current pause to be subtracted
      sw.index=unpauses[p.index]                      #store index for where to start subtraction in stopwatch
      stopwatch[sw.index:stopwatch.num]=stopwatch[sw.index:stopwatch.num]-pause.len[p.index]    #subtract pause length from sw.index to end of stopwatch vector
    }
    
    stopwatch=stopwatch[-c(pauses,unpauses)]          #delete pause times
    int.code=int.code[-c(pauses,unpauses)]            #delete pause codes
    
  }
  
  ad.index=which(int.code=="ad")
  unad.index=which(int.code=="unad")
  
  ###separate each part of code
  int.er=substr(int.code,1,1)
  int.ee=substr(int.code,2,2)
  int.conv=substr(int.code,3,3)
  
  ##put back start, ad, unad, and stop in each vector
  int.er[1]="start"
  int.er[length(int.code)]="stop"
  int.er[ad.index]="ad"
  int.er[unad.index]="unad"
  
  int.ee[1]="start"
  int.ee[length(int.code)]="stop"
  int.ee[ad.index]="ad"
  int.ee[unad.index]="unad"
  
  int.conv[1]="start"
  int.conv[length(int.code)]="stop"
  int.conv[ad.index]="ad"
  int.conv[unad.index]="unad"
  
  
  ###final data frame
  final.ep.data=data.frame(Time=stopwatch,
                           Interrupter=int.er,
                           Interruptee=int.ee,
                           Conversation=int.conv,
                           stringsAsFactors=FALSE)
  
  return(final.ep.data)
  
}

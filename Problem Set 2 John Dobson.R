#John Dobson
#Problem Set 2

#Selection Sort Function

selsort<- function(v){
  l<-length(v)
  for (i in 1:l){
    min<-i
    for(j in i:l){
      if(v[j]<v[min]){
        min<-j
      }
    }
    temp<-v[i]
    v[i]<-v[min]
    v[min]<-temp
  }
  return(v)
}

#Bubble Sort Function 

bubblesort<- function(v){
  n<-length(v)
  counter = 0
  while(1){
    c = 0
    for (i in 1:(n - 1 - c)){
      if (v[i] > v[i+1]){
        counter<-counter+1
        temp<-v[i]
        v[i]<-v[i+1]
        v[i+1]<-temp
        c = c + 1
      }
    }
    counter = counter + 1
    if (c==0)
      break
  }
  return(v)
}

#Testing Functions

v <- rnorm(5000)
selsort(v)
bubblesort(v)
v2<- 1:5000
v2[10]<- -4
v2[4900]<- 20000
selsort(v2)
bubblesort(v2)

system.time(selsort(v2))
system.time(bubblesort(v2))


#BubbleSort
#it takes R significantly longer to bubblesort (v) (~34 seconds)
#vs bubblesorting(v2). The reason for this is because bubblesort is
#O(n^2) (meaning worst case a la when the vector is arranged in descending order) 
#and is omege(n) (meaning best case where vector is already aranged in order). 
#The n value represents the number of operations requrired to run, and this is because
#bubble sort looks only at two elements at a time and must run through
#the entire vector multiple times if O and only once if omega.

#Sort
#Sort is easily the fasted sorting function of the three taking less than a tenth
#of a second for each v and v2. This is the built in sort function in R
#and hence one of if not the most efficient sorting functions.

#SelSort
#both selsorts (on v and v2) took about a similiar amount of time
#and thats because both are n^2 in O and omega.



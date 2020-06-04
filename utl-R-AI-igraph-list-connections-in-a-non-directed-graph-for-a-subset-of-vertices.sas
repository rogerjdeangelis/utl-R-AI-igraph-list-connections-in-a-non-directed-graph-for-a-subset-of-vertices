Given a non-directed graph g and a subset of vertices v, 1-5 list all possible connections.                                                
                                                                                                                                           
  Two Solutions                                                                                                                            
                                                                                                                                           
       a. R                                                                                                                                
       b. sas                                                                                                                              
          Rob Pratt                                                                                                                        
          Rob.Pratt@sas.com                                                                                                                
                                                                                                                                           
Graph                                                                                                                                      
https://tinyurl.com/y7n2bfy5                                                                                                               
https://github.com/rogerjdeangelis/utl-R-AI-igraph-list-connections-in-a-non-directed-graph-for-a-subset-of-vertices/blob/master/edges.png 
                                                                                                                                           
github                                                                                                                                     
https://tinyurl.com/y7y5ojjl                                                                                                               
https://github.com/rogerjdeangelis/utl-R-AI-igraph-list-connections-in-a-non-directed-graph-for-a-subset-of-vertices                       
                                                                                                                                           
The non-directed graph                                                                                                                     
https://tinyurl.com/y7n2bfy5                                                                                                               
https://github.com/rogerjdeangelis/utl_remove_isolated_nodes_from_an_network_r_igraph                                                      
                                                                                                                                           
G5W profile                                                                                                                                
https://stackoverflow.com/users/4752675/g5w                                                                                                
                                                                                                                                           
Other AI repos                                                                                                                             
https://github.com/rogerjdeangelis?tab=repositories&q=+AI+in%3Aname&type=&language=                                                        
                                                                                                                                           
*_                   _                                                                                                                     
(_)_ __  _ __  _   _| |_                                                                                                                   
| | '_ \| '_ \| | | | __|                                                                                                                  
| | | | | |_) | |_| | |_                                                                                                                   
|_|_| |_| .__/ \__,_|\__|                                                                                                                  
        |_|                                                                                                                                
;                                                                                                                                          
                                                                                                                                           
  Here is a graph with 9 nodes.                                                                                                            
  The op wants to count the internal connectios for                                                                                        
  vertices,1 through 5/.                                                                                                                   
                                                                                                                                           
  In this probem we want to define the connections for                                                                                     
  just vertices 1 to 5. We are not interested in connections                                                                               
  that involve 6 to 9, external vertices.                                                                                                  
                                                                                                                                           
  Internal vertices 1,2,3,4,5                                                                                                              
  External vertices are 6x, 7x, 8x, 9x (removed from graph?)                                                                               
                                                                                                                                           
       Non-Directed graph                                                                                                                  
                   6x                                                                                                                      
                  /|                                                                                                                       
                 / |     Possible Internal connection involving vertices 1-5                                                               
                /  |     (after removing 6x.7x.8x and 9x)                                                                                  
               /   |                                                                                                                       
    2--------4----5            VERTICE1    VERTICE2                                                                                        
    |        |\./  |\                                                                                                                      
    |        | \". | \             1           4                                                                                           
    |        |/ \ `|. \            2           4                                                                                           
    8x-------1---9x----3           3           4                                                                                           
             \   |    /            1           5                                                                                           
              \  |   /             3           5                                                                                           
               \ |  /              4           5                                                                                           
                \| /                                                                                                                       
                 7x         (2,3) is not a possible edge                                                                                   
                                                                                                                                           
*            _               _                                                                                                             
  ___  _   _| |_ _ __  _   _| |_                                                                                                           
 / _ \| | | | __| '_ \| | | | __|                                                                                                          
| (_) | |_| | |_| |_) | |_| | |_                                                                                                           
 \___/ \__,_|\__| .__/ \__,_|\__|                                                                                                          
                |_|                                                                                                                        
;                                                                                                                                          
                                                                                                                                           
Identical output R and SAS                                                                                                                 
                                                                                                                                           
WORK.WANT total obs=6                                                                                                                      
                                                                                                                                           
Testricted to vertices 1-5                                                                                                                 
                                                                                                                                           
 VERTICE1    VERTICE2                                                                                                                      
                                                                                                                                           
     1           4                                                                                                                         
     2           4                                                                                                                         
     3           4                                                                                                                         
     1           5                                                                                                                         
     3           5                                                                                                                         
     4           5                                                                                                                         
                                                                                                                                           
Note (2,3) and (2,5) are examples of impossible connections                                                                                
                                                                                                                                           
*                                                                                                                                          
 _ __  _ __ ___   ___ ___  ___ ___                                                                                                         
| '_ \| '__/ _ \ / __/ _ \/ __/ __|                                                                                                        
| |_) | | | (_) | (_|  __/\__ \__ \                                                                                                        
| .__/|_|  \___/ \___\___||___/___/                                                                                                        
|_|                                                                                                                                        
           ____                                                                                                                            
  __ _    |  _ \                                                                                                                           
 / _` |   | |_) |                                                                                                                          
| (_| |_  |  _ <                                                                                                                           
 \__,_(_) |_| \_\                                                                                                                          
                                                                                                                                           
;                                                                                                                                          
                                                                                                                                           
%utl_submit_r64('                                                                                                                          
library(igraph);                                                                                                                           
library(data.table);                                                                                                                       
library(SASxport);                                                                                                                         
set.seed(5);                                                                                                                               
g <- sample_gnp(9,0.5,directed = FALSE);                                                                                                   
v = V(g)[1:5];                                                                                                                             
png(file="d:/png/edges.png");                                                                                                              
plot(g);                                                                                                                                   
E1 = which(sapply(E(g), function(e) ends(g, e)[1]) %in% v);                                                                                
E2 = which(sapply(E(g), function(e) ends(g, e)[2]) %in% v);                                                                                
Internal = intersect(E1, E2);                                                                                                              
External = setdiff(union(E1,E2), Internal);                                                                                                
ends(g, External);                                                                                                                         
ends(g, Internal);                                                                                                                         
want<-as.data.table(ends(g, Internal));                                                                                                    
colnames(want)<-c("vertice1","vertice2");                                                                                                  
write.xport(want,file="d:/xpt/want.xpt");                                                                                                  
');                                                                                                                                        
                                                                                                                                           
libname xpt xport "d:/xpt/want.xpt";                                                                                                       
data want;                                                                                                                                 
  set xpt.want;                                                                                                                            
run;quit;                                                                                                                                  
libname xpt clear;                                                                                                                         
                                                                                                                                           
INTERNAL CONNECTIONS                                                                                                                       
=====================                                                                                                                      
                                                                                                                                           
     [,1] [,2]                                                                                                                             
[1,]    1    4                                                                                                                             
[2,]    2    4                                                                                                                             
[3,]    3    4                                                                                                                             
[4,]    1    5                                                                                                                             
[5,]    3    5                                                                                                                             
[6,]    4    5                                                                                                                             
                                                                                                                                           
                                                                                                                                           
EXTERNAL CONNECTIONS                                                                                                                       
===================                                                                                                                        
                                                                                                                                           
      [,1] [,2]                                                                                                                            
 [1,]    4    6                                                                                                                            
 [2,]    5    6                                                                                                                            
 [3,]    1    7                                                                                                                            
 [4,]    3    7                                                                                                                            
 [5,]    1    8                                                                                                                            
 [6,]    2    8                                                                                                                            
 [7,]    1    9                                                                                                                            
 [8,]    3    9                                                                                                                            
 [9,]    4    9                                                                                                                            
                                                                                                                                           
                                                                                                                                           
*_                                                                                                                                         
| |__     ___  __ _ ___                                                                                                                    
| '_ \   / __|/ _` / __|                                                                                                                   
| |_) |  \__ \ (_| \__ \                                                                                                                   
|_.__(_) |___/\__,_|___/                                                                                                                   
                                                                                                                                           
;                                                                                                                                          
                                                                                                                                           
data links;                                                                                                                                
   input from to @@;                                                                                                                       
   datalines;                                                                                                                              
1 4  1 5  1 7  1 8  1 9                                                                                                                    
2 4  2 8  3 4  3 5  3 7                                                                                                                    
3 9  4 5  4 6  4 9  5 6                                                                                                                    
6 9  7 9                                                                                                                                   
;                                                                                                                                          
                                                                                                                                           
data nodesubset;                                                                                                                           
   input node @@;                                                                                                                          
   datalines;                                                                                                                              
1 2 3 4 5                                                                                                                                  
;                                                                                                                                          
                                                                                                                                           
proc sql;                                                                                                                                  
   create table want as                                                                                                                    
   select from, to                                                                                                                         
   from links                                                                                                                              
   where from in (select node from nodesubset)                                                                                             
     and to   in (select node from nodesubset);                                                                                            
quit;                                                                                                                                      
                                                                                                                                           
proc optmodel;                                                                                                                             
   set <num,num> LINKS;                                                                                                                    
   read data links into LINKS=[from to];                                                                                                   
   set NODES_SUBSET;                                                                                                                       
   read data nodesubset into NODES_SUBSET=[node];                                                                                          
   create data want from [from to]={<i,j> in LINKS: {i,j} within NODES_SUBSET};                                                            
quit;                                                                                                                                      
                                                                                                                                           
                                                                                                                                           
Up to 40 obs from WANT total obs=6                                                                                                         
                                                                                                                                           
Obs    FROM    TO                                                                                                                          
                                                                                                                                           
 1       1      4                                                                                                                          
 2       1      5                                                                                                                          
 3       2      4                                                                                                                          
 4       3      4                                                                                                                          
 5       3      5                                                                                                                          
 6       4      5                                                                                                                          
                                                                                                                                           
                                                                                                                                           

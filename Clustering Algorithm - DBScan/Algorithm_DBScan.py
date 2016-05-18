# kakilai (erica.kklai@gmail.com)
# Implementation of DB Scan Algorithm
# Nov 27, 2015


from math import sqrt, pow 

class DBSCAN:
	def _init_(self):
		self.DB = []
		self.esp=4
		self.MinPts=2
		self.cluster_inx = -1  
     	self.cluster = []  

	def dbscan(self):
		for i in range(len(self.DB)):
			pt = self.DB[i]



 	def DBSCAN(self):  
    	 for i in range(len(self.DB)):  
     	  p_tmp = self.DB[i]  
       	 if (not p_tmp.visited):  
         #for each unvisited point P in dataset  
         p_tmp.visited = True  
         NeighborPts = self.regionQuery(p_tmp)  
         if(len(NeighborPts) < self.MinPts):  
           #that point is a noise  
           p_tmp.isnoise = True  
           print p_tmp.show(), 'is a noise'  
         else:  
           self.cluster.append([])  
           self.cluster_inx = self.cluster_inx + 1  
           self.expandCluster(p_tmp, NeighborPts)   

	def regionQuery(self, P):  
   		#return all points within P's eps-neighborhood, except itself  
     	pointInRegion = []  
     	for i in range(len(self.DB)):  
      		 p_tmp = self.DB[i]  
       		if (self.dist(P, p_tmp) < self.esp and P.x != p_tmp.x and P.y != p_tmp.y):  
        	 	pointInRegion.append(p_tmp)  
     	return pointInRegion  

    def dist(self, p1, p2):  
   #return distance between two point  
     dx = (p1.x - p2.x)  
     dy = (p1.y - p2.y)  
     return sqrt(pow(dx,2) + pow(dy,2))  

     def expandCluster(self, P, neighbor_points):  
     	self.cluster[self.cluster_inx].append(P)  
     	iterator = iter(neighbor_points)  
     	while True:  
      	 try:   
         	npoint_tmp = iterator.next()  
      	 except StopIteration:  
         # StopIteration exception is raised after last element  
         break  
       	if (not npoint_tmp.visited):  
         #for each point P' in NeighborPts   
         npoint_tmp.visited = True  
         NeighborPts_ = self.regionQuery(npoint_tmp)  
         if (len(NeighborPts_) >= self.MinPts):  
           for j in range(len(NeighborPts_)):  
             neighbor_points.append(NeighborPts_[j])  
      	 if (not self.checkMembership(npoint_tmp)):  
         #if P' is not yet member of any cluster  
         	self.cluster[self.cluster_inx].append(npoint_tmp)  
      	 else:  
        	 print npoint_tmp.show(), 'is belonged to some cluster'  


class Point:  
   def __init__(self, x = 0, y = 0, visited = False, isnoise = False):  
     self.x = x  
     self.y = y  
     self.visited = False  
     self.isnoise = False

vecPoint = [Point(11,3), Point(10,4), Point(11,5), Point(12,4), Point(13,5), Point(12,6), Point(6,10), Point(8,10), Point(5,12), Point(7,12)]  
scan = DBSCAN()
scan.DB= vecPoint


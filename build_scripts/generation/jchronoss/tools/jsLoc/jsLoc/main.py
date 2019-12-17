#!/usr/bin/python
# -*-coding:utf-8 -*
############################################################################
#                                                                          #
#                         Copyright or (C) or Copr.                        #
#       Commissariat a l'Energie Atomique et aux Energies Alternatives     #
#                                                                          #
# Version : 1.2                                                            #
# Date    : Tue Jul 22 13:28:10 CEST 2014                                  #
# Ref ID  : IDDN.FR.001.160040.000.S.P.2015.000.10800                      #
# Author  : Julien Adam <julien.adam@cea.fr>                               #
#           Marc Perache <marc.perache@cea.fr>                             #
#                                                                          #
# This file is part of JCHRONOSS software.                                 #
#                                                                          #
# This software is governed by the CeCILL-C license under French law and   #
# abiding by the rules of distribution of free software.  You can  use,    #
# modify and/or redistribute the software under the terms of the CeCILL-C  #
# license as circulated by CEA, CNRS and INRIA at the following URL        #
# "http://www.cecill.info".                                                #
#                                                                          #
# As a counterpart to the access to the source code and  rights to copy,   #
# modify and redistribute granted by the license, users are provided only  #
# with a limited warranty  and the software's author,  the holder of the   #
# economic rights,  and the successive licensors  have only  limited       #
# liability.                                                               #
#                                                                          #
# In this respect, the user's attention is drawn to the risks associated   #
# with loading,  using,  modifying and/or developing or reproducing the    #
# software by the user in light of its specific status of free software,   #
# that may mean  that it is complicated to manipulate,  and  that  also    #
# therefore means  that it is reserved for developers  and  experienced    #
# professionals having in-depth computer knowledge. Users are therefore    #
# encouraged to load and test the software's suitability as regards their  #
# requirements in conditions enabling the security of their systems and/or #
# data to be ensured and,  more generally, to use and operate it in the    #
# same conditions as regards security.                                     #
#                                                                          #
# The fact that you are presently reading this means that you have had     #
# knowledge of the CeCILL-C license and that you accept its terms.         #
#                                                                          #
############################################################################

#imports
import os
import sys
from Drawer import *
from TestBloc import *
from OptionsHandler import *


def main(argv=None):
	#loading options
	optParser=OptionsHandler()
	optParser.load()

	#file reading
	file_in = open(optParser.input_file, "r")
	buff = file_in.read().split("\n")
	file_in.close()

	#drawing configuration setting
	drawer = Drawer(buff[0].split(optParser.pattern_delimiter))

	mainList = TestsList(drawer.nbX)
	totalNbJobs=0
	#each line loop
	for line in buff:
		if line == "":
			continue
		if line[0] == '#':
			continue
		
		jobLine = line.split(optParser.pattern_delimiter)
		job_ts_start = TimeStamp()
		job_ts_end = TimeStamp()
		job_res = 0
		
		job_nom = jobLine[0]
		#throw exception( cast str -> int and assertion)
		try:
			job_ts_start.initTimeStamp(jobLine[1])
			job_ts_end.initTimeStamp(jobLine[2])
			assert job_ts_start.getTimeStamp() <= job_ts_end.getTimeStamp()
			job_res = int(jobLine[3])
		except:
			print("ERROR: Unable to parse line: ",line);
			continue
		#adding
		mainList.append(job_nom, job_res, job_ts_start, job_ts_end)
		totalNbJobs+=1
	
	sorted(mainList.testList, key=lambda obj: obj.t0.getTimeStamp())

	#scheduling loop
	for i in range(0,totalNbJobs):
		#find test with time the smallest
		nextObj = mainList.testList.pop(0)
		#compare with treated tests
		mainList.fixPosition(nextObj)
		#update refNodes (adjust to the good size)
		nextObj.finalize(drawer.nbX)
		#fix treated test
		mainList.treatedTestList.append(nextObj)
		totalFilledSurface+=(nextObj.nbBloc*(nextObj.t1.getTimeStamp()-nextObj.t0.getTimeStamp()))
	
	sys.exit(0)

	#header writing in file
	file_out = open(optParse.output_file, "w")
	nb = file_out.write(header_svg)
	
	#setting Y dimensions
	y_timeMax = mainList.findLastTest().t1.getTimeStamp()-drawer.initTimeStamp
	if y_timeMax <= 1:
		y_timeMax = 1
	
	y_timeUnit = float(float(y_timeMax)/float(y_nbUnits))
	y_timePerPx = float(float(y_timeUnit)/float(y_sizeUnit))
	
	#color cpt
	j=0
	#tests writing
	for current_test in mainList.treatedTestList:
		current_test.writingJob(file_out, x_sizeUnit, y_timePerPx, x_nbUnits, x_oh, y_oh, begin_time,colorTab[j%NB_COLORS])
		#color id
		j+=1
	
	#line trace to delimit the graph
	#totalAvailableSurface=int(x_nbUnits*y_timeMax)
	#surfacefilledAverage=float((totalFilledSurface/totalAvailableSurface)*100)
	
	drawer.generate(file_out)
	file_out.close()


##########################
########## MAIN ##########
##########################
if __name__ == "__main__":
	sys.exit(main())

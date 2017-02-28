#Scripts/GenerateJSONforcountry.py Output/Fakecountry.csv
import sys
import json
import operator

projMin = 1;
projMax = 496;
minBubbleSize = 4;
maxBubblesize = 20;

linksMax = 50;

class JNode:
	def __init__(self, index, links, line, country):
		self.index = index
		self.links = links
		self.country = country
		self.size = (((float(line[3]) - projMin)*(maxBubblesize - minBubbleSize)) / (projMax - projMin)) + minBubbleSize
		self.id = index
		self.level = 1

	def toJSON(self):
		return json.dumps({'index':self.index,'links':self.links,'label':self.country,'size':self.size,'id':self.index, 'level':self.level},separators=(',', ':'),indent=4)

	def setSize(self, projectNbr):
		self.size = (((float(projectNbr) - projMin)*(maxBubblesize - minBubbleSize)) / (projMax - projMin)) + minBubbleSize
		#NewValue = (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin

	def getID(self):
		return self.id;

class JLink:
	def __init__(self, source, target, weight):
		self.source = source
		self.target = target
		self.weight = weight

	def toJSON(self):
		return json.dumps({'source':self.source,'target':self.target,'weight':self.weight},separators=(',', ':'),indent=4)


errorLog = open('error.log', 'w')

#write JSON output file
def WriteJSON(nameOutputFile, orig_nodelist, linksList):
	try:
		nodelist = sorted(orig_nodelist, key=operator.attrgetter("id"))
		outputName = nameOutputFile
		# TODO: create dynamic file name
		outputFile = open(outputName, 'w')
		outputFile.write('{\n')
		outputFile.write('"nodes": [\n');
		for i in range(len(nodelist)-1):
			outputFile.write(nodelist[i].toJSON())
			outputFile.write(',\n')
		outputFile.write(nodelist[len(nodelist)-1].toJSON())
		#TODO handle the case for no links
		outputFile.write('\n')
		outputFile.write('],\n')
		outputFile.write('"links":[\n')
		if len(linksList) > 0:
			for i in range(len(linksList)-1):
				outputFile.write(linksList[i].toJSON())
				outputFile.write(',\n')
			outputFile.write(linksList[len(linksList)-1].toJSON())
		outputFile.write(']\n')
		outputFile.write('}')
	except Exception as e:
		print(e)
		print(nameOutputFile)
		errorLog.write("-------------------------------------\n");
		errorLog.write("Error while writing the folowing file: "+nameOutputFile+'\n')
		errorLog.write("-------------------------------------\n");

#define some parameters
sepChar = ';'

def extractForOneFC(index, linesArray):
	currentFC = linesArray[index].split(sepChar)[0];
	countryDict = {};
	countryList = [];
	linksList = [];
	count = 0;
	copyIndex = index;
	while(copyIndex < len(linesArray) and currentFC == linesArray[copyIndex].split(sepChar)[0]):
		lineList = linesArray[copyIndex].split(sepChar);
		try:
			value = countryDict[lineList[1]];
		except KeyError:
			countryDict[lineList[1]] = count;
			countryList.append(JNode(count,[],lineList,lineList[1]));
			count += 1
		try:
			value = countryDict[lineList[2]];
		except KeyError:
			if lineList[2] != '':
				countryDict[lineList[2]] = count;
				countryList.append(JNode(count,[],lineList,lineList[2]))
				count += 1
		#add links to JNode objects
		if lineList[2] != '':
			country1Index = countryList[countryDict[lineList[1]]].id
			country2Index = countryList[countryDict[lineList[2]]].id
			countryList[countryDict[lineList[1]]].links.append(country2Index);
			countryList[countryDict[lineList[2]]].links.append(country1Index);
			#create JLink Object
			#NewValue = (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin
			linkWeight = (((float(lineList[5])-1)*(5-1))/(linksMax-1)+1)
			linksList.append(JLink(country1Index, country2Index, linkWeight))
		copyIndex +=1
	WriteJSON(currentFC + ".json", countryList, linksList)
	return copyIndex;


import os
print(os.getcwd())


#read file
FileLink = sys.argv[1]
#linesLink = [line.rstrip('\n') for line in open(FileLink, encoding="utf-8")]
linesLink = [line.rstrip('\n') for line in open(FileLink,  encoding="utf-8")]

currLine = 1;
while(currLine < len(linesLink)):
	currLine = extractForOneFC(currLine,linesLink);

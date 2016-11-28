#Scripts/GenerateJSON.py Output/NbLink.csv Output/CountryInformation.csv
import sys
import json

maxNbrProjectsFP6 = 1
maxNbrProjectsFP7 = 1
maxNbrProjectsH2020 = 1
maxBubblesize = 50

class JNode:
	def __init__(self, index, links, line):
		self.index = index
		self.links = links
		self.label = line[1]
		self.NumberProjectParticipation = line[3]
		self.NumberProjectCoordination = line[4]
		self.OverallNumberProject = line[5]
		self.NumberInstitution = line[6]
		self.FullName = line[8]
		self.GDP = line[11]
		self.Population = line[12]
		self.GDPPerCapita = line[13]
		if(line[2] == "FP6"):
			self.score = (((float(line[5]) - 1)*(maxBubblesize - 1)) / (maxNbrProjectsFP6 - 1)) + 1
		elif(line[2] == "FP7"):
			self.score = (((float(line[5]) - 1)*(maxBubblesize - 1)) / (maxNbrProjectsFP7 - 1)) + 1
		else:
			self.score = (((float(line[5]) - 1)*(maxBubblesize - 1)) / (maxNbrProjectsH2020 - 1)) + 1
		self.id = index
	def toJSON(self):
		return json.dumps({'index':self.index,'links':self.links,'label':self.label,'NumberProjectParticipation':self.NumberProjectParticipation,'NumberProjectCoordination':self.NumberProjectCoordination,'OverallNumberProject':self.OverallNumberProject,'NumberInstitution':self.NumberInstitution,'FullName':self.FullName,'GDP':self.GDP,'Population':self.Population,'GDPPerCapita':self.GDPPerCapita,'score':self.score,'id':self.index},separators=(',', ':'),indent=4)

class JLink:
	def __init__(self, source, target, weight):
		self.source = source
		self.target = target
		self.weight = weight

	def toJSON(self):
		return json.dumps({'source':self.source,'target':self.target,'weight':self.weight},separators=(',', ':'),indent=4)


sepChar = ';'

countriesListFP6 = [];
countriesListFP7 = [];
countriesListH2020 = [];
nodesListFP6 = [];
nodesListFP7 = [];
nodesListH2020 = [];

linksListFP6 = [];
linksListFP7 = [];
linksListH2020 = [];

FileLink = sys.argv[1]
FileCountry = sys.argv[2]


linesLink = [line.rstrip('\n') for line in open(FileLink)]
linesCountry = [line.rstrip('\n') for line in open(FileCountry)]

for i in range(1, len(linesCountry)):
	lineList = linesCountry[i].split(sepChar)
	if lineList[2] == "FP6":
		if(float(lineList[5]) > maxNbrProjectsFP6):
			maxNbrProjectsFP6 = float(lineList[5])
	elif lineList[2] == "FP7":
		if(float(lineList[5]) > maxNbrProjectsFP7):
			maxNbrProjectsFP7 = float(lineList[5])
	else:
		if(float(lineList[5]) > maxNbrProjectsH2020):
			maxNbrProjectsH2020 = float(lineList[5])

for i in range(1, len(linesCountry)):
	lineList = linesCountry[i].split(sepChar)
	framework = lineList[2]
	if (framework=="FP6"):
		countriesListFP6.append(lineList[1])
		nodesListFP6.append(JNode(len(countriesListFP6)-1,[],lineList))
	elif framework == "FP7":
		countriesListFP7.append(lineList[1])
		nodesListFP7.append(JNode(len(countriesListFP7)-1,[],lineList))
	else:
		countriesListH2020.append(lineList[1])
		nodesListH2020.append(JNode(len(countriesListH2020)-1,[],lineList))


for i in range(1, len(linesLink)):
	lineList = linesLink[i].split(sepChar)
	if(lineList[1] != lineList[2]):
		#node
		framework = lineList[4]
		if framework == "FP6":
			currentCountryIndex = countriesListFP6.index(lineList[1])
			cNode = nodesListFP6[currentCountryIndex]
			cNode.links.append(countriesListFP6.index(lineList[2]))
			linksListFP6.append(JLink(cNode.index, countriesListFP6.index(lineList[2]), float(lineList[5])))
		elif framework == "FP7":
			currentCountryIndex = countriesListFP7.index(lineList[1])
			cNode = nodesListFP7[currentCountryIndex]
			cNode.links.append(countriesListFP7.index(lineList[2]))
			linksListFP7.append(JLink(cNode.index, countriesListFP7.index(lineList[2]), float(lineList[5])))
		else:
			currentCountryIndex = countriesListH2020.index(lineList[1])
			cNode = nodesListH2020[currentCountryIndex]
			cNode.links.append(countriesListH2020.index(lineList[2]))
			linksListH2020.append(JLink(cNode.index, countriesListH2020.index(lineList[2]), float(lineList[5])))

#write JSON output file
def WriteJSON(nameOutputFile, nodelist, linksList):
	outputName = nameOutputFile
	outputFile = open(outputName, 'w')
	outputFile.write('{\n')
	outputFile.write('"nodes": [\n');
	for i in range(len(nodelist)-1):
		outputFile.write(nodelist[i].toJSON())
		outputFile.write(',\n')
	outputFile.write(nodelist[len(nodelist)-1].toJSON())
	outputFile.write('\n')
	outputFile.write('],\n')
	outputFile.write('"links":[\n')
	for i in range(len(linksList)-1):
		outputFile.write(linksList[i].toJSON())
		outputFile.write(',\n')
	outputFile.write(linksList[len(linksList)-1].toJSON())
	outputFile.write(']\n')
	outputFile.write('}')



WriteJSON("outputJS/FP6.json",nodesListFP6, linksListFP6)
WriteJSON("outputJS/FP7.json",nodesListFP7, linksListFP7)
WriteJSON("outputJS/H2020.json",nodesListH2020, linksListH2020)

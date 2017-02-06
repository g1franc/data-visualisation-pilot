#Scripts/GenerateJSONfororg.py Output/FakeOrg.csv
import sys
import json
import operator

projMin = 1;
projMax = 1;
maxBubblesize = 20;

class JNode:
	def __init__(self, index, links, line, label, country, activity):
		self.index = index
		self.links = links
		self.label = label
		self.score = float(line[9])
		self.id = index
		self.level = 1
		self.country = country
		self.activity = activity

	def toJSON(self):
		return json.dumps({'index':self.index,'links':self.links,'label':self.label,'score':self.score,'id':self.index, 'level':self.level, 'country':self.country, 'activity': self.activity},separators=(',', ':'),indent=4)

	def setScore(self, projectNbr):
		self.score = (((float(projectNbr) - projMin)*(maxBubblesize - 1)) / (projMax - projMin)) + 1
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
		outputFile.write('\n')
		outputFile.write('],\n')
		outputFile.write('"links":[\n')
		for i in range(len(linksList)-1):
			outputFile.write(linksList[i].toJSON())
			outputFile.write(',\n')
		outputFile.write(linksList[len(linksList)-1].toJSON())
		outputFile.write(']\n')
		outputFile.write('}')
	except Exception as e:
		print(e)
		print(nameOutputFile)


#define some parameters
sepChar = ';'
orgDictionary = {};

#read file
FileLink = sys.argv[1]
#linesLink = [line.rstrip('\n') for line in open(FileLink, encoding="utf-8")]
linesLink = [line.rstrip('\n') for line in open(FileLink)]


#Create a dictionary of all organizations with their "id, empty links (filled in later), line, label, country, activity"
count = 0
for i in range(1, len(linesLink)):
	lineList = linesLink[i].split(sepChar)
	try:
		value = orgDictionary[lineList[1]];
	except KeyError:
		orgDictionary[lineList[1]] = JNode(count,[],lineList,lineList[1],lineList[3],lineList[5])
		count += 1

for i in range(1, len(linesLink)):
	lineList = linesLink[i].split(sepChar)
	try:
		value = orgDictionary[lineList[2]];
	except KeyError:
		orgDictionary[lineList[2]] = JNode(count,[],lineList,lineList[2],lineList[4],lineList[6])
		count += 1


#
NodeDic = {};
linksList = [];

lineNumber = 1
while (lineNumber < len(linesLink)-1):
	curName = linesLink[lineNumber].split(sepChar)[0]
	nameOnLineToProcess = curName

	while (curName == nameOnLineToProcess):

		lineList = linesLink[lineNumber].split(sepChar)
	
		org1Name = lineList[1]
		org2Name = lineList[2]
		if (not org1Name in NodeDic):
			NodeDic[org1Name] = orgDictionary[org1Name]
		if (not org2Name in NodeDic):
			NodeDic[org2Name] = orgDictionary[org2Name]

		org1Index = NodeDic[org1Name].id
		org2Index = NodeDic[org2Name].id

		NodeDic[org1Name].links.append(org2Index)
		NodeDic[org2Name].links.append(org1Index)
		#compute weight according to formula :
		#NewValue = (((OldValue - OldMin) * (NewMax - NewMin)) / (OldMax - OldMin)) + NewMin
		linkWeight = (((float(lineList[9])-1)*(10-1))/(39-10)+1)
		linksList.append(JLink(org1Index, org2Index, linkWeight))

		lineNumber += 1
		try:
			nameOnLineToProcess = linesLink[lineNumber].split(sepChar)[0]
		except IndexError:
			nameOnLineToProcess = " "
	#write the file and empty lists and dictionaries
	NodeList = [];
	for key, value in NodeDic.items():
		NodeList.append(value)
	WriteJSON("outputJS/" + curName.replace(' ','_') + ".json", NodeList, linksList)
	NodeDic = {};
	linksList = [];
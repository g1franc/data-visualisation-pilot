import sys

countries = [line.rstrip('\n') for line in open(sys.argv[1], encoding="utf-8")]
outputFile = open(sys.argv[2], 'w')


for i in range(1, len(countries)):
    outputFile.write('<div class="checkbox" fjs-criteria="field=country,ele=#countries input:checkbox">\n')
    outputFile.write("\t<label>\n")
    outputFile.write('\t\t<input type="checkbox" value="'+countries[i]+'">\n')
    outputFile.write('\t\t<span>'+countries[i]+'</span>\n')
    outputFile.write("\t</label>\n")
    outputFile.write("</div>\n")

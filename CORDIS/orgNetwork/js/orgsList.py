from os import listdir

outputFile = open("orgsList.js", 'w');

outputFile.write('var orgsList=[');
for f in listdir("C:\Data\orgNetwork"):
    try:
        print(f);
        outputFile.write('"'+f+'",\n');
    except UnicodeEncodeError:
        print('error');

outputFile.write('];');

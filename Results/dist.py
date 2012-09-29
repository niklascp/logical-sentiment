import csv
import nltk
from nltk.corpus import PlaintextCorpusReader
corpus_root = '/DTU/MSc/Code/Data'
wordlists = PlaintextCorpusReader(corpus_root, ['food_holiday_inn_london.txt.data', 'food_swissotel_chicago.txt.data', 'room_holiday_inn_london.txt.data', 'rooms_swissotel_chicago.txt.data', 'rooms_swissotel_chicago.txt.data', 'service_bestwestern_hotel_sfo.txt.data', 'service_holiday_inn_london.txt.data', 'service_swissotel_hotel_chicago.txt.data', 'staff_bestwestern_hotel_sfo.txt.data', 'staff_swissotel_chicago.txt.data']) 
wsj = nltk.corpus.treebank;

print wordlists.fileids()
print wsj.fileids()

print (len(wordlists.sents()))
senLengths1 = [len(s) for s in wordlists.sents()]
freqDist1 = nltk.FreqDist(senLengths1)

print (len(wsj.sents()))
senLengths2 = [len(s) for s in wsj.sents()]
freqDist2 = nltk.FreqDist(senLengths2)


propDist1 = nltk.DictionaryProbDist(freqDist1, normalize=True)
propDist2 = nltk.DictionaryProbDist(freqDist2, normalize=True)

myfile = open('../Thesis/wsjdist.dat', 'wb')
wr = csv.writer(myfile, quoting=csv.QUOTE_NONE)


wr.writerow(["x", "y1", "y2"])

for x in range(1,70):
	wr.writerow([x, 100*propDist1.prob(x), 100*propDist2.prob(x)])




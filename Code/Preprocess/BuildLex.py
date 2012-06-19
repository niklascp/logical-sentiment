from collections import defaultdict
from itertools import *
import pickle
import nltk
from nltk.corpus import wordnet as wn

maxReviews = 2

#text = nltk.word_tokenize("And now for something completely different")
#print text

filename = '../Data/service_swissotel_hotel_chicago.txt.data'
#filename = '../Data/service_holiday_inn_london.txt.data'
#filename = '../Data/speed_windows7.txt.data'

posString = "good, beautiful, happy, pleasant, clean, friendly, healthy, correct, lucky, alive, clever"
posList = [x.strip().lower() for x in posString.split(',')]

negString = "bad, hideous, sad, unpleasant, dirty, hostile, sick, wrong, unfortunate, dead, stupid"
negList = [x.strip().lower() for x in negString.split(',')]

posSynsets = set(chain(*[wn.synsets(w) for w in posList]))
#print len(posSynsets)



# Load reviews from data file
reviews = []
words = { };
textf = open(filename, 'r')
lines = textf.readlines()
for line in lines[:maxReviews]:
	line = line.replace('\n', '')
	line = line.replace('\r', '')
	line = line.replace(' .', '.')

	reviews.append(line.strip())

	line = line.replace('.', '')
	line = line.replace(',', '')
	line = line.replace('!', '')

	for w in line.split(' '):
		w = w.strip().lower()
		if  w != '':
			words[w] = words.get(w, 0) + 1
textf.close()


# Load syntactic lexicon
lex_file = open('lex.data', 'rb')
dictonary = pickle.load(lex_file)
lex_file.close()

#print dictonary[:9]

print "No. of words in Reviews Set:", len(words)
print "No. of words in Brown:", len(dictonary)

topWords = sorted(words, reverse=True, key=lambda k: words[k])

# Count how many words for which WordNet has non-empty synsets.
#i = 0;
#for w in topWords:
#	synsets = wn.synsets(w)
#	if not synsets:
#		i = i + 1
#print len(topWords), i

print dictonary.get("master", [])

synsets = defaultdict(set)
i = 0.0
j = 0.0
for w in topWords:	
	categories = dictonary.get(w, [])
	print w, categories
	#if any
	n = len(categories)
	if not n == 0:
		i = i + 1.0
	j = j + 1.0
	#print w, len(dictonary[w])

print "Max. Coverage:", 100.0*(i / j)

#	synsets = wn.synsets(w, pos=wn.ADJ)
#	if (synsets):
#		print w + ":", words[w], len(synsets)

print posString

#print 
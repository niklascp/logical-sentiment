import csv
import nltk
from nltk.corpus import PlaintextCorpusReader
from nltk.corpus import brown

corpus_root = '/DTU/MSc/Code/Data'
test = PlaintextCorpusReader(corpus_root, ['food_holiday_inn_london.txt.data', 'food_swissotel_chicago.txt.data', 'room_holiday_inn_london.txt.data', 'rooms_swissotel_chicago.txt.data', 'rooms_swissotel_chicago.txt.data', 'service_bestwestern_hotel_sfo.txt.data', 'service_holiday_inn_london.txt.data', 'service_swissotel_hotel_chicago.txt.data', 'staff_bestwestern_hotel_sfo.txt.data', 'staff_swissotel_chicago.txt.data']) 

uniqueBrown = set(brown.words())
uniqueTest = set(test.words())

print "No. words in Brown:"
print "All    :", (len(brown.words()))
print "Unique :", (len(uniqueBrown))

print "No. words in test data:"
print "All    :", (len(test.words()))
print "Unique :", (len(uniqueTest))

print "Max. procentage of known words:"
print "All    :", len(uniqueBrown & uniqueTest) * 100.0 / len(uniqueTest)
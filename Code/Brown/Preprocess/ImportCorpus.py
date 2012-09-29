import pickle
import nltk
import re
from collections import defaultdict
from itertools import *

# Create a map from the Brown category to CCG
brownIgnore = [
	"NIL",
	"CD",
	"CD-HL",
	"CD-TL",
	"OD",
	"OD-HL",
	"OD-TL",
	".",
	"(",
	")",
	"--",
	",",
	":",
	"``",
	"''",
	"'"
]

brownMap = {
	"AT"  : ["NP x/N x"],
	"CC"  : ["CONJ x"],

	# Special Verb, To be
	# TODO: Tense?
	#"BE"  : 	# be
	"BED" : ["(S [Dcl]\NP [Pl])/(S [Adj]\NP [Pl])"],	# were 
	"BEDZ": ["(S [Dcl]\NP [Sg])/(S [Adj]\NP [Sg])"],	# was
	#"BEG" : 	# being
	#"BEM" : 	# am
	#"BEN" : 	# been
	"BER" : ["(S [Dcl]\NP [Pl])/(S [Adj]\NP [Pl])"],	# are, art
	"BEZ" : ["(S [Dcl]\NP [Sg])/(S [Adj]\NP [Sg])"],	# is

	"DO"  : [#"S [Dcl]\NP x", 							# do
			 "(S [Dcl]\NP x)/(S x\NP x)"],				# auxiliary verb 
	"DOD" : [#"S [Dcl]\NP x", 							# did
			 "(S [Dcl]\NP x)/(S x\NP x)"],				# auxiliary verb 
	"DOZ" : [#"S [Dcl]\NP x",							# does
			 "(S [Dcl]\NP x)/(S x\NP x)"],				# auxiliary verb 

	# HAVING
	# TODO: Tense?
	"HV"  : ["(S [Dcl]\NP [])/NP []"],			# have
	"HVZ" : ["(S [Dcl]\NP [Sg,Thrd])/NP []"],	# has
	"HVD" : ["(S [Dcl]\NP [])/NP []"],			# had (past tense)
	"HVG" : ["(S [Ng]\NP x)/NP x"],				# having (present participle)
	"HVN" : ["(S [Pt]\NP x)/NP x"],				# had (past participle)

	# NOUNS
	# common nouns
	# TODO Person?
	"NN"  : ["N [Sg]"],							# singular
	"NN$" : ["N [Sg,Gen]"],  					# possessive singular
	"NNS" : ["N [Pl]"],							# plural 
	"NNS$": ["N [Pl,Gen]"],  					# possessive plural 

	# proper nouns
	# TODO Person?
	"NP"  : ["N [Sg,Thrd]"],					# singular
	"NP$" : ["N [Sg,Gen]"],						# possessive singular
	"NPS" : ["N [Pl]"],							# plural 
	"NPS$": ["N [Pl,Gen]"], 					# possessive singular

	# PRONOUNS
	# TODO Check with CS
	"PN"  : ["N []"],							# nominal pronoun (everybody, nothing)
	"PN$" : ["N [Gen]"],						# possessive nominal pronoun
	"PP$" : ["N [Pers,Gen]"],                   # possessive personal pronoun (my, our)
    "PP$$":	["N [Pers,Gen]"],                   # second (nominal) possessive pronoun (mine, ours)	

	"PPL" : ["N [Sg,Refl]"],				    # singular reflexive/intensive personal pronoun (myself)
	"PPLS": ["N [Pl,Refl]"],					# plural reflexive/intensive personal pronoun (ourselves)
	"PPO" : ["N []"],                           # objective personal pronoun (me, him, it, them)
    "PP$$":	["N [Sg,Nom,Thrd]"],                # 3rd. singular nominative pronoun (he, she, it, one)
	"PPSS":	["N [Nom]"],                        # other nominative personal pronoun (I, we, they, you)

	# VERBS
	# base form
	"VB"  :	["S [Dcl]\NP x",					# (intransitive)
			 "(S [Dcl]\NP x)/NP x"],	 		# (transitive)

	# past tense 
	# TODO: Tense?
	"VBD" :	["S [Dcl]\NP x",					# (intransitive)
			 "(S [Dcl]\NP x)/NP x"], 			# (transitive)	

	# present participle ("-ing")
	# TODO: Tense?
	"VBG" :	["S [Ng]\NP x",						# (intransitive)
			 "(S [Ng]\NP x)/NP x"], 			# (transitive)	

	# past participle ("-ed")
	# TODO: Tense?
	"VBN" :	["(S [Pt]\NP x)",					# (intransitive)
			 "(S [Pt]\NP x)/NP x"], 			# (transitive)	

	# 3rd. singular 
	# TODO Person?
	"VBZ" : ["(S [Dcl]\NP [Sg])/(S [Ng]\NP x)",	# present 
			 "(S [Dcl]\NP [Sg])/(S [Pt]\NP x)"],# past

	# ADVERBS
	"RB"  :	["(S x\NP y)\(S x\NP y)",			# adverb
	         "(S [Adj]\NP y)/(S [Adj]\NP y)"],	# TODO: What is this?
#	"RBR" : ???									# comparative adverb
#	"RBT" : ???									# superlative adverb

	# ADJECTIVES
	# adjective
	"JJ"  :	["N x/N x",
	         "S [Adj]\NP x"],					# TODO: What is this?
#	"JJR" : ???									# comparative
#	"JJS" : ???									# semantically superlative (chief, top)
#	"JJT" : ???									# morphologically superlative (biggest)

	# PRONOUNS 2
	"WDT" : ["(NP x\NP x)/(S [Dcl]\NP y)",		# wh- determiner (what, which)
			 "(NP x\NP x)/(S [Dcl]/NP y)"],
	#"WP$" : ["(NP x\NP x)/(S [Dcl]\NP y)",		# possessive wh- pronoun (whose)
	#		 "(NP x\NP x)/(S [Dcl]/NP y)"],
	"WPO" :	["(NP x\NP x)/(S [Dcl]\NP y)",		# objective wh- pronoun (whom, which, that)
			 "(NP x\NP x)/(S [Dcl]/NP y)"],
	"WPS" :	["(NP x\NP x)/(S [Dcl]\NP y)",		# nominative wh- pronoun (who, which, that)
			 "(NP x\NP x)/(S [Dcl]/NP y)"],


	# PREPOSITION
	"IN" : ["(NP []\NP [])/NP []",
		    "((S []\NP [])\(S []\NP []))/NP []"]
}


# Create a map from token (i.e. lower-case word) to a set of syntactic categories of which the token occurs in the corpus.
taggedCorpus = nltk.corpus.brown.tagged_words()
processed = defaultdict(set)
dictonary = defaultdict(set) #TODO: Should we sort with respect to freq.?
for (w, c) in taggedCorpus:

	w = w.lower()
	w = re.sub(r'[^a-z0-9\'\$.-]', '', w)

	if c in brownIgnore or c in processed[w.lower()]:
		continue

	# For some reason some tags contain additional annotations, no documentation of this as been found?
	categoryParts = re.findall(r'[\w\$*+]+', c) 
	#print c, len(categoryParts), ":", w
	if categoryParts[0] == "FW":
		category = categoryParts[1]
	else:
		category = categoryParts[0]

	# TODO - negate semantics when we are comming to this point...
	if category.endswith("*"):
		category = category[:-1]

	ccgCategories = brownMap.get(category, [])
	if len(ccgCategories) == 0:
		print "Warning, no map for:", c, ":", category,  ":", w
	for ccgCategory in ccgCategories:
		dictonary[w.lower()].add(ccgCategory)
	
	# We also add adjectives as adverbs
	if category == "JJ" and not "RB" in processed[w.lower()]:
		ccgCategories = brownMap.get("RB", [])
		for ccgCategory in ccgCategories:
			dictonary[w.lower()].add(ccgCategory)
		processed[w.lower()].add("RB")

	# Mark this as a processed word/category pair.
	processed[w.lower()].add(c) 

# write lexicon to file
output = open('lex.data', 'w')
words = sorted(dictonary)
for w in words:
	if len(w) == 0:
		continue
	output.write(w)
	output.write(" :- ")
	output.write(', '.join(dictonary[w]))
	output.write("\n")
output.close()

# write lexicon to file (for analysis)
output = open('lex.pickle', 'w')
pickle.dump(dict(dictonary), output)
output.close()

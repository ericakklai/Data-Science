# kakilai (kakilai2@illinois.edu)
# Implementation of Frequent Pattern Mining Algorithm
# Nov 10, 2015

import copy
import itertools

class Node:
	# Constructor of the FP Tree Node
	# In each node, we store the corresponding vocab id and the frequency
	def __init__(self, id):
		self.id = id    
		self.child = {}
		self.count = 0 # frequency


	# Add the list of children(ids) under the current node & count their frequency
	def add(self, ids):
		if (self.id is not None): 
			self.count += 1

		# No more new node have to been add, stop it
		if (len(ids) == 0): return

		# Add the new node if it is not a children of current node
		id = ids[0]
		if (id not in self.child):
			node = Node(id)
			self.child[id] = node

		# Countine to add the rest of children
		self.child[id].add(ids[1:])


	# Find the deepest height of each vocab id & find their frequency in FP Tree
	def depth(self, dependency, count, n):
		if (self.id is not None):
			if (self.id in dependency):
				dependency[self.id] = max(dependency[self.id], n)
				count[self.id] += self.count
			else:
				dependency[self.id] = n
				count[self.id] = self.count
		for key in self.child:
			self.child[key].depth(dependency, count, n+1)


	# Find the list of vocab that it's frequency >= min support & sort it by frequency in descending order
	def dependency(self):
		d = {}
		count = {}
		self.depth(d, count, 0)
		result = []
		for id in d:
			if (count[id] >= minSup):
				result.append((id, d[id], count[id]))
		result.sort(compare)
		return result


	# Check if the FP Tree has fork tree
	def fork(self):
		if (len(self.child) > 1): return True
		if (len(self.child) == 0): return False
		return self.child.values()[0].fork()

	def pop(self, target):
		# We find the target, return it's frequency
		if (self.id == target): return self.count

		
		# For each children node X, 
		# If the ans is -1, it means the target is not the children of X, we can remove it
		# If the ans is not -1, it means the target is the children of X, we update the frequency of current node & delete it if it is target
		count = 0
		remove = []
		for x in self.child:
			ans = self.child[x].pop(target)
			if ans == -1:
				remove.append(x)
			else:
				count += ans
				if (self.child[x].id == target):
					remove.append(x)
		self.count = count

		# Delete the children if it is not our target's parent node
		for x in remove:
			del self.child[x]

		if (count == 0):
			return -1

		return count


	def growth(self, size, prefix):
		# Find the list of vocab that it's frequency >= min support & sort it by frequency in descending order
		results = self.dependency()

		# print prefix
		# print results
		# self.transversal()
		# print '------'


		# If the current sub FP tree is not forked, we generate all combinations
 		if self.fork() is False:
 			patterns = generate(results, prefix)
 			# print '>>', patterns
 			return patterns

		# For each dependency, find the combination in their sub FP tree
		patterns = generate([], prefix)
		for d in results:
			newTree = copy.deepcopy(self) # Clone the tree
			newTree.pop(d[0])
			newPrefix = copy.deepcopy(prefix) # Clone the prefix
			newPrefix.append(d) 
			ans = newTree.growth(size, newPrefix) # Calculate the combinations in the sub FP tree
			# print 'answer', ans
			if (len(ans) != 0):
				patterns += ans
				# print 'patterns', patterns
			# self = tempTree

		

		# print patterns

		return patterns

	# Print the FP Tree of current node
	def transversal(self, n=0):
		print ''.rjust(n*2), self
		for key in self.child:
			self.child[key].transversal(n+1)

	# Print helper function
	def __repr__(self):
		return "[%s %s]" % (self.id, self.count)



# Generate all combinations of the list & the minimum frequency of each combination
def generate(lists, prefix):
	# prefix : [(2, 5, 4), (0, 3, 3)]
	# lists: [(3, 2, 3), (1, 1, 3)]   it means we have vocab id 3 & 1 which their frequencies are 3
	# patterns: [([2, 0, 3], 3), ([2, 0, 1], 3), ([2, 0, 3, 1], 3)]
	# it means that we can generate the combination 
	# 2 0 3
	# 2 0 1
	# 2 0 3 1
	# which their frequencies are also 3

	# print '###########', prefix
	prefixIds = []
	freq = 999999999999999
	if (len(prefix) > 0):
		for p in prefix:
			prefixIds.append(p[0])
			freq = min(freq, p[2])
	# print prefixIds
	# print freq

	patterns = []
	# for number of items that we want to generate (1, 2, ... len(results))
	for L in range(0, len(lists)+1):
		# for the combination that we have
		for subset in itertools.combinations(lists, L):
			if (len(subset) > 0):
				c = subset[0]
				ids = []
				for item in subset:
					ids.append(item[0])
					c = min(c, item[2])
				patterns.append((prefixIds+ids, min(c,freq)))
			else:
				if (len(prefixIds) > 0):
					patterns.append((prefixIds, freq))
			
	# print prefix
	# print lists
	# print patterns

	return patterns



# Input two pairs item1 & item2. We compare their second value only 
def compare(item1, item2):
	if (item1[1] < item2[1]):
		return 1
	if (item1[1] == item2[1]):
		return 0
		if (item1[0] > item2[0]):
			return 1
		if (item1[0] == item2[0]):
			return 0
		return -1
	return -1 


def build(lines, vocabs):
	# For each lines of text file, we have to sort the vocabs of each line order by their frequency
	# For example, 
	# vocabs : [(A, 4), (B, 6), (D, 4), (E, 5), (C, 4)]
	# from:
	# [[0, 1, 2, 3], [1, 4, 3], [0, 1, 2, 3], [0, 1, 4, 3], [0, 1, 4, 2, 3], [1, 4, 2]]
	# to:
	# [[1, 3, 0, 2], [1, 3, 4], [1, 3, 0, 2], [1, 3, 0, 4], [1, 3, 0, 4, 2], [1, 4, 2]]
	lists = []
	for line in lines:
		ids = []
		for id in line:
			if (vocabs[id].count >= minSup): # We only need the vocab if it's frequency >= min support
				ids.append((id, vocabs[id].count))
		ids.sort(compare) # Sort the array by compare function
		list = []
		for id in ids:
			list.append(id[0])
		lists.append(list)	
	# print vocabs
	# print lines
	# print lists
	

	# Build the tree at here
	root = Node(None)
	for ids in lists:
		root.add(ids)

	return root



class Vocab:
	# Constructor of Vocab
	# The default value of count is 0
	def __init__(self, vocab, count=0):
		self.vocab = vocab
		self.count = count
	# Print helper function
	def __repr__(self):
		return "\n(%s, %s)" % (self.vocab, self.count)

def read(file, vocab):
	# Read all vocabs & set the frequency of each vocab is 0
	# [(A, 0), (B, 0), (D, 0), (E, 0), (C, 0)]
	vocabs = []
	for l in vocab:
		text = l.strip().split('\t')
		vocabs.append(Vocab(text[1]))

	# Read the text file & store it into lines
	# Count the vocab frequency & update it
	lines = []
	for line in file:
		text = line.strip().split(' ')
		ids = []
		for id in text:
			ids.append(int(id))
			vocabs[int(id)].count += 1 # Update the vocab frequency
		lines.append(ids)

	return [vocabs, lines]

def run(i):
	# Read the input file & calculate the frequent of each vocab
	results = read(open('topic-'+str(i)+'.txt','r'), open('vocab.txt','r'))
	# results = read(open('test.txt','r'), open('test_vocab.txt','r'))
	vocabs = results[0]
	lines = results[1]
	# print vocabs
	# print lines


	# Build the FP Tree at here, root is the first node of the FP Tree 
	root = build(lines, vocabs)
	# root.transversal()

	# Run the FP Growth algorithm 
	patterns = root.growth(len(vocabs), [])


	# Convert the vocab id to vocab text & sort by support 
	items = []
	for pattern in patterns:

		words = []
		for id in pattern[0]:
			words.append(vocabs[id].vocab)
		# words.sort()

		a = ''
		for word in words:
			a += ' ' + word
		items.append((pattern[1], a.strip()))
	
	items.sort(reverse=True)

	results = []
	for item in items:
		results.append(str(item[0])+'\t'+item[1])

	output = open('pattern-'+str(i)+'.txt','w')
	for result in results:
		# print result
		output.write(result)
		output.write('\n')
	output.close()


# Program start at here

# minSup = 3
# run(0)

minSup = 100
for i in range(0,5):
	print 'Running ' + str(i)
	run(i)



# kakilai (erica.kklai@gmail.com)
# Image Recognition by EM Clustering
# Mar 14, 2016

# Prerequest
# scipy 0.14
# Install
# sudo easy_install scipy==0.14

from PIL import Image
import random
import math

def distance(a, b):
	x = [a[0]-b[0], a[1]-b[1], a[2]-b[2]]
	d = x[0]*x[0] + x[1]*x[1] + x[2]*x[2]
	return d

def multi(a, b):
	return [a[0]*b, a[1]*b, a[2]*b]

def add(a, b):
	return [a[0]+b[0], a[1]+b[1], a[2]+b[2]]

class Pixel:
	def __init__(self, rgb, location):
		self.rgb = rgb
		self.location = location

class Cluster:
	def __init__(self, mean, weight):
		self.mean = mean
		self.weight = weight

	def pdf(self, x, constant):
		t = (-0.5) * (distance(x, self.mean) - constant)
		ans = math.pow(math.e, t)
		return ans


##########################################
# 1. Config
imagename = ["balloons.jpg", "mountains.jpg","nature.jpg", "ocean.jpg", "polarlights.jpg"]
#input_image_filename = imagename[0]
#output_image_filename = "output_" + input_image_filename + k
delta = 10
# no of clusters
#k = 10

def run(k):
    output_image_filename = "output_" + input_image_filename + str(k)
##########################################
# 2. Load Image
    im = Image.open(input_image_filename)
    w = im.width
    h = im.height
    n = w * h
    pix = im.load()

##########################################
# 3. Expectation and Maximization Step
##########################################
# 3.1. Boot step, Initialize K clusters

    pixels = list()
    for x in range(w):
            for y in range(h):
                pixels.append(Pixel(list(pix[x, y]), (x, y)))   # ((R,G,B), (X,Y))

    clusters = list()
    for i in range(k):
        r = random.randint(0, n-1)
        clusters.append(Cluster(pixels[r].rgb, 1.0/k))

    count = 0
    means = [x.mean for x in clusters]

    while (True):
        count += 1
        print '---------------------------------------------------------'
        print 'Iteration: ', count

	##########################################
	# 3.2 Expectation Step
        prob_c_x = [[0 for y in range(n)] for x in range(k)]
        for i in range(n):
                min_distance = float("inf")
                for c in clusters:
                        min_distance = min(min_distance, distance(pixels[i].rgb, c.mean))

                t = 0
                for c in clusters:
                        t += c.pdf(pixels[i].rgb, min_distance) * c.weight

                for j in range(k):
                        prob_c_x[j][i] = clusters[j].pdf(pixels[i].rgb, min_distance) * clusters[j].weight / t

	##########################################
	# 3.3 Maximication Step
        for j in range(k):
                t = 0
                w = [0,0,0]
                for i in range(n):
                        t = t + prob_c_x[j][i]
                        w = add(w, multi(pixels[i].rgb, prob_c_x[j][i]))
                clusters[j].mean = multi(w, 1.0/t)
                clusters[j].weight = t / n

	##########################################
	# 3.4 Check if it could be stop
        new_means = [x.mean for x in clusters]

        print ''
        print 'current mean, new mean:'
        distances = list()
        for i in range(k):
                print means[i], new_means[i]
                distances.append( distance(means[i], new_means[i]))

        print ''
        print 'All distances:'
        print distances
        print 'Max distance: ', max(distances)
        print ''

        if (max(distances) < delta):
                break

        means = new_means


##########################################
# 3.5 Clustering
    cluster_pixels = [list() for x in range(k)]
    for i in range(n):
            max_prob = 0
            max_cluster = -1
            for j in range(k):
                    if (prob_c_x[j][i] > max_prob):
                            max_prob = prob_c_x[j][i]
                            max_cluster = j
            cluster_pixels[max_cluster].append(i)

##########################################
# 4. Output Image
    for i in range(k):
            for x in cluster_pixels[i]:
                    color = (int(round(clusters[i].mean[0])),
                             int(round(clusters[i].mean[1])),
                             int(round(clusters[i].mean[2])))
            im.putpixel(pixels[x].location, color)

    im.save(output_image_filename)
    im.show()
    im.close()
##########################################
#5. Program Starts Here !
for image in imagename:
    input_image_filename = image
    #for k in [10,20,50]: 
    #    run(k)
    run(2)
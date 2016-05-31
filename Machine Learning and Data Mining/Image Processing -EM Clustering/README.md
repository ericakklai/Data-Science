# Image Recognition by EM Clustering


### Prerequesite
scipy 0.14
###Install

```sh
$ sudo easy_install scipy==0.14
```


Image segmentation using EM You can segment an image using a clustering method - each segment is the cluster center to which a pixel belongs. In this exercise, you will represent an image pixel by its r, g, and b values (so use color images!). Use the EM algorithm applied to the mixture of normal distribution model lectured in class to cluster image pixels, then segment the image by mapping each pixel to the cluster center with the highest value of the posterior probability for that pixel. You must implement the EM algorithm yourself (rather than using a package). We will release a set of test images shortly; till then, use any color image you care to.
Segment each of the test images to 10, 20, and 50 segments. You should display these segmented images as images, where each pixel's color is replaced with the mean color of the closest segment
We will identify one special test image. You should segment this to 20 segments using five different start points, and display the result for each case. Is there much variation in the result?
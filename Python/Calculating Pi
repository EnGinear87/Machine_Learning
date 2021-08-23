from pyspark import SparkConf, SparkContext
from decimal import *
import random

conf = SparkConf()
sc = SparkContext('local[*]', conf = conf)

num_samples = 100000000 #The number of samples to use 
def inside(p):     
  x, y = random.random(), random.random() #generate two random numbers for x and y with numbers between 0 and 1
  return x*x + y*y < 1 #returns either 0 or 1(true or false) if the coordinates fall within the area of the circle

count = sc.parallelize(range(0, num_samples)).filter(inside).count() #count the number of samples within the circle that has a value of 1
pi = 4 * count / num_samples #Since the area of the circle is pi rsquared, we can inverse to calculate pi approximately with a large number of samples
print(pi) #print the value of pi

#If we add a decimal to our print statement, we can increase the amount of decimal places
#which can yield a higher precision but it is based on the number of samples

print(Decimal(pi))


#If we add the getcontext().prec from the decimal module, we can specify how precise we want to be
#by changing the numberical value of .prec

getcontext().prec = 7
pi2 = Decimal(4 * count)/ num_samples
print(Decimal(pi2))
sc.stop()


# !spark-submit M5_Spark Assignment_Calculating Pi.py
# Reference:  https://blog.codecentric.de/en/2016/04/calculating-pi-apache-spark/

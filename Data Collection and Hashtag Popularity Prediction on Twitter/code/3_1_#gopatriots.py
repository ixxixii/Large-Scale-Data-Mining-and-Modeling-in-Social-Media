import json
import time
import datetime

print 'Processing #gopatriots'

f = open('tweet_data/tweets_#gopatriots.txt', 'r')
# k = open('num_tweet_#gopatriots', 'w')
g = open('3_1/status_#gopatriots', 'w')

# 3000 is the estimated max number of hour slots
# num_tweet = [0] * 3000
num_hour = -1
total_tweet = 0
total_follow = 0
total_retweet = 0

# Make sure to go back to file head
f_start = f.tell()
first_line = f.readline()
start_date = (json.loads(first_line))['firstpost_date']
f.seek(f_start)

for line in f.readlines():
	tweet = json.loads(line)

	post_date = tweet['firstpost_date']
	num_hour = (post_date - start_date) / 3600
	# if num_hour > 2999:
	#	print 'Over boundary of hour slots!'
	#	num_hour = -2
	#	break
	# num_tweet[num_hour] += 1
	
	total_tweet += 1
	total_follow += tweet['original_author']['followers']
	total_retweet += tweet['metrics']['citations']['total']

# for i in range(num_hour+1):
#	k.write(str(num_tweet[i]))
#	k.write('\n')

g.write('total_tweet ')
g.write(str(total_tweet))
g.write('\nnum_hour ')
g.write(str(num_hour+1))
g.write('\ntotal_follow ')
g.write(str(total_follow))
g.write('\ntotal_retweet ')
g.write(str(total_retweet))
	

f.close()
# k.close()
g.close()

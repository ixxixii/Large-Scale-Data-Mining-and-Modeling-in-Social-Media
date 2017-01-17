import json
import time
import datetime

print 'Processing #superbowl'

f = open('tweet_data/tweets_#superbowl.txt', 'r')
k = open('3_1/num_tweet_#superbowl', 'w')
g = open('3_1/status_#superbowl', 'w')
fea = open('3_2/feature_#superbowl', 'w')
nex = open('3_2/next_#superbowl', 'w')

# 1000 is the estimated max number of hour slots
num_tweet = [0] * 1000
num_hour = -1
total_tweet = 0
total_follow = 0
total_retweet = 0

hour_follow = 0
hour_retweet = 0
hour_max_follow = -1
last_num_hour = 0
last_num_tweet = 0

# Make sure to go back to file head
f_start = f.tell()
first_line = f.readline()
f.seek(f_start)
start_date = (json.loads(first_line))['firstpost_date']
# From the nearest o'clock
tmp_date = datetime.datetime.fromtimestamp(start_date)
tmp_date = tmp_date.replace(minute=0, second=0)
start_date = int(time.mktime(tmp_date.timetuple()))

for line in f.readlines():
	tweet = json.loads(line)

	post_date = tweet['firstpost_date']
	num_hour = (post_date - start_date) / 3600
	if num_hour > 999:
		print 'Over boundary of hour slots!'
		num_hour = -2
		break
	num_tweet[num_hour] += 1
	
	follow = tweet['original_author']['followers']
	retweet = tweet['metrics']['citations']['total']
	total_tweet += 1
	total_follow += follow
	total_retweet += retweet
	
	# Ensure nonzero num_tweet
	if last_num_hour>=376 and last_num_hour<=952 and num_hour!=last_num_hour:
		fea.write(str(last_num_tweet))
		fea.write(',')
		fea.write(str(hour_retweet))
		fea.write(',')
		fea.write(str(hour_follow))
		fea.write(',')
		fea.write(str(hour_max_follow))
		fea.write(',')
		dt = datetime.datetime.fromtimestamp(post_date)
		fea.write(str(dt.hour))
		fea.write('\n')
	if last_num_hour>=377 and last_num_hour<=953 and num_hour!=last_num_hour:
		nex.write(str(last_num_tweet))
		nex.write('\n')			
	
	if num_hour == last_num_hour:
		hour_follow += follow
		hour_retweet += retweet
		if follow > hour_max_follow:
			hour_max_follow = follow
	else:
		hour_follow = follow
		hour_retweet = retweet
		hour_max_follow = follow
		
	last_num_hour = num_hour
	last_num_tweet = num_tweet[num_hour]

for i in range(num_hour+1):
	k.write(str(num_tweet[i]))
	k.write('\n')

g.write('total_tweet ')
g.write(str(total_tweet))
g.write('\nnum_hour ')
g.write(str(num_hour+1))
g.write('\ntotal_follow ')
g.write(str(total_follow))
g.write('\ntotal_retweet ')
g.write(str(total_retweet))
	

f.close()
k.close()
g.close()
fea.close()
nex.close()

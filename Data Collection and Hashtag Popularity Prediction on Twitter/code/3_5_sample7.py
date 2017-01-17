import io
import json
import time
import datetime
from sets import Set

print 'Processing #sample7_period3'

f = io.open('./test_data/sample7_period3.txt', 'r',encoding='utf8')
fea = open('./3_5/feature_7', 'w')
nex = open('./3_5/next_7', 'w')

# 1000 is the estimated max number of hour slots
num_tweet = [0] * 1000
num_hour = -1

hour_follow = 0
hour_retweet = 0
hour_max_follow = -1
hour_ranking_score=0;
hour_mention = 0;
hour_author = Set([])
last_num_hour = 0
last_num_tweet = 0

# Make sure to go back to file head
f_start = f.tell()
first_line = f.readline()
f.seek(f_start)
start_date = (json.loads(first_line))['firstpost_date']
# From the nearest o'clock
# tmp_date = datetime.datetime.fromtimestamp(start_date)
# tmp_date = tmp_date.replace(minute=0, second=0)
# start_date = int(time.mktime(tmp_date.timetuple()))

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
	ranking_score=tweet['metrics']['ranking_score']
	
	# Ensure nonzero num_tweet
	if num_hour!=last_num_hour:
		fea.write(str(hour_retweet))
		fea.write(',')
		fea.write(str(hour_ranking_score))
		fea.write(',')
		fea.write(str(hour_mention))
		fea.write('\n')
		nex.write(str(last_num_tweet))
		nex.write('\n')			
	
	if num_hour == last_num_hour:
		hour_follow += follow
		hour_retweet += retweet
		hour_ranking_score += ranking_score
		hour_mention += len(tweet['tweet']['entities']['user_mentions'])
		hour_author.add(tweet['author']['name'])
		if follow > hour_max_follow:
			hour_max_follow = follow
	else:
		hour_follow = follow
		hour_retweet = retweet
		hour_max_follow = follow
		hour_ranking_score = ranking_score
		hour_mention = len(tweet['tweet']['entities']['user_mentions'])
		hour_author=Set([])
		
	last_num_hour = num_hour
	last_num_tweet = num_tweet[num_hour]



f.close()
fea.close()
nex.close()

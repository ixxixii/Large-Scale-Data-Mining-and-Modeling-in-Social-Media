import io
import json
import time
import datetime
from sets import Set
#for the first period(hour 697-816), we take 10*12 data
#for the second perid(hour 817-828), we take 1*12 data
#for the third period(hour 829-948), we take 10*12 data
#we use 12 cross fold validation, which works as well as 10 cross fold validation

print 'Processing #superbowl'

f = io.open('./tweet_data/tweets_#superbowl.txt', 'r', encoding='utf8')
fea1 = open('3_4/newfeature1_#superbowl', 'w')
nex1 = open('3_4/next1_#superbowl', 'w')
fea2 = open('3_4/newfeature2_#superbowl', 'w')
nex2 = open('3_4/next2_#superbowl', 'w')
fea3 = open('3_4/newfeature3_#superbowl', 'w')
nex3 = open('3_4/next3_#superbowl', 'w')

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
	ranking_score=tweet['metrics']['ranking_score']
	
	# period 1
	if last_num_hour>=696 and last_num_hour<=815 and num_hour!=last_num_hour:
		fea1.write(str(last_num_tweet))
		fea1.write(',')
		fea1.write(str(hour_retweet))
		fea1.write(',')
		fea1.write(str(hour_follow))
		fea1.write(',')
		dt = datetime.datetime.fromtimestamp(post_date)
		fea1.write(str(dt.hour))
		fea1.write(',')
		fea1.write(str(hour_ranking_score))
		fea1.write(',')
		fea1.write(str(hour_mention))
		fea1.write(',')
		fea1.write(str(len(hour_author)))
		fea1.write('\n')
	if last_num_hour>=697 and last_num_hour<=816 and num_hour!=last_num_hour:
		nex1.write(str(last_num_tweet))
		nex1.write('\n')	

	# period 2
	if last_num_hour>=816 and last_num_hour<=827 and num_hour!=last_num_hour:
		fea2.write(str(last_num_tweet))
		fea2.write(',')
		fea2.write(str(hour_retweet))
		fea2.write(',')
		fea2.write(str(hour_follow))
		fea2.write(',')
		dt = datetime.datetime.fromtimestamp(post_date)
		fea2.write(str(dt.hour))
		fea2.write(',')
		fea2.write(str(hour_ranking_score))
		fea2.write(',')
		fea2.write(str(hour_mention))
		fea2.write(',')
		fea2.write(str(len(hour_author)))
		fea2.write('\n')
	if last_num_hour>=817 and last_num_hour<=828 and num_hour!=last_num_hour:
		nex2.write(str(last_num_tweet))
		nex2.write('\n')

	# period 3
	if last_num_hour>=828 and last_num_hour<=947 and num_hour!=last_num_hour:
		fea3.write(str(last_num_tweet))
		fea3.write(',')
		fea3.write(str(hour_retweet))
		fea3.write(',')
		fea3.write(str(hour_follow))
		fea3.write(',')
		dt = datetime.datetime.fromtimestamp(post_date)
		fea3.write(str(dt.hour))
		fea3.write(',')
		fea3.write(str(hour_ranking_score))
		fea3.write(',')
		fea3.write(str(hour_mention))
		fea3.write(',')
		fea3.write(str(len(hour_author)))
		fea3.write('\n')
	if last_num_hour>=829 and last_num_hour<=948 and num_hour!=last_num_hour:
		nex3.write(str(last_num_tweet))
		nex3.write('\n')		
	
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
fea1.close()
nex1.close()
fea2.close()
nex2.close()
fea3.close()
nex3.close()

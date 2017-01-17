load 'feature_#nfl';
load 'feature_#superbowl';
load 'next_#nfl';
load 'next_#superbowl';
reg_nfl=[284.6759;-1.2598;0.6902;0.0002;-0.0004;-0.5690];
reg_sb=[222.7326;0.7770;0.0198;4.819e-06;-1.392e-05;-11.5429];

nfl=feature__nfl(360:460-1,:);
sb=feature__superbowl(324:424-1,:);
tag_nfl=next__nfl(360:460-1);
tag_sb=next__superbowl(324:424-1);

result_nfl=nfl*reg_nfl(2:6)+reg_nfl(1,1);
result_sb=sb*reg_sb(2:6)+reg_sb(1,1);

error_nfl=mean(abs(result_nfl-tag_nfl)./tag_nfl);
error_sb=mean(abs(result_sb-tag_sb)./tag_sb);
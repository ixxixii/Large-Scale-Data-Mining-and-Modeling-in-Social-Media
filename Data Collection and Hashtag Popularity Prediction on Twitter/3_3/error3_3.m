load 'newfeature_#nfl';
load 'newfeature_#superbowl';
load 'next_#nfl';
load 'next_#superbowl';
reg_nfl=[127.1865;4.4626;-0.4967;3.222e-5;-10.9526;-1.2193;9.1090;0.4904];
reg_sb=[284.9362;-11.2907;0.0315;-2.24e-06;-10.1042;2.6218;0.9003;-0.7862];

nfl=newfeature__nfl(360:460-1,:);
sb=newfeature__superbowl(324:424-1,:);
tag_nfl=next__nfl(360:460-1);
tag_sb=next__superbowl(324:424-1);

result_nfl=nfl*reg_nfl(2:8)+reg_nfl(1,1);
result_sb=sb*reg_sb(2:8)+reg_sb(1,1);

error_nfl=mean(abs(result_nfl-tag_nfl)./tag_nfl);
error_sb=mean(abs(result_sb-tag_sb)./tag_sb);
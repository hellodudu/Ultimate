update arena_player set score = 1000 where score > 1000 and score < 1500;
update arena_player set score = 1200 where score >= 1500 and score < 1800;
update arena_player set score = 1500 where score >= 1800 and score < 2100;
update arena_player set score = 1800 where score >= 2100 and score < 2500;
update arena_player set score = 2100 where score >= 2500;

alter table arena_champion add column player_name varchar(32) default '' not null;
alter table arena_champion add column server_name varchar(32) default '' not null;
alter table arena_champion add column master_id int(10) default 1 not null;
alter table arena_champion add column fashion_id int(10) default -1 not null;

update arena_champion set player_name = "龙井虾仁儿", server_name = "S17-风生水起", master_id = 17, fashion_id = 158 where champion_rank = 0;
update arena_champion set player_name = "买买买、", server_name = "S33-古圣先贤", master_id = 17, fashion_id = 103 where champion_rank = 1;
update arena_champion set player_name = "夜殇羽", server_name = "S65-仙山楼阁", master_id = 17, fashion_id = 48 where champion_rank = 2;

drop database if exists `db_ultimate`;
create database `db_ultimate` character set utf8mb4;

use `db_ultimate`;
drop table if exists `global`;
create table global (
    `id` int(10) not null default '0' comment 'ultimate id',
    `time_stamp` int(10) not null default '0' comment 'current time',
    `arena_season` int(10) not null default '0' comment 'arena seasons',
    `arena_season_end_time` int(10) not null default '0' comment 'arena season end time',
    primary key (`id`)
) engine=innodb default charset=utf8mb4 collate utf8mb4_general_ci comment='global table';

drop table if exists `world`;
create table world (
    `id` int(10) not null default '0' comment 'world id',
    `name` varchar(32) not null default '' comment 'world name',
    `last_connect_time` int(10) not null default '0' comment 'last connect time stamp',
    primary key (`id`)
) engine=innodb default charset=utf8mb4 collate utf8mb4_general_ci comment='world';

drop table if exists `arena_player`;
create table arena_player (
    `player_id` bigint(20) not null default '-1' comment 'player id',
    `score` int(10) not null default '0' comment 'arena score',
    `reach_time` int(10) not null default '0' comment 'arena score update time',
    `last_target` bigint(20) not null default '-1' comment 'same target cannot be met twice',
    primary key (`player_id`)
) engine=innodb default charset=utf8mb4 collate utf8mb4_general_ci comment='arena player';

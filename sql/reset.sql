drop database if exists `db_ultimate`;
create database `db_ultimate` character set utf8mb4;

use `db_ultimate`;
drop table if exists `global`;
create table global (
    `id` int(10) not null default '0' comment 'ultimate id',
    `time_stamp` int(10) not null default '0' comment 'current time',
    primary key (`id`)
) engine=innodb default charset=utf8mb4 collate utf8mb4_general_ci comment='global table';

drop table if exists `world`;
create table world (
    `id` int(10) not null default '0' comment 'world id',
    `last_connect_time` int(10) not null default '0' comment 'last connect time stamp',
    primary key (`id`)
) engine=innodb default charset=utf8mb4 collate utf8mb4_general_ci comment='world';

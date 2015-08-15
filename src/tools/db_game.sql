DROP DATABASE IF EXISTS db_game;
CREATE DATABASE db_game CHARACTER SET utf8;
USE db_game;

/* 角色表 */
CREATE TABLE role (
    role_id int(10) not null default '0' comment '角色id',
    account_id int(10) not null default '0' comment '账号id',
    name varchar(20) COMMENT '角色名',
    sex tinyint(3) not null default '1' comment '性别',
    level smallint(5) not null default '1' comment '等级',
    diamond int(10) not null default '0' comment '钻石',
    PRIMARY KEY (role_id)
) ENGINE=MYISAM DEFAULT CHARSET=utf8 COMMENT='角色表';

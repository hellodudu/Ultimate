## 监视服务器状态

* 接口(http post)

```
    http://127.0.0.1:8080/debug/vars
```

* 参数

    无

* 返回(json)

```
    {
    "arena_player_data_num": 1,
    "arena_record_num": 0,
    "cmdline": [
        "/var/folders/d2/sj12ms557b5_tq5yg83vhxf40000gn/T/go-build443476864/b001/exe/main"
    ],
    "cores": 4,
    "gcpause": 0,
    "goroutine": 24,
    ...
    }
```

> arena_player_data_num:参与鸿蒙的玩家数量(已有鸿蒙数据)
> arena_record_num:玩家镜像数量
> ...其他系统信息

## 单个玩家鸿蒙数据

* 接口(http post)

```
    http://127.0.0.1:8080/arena_get_player_data
```

* 参数(json)

```
    {"id": 1688568393877168450} 
```

* 返回(json)

```
    {
    "player_id": 1688568393877168450,
    "score": 1430,
    "reach_time": 1555860064,
    "last_target": 568860936522177381
    }
```

## 等待匹配的玩家列表

* 接口(http post)

```
    http://127.0.0.1:8080/arena_matching_list
```

* 参数

    无

* 返回(json)

```
    {
    "id": []
    }
```

## 正在请求镜像的玩家列表

* 接口(http post)

```
    http://127.0.0.1:8080/arena_record_req_list
```

* 参数

    无

* 返回(json)

```
    {
    "1407656367119925422": 1555993212,
    "1407656367119925449": 1555993212,
    }
```

## 单个玩家镜像数据

* 接口(http post)

```
    http://127.0.0.1:8080/arena_get_record
```

* 参数

    无

* 返回(json)

```
    {"player_id":281483566645250...}
```

## 单个玩家info

* 接口(http post)

```
    http://127.0.0.1:8080/player_info
```

* 参数

```
    {"id": 281483566645249} 
```

* 返回(json)

```
    {"player_id":281483566645249,"server_id":1,"gender":1,"race":1,"head_quality":1,"player_name":"慕无情","guild_id":281474976710657,"last_logoff_time":1295053538,"online":true,"level":100,"player_score":3603,"history_score":3603,"hero_type_id":[4294967295,5,1029,4294967295,4294967295,4294967295],"fashion_id":[4294967295,4294967295,4294967295,4294967295,4294967295,4294967295],"mount_type_id":[4294967295,4294967295,4294967295,4294967295,4294967295,4294967295],"rune_type_id":[101,501,4294967295,4294967295],"state_flag":[0,0,0,0,0,0],"return_player_id":-1,"last_square_action_time":4294967295}
```

## 单个帮会info

* 接口(http post)

```
    http://127.0.0.1:8080/guild_info
```

* 参数

```
    {"id": 281474976710657} 
```

* 返回(json)

```
    {"guild_id":281474976710657,"guild_name":"1","server_id":1,"master_id":281483566645249,"master_name":"慕无情","create_time":1295053602,"hall_level":1,"score":36,"castle_score":1000,"member_num":1}
```

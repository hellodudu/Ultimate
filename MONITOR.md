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

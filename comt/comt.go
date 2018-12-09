package comt

import "time"

type Comt struct {
	ComtID   int64     `json:id`        // AppID(16) + TopicID(16) + commentid(32)
	AutherID int64     `json:auther_id` // player id
	Auther   string    `json:auther`    // player name
	Text     string    `json:text`      // comment text
	Evaluate int       `json:evaluate`  // 1-5 star
	GenTime  time.Time `json:time`
}

type Topic struct {
	TopicID int `json:id`
}

type App struct {
	AppID  int    `json:id`
	PubKey string `json:pub_key`
	PriKey string `json:pri_key`
}

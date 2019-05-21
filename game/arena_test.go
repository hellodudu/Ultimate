package game

import (
	"testing"
)

func init() {

}

func TestNewSeasonRank(t *testing.T) {

	mapTest := make(map[int32]int32)
	mapTest[1001] = 1000
	mapTest[1100] = 1000
	mapTest[1199] = 1000
	mapTest[1201] = 1000
	mapTest[1499] = 1000
	mapTest[1500] = 1200
	mapTest[1788] = 1200
	mapTest[1801] = 1500
	mapTest[2099] = 1500
	mapTest[2101] = 1800
	mapTest[2499] = 1800
	mapTest[2501] = 2100
	mapTest[2999] = 2100
	mapTest[3001] = 2100
	mapTest[3569] = 2100
	mapTest[3600] = 2100
	mapTest[4000] = 2100

	for k, v := range mapTest {
		oldSec := getSectionIndexByScore(k)

		newSec := func(s int32) int32 {
			if s > 4 {
				return 4
			}

			if s == 0 {
				return 0
			}

			return s - 1
		}(oldSec)

		newScore := getDefaultScoreBySection(newSec)

		if newScore != v {
			t.Error("score: ", k, " 's result:", newScore, " is wrong, should be:", v)
		}
	}
}

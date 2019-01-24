package game

import "context"

// global
var game *Game

type Game struct {
	arena  *Arena // arena
	ctx    context.Context
	cancel context.CancelFunc
}

func NewGame() (*Game, error) {
	if game != nil {
		return game, nil
	}

	game = &Game{}

	game.ctx, game.cancel = context.WithCancel(context.Background())
	game.arena = NewArena(game.ctx)
	return game, nil
}

func GetGame() *Game {
	if game == nil {
		NewGame()
	}

	return game
}

func (game *Game) GetArena() *Arena {
	return game.arena
}

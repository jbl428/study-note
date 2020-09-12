package connect

// ResultOf determins the winner of the given Hex board
func ResultOf(board []string) (string, error) {
	if isWinner('O', board) {
		return "O", nil
	}

	invertedBoard := make([]string, len(board[0]))
	for i := range board[0] {
		var temp []byte
		for j := range board {
			temp = append(temp, board[j][i])
		}
		invertedBoard[i] = string(temp)
	}

	if isWinner('X', invertedBoard) {
		return "X", nil
	}

	return "", nil
}

func isWinner(user byte, board []string) bool {
	hasStoneAtEndLine := false
	for _, stone := range board[len(board)-1] {
		if byte(stone) == user {
			hasStoneAtEndLine = true
			break
		}
	}

	if !hasStoneAtEndLine {
		return false
	}

	isConnected := makeChecker(user, board)
	for i, stone := range board[0] {
		if byte(stone) == user && isConnected(i, 0) {
			return true
		}
	}

	return false
}

func makeChecker(user byte, board []string) (fn func(int, int) bool) {
	maxX := len(board[0]) - 1
	maxY := len(board) - 1
	visitBoard := makeVisitBoard(maxX+1, maxY+1)

	isNextStone := func(x int, y int) bool {
		return !(x < 0 || x > maxX || y < 0 || y > maxY) &&
			board[y][x] == user &&
			!visitBoard[y][x]
	}

	fn = func(x int, y int) bool {
		if y == maxY {
			return true
		}

		visitBoard[y][x] = true

		if isNextStone(x-1, y) && fn(x-1, y) {
			return true
		}
		if isNextStone(x+1, y) && fn(x+1, y) {
			return true
		}
		if isNextStone(x, y-1) && fn(x, y-1) {
			return true
		}
		if isNextStone(x+1, y-1) && fn(x+1, y-1) {
			return true
		}
		if isNextStone(x, y+1) && fn(x, y+1) {
			return true
		}
		if isNextStone(x-1, y+1) && fn(x-1, y+1) {
			return true
		}

		return false
	}

	return
}

func makeVisitBoard(x int, y int) [][]bool {
	visitBoard := make([][]bool, y)
	for i := range visitBoard {
		visitBoard[i] = make([]bool, x)
	}

	return visitBoard
}

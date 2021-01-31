package diamond

import "errors"

// Gen genrate a diamond shape string
func Gen(char byte) (string, error) {
	if char < 'A' || char > 'Z' {
		return "", errors.New("error")
	}

	size := 2*(char-'A') + 1
	diamond := make([][]byte, size)
	for i := range diamond {
		diamond[i] = make([]byte, size)
	}

	middleIndex := size / 2
	index := byte(0)
	for ; index <= (middleIndex); index++ {
		diamond[index][middleIndex+index] = 'A' + index
		diamond[index][middleIndex-index] = 'A' + index
		diamond[size-1-index][middleIndex+index] = 'A' + index
		diamond[size-1-index][middleIndex-index] = 'A' + index
	}

	return byteArrayToString(diamond), nil
}

func byteArrayToString(arr [][]byte) (result string) {
	for _, row := range arr {
		for _, col := range row {
			if col == 0 {
				result += " "
			} else {
				result += string(col)
			}
		}
		result += "\n"
	}

	return
}

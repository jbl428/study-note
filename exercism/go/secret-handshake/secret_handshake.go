package secret

func decimalToBinary(number uint, result []uint) []uint {
	if number == 0 {
		return result
	} else if number == 1 {
		result = append(result, number)
		return result
	}

	return decimalToBinary(number/2, append(result, number%2))
}

func reverse(input []string) {
	for i, j := 0, len(input)-1; i < j; i, j = i+1, j-1 {
		input[i], input[j] = input[j], input[i]
	}

	return
}

// Handshake converts the given number to the appropriate sequence of events for a secret handshake.
func Handshake(number uint) (result []string) {
	binary := decimalToBinary(number, make([]uint, 0))

	for i, val := range binary {
		if val == 0 {
			continue
		}

		switch i {
		case 0:
			result = append(result, "wink")
		case 1:
			result = append(result, "double blink")
		case 2:
			result = append(result, "close your eyes")
		case 3:
			result = append(result, "jump")
		case 4:
			reverse(result)
		}
	}

	return
}

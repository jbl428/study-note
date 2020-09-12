package react

type cell struct {
	value     int
	callbacks []func(int)
}

var _ Cell = (*cell)(nil)
var _ InputCell = (*cell)(nil)
var _ ComputeCell = (*cell)(nil)

func (c cell) Value() int {
	return c.value
}

func (c *cell) SetValue(value int) {
	if c.value != value {
		for _, callback := range c.callbacks {
			if callback != nil {
				callback(value)
			}
		}
	}
	c.value = value
}

type canceler struct {
	do func()
}

func (c canceler) Cancel() {
	c.do()
}

func (c *cell) AddCallback(callback func(int)) Canceler {
	c.callbacks = append(c.callbacks, callback)

	temp := &canceler{}
	length := len(c.callbacks) - 1

	temp.do = func() {
		c.callbacks[length] = nil
	}

	return temp
}

type reactor struct{}

func (r reactor) CreateInput(val int) InputCell {
	cell := &cell{val, nil}

	return cell
}

func (r reactor) CreateCompute1(c Cell, f func(int) int) ComputeCell {
	var computeCell = c.(ComputeCell)
	cell := &cell{f(c.Value()), nil}
	computeCell.AddCallback(func(v int) {
		cell.SetValue(f(v))
	})

	return cell
}

func (r reactor) CreateCompute2(a Cell, b Cell, f func(int, int) int) ComputeCell {
	var aCell = a.(ComputeCell)
	var bCell = b.(ComputeCell)
	cell := &cell{f(aCell.Value(), bCell.Value()), nil}
	aCell.AddCallback(func(v int) {
		cell.SetValue(f(v, bCell.Value()))
	})
	bCell.AddCallback(func(v int) {
		cell.SetValue(f(aCell.Value(), v))
	})

	return cell
}

// New returns new reactor
func New() Reactor {
	return reactor{}
}

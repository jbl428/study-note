// Package react implements a basic reactive system.
package react

type reactor struct {
	registry []*computeCell
}

type inputCell struct {
	value int
	r     *reactor
}

type computeCell struct {
	compute   func() int
	callbacks map[int]func(int)
	prevVal   int
}

type canceler struct {
	id           int
	registeredTo *computeCell
}

// New returns new reactor.
func New() Reactor {
	return &reactor{
		registry: make([]*computeCell, 0),
	}
}

// CreateInput creates an input cell.
func (r *reactor) CreateInput(i int) InputCell {
	return &inputCell{
		value: i,
		r:     r,
	}
}

// CreateCompute1 creates a compute 1 cell.
func (r *reactor) CreateCompute1(c Cell, compute func(int) int) ComputeCell {
	return r.createCompute(
		func() int {
			return compute(c.Value())
		},
	)
}

// CreateCompute2 creates a compute 2 cell.
func (r *reactor) CreateCompute2(c1, c2 Cell, compute func(int, int) int) ComputeCell {
	return r.createCompute(
		func() int {
			return compute(c1.Value(), c2.Value())
		},
	)
}

func (r *reactor) createCompute(compute func() int) *computeCell {
	var cc computeCell

	cc.compute = compute
	cc.callbacks = make(map[int]func(int))
	cc.prevVal = cc.Value()
	r.registry = append(r.registry, &cc)

	return &cc
}

// SetValue sets the value on an input cell.
func (ic *inputCell) SetValue(i int) {
	ic.value = i
	ic.r.update()
}

// Value returns the value of an input cell.
func (ic *inputCell) Value() int {
	return ic.value
}

// Value returns the value of a compute cell.
func (cc *computeCell) Value() int {
	return cc.compute()
}

// AddCallback adds a callback function to a compute cell.
func (cc *computeCell) AddCallback(cb func(int)) Canceler {
	cc.callbacks[len(cc.callbacks)] = cb

	return &canceler{
		id:           len(cc.callbacks) - 1,
		registeredTo: cc,
	}
}

// Cancel removes a callback function from a compute cell.
func (c *canceler) Cancel() {
	delete(c.registeredTo.callbacks, c.id)
}

func (r *reactor) update() {
	for _, cc := range r.registry {
		if len(cc.callbacks) == 0 || cc.Value() == cc.prevVal {
			continue
		}

		for _, cb := range cc.callbacks {
			cb(cc.Value())
		}

		cc.prevVal = cc.Value()
	}
}

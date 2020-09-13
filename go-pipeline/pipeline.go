package pipeline

// Stage is the operation of pipeline
type Stage func(input chan int) (output chan int)

func makeStage(fn func(int) int) Stage {
	return func(input chan int) (output chan int) {
		output = make(chan int)
		go func() {
			defer close(output)
			for val := range input {
				output <- fn(val)
			}
		}()

		return
	}
}

// Take makes a stage taking the given number of results from a input channel
func Take(num uint) Stage {
	return func(input chan int) (output chan int) {
		output = make(chan int)
		go func() {
			defer close(output)
			var count uint
			for val := range input {
				output <- val
				count++
				if count == num {
					return
				}
			}
		}()

		return
	}
}

// Adder makes a stage adding the given number from a inputs channel
func Adder(num int) Stage {
	return makeStage(func(val int) int {
		return num + val
	})
}

// Multiply makes a stage multiplying the given number from a inputs channel
func Multiply(num int) Stage {
	return makeStage(func(val int) int {
		return num * val
	})
}

// Pipeline is the interface that represent the pipeline
type Pipeline interface {
	Send(int)
	Receive(callback func(int))
	Close()
}

type pipeline struct {
	source chan int
	output chan int
}

func (p pipeline) Send(val int) {
	p.source <- val
}

func (p pipeline) Receive(callback func(int)) {
	for result := range p.output {
		callback(result)
	}
}

func (p pipeline) Close() {
	close(p.source)
}

// NewPipeline returns a new pipeline
func NewPipeline(stages ...Stage) Pipeline {
	source := make(chan int)
	var output chan int
	for _, stage := range stages {
		if output == nil {
			output = stage(source)
		} else {
			output = stage(output)
		}
	}

	return pipeline{source, output}
}

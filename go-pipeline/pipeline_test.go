package pipeline

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestPipeline(t *testing.T) {
	pipeline := NewPipeline(Adder(3), Multiply(5), Adder(-3), Take(2), Adder(10))

	go func() {
		for _, val := range []int{1, 2, 3, 4, 5} {
			pipeline.Send(val)
		}
		pipeline.Close()
	}()

	var results []int
	pipeline.Receive(func(num int) {
		results = append(results, num)
	})

	assert.Equal(t, []int{27, 32}, results)
}

func BenchmarkPipeline(b *testing.B) {
	pipeline := NewPipeline(Adder(3), Multiply(5), Adder(-3))

	go func() {
		for i := 0; i < b.N; i++ {
			pipeline.Send(i)
		}
		pipeline.Close()
	}()

	pipeline.Receive(func(num int) {})
}

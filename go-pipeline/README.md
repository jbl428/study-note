# Golang pipeline implementation using channel

- usage

```golang
pipeline := NewPipeline(Multiply(5), Adder(-3), Take(2))

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
```

- benchmark

```text
Running tool: /usr/local/bin/go test -benchmem -run=^$ pipeline -bench ^(BenchmarkPipeline)$

goos: darwin
goarch: amd64
pkg: pipeline
BenchmarkPipeline-4      875965       1356 ns/op        0 B/op        0 allocs/op
PASS
ok   pipeline 1.324s
```

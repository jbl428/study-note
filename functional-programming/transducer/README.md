# Transducers

함수형 javascript 라이브러리 중 하나인 ramda에는 `transduce`라는 함수가 있다.
array를 받는 여러 단일 인자 함수들을 `compose`나 `pipe`로 합성한 함수에
배열을 넘겨주면 배열의 순회가 합성하기 전 함수의 개수만큼 실행된다.
예를 들면 다음과 같은 함수가 있다고 하자

```typescript
let transform = R.compose(
  R.filter(x => /r/i.test(x)),
  R.map(R.toUpper),
  R.map(R.reverse)
)
```

위 `transform`함수에 배열을 전달하면 새로운 배열이 map함수에 의해 2번 생성, 순회를 하고 filter에 의해 다시 한 번 생성, 순회를 한다. 배열의 크키가 작으면 문제가 없지만 크다면 비효율적이다. 이를 해결하기 위해 `transduce`가 존재하며 내부적으로 reduce를 이용해서 순회작업이 한번만 이루어지도록 해준다.

```typescript
const autobots = ['Optimus Prime','Bumblebee','Ironhide','Sunstreaker','Ratchet']

R.transduce(transform, R.flip(R.append), [], autobots)
```

위와 같은 경우 `autobots`의 첫 번째 항목인 'Optimus Prime'이 `R.reverse`, `R.toUpper`가 차례로 적용 후 `R.filter`의 조건에 맞다면 결과배열에 추가된다.
이후 두 번째 항목에 대해서도 같은방식으로 수행되어 결국 생성, 순회는 한번만 이루어진다.

index.ts에 typescript를 이용해서 transduce를 구현한 내용을 넣어두었다.

## 참고 문서

<https://www.jeremydaly.com/transducers-supercharge-functional-javascript/>

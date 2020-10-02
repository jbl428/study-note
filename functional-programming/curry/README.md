# Typescript로 커링과 부분적용 구현

즉시실행함수를 이용해서 커링과 부분적용을 동시에 구현한 코드를 보고 참신하다고 생각되어서 기록함

```typescript
function curry(fn: (...args: any[]) => any) {
  const arity = fn.length;

  return (function resolver(...args: any[]) {
    const memory = [...args];

    return function (...innerArgs: any[]) {
      const local = [...memory, ...innerArgs];
      const next = local.length >= arity ? fn : resolver;

      return next(...local);
    };
  })();
}

function adder(a: number, b: number, c: number) {
  return a + b + c;
}

const curriedAdder = curry(adder);

console.log(curriedAdder(1, 2, 3)); // 6
console.log(curriedAdder(1)(2)(3)); // 6
console.log(curriedAdder(1, 2)(3)); // 6
console.log(curriedAdder(1)(2, 3)); // 6
```

위 소스를 실행해주는 `Dockerfile`을 만들어 두었으니 빌드하여 실행해볼 수 있다.

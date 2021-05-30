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

console.log(curriedAdder(1, 2, 3));
console.log(curriedAdder(1)(2)(3));
console.log(curriedAdder(1, 2)(3));
console.log(curriedAdder(1)(2, 3));
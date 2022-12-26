import { describe, it } from "node:test";
import { deepStrictEqual } from "node:assert/strict";
import { ap, Distributions, liftA2, map } from "./Distributions";
import { pipe } from "fp-ts/function";

describe("Distributions", () => {
  it("distributions 객체를 생성한다", () => {
    const dist = Distributions.of([
      [1, 0.5],
      [2, 0.5],
    ]);

    deepStrictEqual(dist.value, [
      [1, 0.5],
      [2, 0.5],
    ]);
  });

  it("같은 case를 가진 항목끼리 합산해 하나로 만든다", () => {
    const dist = Distributions.of([
      [1, 0.1],
      [2, 0.3],
      [2, 0.2],
      [1, 0.4],
    ]);

    deepStrictEqual(dist.value, [
      [1, 0.5],
      [2, 0.5],
    ]);
  });

  it("각 확률의 합을 1로 만든다", () => {
    const dist = Distributions.of([
      [1, 20],
      [2, 40],
      [3, 40],
    ]);

    deepStrictEqual(dist.value, [
      [1, 0.2],
      [2, 0.4],
      [3, 0.4],
    ]);
  });

  it("동일한 확률을 가진 인스턴스를 생성한다", () => {
    const dist = Distributions.uniform([1, 2, 3, 4]);

    deepStrictEqual(dist.value, [
      [1, 0.25],
      [2, 0.25],
      [3, 0.25],
      [4, 0.25],
    ]);
  });

  it("주사위의 값이 짝수가 될 확률을 계산한다", () => {
    const dist = Distributions.uniform([1, 2, 3, 4, 5, 6]);

    deepStrictEqual(
      dist.evaluate((n) => n % 2 === 0),
      0.5
    );
  });

  it("map을 통해 주변분포를 구한다", () => {
    type Tuple = [number, number];
    const dist = Distributions.of<Tuple>([
      [[0, 0], 0.2],
      [[0, 1], 0.4],
      [[1, 0], 0.1],
      [[1, 1], 0.3],
    ]);

    const result = pipe(
      dist,
      map(([_, y]) => y)
    );

    deepStrictEqual(result.value, [
      [0, 0.1 + 0.2],
      [1, 0.4 + 0.3],
    ]);
  });

  it("ap를 통해 결합분포를 구한다", () => {
    const join = (a: number) => (b: number) => `(${a}, ${b})`;
    const dist1 = Distributions.uniform([1, 2]);
    const dist2 = Distributions.uniform([1, 2, 3, 4]);

    const result = pipe(dist1, map(join), ap(dist2));

    deepStrictEqual(result.value, [
      ["(1, 1)", 0.125],
      ["(1, 2)", 0.125],
      ["(1, 3)", 0.125],
      ["(1, 4)", 0.125],
      ["(2, 1)", 0.125],
      ["(2, 2)", 0.125],
      ["(2, 3)", 0.125],
      ["(2, 4)", 0.125],
    ]);
  });

  it("독립시행횟수 10, 성공확률이 0.3인 이항분포를 구한다", () => {
    const sum = (a: number, b: number) => a + b;
    const dists = Array.from({ length: 10 }, () =>
      Distributions.of([
        [0, 0.7],
        [1, 0.3],
      ])
    );

    const result = dists.reduce(liftA2(sum));

    deepStrictEqual(
      result.value.map(([a, b]) => [a, +b.toFixed(5)]),
      [
        [0, 0.02825],
        [1, 0.12106],
        [2, 0.23347],
        [3, 0.26683],
        [4, 0.20012],
        [5, 0.10292],
        [6, 0.03676],
        [7, 0.009],
        [8, 0.00145],
        [9, 0.00014],
        [10, 0.00001],
      ]
    );
  });
});

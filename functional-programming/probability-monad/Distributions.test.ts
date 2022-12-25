import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { Distributions } from "./Distributions";

describe("Distributions", () => {
  it("distributions 객체를 생성한다", () => {
    const dist = Distributions.of([
      [1, 0.5],
      [2, 0.5],
    ]);

    assert.deepStrictEqual(dist.value, [
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

    assert.deepStrictEqual(dist.value, [
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

    assert.deepStrictEqual(dist.value, [
      [1, 0.2],
      [2, 0.4],
      [3, 0.4],
    ]);
  });
});

import "fp-ts/lib/HKT";

type Probability = number;

export class Distributions<CASE> {
  readonly #value: [CASE, Probability][];

  constructor(value: [CASE, Probability][]) {
    this.#value = value;
  }

  static of<CASE>(value: [CASE, Probability][]): Distributions<CASE> {
    const grouped = value.reduce((acc, [c, prob]) => {
      acc.set(c, (acc.get(c) ?? 0) + prob);

      return acc;
    }, new Map<CASE, Probability>());

    const sum = Array.from(grouped.values()).reduce(
      (acc, prob) => acc + prob,
      0
    );
    const normalized = Array.from(grouped.entries()).map(
      ([c, prob]) => [c, prob / sum] as [CASE, Probability]
    );

    return new Distributions(normalized);
  }

  get value(): [CASE, Probability][] {
    return this.#value;
  }
}

export const URI = "Distributions";

export type URI = typeof URI;

declare module "fp-ts/lib/HKT" {
  interface URItoKind<A> {
    readonly [URI]: Distributions<A>;
  }
}

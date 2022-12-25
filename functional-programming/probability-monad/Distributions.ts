import "fp-ts/lib/HKT";

type Probability = number;

type Event<CASE> = (a: CASE) => boolean;

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

  static uniform<CASE>(cases: CASE[]): Distributions<CASE> {
    const prob = 1 / cases.length;

    return new Distributions(cases.map((c) => [c, prob]));
  }

  get value(): [CASE, Probability][] {
    return this.#value;
  }

  evaluate(event: Event<CASE>): Probability {
    return this.#value.reduce((acc, [c, prob]) => {
      return event(c) ? acc + prob : acc;
    }, 0);
  }
}

export const URI = "Distributions";

export type URI = typeof URI;

declare module "fp-ts/lib/HKT" {
  interface URItoKind<A> {
    readonly [URI]: Distributions<A>;
  }
}

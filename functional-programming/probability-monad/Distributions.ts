import "fp-ts/lib/HKT";

type Probability = number;

type Event<CASE> = (a: CASE) => boolean;

export class Distributions<CASE> {
  readonly #value: [CASE, Probability][];

  private constructor(value: [CASE, Probability][]) {
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

  static pure<CASE>(c: CASE): Distributions<CASE> {
    return new Distributions([[c, 1]]);
  }

  get value(): [CASE, Probability][] {
    return this.#value.map(([c, prob]) => [c, prob]);
  }

  evaluate(event: Event<CASE>): Probability {
    return this.#value.reduce(
      (acc, [c, prob]) => (event(c) ? acc + prob : acc),
      0
    );
  }
}

export const URI = "Distributions";

export type URI = typeof URI;

declare module "fp-ts/lib/HKT" {
  interface URItoKind<A> {
    readonly [URI]: Distributions<A>;
  }
}

export const map =
  <A, B>(f: (a: A) => B) =>
  (fa: Distributions<A>): Distributions<B> =>
    Distributions.of(fa.value.map(([c, prob]) => [f(c), prob]));

export const ap =
  <A, B>(fa: Distributions<A>) =>
  (fab: Distributions<(a: A) => B>): Distributions<B> =>
    Distributions.of(
      fab.value.flatMap(
        ([f, prob1]) =>
          fa.value.map(([b, prob2]) => [f(b), prob1 * prob2]) as [
            B,
            Probability
          ][]
      )
    );

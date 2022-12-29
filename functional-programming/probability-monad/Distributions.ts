import "fp-ts/lib/HKT";
import { pipe } from "fp-ts/function";

type Probability = number;

type Event<RANDOM_VARIABLE> = (a: RANDOM_VARIABLE) => boolean;

export class Distributions<RANDOM_VARIABLE> {
  readonly #value: [RANDOM_VARIABLE, Probability][];

  private constructor(value: [RANDOM_VARIABLE, Probability][]) {
    this.#value = value;
  }

  static of<RANDOM_VARIABLE>(
    value: [RANDOM_VARIABLE, Probability][]
  ): Distributions<RANDOM_VARIABLE> {
    const grouped = value.reduce((acc, [variable, prob]) => {
      acc.set(variable, (acc.get(variable) ?? 0) + prob);

      return acc;
    }, new Map<RANDOM_VARIABLE, Probability>());

    const sum = Array.from(grouped.values()).reduce((acc, prob) => acc + prob);
    const normalized = Array.from(grouped.entries()).map(
      ([variable, prob]) =>
        [variable, prob / sum] as [RANDOM_VARIABLE, Probability]
    );

    return new Distributions(normalized);
  }

  static uniform<RANDOM_VARIABLE>(
    variables: RANDOM_VARIABLE[]
  ): Distributions<RANDOM_VARIABLE> {
    const prob = 1 / variables.length;

    return new Distributions(variables.map((variable) => [variable, prob]));
  }

  static pure<RANDOM_VARIABLE>(
    variable: RANDOM_VARIABLE
  ): Distributions<RANDOM_VARIABLE> {
    return new Distributions([[variable, 1]]);
  }

  get value(): [RANDOM_VARIABLE, Probability][] {
    return this.#value.map(([variable, prob]) => [variable, prob]);
  }

  evaluate(event: Event<RANDOM_VARIABLE>): Probability {
    return this.#value.reduce(
      (acc, [variable, prob]) => (event(variable) ? acc + prob : acc),
      0
    );
  }

  conditional(event: Event<RANDOM_VARIABLE>): Distributions<RANDOM_VARIABLE> {
    return Distributions.of(
      this.#value.filter(([variable, _]) => event(variable))
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

export const evaluate =
  <RANDOM_VARIABLE>(event: Event<RANDOM_VARIABLE>) =>
  (dist: Distributions<RANDOM_VARIABLE>): Probability =>
    dist.evaluate(event);

export const conditional =
  <RANDOM_VARIABLE>(event: Event<RANDOM_VARIABLE>) =>
  (dist: Distributions<RANDOM_VARIABLE>): Distributions<RANDOM_VARIABLE> =>
    dist.conditional(event);

export const map =
  <A, B>(f: (a: A) => B) =>
  (fa: Distributions<A>): Distributions<B> =>
    Distributions.of(fa.value.map(([variable, prob]) => [f(variable), prob]));

export const ap =
  <A, B>(fa: Distributions<A>) =>
  (fab: Distributions<(a: A) => B>): Distributions<B> =>
    Distributions.of(
      fab.value.flatMap(
        ([f, prob1]) =>
          fa.value.map(([a, prob2]) => [f(a), prob1 * prob2]) as [
            B,
            Probability
          ][]
      )
    );

export const liftA2 =
  <A, B, C>(f: (a: A, b: B) => C) =>
  (fa: Distributions<A>, fb: Distributions<B>): Distributions<C> =>
    pipe(
      fa,
      map((a) => (b: B) => f(a, b)),
      ap(fb)
    );

export const chain =
  <A, B>(f: (a: A) => Distributions<B>) =>
  (fa: Distributions<A>): Distributions<B> =>
    Distributions.of(
      fa.value.flatMap(
        ([variable, prob1]) =>
          f(variable).value.map(([b, prob2]) => [b, prob1 * prob2]) as [
            B,
            Probability
          ][]
      )
    );

import {filter, map, reduce} from 'fp-ts/Array'
import {flow, pipe} from "fp-ts/function";
import {log} from "fp-ts/Console";
import {
  chain,
  Either,
  left,
  mapLeft,
  right,
  map as eitherMap,
  getApplicativeValidation
} from "fp-ts/Either";
import {NonEmptyArray, getSemigroup} from "fp-ts/NonEmptyArray";
import {sequenceT} from "fp-ts/Apply";

pipe(
  [1, 2, 3, 4, 5],
  map(value => value * 3),
  filter(value => value % 2 == 0),
  reduce(0, (a, b) => a + b),
  log
)()

const checkLength = (str: string) => str.length < 6 ? left('at least 6 characters') : right(str)
const checkCapital = (str: string) => !/[A-Z]/.test(str) ? left('at least one capital letter') : right(str)
const checkNumber = (str: string) => !/[0-9]/.test(str) ? left('at least one number') : right(str)

const lift = <E, A>(check: (a: A) => Either<E, A>): (a: A) => Either<NonEmptyArray<E>, A> =>
  a =>
    pipe(
      check(a),
      mapLeft(a => [a])
    )

const minLength = lift(checkLength)
const oneCapital = lift(checkCapital)
const oneNumber = lift(checkNumber)

const validator = flow(
  checkLength,
  chain(checkCapital),
  chain(checkNumber),
  log
)

const validatePassword = (s: string): Either<NonEmptyArray<string>, string> =>
  pipe(
    sequenceT(getApplicativeValidation(getSemigroup<string>()))(
      minLength(s),
      oneCapital(s),
      oneNumber(s)
    ),
    eitherMap(() => s)
  )


validator('short')()
validator('no capital')()
validator('No number')()
validator('Password123')()

log(validatePassword('short'))()
log(validatePassword('no capital'))()
log(validatePassword('No number'))()
log(validatePassword('Password123'))()

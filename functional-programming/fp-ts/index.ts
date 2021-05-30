import {filter, map, reduce} from 'fp-ts/Array'
import {flow, pipe} from "fp-ts/function";
import {log} from "fp-ts/Console";
import {chain, left, right,} from "fp-ts/Either";

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

const validator = flow(
  checkLength,
  chain(checkCapital),
  chain(checkNumber),
  log
)

validator('short')()
validator('no capital')()
validator('No number')()
validator('Password123')()

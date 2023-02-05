import '@effect-ts/core/Tracing/Enable'

import * as E from '@effect-ts/core/Either'
import * as T from '@effect-ts/core/Effect'
import * as S from '@effect-ts/core/Effect/Schedule'
import {runMain} from '@effect-ts/node/Runtime'
import {tag} from '@effect-ts/core/Has'
import {pipe} from "@effect-ts/core";

export function pureFunction() {
  function helloWorld(name: string) {
    return () => {
      console.log(`Hello world: ${name}!`)
    }
  }

  const call = helloWorld('Mike')

  call()
}

export function either() {
  function helloWorld(name: string) {
    return (): E.Either<never, void> => {
      console.log(`Hello world: ${name}!`)
      return E.right(undefined)
    }
  }
}

export function io() {
  type IO<A> = () => A

  interface ConsoleService {
    Console: {
      log: (message: string) => IO<E.Either<never, void>>
    }
  }

  function helloWorld(name: string) {
    return ({Console}: ConsoleService) => Console.log(`Hello world: ${name}!`)
  }

  const call = helloWorld('Mike')

  call({
    Console: {
      log: (message: string) => () => {
        console.log(message)
        return E.right(undefined)
      }
    }
  })
}

export function effect() {
  interface ConsoleService {
    log: (message: string) => T.Effect<unknown, never, void>
  }

  const ConsoleService = tag<ConsoleService>()

  const log = (message: string) => T.accessServiceM(ConsoleService)(_ => _.log(message))

  function helloWorld(name: string) {
    return log(`Hello world: ${name}!`)
  }

  const program = pipe(
    helloWorld('Mike'),
    T.chain(() => helloWorld('John'))
  )

  // const program = T.gen(function* (_) {
  //   yield* _(helloWorld('Mike'))
  //   yield* _(helloWorld('John'))
  // })

  pipe(
    program,
    T.provideService(ConsoleService)({
      log: (message: string) => new T.IEffectTotal(() => {
        console.log(message)
      })
    }),
    T.runPromise
  )
}

interface ConsoleService {
  log: (message: string) => T.Effect<unknown, never, void>
}

const ConsoleService = tag<ConsoleService>()
const log = (message: string) => T.accessServiceM(ConsoleService)(_ => _.log(message))

interface RandomService {
  rand: T.Effect<unknown, never, number>
}

const RandomService = tag<RandomService>()
const rand = T.accessServiceM(RandomService)(_ => _.rand)

class BadRandomValue {
  readonly _tag = 'BadRandomValue'

  constructor(readonly value: number) {
  }
}

export const program = T.gen(function* (_) {
  const value = yield* _(rand)

  if (value > 0.5) {
    return yield* _(T.fail(new BadRandomValue(value)))
  }

  yield* _(log(`got: ${value}`))
})

export function main() {
  pipe(
    program,
    T.retry(S.exponential(10)["&&"](S.recurs(10))),
    T.provideService(ConsoleService)({
      log: (message: string) => new T.IEffectTotal(() => {
        console.log(message)
      })
    }),
    T.provideService(RandomService)({
      rand: new T.IEffectTotal(() => Math.random())
    }),
    runMain,
  )
}

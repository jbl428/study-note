import {runtime} from "@effect-ts/jest/Test"
import {BadRandomValue, ConsoleService, program, RandomService} from "./index";
import {pipe} from "@effect-ts/core";
import * as T from "@effect-ts/core/Effect";
import * as L from "@effect-ts/core/Effect/Layer";

describe('program', () => {
  const {it} = runtime()

  it('should print value if it is less then 0.5', () => {
    const messages: string[] = []
    const TestConsole = L.fromEffect(ConsoleService)(
      T.succeed({
        log: (message: string) => T.succeedWith(() => messages.push(message))
      })
    )
    const TestRandom = L.fromEffect(RandomService)(
      T.succeed({rand: T.succeed(0.3)})
    )

    return pipe(
      program,
      T.provideLayer(TestConsole['+++'](TestRandom)),
      T.map(() => expect(messages).toEqual(['got: 0.3']))
    )
  })

  it('should print error if it is more then 0.5', () => {
    const messages: string[] = []
    const TestConsole = L.fromEffect(ConsoleService)(
      T.succeed({
        log: (message: string) => T.succeedWith(() => messages.push(message))
      })
    )
    const TestRandom = L.fromEffect(RandomService)(
      T.succeed({rand: T.succeed(0.6)})
    )

    return pipe(
      program,
      T.provideLayer(TestConsole['+++'](TestRandom)),
      T.catchTag('BadRandomValue', (e) => T.succeed(e)),
      T.map(e => expect(e?.value).toEqual(0.6))
    )
  })
});

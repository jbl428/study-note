import {runtime} from "@effect-ts/jest/Test"
import {BadRandomValue, ConsoleService, program, RandomService} from "./index";
import {Exit, pipe} from "@effect-ts/core";
import * as T from "@effect-ts/core/Effect";
import * as L from "@effect-ts/core/Effect/Layer";

describe('program', () => {
  const {it} = runtime()

  it('should print value if it is less then 0.5', () => {
    const messages: string[] = []
    const TestConsole = L.fromEffect(ConsoleService)(
      T.succeed({
        log: (message: string) => new T.IEffectTotal(() => messages.push(message))
      })
    )
    const TestRandom = L.fromEffect(RandomService)(
      T.succeed({
        rand: new T.IEffectTotal(() => 0.3)
      })
    )

    return pipe(
      program,
      T.provideLayer(TestConsole['+++'](TestRandom)),
      T.map(() => expect(messages).toEqual(['got: 0.3']))
    )
  })

  test('should print error if it is more then 0.5', async () => {
    const messages: string[] = []
    const TestConsole = L.fromEffect(ConsoleService)(
      T.succeed({
        log: (message: string) => new T.IEffectTotal(() => messages.push(message))
      })
    )
    const TestRandom = L.fromEffect(RandomService)(
      T.succeed({
        rand: new T.IEffectTotal(() => 0.6)
      })
    )

    const result = await pipe(
      program,
      T.provideLayer(TestConsole['+++'](TestRandom)),
      T.runPromiseExit
    )

    expect(result).toEqual(Exit.fail(new BadRandomValue(0.6)))
  })
});

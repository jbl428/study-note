import {runtime} from "@effect-ts/jest/Test"
import {ConsoleService, program, RandomService} from "./index";
import {pipe} from "@effect-ts/core";
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
});

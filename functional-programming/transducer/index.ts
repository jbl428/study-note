import * as R from 'ramda'

let autobots = ['Optimus Prime','Bumblebee','Ironhide','Sunstreaker','Ratchet']

let _map = <T>(fn: (param: T) => T) => (transformerFn: ((acc: T[], item: T) => T[])) => {
  return (acc: T[], item: T) => transformerFn(acc, fn(item))
}

let _filter = <T>(predicate: (param: T) => boolean) => (transformerFn: ((acc: T[], item: T) => T[])) => {
    return (acc: T[], item: T) => predicate(item) ? transformerFn(acc, item) : acc
}

let transform = R.compose(
  _filter((x: string) => /r/i.test(x)),
  _map(R.toUpper),
  _map(R.reverse)
)

console.log(
    R.reduce(transform((acc, item) =>  R.append(item, acc)), [], autobots)
);


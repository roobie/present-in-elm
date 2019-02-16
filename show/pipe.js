function pipe(what, initialTransforms = []) {
    return function pipeline(...transforms) {
        if (transforms.length === 0) {
            let result = what
            for (let i = 0; i < initialTransforms.length; ++i) {
                try {
                    result = initialTransforms[i](result)
                } catch (e) {
                    console.error(e)
                }
            }
            return result
        } else {
            return pipe(what, initialTransforms.concat(transforms))
        }
    }
}

// pure (pipe) => cache

// map : (a -> b) -> (Array a | a) -> (Array b | b)
const map = (fn) => (a) => (a instanceof Array) ? a.map(fn) : fn(a)
// filter : (a -> bool) -> Array a -> Array a
const filter = (fn) => (a) => a.filter(fn)
// reduce : (b -> a -> b) -> b -> Array a -> b
const reduce = (fn) => (init) => (a) => a.reduce(fn, init)

const sumFrom = reduce((acc, v) => acc + v)
const sum = sumFrom(0)

{ // TEST
    let counter = 0
    const arr = [1, 2, 3, 4]
    const pipeline1 = pipe(arr, [map(v => v * 2)])
    const pipeline2 = pipeline1(
        map(v => v * 9),
        filter(v => v % 2 === 0),
        sum
    )
    const pipeline3 = pipeline2(map(v => v + 1))
    const pipeline4 = pipeline3(map(v => (Math.pow(v, 3)).toString(2)))

    const results = [
        pipeline1(),
        pipeline2(),
        pipeline3(),
        pipeline4(),
    ]
    console.log(JSON.stringify(results, null, 2))
}

function assertPredicate(v, predicate, m) {
    console.assert(predicate(v), m)
    return v
}
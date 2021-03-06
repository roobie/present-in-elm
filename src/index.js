import {
    Elm
} from './Main.elm'

document.title = 'Exceptions'

const app = Elm.Main.init({
    node: document.querySelector('main')
})

setTimeout(init, 0)

function init() {
    if (app && app.ports) {
        document.body.addEventListener('keyup', (e) => {
            app.ports.globalKeyUp.send(e.keyCode)
        }, {
            passive: true
        })
    }
}
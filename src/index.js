import {Elm} from './Main.elm'

document.title = 'Exceptions'

const app = Elm.Main.init({
    node: document.querySelector('main')
})

// requestAnimationFrame(() => {})

document.body.addEventListener('keyup', (e) => {
    app.ports.globalKeyUp.send(e.keyCode)
}, {passive: true})
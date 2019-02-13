const m = require('mithril')
const {stream} = require('flyd')

const mstream = () => {
    const $ = stream()
    $.map(v => {
        requestAnimationFrame(() => m.redraw())
    })
    return $
}

const mstreamEvent = (elem, eventName) => {
    const $ = mstream()
    elem.addEventListener(eventName, $)
    return $
}

module.exports = {mstream, mstreamEvent}
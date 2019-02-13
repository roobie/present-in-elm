/**
 * 
    <div id="app" class="flex fill justify-center"></div>
 */
const m = require('mithril')
const {stream} = require('flyd')
const {mstream, mstreamEvent} = require('./utilities/mithril-stream')
const tags = require('./utilities/mithril-tags')
const K = require('keycode-js')

const bodyClicks = mstreamEvent(document.body, 'click')
const bodyKeyups = mstreamEvent(document.body, 'keyup')

const {div, h1, h3, h4, hr, section, span} = tags

const TitleSlide = {
    view (vnode) {
        const data = vnode.attrs
        return div(
            {className: 'row fade-in'},
            div(
                {className: 'col justify-center'},
                div(
                    {className: 'row justify-center'},
                    div(
                        {className: 'col'},
                        h1(data.title),
                    ),
                ),
                hr(),
                div(
                    {className: 'row justify-center'},
                    h4(data.subtitle),
                ),
            ),
        )
    }
}

const PointsSlide = {
    view (vnode) {
        const data = vnode.attrs
        return div(
            {className: 'row fade-in'},
            div(
                {className: 'col'},
                h3(data.title),
                hr(),
                div(
                    {className: 'col'},
                    ...data.points.map(p => div(p))
                )
            ),
        )
    }
}

const slides = [
    {renderer: TitleSlide, data: {title: 'Exceptions', subtitle: `Why they're good, when to use them and how.`}},
    {renderer: PointsSlide, data: {title: 'ABC', points: ['asdf', 'asdfasdf']}},
    {renderer: TitleSlide, data: {title: 'asdfasdf', subtitle: `asdfawefaoweifj`}},
]

const appState = {
    slideIndex: 0,
    currentSlide () {
        return slides[this.slideIndex]
    }
}

const actions = {
    prevSlide () {
        if (appState.slideIndex > 0) appState.slideIndex--
    },
    nextSlide () {
        if (appState.slideIndex < (slides.length - 1)) appState.slideIndex++
    }
}

bodyClicks.map((e) => console.log(e))
bodyKeyups.map((e) => {
    if (e.keyCode === K.KEY_RIGHT) actions.nextSlide()
    if (e.keyCode === K.KEY_LEFT) actions.prevSlide()
})

const Root = {
    onupdate (vnode) {},
    view (vnode) {
        const slide = slides[appState.slideIndex]
        return m(slide.renderer, slide.data)
    }
}

document.addEventListener('DOMContentLoaded', () => {
    m.mount(document.getElementById('app'), Root)
})
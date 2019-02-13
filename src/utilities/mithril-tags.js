const m = require('mithril')
const htmlTags = require('html-tags')

const tags = {}
htmlTags.forEach(tag => {
    tags[tag] = m.bind(m, tag)
})

module.exports = tags
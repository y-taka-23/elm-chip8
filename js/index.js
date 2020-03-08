require('../css/html5doctor.css')
require('../css/style.css')
import { Elm } from '../src/Main.elm'

Elm.Main.init({
    node: document.querySelector('main')
})

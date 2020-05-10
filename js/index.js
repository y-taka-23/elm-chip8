require('../css/html5doctor.css')
require('../css/style.css')
import { Elm } from '../src/Main.elm'

const audioCtx = new (window.AudioContext || window.webkitAudioContext)()
const gainNode = audioCtx.createGain()
let oscillator

const app = Elm.Main.init({
    node: document.querySelector('main')
})

app.ports.startWebAudio.subscribe(num => {

    oscillator = audioCtx.createOscillator()
    oscillator.type = "square"
    oscillator.frequency.value = 440

    oscillator.connect(gainNode)
    gainNode.connect(audioCtx.destination)

    gainNode.gain.value = 0
    oscillator.start()
})

app.ports.stopWebAudio.subscribe(num => {
    oscillator.stop()
})

app.ports.sendTimerToWebAudio.subscribe(num => {
    if (num) {
        gainNode.gain.value = 0.1
    } else {
        gainNode.gain.value = 0
    }
})

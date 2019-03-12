import { Elm } from './Main'
import Clipboard from 'clipboard'

const app = Elm.Main.init({
  flags: null,
  node: document.getElementById('app'),
})

const clipboard = new Clipboard('#clipboard-copy')
clipboard.on('success', ({ text }) => {
  app.ports.confirmCopy.send([true, text])
})
clipboard.on('error', ({ text }) => {
  app.ports.confirmCopy.send([false, text])
})

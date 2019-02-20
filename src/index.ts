import { Elm } from './Main'
import Clipboard from 'clipboard'

const app = Elm.Main.init({
  flags: null,
  node: document.getElementById('app'),
})

const clipboard = new Clipboard('#gradient')

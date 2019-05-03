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

interface Palette {
  colors: string[]
  widths: number[]
}

const updateFavicon = ({ colors, widths }: Palette) => {
  const canvas = document.createElement('canvas')
  canvas.width = 32
  canvas.height = 32
  const ctx = canvas.getContext('2d')
  if (ctx) {
    let x = 0
    colors.forEach((color: string, i: number) => {
      ctx.fillStyle = `#${color}`
      ctx.fillRect(x, 0, 32, 32)
      x += Math.round(widths[i] * 32)
    })

    const link = document.querySelector(
      'head > link[rel="icon"]'
    ) as HTMLLinkElement
    if (link) {
      link.href = canvas.toDataURL('image/x-icon')
    }
  }
}

app.ports.updateFavicon.subscribe(updateFavicon)

// Register service-workder
if ('serviceWorker' in navigator) {
  window.addEventListener('load', async () => {
    try {
      const registration = await navigator.serviceWorker.register('/sw.js')
      console.log(
        'ServiceWorker registration successful with scope: ',
        registration.scope
      )
    } catch (err) {
      console.log('ServiceWorker registration failed: ', err)
    }
  })
}

import fs from 'fs'
import path from 'path'
import { createCanvas } from 'canvas'

const palettes = JSON.parse(
  fs.readFileSync(path.join(__dirname, '../public/palettes.json'), 'utf-8')
)

const { colors, colorWidths } = palettes[
  Math.round(palettes.length * Math.random())
]

const canvas = createCanvas(32, 32)
const ctx = canvas.getContext('2d')

let x = 0
colors.forEach((color: string, i: number) => {
  ctx.fillStyle = `#${color}`
  ctx.fillRect(x, 0, 32, 32)
  x += Math.round(colorWidths[i] * 32)
})

fs.writeFileSync(
  path.join(__dirname, '../public/favicon.png'),
  canvas.toBuffer()
)

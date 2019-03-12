#!/usr/bin/env node
import fs from 'fs'
import path from 'path'
import axios from 'axios'

// Only interested in colors and colorWidths fields for now
interface Palette {
  colors: string[]
  colorWidths: string[]
}

interface ServerData {
  id: number
  title: string
  colors: string[]
  colorWidths: string[]
  url: string
}

const TOTAL = Number(process.argv[2]) || 1000
const PAGE_SIZE = TOTAL < 100 ? TOTAL : 100
// const pages = Math.floor(TOTAL / PAGE_SIZE)
// const rest = TOTAL % PAGE_SIZE

const getData = async (offset = 0): Promise<any[]> => {
  if (offset >= TOTAL) {
    return []
  }

  const { data } = await axios.get(
    'http://www.colourlovers.com/api/palettes/top',
    {
      params: {
        format: 'json',
        showPaletteWidths: 1,
        numResults: PAGE_SIZE,
        resultOffset: offset,
      },
    }
  )

  return data
    .filter(({ colors }: ServerData) => colors.length > 1 && colors.length <= 5)
    .map(({ colors, colorWidths }: ServerData) => ({ colors, colorWidths }))
    .concat(await getData(offset + PAGE_SIZE))
}

const main = async () => {
  try {
    const palettes: Palette[] = await getData()
    const file = path.resolve(__dirname, '../data/palettes.json')
    await fs.writeFileSync(file, JSON.stringify(palettes), 'utf8')
    console.log(`Saved ${palettes.length} color palettes in ${file}`)
  } catch (err) {
    console.error(err)
  }
}

main()

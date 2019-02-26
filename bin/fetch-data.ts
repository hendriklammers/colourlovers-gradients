#!/usr/bin/env node
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

const TOTAL = 100
const PAGE_SIZE = 10

const dataToPalette = (data: ServerData[]): Palette[] => {
  return data.map(({ colors, colorWidths }) => ({
    colors,
    colorWidths,
  }))
}

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
    .map(({ colors, colorWidths }: ServerData) => ({ colors, colorWidths }))
    .concat(await getData(offset + PAGE_SIZE))
}

const main = async () => {
  try {
    const result = await getData()
    console.log(result.length)
  } catch (err) {
    console.error(err)
  }
}

main()

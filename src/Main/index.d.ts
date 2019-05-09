// WARNING: Do not manually modify this file. It was generated using:
// https://github.com/dillonkearns/elm-typescript-interop
// Type definitions for Elm ports

export namespace Elm {
  namespace Main {
    export interface App {
      ports: {
        confirmCopy: {
          send(data: [boolean, string]): void
        }
        updateFavicon: {
          subscribe(callback: (data: { colors: string[]; widths: number[] }) => void): void
        }
      };
    }
    export function init(options: {
      node?: HTMLElement | null;
      flags: { touch: boolean; online: boolean };
    }): Elm.Main.App;
  }
}
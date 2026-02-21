# Scoundrel

A solo dungeon-crawler card game built with ClojureScript, Reagent, and Tailwind CSS. Draw rooms, fight monsters, equip weapons, and drink potions to survive the dungeon. Installable as a Progressive Web App (PWA).

## Rules

Scoundrel uses a modified 44-card deck (standard 52 minus the 6 red face cards and 2 red aces). Cards map to three types:

| Suit             | Type      | Effect                                                         |
| ---------------- | --------- | -------------------------------------------------------------- |
| ♥ Hearts         | Potion    | Heal HP equal to the card's rank (capped at 20)                |
| ♦ Diamonds       | Weapon    | Equip as your weapon (replaces any existing weapon)            |
| ♣ Clubs / ♠ Spades | Monster | Deal damage equal to rank, reduced by equipped weapon value |

Each turn a **room** of 4 cards is drawn. You must resolve at least 2 cards before you can end the room. Any remaining card carries over to the next room.

- **Weapons** can only be used against a monster whose value is strictly less than the last monster you used the weapon on (resets when you equip a new weapon).
- **Escape** — you may flee a room without resolving any cards, but you cannot escape two rooms in a row.
- **Win** — clear all cards from the dungeon.
- **Lose** — your HP drops to 0 or below.

## Tech Stack

- **ClojureScript** — game logic ([src/game/core.cljs](src/game/core.cljs)) and UI ([src/game/ui.cljs](src/game/ui.cljs))
- **Reagent** (React wrapper) — reactive UI components
- **Shadow-CLJS** — ClojureScript build tool
- **Tailwind CSS v4** — utility-first styling
- **PWA** — service worker + web manifest for offline/installable support

## Prerequisites

- **Java** (JDK 11+)
- **Node.js** (v18+)

## Getting Started

Install dependencies:

```sh
npm install
```

Start the development server (Tailwind CSS watch + Shadow-CLJS hot-reload):

```sh
npm run dev
```

The app will be available at [http://localhost:3000](http://localhost:3000).

## Scripts

| Command           | Description                                      |
| ----------------- | ------------------------------------------------ |
| `npm run dev`     | Start Tailwind watcher + Shadow-CLJS dev server  |
| `npm run css`     | Watch and rebuild Tailwind CSS only               |
| `npm run css:build` | One-shot minified Tailwind CSS build            |
| `npm test`        | Compile and run ClojureScript tests (Node)        |

## Project Structure

```
src/
  game/
    core.cljs        # Pure game logic (deck, dealing, combat, state machine)
    ui.cljs          # Reagent UI components
  styles/
    main.css         # Tailwind CSS entry point
test/
  game/
    core_test.cljs   # Game logic tests
public/
  index.html         # App shell
  manifest.json      # PWA manifest
  sw.js              # Service worker
```

## Testing

```sh
npm test
```

Tests are compiled with Shadow-CLJS targeting Node and executed with `node tests/tests.js`.

## License

This project is unlicensed / private.

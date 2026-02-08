# Paris Metro Route Finder

<p align="center">
  <img src="https://img.shields.io/badge/Ada-2012-blue.svg" alt="Ada 2012">
  <img src="https://img.shields.io/badge/license-MIT-green.svg" alt="MIT License">
</p>


An Ada program that finds the shortest route between two metro stations in the Paris metro network.

## Features

- **Shortest Path Finding**: Finds shortest route between any two stations (by number of stations)
- **Enhanced Metrics**: Shows distance (km) and travel time for routes using real GTFS data
- **Transfer Information**: Shows which lines to take and where to transfer
- **Station Autocomplete**: Smart prefix matching with '?' to help find station names
- **Comprehensive Coverage**: Supports all Paris metro lines (M1-M14, including M3b and M7b)
- **Multiple Modes**: Command-line and interactive modes
- **Test Suite**: Comprehensive automated testing

## Quick Start

### Interactive Mode (with Autocomplete)
```bash
./bin/metro_main

# Use '?' for autocomplete suggestions:
Enter start station: bast?
Auto-completing to: Bastille

Enter destination station: oper?
Auto-completing to: Opéra
```

### Command-Line Mode
```bash
./bin/metro_main "Bastille" "Opéra"
```

See `AUTOCOMPLETE.md` for detailed autocomplete usage and `ENHANCED_FEATURES.md` for information about distance/time calculations.

## Building

Requires GNAT Ada compiler (gnatmake):

```bash
make          # Build main program (also copies data to bin/)
make test     # Build and run tests
make clean    # Clean build artifacts
make help     # Show all targets
```

## Usage

The program must be run from the `bin/` directory (where data is located):

### Command-line mode:
```bash
cd bin
./metro_main "Concorde" "Bastille"
```

### Interactive mode:
```bash
cd bin
./metro_main
```

Then enter the start and destination stations when prompted.

## Examples

### Enhanced Route Display (with Distance and Time)
```bash
$ ./bin/metro_main "Bastille" "Opéra"

Route from Bastille to Opéra:
========================================
  >> Transfer to line M14 at Bastille
  Take line M14 from Bastille to Châtelet (2.46 km,  3 min)
  >> Transfer to line M7 at Châtelet
  Take line M7 from Châtelet to Opéra ( 466 m,  2 min)

Total stations: 5
Transfers: 2
Total distance: 2.93 km
Estimated time:  9 min
```

### Autocomplete Feature
```bash
$ ./bin/metro_main

Enter start station: char?
Found 2 matching stations:
  - Charles de Gaulle-Etoile
  - Charles Michels
Please be more specific or select one from above.

Enter start station: charles de?
Auto-completing to: Charles de Gaulle-Etoile
```

## Tests

Run the test suite:

```bash
make test
```

The test suite validates:
- Same-line routes (no transfers)
- Routes with one transfer
- Routes with multiple transfers
- Edge cases (same start/end station)

## Station Names

- Station names must be entered exactly as they appear in the data files
- Names are case-sensitive
- Some stations use hyphens (e.g., "Montparnasse-Bienvenue")
- Accented characters must be entered correctly (e.g., "République")

## Data Files

The program reads metro line data from `data/` directory:
- Each file (M1.txt, M2.txt, etc.) contains an ordered list of stations for that line
- One station name per line
- UTF-8 encoding
- Unix line endings (LF)

## Algorithm

The program uses:
- **Graph representation**: Each station is a node, connections between consecutive stations on lines are edges
- **BFS (Breadth-First Search)**: Finds shortest path by number of stations
- **Transfer detection**: Stations appearing on multiple lines are identified as transfer points


## Implementation Notes

- Built with Ada 2012

- Uses `Ada.Containers` for dynamic data structures
- Supports UTF-8 station names with French characters
- Bidirectional metro lines (can travel in both directions)
- This is an old project so the metro lines may not be up to date in 2026. 

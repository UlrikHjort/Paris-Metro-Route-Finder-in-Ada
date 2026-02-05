# Station Name Autocomplete Feature

## Overview

The Paris Metro Route Finder includes an autocomplete feature that helps you find station names without knowing the exact spelling or full name.

## How to Use

### Interactive Mode

When running without command-line arguments, the program enters interactive mode:

```bash
./bin/metro_main
```

### Autocomplete Syntax

**Add `?` after a partial station name to see suggestions:**

```
Enter start station: bast?
```

### Behavior

1. **Unique Match (Auto-complete)**
   - If only one station matches your prefix, it auto-completes:
   ```
   Enter start station: bastil?
   Auto-completing to: Bastille
   ```

2. **Multiple Matches (Show Suggestions)**
   - If multiple stations match, it shows all options:
   ```
   Enter start station: char?
   Found 2 matching stations:
     - Charles de Gaulle-Etoile
     - Charles Michels
   Please be more specific or select one from above.
   ```

3. **No Matches**
   - If no stations match, you get a helpful message:
   ```
   Enter start station: xyz?
   No stations found matching 'xyz'
   ```

4. **Direct Entry**
   - You can still type the complete name directly:
   ```
   Enter start station: Bastille
   ```

## Examples

### Example 1: Finding a Station by Prefix

```
Enter start station: gare?
Found 6 matching stations:
  - Gare de Lyon
  - Gare du Nord
  - Gare d'Austerlitz
  - Gare de l'Est
  - Gare Montparnasse
  - Gare Saint-Lazare
Please be more specific or select one from above.

Enter start station: gare d?
Found 3 matching stations:
  - Gare de Lyon
  - Gare du Nord
  - Gare d'Austerlitz
Please be more specific or select one from above.

Enter start station: gare de l?
Auto-completing to: Gare de Lyon
```

### Example 2: Quick Auto-complete

```
Enter start station: nation?
Auto-completing to: Nation

Enter destination station: repu?
Auto-completing to: République
```

### Example 3: Handling Ambiguity

```
Enter start station: porte?
Found 28 matching stations:
  - Porte de Bagnolet
  - Porte de Champerret
  - Porte de Choisy
  - Porte de Clignancourt
  ... (and 24 more)
Please be more specific or select one from above.

Enter start station: porte de c?
Found 3 matching stations:
  - Porte de Champerret
  - Porte de Choisy
  - Porte de Clignancourt
Please be more specific or select one from above.

Enter start station: porte de cho?
Auto-completing to: Porte de Choisy
```

## Features

### Case-Insensitive Matching
- Searches are case-insensitive
- "bast" matches "Bastille"
- "GARE" matches "Gare de Lyon"

### Prefix Matching
- Only matches stations that **start with** your input
- "char" matches "Charles de Gaulle-Etoile" ✓
- "char" does NOT match "Réaumur-Sébastopol" ✗

### Smart Suggestions
- Shows all matching stations when ambiguous
- Auto-completes when only one match
- Provides helpful error messages

### Fallback to Exact Entry
- Can always enter the full, exact station name
- No need to use autocomplete if you know the name

## Technical Details

### Implementation
- Function: `Find_Matching_Stations` in `metro_network.adb`
- Algorithm: Prefix matching with normalized strings (lowercase)
- Data Structure: Returns a Vector of matching Station_Names


## Tips for Users

1. **Start with a few letters**
   - `bas?` is faster to type than `bastille?`

2. **Use unique prefixes**
   - `chatele?` uniquely identifies "Châtelet"
   - `chat?` might show multiple "Château" stations

3. **Progressive refinement**
   - If you get too many matches, add more letters
   - `por? → porte? → porte d? → porte de c?`

4. **Works with accents**
   - Input: `chateau?`
   - Matches: `Château de Vincennes`
   - Case and accent insensitive

## Command-Line Mode

The autocomplete feature is only available in interactive mode. In command-line mode, station names must be exact:

```bash
# Command-line (no autocomplete)
./bin/metro_main "Bastille" "Opéra"

# Interactive (with autocomplete)
./bin/metro_main
```


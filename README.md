![Release](https://github.com/pnotequalnp/curve/workflows/Release/badge.svg)
![Nix](https://github.com/pnotequalnp/curve/workflows/Nix/badge.svg)
![Cabal](https://github.com/pnotequalnp/curve/workflows/Cabal/badge.svg)

# Curve
Curve is a rudimentary Discord bot written in Haskell. Its current primary feature is greeting users after they react to a specific message with a specific reaction, to complement role-assignment via reactions.

## Installation
Binaries are available on the [releases](https://github.com/pnotequalnp/curve/releases) page.

### Nix (Recommended)
```bash
nix build github:pnotequalnp/curve/v0.1.0.8
```

### Docker
```bash
docker pull pnotequalnp/curve:0.1.0.8
```

### Cabal
Requires ghc 8.10.3, cabal-install 3.2

```bash
git clone https://github.com/pnotequalnp/curve.git
cd curve
cabal install
```

## Usage
Curve requires no CLI options to run.

### Environment variables
Curve expects a `DISCORD_TOKEN` environment variable with your bot account's Discord token. `CURVE_CONFIG` is an optional variable which contains the path of the intended configuration file. If empty, `curve.yml` in the working directory will be used.
Additionally, Curve expects the following self-explanatory Postgres-related environment variables:
```
POSTGRES_HOST
POSTGRES_USER
POSTGRES_PASSWORD
POSTGRES_PORT
POSTGRES_DB
```

### Flags
Curve currently suports the `--version` flag as well as the `--fillGuild` flag. The `--fillGuild` flag takes a guild ID as an argument and populates the database with all current users of that guild.

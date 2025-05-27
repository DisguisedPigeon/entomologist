# Entomologist

[![Package Version](https://img.shields.io/hexpm/v/entomologist)](https://hex.pm/packages/entomologist)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/entomologist/)

## Roadmap
- [x] [Basic exception handling and storage](https://github.com/DisguisedPigeon/entomologist/issues/1)
- [ ] [Minimal Frontend](https://github.com/DisguisedPigeon/entomologist/issues/2)
- [ ] [Exception tagging on the backend](https://github.com/DisguisedPigeon/entomologist/issues/3)
- [ ] [Exception tagging on the frontend](https://github.com/DisguisedPigeon/entomologist/issues/4)
- [ ] [DB exporting and imporing](https://github.com/DisguisedPigeon/entomologist/issues/5)
- [ ] [Backend filtering and handling](https://github.com/DisguisedPigeon/entomologist/issues/7)
- [ ] [Frontend filtering and handling](https://github.com/DisguisedPigeon/entomologist/issues/6)
- [ ] [UPDATE README AS THINGS CHANGE](https://github.com/DisguisedPigeon/entomologist/issues/8) - this is important

## Usage

> [!TODO]
> Update as development progresses. This is more a wishlist than an actual readme for now.

```sh
gleam add entomologist
```

> [!IMPORTANT]
> Before using this library, you have to add this to your sql migrations.
> If you don't have any migration system, dbmate is a good option.

```sql
create type level as enum (
    'emergency',
    'alert',
    'critical',
    'error','Warning',
    'notice',
    'info',
    'debug'
);

create table if not exists errors (
    id bigserial not null unique primary key,
    message text not null,
    level level not null,
    module text not null,
    function text not null,
    arity int not null,
    file text not null,
    line int not null,
    resolved bool not null default false,
    last_occurrence bigint not null,
    snoozed bool not null default false
);

create table if not exists occurrences (
    id bigserial not null unique primary key,
    error bigint references errors(id) on delete cascade,
    timestamp bigint not null,
    full_contents json
    -- breadcrumbs
    --   This comes from elixir's Error Tracker. It's an infinite list of texts to help track the error. Might add it later.
);
```

### Logging module
> [!WARNING]
> This is still unimplemented or subject to change.
> For accurate documentation, follow the link at the bottom of the usage section.
> It updates automagically with releases.

```gleam
import entomologist/logging

pub fn main() {
  //... db setup with pog
  let connection = pog.connect(config)

  logging.log(logging.Debug, "I just did stuff here", connection)
  logging.log_and_crash("This is bad", connection)
}
```

### Wisp integration
> [!WARNING]
> This is still unimplemented or subject to change.
> For accurate documentation, follow the link at the bottom of the usage section.
> It updates automagically with releases.

```gleam
import entomologist/wisp as wisp_integration

pub fn response_handler(connection: pog.Connection...) {
    // use ...(whatever handlers you are calling)

    // this is a replacement for wisp/rescue_crashes. It logs them to DB.
    use <- wisp_integration.rescue_and_inspect_crashes(request, connection)
}
```


Further documentation can be found at <https://hexdocs.pm/entomologist>.

## Development

```sh
gleam test  # Run the tests
```

If you are confused about the emojis on the commits, check out <https://gitmoji.dev>

### For nix users

This repository counts with a `flake.nix`

## Namesake

From wikipedia:
> Entomology \[...\] is the branch of zoology that focuses on insects. Those who study entomology are known as entomologists

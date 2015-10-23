# QuickLift

QuickLift is a web app for logging weightlifting sessions.

## Installation

- Download and install [stack](https://github.com/commercialhaskell/stack)
- Run `stack build` to build the project
- Run `stack test` to run the tests

### Database

- Ensure that PostgreSQL is installed.
- Create a database `quicklift` with `username:password` of `test:test`

### Front end

There are three main ways to use the front-end:

1. `pulp server` for fast refresh that doesn't rely on the back end.
2. `./dev` script which watches the source and rebuilds on changes.
3. `./deploy` script which installs all dependencies and does an optimized build.

## Features on the way

* Implement authentication

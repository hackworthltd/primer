# Add a new fixture
## Replaying edits only
Run a local instance of primer and capture the log (in a verbose format intended for capturing replay fixtures):
`cabal run exe:primer-service -- serve v1 --record-replay > log`.
Interact with this session to generate the activity you wish to capture.
NB: you *must* start from a `Primer.API.newSession` (i.e. one corresponding to a `Primer.App.newApp`), since the replay starts from this state.
Extract information from the log by running
`cabal run -v0 primer-benchmark-mkfixture <log >fixtures/newFixture.edits`
to create a new fixture.
Add a line like `runFixture "newFixture"` to `src/Main.hs`.

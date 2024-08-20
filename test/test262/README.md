# Test262

## Running with Locally

To run the Test262 tests locally simply, run:

```sh
dune exec -- ecma-sl test ./tests
```

## Running with GitHub

1. On GitHub.com, navigate to the main page of the repository.
2. Click Actions.
3. In the left sidebar, click the 'Run Test262 Benchmarks'.
4. Above the list of workflow runs, click the **Run workflow** button.
5. Select the **Branch** dropdown menu and click a branch to run the workflow on.
6. Click **Run workflow**.

## Running with GitHub CLI

To launch the Test262 workflow on GitHub, through the command line use:

```sh
gh workflow run test262.yml
```

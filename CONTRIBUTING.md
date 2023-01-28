# Contributing to ECMA-SL

We appreciate contributions to ECMA-SL, but please be aware of the following guidelines:

- When working on an ECMA-SL interpreter a [test-driven methodology](https://en.wikipedia.org/wiki/Test-driven_development) methodology is **highly** encouraged. Refer to [#154](https://github.com/j3fsantos/ECMA-SL/pull/154) for unit-testing creation in ocaml.

- Always **create a new branch to start developing**, and make sure that the base branch is the most recent version of `master`.
  - To create a branch, go to `master` and execute the following commands:
    ```bash
    git pull origin master # to make sure your local master is updated with the one on github
    git checkout -b "name-of-your-branch" # to create a new branch
    ```

- When you are **done developing**
  - Make sure your branch is still up-to-date with the master branch.
    - To do that, execute on your branch
      ```bash
      git fetch origin # to make you have the latests updates of github branchs
      git merge origin/master # merge the current master branch to your development branch
      ```

  - Run all Test262 tests on your branch.
    - To do that you can run:
    ```bash
      ./exec_test262_tests.sh -6 -r test/test262/tests
    ```

  - [**Alternatively**] If you have a mac, you can also run the script with a timeout:
    ```bash
    ./exec_test262_with_timeout.sh -6 -r test/test262/tests
    ```

  - If the **CurrentResults.md** file exists in the project, you need to compare your test results to the previous ones.
    - To run the comparisson do:
    ```bash
    node compare_test_results CurrentResults.md <YOUR_NEW_RESULTS.md>
    ```
    - These results can be found in the `implementation/logs` folder.


  - If the results did not make the test coverage worst, then you can create a new pull request.
    - Make sure your pull request has a **proper description and also a breakdown of the new passing tests**.


- Once your pull request is approved by some other member, you can merge it into master.

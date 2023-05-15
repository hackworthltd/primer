# Contributing to the Primer project

Thanks for your interest in contributing to the Primer project!

We welcome any and all forms of contribution, so long as contributors adhere to the project's rules and guidelines, as prescribed in this document.

## <a name="code-of-conduct"/> Our code of conduct

First and foremost, please review & agree to abide by our [code of conduct](CODE_OF_CONDUCT.md). We reserve the right to reject contributions from anyone whose behavior contravenes this code of conduct.

## <a name="two-projects"/> The two different Primer projects

Our implementation of Primer is unusual for a programming language, because it's not *just* a programming language: it's implemented as multiple artifacts, all designed to work together to form a holistic programming environment.

This particular project implements what we call the Primer *backend*, which is comprised of the core programming language, several database engine adapters for storing and retrieving Primer programs, and 2 different HTTP APIs for serving Primer client applications, which we informally call *frontends*. 

In addition to this backend project, we've also released a Primer client project, [`primer-app`](https://github.com/hackworthltd/primer-app), which implements a browser-based frontend for reading, writing, running, and debugging Primer programs. For various reasons, these 2 projects are maintained in separate GitHub repositories, so make sure you submit your contribution to the appropriate one.

As a general rule, if you want to contribute to the Haskell implementation of Primer, or make a contribution related to the Primer programming language specification, submit it to this project. If you want to contribute to the web-based programming environment, submit it to the [`primer-app`](https://github.com/hackworthltd/primer-app) project.

Note that in many cases, changes to one project will require changes to the other, but usually we lead with changes to the backend project before moving on to the frontend work.

## <a name="questions"/> General questions or comments

If you have a question or comment about this project, the Primer programming language, or if you just need help getting started, please start a discussion in the project's [GitHub Discussions](https://github.com/hackworthltd/primer/discussions), rather than opening a GitHub issue. Please reserve GitHub issues for reporting specific bugs, or for making detailed feature requests.

## <a name="security-issue"/> Reporting security issues

If you believe you've found a security issue in our code, or in a public deployment of the Primer service, then in the interest of [responsible disclosure](https://en.wikipedia.org/wiki/Coordinated_vulnerability_disclosure), please **do not** report it via the GitHub issue tracker. Instead, see our [security policy](https://github.com/hackworthltd/primer/security/policy), and follow the instructions given there.

**Important note**: currently, this project does not implement any authentication or authorization features, and therefore you should *not* host it on a publicly available service. There's no need to report this issue to us, as we're well aware of this limitation. We'll provide a solution(s) as we get closer to a 1.0 release of the project.

## <a name="submission-guidelines"/> Submission guidelines

### <a name="open-an-issue"/> Opening a GitHub Issue

Before you open a GitHub Issue on this project, please first search the project's issue tracker. An issue describing an identical or similar problem or feature to yours may already exist. If so, feel free to add your own comment to the existing issue if you have additional information to add (e.g., for a bug, new information about the root cause), but please don't add to the noise level of the issue tracker by adding simple comments like "Me too," as these contributions aren't helpful. Instead, add the 👍 emoji to the original post in order to bring more attention to it: we use the 👍 count as a rough idea of how many people are affected by a given issue, or are interested in a new feature.

#### <a name="reporting-bugs"/> Reporting bugs

If you've found a non-security-related bug in the project's source code, and no existing issue plausibly covers the bug you've found, please [open a new issue](https://github.com/hackworthltd/primer/issues/new/choose) using the "Bug report" button, and fill out the form in as much detail as you can. We appreciate that it may take some time & effort to provide the requested details, but the more information we have about the bug, the easier it'll be for us to find it and fix it.

The bug report form includes a section where you can provide an optional *bug reproducer*. Normally, a reproducer takes the form of a link to a small project, example, or test case which triggers the issue. Reproducers are not required for bug reports, and are sometimes not applicable, but when provided, they're extremely helpful, and it's likely that bugs with reproducers will be fixed more quickly than those without.

#### <a name="requesting-features"/> Feature requests

If you have a new feature in mind, [open a new issue](https://github.com/hackworthltd/primer/issues/new/choose) and use the "Feature request" button to explain how you imagine the feature might work, providing as much detail as you can. Please note that we cannot provide any definite timelines for when new feature requests might be implemented, and not all feature requests will be accepted by this project, in any case.

If you'd like to *implement* a new feature yourself, then so as not to waste your time or ours, please first consider the scope of the changes required to implement it:

* If the new feature is minor (say, less than a few hours of work to implement), and especially if there's an existing GitHub Issue that explains how the feature should work, then feel free to [submit a GitHub pull request](#submit-a-pr) directly.

* If the new feature is a major undertaking, and especially if there's not already an issue in the project's GitHub Issues tracker that describes the new feature you plan to implement, please first [open a GitHub issue](#open-an-issue) using the "Feature request specification" button, and fill it out as comprehensively as you can. This will give the project's maintainers a chance to review the specification, ask for clarifications, request changes, and decide whether the feature would likely be accepted by the project, before you spend any time implementing it. It may also be the case that someone else is currently implementing the same/similar functionality, in which case you may instead be invited to participate in the in-progress work, rather than writing your own implementation.

As this particular project is implemented in Haskell, only Haskell code submissions will be accepted for feature request implementations, with the exception of build- and database-related tooling changes and improvements. Before writing any Haskell code for this project, please review our [Haskell style guide](docs/haskell-style-guide.md).

### <a name="submit-a-pr"/> Submitting a pull request (PR)

Before you submit a PR to our project, please review the following checklist:

1. Search the project's existing [GitHub PR's](https://github.com/hackworthltd/primer/pulls) for an open, closed, or merged PR that's related to your planned contribution, in case yours duplicates or overlaps with previously submitted contributions.

2. Consider [opening a GitHub Issue](#open-an-issue) that describes the problem your PR solves, and see this document's guidance on [developing new features](#requesting-features), if applicable. Though it's not a prerequisite, in general we prefer PR's that refer to existing issues, as these PR's solve a known problem or implement a requested feature.

3. Review the project's license, which is [version 3 of the GNU Affero Public License (AGPLv3)](COPYING). Contributions that you make to the project must be submitted under this license.

4. Review the terms of version 1.1 of The Linux Foundation's [Developer Certificate of Origin (DCO)](DCO.md). We require that all submissions to the project are made according to the terms of the DCO, which effectively means that you have the legal right to make your contribution. For background on why we require this step, please see the following links:

   * https://wiki.openstack.org/wiki/OpenStackAndItsCLA
   * https://lwn.net/Articles/592503/

Once you've completed these preliminaries, we recommend that you proceed as follows:

1. [Fork this repo](https://github.com/hackworthltd/primer/fork), and create a new branch for your work, starting from this project's `main` branch.

2. Implement your changes, and commit them to your branch. See the [`git` commit recommendations](#git-commit) section of this document for advice on how to structure your commits when making contributions to this project.

3. If your contribution includes code changes, please run the project's test suites, linting checks, and code formatting checks on your own development machine before opening the pull request. If your contribution consists only of documentation, you can skip these checks.

4. As is customary for open source projects that use a DCO check, we require that you specify the `--signoff` flag when running `git commit` on commits to your branch, in order to signify your agreement to the DCO. Should you forget to use this flag on one or more commits on your branch, in most cases, retroactively applying it is relatively easy &mdash; for any given series of commits in a branch, you can usually run:

   ```sh
   git rebase HEAD~N --signoff
   ```

   where `N` is the number of individual commits in your branch, and then force-push the branch to GitHub, if necessary. If some of the commits are already signed off, this command will only add a signoff to the commits for which the signoff is missing.

5. Open a pull request on this repo and target the `main` branch.

## <a name="review-process"/> The pull request review process

Once your PR is submitted, one or more project maintainers will review it in due course. Note that we can't guarantee a specific time frame for reviewing PR's. We'll try to do so in a timely fashion, but please keep in mind that the maintainers have other commitments within and outside this project, and some may be volunteering their time, to boot.

Upon review, the maintainers may ask you to make changes to your contribution. The maintainers also reserve the right to make their own changes to your contribution, and to co-commit the modified contribution to the project instead of the original, retaining your original copyright in the process.

Please be aware that we may decide not to accept some contributions. Reasons for PR rejection may include, but are not limited to, the following:

* The contribution would break compatibility with existing Primer programs, deployments, clients, etc. (This is not a blanket rule: breaking changes are sometimes required.)

* The contribution removes, disables, or detrimentally alters existing functionality that's outside the scope of the feature request or bug fix.

* The contribution doesn't work as advertised, or is buggy or unreliable.

* The contribution attempts to implement a previously requested feature, but deviates from the feature's specification in some significant way.

* The contribution would take the project in a direction that the project maintainers feel would be detrimental or not useful to the project and/or its purpose; e.g., the contribution adds a feature that might be counter to the project's pedagogical aims.

* The contribution doesn't meet the project's [Haskell style guidelines](docs/haskell-style-guide.md).

* One or more commit messages in the PR don't match the project's [commit message rules](#commit-messages).

* The contribution has been submitted under a license that's incompatible with the project's own license.

* The contributor hasn't agreed to the DCO, or hasn't signed off on all commits.

* The maintainers are uncertain whether the contributor has the right to submit some or all of the contribution's content to the project.

* The contributor has contravened the project's [code of conduct](CODE_OF_CONDUCT.md).

## <a name="git-commit"/> `git` commit recommendations

We're appreciative of anyone who takes the time to submit a contribution to our project, so assuming that you've made a contribution that we think will improve the project, and the contribution meets the project guidelines as prescribed in this document, it's very likely that we'll accept the contribution as-is.

Having said that, while we don't want to impose a particular working style or development process on anyone who makes an occasional contribution, it would be very helpful to the project maintainers if you would consider the following recommendations when deciding how to structure the work in your PR branch:

* Each `git commit` should make a single, focused change, while ensuring that all tests pass. For example, if you want to implement a new API feature, first implement the core functionality (and write a few tests for it) in the `primer` package, and commit those changes as one (or more) `git commit`s; then add a final `git commit` which exposes the new functionality to client applications via the HTTP API in the `primer-service` package.

* Don't cross purposes within a single `git commit`. For example, if you're implementing a new feature and, in the process, you discover a bug in some existing feature, don't make a single `git commit` that both fixes the bug and simultaneously adds the new feature: fix the bug in one commit, and then add the new feature in a subsequent commit. (In fact, in a case like this example, you should probably instead [open a new GitHub Issue](#open-an-issue) describing the bug you've found, and then [submit an entirely different PR](#submit-a-pr) that fixes the bug, separate from the feature work you're doing.) Note that if you're following our [commit message rules](#commit-messages), this practice should happen more or less automatically.

* As a general rule, don't squash commits unless the later commit(s) corrects an obvious mistake you made while working on the in-progress PR, such as a fix for a bug you introduced, or correcting a typo you made. We prefer that the project's `git` history tell a sort of story about how a feature evolved during its development, rather than presenting a single, atomic commit manifesting a perfectly-crafted gem out of thin air. On the other hand, "fixup"-style `git` commits are not very useful, especially when tacked on at the end of a PR commit train, so we prefer that you squash or rebase those.

In our experience, structuring `git` commits in a PR branch as a sequence of small, focused changes makes the PR review process much more effective and efficient: it's easier to identify problems, and helps ensure that reviewers properly understand the changes that are being made, because multiple smaller commits in series usually means less cognitive load for the reviewer, as compared to a single mega-commit.

We're aware that some contributors will prefer not to work this way, so please understand that these are *recommendations*, not rules. That said, the project maintainers do themselves follow these guidelines as a rule. Here are some good examples of non-trivial PR's that were developed using this approach:

* https://github.com/hackworthltd/primer/pull/958

* https://github.com/hackworthltd/primer/pull/949

* https://github.com/hackworthltd/primer/pull/892

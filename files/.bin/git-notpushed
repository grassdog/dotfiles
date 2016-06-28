#!/bin/sh
#
# Shows commits you haven't pushed to the remote yet.  Accepts same
# arguments as git-log.  Assumes 'origin' is the default remote if no
# branch.Foo.remote configuration exists.

curr_branch=$(git symbolic-ref -q HEAD | sed -e 's|^refs/heads/||')
origin=$(git config --get "branch.$curr_branch.remote")
origin=${origin:-origin}

git log $@ $curr_branch ^remotes/$origin/$curr_branch


function in_git_repos() {
    test "`git rev-parse --is-inside-work-tree 2>/dev/null`" = "true"
}

function git_current_branch() {
    if in_git_repos; then
        ref=$(git symbolic-ref HEAD 2> /dev/null) || \
        ref=$(git rev-parse --short HEAD 2> /dev/null) || return
        echo ${ref#refs/heads/}
    fi
}

function git_status_is_clean() {
    if in_git_repos; then
        local lines=$(git status --porcelain | egrep -v '^\?\? ' | wc -l)
        test $lines = 0
    fi
}

function git_unknown_files() {
    if in_git_repos; then
        local lines=$(git status --porcelain | egrep '^\?\? ' | wc -l)
        test $lines = 0
    fi
}

function git_stash_is_clean() {
    if in_git_repos; then
        local lines=$(git stash list | wc -l)
        test $lines = 0
    fi
}

function git_no_branches() {
    if in_git_repos; then
        local lines=$(git branch | wc -l)
        test $lines = 1
    fi
}

function git_single_remote() {
    if in_git_repos; then
        local lines=$(git remote | wc -l)
        test $lines -le 1
    fi
}

function git_no_remote() {
    if in_git_repos; then
        local lines=$(git remote | wc -l)
        test $lines = 0
    fi
}

function git_branch_is_pushed() {
    if in_git_repos; then
        if git branch -r | grep "origin/master" >/dev/null 2>&1; then
            git_no_remote || git diff-tree --quiet origin/master heads/master
        fi
    fi
}

#-----------------------------------------------------------------------------

function git_prompt_precmd() {
    local GITINFO=""
    if [ ! -z `git_current_branch` ]; then
        GITINFO=" "
        if ! git_status_is_clean; then
            GITINFO="$GITINFO*"
        fi
        if ! git_unknown_files; then
            GITINFO="${GITINFO}?"
        fi
        if ! git_branch_is_pushed; then
            GITINFO="$GITINFO➚"
        fi
        if ! git_stash_is_clean; then
            GITINFO="$GITINFO☰"
        fi
        if ! git_no_branches; then
            GITINFO="$GITINFO⌥"
        fi
        if ! git_single_remote; then
            GITINFO="$GITINFO®"
        fi
        GITINFO="$GITINFO "
    fi
    echo $GITINFO
}


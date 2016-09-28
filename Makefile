PROJECT = zone_man
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1


DEPS = cowboy jsx
dep_cowboy = git https://github.com/ninenines/cowboy.git e80291f
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.8.0

SHELL_OPTS = -eval "application:ensure_all_started(zone_man)" # -config zone_man

include erlang.mk

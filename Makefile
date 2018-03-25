PROJECT = zone_man
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1
# PROJECT_VERSION = $(shell git describe --tags --abbrev=0)

BUILD_DEPS = elvis_mk
DEPS = cowboy jsx lager
dep_cowboy_commit = 2.2.2
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.9.0
dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0
dep_lager = git https://github.com/erlang-lager/lager.git 3.4.1

DEP_PLUGINS = cowboy elvis_mk

SHELL_OPTS = -eval "application:ensure_all_started(zone_man)" # -config zone_man

# OmniOS build/packaging
BUILDDIR = build
BUILDTMP = tmp

BUILD_TIME=$(shell TZ=UTC date +"%Y%m%dT%H%M%SZ")
export IPS_FMRI=server/${PROJECT}@${PROJECT_VERSION}:${BUILD_TIME}
export IPS_DESCRIPTION=${PROJECT_DESCRIPTION}
export IPS_SUMMARAY=${IPS_DESCRIPTION}
#PKG_VERSION	?= $(shell git describe --tags | tr - .)
ARCH=$(shell uname -p)

certs/ca.pem:
	cd certs; cfssl gencert -initca ca.json | cfssljson -bare ca

certs/server.pem:
	cd certs; cfssl gencert -ca ca.pem -ca-key ca-key.pem server.json | cfssljson -bare server

certs/client.pem:
	cd certs; cfssl gencert -ca ca.pem -ca-key ca-key.pem client.json | cfssljson -bare client


.PHONY: certs
certs: certs/ca.pem certs/server.pem certs/client.pem

package: ips-prototype
	mkdir -p ${IPS_BUILD_DIR}/var/lib/zone_man

	# SMF
	mkdir -p ${IPS_BUILD_DIR}/lib/svc/manifest/application/
	cp smf.xml ${IPS_BUILD_DIR}/lib/svc/manifest/application/${PROJECT}.xml
	cp epmd.xml ${IPS_BUILD_DIR}/lib/svc/manifest/application/epmd.xml

	# Config
	cp omnios.config "${IPS_BUILD_DIR}/etc/${PROJECT}.config"
	$(call add-ips-transform, "<transform file path=etc/zone_man.config -> add preserve true>")

publish: ips-package
ifndef PKGSRVR
	echo "Need to define PKGSRVR, something like http://localhost:10000"
	exit 1
endif
	pkgsend publish -s ${PKGSRVR} -d ${IPS_BUILD_DIR} ${IPS_TMP_DIR}/pkg.pm5.final
	pkgrepo refresh -s ${PKGSRVR}

include erlang.mk
include erlang-ips.mk

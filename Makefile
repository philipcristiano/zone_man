PROJECT = zone_man
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.0.1
# PROJECT_VERSION = $(shell git describe --tags --abbrev=0)

DEPS = cowboy jsx mixer cowboy_swagger
dep_cowboy = git https://github.com/ninenines/cowboy.git 1.0.4
dep_mixer  = git https://github.com/inaka/mixer.git 0.1.4
dep_jsx = git https://github.com/talentdeficit/jsx.git v2.8.0
dep_cowboy_swagger = git https://github.com/inaka/cowboy-swagger.git 1.1.0

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

define IPS_METADATA
set name=pkg.fmri value=${IPS_FMRI}
set name=pkg.description value="${IPS_DESCRIPTION}"
set name=pkg.summary value="${IPS_SUMMARAY}"
set name=variant.arch value=${ARCH}
endef
export IPS_METADATA

package: rel
	rm -rf ${BUILDDIR} ${BUILDTMP}
	mkdir -p ${BUILDDIR}/opt/ ${BUILDTMP}
	cp -R _rel/zone_man_release ${BUILDDIR}/opt/zone_man

	# SMF
	mkdir -p ${BUILDDIR}/lib/svc/manifest/application/
	cp smf.xml ${BUILDDIR}/lib/svc/manifest/application/zone-man.xml

	pkgsend generate build | pkgfmt > ${BUILDTMP}/pkg.pm5.1
	cp LICENSE ${BUILDDIR}/

	# Store metadata into a file
	echo "$$IPS_METADATA" > ${BUILDTMP}/pkg.mog

	pkgmogrify ${BUILDTMP}/pkg.pm5.1 ${BUILDTMP}/pkg.mog transform.mog | pkgfmt > ${BUILDTMP}/pkg.pm5.final


	pkglint ${BUILDTMP}/pkg.pm5.final

publish:
ifndef PKGSRVR
	echo "Need to define PKGSRVR, something like http://localhost:10000"
	exit 1
endif
	pkgsend publish -s ${PKGSRVR} -d ${BUILDDIR} ${BUILDTMP}/pkg.pm5.final
	pkgrepo refresh -s ${PKGSRVR}

include erlang.mk

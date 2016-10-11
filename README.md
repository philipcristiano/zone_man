# Zone Manager

[![Build Status](https://travis-ci.org/philipcristiano/zone_man.svg)](https://travis-ci.org/philipcristiano/zone_man)

Manage Solaris zones via an HTTP API.

## Building (on OmniOS)

Requires gnu-make and erlang, tested with @19.
```
PATH=/opt/omni/bin/:/usr/gnu/bin:$PATH make deps app
```

## Publishing the IPS package

```
PKGSRVR=YOUR_IPS_REPO PATH=/opt/omni/bin/:/usr/gnu/bin:$PATH make package publish
```

# API


Full API Docs are available via Swagger at the path `/api-docs/`.

At the moment only listing zones is supported. Creating zones is the next
feature.

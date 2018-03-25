# Zone Manager

[![Build Status](https://travis-ci.org/philipcristiano/zone_man.svg)](https://travis-ci.org/philipcristiano/zone_man)

Manage Solaris zones via an HTTP API.


## Building (on OmniOS)

Requires gnu-make and erlang, tested with @19.
```
PATH=/opt/omni/bin/:/usr/gnu/bin:$PATH make deps app
```

### Generating certs

The `certs` directory contains example cfssl files. For development running `make certs` will generate enough to work with.

## Publishing the IPS package

```
PKGSRVR=YOUR_IPS_REPO PATH=/opt/omni/bin/:/usr/gnu/bin:$PATH make package publish
```

# API


## Listing Zones

Zones on a machine can be listed with the `/v1/zones` endpoint.

```
% curl http://API/v1/zones
{"zones":[{"id":"2",
           "name":"dev",
           "status":"running",
           "path":"/",
           "uuid":"4872c422-3ad9-4b99-8b9a-a416f282b866",
           "brand":"native",
           "iptype":"excl"}]}
%
```

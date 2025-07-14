<!-- @format -->

# vLLM Proxy

[Hosted on Robert Fischer's GitHub](https://github.com/robertfischer/vllm-proxy)

## THIS PROJECT IS JUST GETTING STARTED.

# See Issues

See [GitHub Issues](https://github.com/RobertFischer/vllm-proxy/issues) for what
features are implemented.

# What This Project Is

A proxy that sits in front of multiple vLLM instances and proxies to them. It
should be a drop-in proxy for vLLM: clients should be able to treat it exactly
like it is a single vLLM instance. The proxy should be lightweight and not
introduce significant request/response overhead. The intention is also for it to
be very easy to use, especially in Kubernetes environments and via Docker
Compose.

# What This Project Is NOT

This project will not implement anything that can be effectively implemented by
putting a web proxy in front of it. This excludes at least the following
features which are all already implemented very well by Nginx:

- Access logging
- Authentication
- Connection limiting
- Rate limiting
- HTTP response caching
- Obscuring Error Pages (ie: removing useful feedback on errors to thwart l33t
  h4x0rZ)

# Features

## Load Balancing

(See [Issue #7](https://github.com/RobertFischer/vllm-proxy/issues/7).)

Weight routing to prefer backends with fewer active requests.

## Multiple Models

(See [Issue #12](https://github.com/RobertFischer/vllm-proxy/issues/12).)

Do you wish that vLLM would serve multiple models? Then run a vLLM instance
serving each model and have them be proxied by this service. You can treat this
service just like a vLLM which serves multiple models.

## Cache-Aware Routing

(See [Issue #2](https://github.com/RobertFischer/vllm-proxy/issues/12).)

Weight routing to prefer routes based on previous cache hits in order to better
leverage vLLM's prefix caching.

# Usage

## Redis and State

The server is effectively stateless: state is stored in Redis. This means that
you can run multiple instances of this proxy pointing to the same Redis database
and they will all share the same back-end state. (We wouldn't want the proxy
becoming a single point of failure, right?)

The default connection string is parsed from the `VLLM_PXY_REDIS` environment
variable. That can be overridden by the `-R` command line flag. The value is a
Redis connection URL, such as:

```
redis://username:password@host:42/2
```

That would connect to the host `host` on port `42` using username `username` and
password `password` and use database `2`. (You do know that a single Redis
server has multiple databases, right?)

You can also use the string `local` to connect to `localhost:6379`. This is the
default.

Note that if the environment variable is configured to an invalid value, the CLI
will fail to start with a useful error message.

vLLM Proxy expects to be the only user of the Redis database. We won't be adding
support for sharing a database (eg: key prefixing) because Redis databases are
cheap and plentiful (and most people just use one database on their server
anyway, so there's 15 empty databases just sitting there).

The way data is stored in Redis is subject to change and should not be relied
upon. The data itself is assumed to be transient: at any point in time, one or
more (or all!) the data might disappear from the Redis database and the system
will continue to function (although performance will degrade).

## Logging

### Logging Level

Logging goes to standard out. If you want it to go to a file, redirect it.

The default verbosity is parsed from the `VLLM_PXY_VERBOSITY` environment
variable. Options are:

- `1` or `Loud`: Every message with all the available details.
- `Verbose`: Every message with some extra details.
- `Debug`: Every message with no extra details.
- `Info`: Informational messages (and more severe). This is the default level.
- `Warning`: Warning messages (and more severe).
- `0`, `Quiet`, or `Error`: Error messages (and more severe).

You can specify multiple `-v` and `-q` on the command line to move more verbose
or more quiet. There is no way to entirely disable logging by design.

Any message at or above the Error level will always include all available
details. All the available details will also be included if the logging is not
to the terminal.

### Logging Format

We use a structured logger for ease of parsing in operational environments. In
other words, instead of seeing messages like
`Failed to parse value: [input="foo"]`, you will get a message
`Failed to parse value` and the log entry will have an associated value named
`input` with the value `"foo"`.

There are three formats: a single-line bracketted format (which developers are
used to), a YAML-based format, and a single-line JSON-based format.

The default format is parsed from the `VLLM_PXY_LOGFORMAT` environment variable.
If it isn't set, then the default for a terminal is to use the bracket format
and the default otherwise is to use a JSON format. The format can also be set
using the `-L` command line flag.

The values for the environment variable and the command line flag are based on
the file suffix for the appropriate format:

- `yml` or `yaml`
- `json`
- `txt` (bracket)

# License

Copyright 2025 Robert Fischer

Licensed under the Apache License, Version 2.0 (the "License"); you may not use
this file except in compliance with the License. You may obtain a copy of the
License at

       http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed
under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
CONDITIONS OF ANY KIND, either express or implied. See the License for the
specific language governing permissions and limitations under the License.

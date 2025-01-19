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
- Caching

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

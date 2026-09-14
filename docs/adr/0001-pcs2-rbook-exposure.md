# ADR 0001: Use Explicit PCS2 Rbook Exposure Relations

## Status

Accepted

## Context

Rbook articles need a server-rendered list of related PCS2 problems. PCS2 already has algorithm tags, but a tag describes a problem's topic; it does not describe whether or where another product should publish that problem. Using a shared `rbook` tag would conflate those meanings and make cross-product publication rules difficult to audit.

Rbook and PCS2 run in separate Docker containers. The rbook service therefore needs an internal API endpoint for low-latency requests while links rendered for readers must use PCS2's public URL.

## Decision

PCS2 problems may declare an optional `showAtRbook` string array containing rbook Article IDs. PCS2 exposes an exact-match `showAtRbook` query filter and returns the normalized field in list and detail responses. Invalid field types fail PCS2 content validation; missing fields behave as an empty list.

The rbook server queries PCS2 using `PCS2_API_BASE_URL` and builds reader-facing links with `PCS2_PUBLIC_BASE_URL`. It requests difficulty ascending order, follows pagination up to a bounded result count, and renders the practice section only when related problems are available. PCS2 failures do not block article rendering.

## Consequences

- Publication relationships are explicit and independent from topic tags.
- One problem can be shown under multiple rbook articles.
- PCS2 does not need to depend on the rbook repository to validate Article IDs.
- Existing tag filters and default ordering remain backward compatible.
- Existing problem files are not migrated automatically; migration is a separate audited task.
- Deployment must put both containers on a shared Docker network and configure separate internal and public PCS2 base URLs.

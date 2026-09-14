# Rbook Practice Linking Context

This context defines how rbook articles are related to programming problems maintained by PCS2. It keeps the shared language stable while the automatic practice-list feature is designed.

## Language

**Article**:
An algorithm-learning page in the rbook book.
_Avoid_: chapter, lesson (when referring to the page as a content unit)

**Article ID**:
The stable front matter identifier of an Article, used as the Article's PCS2 matching tag.
_Avoid_: file name, URL slug

**PCS2 Problem**:
A programming problem published by the PCS2 problem-solution service.
_Avoid_: exercise, question (when referring to the PCS2 record)

**Problem Tag**:
A tag attached to a PCS2 Problem for grouping and retrieval.
_Avoid_: article tag (the rbook Article has its own tags)

**Automatic Practice Section**:
The end-of-Article section that lists PCS2 Problems whose Rbook Exposure includes the Article ID.
_Avoid_: manually managed problem list, problem mapping

**Internal PCS2 Endpoint**:
The container-to-container address used by the rbook service to reach PCS2 over a shared Docker network.
_Avoid_: localhost PCS2 address, public PCS2 URL (when discussing internal service communication)

**Rbook Exposure**:
The explicit list of rbook Article IDs on a PCS2 Problem that determines where that problem is shown in the book.
_Avoid_: rbook tag, topic tag

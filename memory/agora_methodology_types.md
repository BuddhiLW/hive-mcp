# Decision: Agora Methodology Types

## Status: PROPOSED

## Context
Agora dialogues currently use a single approach (opinion-based quick debates). This document formalizes three distinct methodology types for different use cases.

## Methodology Types

### 1. Opinion-Based Agora (Current Default)
**When to use:**
- Architectural preferences and style decisions
- Code review feedback cycles (writer/critic)
- Design trade-off discussions
- Quick consensus on implementation approaches

**Required Preparation:**
- Participants with opposing viewpoints or complementary expertise
- Clear topic/question framing
- No prerequisite research needed

**Signal Types:**
- Primary: `propose`, `counter`, `approve`
- Fast iteration: multiple proposal/counter cycles acceptable
- Goal: Reach Nash equilibrium through discussion

**Example Use Cases:**
- "Should we use Strategy vs Decorator pattern here?"
- "KG Permeation as Theorem Derivation" debate
- Drone architecture scope decisions
- Team preset composition choices

**Typical Duration:** 2-6 turns

---

### 2. Fact-Based Agora (Research-Heavy)
**When to use:**
- Empirical questions requiring evidence
- Performance comparisons (needs benchmarks)
- Compatibility/feasibility verification
- "Does X actually work?" questions

**Required Preparation:**
- Each participant gathers evidence BEFORE dialogue starts
- Data collection phase (grep codebase, run tests, check docs)
- Evidence must be citable (file:line, benchmark results, external docs)

**Signal Types:**
- Heavy use of `defer` until evidence gathered
- `propose` only when backed by data
- `counter` must include counter-evidence, not just opinion
- `approve` means "evidence is convincing"

**Example Use Cases:**
- "Is KG actually used? How many edges exist?" (empirical check)
- "Which approach has better performance?" (benchmark required)
- "Does this library support X feature?" (doc verification)
- "Is there technical debt in module Y?" (code analysis)

**Typical Duration:** 4-10 turns (longer due to research phases)

**Special Rule:** Participants may request `defer` to gather more data mid-dialogue.

---

### 3. Mixed Agora (Research → Opinion Synthesis)
**When to use:**
- Complex decisions requiring both facts AND judgment
- Architecture decisions with empirical constraints
- "What should we do about X?" where X needs investigation first

**Required Preparation:**
- Phase 1: Fact-gathering (each participant researches independently)
- Phase 2: Share findings (propose/counter on interpretations)
- Phase 3: Synthesize decision (approve consensus position)

**Signal Types:**
- Phase 1: `defer` while researching, `propose` to share findings
- Phase 2: `counter` to challenge interpretations, `propose` alternatives
- Phase 3: `approve` or `no-change` to reach equilibrium

**Example Use Cases:**
- "KG value proposition" - first measure usage, then debate value
- "Crystallization lifecycle gaps" - audit current state, then propose fixes
- "Drone coordination scope" - research current capabilities, then decide boundaries

**Typical Duration:** 6-15 turns (3 distinct phases)

**Special Rule:** Coordinator may declare phase transitions.

---

## Choosing the Right Methodology

| Question Type | Methodology | Key Signal |
|--------------|-------------|------------|
| "Which approach do you prefer?" | Opinion | propose/counter |
| "Does X work?" / "How many Y exist?" | Fact-based | defer until evidence |
| "What should we do about X?" | Mixed | phased approach |
| "Review my implementation" | Opinion | propose → approve |
| "Is this performant?" | Fact-based | benchmark first |

## Implementation Notes
- Current Agora tools support all methodologies (no code changes needed)
- Coordinator should specify methodology when spawning dialogue
- `mediator-needed?` flag helps detect stuck fact-based dialogues
- Timeout handling differs: opinion (5min), fact-based (15min), mixed (20min)

## Tags
decision, agora, methodology, nash-equilibrium, architecture, dialogue-protocol

## Duration
permanent
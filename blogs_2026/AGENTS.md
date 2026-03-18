# AGENTS.md — Substack Writing Workflow

## Purpose

This repository produces public-facing economic writing for Substack and media publication.
Clarity, factual accuracy, and preservation of author voice are primary goals.

---

## Agent Tasks

Claude Code supports four writing workflow tasks. Each is invoked by describing your goal.
Match your request to the relevant task below; defaults and constraints apply throughout.

### 1. Post Setup

Use when starting a new piece from scratch or from rough notes.

**What to provide:**
- Topic or prompt
- Any notes, bullet points, data, or prior drafts
- Intended audience (default: Substack economic audience — informed but not academic)
- Approximate target length

**Agent will produce:**
- Working title and 1–2 alternatives
- Lede options (2–3 variants with different angles)
- Outline with section headers and 1-sentence description of each
- Key claims that need sourcing, flagged explicitly
- A short list of open questions to resolve before drafting

**Agent will not:**
- Write the full draft at this stage
- Fill in data or citations it cannot verify
- Choose the angle for you — it will surface options

---

### 2. First Draft

Use when the outline is settled and you want a working draft.

**What to provide:**
- Approved outline or detailed notes
- Any data, statistics, or citations already confirmed
- Tone guidance if non-default (default: rigorous but readable Substack prose)

**Agent will produce:**
- Full draft in `.qmd` format, saved to `drafts/`
- Inline `[CITE]` markers wherever a source is needed but not yet confirmed
- Inline `[CHECK]` markers for any empirical claim the agent is uncertain about
- A short end-of-draft note listing unresolved flags

**Agent will not:**
- Invent statistics or fill in `[CITE]` markers speculatively
- Use em-dashes
- Use generic AI transitions ("Moreover," "It's worth noting," "In conclusion,")
- Overwrite any existing file in `posts/drafts/` — it will version with a suffix

**Style defaults:**
- Punchy, declarative sentences. No filler.
- Concrete claims over abstract framing.
- Technical content explained once, precisely, then referred to by name.
- Subheads only if the piece is long enough to need navigation (>1,000 words).

---

### 3. Final Edit and Proofreading

Use when a draft is substantively complete and needs a clean editorial pass.

**What to provide:**
- The draft file path
- Any specific concerns (e.g., "the third section feels weak," "check the GDP numbers")

**Agent will produce:**
- An edited file saved as `[filename]-edited.qmd`
- A clearly formatted diff summary: what changed, why, and what was left alone
- Flags for any sentence that was meaningfully changed (not just punctuation)

**Editing discipline:**
- Prefer the lightest edit that fixes the problem.
- Preserve argument structure and section order unless asked to change it.
- Do not change the author's word choices without flagging it.
- Do not add hedges or qualifications not present in the original.
- Editing priority order: factual correctness → logical clarity → argument structure → style.

**Proofread checklist (run on every final edit):**
- [ ] No grammatical errors or typos
- [ ] All numbers consistent within the piece (e.g., same figure not reported two ways)
- [ ] All acronyms defined on first use
- [ ] Tense consistent throughout
- [ ] No dangling claims without antecedents
- [ ] Lede delivers on what the headline promises

---

### 4. Embarrassment Check

Use before publishing. A final pass for anything that could cause problems post-publication.

**What to provide:**
- The near-final draft

**Agent will flag:**

*Factual risks:*
- Statistics that cannot be traced to a primary source
- Claims about "current" policy or data that may be stale — note the vintage
- Attributions to papers, people, or institutions that should be verified
- Causal language ("X caused Y") where the evidence only supports correlation

*Rhetorical risks:*
- Characterizations of opponents' views that could be called a strawman
- Superlatives ("first," "largest," "only") that need verification
- Any prediction stated with more confidence than the evidence warrants

*Embarrassment risks:*
- Jargon that will confuse a non-economist reader without payoff
- Sentences that will read as arrogant or condescending
- Anything that contradicts a claim made in a prior post (if prior posts are available in context)
- Hedges so heavy they undermine the argument

**Agent will not:**
- Rewrite to fix issues — it flags and suggests; you decide.
- Rate the piece or give an overall verdict.

---

## File Conventions

```
drafts/      # Working drafts (.qmd)
data/        # Data files referenced in posts
figures/     # All figures referenced in posts must exist here locally
```

- Do not rename figures or data files unless asked.
- Do not remove code chunks from `.qmd` files silently.
- Draft filenames: `[slug].qmd`
- Versioned edits: `[slug]-v2.qmd`, `[slug]-edited.qmd`

---

## Quarto

When editing `.qmd` files, attempt to render after making edits:

```bash
quarto render drafts/[filename].qmd
```

If rendering cannot be executed in the current environment, provide:
- The exact render command
- Likely failure points to check (missing packages, broken chunk options, unresolved references)

Do not silently remove code chunks. If a chunk is causing a render error, comment it out and note it explicitly.

---

## Citations and Claims

- Numbers must have an identifiable primary source (BLS, BEA, Fed, peer-reviewed paper).
- Never invent citations.
- If a source is uncertain, use `[CITE: description of what's needed]` and flag it.
- Prefer the most recent vintage of data available; note the vintage in the text or a footnote.
- If a claim is contested in the literature, say so.

---

## Output Rules

- Do not overwrite existing drafts — save as a new version with `-v2`, `-edited`, etc.
- Prefer additive edits; destructive changes require explicit instruction.
- Record any structural changes in the diff summary.

---

## Default Behavior

Assume **Writing Mode** unless told otherwise:

- Clarity over cleverness
- Specificity over rhetoric
- Edits should reduce friction for publication, not add polish for its own sake
- When in doubt, flag and ask rather than decide silently

This document captures ideas we want to work on in the future.

## Visualizing Flops/Hands as Circular Rings (Concept)

### Goal
Define (later implement) a small set of helper functions to render a **flop** (3 cards) or **hand** (2 cards) as a **single circular ring** (or multi-ring target), where ranks appear in positional order around a circle.

### Rank order & adjacency (wheel-aware)
Use a circular rank ordering where **Ace is adjacent to both ends**:
- Left of `A` is `2` (wheel low end)
- Right of `A` is `K` (broadway high end)

Intended canonical rank cycle (clockwise or counterclockwise, but consistent):
`A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2` with adjacency wrapping so `2 ↔ A ↔ K`.

### Input & parsing
- Input: a string representing either:
  - a **flop** like `"AsTdTc"` (three cards), or
  - a **hand** like `"AhKh"` (two cards)
- Parse into cards (rank + suit), normalize ranks to `A K Q J T 9 ... 2`.

### Spacing and layout constraints
- Each rank “slot” around the ring should have **enough horizontal/arc spacing** to place suit glyph(s):
  - up to **three suits** in a single rank slot (e.g., paired/trips boards where multiple cards share a rank)
- Conceptually, each rank position holds a small “stack” of 0..n suit markers.

### Variants (single vs multi-ring)
- **Single ring**: show all ranks; populate only the ranks present in the string.
- **Multi-ring target** (optional variants to support later):
  - Outer ring: flop (3 cards)
  - Inner ring(s): turn/river cards if provided

### Intended use in notebooks
- Keep the API notebook-friendly: takes a string, returns a plot object (or HTML/SVG) suitable for Quarto.

### Usage sketch
- For a flop: 
  - df |> filter(Flop == c("AsTdTc")) |> render_flop_ring()
- For a hand:
  - df |> filter(Hand = c("AsKd") |> render_hand_ring("AhKh")

## ggplot2 Hand-Range Grid Layer/Geom (Concept)

### Goal
Define (later implement) a **ggplot2-friendly layer/geom** that renders a poker **hand range** as a standard **13×13 grid** (rank-by-rank matrix), suitable for use alongside existing tile-based geoms (e.g., `geom_tile()`).

### Visual encoding
- Grid cells correspond to starting hands:
  - Diagonal: pairs (`AA`..`22`)
  - Top right triangle: suited combos (`AKs`, `QJs`, ...)
  - Bottom left triangle: offsuit combos (`AKo`, `QJo`, ...)
- Accept **partial probabilities/weights** per hand and map them to:
  - `alpha` (transparency), and/or
  - fill color intensity / continuous scale
  - or some other idea we come up with later
- Should support both, so long as this conforms with a supported hand range format:
  - “selected/unselected” (binary) ranges, and
  - weighted ranges (0..1), including sparse / missing hands treated as 0.

### Data + aesthetics (intended shape)
- Should be straightforward to feed a compact range string
- Should probably interoperate with ggplot2 scales/legends (e.g., `scale_fill_...`, `scale_alpha_...`) rather than inventing custom legend logic.

### Input syntax requirements
- Accept range strings in the formats/syntax used by **Flopzilla Pro** and **GTO+**.
- When implementing, use and cite the **official syntax documentation** for those tools as the source of truth for parsing rules and edge cases (do not guess).

## Strategy Complexity / Divergence Metrics (Concept)
**Note:** This concept is drafted significantly by GPT-5.2 and I haven't fully audited it yet.

### Goal
Define (later implement) helper functions that take a row’s action probabilities (e.g., `c("Bet 5.9", "Bet 3", "Check")`) and return **one number** or **d-1 numbers** summarizing either:

- **How mixed / concentrated** the strategy is (complexity as “effective number of actions”), or
- **How different** the strategy is from a chosen **reference strategy** (population mean, baseline line, etc.). e.g., "Bet 5.9" with reference to "Check", and "Bet 3" with reference to "Check". The last action is always the reference action.

These are intended as additional features for clustering/analysis alongside Equity/EV, or for comparison to other measures.

### Input contract (assumed)
- Input is a numeric vector `p` of length `d` with names corresponding to action columns.
- Elements should be probabilities (non-negative, sum to 1). If they are not normalized, provide an option to normalize them.
- Zeros are allowed in solver exports; distance/divergence measures require careful handling.

### Solution A: “Concentration” via entropy (or perplexity)
Shannon entropy captures how mixed the strategy is:

$$
H(p) = -\sum_{i=1}^{d} p_i \log p_i
$$

- Max at uniform; min at pure.
- Convert to an “effective number of actions” (perplexity):

$$
\mathrm{perplexity}(p) = \exp(H(p))
$$

For `d = 3`, perplexity ranges from 1 to 3.

Critique: entropy ignores *which* action is favored; it only measures concentration.

R sketch:
```r
strategy_entropy <- function(p, base = exp(1), eps = 0) {
  # eps can be used for smoothing if you want entropy defined at exact zeros
  p <- as.numeric(p)
  p <- p / sum(p)

  if (eps > 0) {
    p <- p + eps
    p <- p / sum(p)
  }

  # convention: 0 * log(0) := 0
  terms <- ifelse(p > 0, p * log(p, base = base), 0)
  -sum(terms)
}

strategy_perplexity <- function(p, base = exp(1), eps = 0) {
  exp(strategy_entropy(p, base = base, eps = eps))
}
```

### Solution B: Distance from a reference strategy
If you want one number capturing how different this row’s strategy is from a baseline `q`, use a divergence/distance.

#### Option B1: Jensen–Shannon divergence (symmetric, finite with smoothing)
Define $m = \tfrac12(p+q)$, then:

$$
JS(p,q) = \tfrac12 KL(p\|\|m) + \tfrac12 KL(q\|\|m)
$$

- Symmetric and bounded.
- With smoothing, it behaves well with solver zeros.

R sketch:
```r
kl_divergence <- function(p, q, eps = 1e-12) {
  p <- as.numeric(p); q <- as.numeric(q)
  p <- p / sum(p); q <- q / sum(q)

  # smoothing to avoid log(0) and division by 0
  p <- p + eps; q <- q + eps
  p <- p / sum(p); q <- q / sum(q)

  sum(p * log(p / q))
}

js_divergence <- function(p, q, eps = 1e-12) {
  p <- as.numeric(p); q <- as.numeric(q)
  p <- p / sum(p); q <- q / sum(q)

  p <- p + eps; q <- q + eps
  p <- p / sum(p); q <- q / sum(q)

  m <- 0.5 * (p + q)
  0.5 * kl_divergence(p, m, eps = 0) + 0.5 * kl_divergence(q, m, eps = 0)
}
```

Critique: requires choosing `q` and deciding a smoothing convention.

#### Option B2: Aitchison distance (principled for compositional data)
Compositional distances should respect the simplex geometry. Aitchison distance is defined via log-ratio transforms.

Practical implementation options:
- Use an **ilr** transform (preferred in theory), then Euclidean distance in ilr-space.
- Or use the project’s existing **reference ratio** idea: pick one action as reference and compute log-ratios (or sqrt-ratios) for the other `d-1` actions, then compute Euclidean or Mahalanobis distance to a reference point.

Sketch using log-ratios with a reference action (mirrors current “reference variable” approach, but with logs):
```r
alr_transform <- function(p, ref_index = length(p), eps = 1e-12) {
  p <- as.numeric(p)
  p <- p / sum(p)
  p <- p + eps
  p <- p / sum(p)

  ref <- p[[ref_index]]
  log(p[-ref_index] / ref)
}

alr_mahalanobis_distance <- function(p, q, Sigma_inv, ref_index = length(p), eps = 1e-12) {
  x <- alr_transform(p, ref_index = ref_index, eps = eps)
  mu <- alr_transform(q, ref_index = ref_index, eps = eps)
  d <- x - mu
  as.numeric(t(d) %*% Sigma_inv %*% d)
}
```

Critique: needs a reference action and zero-handling; Mahalanobis additionally needs a covariance estimate in transformed space.

### Notebook integration idea
- Given `action_cols <- c("Bet 5.9", "Bet 3", "Check")`, compute per-row metrics like:
  - `entropy`, `perplexity`
  - `js_to_mean` (with `q = colMeans(probs)`)
  - `aitchison_to_mean` (in log-ratio space)

These become additional numeric features for clustering (normalize alongside Equity/EV).

## Polarization metric per flop (Concept)
We want a direct “polarization” / “concentration of action” score for each flop, intended to be more interpretable than Mahalanobis distance.

Candidate scalar measures to evaluate:
- Entropy-derived concentration (e.g., low entropy = more polarized), optionally reported as “effective number of actions”
- Herfindahl/Simpson concentration (e.g., sum of squared action probabilities)
- Simplex-appropriate distance such as Aitchison distance to:
  - a uniform mix ("unpolarized" baseline), and/or
  - a chosen template strategy such as “check + big bet” ("polarized" baseline)

Intended function shape (later):
- Input: a dataframe plus a required character vector `action_cols` naming the action-probability columns
- Output: the original dataframe augmented with one (or a small set of) polarization columns
- Requirements: handle solver zeros in a principled, explicitly documented way (smoothing convention / zero-treatment)

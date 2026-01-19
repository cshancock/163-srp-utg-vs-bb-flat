
# Tools for evaluating flops (for two opposing ranges)

## Grouping flops with similar flops
For each flop, each player has an average probability that they take each action available in the betting node. We cannot use these raw values to perform clustering however. We need to first transform them. Two possible ways to calculate distances for clustering with these variables, once they are transformed:
* Aitchinson distance via log transformations
	* The centered log-ratio, in MathJax (for a four-part composition): \[
\mathrm{clr}(f)_i = \log f_i - \frac{1}{4}\sum_{j=1}^{4}\log f_j
\]
	* Or, better, we can use the isometric log-ratio transformation for numerical stability: \[
z_i \;=\; \sqrt{\frac{D-i}{D-i+1}}\;\ln\!\left(\frac{f_i}{\left(\prod_{j=i+1}^D f_j\right)^{1/(D-i)}}\right),\qquad i=1,\dots,D-1
\]
	* CLR is fine for intuition; ILR is better for modeling.
* Jensen-Shannon divergence or Hellinger distance 

## Dynamism
Dynamic boards are those where hands can more easily change value.

A static board is one where the strongest hands on the flop are disproportionately likely to remain strong hands on the river.

Dynamism captures how likely the turn or river is to change equities or 'the nuts' with respect to the current flop or turn.

Dynamism is likely closely related to connectivity, but we measure it with a different approach. Dynamism of a flop
can be said to be measured by how much the next card changes equities. This idea could extend to moving from
the turn to the river as well. 

### Dynamism measures
1. Equity Volatility
	* How jumpy equities are on average. 
2. Lead-Reversal Rate
	* How often the current leader loses the lead on the turn.
3. Nuts Turnover
	* How often the nuts changes. 
4. Rank-Stability
	* Spearman or Kendall correlation between range equity rankings on the flop vs after the turn. 
	* Low correlation implies high dynamism.
	* Hand Strength Persistence is an idea that would have similar methodology.
5. Mutual Information with the Turn


## Connectivity of flops
Some of these could be 'range-aware', or they could be range agnostic. 
- The range agnostic variants could assume fixed ranges, or have a range of 100% of all hands.

### Connectivity Measures
#### Flushing 
* A function that quantifies how 'flushing' each flop is. 
	- A discrete version could just count the number of suits, or classify as "monotone", "two-tone", or "rainbow"
	- A clever version of this would consider, where there are 2 suits, whether the top ranked card 
	is of the same suit as the suit of the draw
* A variant might quantify how many flushes are possible for each player, or for both players, given their ranges
	- Or, just use a fixed, empirical range reference set.

#### Straightening
* A function that quantifies how 'straightening' each flop is
	- It should also capture wheel straights (A2345)


#### Combining flushing and straightening
* A function that combines flushing and straightening concepts to capture 'connectivity'.
	- It should 'reward overlap' between flushing and straightening, so the core logic should be AND, not OR, e.g., (2 * SI * FI) / (SI + FI), where SI = Straightening Index and FI  = Flushing Index


# Tools for evaluating hands (the two hole cards)


# Tools for evaluating ranges (distributions of hole cards)

## A way to visualize ranges: range matrix
A function that takes a string of text representing a condensed hand range, and converts it into a 13x13 matrix.
* The x axis indicates suited hands
* The y axis indicates off-suit hands

Accepted formats include those used by GTO+ and PokerCruncher.

## Nuts advantage
"Nuts advantage" isn't well-defined in poker circles. There are various valid ways of quantifying it. 

We expect that however measured, 'nuts advantage' is correlated with Equity Realization Rate (Expected value / Equity)

However we also expect that nuts advantage is not as relevant on a draw-heavy, dynamic flop. 

Some possibles ways to measure nuts advantage. 
1. **Current Nuts Share**: Who owns the actual or effective nuts on this street.
2. **Top-Tail Occupancy**: How much of the very top belongs to each player. Could be Top-K Occupancy or Top-q% Mean Equity.
3. **Future Nuts Potential**: Probability you'll have the nuts by the river (or next street).


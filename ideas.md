
# Tools for evaluating flops (for two opposing ranges)

## Grouping flops by betting actions
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

Dynamism is likely closely related to connectivity, but we measure it with a different approach. Dynamism of a flop can be said to be measured by how much the next card changes equities. This idea could extend to moving from the turn to the river as well. 

### Dynamism measures
1. Equity Volatility
	* How jumpy equities are on average. 
2. Lead-Reversal Rate
	* How often the current leader loses the lead on the next street.
3. Nuts Turnover
	* How often the nuts changes. ("The nuts" being the top n hands or top q% of equities)
4. Rank-Stability
	* Spearman or Kendall correlation between range equity rankings on the flop vs after the turn. 
	* Low correlation implies high dynamism.
	* Hand Strength Persistence is an idea that would have similar methodology.
5. Mutual Information with the Turn


## Connectivity of flops
Some of these could be 'range-aware', or they could be range agnostic. 
- The range agnostic variants could assume fixed ranges.

### Connectivity Measures
#### Flushing 
* A function that quantifies how 'flushing' each flop is. 
	- A discrete version could just count the number of suits, or classify a flop as "monotone", "two-tone", or "rainbow"
	- A clever version of this would consider, where there are 2 suits, whether the top ranked card is of the same suit as the suit of the draw
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

## Polarization measures
A range is polarized when it contains mostly very strong hands and very weak hands, with few medium-strength hands. Strong hands are those with high equity.

Some possible polarization measures:
1. **Equity Variance**: The variance of hand equities in the range.
	- Variance can conlate 'spread' with 'bimodality', so it may not be the best measure alone.
2. **Polarization score**: A custom score that weights hands based on their distance from the mean equity (or from a threshold equity, such as 0.5).
3. **Extreme share**: Mass within epsilon of either pole (0 and 1). Shows how concentrated the mass is at the ends. 
4. **Bimodality Coefficient**: A statistical measure that combines skewness and kurtosis to assess the bimodality of the equity distribution. BC= (γ^2 + 1) / κ, where γ is skewness and κ is Pearson kurtosis. Measures evidence for two modes, versus unomodal. The rule of thumb is that BC > 5/9 (≈0.555) indicates bimodality.

Polarization score and Bimodality Coefficient are likely the best measures of polarization here.

We can calculate any of these polarization measures for a range on a given board. Alternatively, we can calculate them for the subset of hands that take a certain action at a given node on the betting tree (e.g., call). (Sometimes our calling range might be bimodal! For example, if we have some slow-plays, or are facing a very large bet size.)
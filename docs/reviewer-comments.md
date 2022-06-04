NPH-MS-2017-24165 Decomposing leaf mass into photosynthetic and structural components explains divergent patterns of trait variation within and among plant species
by Katabuchi, Masatoshi; Kitajima, Kaoru; Wright, S Joseph; Van Bael, Sunshine; Osnas, Jeanne; Lichstein, Jeremy

Dear Dr Katabuchi

Thank you for submitting your interesting manuscript to New Phytologist.  Four experts have reviewed the manuscript, the comments of which you may find below this message.  Based on these reviews the decision is reject.  The referees consistently found issues of circularity and other numerous methodological concerns that preclude consideration for publication. You may find the list of concerns below within the referee comments.  I realize this is disappointing however, NP currently accepts ~18% of submissions and I have seen many papers rejected by NP come out later in very good journals after revision per the suggestions of the NP referees.  I hope the referee comments will be of use to you as you revise this manuscript for another outlet.
Good luck!

Sincerely,

Nate McDowell
Editor, New Phytologist

*****************************************************************
In any correspondence regarding this manuscript, please include the manuscript reference number and copy the correspondence to New Phytologist Central Office (np-managinged@lancaster.ac.uk).


Decision: Reject

Referee: 1

Comments to the Author
ACHIEVEMENT OF THE MS
This manuscript sets up the idea you can think of LMA as being made up of two fractions, one (LMAp for photosynthetic) contributing to gas-exchange rates and the other (LMAs for structural) contributing to leaf lifespan. It cross-cites for this idea to an Osnas et al manuscript in review, which is not very helpful to reviewers, but so far as I could tell it did not actually rely on anything in this other ms that was not also explained here.

This seems fair enough as a concept – it’s pretty obvious that different leaf tissues have different functions, and in that sense the idea is hardly new. There will be complexities, though, in actually allocating specific tissues cleanly to one or the other. Venation, for example -- would only the vessel lumens (with zero mass) be seen as contributing to photosynthesis, and the vessel walls to structure? And mesophyll cell wall thickness contributes to structural strength, but is thought also to influence carbon assimilation via mesophyll resistance.

They then estimate these fractions of LMA for each leaf in two datasets. The estimation uses Bayesian methods (which I’m not able to assess in detail) and the fit is produced by how well each component predicts different things. LMAp is asked to predict gross photosynthesis rates (sum of assimilation and dark respiration), LMAs is asked to predict leaf lifespan, both can contribute to predicting respiration. The metabolic rates are expressed on area basis for this purpose.

The effect of this is that estimated LMAp does a better job than aggregate LMA in predicting metabolic rates (Fig 1b compared to 1a), and estimated LMAs does a better job in predicting leaf lifespan (Fig 1i compared to 1g), for the same two datasets. In other words, the partitioning into two fractions works, and to that extent the concept is supported that LMA can be thought of as two fractions with separate influences.

There is surely a considerable circularity about this exercise? Seeing that the LMA fractions are estimated by maximizing the power to separately predict metabolic rates and leaf lifespan, it’s no surprise that they do indeed make those predictions at least somewhat better than aggregate LMA.

Then two further things are shown. First, that LMAp predicts N and P amounts better than LMAs. This reflects what was already well known, that gas exchange rates are quite well correlated with N and P amounts. And indeed it’s obvious that N and P will be concentrated mainly in mesophyll tissues. Second, that LMAs contributes more strongly than LMAp to variation in aggregate LMA. Again this reflects well-established knowledge (which the manuscript itself points out during the Introduction, and which was discussed in Wright et al 2004), that aggregate LMA varies with wider amplitude than area-based metabolic rates.


PRODUCTIVE PATH FORWARD?
Does this concept of estimating two fractions within LMA hold promise for moving forward in practice with trait ecology, or global vegetation modelling? The actual estimating method applied here obviously does not, since you need already to have direct measurements for metabolic rates and leaf lifespans in order to make the estimates of LMA fractions.

So then, are there possibilities for estimating the two components from anatomy? At present, quantitative anatomy remains very time-expensive. Whereas the point of trait ecology has been to find easy-to-measure strategy indicators that can span thousands of species, complementing more detailed physiology on fewer species. It’s possible that better image analysis via machine learning will turn quantitative anatomy into something that can be applied across very large numbers of species. But even then there will remain issues in allocating tissues as between structure and photosynthesis.

In summary, I don’t see that applying this manuscript’s methodology provides a particularly effective pathway for advancing trait comparisons worldwide or for parameterization of vegetation modelling. The Discussion reinforces this impression – it does address the question what might happen next, but it does not have solutions that are ready-to-go. Having said that, the demonstration that LMA can be partitioned into two fractions, which separately have clearer consequences for leaf outcomes than together, is certainly satisfying, without exactly being surprising.

FRAMING OF THE QUESTION
The manuscript frames the present situation as one where Wright et al 2004 advocated, via the leaf economic spectrum, that leaf variation should be thought of along a single dimension. Then more recently Lloyd et al (2013) and Osnas et al (2013) are cited as showing that variation in mass-basis photosynthesis and respiration was greater than area-basis variation, or as they put it, the leaf economic spectrum became considerably weaker if expressed on area-basis. For example Kitabuchi et al lines 475-481 “The improved predictions and understanding provided by decomposing LMA into photosynthetic and structural components challenge the view that leaf functional diversity can be accurately represented by a single leaf economics spectrum (LES) axis (Wright et al. 2004). Lloyd et al. (2013) argued that the apparent dominance of a single LES axis is an artifact of expressing area-dependent leaf traits on a per-mass basis, and Osnas et al. (2013) demonstrated that across the global flora, traits related to photosynthesis and metabolism are indeed area-dependent.”

As one of the authors of Wright et al 2004, I have to say this is a grossly unfair representation of the history of the ideas.  The truth is that discussion what can be learnt from representing leaf traits on area basis versus mass basis goes back at least into the 1980s. Wright et al 2004 actually includes a distinct section with the title “area vs mass basis of expression” – it’s a page long, nearly 20% of the paper.

I can understand an argument that many subsequent citers of the leaf economic spectrum have oversimplified what Wright et al said. In Discussion, the Katabuchi et al ms writes (lines 514-7) “for over a decade, the vast literature on leaf traits has been strongly influenced by the view that leaf trait variation can usefully be represented by a single LES axis. Our results provide quantitative evidence that this one-dimensional view of leaf trait variation is insufficient.” This is a defensible statement. But the rest of the paper gives the impression that the Wright et al paper focussed on mass-basis traits without thinking about it, or perhaps even to deliberately hide the nature of the area-basis relationships. Surely if you’re going to build your narrative around criticism of previous work, you have a responsibility to go back and re-read it thoroughly to see what it actually said, as opposed to how others have summarised it subsequently?

Did Kitabuchi et al not consider why the leaf economic spectrum was called that, rather than (say) the leaf physiological spectrum or the leaf trait spectrum? It was presented and interpreted as a relationship between the cost of investing in leaf area, and the rate at which the investment is paid back. The investment is LMA, and is naturally measured in mass units.

Osnas et al 2013 seemingly did not understand this. They undertook to “correct” the leaf economic spectrum for LMA. The effect of their statistics was to look through the multivariate cloud along the length of the LMA axis, and to identify the next most important dimension of variation after that. That in itself was a perfectly reasonable thing to do. However, they called this orthogonal-to-LMA dimension the “normalization-independent leaf economic spectrum”. This was unhelpful terminology, since what they had actually done was to extract and discard the economics of investment from the spectrum. The spectrum they were looking at captured variation other than along the LMA - leaf lifespan relationship, and needed to be called something else to reflect that.


OTHER COMMENTS

Lines 73-83 argue that supposing there were a leaf trait correlated only with area, then expressing it on a mass basis would introduce correlation with LMA. To be sure, but the reverse is also true – some traits are correlated mainly with mass, and expressing them on an area basis introduces correlation with LMA. It doesn’t help understanding to throw around denigratory words like “spurious”. The question is what’s the correlation structure, and why.

Lines 84-99 emphasize that you can get different outcomes within vs between species. Another important distinction is within versus across habitats. It’s quite common for two traits to be positively correlated across habitats when the relationship is negative within-site.

In eqn 5, it’s assumed that the relationship between LMAs and leaf lifespan will be the same in light (open canopy) as in shade (within-canopy) situations. I’m not sure whether that’s a reasonable thing to expect.

Lines 205 et seq deal with “optimal” leaf lifespan theory, maximizing return per time for a leaf. This body of theory only maximizes return to the plant (which is what you might expect evolution to optimize) if total leaf area is fixed, i.e. production of new leaf requires an old leaf to be discarded. Plants with increasing total leaf area might be expected to keep old leaves longer if they still have positive carbon budgets.




Referee: 2

Comments to the Author
Katabuchi et al. statistically decomposed leaf mass per area (LMA) into photosynthetic- and structural-LMA, and analyzed how these two components are associated with area-based photosynthetic rates (Aarea) and leaf longevity (LL). While I cannot understand the detail of analysis as I am not familiar with Bayesian statistics, some major points in their presented results include: (1) Relationships among LMA, Aarea and LL are better explained by the two components of LMA.  (2) Trait correlations across species are different from those within species in relation to light regimes. (3) Employing the optimal LL model enables them to explain why LL is longer for shade leaves for a given LMA. I found these results are interesting.
This paper is written by a group of scientists who I highly respect, but I have several concerns in the view of this study.

(1) The authors repeatedly use a phrase “one-dimensional leaf economic spectrum” (L54, 70, 482, 516). I don’t think this is a proper term. LES represents a single major axis in multiple dimensional trait space (but see Wright et al. 2005 GEB) but LES itself is not single dimension in my recognition. Furthermore, the authors argued that “one-dimensional LES” is largely the result of mass-normalization of traits (L70).  However leaf lifespan, which is a fundamental, very important trait in LES, is neither mass-basis nor area-basis. I think “one-dimensional LES” is a misleading term.

(2) For a similar reason, I think the 1st and 2nd paragraphs of Introduction and the 5th paragraph of Discussion have a biased view (at least for me). The LES is a correlation and not necessarily causal relationship. I think knowledge such as species with high LMA tend to have higher leaf lifespan and a slow return rate (Amass) is ecologically useful and not “spurious correlation (L79)”. There are different views on the LES among scientists especially between different fields (e.g. plant ecological strategy or C flux), and I do not think the authors need to mount a tangential argument about these different views.  Some criticisms of Lloyd et al. (2013) are based on their misunderstanding of Wright et al. (2004), which are pointed out in Westoby et al. (2013 New Phytol). I think it is not fair to cite only Llyod et al. (2013) without citing Westoby et al. (2013) and also Poorter et al. (2014, New Phytol).

(3) While a statistically strong correlation can be found between LMA and mass-normalization of area-dependent traits, it is hasty to conclude such normalization results into the LES. If the physiological processes underlying the LES are considered, it is clear that large reduction of Amass in high LMA is not simply due to mass-normalization. For example, high LMA leaves have high N content per unit leaf area but have lower Aarea/Narea (PNUE). Reduction of PNUE with high LMA is one of the important reasons for lower Amass in high LMA leaves (despite of their high Narea). This evidence has been known since late 90’s (e.g. Hikosaka et al. 1998; Poorter et al. 1998; Reich et al. 1998; Peterson et al. 1999; Onoda et al. 2004; Hikosaka 2004; Takashima et al. 2004; Wright et al. 2004; 2005…), but the authors did not consider them at all.

(4) “intraspecific variation” is a general term for any variation within species. In this study the authors studied one case of intraspecific variation in relation to light, but they use this term without specifying the condition in Title and Abstract (L2, 40 etc). In the 2nd line of Abstract, they said “intraspecific variation in these rates is strongly mass-dependent”. How can you generalize it?  I think it is not possible to assume that intraspecific variation across light regimes is a general type of intraspecific variation.  For example, Onoda et al. (2004 Func Ecol) showed that deciduous herbaceous species germinated later in the growing season produced lower LMA leaves with a lower fractional allocation to cell walls compared to that germinated earlier. This response was both mass- and area-dependent. Another example is that plants grown at low nutrient availability have more starch accumulation and lower N concentration than those grown at high nutrient availability while LMA often does not change much within a species across soil nutrient availability. Since there are many examples that do not fit to the statement “intraspecific variation in these rates is strongly mass-dependent”, I think the authors should clearly specify what type of intraspecific variation is studied in Title, Abstract and other places.

(5) The authors roughly divided LMA into photosynthetic leaf mass and structural leaf mass. While the concept itself may be ok, it may be necessary to consider that a real leaf consists of many compounds which are not readily categorized into photosynthetic- or structural mass.  Most notably, total non-structural carbodydrate may account for ca. 20% of leaf mass (Poorter et al. 2006 JXB). There are other compounds such as leaf phenolics or tannin whose contribution to leaf mass could be significant in woody species (5-20% Turner 1995 FE, Kurokawa et al. 2010 FE). At least the authors need to discuss other leaf compounds and how those compounds could be grouped/analyzed in their framework.

(6) There are a fair number of studies that reported cell wall (fiber) mass in leaves while the authors did not consider them.  Most recently Onoda et al. (2017 New Phytol) summarized these data and the data are freely available.  Even before this work, several studies reported cell wall mass (fibre mass) in relation to leaf physical strength (e.g. Turner et al. 2000 Biotropica, Read et al. 2003 New Phytol, Onoda et al. 2011 Ecol Lett).

(7) I am not familiar with the Bayesian statistics, so I cannot understand the detail of analysis.  My concern is how to avoid the circularity.  Because the authors estimated from LMAp and LMAs from Aarea, Rdarea and LL, I was wondering the correlations between Aarea and LMAp and between LL and LMAs are mostly auto-correlation. I understand they employed random values to test whether the observed patterns are different from those based on random data, but still there are correlations in these relationships based on random data. So I think the observed patterns are heavily based on auto-correlation and circularity. I would suggest that the authors make stronger efforts to make very clear to a non-statistician how their method works, and what the key caveats are.

(8) In their model, gross photosynthetic capacity is determined by LMAp (L180).  However, in real plants, high amount of photosynthetic proteins is not always translated into high photosynthetic capacity due to lower mesophyll conductance or low specific activity of enzyme (Onoda et al. 2017), how does this affect your results and conclusion?  You reported similar LMAp for deciduous and evergreen species. Is this pattern simply due to this assumption?

Other specific comments
L45 “global leaf-trait database” consists of only 198 species.  Would it be a bit misleading to say “global leaf-trait database” without specifying the number of species in Abstract?
L97, 104, 257, 441. Osnas et al. in review is cited, but I cannot judge to what extent this ms and Osnas et al. in review are overlapped.
L146 I think “toughness” instead of “structural toughness” is correct in this sentence.
L209 How do you consider “the length of unfavorable season (winter or dry season)” in calculation of optimal LL?  (this may be important for LLopt in evergreen plants at high latitude.)
L226 Where can I see the fitted values of theta L?
L337-346. For plant ecophysiologist like me, this paragraph is a bit odd because they stressed that they “accurately” partitioned LMA into photosynthetic and structural LMA components, which are conceptual and imaginary components. This paragraph is just about validation of their assumptions. Even if LMAp and LMSs are real components, for me it is not possible to say “accurately partition LMA into photosynthetic and structural LMA components” unless they are directly measured.
L351-352 “building costs are likely similar for different leaf components”. This is not correct. There is more than two-fold variations in the construction cost depending on leaf components (1.2 for structural carbohydrate, 2.5 for protein and 3 for lipid etc, Penning de Vries et al. 1983).
L455-474 A related argument was also made in Onoda et al. (2011).
References: Several references lack in information about volume and page numbers.
Fig. 1 and 2; a consistent label for x-axes may be preferred.
Other possible useful reference: Dong N, Prentice IC, Evans BJ, Caddy-Retalic S, Lowe AJ, Wright IJ (2017) Leaf nitrogen from first principles: field evidence for adaptive variation with climate. Biogeosciences 14: 481-495

Referee: 3

Comments to the Author
This is an interesting paper which essentially, by conceptually separating the variation in leaf
LMA into two components (one photosynthetic capacity and the other through leaf time time)
manages to find a simultaneous solution through a model fitting procedure that, for a subset of
the GLOPNET database at least, seems to make sense. In principle it is publishable, but I do
offer the following comments which I believe need to be addressed before acceptance.
1. What exactly was used from (and why just) the GLOPNET Database?
Actually, this database – as far as I know – has more or less now become part of TRY (which
itself must have many more useful observations potentially available) and contains complete
records from only about 6 studies and it thus is hardly representative of the global biome
distribution. So, it would be good to have some sort of SI Table summarising what sort of
vegetation types (biome, continent, no of observations, original reference etc) so that the
interested reader can see what sort of plants have actually included in the analysis. I am also
confused as to what the authors mean in the sentence starting with “After deleting leaf
samples… unique species” . Does that mean they simply excluded all species with more than
one observation (or what?).
2. Sources of variability
According to my understanding, intra-specific variability refers to differences in traits
between members of the same species when growing in the same environment (so usually
close to each other) and when exposed to the same climatic conditions (including light
regime). It is not correctly defined as done by the authors here because even for within the
one individual, traits vary systematically with light regime (lower LMA leaves in the shade) as
the authors of the current submission surely understand (but if not for tropical forest trees
this is described in some detail in Lloyd, J., Patiño, S., Paiva, R. Q., Nardoto, G. B.,
Quesada, C. A., Santos, A. J. B., Baker, T. R., Brand, W. A., Hilke, I., Gielmann, H., Raessler,
M., Luizão, F. J., Martinelli, L. A., and Mercado, L. M.: Optimisation of photosynthetic
carbon gain and within-canopy gradients of associated foliar traits for Amazon forest trees,
Biogeosciences, 7, 1833-1859, 2010).
So, for the Panama study the authors are not looking at intra-specific variability at all, but
rather, the differences between sun leaves of mature leaves at the top of the canopy and
shade leaves of (presumably) seedlings. Thus, apart from the potentially confounding effects
of ontogeny (quite likely seedling leaves still live longer than those of mature leaves even
when grown in full sunlight) the authors are really mixing variations due to standmicroclimate
with actual intra-specific genetic variation.
Likewise, there are several studies showing that much of the (apparent) variability in the LES
is due to systematic differences between pots, rather than or different species within plots
(i.e. correlations within plots are typically low). Thus, to me at least it doesn’t make sense to
simply lump the data from the few GLOPNET studies together. Or putting it another way,
if the authors just chose one value of f for each of the seven or so sites/study combinations
making up their database, would the fit actually be inferior to allowing it to vary for each
species? (I would hope not, but especially in terms of the Panama LL result, this really needs
to be checked). Perhaps multilevel modelling (with site as a random effect) might be a way to
address this issue (in R with MCMCglmm would probably do it).
3. Other matters
a. “Gross photosynthesis” is not an accurate term because there are CO2 s coming off
from photorespiration and so A_net + R_dark still does not equal the actual rate of
carboxylation by Rubisco. There is also the problem that, as set up, R_dark is ‘double
dipped’ in the fitting procedure (fitted twice). I know R_dark is a small term relative
to A_net, but for (4) I suggest just A_net. This also circumvents the problem that
you don’t know the extent to which R_dark is actually inhibited in the light.
b. Lines 214 to 218: Especially when considered across biomes, leaf construction costs
are NOT strongly conserved. See the work of H. Poorter and subsequent citations.
c. Fig 1: It would be good to see somewhere a comparison of the AIC of the simple
bivariate linear relationships (leaf had side panels) compared with the more
complicated models. Or is that not a valid comparison? Note also an error in the
symbols legend.
d. It’s just a conceptual model estimated parameter, so perhaps the authors might like
to remind themselves and the reader of this by putting some hats on: something like

  or (more elegantly by using single letter symbols as is the way in
maths/physics journals which take their mathematics seriously)
 (      ).
e. Again, for the Panama study what happens if you just have four categorical groups
for which f is the same within each one? Again this question relates to where the
model is accounting for the variability. Is it within the four groups or simply between
them?
f. Differences in sun/shade leaf structure go back to Bjorkmann (1972). This is a
reference that needs to be cited.
g. Finally, if it were me, I would take a few of the samples from the Panama study to
help understand how the fitted partitioning of the LMA relates to obvious
differences in leaf anatomy both within and across the four groups.

Referee: 4

Comments to the Author
This paper used a Bayesian modeling framework to decompose the leaf biomass to two parts: 1) biomass support photosynthetic and metabolic processes; and 2) biomass support the structure of cells.  This analysis allows us to better understand the difference of photosynthetic response to LMA across species vs within species. As the traditional approach only use LMA for the trait analysis, this work extend the usefulness of traits by decomposing it into different components for a better understanding of the underlying mechanisms. Therefore, I strongly support the publication of this paper in this Journal.

One caveat for this type of analysis is that, in principle, the statistically analysis will decompose the LMA into two parts: 1) one part correlated with photosynthesis and 2) another part that is not correlated with photosynthesis. Thus, statistically it is expected that LMAp will be better correlated with Aarea or Rarea. Thus, what showed in Fig. 1, 2 and 6 is expected based on statistical analysis itself.

Furthermore, I do want the authors to point out that they made an important assumption that the carb use efficiency for photosynthesis (alpha in eq.(4)) is the same across different species, but could be very different in the real word due to the nitrogen content difference, the amount of nitrogen allocated to RUBISCO and the activation status. Therefore, the amount of LMAs that not correlated to Aarea could potentially not result from the carbon investment in structure but instead from the difference of photosynthetic use efficiency of the carbon across species.  I hope the authors could make it clear to the readers of this important assumption and its implications for the interpretations.


Specific comments:
1.      It would be great if the authors could show the value of Robs in Fig. 1 , 2. and 6 for LMAp and LMAs so that it is easier for the reader to see the difference from LMA.
2.      Line 327, for clarity, it would be great to point out what are the variables to calculate robs and rrand?

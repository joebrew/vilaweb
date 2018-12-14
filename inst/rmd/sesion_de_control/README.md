Analysis of congressional speeches
================

    Error in eval(lhs, parent, parent): object 'transcript' not found

    Error in tolower(x): object 'transcript' not found

    Error in tolower(x): object 'transcript' not found

    Error in tolower(x): object 'transcript' not found

    Error in eval(lhs, parent, parent): object 'transcript' not found

    Error in eval(lhs, parent, parent): object 'transcript' not found

    Error in eval(lhs, parent, parent): object 'transcript' not found

    Error in eval(lhs, parent, parent): object 'bigrams' not found

    Error in eval(lhs, parent, parent): object 'bigram_freqs' not found

    Error in eval(lhs, parent, parent): object 'bigram_freqs' not found

Congreso de los diputados speeches
----------------------------------

    Error in gsub("[\r\n]", "", x): object 'flattened_df' not found

    Error in eval(expr, envir, enclos): object 'flattened_df' not found

    Error in is(x, "dfm"): object 'corpus_x' not found

    Error in unique(dim(my_dfm)[1]): object 'my_dfm' not found

    Error in textplot_wordcloud(my_dfm, min_count = 1, random_order = FALSE, : object 'my_dfm' not found

On Wednesday, December 12th, Spanish President Pedro Sánchez delivered an address to the Congreso de los Diputados regarding Brexit and the political situation in Catalonia ([official transcription here](http://www.congreso.es/public_oficiales/L12/CONG/DS/PL/DSCD-12-PL-170.PDF)). The speech reflected rising tensions between pro-independence Catalans and the pro-union Sánchez government, and marked a sharp break with Sánchez's previous more conciliatory tone towards Catalonia. The following back-and-forth between Sánchez and the leaders of other major Spanish political parties was tense, and marked by repeated references to violence.

What follows is linguistic analysis of the speeches and counter-speeches of 6 politicians:

-   Pedro Sánchez (President, PSOE, unionist)
-   Pablo Casado (PP, unionist)
-   Albert Rivera (Ciudadanos, unionist)
-   Pablo Iglesias (Podemos, ambivalent)
-   Carles Campuzano (PDeCat, independentist)
-   Joan Tardà (Catalan Left, independentist)

The questions
-------------

1.  Are there differences in "polarity" (postivity-negativity) between the different politicians' speeches?

2.  Does polarity change when different subjects are discussed (specifically, Catalonia)?

3.  What is the relationship between emotional polarity, violence and references to Catalonia?

4.  Are there differences in lexical diversity between different politicians' speeches?

The methods
-----------

We digitized the speeches from December 12 into a [machine-readable format](https://github.com/joebrew/vilaweb/blob/master/inst/rmd/sesion_de_control/data/transcript.csv), and then used an algorithm based on the [AFINN library](http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010) (a dictionary of words with assigned sentimental polarity) to classify each sentence's average emotional direction. Certain words are categorized as positive or negative, with -5 being the most negative (for example, "bastard", "slut") and +5 being the most positive (for example, "superb" (magnífico) or "thrilled" (encantado)). The majority of words do not have an emotional weight ("to act", "administration", etc.) and are classified as 0. The average of a sentence's emotionally-weighted words constitute its positivity.

The below is an example of how the algorithm works on an ctual sentene from the speech. The sentence contained some negative words and some positive words, and was classified as neutral.

<table style="width:100%">
<tr>
    <td><img src="img/sanchez2.png" /></td>

</tr>
</table>
We ran the algorithm on the entire content of speeches, and analyzed trends in positivity. We also tabulated word frequencies and associations.

The results
-----------

### Are there differences in "polarity" (postivity-negativity) between the different politicians' speeches?

Yes.

Of the 6 speakers examined, 4 had generally "negative" speeches, whereas 2 had "positive" speeches.

    Error in eval(lhs, parent, parent): object 'transcript' not found

    Error in ggplot(data = plot_data, aes(x = person, y = sentiment, fill = person)): object 'plot_data' not found

The above should come as no surprise. Generally formal speeches are more positive than the critiques that follow them (hence Sánchez positive score), and Iglesias' support of the Sánchez government...

### Does polarity change when different subjects are discussed (specifically, Catalonia)?

Yes.

The previous chart oversimplifies very large changes in positivity throughout each person's interventions. For example, in the below, we can see wide swings in emotionality. About 1/4 through Sánchez's opening speech, for example, he hit his emotional low point. What was he talking about then? Catalonia.

    Error in eval(lhs, parent, parent): object 'transcript' not found

    Error in ggplot(data = plot_data, aes(x = sentence_number_all, y = sentiment, : object 'plot_data' not found

In fact, if we filter for only those sentences which contained references to Catalonia\*, the emotional polarity values take on a radically different form.

    Error in eval(lhs, parent, parent): object 'transcript' not found

    Error in ggplot(data = plot_data, aes(x = person, y = sentiment, fill = person)): object 'plot_data' not found

### What is the relationship between emotional polarity, violence and references to Catalonia?

Much of this emotional parity is attributible to violence-related words. For example, Albert Rivera used the words golpe (4), guerra (1), muertos (2), terrorismo (1), and violencia (1). Pablo Casado took on a similar tone, saying golpe/golpista (3), violencia (2), but adding more evocative, specific words like batasunización (1), balcanizar (1), and kale borroka (1). The irony of violence vocabulary is that once it is injected into the discourse, even those who deny it still end up talking about it. For example, even Joan Tardà used the words golpe (3) and violencia/violentos (2)

Such a high level of talk about violence is clearly not a reflection of reality - there has been no notable increase in violence in recent months, and the much discussed acts of the last week in which pro-independence protestors blocked roadways is arguably illegal, but certainly not violent. No, rather, the high frequency of violence-related words is an *anticipatory* violence, creating a mental frame primed to interpret the upcoming protests of December 21 as war-like.

As evidence of this interpretation, have a look at today's newspaper headlines:

<table style="width:100%">
<tr>
    <td><img src="img/elpais.jpg" /></td>
    <td><img src="img/abc.jpg" /></td>

</tr>
<tr>
    <td><img src="img/larazon.jpg" /></td>
    <td><img src="img/elmundo.jpg" /></td>

</tr>
</table>
El País headlines about security force increases and includes the line that the CDR (pro-independence protest groups) "llaman a dar batalla" (have called to battle). ABC uses the military words "comandos" and "asaltar" (to assault) to describe next week's planned protests. La Razón takes a similarly military-esque tone with the words "ejército" (army) and "guerrilla". Meanwhile, El Mundo front-pages an interview with former Spanish President Aznar saying that "the intervention in Catalonia should be total and without a time limit".

Just like in the congressional speeches, the newspapers are not covering real violence (of which there is not), but rather anticipatory violence.

#### Are there differences in lexical diversity between different politicians' speeches?

Lexical diversity is a measure of how many different words are used (ie, how often one repeats words). It is a reflection of how complex or advanced a speech is. For example, children have much lower lexical diversity than adults. Analysis has shown that lexical diversity has *decreased* among American politicians over time. A speech with high lexical diversity generally correlates with high intelligence and a complicated message. A speech with low lexical diversity among politicians does not generally reflect low intelligence (most politicians are smart), but rather an intentional effort to target a specific audience with simplistic, repetitive messaging.

TTR (Type-Token Ratio) is a measure of lexical diversity.

    Error in eval(expr, envir, enclos): object 'flattened_df' not found

![](figures/unnamed-chunk-9-1.png)![](figures/unnamed-chunk-9-2.png)

Percent below 60

![](figures/unnamed-chunk-10-1.png)

    Error in unique(transcript$person): object 'transcript' not found

    Error in eval(expr, envir, enclos): object 'people' not found

Lexical diversity
=================

    Error in textstat_lexdiv(my_dfm, measure = c("all"), log.base = 10): object 'my_dfm' not found

    Error in eval(lhs, parent, parent): object 'ld' not found

    Error in ggplot(data = ld, aes(x = person, y = value)): object 'ld' not found

    Error in ggplot(data = transcript, aes(x = sentence_number, y = sentiment_cumulative_average)): object 'transcript' not found

    Error in eval(lhs, parent, parent): object 'transcript' not found

    Error in eval(lhs, parent, parent): object 'transcript' not found

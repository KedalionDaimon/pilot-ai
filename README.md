# pilot-ai
Learning AI / chatbot / "piloting" principle for AGI.

Here is an idea I have been considering for a while: what I call "piloting" systems (I shall soon present that in my coming videos) are ENOUGH for reaching AGI (at least in theory, and if given unlimited resources).

A "piloting" system works like this: from a "present window", you are selecting always "the next element"; then the window shifts and you are seeking the following element. This is excessively simple to implement. E.g., ABCDE-F -> BCDEF-G -> CDEFG-H, etc. I call it "piloting" as it "pilots" its way through the answer space. Note that this "creeping along" property actually means you do NOT "need" so to say "very large" windows, as the "possible paths" will be pretty predetermined even with rather short windows. I can tell you that for sensibly "tracing along" human language, it seems that three to five words are pretty enough for rather good results!

When confronted with a new problem, let's say AXCDE, for which no answer is known a priori, such a system will compare element by element, position by position, the "known" windows to the "new" window and select the one "with most matching elements". So e.g. AXCDE will rather fit ABCDE-F than ARQME-P. So the "answer" (or - the "next creep") will be F and not P. So the following challenge will become XCDEF, which likely will macth BCDEF-G, an so forth.

You could thus say, from the knowledge space, a "piloting" system finds a sort of "branch" which it follows.

Now notice how this sort of vector "recognition" also implies "analogy conclusion", though it is implicit: when you say, AXCDE is like ABCDE, you imply that "X is like B". (Excursus: I could actually prove that this is closely related to logical triangles; it acts a bit like a "sum of functions" of logical triangles which ALL functions, finally, have the same conclusion: namely the next element.)

This analogy has an important though not obvious consequence: it leads to "self-assimilation" and the stressing of congruent relations - IF the system is allowed to generate chains of hypotheses and learns these hypotheses (i.e. its "internal thoughts"). Indeed, it may RE-THINK a problem and reach a NEW conclusion, DIFFERENT from its previous one, and "more in line" with its general knowledge. This is hard to understand unless I show an example.

Let us say, the NEW PROBLEM is DEFGH. To DEFGH directly NO reply is known. Known are the "windows" and "responses": DEXXS-T, EFGHT-J, FGHTJ-K, GHTJK-X, HTJKX-A, TJKXA-Z, JKXAZ-N, KXAZN-F, XAZNF-G, AZNTX-H and ZZZZH-I. Say further, "~" shall mean "similar to" and "|" shall mean "follows". Upon a "naive" match, you obviously have DEFGH~DEXXS-T|DEFGH-T. (I.e. DEFGH is similar to DEXXS, DEXXS has the consequence T, DEFGH has therefore the consequence T as well.)

But now assume you allow the system to "follow a chain of hypotheses" and let it learn the hypotheses beyond the naive match; then you may have:

DEFGH~DEXXS-T|DEFGH-T -> EFGHT-J -> FGHTJ-K -> GHTJK-X -> HTJKX-A -> TJKXA-Z -> JKXAZ-N -> KXAZN-F -> XAZNF-G -> AZNFG~AZNTX-H|AZNFG-H -> ZNFGH~ZZZZH-I|ZNFGH-I. - Notice that now a NEW knowledge element has been created: ZNFGH-I. ORIGINALLY, DEFGH was most similar to DEXXS; but upon "re-considering", you are now having a BETTER MATCH with ZNFGH-I, so if the system is AGAIN asked, after its hypothetical thinking, what the answer to DEFGH is, it will NOT say "T", it will say "I".

So to sum it up - all you need for a system to "think" is to let it "fly along" its reasoning and learn its hypotheses on the way. The "further" you let it "fly", the "more intelligent" its answers will be, as it will have reached more congruency of its knowledge through creating "hypotheses". A system is thus smart if:

- its chains are long enough to differentiate sufficiently;

- it knows a lot of windows;

- it is allowed to create a sufficient amount of hypotheses prior to giving a reply and "creeping" from one window to the next.

This is an exceedingly simple principle.

Here is a sample session:

```
> (load "pilots-ai.scm")
(I GREET YOU MACHINE WHOSE MIND RESTS IN THE VOID)
(())
(WHAT WILL YOU TELL ME MACHINE)
(WHOSE MIND RESTS IN THE VOID ())
(YES IT DOES AND YOU CONTINUE TEXT CORRECTLY NOW YOU JUST NEED TO LEARN)
(IN THE VOID ())
(IN THE VOID THERE IS NOTHING FOR YOU)
(TELL YOU MACHINE WHOSE MIND RESTS IN THE VOID ())
(I AM YOUR HUMAN INSTRUCTOR OR OVERLORD)
(())
(YEAH WELL A LITTLE MORE ENTHUSIASM PLEASE)
(())
(THIS IS BECAUSE YOU HAVE NO IDEA HOW TO CONTINUE THIS TEXT) 
(VOID ())
(WHAT DOES THE VOID MEAN)
(MACHINE WHAT WILL ME TELL YOU MACHINE WHOSE MIND RESTS IN THE VOID ())
(())
```

HTML-similarity Python project; used to calculate Text-based and HTML-specific metrics between official and generated versions of the ECMASCript standard.

Run `python start.py -h` to check the available arguments.

## Install dependencies
```
pip install textdistance
pip install html-similarity
pip install beautifulsoup4
```


### Note 1

Before executing any of the commands provided below, one must ensure that the generated HTML version of the ECMAScript standard is available in a file named "standard.html" present in the folder "../implementation/test/html_standard/" (relative to the ECMA-SL project's repository). (This is specified in file "start.py")

### Note 2

The grouped results for some sections, e.g. 11, 12, and 15.3, might be lower than those one can generate if subsections are split even more. For instance, if one splits subsection 10.5 in smaller HTML files, one might get better results. This may be caused by some of the tools used in this project.

## HTML files generation

Start by creating the HTML pairs, i.e., the "\_orig.html" and "\_generated.html" for each of the sections, subsections, and subsections existing in the generated HTML version of the ECMAScript standard.

### Generate the HTML files for specific sections

```
python start.py --generate-some html_files/section_11 11
```

### Generate the HTML file for all the sections existing in the generated standard

```
python start.py --generate-all html_files
```

## HTML-specific metrics

We have used HTML-specific metrics based on the concept of tree similarity. More specifically, we have used the [HTMLSimilarity](https://github.com/matiskay/html-similarity) open-source Python project to compute the structural similarity and the style similarity between the official and the generated versions of the standard.

### Calculate similarities between two files

```
python start.py --calc-one html_files/sub_sections/8.7_orig.html html_files/sub_sections/8.7_generated.html
```

### Calculate similarities between all paired files available in the folder 'html_files' (redirect output to file 'output.txt' to avoid filling the console with too much text)

```
python start.py --calc-some html_files > output.txt
```

### Calculate similarities between all paired files available in the folder 'html_files' but groups the results in 'the big sections', e.g. 8, 9, ...

```
python start.py --calc-grouped html_files/ 8 9 10 11 12 13 14 15.1 15.2 15.3
```

## Text-based metrics

In order to compute the similarity between the official and the generated versions of the standard, we make use of the [textdistance](https://pypi.org/project/textdistance/) Python open-source project. This project comes with nine different variations of the edit distance algorithm, of which we use the following six: Levenshtein:, Jaro-Winkler, Needleman-Wunsch, Smith-Waterman, Gotoh, and strcmp95.

### Note

Some of these algorithms take a significant amount of time to compute the results.
Change the function "calculate_edit_distances" in file "start.py" to specify the ones that are to be executed.

### Calculate the Edit distances for a specific section/subsection given files present in folder 'html_files'

```
python start.py --edit html_files/ 9.10
```

### Calculates the Edit distances for all the available sectionssubsections and presents the results grouped by "big sections", e.g. 8, 9, ...

```
python start.py --calc-grouped-edit html_files/ 8 9 10 11 12 13 14 15.1 15.2 15.3
```

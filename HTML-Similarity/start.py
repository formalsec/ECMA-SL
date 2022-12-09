# from main import process_sections
import glob
import argparse
import importlib
import requests
import os
from functools import partial
import edit
import multiprocessing as mp

URL = "https://262.ecma-international.org/5.1"
FILE = "../implementation/test/html_standard/standard.html"
# FILE = "input/8.12.5.html"
# FILE_ORIG = "input/orig_8.12.5.html"


class GroupedSimilarities:
    def __init__(self, nr_lines: int, style: float, structural: float, joint: float):
        self.nr_lines = nr_lines
        self.style = style
        self.structural = structural
        self.joint = joint

    def __repr__(self):
        return f'Nr. Lines: {self.nr_lines}\n\tStyle: {self.style}\n\tStructural: {self.structural}\n\tJoint: {self.joint}\n'


def dump_sections_to_files(dest_folder: str, section_id: str, orig_section: str, std_section: str):
    if not os.path.exists(dest_folder):
        os.mkdir(dest_folder)

    with open(f"{dest_folder}/{section_id}_orig.html", "w") as file:
        file.write(orig_section)

    with open(f"{dest_folder}/{section_id}_generated.html", "w") as file:
        file.write(std_section)


def generate_html_files(dest_folder: str):
    request_ecma = requests.get(URL)

    # Opening the html file
    HTMLFile = open(FILE, "r")
    # Reading the file
    index = HTMLFile.read()

    MainModule.process_sections(
        'output.txt', index, request_ecma.content, partial(dump_sections_to_files, dest_folder))


def generate_html_files_groups(dest_folder: str, groups: list):
    request_ecma = requests.get(URL)

    # Opening the html file
    HTMLFile = open(FILE, "r")
    # Reading the file
    index = HTMLFile.read()

    MainModule.process_sections_grouped(
        groups, index, request_ecma.content, partial(dump_sections_to_files, dest_folder))


def calculate_similarities(source_folder: str):
    originals = sorted(glob.glob(f'{source_folder}/*_orig.html'))
    generated = sorted(glob.glob(f'{source_folder}/*_generated.html'))

    combined = zip(originals, generated)
    all_similarities = []

    for pair in combined:
        # Opening the html files
        orig_contents, orig_nr_lines, orig_size = MainModule.open_file(
            pair[0])
        generated_contents, generated_nr_lines, generated_size = MainModule.open_file(
            pair[1])

        similarities = MainModule.calc_similarities(
            orig_contents, generated_contents)

        last_slash_idx = pair[0].rfind('/')
        all_similarities.append(
            (pair[0][last_slash_idx + 1:-10],) + similarities + (orig_nr_lines, generated_nr_lines, orig_size, generated_size))

    return all_similarities


def calculate_grouped_similarities(source_folder: str, sections: list) -> dict:
    all_similarities = calculate_similarities(source_folder)
    grouped_sections = {}
    for section in sections:
        grouped_sections[section] = GroupedSimilarities(0, 1.0, 1.0, 1.0)

    for similarities in all_similarities:
        for section in sections:
            sec = f'sec-{section}'
            if sec == similarities[0][0:len(sec)]:
                grouped_sections[section] = update_average(
                    grouped_sections[section], similarities)
                break

    return grouped_sections


def calculate_edit_distances(source_folder: str, sections: list) -> dict:
    originals = sorted(glob.glob(f'{source_folder}/*_orig.html'))
    generated = sorted(glob.glob(f'{source_folder}/*_generated.html'))

    combined = zip(originals, generated)

    grouped_sections = {}

    for pair in combined:
        last_slash_idx = pair[0].rfind('/')
        sec_id = pair[0][last_slash_idx + 5:-10]

        try:
            sections.index(sec_id)
        except ValueError:
            continue

        # Open the html files
        orig_contents, orig_nr_lines, orig_size = MainModule.open_file(pair[0])
        generated_contents, generated_nr_lines, generated_size = MainModule.open_file(pair[1])

        # Calc edit distances
        # Parallelize processing
        algorithms = [
            # "Hamming",
            # "MLIPNS",
            # "Levenshtein",
            # "Damerau-Levenshtein",
            "Jaro-Winkler",
            # "Strcmp95",
            # "Needleman-Wunsch",
            # "Gotoh",
            # "Smith–Waterman"
        ]
        pool = mp.Pool(mp.cpu_count())

        result_objects = [pool.apply_async(edit.calc_edit_distance, args=(
            algorithm, orig_contents, generated_contents)) for algorithm in algorithms]
        # result_objects is a list of pool.ApplyResult objects
        distances = [r.get() for r in result_objects]

        pool.close()
        pool.join()
        # distances = []
        # distances.append(edit.calc_edit_distance(
        #     "Hamming", orig_contents, generated_contents))
        # print("Hamming")
        # distances.append(edit.calc_edit_distance(
        #     "MLIPNS", orig_contents, generated_contents))
        # print("MLIPNS")
        # distances.append(edit.calc_edit_distance(
        #     "Levenshtein", orig_contents, generated_contents))
        # print("Levenshtein")
        # distances.append(edit.calc_edit_distance(
        #     "Damerau-Levenshtein", orig_contents, generated_contents))
        # print("Damerau-Levenshtein")
        # distances.append(edit.calc_edit_distance(
        #     "Jaro-Winkler", orig_contents, generated_contents))
        # print("Jaro-Winkler")
        # distances.append(edit.calc_edit_distance(
        #     "Strcmp95", orig_contents, generated_contents))
        # print("Strcmp95")
        # distances.append(edit.calc_edit_distance(
        #     "Needleman-Wunsch", orig_contents, generated_contents))
        # print("Needleman-Wunsch")
        # distances.append(edit.calc_edit_distance(
        #     "Gotoh", orig_contents, generated_contents))
        # print("Gotoh")
        # distances.append(edit.calc_edit_distance(
        #     "Smith–Waterman", orig_contents, generated_contents))
        # print("Smith–Waterman")

        grouped_sections[sec_id] = edit.EditDistance(
            orig_size, generated_size, distances)

    return grouped_sections


def calculate_grouped_edit_distances(source_folder: str, sections: list):
    originals = sorted(glob.glob(f'{source_folder}/*_orig.html'))

    inner_sections = []

    # find the inner sections that correspond to the sections given as argument
    # these inner section are passed to the "calculate_edit_distances" function
    for file in originals:
        last_slash_idx = file.rfind('/')
        sec_id = file[last_slash_idx + 5:-10]

        for sec in sections:
            if sec_id[0:len(sec)] == sec:
                inner_sections.append(sec_id)

    calculated_sections = calculate_edit_distances(source_folder, inner_sections)

    grouped_sections = {}
    for section in sections:
        grouped_sections[section] = edit.EditDistance(0, 0, [])
        for algorithm in edit.ALGORITHMS:  # only the keys are of interest here
            grouped_sections[section].distances.append(
                # only working for normalized_distance
                edit.DistanceStats(algorithm, None, None, None, 0))

    for sec_id, calc_obj in calculated_sections.items():
        for section in sections:
            if section == sec_id[0:len(section)]:
                grouped_sections[section] = edit.sum_chars_and_distances(
                    grouped_sections[section], calc_obj)
                break

    # calculate averages
    for grouped_section in grouped_sections.values():
        edit.calc_distances_avg(grouped_section)

    return grouped_sections


def update_average(grouped_similarities_obj: GroupedSimilarities, similarities: tuple) -> tuple:
    # Calc the average between the paired files (orig.html and generated.html)
    pair_avg_lines = round((similarities[4] + similarities[5]) / 2)
    # pair_avg_style = (similarities[1] * similarities[4] + similarities[1]
    #                   * similarities[5])/(similarities[4] + similarities[5])
    # pair_avg_structural = (similarities[2] * similarities[4] + similarities[1]
    #                        * similarities[5])/(similarities[4] + similarities[5])
    # pair_avg_joint = (similarities[3] * similarities[4] + similarities[1]
    #                   * similarities[5])/(similarities[4] + similarities[5])

    total_avg_style = (
        grouped_similarities_obj.style * grouped_similarities_obj.nr_lines +
        # pair_avg_style * pair_avg_lines
        similarities[1] * pair_avg_lines
    ) / (grouped_similarities_obj.nr_lines + pair_avg_lines)

    total_avg_structural = (
        grouped_similarities_obj.structural * grouped_similarities_obj.nr_lines +
        # pair_avg_structural * pair_avg_lines
        similarities[2] * pair_avg_lines
    ) / (grouped_similarities_obj.nr_lines + pair_avg_lines)

    total_avg_joint = (
        grouped_similarities_obj.joint * grouped_similarities_obj.nr_lines +
        # pair_avg_joint * pair_avg_lines
        similarities[3] * pair_avg_lines
    ) / (grouped_similarities_obj.nr_lines + pair_avg_lines)

    return GroupedSimilarities(grouped_similarities_obj.nr_lines + pair_avg_lines, total_avg_style, total_avg_structural, total_avg_joint)


MainModule = importlib.import_module("main")

# create parser
parser = argparse.ArgumentParser()

# add arguments to the parser
parser.add_argument('--calc-one', nargs=2, metavar=('file1', 'file2'), dest='calc_one',
                    help='Calculates HTML similarities between the two files')
parser.add_argument('--calc-some', action='store', metavar='origin_folder', dest='calc_some',
                    help='Calculates the HTML similarities between all the paired files ("_orig.html" and "_generated.html")\
                          present in the folder "origin_folder".')
parser.add_argument('--calc-grouped', nargs='+', metavar=('origin_folder', 'sections'), dest='calc_grouped',
                    help='Calculates the HTML similarities between all the paired files ("_orig.html" and "_generated.html")\
                          present in the folder "origin_folder". Groups the results in the big sections using the following formula:\
                          (nr.lines_origin * similarity + nr.lines_generated * similarity) / (nr.lines_origin + nr.lines_generated)')
parser.add_argument('--generate-some', nargs='+', metavar=('dest_folder', 'sections'), dest='generate_some',
                    help='Generates HTML files for each provided section. Subsections are grouped in the same parent section.\
                          First argument is the destination folder.')
parser.add_argument('--generate-all', action='store', metavar='dest_folder', dest='generate_all',
                    help='Generates the HTML files for all the sections available and stores files in the folder "dest_folder"')
parser.add_argument('--edit', nargs='+', metavar=('origin_folder', 'sections'), dest='edit',
                    help='TODO')
parser.add_argument('--calc-grouped-edit', nargs='+', metavar=('origin_folder', 'sections'), dest='calc_grouped_edit',
                    help='TODO')

# parse the arguments
args = parser.parse_args()

if args.calc_one:
    print(args.calc_one[0])
    print(MainModule.process_section_files(args.calc_one[0], args.calc_one[1]))

elif args.calc_some:
    if not os.path.exists(args.calc_some):
        raise ValueError(f'Folder "{args.calc_some}" doesn\'t exist')
    all_similarities = calculate_similarities(args.calc_some)
    for similarities in all_similarities:
        print(
            f'{similarities[0]}\n\tStyle: {similarities[1]}\n\tStructural: {similarities[2]}\n\tJoint: ' +
            f'{similarities[3]}\n\tNr lines original: {similarities[4]}\n\tNr lines generated: {similarities[5]}\n')

elif args.calc_grouped:
    folder, *sections = args.calc_grouped
    if not os.path.exists(folder):
        raise ValueError(f'Folder "{folder}" doesn\'t exist')
    print(calculate_grouped_similarities(folder, sections))

elif args.generate_all:
    generate_html_files(args.generate_all)

elif args.generate_some:
    folder, *sections = args.generate_some
    generate_html_files_groups(folder, sections)

elif args.edit:
    folder, *sections = args.edit
    if not os.path.exists(folder):
        raise ValueError(f'Folder "{folder}" doesn\'t exist')
    print(calculate_edit_distances(folder, sections))

elif args.calc_grouped_edit:
    folder, *sections = args.calc_grouped_edit
    if not os.path.exists(folder):
        raise ValueError(f'Folder "{folder}" doesn\'t exist')
    print(calculate_grouped_edit_distances(folder, sections))

else:
    parser.print_help()

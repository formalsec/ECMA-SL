from html_similarity import style_similarity, structural_similarity, similarity
from typing import List
from bs4 import BeautifulSoup, element
import copy


class SectionSimilarities:
    def __init__(self, section_id: str, style: float, structural: float, joint: float):
        self.section_id = section_id
        self.style_similarity = style
        self.structural_similarity = structural
        self.joint_similarity = joint

    def toString(self):
        return (f"{self.section_id}\n\tStyle Similarity: {self.style_similarity}\n\tStructural Similarity: " +
                f"{self.structural_similarity}\n\tJoint Similarity: {self.joint_similarity}\n\n")


only_these = [
    # "sec-11.1.4"
    # "sec-8.12.5"
]
exclude_these = [
    # "sec-11",
    # "sec-11.1",
    # "sec-11.2",
    # "sec-11.3",
    # "sec-11.4",
    # "sec-11.6",
    # "sec-11.7",
    # "sec-11.8",
    # "sec-11.9",
    # "sec-11.13",
    # "sec-12",
    # "sec-12.6"
]


# Data structure that holds the lowest calculated similarity values.
# Each value is a pair where the first element is the name of the section
# and the second element is the calculated similarity value.
STATISTICS = {
    "style": ("", 1),
    "structural": ("", 1),
    "joint": ("", 1)
}


def calc_similarities(first_html_str: str, second_html_str: str) -> str:
    """
    Executes the similarity calculations for the provided HTML strings.

    Returns a triple containing the three calculated similarities:
        style similarity, structural similarity and joint similarity
    """
    res_style_similarity = style_similarity(
        first_html_str, second_html_str)
    res_structural_similarity = structural_similarity(
        first_html_str, second_html_str)
    res_joint_similarity = similarity(first_html_str, second_html_str)

    return (res_style_similarity, res_structural_similarity, res_joint_similarity)


def update_statistics(section_similarities_obj: SectionSimilarities):
    """
    Compares similarities of the provided argument against the previously saved lowest values.

    Update saved values if an argument similarity value is lower than the one previously saved.
    """
    if (section_similarities_obj.style_similarity <= STATISTICS["style"][1]):
        STATISTICS["style"] = (
            section_similarities_obj.section_id, section_similarities_obj.style_similarity)

    if (section_similarities_obj.structural_similarity <= STATISTICS["structural"][1]):
        STATISTICS["structural"] = (
            section_similarities_obj.section_id, section_similarities_obj.structural_similarity)

    if (section_similarities_obj.joint_similarity <= STATISTICS["joint"][1]):
        STATISTICS["joint"] = (
            section_similarities_obj.section_id, section_similarities_obj.joint_similarity)


def filter_contents(section_tag: BeautifulSoup):
    """
    Elements between and including heading "Syntax" and heading "Semantics" are removed
    """
    section_tag_copy = copy.copy(section_tag)
    syntax = section_tag_copy.find(
        lambda tag: tag.name == 'h2' and tag.string == 'Syntax',
        recursive=False)
    semantics = section_tag_copy.find(
        lambda tag: tag.name == 'h2' and tag.string == 'Semantics',
        recursive=False)

    for section_to_extract in section_tag_copy.find_all('section'):
        section_to_extract.extract()

    in_between = False
    for child in section_tag_copy.contents:
        if child == syntax:
            in_between = True

        if in_between:
            child.extract()

        if child == semantics:
            in_between = False

    return section_tag_copy


def write_similarities_to_file(filename: str, section_similarities_objs: List[SectionSimilarities]):
    with open(filename, 'w') as file:
        for similarities in section_similarities_objs:
            file.write(similarities.toString())


def process_sections(filename: str, std_contents: str, ecma_contents: bytes, func=None):
    # Creating a BeautifulSoup object and specifying the parser
    soup_std = BeautifulSoup(std_contents, 'lxml')
    soup_ecma = BeautifulSoup(ecma_contents, 'lxml')

    all_id_sections = soup_std.select("section[id]")
    section_similarities_list = []

    for id_section in all_id_sections:
        section_id = id_section['id']

        # filter sections
        if len(only_these) == 0:
            if section_id in exclude_these:
                continue
        elif not (section_id in only_these):
            continue

        encoded_id = section_id.replace(".", "\.")
        orig_id_section = soup_ecma.select_one(f'#{encoded_id}')

        # Apply any filter to the section contents
        filtered_orig_section = filter_contents(orig_id_section)
        filtered_std_section = filter_contents(id_section)

        # Calc similarities
        similarities = calc_similarities(
            str(filtered_orig_section), str(filtered_std_section))

        # Create SectionSimilarities object
        section_similarities_obj = SectionSimilarities(
            section_id, similarities[0], similarities[1], similarities[2])

        # Compare results
        update_statistics(section_similarities_obj)

        # Append to list of SectionSimilarities
        section_similarities_list.append(section_similarities_obj)

        # Execute provided function
        if callable(func):
            func(section_id, filtered_orig_section.prettify(),
                 id_section.prettify())

        print(section_id, end=" ", flush=True)

    # Write to file
    write_similarities_to_file(filename, section_similarities_list)

    print("\n")
    # print(lowest_style_similarity, lowest_structural_similarity,
    #       lowest_similarity, sep="\n")
    print(STATISTICS)


def process_sections_grouped(groups: list, std_contents: str, ecma_contents: bytes, func=None):
    # Creating a BeautifulSoup object and specifying the parser
    soup_std = BeautifulSoup(std_contents, 'lxml')
    soup_ecma = BeautifulSoup(ecma_contents, 'lxml')

    for group in groups:
        encoded_group = group.replace(".", "\.")
        all_group_sections = soup_std.select(
            f"section[id^=sec-{encoded_group}]")
        processed_sections = []
        orig_group_subsections = []
        std_group_subsections = []

        for section in all_group_sections:
            section_id = section['id']

            # Check if a parent section was already processed.
            # A parent section previously processed already contains this subsection in its contents
            exists = False
            for processed_section in processed_sections:
                length = len(processed_section)
                if section_id[0:length] == processed_section:
                    exists = True
                    break

            if exists:
                break

            # filter sections
            if len(only_these) == 0:
                if section_id in exclude_these:
                    continue
            elif not (section_id in only_these):
                continue

            encoded_id = section_id.replace(".", "\.")
            orig_section = soup_ecma.select_one(f'#{encoded_id}')

            # Apply any filter to the section contents
            filtered_orig_section = filter_contents(orig_section)
            filtered_std_section = filter_contents(section)

            processed_sections.append(section_id)
            orig_group_subsections.append(filtered_orig_section)
            # std_group_subsections.append(section)
            std_group_subsections.append(filtered_std_section)

        orig_main_section_soup = BeautifulSoup(
            f'<section"></section>', 'lxml')
        orig_main_section_soup.section.extend(orig_group_subsections)

        std_main_section_soup = BeautifulSoup(
            f'<section"></section>', 'lxml')
        std_main_section_soup.section.extend(std_group_subsections)

        # Execute provided function
        if callable(func):
            func(group, orig_main_section_soup.prettify(),
                 std_main_section_soup.prettify())


def open_file(file_name: str) -> tuple:
    # Open file
    file = open(file_name, 'r')
    # Read file contents and count number of lines
    nr_lines = 0
    contents = ''
    for line in file:
        nr_lines += 1
        contents += line

    # Return file contents, number of lines in file and number of characters
    return (contents, nr_lines, len(contents))


def process_section_files(orig_file: str, generated_file: str) -> tuple:
    # Opening the html files
    orig_contents, orig_nr_lines, orig_size = open_file(orig_file)
    generated_contents, generated_nr_lines, generated_size = open_file(
        generated_file)

    # Calc similarities
    similarities = calc_similarities(orig_contents, generated_contents)

    return similarities + (orig_nr_lines, generated_nr_lines, orig_size, generated_size)

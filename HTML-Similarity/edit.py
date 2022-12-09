import textdistance


class DistanceStats:
    def __init__(self, algorithm: str, distance: int, similarity: int, normalized_distance: float, normalized_similarity: float):
        self.algorithm = algorithm
        self.distance = distance
        self.similarity = similarity
        self.normalized_distance = normalized_distance
        self.normalized_similarity = normalized_similarity

    def __repr__(self):
        return (f'Algorithm: {self.algorithm}; Distance: {self.distance}; Similarity: {self.similarity}; Normalized Distance: {self.normalized_distance}; ' +
                f'Normalized Similarity: {self.normalized_similarity}')


class EditDistance:
    def __init__(self, nr_chars_original: int, nr_chars_generated: int, distances: list):
        self.nr_chars_original = nr_chars_original
        self.nr_chars_generated = nr_chars_generated
        self.distances = distances

    def __repr__(self):
        return f'Nr. characters original: {self.nr_chars_original}\nNr. characters generated: {self.nr_chars_generated}\nDistances: {self.distances}\n'


ALGORITHMS = {
    'Hamming': textdistance.hamming,
    'MLIPNS': textdistance.mlipns,
    'Levenshtein': textdistance.levenshtein,
    'Damerau-Levenshtein': textdistance.damerau_levenshtein,
    'Jaro-Winkler': textdistance.jaro_winkler,
    'Strcmp95': textdistance.strcmp95,
    'Needleman-Wunsch': textdistance.needleman_wunsch,
    'Gotoh': textdistance.gotoh,
    'Smith–Waterman': textdistance.smith_waterman
}


def calc_edit_distance(algorithm: str, first_text: str, second_text: str) -> DistanceStats:
    algorithm_function = ALGORITHMS.get(algorithm, None)

    if algorithm_function is None:
        raise ValueError

    return DistanceStats(
        algorithm,
        # algorithm_function.distance(first_text, second_text),
        None,
        # algorithm_function.similarity(first_text, second_text),
        None,
        # algorithm_function.normalized_distance(first_text, second_text),
        None,
        algorithm_function.normalized_similarity(first_text, second_text)
    )


def sum_chars_and_distances(grouped_section: EditDistance, subsection: EditDistance) -> EditDistance:
    avg_nr_chars = round((subsection.nr_chars_generated + subsection.nr_chars_original) / 2)

    for distance in subsection.distances:
        grouped_section.nr_chars_generated += subsection.nr_chars_generated
        grouped_section.nr_chars_original += subsection.nr_chars_original

        for d in grouped_section.distances:
            if d.algorithm == distance.algorithm:
                # d.distance += distance.distance * avg_nr_chars
                # d.similarity += distance.normalized_similarity * avg_nr_chars
                # d.normalized_distance += distance.normalized_distance * avg_nr_chars
                d.normalized_similarity += distance.normalized_similarity * avg_nr_chars
                break

    return grouped_section


def calc_distances_avg(grouped_section: EditDistance) -> EditDistance:
    avg_nr_lines = round((grouped_section.nr_chars_original + grouped_section.nr_chars_generated) / 2)
    for distance in grouped_section.distances:
        distance.normalized_similarity /= avg_nr_lines

    return grouped_section

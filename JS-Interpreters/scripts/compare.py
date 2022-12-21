#!/usr/bin/env python3
from __future__ import annotations

import os
import sys
import glob
import argparse

OK="_OK_"
FAIL="**FAIL**"
ERROR="**ERROR**"
UNSUPPORTED="**UNSUPPORTED**"

OUTCOMES = [OK, FAIL, ERROR, UNSUPPORTED]

def get_parser():
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--broke",
        dest="broke",
        action="store_true",
        default=False,
        help="show tests that we broke"
    )

    parser.add_argument(
        "--all",
        dest="all",
        action="store_true",
        default=False,
        help="show all not OK tests"
    )

    parser.add_argument(
        "--fail",
        dest="fail",
        action="store_true",
        default=False,
        help="show all failing tests"
    )

    parser.add_argument(
        "--error",
        dest="error",
        action="store_true",
        default=False,
        help="show all erroring tests"
    )

    parser.add_argument(
        "--unsupported",
        dest="unsupported",
        action="store_true",
        default=False,
        help="show all unsupported tests"
    )

    parser.add_argument(
        "--export",
        dest="export",
        action="store_true",
        default=False,
        help="export only filenames"
    )

    parser.add_argument(
        "--save-statistics",
        dest="statistics",
        action="store_true",
        default=False,
        help="save language and built-in statistics as csvs"
    )

    parser.add_argument("file", type=argparse.FileType("r"), nargs=2)

    return parser

def parse(argv):
    parser = get_parser()
    return parser.parse_args(argv)

def to_dict(lines : list) -> dict:
    d = {}
    d[OK], d[FAIL], d[ERROR], d[UNSUPPORTED] = 0, 0, 0, 0
    for line in lines:
        line = line.strip().split("|")
        key = line[0].replace("../", "").strip()
        res = line[1].strip()
        if not (res in OUTCOMES):
            print("Corrupted results file.")
            return {}
        # even if key is repeated same test cannot have a different result
        # so we can ignore
        if key in d:
            continue
        d[res] += 1
        d[key] = res
    return d

def parse_file(f) -> dict:
    prefix = "test/test262/tests/"
    lines = list(filter(
        lambda l: l.startswith(f"../{prefix}") or l.startswith(prefix),
        f.readlines()
    ))
    return to_dict(lines)

def find_cmp(d1 : dict, d2 : dict, cmp, print_func) -> None:
    for k, v in d1.items():
        if not (k in d2):
            d2[k] = "**NOT_EXECUTED**"
        if cmp(d2, k, v):
            print_func(d2, k, v)

def save_csv(d : dict, output_file : str) -> None:
    csv_d = {}
    prefix = "test/test262/tests/"
    for k, v in d.items():
        if k in OUTCOMES:
            continue
        k = k.replace(prefix, "").split("/")
        if k[0] not in csv_d:
            csv_d[k[0]] = {}
        category = csv_d[k[0]]
        if k[1] not in category:
            category[k[1]] = {}
            category[k[1]][OK] = 0
            category[k[1]][FAIL] = 0
            category[k[1]][ERROR] = 0
            category[k[1]][UNSUPPORTED] = 0
        category[k[1]][v] += 1

    language = list(csv_d["language"].items())
    language.sort()
    with open(f"{output_file}_language.csv", "w") as fd:
        for k, v in language:
            fd.write(f"{k}, {v[OK]}, {v[FAIL]}, {v[ERROR]}, {v[UNSUPPORTED]}\n")

    built_ins = list(csv_d["built-ins"].items())
    built_ins.sort()
    with open(f"{output_file}_built-ins.csv", "w") as fd:
        for k, v in built_ins:
            fd.write(f"{k}, {v[OK]}, {v[FAIL]}, {v[ERROR]}, {v[UNSUPPORTED]}\n")


def main(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    args = parse(argv)

    d1 = parse_file(args.file[0])
    if d1 == {}:
        print("Failed to parse file1")
        return 1

    # We use # to print comments to the stdout
    if not args.export:
        print(
            "# Stats File 1: OK: {} FAIL: {} ERROR: {} UNSUPPORTED: {} TOTAL: {}".format(
            d1[OK], d1[FAIL], d1[ERROR], d1[UNSUPPORTED], len(d1)-4
        ))

    d2 = parse_file(args.file[1])
    if d2 == {}:
        print("Failed to parse file2")
        return 1
    if not args.export:
        print(
            "# Stats File 2: OK: {} FAIL: {} ERROR: {} UNSUPPORTED: {} TOTAL: {}".format(
            d2[OK], d2[FAIL], d2[ERROR], d2[UNSUPPORTED], len(d2)-4
        ))

    def print_func(d : dict, k : str, v : str):
        if args.export:
            print(k)
        else:
            print(f"{k} {v} vs {d[k]}")

    if args.broke:
        if not args.export:
            print("# We broke")
        cmp_func = lambda d, k, v: v != OK and d[k] == OK
        find_cmp(d1, d2, cmp_func, print_func)

    if args.all:
        if not args.export:
            print("# Not passing")
        cmp_func = lambda d, k, v: v != OK and v in OUTCOMES
        find_cmp(d1, d2, cmp_func, print_func)

    if args.fail:
        if not args.export:
            print("# Failing")
        cmp_func = lambda d, k, v: v == FAIL
        find_cmp(d1, d2, cmp_func, print_func)

    if args.error:
        if not args.export:
            print("# Erroring")
        cmp_func = lambda d, k, v: v == ERROR
        find_cmp(d1, d2, cmp_func, print_func)

    if args.unsupported:
        if not args.export:
            print("# Unsupported")
        cmp_func = lambda d, k, v: v == UNSUPPORTED
        find_cmp(d1, d2, cmp_func, print_func)

    if args.statistics:
        save_csv(d1, "d1")
        save_csv(d2, "d2")

    return 0

if __name__ == "__main__":
    sys.exit(main())

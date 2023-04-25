#!/usr/bin/env python3
import os
import sys
import json
import glob
import time
import argparse
import subprocess

from concurrent.futures import ThreadPoolExecutor

def read_json(file):
    with open(file, "r") as fd:
        return json.load(fd)

def parse_report(file):
    try:
        return read_json(file)
    except json.JSONDecodeError:
        return { "errors" : 1, "unknowns" : 0, "time_analysis" : "0.0" }

def run_test(file):
    print(file, end="...\n")
    f = "".join(file.split(".")[0:-1])
    output_dir = f + "_output"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    start = time.time()
    try:
        subprocess.run(["ecma-se", "-o", output_dir, file], check=True,
                       timeout=900)
        report = parse_report(os.path.join(output_dir, "report.json"))
        return dict(spec=report["errors"] == 0,
                    time=float(report["time_analysis"]))
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
        return dict(spec=False, time=time.time() - start)

def get_parser():
    parser = argparse.ArgumentParser()

    parser.add_argument("-j", "--jobs", dest="jobs", type=int, default=1)
    parser.add_argument("test_dir")

    return parser

def parse(argv):
    return get_parser().parse_args(argv)

def main(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    args = parse(argv)

    if not os.path.isdir(args.test_dir):
        print(f"\"{args.test_dir}\" is not a directory!", file=sys.stderr)
        return 1

    tests = list(glob.glob(os.path.join(args.test_dir, "**", "*.cesl"), recursive=True))
    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        results = list(pool.map(run_test, tests))

    oks = list(filter(lambda d: d["spec"], results))
    print("Total:", len(oks), "/", len(results), "Passed",
          f"{sum(map(lambda d: d['time'], results))}s")
    return 0

if __name__ == "__main__":
    sys.exit(main())

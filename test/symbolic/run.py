#!/usr/bin/env python3
import os
import sys
import json
import glob
import time
import argparse
import subprocess
import pandas

from concurrent.futures import ThreadPoolExecutor


def create_results_dict(f, paths = 0, errors = 0, 
                        unknowns = 0, t = 0.0, 
                        time_solver = 0.0, status = "unknown", 
                        successful = False):
    return {
        "file" : f,
        "paths" : paths,
        "errors" : errors, 
        "unknowns" : unknowns, 
        "time_analysis" : t ,
        "time_solver" :  time_solver,
        "status" : status,
        "success" : successful
    }


def read_json(file):
    with open(file, "r") as fd:
        return json.load(fd)


def is_successful(report, is_bug):
    return report["errors"] != 0 if is_bug else report["errors"] == 0


def parse_report(file, output_file, is_bug):
    try:
        report = read_json(output_file)
        report["status"] = "completed"
        report["success"] = is_successful(report, is_bug)
        report["time_analysis"] = float(report["time_analysis"])
        report["time_solver"] = float(report["time_solver"])
        
        return report
    except json.JSONDecodeError:
        return create_results_dict(file, errors = 1)
    except FileNotFoundError:
        return create_results_dict(file, errors = 1, status = "crash")


def process_results(report, bug, test_name, test_dir):
    table = {
        "Category" : [],
        "Falses": [],
        "Trues": [],
        "Timeouts" : [],
        "Crashes" : [],
        "Time" : [],
        "Solver" : [],
        "Paths" : []
    }

    test_success = (report["errors"] != 0) if bug else (report["errors"] == 0)

    return dict(spec=test_success, time=float(report["time_analysis"]))


def run_test(file):
    print(file, end="...\n")
    test_dir = (os.sep).join(file.split(os.sep)[0:-1])
    f = "".join(file.split(".")[0:-1])
    
    bug = True if f.find("_bug") != -1 else False

    output_dir = f + "_output"
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    start = time.time()
    try:
        subprocess.run(["ecma-se", "-o", output_dir, file], check=True,
                       timeout=900)
        report = parse_report(f, os.path.join(output_dir, "report.json"), bug)
        return report

    except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
        return create_results_dict(f, t = time.time() - start, status = "timeout")


def get_parser():
    parser = argparse.ArgumentParser()

    parser.add_argument("-j", "--jobs", dest="jobs", type=int, default=1)
    parser.add_argument("-tf", "--tableF", dest="tableFormat", type=str, default="latex")
    parser.add_argument("-to", "--tableO", dest="tableOutput", type=str, default="")
    parser.add_argument("test_dir")

    return parser


def parse(argv):
    return get_parser().parse_args(argv)


def create_default_table():
    return {     
        "file" : [],
        "paths" : [],
        "errors" : [], 
        "unknowns" : [], 
        "time analysis" : [] ,
        "time solver" :  [],
        "status" : [],
        "success" : [],
    }


def update_table(table, report):
    for key in report:
        if key == "time_analysis":
            table["time analysis"].append(str(report[key]))
        elif key == "time_solver":
            table["time solver"].append(str(report[key]))
        else:
            table[key].append(str(report[key]))


def create_result_table(results, fmt, outfile):
    def format_table(dtf, fmt):
        if fmt == "latex":
            return dtf.style.to_latex()
        else:
            return dtf.style.to_str()
    
    groupedResults = {}

    f = open(outfile, "w")
    acc = ""

    for report in results:
        f_name = report["file"]
        test_dir = (os.sep).join(f_name.split(os.sep)[0:-1])

        if test_dir not in groupedResults:
            groupedResults[test_dir] = create_default_table()
            update_table(groupedResults[test_dir], report)
        else:
            update_table(groupedResults[test_dir], report)

    for folder in groupedResults:
        for column in groupedResults[folder]:
            if column == "file":
                groupedResults[folder][column].append("Total")
            elif column == "success":
                success_count = groupedResults[folder][column].count("True")
                l = len(groupedResults[folder][column])
                groupedResults[folder][column].append(f"{success_count}/{l} passed")
            elif column == "status":
                completed_count = groupedResults[folder][column].count("completed")
                l = len(groupedResults[folder][column])
                groupedResults[folder][column].append(f"{completed_count}/{l} completed")
            elif column in ["time analysis", "time solver"]:
                 groupedResults[folder][column].append(sum(map(float, groupedResults[folder][column])))
            else:
                groupedResults[folder][column].append(sum(map(int, groupedResults[folder][column])))
        
        fmt_t = format_table(pandas.DataFrame(groupedResults[folder]), fmt)
        acc = acc + fmt_t

    f.write(acc)
    f.close()

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

    if args.tableOutput != "":
        create_result_table(results, args.tableFormat, args.tableOutput)
    
    oks = list(filter(lambda d: d["success"], results))
    print("Total:", len(oks), "/", len(results), "Passed",
          f"{sum(map(lambda d: d['time_analysis'], results))}s")
    return 0

if __name__ == "__main__":
    sys.exit(main())

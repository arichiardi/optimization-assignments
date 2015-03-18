#!/usr/bin/python
# -*- coding: utf-8 -*-

import os, argparse, sys
from subprocess import Popen, PIPE

def solve_it(input_data, launch_repl):

    if (launch_repl == False):
        # Writes the inputData to a temporay file
        tmp_file_name = 'tmp.data'
        tmp_file = open(tmp_file_name, 'w')
        tmp_file.write(input_data)
        tmp_file.close()

        # Runs lein uberjar synchrously
        lein = Popen(['lein', 'uberjar'])
        retcode = lein.wait()

        # Runs the command: java *-standalone.jar -file=tmp.data
        process = Popen(['java', '-cp', '../../target/assignments-0.1.0-SNAPSHOT-standalone.jar', 'knapsack.solver', '-f ' + tmp_file_name], stdout=PIPE)
        (stdout, stderr) = process.communicate()
        # removes the temporay file
        os.remove(tmp_file_name)
        return stdout.strip()

    else:
        # Runs lein uberjar synchrously
        lein = Popen(['lein', 'repl'])
        retcode = lein.wait()


if __name__ == '__main__':
    file_location = ''
    is_repl = False
    parser = argparse.ArgumentParser(description='Solves the knapsack problem', usage='python solver.py [-f|--file] FILE_PATH [-r|--repl]')
    parser.add_argument('-f', '--file', required=True, dest='file', help='The file to process (required)')
    parser.add_argument('-r', '--repl', dest='repl', action='store_true', help='Whether to launch a REPL session')
    args = parser.parse_args()

    print 'Input file: ', args.file
    print 'Requires Repl: ', args.repl

    input_data_file = open(args.file.strip(), 'r')
    input_data = ''.join(input_data_file.readlines())
    input_data_file.close()
    print solve_it(input_data, args.repl)

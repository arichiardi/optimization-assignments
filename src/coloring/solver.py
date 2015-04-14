#!/usr/bin/python
# -*- coding: utf-8 -*-

import os, argparse, sys
from subprocess import Popen, PIPE

def repl_it():

    lein = Popen(['lein', 'repl'])
    retcode = lein.wait()

def solve_it(input_data):

    # Writes the inputData to a temporay file
    tmp_file_name = 'tmp.data'
    tmp_file = open(tmp_file_name, 'w')
    tmp_file.write(input_data)
    tmp_file.close()

    # Runs the command: java *-standalone.jar -file=tmp.data
    process = Popen(['java', '-d64', '-Xmx4g', '-cp', '../../target/assignments-0.1.0-SNAPSHOT-standalone.jar', 'coloring.solver', '-f ' + tmp_file_name], stdout=PIPE)
    (stdout, stderr) = process.communicate()

    # removes the temporay file
    os.remove(tmp_file_name)
    return stdout.strip()

def launch_it(file_name, skip_compile):

    input_data_file = open(file_name.strip(), 'r')
    input_data = ''.join(input_data_file.readlines())
    input_data_file.close()

    # Writes the inputData to a temporay file
    tmp_file_name = 'tmp.data'
    tmp_file = open(tmp_file_name, 'w')
    tmp_file.write(input_data)
    tmp_file.close()

    if (skip_compile == False):
       # Runs lein uberjar synchrously
       lein = Popen(['lein', 'do', 'clean,', 'uberjar'])
       retcode = lein.wait()

    # Runs the command: java *-standalone.jar -file=tmp.data
    process = Popen(['java', '-d64', '-Xmx4g', '-cp', '../../target/assignments-0.1.0-SNAPSHOT-standalone.jar', 'coloring.solver', '-f ' + tmp_file_name], stdout=PIPE)
    (stdout, stderr) = process.communicate()
    # removes the temporay file
    os.remove(tmp_file_name)
    return stdout.strip()


if __name__ == '__main__':
    file_location = ''
    is_repl = False
    parser = argparse.ArgumentParser(description='Solves the knapsack problem')
    parser.add_argument('-s', '--skip-compile', dest='skip_compile', action='store_true', help='Skip the compilation step and run old jar file')
    group = parser.add_mutually_exclusive_group()
    group.add_argument('-f', '--file', dest='file', help='The file to process')
    group.add_argument('-r', '--repl', dest='repl', action='store_true', help='Launch a REPL session')
    args = parser.parse_args()

    # print 'Input file: ', args.file
    # print 'Requires Repl: ', args.repl

    if (args.file != None):
        print launch_it(args.file, args.skip_compile)
    elif (args.repl == True):
        repl_it()
    else:
        parser.print_help()

#!/usr/bin/env python

import sys
import re

def paren_matcher (n):
    # poor man's matched paren scanning, gives up
    # after n+1 levels.  Matches any string with balanced
    # parens inside; add the outer parens yourself if needed.
    # Nongreedy.
    return r"[^()]*?(?:\("*n+r"[^()]*?"+r"\)[^()]*?)*?"*n

def rename(f):
    if '0' <= f <= '9':
        return 'f' + f
    return f

if __name__ == "__main__":
    model = sys.stdin.read()
    metas = re.findall('(meta\([^\(\)]+?,[^\(\)]+?,[^\(\)]+?,[^\(\)]+?\))', model)
    if metas == []: # typed case
        metas = re.findall('(meta\(.+?,\(' + paren_matcher(20) + '\),.+?,.+?,.+?\))', model)
        metarules = [re.match('meta\((.+?),(\(' + paren_matcher(20) + '\)),(.+?),(.+?),(.+?)\)', m).groups() for m in metas]
    else: 
        metarules = [re.match('meta\((.+?),(.+?),(.+?),(.+?)\)', m).groups() for m in metas]
        metarules = [(r,None,f1,f2,f3) for (r,f1,f2,f3) in metarules]
    
    clauses = list()
    for (rule,type_,f1,f2,f3) in metarules:
        f1, f2, f3 = rename(f1), rename(f2), rename(f3)
        if rule == 'chain':
            clauses.append("{f1}(A,B):-{f2}(A,C),{f3}(C,B).".format(f1=f1,f2=f2,f3=f3))
        if rule == 'tohigherorder':
            clauses.append("{f1}(A,B):-{f2}(A,B,{f3}).".format(f1=f1,f2=f2,f3=f3))
    clauses.sort()

    for m in clauses:
        print(m)

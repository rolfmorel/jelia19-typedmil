#!/usr/bin/python

import random
import sys
from preds import PREDS

INT_LIMIT=10

map_def = """
map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).
"""

map_def_typed = """
inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).
"""


filter_def = """
filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).
"""

filter_def_typed = """
inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).
"""

HO = 'higher-order'
FO = 'first-order'

dist = [
#[HO,'my_map',lambda n: map_def + map_def_typed, None,None],
#[HO,'my_filter',lambda n: filter_def + filter_def_typed, None,None],
[FO,'my_len',lambda n: "my_len{}(A,B):-length(A,B).".format(n),'/2','[list(_),int]'],
[FO,'my_sumlist',lambda n: "my_sumlist{}(A,B):-sumlist(A,B).".format(n),'/2','[list(int),int]'],
[FO,'my_max_list',lambda n: "my_max_list{}(A,B):-max_list(A,B).".format(n),'/2','[list(int),int]'],
[FO,'my_min_list',lambda n: "my_min_list{}(A,B):-min_list(A,B).".format(n),'/2','[list(int),int]'],
[FO,'my_succ',lambda n: "my_succ{}(A,B):-succ(A,B),B =< {}.".format(n,INT_LIMIT),'/2','[int,int]'],
[FO,'my_pred',lambda n: "my_pred{}(A,B):-succ(B,A),A > 0.".format(n),'/2','[int,int]'],
[FO,'my_head',lambda n: "my_head{}([H|_],H).".format(n),'/2','[list(T),T]'],
[FO,'my_tail',lambda n: "my_tail{}([_|TL],TL).".format(n),'/2','[list(T),list(T)]'],
[FO,'my_reverse',lambda n: "my_reverse{}(A,B):-reverse(A,B).".format(n),'/2','[list(T),list(T)]'],
[FO,'my_last',lambda n: "my_last{}(A,B):-last(A,B).".format(n),'/2','[list(T),T]'],
[FO,'my_double',lambda n: "my_double{}(N,M):-M is 2*N,M =< {}.".format(n,INT_LIMIT),'/2','[int,int]'],
#[FO,'my_charcode',lambda n: "my_charcode{}(A,B):-char_code(A,B),format(atom(C),\"~w\",[X]).".format(n),'/2','[int,char]'],  # this is just a bad idea, especially for ASP
[FO,'my_flatten',lambda n: "my_flatten{}(A,B):-flatten(A,B).".format(n),'/2','[list(list(T)),list(T)]'],
[FO,'my_list_to_set',lambda n: "my_list_to_set{}(A,B):-list_to_set(A,B).".format(n),'/2','[list(T),list(T)]'],
[FO,'my_element',lambda n: "my_element{}(A,B):-member(B,A).".format(n),'/2','[list(T),T]'],
[FO,'my_msort',lambda n: "my_msort{}(A,B):-msort(A,B).".format(n),'/2','[list(int),list(int)]'],
[FO,'my_toupper',lambda n: "my_toupper{}(A,B):-upcase_atom(A,B).".format(n),'/2','[char,char]'],
[FO,'my_tolower',lambda n: "my_tolower{}(A,B):-downcase_atom(A,B).".format(n),'/2','[char,char]'],
[FO,'my_set',lambda n: "my_set{}(A):-list_to_set(A,A).".format(n),'/1','[list(_)]'],
[FO,'my_even',lambda n: "my_even{}(A):-0 is A mod 2.".format(n),'/1','[int]'],
[FO,'my_odd',lambda n: "my_odd{}(A):-1 is A mod 2.".format(n),'/1','[int]'],
[FO,'my_uppercase',lambda n: "my_uppercase{}(A):-upcase_atom(A,A).".format(n),'/1','[char]'],
[FO,'my_lowercase',lambda n: "my_lowercase{}(A):-downcase_atom(A,A).".format(n),'/1','[char]'],
]

def get_prog(prims, types_enabled):
  lib = 'metagol-typed' if types_enabled else 'metagol'
  header = """:- use_module('../../{}').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).
""".format(lib)

  metarules = ("""
metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
%metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
""" 
if types_enabled else
"""
metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
%metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
""")

  base_bk = """
"""
##tail([_|T],T).
#"""

  interpreted = ("""
inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).
"""
if types_enabled else
"""
map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).
""")
  interpreted=""

  annotation = ',[list(T),list(T)]' if types_enabled else '/2'
  primitives = """
"""
#prim(tail{0}).
#prim(reverse{0}).
#""".format(annotation)

  add_defs = []
  add_prims = []
  for i, prim in enumerate(prims):
    order, name, defi, suffix, type_ = prim
    if order == HO:
        add_defs.append(defi(0))
    else:
        add_defs.append(defi(str(i)))
        suf = (',' + type_ if types_enabled else suffix)
        add_prims.append("prim({name}{i}{suf}).".format(name=name, i=str(i), suf=suf))

#  for i in range(1,num_well_typed+1):
#    clauses.append(f"my_succ{i}(X,0).")
#    prim_defs.append(f"prim(my_succ{i},[int,int])." if types_enabled else f"prim(my_succ{i}/2).")
#  for i in range(num_well_typed+1,total_prims+1):
#    clauses.append(f"sucker{i}(X,0).")
#    prim_defs.append(f"prim(sucker{i},[bottom,bottom])." if types_enabled else f"prim(sucker{i}/2).")
#

  invocation = "learntyped(Pos,Neg,[list(char),char],H)" if types_enabled else "learn(Pos,Neg,H)"
 
  learn = """run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, ({invocation};true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\\n',[Duration]),
  format("%data,num_clauses,{num_clauses}\\n"),
  format("%data,types_enabled,{types_enabled}\\n").""".format(invocation=invocation,num_clauses=num_clauses,types_enabled=types_enabled)

  return header + base_bk + primitives + interpreted + metarules + '\n'.join(add_defs + add_prims) + '\n' + learn

if __name__ == "__main__":
  num_clauses = int(sys.argv[1]) # ignored for now
  num_add_preds = int(sys.argv[2])
  untyped_file = open(sys.argv[3], 'w')
  typed_file = open(sys.argv[4], 'w')
#  prims = [random.choice(dist) for _ in range(num_add_preds)]
  def select_and_remove_pred(name):
    for i, pred in enumerate(dist):
        if pred[1] == name:
            dist.pop(i)
            return pred
    raise Exception("pred by name '{}' not found".format(name))
  default_prims = [select_and_remove_pred(name) for name in PREDS]
  prims = default_prims + random.sample(dist, num_add_preds)

  print(get_prog(prims,True), file=typed_file)
  print(get_prog(prims,False), file=untyped_file)


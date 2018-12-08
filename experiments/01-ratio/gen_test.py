#!/usr/bin/python

import random
import sys

INT_LIMIT=5000

def gen_simple_test(num_clauses, num_well_typed, total_prims, types_enabled):
  header = """:- use_module('metagol{}').
:- use_module(library(system)).
metagol:max_clauses({num_clauses}).
""".format('-typed' if types_enabled else '')
  metarules = ("""
metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
""" 
if types_enabled else
"""
metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
""")

  clauses = []
  prim_defs=[]
  for i in range(1,num_well_typed+1):
    clauses.append("my_succ{}(X,0).".format(i))
    prim_defs.append("prim(my_succ{},[int,int]).".format(i) if types_enabled else "prim(my_succ{}/2).".format(i))
  for i in range(num_well_typed+1,total_prims+1):
    clauses.append("sucker{}(X,0).".format(i))
    prim_defs.append("prim(sucker{},[bottom,bottom]).".format(i) if types_enabled else "prim(sucker{}/2).".format(i))

  invocation = "learntyped([p(0,1)],[],[int,int],H)" if types_enabled else "learn([p(0,1)],[],H)"
  
  learn = """run :-get_time(T1),
  MaxTime=600, % 10 min
  catch(call_with_time_limit(MaxTime, ({invocation};true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  format('%prog,~w\\n',[H]),
  format('%data,time,~f\\n',[Duration]),
  format("%data,num_clauses,{num_clauses}\\n"),
  format("%data,num_well_typed,{num_well_typed}\\n"),
  format("%data,total_preds,{total_prims}\\n"),
  format("%data,types_enabled,{types_enabled}\\n").""".format(invocation=invocation,
          num_clauses=num_clauses,num_well_typed=num_well_typed,total_prims=total_prims,types_enabled=types_enabled)

  return header + metarules + '\n'.join(clauses + prim_defs) + '\n' + learn

def gen_simple_test2(num_clauses, num_well_typed, total_prims, types_enabled):
  header = """:- use_module('metagol{}').
:- use_module(library(system)).
metagol:max_clauses({num_clauses}).
""".format('-typed' if types_enabled else '',num_clauses=num_clauses)
  metarules = ("""
metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
""" 
if types_enabled else
"""
metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
""")

  clauses = []
  prim_defs=[]
  for i in range(1,num_well_typed+1):
    clauses.append("my_succ{i}(X,Y):-Y is X+1,Y<{INT_LIMIT}.".format(i=i,INT_LIMIT=INT_LIMIT))
    prim_defs.append("prim(my_succ{i},[int,int]).".format(i=i) if types_enabled else "prim(my_succ{i}/2).".format(i=i))
  for i in range(num_well_typed+1,total_prims+1):
    clauses.append("sucker{i}(X,Y):-Y is X+1,Y<{INT_LIMIT}.".format(i=i,INT_LIMIT=INT_LIMIT))
    prim_defs.append("prim(sucker{i},[bottom,bottom]).".format(i=i) if types_enabled else "prim(sucker{i}/2).".format(i=i))

  invocation = "learntyped([p(1,0)],[],[int,int],H)" if types_enabled else "learn([p(1,0)],[],H)"
  
  learn = """run :-get_time(T1),
  MaxTime=600, % 10 min
  catch(call_with_time_limit(MaxTime, ({invocation};true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  format('%prog,~w\\n',[H]),
  format('%data,time,~f\\n',[Duration]),
  format("%data,num_clauses,{num_clauses}\\n"),
  format("%data,num_well_typed,{num_well_typed}\\n"),
  format("%data,total_preds,{total_prims}\\n"),
  format("%data,types_enabled,{types_enabled}\\n").""".format(invocation=invocation,
          num_clauses=num_clauses,num_well_typed=num_well_typed,total_prims=total_prims,types_enabled=types_enabled)


  return header + metarules + '\n'.join(clauses + prim_defs) + '\n' + learn



print(gen_simple_test2(int(sys.argv[1]), int(sys.argv[2]), int(sys.argv[3]), sys.argv[4] == 'typed'))

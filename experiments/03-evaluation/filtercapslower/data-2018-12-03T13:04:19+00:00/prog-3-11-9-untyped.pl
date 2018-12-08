:- use_module('../../metagol').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P,Q,R],([P,A,B] :- [[Q,A,C],[R,C,B]])).
%metarule(dident,[P,Q,R],([P,A,B] :- [[Q,A,B],[R,A,B]])).
metarule(tohigherorder,[P,Q,F],([P,A,B] :- [[Q,A,B,F]])).
%metarule(tailrec,[P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).
my_uppercase0(A):-upcase_atom(A,A).
my_tolower1(A,B):-downcase_atom(A,B).

filter([],[],_F).
filter([A|T1],[A|T2],F):-
  call(F,A),
  filter(T1,T2,F).
filter([_|T1],T2,F):-
  filter(T1,T2,F).
interpreted(filter/3).

inter(filter_base,([filter,[],[],_]:[list(T),list(T),[T]]:-[])).
inter(filter_ind_incl,([filter,[H1|T1],[H1|T2],F]:[list(T),list(T),[T]]:-[[F,H1]:[T],[filter,T1,T2,F]:[list(T),list(T),[T]]])).
inter(filter_ind_excl,([filter,[_|T1],T2,F]:[list(T),list(T),[T]]:-[[filter,T1,T2,F]:[list(T),list(T),[T]]])).


map([],[],_F).
map([A|As],[B|Bs],F):-
  call(F,A,B),
  map(As,Bs,F).
interpreted(map/3).

inter(map_base,([map,[],[],_]:[list(S),list(T),[S,T]]:-[])).
inter(map_ind,([map,[H1|T1],[H2|T2],F]:[list(S),list(T),[S,T]]:-[[F,H1,H2]:[S,T],[map,T1,T2,F]:[list(S),list(T),[S,T]]])).

my_pred4(A,B):-succ(B,A),A > 0.
my_succ5(A,B):-succ(A,B),B =< 10.
my_element6(A,B):-member(B,A).
my_even7(A):-0 is A mod 2.
my_lowercase8(A):-downcase_atom(A,A).
my_list_to_set9(A,B):-list_to_set(A,B).
my_double10(N,M):-M is 2*N,M =< 10.
my_msort11(A,B):-msort(A,B).
my_last12(A,B):-last(A,B).
my_sumlist13(A,B):-sumlist(A,B).
my_reverse14(A,B):-reverse(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_pred4/2).
prim(my_succ5/2).
prim(my_element6/2).
prim(my_even7/1).
prim(my_lowercase8/1).
prim(my_list_to_set9/2).
prim(my_double10/2).
prim(my_msort11/2).
prim(my_last12/2).
prim(my_sumlist13/2).
prim(my_reverse14/2).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learn(Pos,Neg,H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,False\n").
p(['G',i,q,b,n,'S',z],[g,s]).
p(['K',c,'N',s],[k,n]).
p(['B',v,n,v,'T',r],[b,t]).
p([v,v,'E',z,'R',y,l,'A'],[e,r,a]).
p(['Y','G',i,'A'],[y,g,a]).
q(['Z',a,w,h,'H','S',w,'Z'],[s,z,z,h,'J']).
q(['G','A',t,'D','O',d,k,'W',d],[i,w,g,o,d,a]).
q(['I','L','Q','W'],['T',w,i,q,l]).
q([p,e,'Q','B',d,c,'E',n],['O',e,q,b]).
q(['O',y,c,'C','A'],[c,a,'U',o]).

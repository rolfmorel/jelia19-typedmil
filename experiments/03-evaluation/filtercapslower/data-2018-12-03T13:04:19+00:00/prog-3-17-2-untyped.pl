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

my_max_list4(A,B):-max_list(A,B).
my_reverse5(A,B):-reverse(A,B).
my_flatten6(A,B):-flatten(A,B).
my_min_list7(A,B):-min_list(A,B).
my_sumlist8(A,B):-sumlist(A,B).
my_even9(A):-0 is A mod 2.
my_head10([H|_],H).
my_double11(N,M):-M is 2*N,M =< 10.
my_pred12(A,B):-succ(B,A),A > 0.
my_list_to_set13(A,B):-list_to_set(A,B).
my_odd14(A):-1 is A mod 2.
my_tail15([_|TL],TL).
my_succ16(A,B):-succ(A,B),B =< 10.
my_len17(A,B):-length(A,B).
my_last18(A,B):-last(A,B).
my_set19(A):-list_to_set(A,A).
my_toupper20(A,B):-upcase_atom(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_max_list4/2).
prim(my_reverse5/2).
prim(my_flatten6/2).
prim(my_min_list7/2).
prim(my_sumlist8/2).
prim(my_even9/1).
prim(my_head10/2).
prim(my_double11/2).
prim(my_pred12/2).
prim(my_list_to_set13/2).
prim(my_odd14/1).
prim(my_tail15/2).
prim(my_succ16/2).
prim(my_len17/2).
prim(my_last18/2).
prim(my_set19/1).
prim(my_toupper20/2).
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
p(['N','J','B',c,'F'],[n,j,b,f]).
p(['W',n,z,p,l,'M','B','B','H'],[w,m,b,b,h]).
p(['I',s,o,'H',a,'J','B',q],[i,h,j,b]).
p([w,'Z','Y',j,'X','G','F',p],[z,y,x,g,f]).
p(['H','U','U',e,o,b,'I',q,'Q'],[h,u,u,i,q]).
q([v,m,'X',h,j,'X',u],[p,x,x]).
q([r,h,d,'L'],['O',l]).
q([h,r,r,h,'Y','Q'],[j,q,y]).
q(['L',y,'S',f,r,n,z,w,v],[s,q,l]).
q([o,v,'A','I',h,'G'],[z,i,a,g]).

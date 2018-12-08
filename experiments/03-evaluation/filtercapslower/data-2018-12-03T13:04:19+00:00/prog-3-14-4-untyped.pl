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

my_flatten4(A,B):-flatten(A,B).
my_sumlist5(A,B):-sumlist(A,B).
my_last6(A,B):-last(A,B).
my_list_to_set7(A,B):-list_to_set(A,B).
my_even8(A):-0 is A mod 2.
my_succ9(A,B):-succ(A,B),B =< 10.
my_max_list10(A,B):-max_list(A,B).
my_reverse11(A,B):-reverse(A,B).
my_msort12(A,B):-msort(A,B).
my_pred13(A,B):-succ(B,A),A > 0.
my_toupper14(A,B):-upcase_atom(A,B).
my_head15([H|_],H).
my_element16(A,B):-member(B,A).
my_lowercase17(A):-downcase_atom(A,A).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_flatten4/2).
prim(my_sumlist5/2).
prim(my_last6/2).
prim(my_list_to_set7/2).
prim(my_even8/1).
prim(my_succ9/2).
prim(my_max_list10/2).
prim(my_reverse11/2).
prim(my_msort12/2).
prim(my_pred13/2).
prim(my_toupper14/2).
prim(my_head15/2).
prim(my_element16/2).
prim(my_lowercase17/1).
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
p(['L','N','R','C',j,'N',n,'C',a],[l,n,r,c,n,c]).
p([c,'J','W',e,m,d,h,v,e],[j,w]).
p([e,d,'I',r],[i]).
p([z,d,'J',p,m],[j]).
p(['W','L','Q','F'],[w,l,q,f]).
q([a,'O',k,'W','G',o,p,f],[w,o,'Z',g]).
q([n,b,r,p],['R']).
q(['Z',g,'V',c],['L',z,v]).
q([i,'L',x,'D','V','L'],[l,d,l,j,v]).
q([w,u,'D','W',m,'E','D','K'],[w,e,d,k,'W',d]).

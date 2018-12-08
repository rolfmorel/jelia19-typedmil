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

my_even4(A):-0 is A mod 2.
my_last5(A,B):-last(A,B).
my_flatten6(A,B):-flatten(A,B).
my_max_list7(A,B):-max_list(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_pred9(A,B):-succ(B,A),A > 0.
my_sumlist10(A,B):-sumlist(A,B).
my_msort11(A,B):-msort(A,B).
my_tail12([_|TL],TL).
my_lowercase13(A):-downcase_atom(A,A).
my_reverse14(A,B):-reverse(A,B).
my_element15(A,B):-member(B,A).
my_toupper16(A,B):-upcase_atom(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_even4/1).
prim(my_last5/2).
prim(my_flatten6/2).
prim(my_max_list7/2).
prim(my_succ8/2).
prim(my_pred9/2).
prim(my_sumlist10/2).
prim(my_msort11/2).
prim(my_tail12/2).
prim(my_lowercase13/1).
prim(my_reverse14/2).
prim(my_element15/2).
prim(my_toupper16/2).
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
p(['J',j,l,y,d,c,'L','J',g],[j,l,j]).
p([q,'K','J',p,c,'X'],[k,j,x]).
p([b,'B','D','S',u,q,i],[b,d,s]).
p([t,'E','I',z,u,x,s],[e,i]).
p(['S','Y',i,j,'D'],[s,y,d]).
q([y,'W',o,'F'],[w,'J',f]).
q([h,'P','D','G','C',c,'Z'],[g,p,d,z,'L',c]).
q(['R','S','Q','W',q,u],[w,c,q,s,r]).
q(['M',w,a,y,f,'V','A'],[a,m,v,o]).
q(['E',o,e,i,m,m,l,z],[e,q]).

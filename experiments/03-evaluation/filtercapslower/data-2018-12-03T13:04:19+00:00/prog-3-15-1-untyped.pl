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
my_odd5(A):-1 is A mod 2.
my_tail6([_|TL],TL).
my_head7([H|_],H).
my_double8(N,M):-M is 2*N,M =< 10.
my_element9(A,B):-member(B,A).
my_list_to_set10(A,B):-list_to_set(A,B).
my_msort11(A,B):-msort(A,B).
my_lowercase12(A):-downcase_atom(A,A).
my_even13(A):-0 is A mod 2.
my_succ14(A,B):-succ(A,B),B =< 10.
my_last15(A,B):-last(A,B).
my_reverse16(A,B):-reverse(A,B).
my_set17(A):-list_to_set(A,A).
my_flatten18(A,B):-flatten(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_max_list4/2).
prim(my_odd5/1).
prim(my_tail6/2).
prim(my_head7/2).
prim(my_double8/2).
prim(my_element9/2).
prim(my_list_to_set10/2).
prim(my_msort11/2).
prim(my_lowercase12/1).
prim(my_even13/1).
prim(my_succ14/2).
prim(my_last15/2).
prim(my_reverse16/2).
prim(my_set17/1).
prim(my_flatten18/2).
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
p(['G',p,p,'Z','X'],[g,z,x]).
p([h,z,'K',d],[k]).
p(['Y','W',t,n,'S'],[y,w,s]).
p(['Q','O',u,r,'Q'],[q,o,q]).
p([w,'Y',t,'E','H','Q',p,j],[y,e,h,q]).
q([t,'Q','N',a,e,n,'O',l],[o,q,n,t]).
q(['Q','J',y,'E',g,'W','V',t],['D',q,j,w,e,v]).
q(['G','Y',f,'P'],[g,y,p,'D']).
q([j,'L','Q','N',g,'G'],[l,n,q,l,g]).
q([y,'S',s,'C'],[s,'U',c]).

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

my_len4(A,B):-length(A,B).
my_list_to_set5(A,B):-list_to_set(A,B).
my_succ6(A,B):-succ(A,B),B =< 10.
my_reverse7(A,B):-reverse(A,B).
my_odd8(A):-1 is A mod 2.
my_set9(A):-list_to_set(A,A).
my_min_list10(A,B):-min_list(A,B).
my_pred11(A,B):-succ(B,A),A > 0.
my_head12([H|_],H).
my_max_list13(A,B):-max_list(A,B).
my_toupper14(A,B):-upcase_atom(A,B).
my_even15(A):-0 is A mod 2.
my_double16(N,M):-M is 2*N,M =< 10.
my_msort17(A,B):-msort(A,B).
my_last18(A,B):-last(A,B).
my_tail19([_|TL],TL).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_len4/2).
prim(my_list_to_set5/2).
prim(my_succ6/2).
prim(my_reverse7/2).
prim(my_odd8/1).
prim(my_set9/1).
prim(my_min_list10/2).
prim(my_pred11/2).
prim(my_head12/2).
prim(my_max_list13/2).
prim(my_toupper14/2).
prim(my_even15/1).
prim(my_double16/2).
prim(my_msort17/2).
prim(my_last18/2).
prim(my_tail19/2).
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
p(['H',o,'P',x,'E',e,'E','L','O'],[h,p,e,e,l,o]).
p([y,'O','G','S'],[o,g,s]).
p(['A',x,t,'E'],[a,e]).
p([b,y,m,'D','W','W'],[d,w,w]).
p([l,'D','Y',z,'G',q,p,t],[d,y,g]).
q([t,p,f,'W',v,'W'],[w,x,w]).
q([m,i,'M','G',k],[m,g,o]).
q(['J',h,'O',z,n,'S',p],[j,s,l,o]).
q(['Q','P','T',u],['W',q,p,t]).
q(['P',x,'C',y],[p,'G',c]).

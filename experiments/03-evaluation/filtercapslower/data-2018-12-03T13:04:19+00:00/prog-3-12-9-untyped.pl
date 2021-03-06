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

my_msort4(A,B):-msort(A,B).
my_head5([H|_],H).
my_set6(A):-list_to_set(A,A).
my_succ7(A,B):-succ(A,B),B =< 10.
my_last8(A,B):-last(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_max_list10(A,B):-max_list(A,B).
my_lowercase11(A):-downcase_atom(A,A).
my_list_to_set12(A,B):-list_to_set(A,B).
my_len13(A,B):-length(A,B).
my_even14(A):-0 is A mod 2.
my_min_list15(A,B):-min_list(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_msort4/2).
prim(my_head5/2).
prim(my_set6/1).
prim(my_succ7/2).
prim(my_last8/2).
prim(my_pred9/2).
prim(my_max_list10/2).
prim(my_lowercase11/1).
prim(my_list_to_set12/2).
prim(my_len13/2).
prim(my_even14/1).
prim(my_min_list15/2).
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
p(['E','S',n,n,'S',h,s,u],[e,s,s]).
p(['M',c,'M',w,g,e],[m,m]).
p([x,e,g,o],[]).
p(['V',m,'T','H',g,'M',l,'C',t],[v,t,h,m,c]).
p(['Z','E','I',l,'A','J','H'],[z,e,i,a,j,h]).
q(['O',z,l,'O'],[o,'M',o]).
q([y,h,n,d,'I',x],[f,i]).
q([u,'C','E',t,i,g,p],[e,c,z]).
q(['J',n,h,'V',t],[q,j,v]).
q([q,'O',e,'H','A'],[o,'D',h,a]).

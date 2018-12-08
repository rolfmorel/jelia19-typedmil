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

my_head4([H|_],H).
my_pred5(A,B):-succ(B,A),A > 0.
my_list_to_set6(A,B):-list_to_set(A,B).
my_succ7(A,B):-succ(A,B),B =< 10.
my_max_list8(A,B):-max_list(A,B).
my_flatten9(A,B):-flatten(A,B).
my_last10(A,B):-last(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_toupper12(A,B):-upcase_atom(A,B).
my_set13(A):-list_to_set(A,A).
my_msort14(A,B):-msort(A,B).
my_reverse15(A,B):-reverse(A,B).
my_tail16([_|TL],TL).
my_element17(A,B):-member(B,A).
my_even18(A):-0 is A mod 2.
my_lowercase19(A):-downcase_atom(A,A).
my_len20(A,B):-length(A,B).
my_odd21(A):-1 is A mod 2.
my_min_list22(A,B):-min_list(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_head4/2).
prim(my_pred5/2).
prim(my_list_to_set6/2).
prim(my_succ7/2).
prim(my_max_list8/2).
prim(my_flatten9/2).
prim(my_last10/2).
prim(my_sumlist11/2).
prim(my_toupper12/2).
prim(my_set13/1).
prim(my_msort14/2).
prim(my_reverse15/2).
prim(my_tail16/2).
prim(my_element17/2).
prim(my_even18/1).
prim(my_lowercase19/1).
prim(my_len20/2).
prim(my_odd21/1).
prim(my_min_list22/2).
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
p(['Q','V','O','H','M',m],[q,v,o,h,m]).
p(['F',z,s,'H','S'],[f,h,s]).
p([q,'H',l,'X','O',y,'A'],[h,x,o,a]).
p([t,'X','O','G',c,i,'M'],[x,o,g,m]).
p([j,'S',c,'G','Z','I','S','E'],[s,g,z,i,s,e]).
q([j,'P',k,'O','A',v,'A','Q',h],['P',a,p,a,q,o]).
q(['S',i,w,j,'V',u,'R','N',k],[r,n,s,v,'V']).
q(['I','Z','M',a],[z,i,m,n]).
q([u,n,u,z,i,i,w],['W']).
q(['F',d,'E',d,'Y'],[y,e,e,f]).
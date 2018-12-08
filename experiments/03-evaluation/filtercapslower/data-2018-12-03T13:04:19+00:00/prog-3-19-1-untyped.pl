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

my_set4(A):-list_to_set(A,A).
my_pred5(A,B):-succ(B,A),A > 0.
my_list_to_set6(A,B):-list_to_set(A,B).
my_toupper7(A,B):-upcase_atom(A,B).
my_flatten8(A,B):-flatten(A,B).
my_len9(A,B):-length(A,B).
my_odd10(A):-1 is A mod 2.
my_sumlist11(A,B):-sumlist(A,B).
my_min_list12(A,B):-min_list(A,B).
my_tail13([_|TL],TL).
my_lowercase14(A):-downcase_atom(A,A).
my_head15([H|_],H).
my_reverse16(A,B):-reverse(A,B).
my_double17(N,M):-M is 2*N,M =< 10.
my_even18(A):-0 is A mod 2.
my_last19(A,B):-last(A,B).
my_msort20(A,B):-msort(A,B).
my_max_list21(A,B):-max_list(A,B).
my_succ22(A,B):-succ(A,B),B =< 10.
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_set4/1).
prim(my_pred5/2).
prim(my_list_to_set6/2).
prim(my_toupper7/2).
prim(my_flatten8/2).
prim(my_len9/2).
prim(my_odd10/1).
prim(my_sumlist11/2).
prim(my_min_list12/2).
prim(my_tail13/2).
prim(my_lowercase14/1).
prim(my_head15/2).
prim(my_reverse16/2).
prim(my_double17/2).
prim(my_even18/1).
prim(my_last19/2).
prim(my_msort20/2).
prim(my_max_list21/2).
prim(my_succ22/2).
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
p([i,m,p,h,'U'],[u]).
p(['O',c,'S',t,'S','D','Y','Y'],[o,s,s,d,y,y]).
p([e,'U',f,x,'N','Y',o,'U'],[u,n,y,u]).
p(['C','D','R','E','H'],[c,d,r,e,h]).
p([c,f,c,'T'],[t]).
q([p,l,'R',t],['H',r]).
q([k,'B',t,i,'Q'],[b,q,m]).
q(['S','N','T',t,'T','V','X'],[v,t,'Y',s,t,x,n]).
q([u,'U',l,'T'],[u,t,t]).
q([d,'V','A','R',r,'R','C','D','N'],[r,d,c,v,n,a,m,r]).

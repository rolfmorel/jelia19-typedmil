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
my_last5(A,B):-last(A,B).
my_reverse6(A,B):-reverse(A,B).
my_len7(A,B):-length(A,B).
my_toupper8(A,B):-upcase_atom(A,B).
my_pred9(A,B):-succ(B,A),A > 0.
my_succ10(A,B):-succ(A,B),B =< 10.
my_even11(A):-0 is A mod 2.
my_odd12(A):-1 is A mod 2.
my_set13(A):-list_to_set(A,A).
my_sumlist14(A,B):-sumlist(A,B).
my_element15(A,B):-member(B,A).
my_head16([H|_],H).
my_msort17(A,B):-msort(A,B).
my_tail18([_|TL],TL).
my_lowercase19(A):-downcase_atom(A,A).
my_min_list20(A,B):-min_list(A,B).
my_flatten21(A,B):-flatten(A,B).
my_double22(N,M):-M is 2*N,M =< 10.
my_list_to_set23(A,B):-list_to_set(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_max_list4/2).
prim(my_last5/2).
prim(my_reverse6/2).
prim(my_len7/2).
prim(my_toupper8/2).
prim(my_pred9/2).
prim(my_succ10/2).
prim(my_even11/1).
prim(my_odd12/1).
prim(my_set13/1).
prim(my_sumlist14/2).
prim(my_element15/2).
prim(my_head16/2).
prim(my_msort17/2).
prim(my_tail18/2).
prim(my_lowercase19/1).
prim(my_min_list20/2).
prim(my_flatten21/2).
prim(my_double22/2).
prim(my_list_to_set23/2).
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
p([p,c,q,'L'],[l]).
p([j,d,y,'X','I',y],[x,i]).
p([s,'L','H',u,a,'E','M'],[l,h,e,m]).
p([n,l,a,'O',a,o,z,j],[o]).
p([a,'O',w,y,m],[o]).
q([u,'X',f,f],['C',x]).
q(['U',n,b,'T'],[t,b,u]).
q(['J',o,i,i,'L',j,'T','L',x],[l,i,t,j,l]).
q([x,a,'N','M',j,o,z,x,p],['E',m,n]).
q(['U','P',g,z,i,'O',j,o],[u,o,'L',p]).

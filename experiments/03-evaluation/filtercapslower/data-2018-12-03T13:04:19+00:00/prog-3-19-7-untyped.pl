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
my_sumlist5(A,B):-sumlist(A,B).
my_min_list6(A,B):-min_list(A,B).
my_msort7(A,B):-msort(A,B).
my_odd8(A):-1 is A mod 2.
my_pred9(A,B):-succ(B,A),A > 0.
my_double10(N,M):-M is 2*N,M =< 10.
my_len11(A,B):-length(A,B).
my_list_to_set12(A,B):-list_to_set(A,B).
my_tail13([_|TL],TL).
my_succ14(A,B):-succ(A,B),B =< 10.
my_set15(A):-list_to_set(A,A).
my_max_list16(A,B):-max_list(A,B).
my_flatten17(A,B):-flatten(A,B).
my_last18(A,B):-last(A,B).
my_even19(A):-0 is A mod 2.
my_toupper20(A,B):-upcase_atom(A,B).
my_reverse21(A,B):-reverse(A,B).
my_lowercase22(A):-downcase_atom(A,A).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_head4/2).
prim(my_sumlist5/2).
prim(my_min_list6/2).
prim(my_msort7/2).
prim(my_odd8/1).
prim(my_pred9/2).
prim(my_double10/2).
prim(my_len11/2).
prim(my_list_to_set12/2).
prim(my_tail13/2).
prim(my_succ14/2).
prim(my_set15/1).
prim(my_max_list16/2).
prim(my_flatten17/2).
prim(my_last18/2).
prim(my_even19/1).
prim(my_toupper20/2).
prim(my_reverse21/2).
prim(my_lowercase22/1).
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
p(['U',d,k,a,w],[u]).
p(['X',l,k,'D','Q','Z',t,i],[x,d,q,z]).
p(['A','A','H',k,r],[a,a,h]).
p(['R',n,j,m,r,'A'],[r,a]).
p([p,j,a,p,'X',a,j,e,'H'],[x,h]).
q([r,'W',r,k],[w,g]).
q([p,'G',m,'V',k],[v,'H',g]).
q([h,s,j,h,s,k,'A'],[o,a]).
q(['U',x,w,'V','S','S'],[s,s,u,v,'P']).
q([h,x,b,x,e,x,'F'],['U',f]).

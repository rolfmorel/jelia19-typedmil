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
my_head5([H|_],H).
my_succ6(A,B):-succ(A,B),B =< 10.
my_lowercase7(A):-downcase_atom(A,A).
my_reverse8(A,B):-reverse(A,B).
my_set9(A):-list_to_set(A,A).
my_last10(A,B):-last(A,B).
my_sumlist11(A,B):-sumlist(A,B).
my_tail12([_|TL],TL).
my_min_list13(A,B):-min_list(A,B).
my_pred14(A,B):-succ(B,A),A > 0.
my_flatten15(A,B):-flatten(A,B).
my_msort16(A,B):-msort(A,B).
my_max_list17(A,B):-max_list(A,B).
my_double18(N,M):-M is 2*N,M =< 10.
my_toupper19(A,B):-upcase_atom(A,B).
my_element20(A,B):-member(B,A).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_len4/2).
prim(my_head5/2).
prim(my_succ6/2).
prim(my_lowercase7/1).
prim(my_reverse8/2).
prim(my_set9/1).
prim(my_last10/2).
prim(my_sumlist11/2).
prim(my_tail12/2).
prim(my_min_list13/2).
prim(my_pred14/2).
prim(my_flatten15/2).
prim(my_msort16/2).
prim(my_max_list17/2).
prim(my_double18/2).
prim(my_toupper19/2).
prim(my_element20/2).
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
p([f,'B','B','P',f,e,a,'E',l],[b,b,p,e]).
p([c,h,'B','G',m,j,d,h],[b,g]).
p(['X',u,l,'I','I','E',j,p],[x,i,i,e]).
p([d,'S','N',g,y,f,'D'],[s,n,d]).
p(['J',o,'A',a],[j,a]).
q(['T','T','X','U'],[t,w,x,u,t]).
q(['D','O',l,'D',a,'A','J',k,m],['F',d,a,j,d,o]).
q([l,'T','Z',f,c,x,'Q','W'],[z,q,t,x,w]).
q([d,h,'N',a,h],[n,y]).
q(['R',i,'R',c,'S',g,'T','E',l],[r,s,e,t,f,r]).

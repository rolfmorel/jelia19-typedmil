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
my_len5(A,B):-length(A,B).
my_head6([H|_],H).
my_sumlist7(A,B):-sumlist(A,B).
my_max_list8(A,B):-max_list(A,B).
my_element9(A,B):-member(B,A).
my_lowercase10(A):-downcase_atom(A,A).
my_double11(N,M):-M is 2*N,M =< 10.
my_reverse12(A,B):-reverse(A,B).
my_even13(A):-0 is A mod 2.
my_pred14(A,B):-succ(B,A),A > 0.
my_tail15([_|TL],TL).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_flatten4/2).
prim(my_len5/2).
prim(my_head6/2).
prim(my_sumlist7/2).
prim(my_max_list8/2).
prim(my_element9/2).
prim(my_lowercase10/1).
prim(my_double11/2).
prim(my_reverse12/2).
prim(my_even13/1).
prim(my_pred14/2).
prim(my_tail15/2).
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
p([b,z,'F',n,y,'G',l],[f,g]).
p([v,k,'U',s],[u]).
p([d,g,'C',s],[c]).
p([s,c,k,'U','B',b,'Z'],[u,b,z]).
p([a,'T',f,'U'],[t,u]).
q(['Y','D','H','P','W','A'],[w,h,y,s,p,d,a]).
q([b,'U','L','D',j,'V',i],[k,v,l,d,u]).
q([c,'T','P','R',w,x,u],['S',t,p,r]).
q(['R','K','M','K',o,y,'T','P'],[k,t,k,m,p,r,'W']).
q([g,'U',x,d,'H','F'],['P',u,f,h]).

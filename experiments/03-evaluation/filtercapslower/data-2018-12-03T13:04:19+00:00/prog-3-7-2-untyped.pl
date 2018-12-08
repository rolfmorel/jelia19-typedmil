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
my_toupper5(A,B):-upcase_atom(A,B).
my_odd6(A):-1 is A mod 2.
my_set7(A):-list_to_set(A,A).
my_head8([H|_],H).
my_flatten9(A,B):-flatten(A,B).
my_msort10(A,B):-msort(A,B).
prim(my_uppercase0/1).
prim(my_tolower1/2).
prim(my_max_list4/2).
prim(my_toupper5/2).
prim(my_odd6/1).
prim(my_set7/1).
prim(my_head8/2).
prim(my_flatten9/2).
prim(my_msort10/2).
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
p([b,r,r,'M','X','V','C'],[m,x,v,c]).
p([z,o,w,s,a],[]).
p(['Y',s,'H','K',b,n,'O'],[y,h,k,o]).
p([m,q,'R','J','A','E','P','G','W'],[r,j,a,e,p,g,w]).
p(['U',i,'M','P',v,v,r],[u,m,p]).
q([n,'G','Q','S'],[q,g,s,'V']).
q([t,n,z,'N',d,k],[n,'P']).
q([t,b,c,'P','C'],[p,c,'S']).
q(['S',n,'I','N',j,'V',a,'I',l],[n,s,i,g,v,i]).
q([j,'G',p,'L'],[l,'B',g]).

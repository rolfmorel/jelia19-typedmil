:- use_module('../../metagol-typed').
:- use_module(library(system)).
:- use_module(library(lists)).
metagol:max_clauses(3).



metarule(chain,[P:[Ta,Tb],Q:[Ta,Tc],R:[Tc,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Tc],[R,C,B]:[Tc,Tb]])).
%metarule(dident,[P:[Ta,Tb],Q:[Ta,Tb],R:[Ta,Tb]],([P,A,B]:[Ta,Tb] :- [[Q,A,B]:[Ta,Tb],[R,A,B]:[Ta,Tb]])).
metarule(tohigherorder,[P:[Ta,Tb],Q:[Ta,Tb,Tf],F:Tf],([P,A,B]:[Ta,Tb] :- [[Q,A,B,F]:[Ta,Tb,Tf]])).
%metarule(tailrec,[P:[Ta,Tb],Q:[Ta,Ta]],([P,A,B]:[Ta,Tb] :- [[Q,A,C]:[Ta,Ta],[P,C,B]:[Ta,Tb]])).
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

my_toupper4(A,B):-upcase_atom(A,B).
my_head5([H|_],H).
my_msort6(A,B):-msort(A,B).
my_reverse7(A,B):-reverse(A,B).
my_succ8(A,B):-succ(A,B),B =< 10.
my_flatten9(A,B):-flatten(A,B).
my_min_list10(A,B):-min_list(A,B).
my_last11(A,B):-last(A,B).
my_set12(A):-list_to_set(A,A).
my_tail13([_|TL],TL).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_toupper4,[char,char]).
prim(my_head5,[list(T),T]).
prim(my_msort6,[list(int),list(int)]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_succ8,[int,int]).
prim(my_flatten9,[list(list(T)),list(T)]).
prim(my_min_list10,[list(int),int]).
prim(my_last11,[list(T),T]).
prim(my_set12,[list(_)]).
prim(my_tail13,[list(T),list(T)]).
run :-get_time(T1),
  MaxTime=600, % 10 min
  findall(p(A,B),(p(A,B)),Pos),
  findall(p(A,B),(q(A,B)),Neg),
  catch(call_with_time_limit(MaxTime, (learntyped(Pos,Neg,[list(char),list(char)],H);true)),
      time_limit_exceeded,
      H = no_answer),
%  time_out((;true),MaxTime,Result),
  get_time(T2),
  Duration is T2-T1,
  pprint(H),
  format('%data,time,~f\n',[Duration]),
  format("%data,num_clauses,3\n"),
  format("%data,types_enabled,True\n").
p(['U','W',n,v,'U',z,a,'W','K'],[u,w,u,w,k]).
p(['P','G','N',e,'F',o],[p,g,n,f]).
p(['C','S','H','A'],[c,s,h,a]).
p([i,v,'F','I',d,z,i,'D'],[f,i,d]).
p(['X','D','M','G',c],[x,d,m,g]).
q(['U',g,z,'R','Y'],[y,n,r,u]).
q([y,'I',j,'H',j,z,c,k,'R'],[r,h,'I',i]).
q(['G',j,u,'M'],[m,g,'E']).
q([h,'H',q,'V',w,w],[h,v,p]).
q(['O','L','T',h,'U'],[t,u,o,'I',l]).

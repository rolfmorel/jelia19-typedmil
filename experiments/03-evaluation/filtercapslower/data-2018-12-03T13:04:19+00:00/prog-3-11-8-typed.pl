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

my_even4(A):-0 is A mod 2.
my_len5(A,B):-length(A,B).
my_odd6(A):-1 is A mod 2.
my_list_to_set7(A,B):-list_to_set(A,B).
my_msort8(A,B):-msort(A,B).
my_succ9(A,B):-succ(A,B),B =< 10.
my_max_list10(A,B):-max_list(A,B).
my_head11([H|_],H).
my_toupper12(A,B):-upcase_atom(A,B).
my_tail13([_|TL],TL).
my_set14(A):-list_to_set(A,A).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_even4,[int]).
prim(my_len5,[list(_),int]).
prim(my_odd6,[int]).
prim(my_list_to_set7,[list(T),list(T)]).
prim(my_msort8,[list(int),list(int)]).
prim(my_succ9,[int,int]).
prim(my_max_list10,[list(int),int]).
prim(my_head11,[list(T),T]).
prim(my_toupper12,[char,char]).
prim(my_tail13,[list(T),list(T)]).
prim(my_set14,[list(_)]).
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
p([v,'E','J',a,'C',p,'I','I'],[e,j,c,i,i]).
p(['C','C','L',g,'U',p,'V','M'],[c,c,l,u,v,m]).
p(['S','R',e,'Z'],[s,r,z]).
p([t,u,'F','C',z,x,a,'Y',s],[f,c,y]).
p(['O',q,'J','B','A',h,i],[o,j,b,a]).
q(['B',z,'D','S',b],[d,b,'J',s]).
q(['P','O',f,'D',o],[p,d,e,o]).
q(['Z','W',s,'K','A',r,'Y',g],[z,a,y,'Z',k,w]).
q(['V',o,'O','P','V',f,'Z'],[p,v,d,o,z,v]).
q([i,g,'R',x,a,'G','P','J',t],[p,j,g,r,c]).

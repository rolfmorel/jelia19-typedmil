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

my_lowercase4(A):-downcase_atom(A,A).
my_msort5(A,B):-msort(A,B).
my_flatten6(A,B):-flatten(A,B).
my_head7([H|_],H).
my_min_list8(A,B):-min_list(A,B).
my_odd9(A):-1 is A mod 2.
my_list_to_set10(A,B):-list_to_set(A,B).
my_set11(A):-list_to_set(A,A).
my_even12(A):-0 is A mod 2.
my_tail13([_|TL],TL).
my_element14(A,B):-member(B,A).
my_reverse15(A,B):-reverse(A,B).
my_pred16(A,B):-succ(B,A),A > 0.
my_double17(N,M):-M is 2*N,M =< 10.
my_len18(A,B):-length(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_lowercase4,[char]).
prim(my_msort5,[list(int),list(int)]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_head7,[list(T),T]).
prim(my_min_list8,[list(int),int]).
prim(my_odd9,[int]).
prim(my_list_to_set10,[list(T),list(T)]).
prim(my_set11,[list(_)]).
prim(my_even12,[int]).
prim(my_tail13,[list(T),list(T)]).
prim(my_element14,[list(T),T]).
prim(my_reverse15,[list(T),list(T)]).
prim(my_pred16,[int,int]).
prim(my_double17,[int,int]).
prim(my_len18,[list(_),int]).
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
p(['S',d,'Q',u],[s,q]).
p([c,'H','I','X','C','S'],[h,i,x,c,s]).
p(['C','O',w,r,'K',y,k,'B','U'],[c,o,k,b,u]).
p([t,'K','E','C','C',p],[k,e,c,c]).
p([d,w,l,'T','D','A',x,'K'],[t,d,a,k]).
q([s,f,'H','M','M',h,'Z','F'],[m,'I',z,m,f,h]).
q(['Z','D','Z','V'],['H',z,d,v,z]).
q(['W',t,e,'Q',g],['K',w,q]).
q([t,o,k,'N',j,n,j],[k,n]).
q([a,l,'A',e,'J','Z',l,'P','M'],[p,z,z,a,m,j]).

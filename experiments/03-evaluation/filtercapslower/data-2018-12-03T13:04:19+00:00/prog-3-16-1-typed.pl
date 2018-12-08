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

my_pred4(A,B):-succ(B,A),A > 0.
my_msort5(A,B):-msort(A,B).
my_double6(N,M):-M is 2*N,M =< 10.
my_sumlist7(A,B):-sumlist(A,B).
my_reverse8(A,B):-reverse(A,B).
my_odd9(A):-1 is A mod 2.
my_flatten10(A,B):-flatten(A,B).
my_list_to_set11(A,B):-list_to_set(A,B).
my_even12(A):-0 is A mod 2.
my_max_list13(A,B):-max_list(A,B).
my_lowercase14(A):-downcase_atom(A,A).
my_set15(A):-list_to_set(A,A).
my_element16(A,B):-member(B,A).
my_len17(A,B):-length(A,B).
my_toupper18(A,B):-upcase_atom(A,B).
my_head19([H|_],H).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_pred4,[int,int]).
prim(my_msort5,[list(int),list(int)]).
prim(my_double6,[int,int]).
prim(my_sumlist7,[list(int),int]).
prim(my_reverse8,[list(T),list(T)]).
prim(my_odd9,[int]).
prim(my_flatten10,[list(list(T)),list(T)]).
prim(my_list_to_set11,[list(T),list(T)]).
prim(my_even12,[int]).
prim(my_max_list13,[list(int),int]).
prim(my_lowercase14,[char]).
prim(my_set15,[list(_)]).
prim(my_element16,[list(T),T]).
prim(my_len17,[list(_),int]).
prim(my_toupper18,[char,char]).
prim(my_head19,[list(T),T]).
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
p(['S',j,e,'P','Y',v,'K',x],[s,p,y,k]).
p([d,j,'C',e,'R',i,'E','Q','K'],[c,r,e,q,k]).
p(['J',e,v,'Z'],[j,z]).
p(['S',p,'Z','O'],[s,z,o]).
p([u,c,o,w],[]).
q(['O',u,h,q,'Q',x],[a,o,q]).
q(['W',y,'V','Y','U',b],[v,w,u,y,f]).
q(['X','U',s,m,'T',d,'L'],[l,u,x,t,j]).
q(['V','Q',h,g,'M'],[q,m,l,v]).
q(['T','I',y,p,s],[t,'S',i]).

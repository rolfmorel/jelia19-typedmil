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

my_odd4(A):-1 is A mod 2.
my_list_to_set5(A,B):-list_to_set(A,B).
my_max_list6(A,B):-max_list(A,B).
my_reverse7(A,B):-reverse(A,B).
my_flatten8(A,B):-flatten(A,B).
my_len9(A,B):-length(A,B).
my_lowercase10(A):-downcase_atom(A,A).
my_set11(A):-list_to_set(A,A).
my_even12(A):-0 is A mod 2.
my_head13([H|_],H).
my_element14(A,B):-member(B,A).
my_pred15(A,B):-succ(B,A),A > 0.
my_double16(N,M):-M is 2*N,M =< 10.
my_msort17(A,B):-msort(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_odd4,[int]).
prim(my_list_to_set5,[list(T),list(T)]).
prim(my_max_list6,[list(int),int]).
prim(my_reverse7,[list(T),list(T)]).
prim(my_flatten8,[list(list(T)),list(T)]).
prim(my_len9,[list(_),int]).
prim(my_lowercase10,[char]).
prim(my_set11,[list(_)]).
prim(my_even12,[int]).
prim(my_head13,[list(T),T]).
prim(my_element14,[list(T),T]).
prim(my_pred15,[int,int]).
prim(my_double16,[int,int]).
prim(my_msort17,[list(int),list(int)]).
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
p(['K','E',j,'L','D','G',i,j,s],[k,e,l,d,g]).
p([f,r,n,m,'P'],[p]).
p(['V','U','L',z,'Z','X','B',v],[v,u,l,z,x,b]).
p(['N',a,'W',c,'J',o,'L'],[n,w,j,l]).
p([u,'H','Q','M','X',n,l,c,'P'],[h,q,m,x,p]).
q([w,'T',h,'U','A'],[u,a,'L',t]).
q([q,z,'A',j,'G',g,a,'V',p],[g,'R',a,v]).
q(['W',c,b,n,d,'C',k,'Y'],[w,w,y,c]).
q([l,'G',s,q,'M','F'],[m,f,'D',g]).
q(['J','A','J',s,l,q,y,r,h],[j,a,j,w]).

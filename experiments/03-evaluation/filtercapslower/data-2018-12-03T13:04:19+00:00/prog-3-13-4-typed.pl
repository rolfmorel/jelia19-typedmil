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

my_sumlist4(A,B):-sumlist(A,B).
my_odd5(A):-1 is A mod 2.
my_list_to_set6(A,B):-list_to_set(A,B).
my_set7(A):-list_to_set(A,A).
my_pred8(A,B):-succ(B,A),A > 0.
my_min_list9(A,B):-min_list(A,B).
my_element10(A,B):-member(B,A).
my_tail11([_|TL],TL).
my_reverse12(A,B):-reverse(A,B).
my_toupper13(A,B):-upcase_atom(A,B).
my_head14([H|_],H).
my_len15(A,B):-length(A,B).
my_last16(A,B):-last(A,B).
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_sumlist4,[list(int),int]).
prim(my_odd5,[int]).
prim(my_list_to_set6,[list(T),list(T)]).
prim(my_set7,[list(_)]).
prim(my_pred8,[int,int]).
prim(my_min_list9,[list(int),int]).
prim(my_element10,[list(T),T]).
prim(my_tail11,[list(T),list(T)]).
prim(my_reverse12,[list(T),list(T)]).
prim(my_toupper13,[char,char]).
prim(my_head14,[list(T),T]).
prim(my_len15,[list(_),int]).
prim(my_last16,[list(T),T]).
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
p([i,d,'J','V',m],[j,v]).
p([a,'Z','J',q,'G'],[z,j,g]).
p(['Z',q,'L',k,'R',j,'X',a],[z,l,r,x]).
p(['A','D','Z','W',p,i,'E'],[a,d,z,w,e]).
p(['I',x,p,p,y,m,'W'],[i,w]).
q(['F','X',u,q,o],[f,x,o]).
q(['B','T',r,q,k,p,'R'],[t,b,d,r]).
q(['G','T','Z','C'],[t,c,g,z,b]).
q([s,p,f,'C'],[s,c]).
q(['K',b,'M','U'],[k,u,'Z',m]).

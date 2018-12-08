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

my_element4(A,B):-member(B,A).
my_odd5(A):-1 is A mod 2.
my_flatten6(A,B):-flatten(A,B).
my_pred7(A,B):-succ(B,A),A > 0.
my_sumlist8(A,B):-sumlist(A,B).
my_set9(A):-list_to_set(A,A).
my_toupper10(A,B):-upcase_atom(A,B).
my_even11(A):-0 is A mod 2.
my_head12([H|_],H).
my_lowercase13(A):-downcase_atom(A,A).
my_list_to_set14(A,B):-list_to_set(A,B).
my_max_list15(A,B):-max_list(A,B).
my_len16(A,B):-length(A,B).
my_msort17(A,B):-msort(A,B).
my_succ18(A,B):-succ(A,B),B =< 10.
prim(my_uppercase0,[char]).
prim(my_tolower1,[char,char]).
prim(my_element4,[list(T),T]).
prim(my_odd5,[int]).
prim(my_flatten6,[list(list(T)),list(T)]).
prim(my_pred7,[int,int]).
prim(my_sumlist8,[list(int),int]).
prim(my_set9,[list(_)]).
prim(my_toupper10,[char,char]).
prim(my_even11,[int]).
prim(my_head12,[list(T),T]).
prim(my_lowercase13,[char]).
prim(my_list_to_set14,[list(T),list(T)]).
prim(my_max_list15,[list(int),int]).
prim(my_len16,[list(_),int]).
prim(my_msort17,[list(int),list(int)]).
prim(my_succ18,[int,int]).
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
p([c,'Z',f,'X','B'],[z,x,b]).
p([d,u,v,o,'C',k,u],[c]).
p(['K','B',o,'Q',r,d,'O',h,'I'],[k,b,q,o,i]).
p([g,o,z,i,'I',o],[i]).
p(['G','I',f,'Z','S'],[g,i,z,s]).
q([u,m,v,x],['E']).
q([n,'H',g,'A','A',z,h],[a,h,a,a]).
q(['R',m,q,'N',e,n],[r,v,n]).
q(['N','W',o,v,'Z','Y','E','T','E'],[e,e,t,y,j,w,z,n]).
q([j,'K',v,'G',p,w,k],[g,k,'U']).

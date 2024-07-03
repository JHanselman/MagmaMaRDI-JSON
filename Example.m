AttachSpec("spec");

file1:= "Testfiles/save_test.txt";
file2:= "Testfiles/save_test2.txt";
file3:= "Testfiles/save_test3.txt";
file4:= "Testfiles/save_test4.txt";
file5:= "Testfiles/save_test5.txt";
file6:= "Testfiles/save_test6.txt";
file7:= "Testfiles/save_test7.txt";

R<x,y,z> := PolynomialRing(Rationals(), 3);
p:= x^4*y^2 - 98*z^5 -23*x*y*z;
save_mardi_json(file1, p);

P<a,b> := PolynomialRing(R, 2);
save_mardi_json(file2, a*x+b*y);

save_mardi_json(file3, 3/5);

R<x> := PolynomialRing(Rationals());
p:= x^4- 32*x + 12/5;
save_mardi_json(file4, p);


K<a> := GF(13,5);
R<x> := PolynomialRing(K);
L, mL:= ext<K| x^2 -7>;
o:= L.1;
P<y,z> := PolynomialRing(L,2);
M := Matrix(P, 3,2, [o*y^2 -z,o^2+y*z,o-1, mL(a)^10, 4-z^4, 0]);
save_mardi_json(file5, M);


M := Matrix(Rationals(), 2, 3, [1,2,3,4,5,6]);
save_mardi_json(file6, M);

R<x,y,z> := PolynomialRing(Rationals(), 3);
M := Matrix(R, 3, 2, [0,0,0,z,x^2-1,y*z]);
save_mardi_json(file7, M);

load_mardi_json(file1);
load_mardi_json(file2);
load_mardi_json(file3);
load_mardi_json(file4);
load_mardi_json(file5);
load_mardi_json(file6);
load_mardi_json(file7);

%%
% Guillaume Vaudescal
% VAUG30119904
% ECO7011 TP1

%% 
clear;
close all;
clc;

try 
    d.macro = xlsread('/Users/guilou/Documents/Maitrise économie financière/02_Hiver 2022/ECO 7011 Macroéconomie avancée /Travaux pratiques/TP1/10_data/eco7011_seriesca.xls');
end

d.macro = readtable('/Users/guilou/Documents/Maitrise économie financière/02_Hiver 2022/ECO 7011 Macroéconomie avancée /Travaux pratiques/TP1/10_data/eco7011_seriesca.xls');

d.variables = table2array(d.macro(:,3:end));
d.date = table2array(d.macro(:,1));

%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%% QUESTION 1

% a)
    
%  Y = PIB
%  C = Consommation
%  I = Investissement
%  G = Depense publique
%  X = Exportations
%  M = Importations

s.Y = log(d.variables(5:end-2,1));
s.C = log(d.variables(5:end-2,2)); 
s.I = log(d.variables(5:end-2,3)); 
s.G = log(d.variables(5:end-2,4)); 
s.X = log(d.variables(5:end-2,5)); 
s.M = log(d.variables(5:end-2,6));       

% On vérifie s'il y a des valeurs manquantes :

s.indexY = find(isnan(s.Y));
s.Y(s.indexY) = [];
s.indexC = find(isnan(s.C));
s.C(s.indexC) = [];
s.indexI = find(isnan(s.I));
s.I(s.indexI) = [];
s.indexG = find(isnan(s.G));
s.G(s.indexG) = [];
s.indexX = find(isnan(s.X));
s.X(s.indexX) = [];
s.indexM = find(isnan(s.M));
s.M(s.indexM) = [];

lambda = 1600;

% On retire la tendance des données avec le filtre HP :

hp.Y = s.Y - filtreHP(s.Y,lambda);
hp.C = s.C - filtreHP(s.C,lambda);
hp.I = s.I - filtreHP(s.I,lambda);
hp.G = s.G - filtreHP(s.G,lambda);
hp.X = s.X - filtreHP(s.X,lambda);
hp.M = s.M - filtreHP(s.M,lambda);

% Calcul des Écarts-types :

et.hp.Y = 100*std(hp.Y);
et.hp.C = 100*std(hp.C);
et.hp.I = 100*std(hp.I);
et.hp.G = 100*std(hp.G);
et.hp.X = 100*std(hp.X);
et.hp.M = 100*std(hp.M);

disp([et.hp.Y,et.hp.C,et.hp.I,et.hp.G,et.hp.X,et.hp.M])

% Calcul de la correlation croisee de chacun de nos variables sur Y : 
% On choisis comme nombres de lags : 5

hp.cross.Y = NaN(1,11);
for i=1:5
   hp.cross.Y(1,6-i) = corr(hp.Y(1+i:end),hp.Y(1:end-i));
   hp.cross.Y(1,6+i) = corr(hp.Y(1:end-i),hp.Y(1+i:end));
end
hp.cross.Y(1,6) = corr(hp.Y, hp.Y);

hp.cross.C = NaN(1,11);
for i=1:5
   hp.cross.C(1,6-i) = corr(hp.Y(1+i:end),hp.C(1:end-i));
   hp.cross.C(1,6+i) = corr(hp.Y(1:end-i),hp.C(1+i:end));
end
hp.cross.C(1,6) = corr(hp.Y, hp.C);

hp.cross.I = NaN(1,11);
for i=1:5
   hp.cross.I(1,6-i) = corr(hp.Y(1+i:end),hp.I(1:end-i));
   hp.cross.I(1,6+i) = corr(hp.Y(1:end-i),hp.I(1+i:end));
end
hp.cross.I(1,6) = corr(hp.Y, hp.I);

hp.cross.G = NaN(1,11);
for i=1:5
   hp.cross.G(1,6-i) = corr(hp.Y(1+i:end),hp.G(1:end-i));
   hp.cross.G(1,6+i) = corr(hp.Y(1:end-i),hp.G(1+i:end));
end
hp.cross.G(1,6) = corr(hp.Y, hp.G);

hp.cross.X = NaN(1,11);
for i=1:5
   hp.cross.X(1,6-i) = corr(hp.Y(1+i:end),hp.X(1:end-i));
   hp.cross.X(1,6+i) = corr(hp.Y(1:end-i),hp.X(1+i:end));
end
hp.cross.X(1,6) = corr(hp.Y, hp.X);

hp.cross.M = NaN(1,11);
for i=1:5
   hp.cross.M(1,6-i) = corr(hp.Y(1+i:end),hp.M(1:end-i));
   hp.cross.M(1,6+i) = corr(hp.Y(1:end-i),hp.M(1+i:end));
end
hp.cross.M(1,6) = corr(hp.Y, hp.M);

disp([hp.cross.Y]);
disp([hp.cross.C]);
disp([hp.cross.I]);
disp([hp.cross.G]);
disp([hp.cross.X]);
disp([hp.cross.M]);

%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%% QUESTION 2

%   Y = PIB
%   C = Consommation
%   I = Investissement
%   SEPH = Emploi SEPH
%   P.SEPH = Productivite du travail SEPH

s.Yn = d.variables(5:end-2,1);
s.indexYn = find(isnan(s.Yn));
s.Yn(s.indexYn) = [];

s.SEPHn = d.variables(5:end-2,12);
s.indexSEPHn = find(isnan(s.SEPHn));
s.SEPHn(s.indexSEPHn) = [];
s.SEPH = log(s.SEPHn);

s.P.SEPHn = s.Yn ./ s.SEPHn;
s.P.SEPH = log(s.P.SEPHn);

% On retire la composante tendancielle avec le filtre HP :

hp.SEPH = s.SEPH - filtreHP(s.SEPH,lambda);
hp.P.SEPH = s.P.SEPH - filtreHP(s.P.SEPH,lambda);

% Calcul des Écart-types :

et.hp.Y;
et.hp.C;
et.hp.I;
et.hp.SEPH = 100*std(hp.SEPH);
et.hp.P.SEPH = 100*std(hp.P.SEPH);

disp([et.hp.Y]);
disp([et.hp.C]);
disp([et.hp.I]);
disp([et.hp.SEPH]);
disp([et.hp.P.SEPH]);

% Calcul de la correlation contemporaine avec le PIB :

corr.Y = corrcoef(hp.Y,hp.Y);
corr.C = corrcoef(hp.Y,hp.C);
corr.I = corrcoef(hp.Y,hp.I);
corr.SEPH = corrcoef(hp.Y,hp.SEPH);
corr.P.SEPH = corrcoef(hp.Y,hp.P.SEPH);

disp([corr.Y]);
disp([corr.C]);
disp([corr.I]);
disp([corr.SEPH]);
disp([corr.P.SEPH]);

% Méthode différence 1er :

[l,c]=size(s.I);
for i=1:l-1
dif.Y(i) = s.Y(i+1) - s.Y(i);
dif.C(i) = s.C(i+1) - s.C(i);
dif.I(i) = s.I(i+1) - s.I(i);
dif.SEPH(i) = s.SEPH(i+1) - s.SEPH(i);
dif.P.SEPH(i) = s.P.SEPH(i+1)- s.P.SEPH(i);
end

% Calcul des Écart-types :

et.dif.Y = 100*std(dif.Y);
et.dif.C = 100*std(dif.C);
et.dif.I = 100*std(dif.I);
et.dif.SEPH = 100*std(dif.SEPH);
et.dif.P.SEPH = 100*std(dif.P.SEPH);

disp([et.dif.Y]);
disp([et.dif.C]);
disp([et.dif.I]);
disp([et.dif.SEPH]);
disp([et.dif.P.SEPH]);

% Calcul de la correlation contemporaine avec le PIB :

corr.dif.Y = corrcoef(dif.Y,dif.Y);
corr.dif.C = corrcoef(dif.Y,dif.C);
corr.dif.I = corrcoef(dif.Y,dif.I);
corr.dif.SEPH = corrcoef(dif.Y,dif.SEPH);
corr.dif.P.SEPH = corrcoef(dif.Y,dif.P.SEPH);

disp([corr.dif.Y]);
disp([corr.dif.C]);
disp([corr.dif.I]);
disp([corr.dif.SEPH]);
disp([corr.dif.P.SEPH]);

% Méthode tendance lineaire :

Time = 1:length(s.Y);
Time = Time';

[gQ,~,~,~,~,~,~,~,~,~,resgg] = OLS(s.Y, Time,1);
Yt.Y = [ones(length(s.Y),1), Time] * gQ;
lin.Y = s.Y - Yt.Y;

[cQ,~,~,~,~,~,~,~,~,~,resgc] = OLS(s.C, Time,1);
Yt.C = [ones(length(s.C),1), Time] * cQ;
lin.C = s.C - Yt.C;

[iQ,~,~,~,~,~,~,~,~,~,resgi] = OLS(s.I, Time,1);
Yt.I = [ones(length(s.I),1), Time] * iQ;
lin.I = s.I - Yt.I;

[sQ,~,~,~,~,~,~,~,~,~,resgs] = OLS(s.SEPH, Time,1);
Yt.SEPH = [ones(length(s.SEPH),1), Time] * sQ;
lin.SEPH = s.SEPH - Yt.SEPH;

[pQ,~,~,~,~,~,~,~,~,~,resg] = OLS(s.P.SEPH, Time,1);
Yt.P.SEPH = [ones(length(s.P.SEPH),1), Time] * pQ;
lin.P.SEPH = s.P.SEPH - Yt.P.SEPH;

% Calcul des Écart-types :

et.lin.Y = 100*std(lin.Y);
et.lin.C = 100*std(lin.C);
et.lin.I = 100*std(lin.I);
et.lin.SEPH = 100*std(lin.SEPH);
et.lin.P.SEPH = 100*std(lin.P.SEPH);

disp([et.lin.Y]);
disp([et.lin.C]);
disp([et.lin.I]);
disp([et.lin.SEPH]);
disp([et.lin.P.SEPH]);

% Calcul de la correlation contemporaine avec le PIB :

corr.lin.Y = corrcoef(lin.Y,lin.Y);
corr.lin.C = corrcoef(lin.Y,lin.C);
corr.lin.I = corrcoef(lin.Y,lin.I);
corr.lin.SEPH = corrcoef(lin.Y,lin.SEPH);
corr.lin.P.SEPH = corrcoef(lin.Y,lin.P.SEPH);

disp([corr.lin.Y]);
disp([corr.lin.C]);
disp([corr.lin.I]);
disp([corr.lin.SEPH]);
disp([corr.lin.P.SEPH]);

%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%% QUESTION 3

%   w = Salaire reel
%   SEPH = Emploi seph
%   LFS = Emploi LFS
%   P.SEPH = Prod. du travail SEPH
%   P.LFS = Prod. du travail LFS

w = d.variables(5:end-2,13);
s.w = log(d.variables(5:end-2,13));
s.indexw = find(isnan(s.w));
s.w(s.indexw) = [];

s.LFSn = d.variables(5:end-2,10);
s.indexLFSn = find(isnan(s.LFSn));
s.LFSn(s.indexLFSn) = [];
s.LFS = log(s.LFSn);

s.P.LFSn = s.Yn ./ s.LFSn;
s.P.LFS = log(s.P.LFSn);


% On enlève la composante tandancielle avec le filtre HP :

hp.w = s.w - filtreHP(s.w,lambda);
hp.LFS = s.LFS - filtreHP(s.LFS,lambda);
hp.P.LFS = s.P.LFS - filtreHP(s.P.LFS,lambda);

% Calcul des Écarts-types :

et.hp.w = 100*std(hp.w);
et.hp.SEPH;
et.hp.LFS = 100*std(hp.LFS);
et.hp.P.SEPH;
et.hp.P.LFS = 100*std(hp.P.LFS);

disp([et.hp.w,et.hp.SEPH,et.hp.LFS,et.hp.P.SEPH,et.hp.P.LFS]);

% Calcul de la correlation croisée de chacune de nos variables sur Y : 
% On choisis comme nombre de lags : 5

hp.cross.w = NaN(1,11);

for i=1:5
   hp.cross.w(1,6-i) = corr(hp.Y(1+i:end),hp.w(1:end-i));
   hp.cross.w(1,6+i) = corr(hp.Y(1:end-i),hp.w(1+i:end));
end
hp.cross.w(1,6) = corr(hp.Y, hp.w);

hp.cross.SEPH = NaN(1,11);
for i=1:5
   hp.cross.SEPH(1,6-i) = corr(hp.Y(1+i:end),hp.SEPH(1:end-i));
   hp.cross.SEPH(1,6+i) = corr(hp.Y(1:end-i),hp.SEPH(1+i:end));
end
hp.cross.SEPH(1,6) = corr(hp.Y, hp.SEPH);

hp.cross.LFS = NaN(1,11);
for i=1:5
   hp.cross.LFS(1,6-i) = corr(hp.Y(1+i:end),hp.LFS(1:end-i));
   hp.cross.LFS(1,6+i) = corr(hp.Y(1:end-i),hp.LFS(1+i:end));
end
hp.cross.LFS(1,6) = corr(hp.Y, hp.LFS);

hp.cross.P.SEPH = NaN(1,11);
for i=1:5
   hp.cross.P.SEPH(1,6-i) = corr(hp.Y(1+i:end),hp.P.SEPH(1:end-i));
   hp.cross.P.SEPH(1,6+i) = corr(hp.Y(1:end-i),hp.P.SEPH(1+i:end));
end
hp.cross.P.SEPH(1,6) = corr(hp.Y, hp.P.SEPH);

hp.cross.P.LFS = NaN(1,11);
for i=1:5
   hp.cross.P.LFS(1,6-i) = corr(hp.Y(1+i:end),hp.P.LFS(1:end-i));
   hp.cross.P.LFS(1,6+i) = corr(hp.Y(1:end-i),hp.P.LFS(1+i:end));
end
hp.cross.P.LFS(1,6) = corr(hp.Y, hp.P.LFS);

disp([hp.cross.SEPH]);
disp([hp.cross.LFS]);
disp([hp.cross.P.SEPH]);
disp([hp.cross.P.LFS]);
disp([hp.cross.w]);

% Calcul de la correlation croisee de la productivite du travail sur le
% salaire :
% On chosis comme nombres de lags : 5

hp.cross.P.SEPHw = NaN(1,11);
for i=1:5
   hp.cross.P.SEPHw(1,6-i) = corr(hp.w(1+i:end),hp.P.SEPH(1:end-i));
   hp.cross.P.SEPHw(1,6+i) = corr(hp.w(1:end-i),hp.P.SEPH(1+i:end));
end
hp.cross.P.SEPHw(1,6) = corr(hp.w, hp.P.SEPH);

hp.cross.P.LFSw = NaN(1,11);
for i=1:5
   hp.cross.P.LFSw(1,6-i) = corr(hp.w(1+i:end),hp.P.LFS(1:end-i));
   hp.cross.P.LFSw(1,6+i) = corr(hp.w(1:end-i),hp.P.LFS(1+i:end));
end
hp.cross.P.LFSw(1,6) = corr(hp.w, hp.P.LFS);

disp([hp.cross.P.SEPHw]);
disp([hp.cross.P.LFSw]);

%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%% QUESTION 4

%   w2 = Salaire reel 1947-1984
%   SEPH2 = Emploi seph 1947-1984
%   LFS2 = Emploi LFS 1947-1984
%   P.SEPH2 = Prod. du travail SEPH 1947-1984
%   P.LFS2 = Prod. du travail LFS 1947-1984
%   w3 = Salaire reel 1985-2017
%   SEPH3 = Emploi seph 1985-2017
%   LFS3 = Emploi LFS 1985-2017
%   P.SEPH3 = Prod. du travail SEPH 1985-2017
%   P.LFS3 = Prod. du travail LFS 1985-2017

s.Y2n = d.variables(5:153,1);
s.indexY2n = find(isnan(s.Y2n));
s.Y2n(s.indexY2n) = [];
s.Y2 = log(s.Y2n);
s.Y3n = d.variables(154:end-2,1);
s.indexY3n = find(isnan(s.Y3n));
s.Y3n(s.indexY3n) = [];
s.Y3 = log(s.Y3n);

s.SEPH2n = d.variables(5:153,12);
s.indexSEPH2n = find(isnan(s.SEPH2n));
s.SEPH2n(s.indexSEPH2n) = [];
s.SEPH2 = log(s.SEPH2n);
s.SEPH3n = d.variables(154:end-2,12);
s.indexSEPH3n = find(isnan(s.SEPH3n));
s.SEPH3n(s.indexSEPH3n) = [];
s.SEPH3 = log(s.SEPH3n);

s.LFS2n = d.variables(5:153,10);
s.indexLFS2n = find(isnan(s.LFS2n));
s.LFS2n(s.indexLFS2n) = [];
s.LFS2 = log(s.LFS2n);
s.LFS3n = d.variables(154:end-2,10);
s.indexLFS3n = find(isnan(s.LFS3n));
s.LFS3n(s.indexLFS3n) = [];
s.LFS3 = log(s.LFS3n);

s.P.SEPH2n = s.Y2n ./ s.SEPH2n;
s.P.SEPH2 = log(s.P.SEPH2n);
s.P.SEPH3n = s.Y3n ./ s.SEPH3n;
s.P.SEPH3 = log(s.P.SEPH3n);

s.P.LFS2n = s.Y2n ./ s.LFS2n;
s.P.LFS2 = log(s.P.LFS2n);
s.P.LFS3n = s.Y3n ./ s.LFS3n;
s.P.LFS3 = log(s.P.LFS3n);

s.w2 = log(d.variables(5:153,13));
s.indexw2 = find(isnan(s.w2));
s.w2(s.indexw2) = [];
s.w3 = log(d.variables(154:end-2,13));
s.indexw3 = find(isnan(s.w3));
s.w3(s.indexw3) = [];

% On enlève la composante tandancielle avec le filtre HP :

hp.Y2 = s.Y2 - filtreHP(s.Y2,lambda);
hp.Y3 = s.Y3 - filtreHP(s.Y3,lambda);

hp.SEPH2 = s.SEPH2 - filtreHP(s.SEPH2,lambda);
hp.SEPH3 = s.SEPH3 - filtreHP(s.SEPH3,lambda);

hp.LFS2 = s.LFS2 - filtreHP(s.LFS2,lambda);
hp.LFS3 = s.LFS3 - filtreHP(s.LFS3,lambda);

hp.P.SEPH2 = s.P.SEPH2 - filtreHP(s.P.SEPH2,lambda);
hp.P.SEPH3 = s.P.SEPH3 - filtreHP(s.P.SEPH3,lambda);

hp.P.LFS2 = s.P.LFS2 - filtreHP(s.P.LFS2,lambda);
hp.P.LFS3 = s.P.LFS3 - filtreHP(s.P.LFS3,lambda);

hp.w2 = s.w2 - filtreHP(s.w2,lambda);
hp.w3 = s.w3 - filtreHP(s.w3,lambda);

% Calcul des Écarts-types :

et.hp.SEPH2 = 100*std(hp.SEPH2);
et.hp.SEPH3 = 100*std(hp.SEPH3);

et.hp.LFS2 = 100*std(hp.LFS2);
et.hp.LFS3 = 100*std(hp.LFS3);

et.hp.P.SEPH2 = 100*std(hp.P.SEPH2);
et.hp.P.SEPH3 = 100*std(hp.P.SEPH3);

et.hp.P.LFS2 = 100*std(hp.P.LFS2);
et.hp.P.LFS3 = 100*std(hp.P.LFS3);

et.hp.w2 = 100*std(hp.w2);
et.hp.w3 = 100*std(hp.w3);

disp([et.hp.SEPH2,et.hp.LFS2,et.hp.P.SEPH2,et.hp.P.LFS2,et.hp.w2]);
disp([et.hp.SEPH3,et.hp.LFS3,et.hp.P.SEPH3,et.hp.P.LFS3,et.hp.w3]);

% Calcul de la correlation contemporaine de chacunes de nos variables sur Y : 

corr.SEPH2 = corrcoef(hp.Y2,hp.SEPH2);
corr.SEPH3 = corrcoef(hp.Y3,hp.SEPH3);

corr.LFS2 = corrcoef(hp.Y2,hp.LFS2);
corr.LFS3 = corrcoef(hp.Y3,hp.LFS3);

corr.P.SEPH2 = corrcoef(hp.Y2,hp.P.SEPH2);
corr.P.SEPH3 = corrcoef(hp.Y3,hp.P.SEPH3);

corr.P.LFS2 = corrcoef(hp.Y2,hp.P.LFS2);
corr.P.LFS3 = corrcoef(hp.Y3,hp.P.LFS3);

corr.w2 = corrcoef(hp.Y2,hp.w2);
corr.w3 = corrcoef(hp.Y3,hp.w3);

disp([corr.SEPH2]);
disp([corr.LFS2]);
disp([corr.P.SEPH2]);
disp([corr.P.LFS2]);
disp([corr.w2]);

disp([corr.SEPH3]);
disp([corr.LFS3]);
disp([corr.P.SEPH3]);
disp([corr.P.LFS3]);
disp([corr.w3]);

%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%% Question 5

%   Y = PIB
%   IPC = Indice des prix a la consommation

s.IPC = log(d.variables(5:end-2,7));

% On retire la composante tandancielle avec le filtre HP :

hp.IPC = s.IPC - filtreHP(s.IPC,lambda);

% Calcul de l'Écart-type :

et.hp.IPC = 100*std(hp.IPC);

disp([et.hp.IPC]);


% Calcul de la correlation croisee de l'IPC sur Y :
% On choisis comme nombres de lags : 5

hp.cross.P.IPC = NaN(1,11);
for i=1:5
   hp.cross.IPC(1,6-i) = corr(hp.Y(1+i:end),hp.IPC(1:end-i));
   hp.cross.IPC(1,6+i) = corr(hp.Y(1:end-i),hp.IPC(1+i:end));
end
hp.cross.IPC(1,6) = corr(hp.w, hp.IPC);

disp([hp.cross.IPC]);

%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

%% Question 6

%   PIB1 = 1946 - 1972
%   PIB2 = 1973 - 1982
%   PIB3 = 1983 - 1999
%   INF1 = Inflation1 : 1946 - 1972
%   INF2 = Inflation2 : 1973 - 1982
%   INF3 = Inflation3 : 1983 - 1999

s.PIB1 = log(d.variables(5:109,1));
s.indexPIB1 = find(isnan(s.PIB1));
s.PIB1(s.indexPIB1) = [];
s.PIB2 = log(d.variables(110:149,1));
s.indexPIB2 = find(isnan(s.PIB2));
s.PIB2(s.indexPIB2) = [];
s.PIB3 = log(d.variables(150:217,1));
s.indexPIB3 = find(isnan(s.PIB3));
s.PIB3(s.indexPIB3) = [];

s.INF1 = d.variables(5:109,8);
s.indexINF1 = find(isnan(s.PIB1));
s.INF1(s.indexINF1) = [];
s.INF2 = d.variables(110:149,8);
s.indexINF2 = find(isnan(s.INF2));
s.INF2(s.indexINF2) = [];
s.INF3 = d.variables(150:217,8);
s.indexINF3 = find(isnan(s.INF3));
s.PIB3(s.indexINF3) = [];

% On enlève la composante tandancielle avec le filtre HP :

hp.PIB1 = s.PIB1 - filtreHP(s.PIB1,lambda);
hp.PIB2 = s.PIB2 - filtreHP(s.PIB2,lambda);
hp.PIB3 = s.PIB3 - filtreHP(s.PIB3,lambda);

% Calcul des moyennes :

moy.INF1 = mean(s.INF1);
moy.INF2 = mean(s.INF2);
moy.INF3 = mean(s.INF3);

disp([moy.INF1,moy.INF2,moy.INF3]);

% Calcul des Écart-types :

et.INF1 = std(s.INF1);
et.INF2 = std(s.INF2);
et.INF3 = std(s.INF3);

disp([et.INF1,et.INF2,et.INF3]);

% Calcul de la correlation contemporaine avec Y :

corr.INF1 = corrcoef(hp.PIB1,s.INF1);
corr.INF2 = corrcoef(hp.PIB2,s.INF2);
corr.INF3 = corrcoef(hp.PIB3,s.INF3);

disp([corr.INF1]);
disp([corr.INF2]);
disp([corr.INF3]);

